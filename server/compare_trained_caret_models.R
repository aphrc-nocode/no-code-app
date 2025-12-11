#### ---- Compare model metrics ----------------------------------- ####
model_training_caret_train_metrics_server = function() {
	
	
	observeEvent(input$model_training_apply, {
		
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(!is.null(rv_training_results$train_metrics_df))) {
					
					observe({
						req(!is.null(rv_training_results$tuned_parameters))
						req(!is.null(rv_training_results$control_parameters))
						output$model_training_caret_train_tuned_parameters = renderUI({
							txt = capture.output(str(rv_training_results$tuned_parameters))
							pre(paste(txt, collapse = "\n"))
						})

						output$model_training_caret_train_training_control = renderUI({
							txt = capture.output(str(rv_training_results$control_parameters))
							pre(paste(txt, collapse = "\n"))
						})
					})

					## Training data
					output$model_training_caret_train_metrics_plot = renderPlot({
						p1 = plot(rv_training_results$train_metrics_df)	
						Rautoml::save_rautoml_plot(objects=p1
							, name="training_performance_metrics"
							, dataset_id=rv_ml_ai$dataset_id
							, session_name=rv_ml_ai$session_id
							, timestamp=Sys.time()
							, output_dir="outputs"
							, metric_type="training_metrics"
						)
						p1
					})
					
					output$model_training_caret_train_metrics_plotdown <- downloadHandler(
					  filename = function(){
					    paste(
					      "train_metrics",Sys.time(), ".png")
					  },
					  content = function(file){
					    ggsave(filename = file,
					           plot = 	plot(rv_training_results$train_metrics_df), dpi = 300
					    )
					  },
					  contentType = "image/png"
					  
					)

					output$model_training_caret_train_metrics_df = DT::renderDT({
					  df = rv_training_results$train_metrics_df

					  DT::datatable(
						 df,
						 escape = FALSE,
						 rownames = FALSE,
						 options = list(
							processing = FALSE,
							autoWidth = FALSE,
							scrollX = TRUE,
							columnDefs = list(
							  list(className = 'dt-center', targets = c("lower", "estimate", "upper"))
							)
						 )
					  ) %>%
						 DT::formatRound(columns = c("lower", "estimate", "upper"), digits = 4)
					})
					
					
					output$model_training_caret_train_metrics_dfdown <- downloadHandler(
					  filename = function(){
					    paste(
					      "train_metrics",Sys.time(), ".csv")
					  },
					  content = function(file){
					    write.csv(rv_training_results$train_metrics_df, file = file,row.names = FALSE
					    )
					  },
					  contentType = "text/csv"
					  
					)
					
					if (isTRUE(!is.null(rv_training_results$test_metrics_objs))) {
						## Test data
						test_plots = tryCatch({
							plot(rv_training_results$test_metrics_objs)
						}, error = function(e) {
							shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
							return(NULL)
						})
					
						if (is.null(test_plots)) return()
						
						## Save test metric plots
						save_test_plots = tryCatch({
							Rautoml::save_rautoml_plot(objects=test_plots
								, name="test_performance_metrics"
								, dataset_id=rv_ml_ai$dataset_id
								, session_name=rv_ml_ai$session_id
								, timestamp=Sys.time()
								, output_dir="outputs"
								, metric_type="test_metrics"
							)
							invisible(TRUE)
						}, error=function(e) {
							shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
							return(NULL)
						})

						if (is.null(save_test_plots)) return()

						output$model_training_caret_test_metrics_plot_specifics = renderPlot({
							test_plots$specifics
						})
						
						
						output$model_training_caret_test_metrics_plot_specificsdown <- downloadHandler(
						  filename = function(){
						    paste(
						      "test_plots_specifics",Sys.time(), ".png")
						  },
						  content = function(file){
						    ggsave(filename = file,
						           plot = 	test_plots$specifics, dpi = 300
						    )
						  },
						  contentType = "image/png"
						  
						)
						
						output$model_training_caret_test_metrics_plot_all = renderPlot({
							test_plots$all	
						})
						
						output$model_training_caret_test_metrics_plot_alldown <- downloadHandler(
						  filename = function(){
						    paste(
						      "test_plots_all",Sys.time(), ".png")
						  },
						  content = function(file){
						    ggsave(filename = file,
						           plot = 	test_plots$all, dpi = 300
						    )
						  },
						  contentType = "image/png"
						)

						output$model_training_caret_test_metrics_plot_roc = renderPlot({
							test_plots$roc
						})
						
						output$model_training_caret_test_metrics_plot_rocdown <- downloadHandler(
						  filename = function(){
						    paste(
						      "test_plots_roc",Sys.time(), ".png")
						  },
						  content = function(file){
						    ggsave(filename = file,
						           plot = test_plots$roc, dpi = 300
						    )
						  },
						  contentType = "image/png"
						  
						)
						
						output$model_training_caret_test_metrics_df = DT::renderDT({
						  df = rv_training_results$test_metrics_objs$all

						  DT::datatable(
							 df,
							 escape = FALSE,
							 rownames = FALSE,
							 options = list(
								processing = FALSE,
								autoWidth = FALSE,
								scrollX = TRUE,
								columnDefs = list(
								  list(className = 'dt-center', targets = c("lower", "estimate", "upper"))
								)
							 )
						  ) %>%
							 DT::formatRound(columns = c("lower", "estimate", "upper"), digits = 4)
						})
						
						
						output$model_training_caret_test_metrics_dfdown <- downloadHandler(
						  filename = function(){
						    paste(
						      "metricsoutputs",Sys.time(), ".csv")
						  },
						  content = function(file){
						    write.csv(rv_training_results$test_metrics_objs$all, file = file,row.names = FALSE
						    )
						  },
						  contentType = "text/csv"
						  
						)
						
                 rv_training_models$all_trained_models = tryCatch({
                    Rautoml::get_rv_objects(pattern="_trained_model$", rv_training_models)
                 }, error=function(e){
                    shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
                    return(NULL)
                 })

                 if (is.null(rv_training_models$all_trained_models)) return()
 
					  ## More options: SHAP values
					  output$model_training_caret_more_options_shap = renderUI({
							prettyRadioButtons(
								inputId = "model_training_caret_more_options_shap_check"
									, label = get_rv_labels("model_training_caret_more_options_shap_check")
									, choices = get_named_choices(input_choices_file, input$change_language, "model_training_caret_more_options_shap_check")
									, inline = TRUE
									, status = "success"
							)
					  })
					  
					  ## Download results
					  output$model_training_caret_metrics_download_all = renderUI({
					  		ROOT_DIR = paste0("outputs/", rv_ml_ai$dataset_id, "/", rv_ml_ai$session_id)
							p(
								selectInput("model_training_caret_metrics_download_all"
										, label = get_rv_labels("model_training_caret_metrics_download_all_ui")
										, choices = unique(c(ROOT_DIR, fs::dir_ls(ROOT_DIR, type = "directory", recurse = FALSE)))
										, selected = ROOT_DIR
										, selectize = TRUE
										, width="100%"
								)
								, br()
								, downloadBttn("model_training_caret_metrics_download_all_zip", get_rv_labels("model_training_caret_metrics_download_all_zip"))
							)
					  })

						output$model_training_caret_metrics_download_all_zip = downloadHandler(
						 filename = function() {
							paste0(basename(input$model_training_caret_metrics_download_all), ".zip")
						 },
						 content = function(file) {
							start_progress_bar(id="model_training_caret_metrics_download_all_zip_pb", att_new_obj=model_training_caret_metrics_download_all_zip_pb, text=get_rv_labels("model_training_caret_metrics_download_all_zip"))
							
							folder = input$model_training_caret_metrics_download_all
							
							# Copy folder to tempdir
							temp_folder = file.path(tempdir(), basename(folder))
							if (dir_exists(temp_folder)) dir_delete(temp_folder)
							dir_copy(folder, temp_folder)
							
							# Quietly create ZIP using utils::zip
							old_wd = setwd(tempdir())
							on.exit(setwd(old_wd), add = TRUE)
							
							# Redirect stdout and stderr to suppress console messages
							suppressMessages(
							  suppressWarnings(
								 utils::zip(
									zipfile = file,
									files = basename(temp_folder),
									extras = "-r"
								 )
							  )
							)
							close_progress_bar(model_training_caret_metrics_download_all_zip_pb)
						 },
						 contentType = "application/zip"
						)
					
					} else {
						output$model_training_caret_test_metrics_plot_specifics = NULL
						output$model_training_caret_test_metrics_plot_all = NULL
						output$model_training_caret_test_metrics_plot_roc = NULL
						output$model_training_caret_test_metrics_df = NULL
						
						rv_training_models$all_trained_models = NULL
						output$model_training_caret_more_options_shap = NULL
			
						output$model_training_caret_train_metrics_plot = NULL
						output$model_training_caret_train_metrics_df = NULL
							
						output$model_training_caret_test_metrics_plot_specifics = NULL
						output$model_training_caret_test_metrics_plot_all = NULL
						output$model_training_caret_test_metrics_plot_roc = NULL
						output$model_training_caret_test_metrics_df = NULL
				
						output$model_training_caret_train_tuned_parameters = NULL
						output$model_training_caret_train_training_control = NULL

						 output$model_training_caret_metrics_download_all = NULL
						 output$model_training_caret_metrics_download_all_zip = NULL
							
					}

					
					output$model_training_caret_post_model_metrics = renderUI({
					  req(rv_training_results$post_model_metrics_objs)
					  if (isTRUE(!is.null(rv_training_results$post_model_metrics_objs))) {
					    post_model_metrics_objs = rv_training_results$post_model_metrics_objs
					    
					    ui_list = list()
					    counter = 1
					    
					    plot_labels = list(
					      cm_plot = get_rv_labels("model_training_caret_post_model_metrics_cm"),
					      var_imp_plot = get_rv_labels("model_training_caret_post_model_metrics_vi")
					    )
					   
					    for (model_name in names(post_model_metrics_objs)) {
					      row_plots = list()
					      
					      # Section header + ALL DOWNLOAD BUTTON
					      model_section = list(
					        h3(model_name, style = "margin-top:30px; color:#2c3e50;"),
					        downloadBttn(
					          paste0("download_all_", model_name),
					          label = get_rv_labels("download_plots"),
					          color = "success"
					        )
					      )
					      
					      # Generate plots
					      for (plot_name in names(post_model_metrics_objs[[model_name]])) {
					        if (!is.null(post_model_metrics_objs[[model_name]][[plot_name]])) {
					          
					          plot_id = paste0("plot_", counter)
					          
					          local({
					            my_model = model_name
					            my_plot  = plot_name
					            my_id    = plot_id
					            
					            output[[my_id]] = renderPlot({
					              post_model_metrics_objs[[my_model]][[my_plot]]
					            })
					          })
					          
					          label = ifelse(
					            plot_name %in% names(plot_labels),
					            plot_labels[[plot_name]],
					            plot_name
					          )
					          
					          row_plots[[length(row_plots) + 1]] = column(
					            width = ifelse(length(names(post_model_metrics_objs[[model_name]])) == 1, 12, 6),
					            h4(label),
					            plotOutput(plot_id, height = "400px")
					          )
					          
					          counter = counter + 1
					        }
					      }
					      
					      if (length(row_plots) > 0) {
					        ui_list[[length(ui_list) + 1]] = tagList(
					          model_section,
					          fluidRow(row_plots)
					        )
					      }
					    }
					    
					    do.call(tagList, ui_list)
					  } else {
					    NULL	
					  }
					})
					
					
					
					observe({
					  req(rv_training_results$post_model_metrics_objs)
					  
					  post_model_metrics_objs <- rv_training_results$post_model_metrics_objs
					  
					  for (model_name in names(post_model_metrics_objs)) {
					    
					    local({
					      my_model <- model_name
					      down_id  <- paste0("download_all_", my_model)
					      
					      output[[down_id]] <- downloadHandler(
					        filename = function() {
					          paste0(my_model, "_ALL_PLOTS_", Sys.Date(), ".zip")
					        },
					        content = function(file) {
					          
					          # temp directory to store PNGs
					          tmpdir <- tempdir()
					          owd <- setwd(tmpdir)
					          on.exit(setwd(owd))
					          
					          plot_files <- c()
					          
					          for (plot_name in names(post_model_metrics_objs[[my_model]])) {
					            p <- post_model_metrics_objs[[my_model]][[plot_name]]
					            
					            if (!is.null(p)) {
					              f <- paste0(my_model, "_", plot_name, ".png")
					              png(f, width = 1200, height = 900)
					              print(p)
					              dev.off()
					              plot_files <- c(plot_files, f)
									}
					          }
					          
					          # create ZIP
					          zip(zipfile = file, files = plot_files)
					        }
					      )
					    })
					  }
					})
					
					
					
					
					## SHAP values option
					output$model_training_caret_test_metrics_trained_shap_switch = renderUI({
						req(rv_training_models$all_trained_models)
						req(input$model_training_caret_test_metrics_trained_models_shap)
						if (isTRUE(!is.null(rv_training_models$all_trained_models))) {
							if (isTRUE(!is.null(input$model_training_caret_test_metrics_trained_models_shap)) & isTRUE(!any(input$model_training_caret_test_metrics_trained_models_shap %in% ""))) {
								prettySwitch("model_training_caret_test_metrics_trained_shap_switch_check"
									, get_rv_labels("model_training_caret_test_metrics_trained_shap_switch_check")
									, value = FALSE
									, status = "success"
								)
							} else {
								NULL
							}
						} else {
							NULL
						}
					})

					## Select trained models
					output$model_training_caret_test_metrics_trained_models_shap = renderUI({

						req(!is.null(rv_training_results$models))
						req(!is.null(rv_training_models$all_trained_models))
						req(!is.null(input$model_training_caret_more_options_shap_check))
						if (isTRUE(!is.null(rv_training_models$all_trained_models))) {
							temp_models = names(rv_training_models$all_trained_models)
							## FIXME: The names should align
							if (inherits(rv_training_results$models, "caretEnsemble")) {
								temp_models = c(temp_models, "ensemble")
								fixed_names = gsub("\\.", " ", temp_models)

							} else {
								fixed_names = gsub("\\.", " ", names(rv_training_results$models))
							}
							temp_models = temp_models[temp_models %in% fixed_names]
							temp_selected = temp_models
							temp_labs = get_rv_labels("model_training_caret_test_metrics_trained_models_shap_ph")
							if (isTRUE(input$model_training_caret_more_options_shap_check=="Select models")) {
								empty_lab = ""
								names(empty_lab) = temp_labs
								temp_models = c(empty_lab , temp_models)
								temp_labs = NULL
								temp_selected = NULL
							}
							selectInput("model_training_caret_test_metrics_trained_models_shap"
								, label = temp_labs
								, selected = temp_selected
								, choices = temp_models
								, multiple=TRUE
								, width="100%"
							)
						} else {
							temp_models = NULL
							NULL
						}
					})

	
					## Select metrics for trained model
					output$model_training_caret_test_metrics_trained_models_options = renderUI({
						req(!is.null(rv_training_results$post_model_metrics_objs))
						if (isTRUE(length(input$model_training_caret_more_options_shap_check)>0)) {
							if ( (isTRUE(input$model_training_caret_more_options_shap_check=="All")) | (isTRUE(!is.null(input$model_training_caret_test_metrics_trained_models_shap)) & isTRUE(any(input$model_training_caret_test_metrics_trained_models_shap!="")))) {
								rv_training_models$all_trained_models_metrics = tryCatch({
									Rautoml::get_metrics_names(rv_training_results$test_metrics_objs)
								}, error=function(e) {
									shinyalert::shinyalert("Error: ", paste0(get_rv_labels("test_metrics_objs_shap_error"), "\n", e$message), type = "error")
									return(NULL)
								})
								if (is.null(rv_training_models$all_trained_models_metrics)) return()
								temp_metrics = rv_training_models$all_trained_models_metrics
								empty_lab = ""
								names(empty_lab) = get_rv_labels("model_training_caret_test_metrics_trained_models_options_ph")
								selectInput("model_training_caret_test_metrics_trained_models_options"
									, label = get_rv_labels("model_training_caret_test_metrics_trained_models_options_ph")
									, choices = temp_metrics #c(empty_lab, temp_metrics)
									, selected = temp_metrics[[1]]
									, multiple=TRUE
									, width="100%"
								)
							} else {
								NULL
							}
						} else {
							NULL
						}
					})

					
					## Apply Metrics/SHAP values selection
					output$model_training_caret_test_metrics_trained_shap_apply_ui = renderUI({
						req(!is.null(rv_training_results$post_model_metrics_objs))
						req(!is.null(rv_training_models$all_trained_models))
						if (((isTRUE(input$model_training_caret_test_metrics_trained_models_options!="") | isTRUE(length(input$model_training_caret_test_metrics_trained_models_options)>0)) & isTRUE(!is.null(input$model_training_caret_test_metrics_trained_models_options))) | isTRUE(input$model_training_caret_test_metrics_trained_shap_switch_check)) {
							actionBttn("model_training_caret_test_metrics_trained_shap_apply"
								, inline=TRUE
								, block = FALSE
								, color = "success"
								, label = get_rv_labels("model_training_caret_test_metrics_trained_shap_apply")
							)
							
						} else {
							NULL
						}
					})

					observeEvent(input$model_training_caret_test_metrics_trained_shap_apply, {
						req(!is.null(rv_training_results$post_model_metrics_objs))
						req(!is.null(rv_training_models$all_trained_models))
						req(!is.null(rv_training_results$test_metrics_objs))
						req(!is.null(rv_training_models$all_trained_models))
						req(!is.null(input$model_training_caret_test_metrics_trained_models_shap))
						req(!is.null(input$model_training_caret_test_metrics_trained_models_options))
						req(isTRUE(length(input$model_training_caret_test_metrics_trained_models_options)>0))
						if (((isTRUE(input$model_training_caret_test_metrics_trained_models_options!="") | isTRUE(length(input$model_training_caret_test_metrics_trained_models_options)>0)) & isTRUE(!is.null(input$model_training_caret_test_metrics_trained_models_options))) | isTRUE(input$model_training_caret_test_metrics_trained_shap_switch_check)) {
							if (isTRUE(input$model_training_caret_test_metrics_trained_models_options!="") | isTRUE(length(input$model_training_caret_test_metrics_trained_models_options)>0)) {
								
								start_progress_bar(id="model_metrics_caret_pb", att_new_obj=model_metrics_caret_pb, text=get_rv_labels("model_metrics_apply_progress_bar"))
								
								rv_training_results$test_metrics_objs_filtered = tryCatch({
									Rautoml::extract_more_metrics(
										object=rv_training_results$test_metrics_objs
										, model_name=input$model_training_caret_test_metrics_trained_models_shap
										, metric_name=input$model_training_caret_test_metrics_trained_models_options
									)
								}, error=function(e) {
									shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
									return(NULL)
								})
								
								if (is.null(rv_training_results$test_metrics_objs_filtered)) return()
								
								## Save explore test performance metrics
								save_more = tryCatch({
									Rautoml::save_boot_estimates(boot_list=rv_training_results$test_metrics_objs_filtered
										, dataset_id=rv_ml_ai$dataset_id
										, session_name=rv_ml_ai$session_id
										, timestamp=Sys.time()
										, output_dir="outputs"
										, sub_dir="explore_trained_models"
									)
									invisible(TRUE)
								}, error=function(e) {
									shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
									return(NULL)
								})

								if (is.null(save_more)) return()
								
								if (NROW(rv_training_results$test_metrics_objs_filtered$all)) {
									test_plots_filtered = plot(rv_training_results$test_metrics_objs_filtered)
									
									## Save filtered plots
									save_more_plots = tryCatch({
										Rautoml::save_rautoml_plot(objects=test_plots_filtered
											, name="explore_performance_metrics"
											, dataset_id=rv_ml_ai$dataset_id
											, session_name=rv_ml_ai$session_id
											, timestamp=Sys.time()
											, output_dir="outputs"
											, metric_type="explore_trained_models"
										)
										invisible(TRUE)
									}, error=function(e) {
										shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
										return(NULL)
									})

									if (is.null(save_more_plots)) return()

									output$model_training_caret_test_metrics_plot_all_filtered = renderPlot({
										req(!is.null(test_plots_filtered$all))
										test_plots_filtered$all	
									})
									
									
									output$model_training_caret_test_metrics_plot_all_filtereddown <- downloadHandler(
									  filename = function(){
									    paste(
									      "test_plots_filtered_all",Sys.time(), ".png")
									  },
									  content = function(file){
									    req(!is.null(test_plots_filtered$all))
									    
									    ggsave(filename = file,
									     plot = test_plots_filtered$all, dpi = 300
									    )
									  },
									  contentType = "image/png"
									  
									)
									
									output$model_training_caret_test_metrics_plot_roc_filtered = renderPlot({
										test_plots_filtered$roc
									})
									
									
									output$model_training_caret_test_metrics_plot_roc_filtereddown <- downloadHandler(
									  filename = function(){
									    paste(
									      "test_plots_filtered_roc",Sys.time(), ".png")
									  },
									  content = function(file){
									    ggsave(filename = file,
									           plot = 	test_plots_filtered$roc, dpi = 300
									    )
									  },
									  contentType = "image/png"
									  
									)

									output$model_training_caret_test_metrics_plot_all_filtered_ui = renderUI({
										req(!is.null(test_plots_filtered$all))
										p(
											hr()
											, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_plot_all_filtered"), ":</b> <br/>"))
											, fluidRow(
												 column(width=12
													, plotOutput("model_training_caret_test_metrics_plot_all_filtered", height = "1000px"),
													br(),
													downloadBttn("model_training_caret_test_metrics_plot_all_filtereddown", label =get_rv_labels("downloadid"),color = "success" )
												)
											)
										)
									})

									output$model_training_caret_test_metrics_plot_roc_filtered_ui = renderUI({
										req(!is.null(test_plots_filtered$roc))
											p(
												hr()
												, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_plot_roc_filtered"), ":</b> <br/>"))
												, fluidRow(
													column(width=12
														, plotOutput("model_training_caret_test_metrics_plot_roc_filtered", height = "1000px"),
														br(),
														downloadBttn("model_training_caret_test_metrics_plot_roc_filtereddown", label =get_rv_labels("downloadid"),color = "success" )
													)
												)
											)
									})

									output$model_training_caret_test_metrics_df_filtered = DT::renderDT({
									  df = rv_training_results$test_metrics_objs_filtered$all

									  DT::datatable(
										 df,
										 escape = FALSE,
										 rownames = FALSE,
										 options = list(
											processing = FALSE,
											autoWidth = FALSE,
											scrollX = TRUE,
											columnDefs = list(
											  list(className = 'dt-center', targets = c("lower", "estimate", "upper"))
											)
										 )
									  ) %>%
										 DT::formatRound(columns = c("lower", "estimate", "upper"), digits = 4)
									})
									
									
									
									output$model_training_caret_test_metrics_df_filtereddown <- downloadHandler(
									  filename = function(){
									    paste(
									      "test_metrics_objs_filtered_all",Sys.time(), ".csv")
									  },
									  content = function(file){
									    write.csv(rv_training_results$test_metrics_objs_filtered$all, file = file,row.names = FALSE
									    )
									  },
									  contentType = "text/csv"
									  
									)

									output$model_training_caret_test_metrics_df_filtered_ui = renderUI({
										req(!is.null(rv_training_results$test_metrics_objs_filtered$all))
										p(
											br()
											, hr()
											, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_df_filtered"), ":</b> <br/>"))
											, fluidRow(
												column(width = 12
													, DT::DTOutput("model_training_caret_test_metrics_df_filtered", width="100%", fill=TRUE),
													br(),
													downloadBttn("model_training_caret_test_metrics_df_filtereddown", label =get_rv_labels("downloadid"),color = "success" )
												)
											)
										)
									})
									
									
								}
							} else {
								rv_training_results$test_metrics_objs_filtered = NULL
								output$model_training_caret_test_metrics_plot_all_filtered = NULL
								output$model_training_caret_test_metrics_plot_roc_filtered = NULL
								output$model_training_caret_test_metrics_df_filtered = NULL
								output$model_training_caret_test_metrics_df_filtered_ui = NULL
							}
						
							if (isTRUE(input$model_training_caret_test_metrics_trained_shap_switch_check)) {
								rv_training_results$test_metrics_objs_shap = tryCatch({
									Rautoml::compute_shap(
										models=rv_training_results$models
										, model_names=gsub("\\ ", ".", input$model_training_caret_test_metrics_trained_models_shap)
										, newdata=rv_ml_ai$preprocessed$test_df
										, response=rv_ml_ai$outcome
										, task=rv_ml_ai$task
										, nsim=50
										, max_n=1000
										, top_n_rank=5
										, total_n_rank=50
									)
								}, error = function(e) {
									shinyalert::shinyalert("Error: ", paste0(get_rv_labels("test_metrics_objs_shap_error"), "\n", e$message), type = "error")
									close_progress_bar(att_new_obj=model_metrics_caret_pb)
									return(NULL)
								})

								if (is.null(rv_training_results$test_metrics_objs_shap)) return()
								
								
								rv_training_results$shap_plots = tryCatch({
									plot(rv_training_results$test_metrics_objs_shap)
								}, error=function(e){
									shinyalert::shinyalert("Error: ", paste0(get_rv_labels("test_metrics_objs_shap_error"), "\n", e$message), type = "error")
									close_progress_bar(att_new_obj=model_metrics_caret_pb)
									return(NULL)
								})
							
								if (is.null(rv_training_results$shap_plots)) return()
								
								## Save SHAP objects and plots
								save_shap = tryCatch({
									Rautoml::save_boot_estimates(boot_list=rv_training_results$test_metrics_objs_shap
										, dataset_id=rv_ml_ai$dataset_id
										, session_name=rv_ml_ai$session_id
										, timestamp=Sys.time()
										, output_dir="outputs"
										, sub_dir="explore_trained_models"
									)
									invisible(TRUE)
								}, error=function(e) {
									shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
									return(NULL)
								})

								if (is.null(save_shap)) return()
									
								save_shap_plots = tryCatch({
									Rautoml::save_rautoml_plot(objects=rv_training_results$shap_plots
										, name="explore_performance_metrics"
										, dataset_id=rv_ml_ai$dataset_id
										, session_name=rv_ml_ai$session_id
										, timestamp=Sys.time()
										, output_dir="outputs"
										, metric_type="explore_trained_models"
									)
									invisible(TRUE)
								}, error=function(e) {
									shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
									return(NULL)
								})

								if (is.null(save_shap_plots)) return()
								
								## Variable importance
								output$model_training_caret_test_metrics_shap_values_varimp = renderPlot({
									rv_training_results$shap_plots$varimp
								})
								
								
								output$model_training_caret_test_metrics_shap_values_varimpdown <- downloadHandler(
								  filename = function(){
								    paste(
								      "shap_plots_varimportance",Sys.time(), ".png")
								  },
								  content = function(file){
								    ggsave(filename = file,
								           plot = 	rv_training_results$shap_plots$varimp, dpi = 300
								    )
								  },
								  contentType = "image/png"
								  
								)

								output$model_training_caret_test_metrics_shap_values_varimp_ui = renderUI({
									p(
										br()
										, hr()
										, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_varimp"), ":</b> <br/>"))
										, fluidRow(
											column(width=12
												, plotOutput("model_training_caret_test_metrics_shap_values_varimp", height = "1000px"),
												br(),
												downloadBttn("model_training_caret_test_metrics_shap_values_varimpdown", label =get_rv_labels("downloadid"),color = "success" )
											)
										)
									)
								})

								## Most frequent variables
								output$model_training_caret_test_metrics_shap_values_varfreq = renderPlot({
									rv_training_results$shap_plots$varfreq
								})
								
								
								output$model_training_caret_test_metrics_shap_values_varfreqdown <- downloadHandler(
								  filename = function(){
								    paste(
								      "test_metrics_shap_values_varfreq",Sys.time(), ".png")
								  },
								  content = function(file){
								    ggsave(filename = file,
								           plot = 	rv_training_results$shap_plots$varfreq, dpi = 300
								    )
								  },
								  contentType = "image/png"
								  
								)

								output$model_training_caret_test_metrics_shap_values_varfreq_ui = renderUI({
									p(
										hr()
										, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_varfreq"), ":</b> <br/>"))
										, fluidRow(
											column(width=12
												, plotOutput("model_training_caret_test_metrics_shap_values_varfreq", height = "1000px"),
												br(),
												downloadBttn("model_training_caret_test_metrics_shap_values_varfreqdown", label =get_rv_labels("downloadid"),color = "success" )
											)
										)
									)
								})

								## Variable dependency
								output$model_training_caret_test_metrics_shap_values_vardep = renderPlot({
									rv_training_results$shap_plots$vardep
								})
								
								
								output$model_training_caret_test_metrics_shap_values_vardepdown <- downloadHandler(
								  filename = function(){
								    paste(
								      "test_metrics_shap_values_vardepolyment",Sys.time(), ".png")
								  },
								  content = function(file){
								    ggsave(filename = file,
								           plot = 	rv_training_results$shap_plots$vardep, dpi = 300
								    )
								  },
								  contentType = "image/png"
								  
								)

								output$model_training_caret_test_metrics_shap_values_vardep_ui = renderUI({
									p(
										hr()
										, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_vardep"), ":</b> <br/>"))
										, fluidRow(
											column(width=12
												, plotOutput("model_training_caret_test_metrics_shap_values_vardep", height = "1000px"),
												br(),
												downloadBttn("model_training_caret_test_metrics_shap_values_vardepdown", label =get_rv_labels("downloadid"),color = "success" )
											)
										)
									)
								})
								
								## Beeswarm plot
								output$model_training_caret_test_metrics_shap_values_beeswarm = renderPlot({
									rv_training_results$shap_plots$beeswarm
								})
								
								output$model_training_caret_test_metrics_shap_values_beeswarmdown <- downloadHandler(
								  filename = function(){
								    paste(
								      "test_metrics_shap_values_beeswarm",Sys.time(), ".png")
								  },
								  content = function(file){
								    ggsave(filename = file,
								           plot = rv_training_results$shap_plots$beeswarm, dpi = 300
								    )
								  },
								  contentType = "image/png"
								  
								)

								## Waterfall plot
								output$model_training_caret_test_metrics_shap_values_waterfall = renderPlot({
									rv_training_results$shap_plots$waterfall
								})
								
								
								output$model_training_caret_test_metrics_shap_values_waterfalldown <- downloadHandler(
								  filename = function(){
								    paste(
								      "test_metrics_shap_values_waterfall",Sys.time(), ".png")
								  },
								  content = function(file){
								    ggsave(filename = file,
								           plot = rv_training_results$shap_plots$waterfall, dpi = 300
								    )
								  },
								  contentType = "image/png"
								  
								)

								## Force plots
								output$model_training_caret_test_metrics_shap_values_force = renderPlot({
									rv_training_results$shap_plots$force
								})
								
								
								output$model_training_caret_test_metrics_shap_values_forcedown <- downloadHandler(
								  filename = function(){
								    paste(
								      "test_metrics_shap_values_force",Sys.time(), ".png")
								  },
								  content = function(file){
								    ggsave(filename = file,
								           plot = rv_training_results$shap_plots$force, dpi = 300
								    )
								  },
								  contentType = "image/png"
								  
								)

								close_progress_bar(att_new_obj=model_metrics_caret_pb)
								
							} else {
								rv_training_results$test_metrics_objs_shap = NULL		
								output$model_training_caret_test_metrics_shap_values_varimp = NULL
								output$model_training_caret_test_metrics_shap_values_varfreq = NULL
								output$model_training_caret_test_metrics_shap_values_vardep = NULL
								output$model_training_caret_test_metrics_shap_values_beeswarm = NULL
								output$model_training_caret_test_metrics_shap_values_waterfall = NULL
								output$model_training_caret_test_metrics_shap_values_force = NULL
								
								output$model_training_caret_test_metrics_shap_values_varimp_ui = NULL
								output$model_training_caret_test_metrics_shap_values_varfreq_ui = NULL
								output$model_training_caret_test_metrics_shap_values_vardep_ui = NULL
								close_progress_bar(att_new_obj=model_metrics_caret_pb)
							}
						} else {
							rv_training_results$test_metrics_objs_filtered = NULL
							output$model_training_caret_test_metrics_plot_all_filtered = NULL
							output$model_training_caret_test_metrics_plot_roc_filtered = NULL
							output$model_training_caret_test_metrics_df_filtered = NULL
								
							rv_training_results$test_metrics_objs_shap = NULL		
							output$model_training_caret_test_metrics_shap_values_varimp = NULL
							output$model_training_caret_test_metrics_shap_values_varfreq = NULL
							output$model_training_caret_test_metrics_shap_values_vardep = NULL
							output$model_training_caret_test_metrics_shap_values_beeswarm = NULL
							output$model_training_caret_test_metrics_shap_values_waterfall = NULL
							output$model_training_caret_test_metrics_shap_values_force = NULL
							
							output$model_training_caret_test_metrics_shap_values_varimp_ui = NULL
							output$model_training_caret_test_metrics_shap_values_varfreq_ui = NULL
							output$model_training_caret_test_metrics_shap_values_vardep_ui = NULL
							close_progress_bar(att_new_obj=model_metrics_caret_pb)
						}
						
						## FIXME: Best way to reset SHAP values
						rv_training_results$test_metrics_objs_shap = NULL		
					})

				} else {
					output$model_training_caret_train_metrics_plot = NULL
					output$model_training_caret_train_metrics_df = NULL
						
					output$model_training_caret_test_metrics_plot_specifics = NULL
					output$model_training_caret_test_metrics_plot_all = NULL
					output$model_training_caret_test_metrics_plot_roc = NULL
					output$model_training_caret_test_metrics_df = NULL
						
					rv_training_models$all_trained_models = NULL
					output$model_training_caret_more_options_shap = NULL
					output$model_training_caret_test_metrics_trained_models_shap = NULL
					output$model_training_caret_test_metrics_trained_models_options = NULL
					output$model_training_caret_test_metrics_trained_shap_switch = NULL

					output$model_training_caret_test_metrics_trained_shap_apply_ui = NULL
					
					rv_training_results$test_metrics_objs_filtered = NULL
					output$model_training_caret_test_metrics_plot_all_filtered = NULL
					output$model_training_caret_test_metrics_plot_roc_filtered = NULL
					output$model_training_caret_test_metrics_df_filtered = NULL
							
					rv_training_results$test_metrics_objs_shap = NULL		
					output$model_training_caret_test_metrics_shap_values_varimp = NULL
					output$model_training_caret_test_metrics_shap_values_varfreq = NULL
					output$model_training_caret_test_metrics_shap_values_vardep = NULL
					output$model_training_caret_test_metrics_shap_values_beeswarm = NULL
					output$model_training_caret_test_metrics_shap_values_waterfall = NULL
					output$model_training_caret_test_metrics_shap_values_force = NULL
					
					output$model_training_caret_test_metrics_shap_values_varimp_ui = NULL
					output$model_training_caret_test_metrics_shap_values_varfreq_ui = NULL
					output$model_training_caret_test_metrics_shap_values_vardep_ui = NULL
					
					output$model_training_caret_train_tuned_parameters = NULL
					output$model_training_caret_train_training_control = NULL
					close_progress_bar(att_new_obj=model_metrics_caret_pb)
				}
			} else {
				output$model_training_caret_train_metrics_plot = NULL
				output$model_training_caret_train_metrics_df = NULL
					
				output$model_training_caret_test_metrics_plot_specifics = NULL
				output$model_training_caret_test_metrics_plot_all = NULL
				output$model_training_caret_test_metrics_plot_roc = NULL
				output$model_training_caret_test_metrics_df = NULL
					
				rv_training_models$all_trained_models = NULL
				output$model_training_caret_more_options_shap = NULL
				output$model_training_caret_test_metrics_trained_models_shap = NULL
				output$model_training_caret_test_metrics_trained_models_options = NULL
				output$model_training_caret_test_metrics_trained_shap_switch = NULL
					
				output$model_training_caret_test_metrics_trained_shap_apply_ui = NULL
				
				rv_training_results$test_metrics_objs_filtered = NULL
				output$model_training_caret_test_metrics_plot_all_filtered = NULL
				output$model_training_caret_test_metrics_plot_roc_filtered = NULL
				output$model_training_caret_test_metrics_df_filtered = NULL
					
				rv_training_results$test_metrics_objs_shap = NULL		
				output$model_training_caret_test_metrics_shap_values_varimp = NULL
				output$model_training_caret_test_metrics_shap_values_varfreq = NULL
				output$model_training_caret_test_metrics_shap_values_vardep = NULL
				output$model_training_caret_test_metrics_shap_values_beeswarm = NULL
				output$model_training_caret_test_metrics_shap_values_waterfall = NULL
				output$model_training_caret_test_metrics_shap_values_force = NULL
				
				output$model_training_caret_test_metrics_shap_values_varimp_ui = NULL
				output$model_training_caret_test_metrics_shap_values_varfreq_ui = NULL
				output$model_training_caret_test_metrics_shap_values_vardep_ui = NULL
				
				output$model_training_caret_train_tuned_parameters = NULL
				output$model_training_caret_train_training_control = NULL
				close_progress_bar(att_new_obj=model_metrics_caret_pb)
			}
		} else {
			output$model_training_caret_train_metrics_plot = NULL
			output$model_training_caret_train_metrics_df = NULL
				
			output$model_training_caret_test_metrics_plot_specifics = NULL
			output$model_training_caret_test_metrics_plot_all = NULL
			output$model_training_caret_test_metrics_plot_roc = NULL
			output$model_training_caret_test_metrics_df = NULL
				
			rv_training_models$all_trained_models = NULL
			output$model_training_caret_more_options_shap = NULL
			output$model_training_caret_test_metrics_trained_models_shap = NULL
			output$model_training_caret_test_metrics_trained_models_options = NULL
			output$model_training_caret_test_metrics_trained_shap_switch = NULL
			
			output$model_training_caret_test_metrics_trained_shap_apply_ui = NULL
					
			rv_training_results$test_metrics_objs_filtered = NULL
			output$model_training_caret_test_metrics_plot_all_filtered = NULL
			output$model_training_caret_test_metrics_plot_roc_filtered = NULL
			output$model_training_caret_test_metrics_df_filtered = NULL
				
			rv_training_results$test_metrics_objs_shap = NULL		
			output$model_training_caret_test_metrics_shap_values_varimp = NULL
			output$model_training_caret_test_metrics_shap_values_varfreq = NULL
			output$model_training_caret_test_metrics_shap_values_vardep = NULL
			output$model_training_caret_test_metrics_shap_values_beeswarm = NULL
			output$model_training_caret_test_metrics_shap_values_waterfall = NULL
			output$model_training_caret_test_metrics_shap_values_force = NULL
			
			output$model_training_caret_test_metrics_shap_values_varimp_ui = NULL
			output$model_training_caret_test_metrics_shap_values_varfreq_ui = NULL
			output$model_training_caret_test_metrics_shap_values_vardep_ui = NULL
						
			output$model_training_caret_train_tuned_parameters = NULL
			output$model_training_caret_train_training_control = NULL
			close_progress_bar(att_new_obj=model_metrics_caret_pb)
		}

	

		output$model_training_caret_train_metrics = renderUI({
			req(!is.null(rv_current$working_df))
			req(!is.null(rv_ml_ai$preprocessed))
			req(!is.null(rv_training_results$train_metrics_df))
			req(!is.null(rv_training_results$tuned_parameters))
			req(!is.null(rv_training_results$control_parameters))
			if (isTRUE(!is.null(rv_current$working_df))) {
				if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
					if (isTRUE(!is.null(rv_training_results$train_metrics_df))) {

							p(br()
								, hr()
								, HTML(paste0("<b>", get_rv_labels("model_training_caret_train_metrics"), ":</b> <br/>"))
								, tabsetPanel(
									tabPanel(get_rv_labels("model_training_caret_train_parameters_out")
										, p(
											br()
											, box(title = NULL 
												, status = "success",
												style = "max-height: 650px; overflow-y: auto;"
												, solidHeader = TRUE
												, collapsible = TRUE
												, collapsed = FALSE
												, width = 12
												, fluidRow(
													column(width = 6
														, HTML(paste0("<b>", get_rv_labels("model_training_caret_train_training_control"), "</b>"))
														, uiOutput("model_training_caret_train_training_control")
													)
													, column(width = 6
														, HTML(paste0("<b>", get_rv_labels("model_training_caret_train_tuned_parameters"), "</b>"))
														, uiOutput("model_training_caret_train_tuned_parameters")
													)
												)
											)
										)
									)
									
									, tabPanel(get_rv_labels("model_training_caret_train_metrics_training")
										, p(
											br()
											, box(title = NULL 
												, status = "success",
												style = "max-height: 650px; overflow-y: auto;"
												, solidHeader = TRUE
												, collapsible = TRUE
												, collapsed = FALSE
												, width = 12
												, fluidRow(
													column(width = 12
														, DT::DTOutput("model_training_caret_train_metrics_df", width="100%", fill=TRUE),
														br(),
														downloadBttn("model_training_caret_train_metrics_dfdown", label =get_rv_labels("downloadid"),color = "success" )
													)
												)
												, hr()
												, fluidRow(
													 column(width=12
														, plotOutput("model_training_caret_train_metrics_plot"),
														br(),
														downloadBttn("model_training_caret_train_metrics_plotdown", label =get_rv_labels("downloadid"),color = "success" )
													)
												)
											)
										)
									)
									, tabPanel(get_rv_labels("model_training_caret_train_metrics_test")
										, p(
											br()
											, box(title = NULL 
												, status = "success",
												style = "max-height: 650px; overflow-y: auto;"
												, solidHeader = TRUE
												, collapsible = TRUE
												, collapsed = TRUE
												, width = 12
												, fluidRow(
													column(width = 6
														, DT::DTOutput("model_training_caret_test_metrics_df", width="100%", fill=TRUE),
														br(),
														downloadBttn("model_training_caret_test_metrics_dfdown", label =get_rv_labels("downloadid"),color = "success" ))
													
													 , column(width=6
														, plotOutput("model_training_caret_test_metrics_plot_specifics", height = "400px"),
														downloadBttn("model_training_caret_test_metrics_plot_specificsdown", label =get_rv_labels("downloadid"),color = "success" )
													)
												)
												, hr()
												, fluidRow(
													 column(width=12
														, plotOutput("model_training_caret_test_metrics_plot_all", height = "400px"),
														br(),
														downloadBttn("model_training_caret_test_metrics_plot_alldown", label =get_rv_labels("downloadid"),color = "success" )
													)
												)
												, hr()
												, fluidRow(
													column(width=12
														, plotOutput("model_training_caret_test_metrics_plot_roc", height = "400px"),
														br(),
														downloadBttn("model_training_caret_test_metrics_plot_rocdown", label =get_rv_labels("downloadid"),color = "success" )
													)
												)
											)
										)
									)
									, tabPanel(get_rv_labels("model_training_caret_more_metrics")
										, p(
											br()
											, box(title = NULL
												, status = "success",
												style = "max-height: 650px; overflow-y: auto;"
												, solidHeader = TRUE
												, collapsible = TRUE
												, collapsed = TRUE
												, width = 12
												, uiOutput("model_training_caret_post_model_metrics")
											)
										)
									)
									, tabPanel(get_rv_labels("model_training_caret_more_options_shap")
										, p(
											br()
											, box(title = NULL 
												, status = "success"
												, style = "max-height: 650px; overflow-y: auto;"
												, solidHeader = TRUE
												, collapsible = TRUE
												, collapsed = FALSE
												, width = 12
												, fluidRow(
													 column(width=6
														, uiOutput("model_training_caret_more_options_shap")
														, uiOutput("model_training_caret_test_metrics_trained_models_shap")
														, uiOutput("model_training_caret_test_metrics_trained_models_options")
														, br()
														, uiOutput("model_training_caret_test_metrics_trained_shap_apply_ui")
													)
													, column(width=6
														, uiOutput("model_training_caret_test_metrics_trained_shap_switch")
													)
												)
												, uiOutput("model_training_caret_test_metrics_df_filtered_ui")
												, fluidRow(
												  column(width = 6, uiOutput("model_training_caret_test_metrics_plot_all_filtered_ui")),
												  column(width = 6, uiOutput("model_training_caret_test_metrics_plot_roc_filtered_ui"))
												)
												, uiOutput("model_training_caret_test_metrics_shap_values_varimp_ui")
												, uiOutput("model_training_caret_test_metrics_shap_values_varfreq_ui")
												, uiOutput("model_training_caret_test_metrics_shap_values_vardep_ui")
												, uiOutput("model_training_caret_train_metrics_shap_values")
											)
										)
									)

									, tabPanel(get_rv_labels("model_training_caret_metrics_download_all")
										, p(
											br()
											, box(title=NULL 
												, status = "success"
												, style = "max-height: 650px; overflow-y: auto;"
												, solidHeader = TRUE
												, collapsible = TRUE
												, collapsed = FALSE
												, width = 12
												, fluidRow(
													 column(width=6
														, uiOutput("model_training_caret_metrics_download_all")
													)
												)
											)
										)
									)


								)
							)

					}
				}
			}
		})
		
		
		output$model_training_caret_train_metrics_shap_values = renderUI({
			req(!is.null(rv_current$working_df))
			req(!is.null(rv_ml_ai$preprocessed))
			req(!is.null(rv_training_results$shap_plots))
			req(isTRUE(input$model_training_caret_test_metrics_trained_shap_switch_check))
			box(title = get_rv_labels("more_model_plots")
			      , status = "success",
			      style = "max-height: 700px; overflow-y: auto;"
			      , solidHeader = TRUE
			      , collapsible = TRUE
			      , collapsed = TRUE
			      , width = 12,

				hr()
				, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_beeswarm"), ":</b> <br/>"))
				, fluidRow(
					column(width=12
						, plotOutput("model_training_caret_test_metrics_shap_values_beeswarm", height = "1000px"),
						br(),
						downloadBttn("model_training_caret_test_metrics_shap_values_beeswarmdown", label =get_rv_labels("downloadid"),color = "success" )
					)
				)
				
				, hr()
				, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_waterfall"), ":</b> <br/>"))
				, fluidRow(
					column(width=12
						, plotOutput("model_training_caret_test_metrics_shap_values_waterfall", height = "1000px"),
						br(),
						downloadBttn("model_training_caret_test_metrics_shap_values_waterfalldown", label =get_rv_labels("downloadid"),color = "success" )
					)
				)

				, hr()
				, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_force"), ":</b> <br/>"))
				, fluidRow(
					column(width=12
						, plotOutput("model_training_caret_test_metrics_shap_values_force", height = "1000px"),
						br(),
						downloadBttn("model_training_caret_test_metrics_shap_values_forcedown", label =get_rv_labels("downloadid"),color = "success" )
					)
				)
			)
			
		})


	})
 

}
