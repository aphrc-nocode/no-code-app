#### ---- Compare model metrics ----------------------------------- ####
model_training_caret_train_metrics_server = function() {
	
	
	observeEvent(input$model_training_apply, {
		
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(!is.null(rv_training_results$train_metrics_df))) {

					## Training data
					output$model_training_caret_train_metrics_plot = renderPlot({
						plot(rv_training_results$train_metrics_df)	
					})

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
					
					if (isTRUE(!is.null(rv_training_results$test_metrics_objs))) {
						## Test data
						test_plots = tryCatch({
							plot(rv_training_results$test_metrics_objs)
						}, error = function(e) {
							shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
							return(NULL)
						})
					
						if (is.null(test_plots)) return()

						output$model_training_caret_test_metrics_plot_specifics = renderPlot({
							test_plots$specifics
						})
						
						output$model_training_caret_test_metrics_plot_all = renderPlot({
							test_plots$all	
						})

						output$model_training_caret_test_metrics_plot_roc = renderPlot({
							test_plots$roc
						})
						
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
						
						rv_training_models$all_trained_models = Rautoml::get_rv_objects(pattern="_trained_model$", rv_training_models)
		  
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
					
					} else {
						output$model_training_caret_test_metrics_plot_specifics = NULL
						output$model_training_caret_test_metrics_plot_all = NULL
						output$model_training_caret_test_metrics_plot_roc = NULL
						output$model_training_caret_test_metrics_df = NULL
						
						rv_training_models$all_trained_models = NULL
						output$model_training_caret_more_options_shap = NULL
					}

					output$model_training_caret_post_model_metrics = renderUI({
						req(rv_training_results$post_model_metrics_objs)
						if (isTRUE(!is.null(rv_training_results$post_model_metrics_objs))) {
						 post_model_metrics_objs = rv_training_results$post_model_metrics_objs
						 
						 ui_list = list()
						 counter = 1
						 plot_labels = list(
							cm_plot = get_rv_labels("model_training_caret_post_model_metrics_cm")
							, var_imp_plot = get_rv_labels("model_training_caret_post_model_metrics_vi")
						 )
						 
						 for (model_name in names(post_model_metrics_objs)) {
							row_plots = list()
							
							model_section = list(
							  h3(model_name, style = "margin-top:30px; color:#2c3e50;")
							)
							
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
						if (isTRUE(!is.null(rv_training_models$all_trained_models))) {
							temp_models = rv_training_models$all_trained_models
							if (isTRUE(input$model_training_caret_more_options_shap_check=="Select models")) {
								empty_lab = ""
								names(empty_lab) = get_rv_labels("model_training_caret_test_metrics_trained_models_shap_ph")
								selectInput("model_training_caret_test_metrics_trained_models_shap"
									, label = NULL
									, selected = NULL
									, choices = c(empty_lab , temp_models)
									, multiple=TRUE
									, width="100%"
								)
							} else {
								selectInput("model_training_caret_test_metrics_trained_models_shap"
									, label = NULL
									, selected = temp_models
									, choices = temp_models
									, multiple=TRUE
									, width="100%"
								)
							}
						} else {
							temp_models = NULL
							NULL
						}
					})

	
					## Select metrics for trained model
					output$model_training_caret_test_metrics_trained_models_options = renderUI({
						req(rv_training_results$post_model_metrics_objs)
						if (isTRUE(length(input$model_training_caret_more_options_shap_check)>0)) {
							if ( (isTRUE(input$model_training_caret_more_options_shap_check=="All")) | (isTRUE(!is.null(input$model_training_caret_test_metrics_trained_models_shap)) & isTRUE(any(input$model_training_caret_test_metrics_trained_models_shap!="")))) {
								rv_training_models$all_trained_models_metrics = Rautoml::get_metrics_names(rv_training_results$test_metrics_objs)
								temp_metrics = rv_training_models$all_trained_models_metrics
								empty_lab = ""
								names(empty_lab) = get_rv_labels("model_training_caret_test_metrics_trained_models_options_ph")
								selectInput("model_training_caret_test_metrics_trained_models_options"
									, label = NULL
									, selected = NULL
									, choices = c(empty_lab , temp_metrics)
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
						req(rv_training_results$post_model_metrics_objs)
						req(rv_training_models$all_trained_models)
						req(input$model_training_caret_test_metrics_trained_models_shap)
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
						req(rv_training_results$test_metrics_objs)
						req(rv_training_results$post_model_metrics_objs)
						req(rv_training_models$all_trained_models)
						req(input$model_training_caret_test_metrics_trained_models_shap)
						req(input$model_training_caret_test_metrics_trained_models_options)
						if (((isTRUE(input$model_training_caret_test_metrics_trained_models_options!="") | isTRUE(length(input$model_training_caret_test_metrics_trained_models_options)>0)) & isTRUE(!is.null(input$model_training_caret_test_metrics_trained_models_options))) | isTRUE(input$model_training_caret_test_metrics_trained_shap_switch_check)) {
							if (isTRUE(input$model_training_caret_test_metrics_trained_models_options!="") | isTRUE(length(input$model_training_caret_test_metrics_trained_models_options)>0)) {
								rv_training_results$test_metrics_objs_filtered = Rautoml::extract_more_metrics(
									object=rv_training_results$test_metrics_objs
									, model_name=input$model_training_caret_test_metrics_trained_models_shap
									, metric_name=input$model_training_caret_test_metrics_trained_models_options
								)
								test_plots_filtered = plot(rv_training_results$test_metrics_objs_filtered)
								output$model_training_caret_test_metrics_plot_all_filtered = renderPlot({
									test_plots_filtered$all	
								})
								
								output$model_training_caret_test_metrics_plot_roc_filtered = renderPlot({
									test_plots_filtered$roc
								})

								output$model_training_caret_test_metrics_plot_all_filtered_ui = renderUI({
									req(!is.null(test_plots_filtered$all))
									p(
										hr()
										, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_plot_all_filtered"), ":</b> <br/>"))
										, fluidRow(
											 column(width=12
												, plotOutput("model_training_caret_test_metrics_plot_all_filtered", height = "1000px")
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
													, plotOutput("model_training_caret_test_metrics_plot_roc_filtered", height = "1000px")
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

								output$model_training_caret_test_metrics_df_filtered_ui = renderUI({
									req(!is.null(rv_training_results$test_metrics_objs_filtered$all))
									p(
										br()
										, hr()
										, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_df_filtered"), ":</b> <br/>"))
										, fluidRow(
											column(width = 12
												, DT::DTOutput("model_training_caret_test_metrics_df_filtered", width="100%", fill=TRUE)
											)
										)
									)
								})

							} else {
								rv_training_results$test_metrics_objs_filtered = NULL
								output$model_training_caret_test_metrics_plot_all_filtered = NULL
								output$model_training_caret_test_metrics_plot_roc_filtered = NULL
								output$model_training_caret_test_metrics_df_filtered = NULL
								output$model_training_caret_test_metrics_df_filtered_ui = NULL
							}
						
							if (isTRUE(input$model_training_caret_test_metrics_trained_shap_switch_check)) {
								start_progress_bar(id="model_metrics_caret_pb", att_new_obj=model_metrics_caret_pb, text=get_rv_labels("model_metrics_apply_progress_bar"))
								rv_training_results$test_metrics_objs_shap = tryCatch({
									Rautoml::compute_shap(
										models=rv_training_results$models
										, model_names=input$model_training_caret_test_metrics_trained_models_shap
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

								close_progress_bar(att_new_obj=model_metrics_caret_pb)
								
								## Variable importance
								output$model_training_caret_test_metrics_shap_values_varimp = renderPlot({
									rv_training_results$shap_plots$varimp
								})

								output$model_training_caret_test_metrics_shap_values_varimp_ui = renderUI({
									p(
										br()
										, hr()
										, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_varimp"), ":</b> <br/>"))
										, fluidRow(
											column(width=12
												, plotOutput("model_training_caret_test_metrics_shap_values_varimp", height = "1000px")
											)
										)
									)
								})

								## Most frequent variables
								output$model_training_caret_test_metrics_shap_values_varfreq = renderPlot({
									rv_training_results$shap_plots$varfreq
								})

								output$model_training_caret_test_metrics_shap_values_varfreq_ui = renderUI({
									p(
										hr()
										, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_varfreq"), ":</b> <br/>"))
										, fluidRow(
											column(width=12
												, plotOutput("model_training_caret_test_metrics_shap_values_varfreq", height = "1000px")
											)
										)
									)
								})

								## Variable dependency
								output$model_training_caret_test_metrics_shap_values_vardep = renderPlot({
									rv_training_results$shap_plots$vardep
								})

								output$model_training_caret_test_metrics_shap_values_vardep_ui = renderUI({
									p(
										hr()
										, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_vardep"), ":</b> <br/>"))
										, fluidRow(
											column(width=12
												, plotOutput("model_training_caret_test_metrics_shap_values_vardep", height = "1000px")
											)
										)
									)
								})
								
								## Beeswarm plot
								output$model_training_caret_test_metrics_shap_values_beeswarm = renderPlot({
									rv_training_results$shap_plots$beeswarm
								})

								## Waterfall plot
								output$model_training_caret_test_metrics_shap_values_waterfall = renderPlot({
									rv_training_results$shap_plots$waterfall
								})

								## Force plots
								output$model_training_caret_test_metrics_shap_values_force = renderPlot({
									rv_training_results$shap_plots$force
								})

								
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
								output$model_training_caret_test_metrics_shap_values_beeswarm_ui = NULL
								output$model_training_caret_test_metrics_shap_values_waterfall_ui = NULL
								output$model_training_caret_test_metrics_shap_values_force_ui = NULL
								
								output$model_training_caret_train_metrics_shap_values = NULL
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
							output$model_training_caret_test_metrics_shap_values_beeswarm_ui = NULL
							output$model_training_caret_test_metrics_shap_values_waterfall_ui = NULL
							output$model_training_caret_test_metrics_shap_values_force_ui = NULL
						
							output$model_training_caret_train_metrics_shap_values = NULL
						}
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
					output$model_training_caret_test_metrics_shap_values_beeswarm_ui = NULL
					output$model_training_caret_test_metrics_shap_values_waterfall_ui = NULL
					output$model_training_caret_test_metrics_shap_values_force_ui = NULL
					
					output$model_training_caret_train_metrics_shap_values = NULL
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
				output$model_training_caret_test_metrics_shap_values_beeswarm_ui = NULL
				output$model_training_caret_test_metrics_shap_values_waterfall_ui = NULL
				output$model_training_caret_test_metrics_shap_values_force_ui = NULL
				
				output$model_training_caret_train_metrics_shap_values = NULL
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
			output$model_training_caret_test_metrics_shap_values_beeswarm_ui = NULL
			output$model_training_caret_test_metrics_shap_values_waterfall_ui = NULL
			output$model_training_caret_test_metrics_shap_values_force_ui = NULL
			output$model_training_caret_train_metrics_shap_values = NULL
		}

	})


	output$model_training_caret_train_metrics = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(!is.null(rv_training_results$train_metrics_df))) {

						p(br()
							, hr()
							, HTML(paste0("<b>", get_rv_labels("model_training_caret_train_metrics"), ":</b> <br/>"))
							, tabsetPanel(
								tabPanel(get_rv_labels("model_training_caret_train_metrics_training")
									, p(
										br()
										, box(title = NULL 
											, status = "success"
											, solidHeader = TRUE
											, collapsible = TRUE
											, collapsed = FALSE
											, width = 12
											, fluidRow(
												column(width = 12
													, DT::DTOutput("model_training_caret_train_metrics_df", width="100%", fill=TRUE)
												)
											)
											, hr()
											, fluidRow(
												 column(width=12
													, plotOutput("model_training_caret_train_metrics_plot")
												)
											)
										)
									)
								)
								, tabPanel(get_rv_labels("model_training_caret_train_metrics_test")
									, p(
										br()
										, box(title = NULL 
											, status = "success"
											, solidHeader = TRUE
											, collapsible = TRUE
											, collapsed = TRUE
											, width = 12
											, fluidRow(
												column(width = 6
													, DT::DTOutput("model_training_caret_test_metrics_df", width="100%", fill=TRUE)
												)
												 , column(width=6
													, plotOutput("model_training_caret_test_metrics_plot_specifics", height = "400px")
												)
											)
											, hr()
											, fluidRow(
												 column(width=12
													, plotOutput("model_training_caret_test_metrics_plot_all", height = "400px")
												)
											)
											, hr()
											, fluidRow(
												column(width=12
													, plotOutput("model_training_caret_test_metrics_plot_roc", height = "400px")
												)
											)
										)
									)
								)
								, tabPanel(get_rv_labels("model_training_caret_more_metrics")
									, p(
										br()
										, box(title = NULL
											, status = "success"
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
											, uiOutput("model_training_caret_test_metrics_shap_values_vardep_ui")
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
		p(

			hr()
			, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_beeswarm"), ":</b> <br/>"))
			, fluidRow(
				column(width=12
					, plotOutput("model_training_caret_test_metrics_shap_values_beeswarm", height = "1000px")
				)
			)
			
			, hr()
			, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_waterfall"), ":</b> <br/>"))
			, fluidRow(
				column(width=12
					, plotOutput("model_training_caret_test_metrics_shap_values_waterfall", height = "1000px")
				)
			)

			, hr()
			, HTML(paste0("<b>", get_rv_labels("model_training_caret_test_metrics_shap_values_force"), ":</b> <br/>"))
			, fluidRow(
				column(width=12
					, plotOutput("model_training_caret_test_metrics_shap_values_force", height = "1000px")
				)
			)
		)
		
	})
	 
}
