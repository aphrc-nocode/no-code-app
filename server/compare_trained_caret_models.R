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
						test_plots = plot(rv_training_results$test_metrics_objs)
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
						
						rv_training_models$all_trained_models = get_rv_objects(pattern="_trained_model$", rv_training_models)
		  
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
						if (isTRUE(!is.null(rv_training_models$all_trained_models))) {
							if (isTRUE(length(input$model_training_caret_more_options_shap_check)>0)) {
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
							if (isTRUE(input$model_training_caret_more_options_shap_check=="Select models")) {
								temp_models = rv_training_models$all_trained_models
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
								NULL
							}
						} else {
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


## 				## Apply Metrics/SHAP values selection
## 				observe({
## 					output$model_training_caret_test_metrics_trained_shap_apply_ui = renderUI({
## 						if (isTRUE(length(input$model_training_caret_more_options_shap_check)>0)) {
## 							req(input$model_training_caret_test_metrics_trained_models_options)
## 							if ((isTRUE(input$model_training_caret_more_options_shap_check=="Select models") & isTRUE(!is.null(input$model_training_caret_test_metrics_trained_models_shap)) & isTRUE(any(input$model_training_caret_test_metrics_trained_models_shap!=""))) | (isTRUE(!is.null(input$model_training_caret_test_metrics_trained_models_options)) & (isTRUE(any(input$model_training_caret_test_metrics_trained_models_options!=""))))) {
## 								actionBttn("model_training_caret_test_metrics_trained_shap_apply"
## 									, inline=TRUE
## 									, block = FALSE
## 									, color = "success"
## 									, label = get_rv_labels("model_training_caret_test_metrics_trained_shap_apply")
## 								)
## 							} else {
## 								NULL
## 							}
## 						} else {
## 							NULL
## 						}
## 					})
## 				})

					if (isTRUE(length(input$model_training_caret_more_options_shap_check)>0)) {
						 if ((isTRUE(is.null(input$model_training_caret_test_metrics_trained_models_shap)) |  isTRUE(any(input$model_training_caret_test_metrics_trained_models_shap==""))) | (isTRUE(is.null(input$model_training_caret_test_metrics_trained_models_options)) | isTRUE(any(input$model_training_caret_test_metrics_trained_models_options=="")))) {
							output$model_training_caret_test_metrics_trained_shap_apply_ui = NULL
						 } else {
						 	output$model_training_caret_test_metrics_trained_shap_apply_ui = renderUI({
								actionBttn("model_training_caret_test_metrics_trained_shap_apply"
									, inline=TRUE
									, block = FALSE
									, color = "success"
									, label = get_rv_labels("model_training_caret_test_metrics_trained_shap_apply")
								)
							})
						 }
					} else {
						output$model_training_caret_test_metrics_trained_shap_apply_ui = NULL
					}
				
## 				## Apply Metrics/SHAP values selection
## 				observe({
## 					output$model_training_caret_test_metrics_trained_shap_apply_ui = renderUI({
## 						if (isTRUE(length(input$model_training_caret_more_options_shap_check)>0)) {
## 							req(input$model_training_caret_test_metrics_trained_models_options)
## 							if ((isTRUE(input$model_training_caret_more_options_shap_check=="Select models") & isTRUE(!is.null(input$model_training_caret_test_metrics_trained_models_shap)) & isTRUE(any(input$model_training_caret_test_metrics_trained_models_shap!=""))) | (isTRUE(!is.null(input$model_training_caret_test_metrics_trained_models_options)) & (isTRUE(any(input$model_training_caret_test_metrics_trained_models_options!=""))))) {
## 								actionBttn("model_training_caret_test_metrics_trained_shap_apply"
## 									, inline=TRUE
## 									, block = FALSE
## 									, color = "success"
## 									, label = get_rv_labels("model_training_caret_test_metrics_trained_shap_apply")
## 								)
## 							} else {
## 								NULL
## 							}
## 						} else {
## 							NULL
## 						}
## 					})
## 				})

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
													, plotOutput("model_training_caret_test_metrics_plot_specifics")
												)
											)
											, hr()
											, fluidRow(
												 column(width=12
													, plotOutput("model_training_caret_test_metrics_plot_all")
												)
											)
											, hr()
											, fluidRow(
												column(width=12
													, plotOutput("model_training_caret_test_metrics_plot_roc")
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
										)
									)
								)


							)
						)

				}
			}
		}
	})


	 
}
