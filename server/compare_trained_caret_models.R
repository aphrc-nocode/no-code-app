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
					} else {
						output$model_training_caret_test_metrics_plot_specifics = NULL
						output$model_training_caret_test_metrics_plot_all = NULL
						output$model_training_caret_test_metrics_plot_roc = NULL
						output$model_training_caret_test_metrics_df = NULL
					}

				} else {
					output$model_training_caret_train_metrics_plot = NULL
					output$model_training_caret_train_metrics_df = NULL
						
					output$model_training_caret_test_metrics_plot_specifics = NULL
					output$model_training_caret_test_metrics_plot_all = NULL
					output$model_training_caret_test_metrics_plot_roc = NULL
					output$model_training_caret_test_metrics_df = NULL
				}
			} else {
				output$model_training_caret_train_metrics_plot = NULL
				output$model_training_caret_train_metrics_df = NULL
					
				output$model_training_caret_test_metrics_plot_specifics = NULL
				output$model_training_caret_test_metrics_plot_all = NULL
				output$model_training_caret_test_metrics_plot_roc = NULL
				output$model_training_caret_test_metrics_df = NULL
			}
		} else {
			output$model_training_caret_train_metrics_plot = NULL
			output$model_training_caret_train_metrics_df = NULL
				
			output$model_training_caret_test_metrics_plot_specifics = NULL
			output$model_training_caret_test_metrics_plot_all = NULL
			output$model_training_caret_test_metrics_plot_roc = NULL
			output$model_training_caret_test_metrics_df = NULL
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
											, collapsed = FALSE
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
							)
						)



				}
			}
		}
	})
	 
}
