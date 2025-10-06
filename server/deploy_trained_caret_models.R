#### ---- Deploy trained model ------------------------------------- ####

deploy_trained_caret_models = function() {
	observeEvent(c(input$manage_data_apply, input$model_training_apply), {
		req(!is.null(rv_current$working_df))
		req(!is.null(rv_current$dataset_id))
		req(isTRUE(Rautoml::check_logs(path=".log_files", pattern="-trained.model.main.log")))
		rv_deploy_models$trained_models_table = Rautoml::collect_logs(path=".log_files", pattern="-trained.model.main.log")
		check_trained_data = Rautoml::check_value_exists(
			df = rv_deploy_models$trained_models_table
			, var = "dataset_id"
			, what = rv_current$dataset_id 
		)

		## Session name
		output$deploy_trained_caret_models_select_session = renderUI({
			req(isTRUE(check_trained_data))
			selectInput("deploy_trained_caret_models_select_session"
				, label = "Select session"
				, choices = Rautoml::extract_value_labels(rv_deploy_models$trained_models_table, "session_name")
				, multiple = TRUE
				, width = NULL
			)	
		})	
		
		## Model metrics
		output$deploy_trained_caret_models_select_metrics = renderUI({
			req(isTRUE(check_trained_data))
			selectInput("deploy_trained_caret_models_select_metrics"
				, label = "Select metric"
				, choices = Rautoml::extract_value_labels(rv_deploy_models$trained_models_table, "metric")
				, width = NULL
			)	
		})	
	  
		output$deploy_trained_caret_models_table = DT::renderDT({
			req(input$deploy_trained_caret_models_select_session)
			req(input$deploy_trained_caret_models_select_metrics)
			req(!is.null(rv_deploy_models$trained_models_table))
			df = Rautoml::filter_session_metric(rv_deploy_models$trained_models_table
				, metric_name=input$deploy_trained_caret_models_select_metrics
				, session_name_=input$deploy_trained_caret_models_select_session
			)
			DT::datatable(
				df |> dplyr::arrange(dplyr::desc(estimate)),
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
			 ) %>% DT::formatRound(columns = c("lower", "estimate", "upper"), digits = 4)
		})
		
		## Output objects
		output$deploy_trained_caret_models_box_ui = renderUI({
			p(
				box(title = "Model deployment" 
					, status = "success"
					, solidHeader = TRUE
					, collapsible = TRUE
					, collapsed = FALSE
					, width = 12
					, fluidRow(
						column(width = 6
							, uiOutput("deploy_trained_caret_models_select_session")
						)
						, column(width = 6
							, uiOutput("deploy_trained_caret_models_select_metrics")
						)
						, br()
						, hr()
						, column(width=12
							, DT::DTOutput("deploy_trained_caret_models_table")
						)
					)
				)
			)	
		})
		
	})
}

