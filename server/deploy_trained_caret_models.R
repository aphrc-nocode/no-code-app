#### ---- Deploy trained model ------------------------------------- ####

deploy_trained_caret_models = function() {
	observeEvent(c(input$manage_data_apply, input$model_training_apply), {
		req(!is.null(rv_current$working_df))
		req(!is.null(rv_current$dataset_id))
		req(isTRUE(Rautoml::check_logs(path=".log_files", pattern="-trained.model.main.log")))
		rv_deploy_models$trained_models_table = isolate(Rautoml::collect_logs(path=".log_files", pattern="-trained.model.main.log"))
		rv_deploy_models$trained_models_table = Rautoml::filter_current_data(rv_deploy_models$trained_models_table, rv_current$dataset_id)
		check_trained_data = Rautoml::check_value_exists(
			df = rv_deploy_models$trained_models_table
			, var = "dataset_id"
			, what = rv_current$dataset_id 
		)
		
		## Session name
		output$deploy_trained_caret_models_select_session = renderUI({
			req(isTRUE(check_trained_data))
			req(NROW(rv_deploy_models$trained_models_table)>0)
			temp_session_labels = Rautoml::extract_value_labels(rv_deploy_models$trained_models_table, "session_name")
			n_selected = min(length(temp_session_labels), 2)
			selectInput("deploy_trained_caret_models_select_session"
				, label = get_rv_labels("deploy_trained_caret_models_select_session")
				, choices = temp_session_labels 
				, selected = temp_session_labels[1:n_selected]
				, multiple = TRUE
				, width = NULL
			)	
		})	
		
		## Model metrics
		output$deploy_trained_caret_models_select_metrics = renderUI({
			req(isTRUE(check_trained_data))
			req(NROW(rv_deploy_models$trained_models_table)>0)
			selectInput("deploy_trained_caret_models_select_metrics"
				, label = get_rv_labels("deploy_trained_caret_models_select_metrics")
				, choices = Rautoml::extract_value_labels(rv_deploy_models$trained_models_table, "metric")
				, width = NULL
			)	
		})	
		
		## Models in summary table
		output$deploy_trained_caret_models_select_model = renderUI({
			req(isTRUE(check_trained_data))
			req(NROW(rv_deploy_models$trained_models_table)>0)
			temp_model_labels = Rautoml::extract_value_labels(rv_deploy_models$trained_models_table, "model")
			selectInput("deploy_trained_caret_models_select_model"
				, label = get_rv_labels("deploy_trained_caret_models_select_model")
				, choices = temp_model_labels 
				, selected = temp_model_labels
				, multiple = TRUE
				, width = NULL
			)	
		})	
	  
		output$deploy_trained_caret_models_table = DT::renderDT({
			req(input$deploy_trained_caret_models_select_session)
			req(input$deploy_trained_caret_models_select_metrics)
			req(input$deploy_trained_caret_models_select_model)
			req(!is.null(rv_deploy_models$trained_models_table))
			req(NROW(rv_deploy_models$trained_models_table)>0)
			df = Rautoml::filter_session_metric(rv_deploy_models$trained_models_table
				, metric_name=input$deploy_trained_caret_models_select_metrics
				, session_name_=input$deploy_trained_caret_models_select_session
				, model_name=input$deploy_trained_caret_models_select_model
			)
			rv_deploy_models$trained_models_table_filtered = df
			DT::datatable(
				df |> dplyr::arrange(dplyr::desc(estimate)),
				escape = FALSE,
				rownames = FALSE,
				selection = "multiple",
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
		
		## Models to deploy
		output$deploy_trained_caret_models_select_deploy = renderUI({
			req(!is.null(input$deploy_trained_caret_models_select_session))
			req(!is.null(input$deploy_trained_caret_models_select_metrics))
			req(!is.null(input$deploy_trained_caret_models_select_model))
			req(!is.null(input$deploy_trained_caret_models_table_rows_selected))
			req(NROW(rv_deploy_models$trained_models_table)>0)
			selected_rows = input$deploy_trained_caret_models_table_rows_selected
			temp_model_labels = rv_deploy_models$trained_models_table_filtered$model_id[selected_rows]
			selectInput("deploy_trained_caret_models_select_deploy"
				, label = get_rv_labels("deploy_trained_caret_models_select_deploy")
				, choices = temp_model_labels 
				, selected = temp_model_labels
				, multiple = TRUE
				, width = NULL
			)	
		})	
		
		## Apply deployment
		output$deploy_trained_caret_models_hostname = renderUI({
			req(input$deploy_trained_caret_models_select_session)
			req(input$deploy_trained_caret_models_select_metrics)
			req(input$deploy_trained_caret_models_select_model)
			req(input$deploy_trained_caret_models_table_rows_selected)
			req(NROW(rv_deploy_models$trained_models_table)>0)
			req(input$deploy_trained_caret_models_select_deploy)
			textInput("deploy_trained_caret_models_hostname"
				, "Enter Hostname or IP:"
				, value = "127.0.0.1"
			)
		})
	
		## Apply deployment
		output$deploy_trained_caret_models_select_deploy_apply = renderUI({
			req(input$deploy_trained_caret_models_select_session)
			req(input$deploy_trained_caret_models_select_metrics)
			req(input$deploy_trained_caret_models_select_model)
			req(input$deploy_trained_caret_models_table_rows_selected)
			req(NROW(rv_deploy_models$trained_models_table)>0)
			req(input$deploy_trained_caret_models_select_deploy)
			actionBttn("deploy_trained_caret_models_select_deploy_apply"
				, inline=TRUE
				, block = FALSE
				, color = "success"
				, label = get_rv_labels("deploy_trained_caret_models_select_deploy_apply")
			)
		})

		observeEvent(input$deploy_trained_caret_models_select_deploy_apply, {
			req(input$deploy_trained_caret_models_select_deploy)
			req(input$deploy_trained_caret_models_hostname)
		   req(NROW(rv_deploy_models$trained_models_table)>0)
			deployed_df = list()
			for (m in input$deploy_trained_caret_models_select_deploy) {
				m_check = rv_deployed_models[[m]]
				if (isTRUE(is.null(m_check))) {
					rv_deployed_models[[m]] = m
					rv_deployed_models[[m]] = Rautoml::start_model_api(
						folder="models"
						, model_name=m
						, host=input$deploy_trained_caret_models_hostname
					)
				} 
				df = filter_deployed_models(
					df = rv_deploy_models$trained_models_table_filtered
					, model_ids = m
				)
				d_check = try(rv_deployed_models[[m]]$process$is_alive())
				if (isTRUE(d_check)) {
					u = rv_deployed_models[[m]]$url
					d = rv_deployed_models[[m]]$docs
					if (Rautoml::check_api_connection(d)) {
						df$url = u 
						df$api = d 
						df$status = "Deployed" 
					} else {
						Sys.sleep(3)
						if (Rautoml::check_api_connection(d)) {
							df$url = u 
							df$api = d 
							df$status = "Deployed" 
						} else {
							df$url = "" 
							df$api = "" 
							df$status = "Stopped" 
						}
					}
				} else {
					df$url = "" 
					df$api = "" 
					df$status = "Stopped" 
				}
				deployed_df[[m]] = df
			}
			display = do.call("rbind", deployed_df)

		  display$action = generate_action_buttons(display)

		  rv_deploy_models$deployed_models_table <- display

		  # Render Table
		  output$deploy_trained_caret_models_select_deployed_table <- renderDT({
			 req(rv_deploy_models$deployed_models_table)
			 DT::datatable(
				(rv_deploy_models$deployed_models_table
					|> dplyr::mutate_at("status", function(x){vapply(x, render_status_icon, FUN.VALUE = character(1))})
				),
				escape = FALSE,
				rownames = FALSE,
				options = list(dom = 't', paging = FALSE, ordering = FALSE)
			 )
		  }, server = FALSE)
		})

		# ---- Independent Observer for Button Clicks ----
		observeEvent(input$btn_click, {
		  req(NROW(rv_deploy_models$trained_models_table)>0)
		  info <- input$btn_click
		  req(info$id, info$action)

		  display <- rv_deploy_models$deployed_models_table
		  i <- which(display$model_id == info$id)
		  # Toggle Status
		  if (info$action == "Stop") {
			 display$status[i] <- "Stopped"
			 display$url[i] = "" 
			 display$api[i] = ""
			 rv_deployed_models[[info$id]]$process$kill()
		  } else {
			 rv_deployed_models[[info$id]] = Rautoml::start_model_api(
				folder="models"
				, model_name=info$id
				, host=input$deploy_trained_caret_models_hostname
			 )
			 d_check = try(rv_deployed_models[[info$id]]$process$is_alive())
				if (isTRUE(d_check)) {
					u = rv_deployed_models[[info$id]]$url
					d = rv_deployed_models[[info$id]]$docs
					if (Rautoml::check_api_connection(d)) {
						display$url[i] = u 
						display$api[i] = d 
						display$status[i] = "Deployed" 
					} else {
						Sys.sleep(3)
						if (Rautoml::check_api_connection(d)) {
							display$url[i] = u 
							display$api[i] = d 
							display$status[i] = "Deployed" 
						} else {
							display$url[i] = "" 
							display$api[i] = "" 
							display$status[i] = "Stopped" 
						}
					}
				} else {
					display$url[i] = "" 
					display$api[i] = "" 
					display$status[i] = "Stopped" 
				}
		  }

		  # Regenerate Buttons
		  display$action <- generate_action_buttons(display)

		  # Save Updated Table
		  rv_deploy_models$deployed_models_table <- display

		  # Re-render Table
		  output$deploy_trained_caret_models_select_deployed_table <- renderDT({
			 req(rv_deploy_models$deployed_models_table)
		    req(NROW(rv_deploy_models$trained_models_table)>0)
			 DT::datatable(
				(rv_deploy_models$deployed_models_table
					|> dplyr::mutate_at("status", function(x){vapply(x, render_status_icon, FUN.VALUE = character(1))})
				),
				escape = FALSE,
				rownames = FALSE,
				options = list(dom = 't', paging = FALSE, ordering = FALSE)
			 )
		  }, server = FALSE)
		}, ignoreInit = TRUE)

		## Output objects
		output$deploy_trained_caret_models_box_ui = renderUI({
		   req(NROW(rv_deploy_models$trained_models_table)>0)
			p(
				box(title = get_rv_labels("deploy_trained_caret_models_box_ui_options") 
					, status = "success"
					, solidHeader = TRUE
					, collapsible = TRUE
					, collapsed = FALSE
					, width = 12
					, fluidRow(
						column(width = 6
							, uiOutput("deploy_trained_caret_models_select_session")
							, uiOutput("deploy_trained_caret_models_select_model")
						)
						, column(width = 6
							, uiOutput("deploy_trained_caret_models_select_metrics")
						)
						, column(width = 6
							, uiOutput("deploy_trained_caret_models_select_deploy")
							, uiOutput("deploy_trained_caret_models_hostname")
							, uiOutput("deploy_trained_caret_models_select_deploy_apply")
						)
					)
				)
				, br()
				, br()
				, hr()
				, br()
				, br()
				, box(title = get_rv_labels("deploy_trained_caret_models_box_ui_ouput")
					, status = "success"
					, solidHeader = TRUE
					, collapsible = TRUE
					, collapsed = TRUE
					, width = 12
					, fluidRow(
						column(width=12
							, DT::DTOutput("deploy_trained_caret_models_table")
						)
					)
				)
				, br()
				, br()
				, hr()
				, br()
				, br()
				, box(title = "Deployed models"#get_rv_labels("deploy_trained_caret_models_box_ui_ouput")
					, status = "success"
					, solidHeader = TRUE
					, collapsible = TRUE
					, collapsed = TRUE
					, width = 12
					, fluidRow(
						column(width=12
							, DT::DTOutput("deploy_trained_caret_models_select_deployed_table")
						)
					)
				)

			)	
		})
		
	})
}

