#### ----- Prediction using trained models ------------------------------------ #####

predict_trained_caret_models = function() {
	

	observeEvent(input$deploy_trained_caret_models_select_deploy_apply, {
		
		## Models to deploy
		output$predict_trained_caret_models_select_model = renderUI({
			req(NROW(rv_deploy_models$trained_models_table)>0)
			req(!is.null(rv_deploy_models$deployed_models_table))
			d_list = (rv_deploy_models$deployed_models_table
				|> dplyr::filter(status=="Deployed")
			)

			req(NROW(d_list)>0)
			d_list_names = paste0(d_list$model, "-", d_list$model_id)
			d_list = d_list$url
			names(d_list) = d_list_names
			rv_deploy_models$deployed_models_list = d_list

			empty_lab = ""
			names(empty_lab) = get_rv_labels("predict_trained_caret_models_select_model_ph")
			selectInput("predict_trained_caret_models_select_model"
				, label = get_rv_labels("predict_trained_caret_models_select_model")
				, choices = c(empty_lab, rv_deploy_models$deployed_models_list) 
				, multiple = TRUE
				, width = NULL
			)	
		})

		## Prediction type
		output$predict_trained_caret_models_prediction_type = renderUI({
			req(!is.null(input$predict_trained_caret_models_select_model), input$predict_trained_caret_models_select_model!="")
			choices =   get_named_choices(input_choices_file, input$change_language,"predict_trained_caret_models_prediction_type_choices")
			prettyRadioButtons(
				inputId = "predict_trained_caret_models_prediction_type_choices"
					, label = get_rv_labels("predict_trained_caret_models_prediction_type_choices")
					, choices = choices
					, selected = character(0)
					, status = "success"
			)
		})
	
		observeEvent(input$predict_trained_caret_models_prediction_type_choices, {
			req(length(input$predict_trained_caret_models_prediction_type_choices)>0)
			req(!is.null(input$predict_trained_caret_models_select_model), input$predict_trained_caret_models_select_model!="")
			rv_deploy_models$endpoint_objects = Rautoml::get_endpoint_data(
				url = unname(input$predict_trained_caret_models_select_model[1])
				, endpoint = "metadata"
			)
			
			## Download template
			req(input$predict_trained_caret_models_prediction_type_choices=="upload_file")
			output$predict_trained_caret_models_download_template = downloadHandler(
				filename = function() paste0("prediction_template", Sys.Date(), ".csv")
				, content = function(file) {
					df = rv_deploy_models$endpoint_objects$template
					write.csv(df, file, row.names = FALSE)
				}
			)
		})

		## Download template UI
		output$predict_trained_caret_models_download_template_ui = renderUI({
			req(length(input$predict_trained_caret_models_prediction_type_choices)>0)
			req(!is.null(input$predict_trained_caret_models_select_model), input$predict_trained_caret_models_select_model!="")
			
			if (isTRUE(input$predict_trained_caret_models_prediction_type_choices=="upload_file")) {
				req(is.data.frame(rv_deploy_models$endpoint_objects$template)
					, input$predict_trained_caret_models_prediction_type_choices=="upload_file"
				)
				downloadButton("predict_trained_caret_models_download_template", get_rv_labels("predict_trained_caret_models_download_template"), class = "btn btn-success")
			
			} else if (input$predict_trained_caret_models_prediction_type_choices=="use_form") {
				actionButton("predict_trained_caret_models_upload_form", paste0("➕ ", get_rv_labels("predict_trained_caret_models_upload_form")), class = "btn btn-primary")
			}

		})
	
		## Upload button
		output$predict_trained_caret_models_upload_data = renderUI({
			req(input$predict_trained_caret_models_prediction_type_choices=="upload_file")
			fileInput("predict_trained_caret_models_upload_data", label = get_rv_labels("predict_trained_caret_models_upload_data")
				, placeholder = paste0(get_rv_labels("drop_files"), ": ", paste0(supported_files, collapse=", "))
				, accept = supported_files
				, width = "100%"
			)
		})


		output$predict_trained_caret_models_upload_data_apply = renderUI({
			req(input$predict_trained_caret_models_prediction_type_choices=="upload_file")
			req(!is.null(input$predict_trained_caret_models_upload_data$datapath))
			req(!is.null(rv_deploy_models$endpoint_objects$template))
		 	preds_obs = tryCatch({
				df_path = Rautoml::get_data_class(input$predict_trained_caret_models_upload_data$datapath)
				prediction_df = Rautoml::upload_data(df_path)
				Rautoml::check_columns(rv_deploy_models$endpoint_objects$template, prediction_df)
			}, error = function(e) {
				shinyalert("Error: ", paste0(get_rv_labels("predict_trained_caret_models_upload_data_apply_error"), "\n", e$message), type = "error")
				return(list(check = FALSE, df = NULL))
		 	})
			rv_deploy_models$prediction_df = preds_obs$df
			if (!isTRUE(preds_obs$check)) {
				rv_deploy_models$prediction_df = NULL
			}
			req(isTRUE(preds_obs$check))
			actionBttn("predict_trained_caret_models_upload_data_apply"
				, inline=TRUE
				, block = FALSE
				, color = "success"
				, label = get_rv_labels("predict_trained_caret_models_upload_data_apply")
			)
		})

		## Upload form
		observeEvent(input$predict_trained_caret_models_upload_form, {
			output$predict_trained_caret_models_upload_form_apply = renderUI({
				req(input$predict_trained_caret_models_prediction_type_choices=="use_form")
				req(!is.null(rv_deploy_models$endpoint_objects$prototype))
				form_ui = create_form_prototype(rv_deploy_models$endpoint_objects$prototype)
				rv_deploy_models$prediction_df = data.frame()
				 showModal(modalDialog(
					title = paste0("🧾 ", get_rv_labels("predict_trained_caret_models_prediction_type_choices_form")),
					create_form_prototype(rv_deploy_models$endpoint_objects$prototype),
					footer = tagList(
					  modalButton(get_rv_labels("predict_trained_caret_models_upload_form_apply_cancel")),
					  actionButton("predict_trained_caret_models_upload_form_apply_save_entry", get_rv_labels("predict_trained_caret_models_upload_form_apply_save_entry"), class = "btn btn-success"),
					  actionButton("predict_trained_caret_models_upload_form_apply_finish_entry", get_rv_labels("predict_trained_caret_models_upload_form_apply_finish_entry"), class = "btn btn-primary")
					),
					easyClose = TRUE
				 ))
			})
		})

	  # Save entry from modal
	  observeEvent(input$predict_trained_caret_models_upload_form_apply_save_entry, {
		 prototype = rv_deploy_models$endpoint_objects$prototype
		 entry = sapply(names(prototype), function(nm) {
			input[[nm]]
		 }, simplify = FALSE)
		 
		 # Convert to data frame
		 entry_df = as.data.frame(entry, stringsAsFactors = FALSE)
		 
		 # Append to reactive data frame
		 if (NROW(rv_deploy_models$prediction_df) == 0) {
			rv_deploy_models$prediction_df = entry_df
		 } else {
			rv_deploy_models$prediction_df = rbind(rv_deploy_models$prediction_df, entry_df)
		 }
		
		 # Reset form inputs inside modal for next entry
		 removeModal()
		 showModal(modalDialog(
			title = paste0("🧾 ", get_rv_labels("predict_trained_caret_models_prediction_type_choices_form_another_entry")),
			create_form_prototype(rv_deploy_models$endpoint_objects$prototype),
			footer = tagList(
			  modalButton(get_rv_labels("predict_trained_caret_models_upload_form_apply_cancel")),
			  actionButton("predict_trained_caret_models_upload_form_apply_save_entry", get_rv_labels("predict_trained_caret_models_upload_form_apply_save_entry"), class = "btn btn-success"),
			  actionButton("predict_trained_caret_models_upload_form_apply_finish_entry", get_rv_labels("predict_trained_caret_models_upload_form_apply_finish_entry"), class = "btn btn-primary")
			),
			easyClose = TRUE
		 ))
	  })
	  
	  # Finish entries and close modal
	  observeEvent(input$predict_trained_caret_models_upload_form_apply_finish_entry, {
		 removeModal()
	  })


		## Prediction button
		observeEvent(c(input$predict_trained_caret_models_upload_data_apply, input$predict_trained_caret_models_upload_form_apply_finish_entry), {
			output$predict_trained_caret_models_predict_apply = renderUI({
				req(length(input$predict_trained_caret_models_prediction_type_choices)>0
					, !is.null(input$predict_trained_caret_models_prediction_type_choices)
				)
				req(!is.null(rv_deploy_models$prediction_df), NROW(rv_deploy_models$prediction_df)>0)
				actionBttn("predict_trained_caret_models_predict_apply"
					, inline=TRUE
					, block = FALSE
					, color = "success"
					, label = get_rv_labels("predict_trained_caret_models_predict_apply")
				)
			})
		})
	
  
		## Prediction output
		observeEvent(input$predict_trained_caret_models_predict_apply, {
			req(!is.null(rv_deploy_models$prediction_df), NROW(rv_deploy_models$prediction_df)>0)
			req(!is.null(rv_deploy_models$endpoint_objects))
			r = rv_deploy_models$endpoint_objects$recipes
			r = Rautoml::get_recipes(name=r, folder="recipes")
			u = input$predict_trained_caret_models_select_model
			n = rv_deploy_models$deployed_models_list
			n = names(n)[n %in% u]
			
			rv_deploy_models$predicted_df = tryCatch({
				Rautoml::predict_endpoint(
					url = u 
					, new_data = rv_deploy_models$prediction_df
					, recipes = r
					, model_name = n
				)	
			}, error = function(e) {
				shinyalert("Error: ", paste0(get_rv_labels("predict_trained_caret_models_predict_apply_error", "\n"), e$message), type = "error")
				return(NULL)
			})

			output$predict_trained_caret_models_predicted_values_table = renderDT({
				req(!is.null(rv_deploy_models$predicted_df), is.data.frame(rv_deploy_models$predicted_df)) 
				 DT::datatable(rv_deploy_models$predicted_df
					, rownames = FALSE
					, options = list(pageLength = 10, scrollX = TRUE)
				)
			})
			
			output$predict_trained_caret_models_predicted_values_plot = renderPlot({
				req(!is.null(rv_deploy_models$predicted_df), is.data.frame(rv_deploy_models$predicted_df)) 
				 Rautoml::viz_pred(rv_deploy_models$predicted_df)
			})
		})
	
		## Output objects
		output$predict_trained_caret_models_box_ui = renderUI({
			p(
				fluidRow(
					column(width=6
						, uiOutput("predict_trained_caret_models_select_model")
						, uiOutput("predict_trained_caret_models_prediction_type")
						, uiOutput("predict_trained_caret_models_download_template_ui")
						, uiOutput("predict_trained_caret_models_upload_form_apply")
						, br()
						, uiOutput("predict_trained_caret_models_predict_apply")
					)
					, column(width=6
						, uiOutput("predict_trained_caret_models_upload_data")
						, uiOutput("predict_trained_caret_models_upload_data_apply")
					)
				)
				, br()
				, hr()
				, br()
				, box(title = "Predictions"
					, status = "success"
					, solidHeader = TRUE
					, collapsible = TRUE
					, collapsed = TRUE
					, width = 12
					, fluidRow(
						column(width = 6
							, DT::DTOutput("predict_trained_caret_models_predicted_values_table")
						)
						, column(width = 6
							, plotOutput("predict_trained_caret_models_predicted_values_plot")
						)
					)
				)
			)
		})
	})


}
