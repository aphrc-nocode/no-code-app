##### ---- List datasets ------------------------------------------####
combine_data_list_datasets = function() {
	observeEvent(input$manage_data_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(length(rv_metadata$dataset_ids)>1)) {
				combine_data_ids = rv_metadata$dataset_ids[!rv_metadata$dataset_ids %in% input$dataset_id]
				empty_lab = ""
				names(empty_lab) = get_rv_labels("combine_data_list_datasets_ph")
				output$combine_data_list_datasets = renderUI({
					selectInput("combine_data_list_datasets"
						, label = get_rv_labels("combine_data_list_datasets") 
						, choices = c(empty_lab, combine_data_ids)
						, multiple=FALSE
					)
				})
			} else {
				output$combine_data_apply = NULL
				output$combine_data_list_datasets = renderText(paste0(get_rv_labels("combine_no_second_data"), "<b> Source data </b>"))
				output$combine_data_apply = NULL
				rv_current$combine_df = NULL 
				rv_current$combine_data_selected_vars = NULL
			}
		} else {
			output$combine_data_apply = NULL
			output$combine_data_list_datasets = NULL
			output$combine_data_apply = NULL
			rv_current$combine_df = NULL 
			rv_current$combine_data_selected_vars = NULL
			updateSelectInput(session = session, "combine_data_list_datasets", selected = "", choices = NULL)
		}
	})
	
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(input$combine_data_list_datasets)) & isTRUE(input$combine_data_list_datasets!="")) {
				output$combine_data_apply = renderUI({
					actionBttn("combine_data_apply"
						, inline=TRUE
						, block = FALSE
						, color = "success"
						, label = get_rv_labels("apply_selection"))
				})
			} else {
				output$combine_data_apply = NULL
				rv_current$combine_df = NULL 
				rv_current$combine_data_selected_vars = NULL
			}
		} else {
				output$combine_data_apply = NULL
				rv_current$combine_df = NULL 
				rv_current$combine_data_selected_vars = NULL
		}
	})
	
	observeEvent(input$combine_data_apply, {
		if (isTRUE(!is.null(input$combine_data_list_datasets))) {
			file_path = get_data_class(paste0("datasets/", input$combine_data_list_datasets))
			combine_df = upload_data(file_path)
			if (any(class(file_path) %in% c("spss", "dta"))) {
				combine_df = (combine_df
					|> as_factor()
				)
			}
			rv_current$combine_df = as.data.frame(combine_df)
			rv_current$combine_data_selected_vars = colnames(combine_df)
		} else {
			rv_current$combine_df = NULL 
			rv_current$combine_data_selected_vars = NULL
		}
	})
}

##### ---- Combine type ------------------------------------------####

combine_data_type = function() {
	observeEvent(input$combine_data_apply, {
		if (isTRUE(!is.null(input$combine_data_list_datasets))) {
			output$combine_data_type_choices = renderUI({
				  radioButtons("combine_data_type_choices"
						, label = get_rv_labels("combine_data_type_choices")
						, choices = get_named_choices(input_choices_file, input$change_language, "combine_data_type_choices")
						, selected = character(0)
						, inline = TRUE
				  )
			})
		} else {
			updateRadioButtons(session = session, inputId = "combine_data_type_choices", selected = character(0))
			output$combine_data_type_choices = NULL
		}
	})
}

#### ---- Matched variables display-------------------------------####
combine_data_matched_vars = function() {
	observeEvent(input$combine_data_type_choices, {
		if (isTRUE(length(input$combine_data_type_choices)>0)) {
			if (isTRUE(!is.null(rv_current$combine_data_selected_vars)) & isTRUE(!is.null(rv_current$selected_vars)) & (isTRUE(input$combine_data_type_choices=="Row-wise"))) {
				## Variables in base data
				output$combine_data_base_vars = renderUI({
					 selectInput('combine_data_base_vars'
						, label = get_rv_labels("combine_data_base_vars")
						, rv_current$selected_vars
						, selectize=FALSE
						, multiple=TRUE
						, size = min(10, length(rv_current$selected_vars))
						, width = "100%"
					 )
				})

				## Variables in new data
				output$combine_data_new_vars = renderUI({
					 selectInput('combine_data_new_vars'
						, label = get_rv_labels("combine_data_new_vars")
						, rv_current$combine_data_selected_vars
						, selectize=FALSE
						, multiple=TRUE
						, size = min(10, length(rv_current$combine_data_selected_vars))
						, width = "100%"
					 )
				})

				## Matched variables
				rv_current$combine_data_matched_vars = intersect(rv_current$selected_vars, rv_current$combine_data_selected_vars)
				if (isTRUE(!is.null(rv_current$combine_data_matched_vars)) & isTRUE(length(rv_current$combine_data_matched_vars)>0)) {
					output$combine_data_matched_vars = renderUI({
						 selectInput('combine_data_matched_vars'
							, label = get_rv_labels("combine_data_matched_vars")
							, rv_current$combine_data_matched_vars
							, selectize=FALSE
							, multiple=TRUE
							, size = min(10, length(rv_current$combine_data_matched_vars))
							, width = "100%"
						 )
					})
				} else {
					output$combine_data_matched_vars = renderUI({
						  radioButtons("combine_data_matched_vars"
							, label = get_rv_labels("combine_data_no_matched_vars")
							, choices = get_named_choices(input_choices_file, input$change_language, "combine_data_no_matched_vars_choices")
							, selected = character(0)
							, inline = FALSE
						  )
					})
				} 
			} else {
				output$combine_data_base_vars = NULL
				output$combine_data_new_vars = NULL
				updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
				output$combine_data_matched_vars = NULL
				updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
			}
		} else {
			output$combine_data_base_vars = NULL
			output$combine_data_new_vars = NULL
			updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
			output$combine_data_matched_vars = NULL
			updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
		}
	})

	## RESET fields 
	observeEvent(input$combine_data_list_datasets, {
		rv_current$combine_data_selected_vars = NULL
		updateRadioButtons(session = session, inputId = "combine_data_type_choices", selected = character(0))
		output$combine_data_type_choices = NULL
		rv_current$combine_data_matched_vars = NULL
		output$combine_data_matched_vars = NULL
		rv_current$combine_df = NULL 
		rv_current$combine_data_new_vars = NULL
		updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
      output$combine_data_new_vars = NULL
      updateSelectInput(session = session, "combine_data_base_vars", selected = NULL, choices = NULL)
		output$combine_data_base_vars = NULL
		output$combine_data_create_id_var = NULL
		updateMaterialSwitch(session, inputId="combine_data_create_id_var_check", value = NULL)
		updateTextInput(session = session, inputId="combine_data_create_id_var_input", value="")
		output$combine_data_create_id_var_input = NULL
		output$combine_data_matched_apply = NULL
	})
	
	## FIXME: There should be a better way
	observe({
		if (!isTRUE(length(input$combine_data_type_choices)>0)) {
			output$combine_data_base_vars = NULL
			output$combine_data_new_vars = NULL
			updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
			output$combine_data_matched_vars = NULL
			updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
		}
	})

}

#### ---- Combine row-wise ----------------------------------------####
combine_data_row_wise = function() {
	observe({
		if (isTRUE(length(input$combine_data_type_choices)>0)) {
			if (isTRUE(input$combine_data_type_choices=="Row-wise") & isTRUE(!is.null(rv_current$combine_data_matched_vars)) & isTRUE(length(rv_current$combine_data_matched_vars)>0)) {
				output$combine_data_combine_modify_vars = renderUI({
				  radioButtons("combine_data_combine_modify_vars"
					, label = get_rv_labels("combine_data_combine_modify_vars")
					, choices = get_named_choices(input_choices_file, input$change_language, "combine_data_combine_modify_vars_choices")
					, selected = character(0)
					, inline = FALSE
				  )
				})
			} else {
				# HERE column-wise
				updateRadioButtons(session = session, inputId="combine_data_combine_modify_vars", selected = character(0))
				output$combine_data_combine_modify_vars = NULL
			}
		} else {
				updateRadioButtons(session = session, inputId="combine_data_combine_modify_vars", selected = character(0))
				output$combine_data_combine_modify_vars = NULL
		}
	})

	observe({
			if (isTRUE(input$combine_data_combine_modify_vars=="Proceed")) {
			  output$combine_data_create_id_var = renderUI({
				 materialSwitch(
					inputId = "combine_data_create_id_var_check",
					label = get_rv_labels("combine_data_create_id_var_check"),
					status = "success",
					right = TRUE
				 )
			  })
			} else {
				output$combine_data_create_id_var = NULL
				updateMaterialSwitch(session, inputId="combine_data_create_id_var_check", value = NULL)
			}
	})


	observe({
		if (isTRUE(input$combine_data_create_id_var_check)) {
			output$combine_data_create_id_var_input = renderUI({
				textInput("combine_data_create_id_var_input"
					, label = NULL
					, value = ""
					, placeholder = "Type ID var"
				)
			})
		} else {
			updateTextInput(session = session, inputId="combine_data_create_id_var_input", value="")
			output$combine_data_create_id_var_input = NULL
		}
	})


	observe({
		if (isTRUE(input$combine_data_combine_modify_vars=="Proceed")) {
			output$combine_data_matched_apply = renderUI({
				actionBttn("combine_data_matched_apply"
					, inline=TRUE
					, block = FALSE
					, color = "success"
					, label = get_rv_labels("apply_selection"))
			})
		} else {
			
			## HERE modify
			output$combine_data_matched_apply = NULL
		}
	})

	## RESET
	observeEvent(input$combine_data_type_choices, {
		output$combine_data_create_id_var = NULL
		updateMaterialSwitch(session, inputId="combine_data_create_id_var_check", value = NULL)
			
		updateTextInput(session = session, inputId="combine_data_create_id_var_input", value="")
		output$combine_data_create_id_var_input = NULL
			
		output$combine_data_matched_apply = NULL
	})

	observeEvent(input$combine_data_combine_modify_vars, {
		updateTextInput(session = session, inputId="combine_data_create_id_var_input", value="")
		output$combine_data_create_id_var_input = NULL
	})

}


