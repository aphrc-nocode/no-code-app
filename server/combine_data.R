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
				p(
					br()
					  , radioButtons("combine_data_type_choices"
							, label = get_rv_labels("combine_data_type_choices")
							, choices = get_named_choices(input_choices_file, input$change_language, "combine_data_type_choices")
							, selected = character(0)
							, inline = TRUE
					  )
				)
			})
		} else {
			updateRadioButtons(session = session, inputId = "combine_data_type_choices", selected = character(0))
			output$combine_data_type_choices = NULL
		}
	})
}


##### ---- Combine data match type --------------------------------####

combine_data_match_type = function() {
	observeEvent(input$combine_data_type_choices, {
		if (isTRUE(length(input$combine_data_type_choices)>0)) {
			if (isTRUE(!is.null(rv_current$combine_data_selected_vars)) & isTRUE(!is.null(rv_current$selected_vars)) & (isTRUE(length(input$combine_data_type_choices)>0))) {
				output$combine_data_match_type = renderUI({
					  radioButtons("combine_data_match_type"
						, label = get_rv_labels("combine_data_match_type")
						, choices = get_named_choices(input_choices_file, input$change_language, "combine_data_match_type_choices")
						, selected = character(0)
						, inline = TRUE
					  )
				})	
			} else {
				updateRadioButtons(session = session, inputId="combine_data_match_type", selected = character(0))
				output$combine_data_match_type = NULL
			}
		} else {
			updateRadioButtons(session = session, inputId="combine_data_match_type", selected = character(0))
			output$combine_data_match_type = NULL
		}
	})
}


##### ---- Combine data variables in the base and new data ---------------------------####
combine_data_variable_matching = function() {
	observeEvent(input$combine_data_match_type, {
		if (isTRUE(length(input$combine_data_type_choices)>0)) {
			if (isTRUE(!is.null(rv_current$combine_data_selected_vars)) & isTRUE(!is.null(rv_current$selected_vars)) & (isTRUE(length(input$combine_data_type_choices)>0))) {
				if (isTRUE(length(input$combine_data_match_type)>0)) {

					if (isTRUE(input$combine_data_match_type=="manual")) {
						
						output$combine_data_matched_vars_manual_ui = renderUI({
							HTML(paste0("<b>", get_rv_labels("combine_data_matched_vars_manual_ui"), "</b>"))
						})

						empty_lab = ""
						names(empty_lab) = get_rv_labels("combine_data_matched_vars_manual_ui_ph")
						output$combine_data_base_vars = renderUI({
							selectInput('combine_data_base_vars'
								, label = get_rv_labels("combine_data_base_vars")
								, c(empty_lab, rv_current$selected_vars[!rv_current$selected_vars %in% rv_current$combine_data_matched_vars])
								, multiple=FALSE
							)
						})

						## Variables in new data
						output$combine_data_new_vars = renderUI({
							 selectInput('combine_data_new_vars'
								, label = get_rv_labels("combine_data_new_vars")
								, c(empty_lab, rv_current$combine_data_selected_vars[!rv_current$combine_data_selected_vars %in% rv_current$combine_data_matched_vars])
								, multiple=FALSE
							 )
						})

					} else {
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
						output$combine_data_matched_vars_manual_ui = NULL
					}


					## Matched variables
					rv_current$combine_data_matched_vars = intersect(rv_current$selected_vars, rv_current$combine_data_selected_vars)
					if (isTRUE(!is.null(rv_current$combine_data_matched_vars)) & isTRUE(length(rv_current$combine_data_matched_vars)>0)) {
						output$combine_data_matched_vars = renderUI({
							 selectInput('combine_data_matched_vars'
								, label = if (isTRUE(input$combine_data_type_choices=="row"))  get_rv_labels("combine_data_matched_vars") else get_rv_labels("combine_data_matched_vars_column")
#								, label = get_rv_labels("combine_data_matched_vars")
								, rv_current$combine_data_matched_vars
								, selectize=FALSE
								, multiple=TRUE
								, size = min(10, length(rv_current$combine_data_matched_vars))
								, width = "100%"
							 )
						})
					} else {
						output$combine_data_matched_vars = renderUI({
							p(
								HTML(paste0("<b>", get_rv_labels("combine_data_matched_vars"), "</b><br>"))
								, get_rv_labels("combine_data_no_matched_vars")
							)
						})
					} 


				} else {
					output$combine_data_base_vars = NULL
					updateSelectInput(session = session, "combine_data_base_vars", selected = NULL, choices = NULL)
					output$combine_data_new_vars = NULL
					rv_current$combine_data_new_vars = NULL
					updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
					output$combine_data_matched_vars = NULL
					rv_current$combine_data_matched_vars = NULL
					updateSelectInput(session = session, "combine_data_matched_vars", selected = NULL, choices = NULL)
					output$combine_data_matched_vars_manual_ui = NULL
				}
			} else {
				output$combine_data_base_vars = NULL
				updateSelectInput(session = session, "combine_data_base_vars", selected = NULL, choices = NULL)
				output$combine_data_new_vars = NULL
				rv_current$combine_data_new_vars = NULL
				updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
				output$combine_data_matched_vars = NULL
				rv_current$combine_data_matched_vars = NULL
				updateSelectInput(session = session, "combine_data_matched_vars", selected = NULL, choices = NULL)
				output$combine_data_matched_vars_manual_ui = NULL
			}
		} else {
			output$combine_data_base_vars = NULL
			updateSelectInput(session = session, "combine_data_base_vars", selected = NULL, choices = NULL)
			output$combine_data_new_vars = NULL
			rv_current$combine_data_new_vars = NULL
			updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
			output$combine_data_matched_vars = NULL
			rv_current$combine_data_matched_vars = NULL
			updateSelectInput(session = session, "combine_data_matched_vars", selected = NULL, choices = NULL)
			output$combine_data_matched_vars_manual_ui = NULL
		}
	})
	
}


##### ---- Combine data perform variable matching  --------------------------------####

combine_data_perform_variable_match = function() {
	observeEvent(c(input$combine_data_match_type, input$combine_data_base_vars, input$combine_data_new_vars), {
		if (isTRUE(length(input$combine_data_type_choices)>0)) {
			if (isTRUE(!is.null(rv_current$combine_data_selected_vars)) & isTRUE(!is.null(rv_current$selected_vars)) & (isTRUE(length(input$combine_data_type_choices)>0))) {
				if (isTRUE(length(input$combine_data_match_type)>0)) {
					if (isTRUE(input$combine_data_match_type=="manual")) {
						if ((isTRUE(!is.null(input$combine_data_base_vars)) & isTRUE(input$combine_data_base_vars!="")) & (isTRUE(!is.null(input$combine_data_new_vars)) & isTRUE(input$combine_data_new_vars != ""))) {
							output$combine_data_manual_match_apply = renderUI({
								p(
									br()
									, actionBttn("combine_data_manual_match_apply"
										, inline=TRUE
										, block = FALSE
										, color = "success"
										, label = get_rv_labels("combine_data_manual_match_apply")
									)
								)
							})
						} else {
							output$combine_data_manual_match_apply = NULL
						}
					} else {
						output$combine_data_manual_match_apply = NULL
						NULL
					}
				} else {
					output$combine_data_manual_match_apply = NULL
					NULL
				}
			} else {
				NULL
				output$combine_data_manual_match_apply = NULL
			}
		} else {
			NULL
			output$combine_data_manual_match_apply = NULL
		}
	})

	observeEvent(input$combine_data_manual_match_apply, {
		
		rv_current$combine_df = (rv_current$combine_df 
			|> Rautoml::rename_vars(input$combine_data_new_vars, input$combine_data_base_vars)
		)
		
		rv_current$combine_data_matched_vars = c(rv_current$combine_data_matched_vars, input$combine_data_base_vars) 
		
		empty_lab = ""
		names(empty_lab) = get_rv_labels("combine_data_matched_vars_manual_ui_ph")
		updateSelectInput(session = session, "combine_data_base_vars", selected = NULL, choices=c(empty_lab,  rv_current$selected_vars[!rv_current$selected_vars %in% rv_current$combine_data_matched_vars]))

		rv_current$combine_data_selected_vars = colnames(rv_current$combine_df)
		empty_lab = ""
		names(empty_lab) = get_rv_labels("combine_data_matched_vars_manual_ui_ph")
		updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices=c(empty_lab,  rv_current$combine_data_selected_vars[!rv_current$combine_data_selected_vars %in% rv_current$combine_data_matched_vars]))
			
		if (isTRUE(!is.null(rv_current$combine_data_matched_vars)) & isTRUE(length(rv_current$combine_data_matched_vars)>0)) {
			output$combine_data_matched_vars = renderUI({
				 selectInput('combine_data_matched_vars'
					, label = if (isTRUE(input$combine_data_type_choices=="row"))  get_rv_labels("combine_data_matched_vars") else get_rv_labels("combine_data_matched_vars_column")
					, rv_current$combine_data_matched_vars
					, selectize=FALSE
					, multiple=TRUE
					, size = min(10, length(rv_current$combine_data_matched_vars))
					, width = "100%"
				 )
			})
		} else {
			output$combine_data_matched_vars = renderUI({
				p(
					HTML(paste0("<b>", get_rv_labels("combine_data_matched_vars"), "</b><br>"))
					, get_rv_labels("combine_data_no_matched_vars")
				)
			})
		} 
	})
}


##### ---- Combine data perform merging  --------------------------------####
combine_data_perform_merging = function() {
	observeEvent(c(input$combine_data_match_type, input$combine_data_base_vars, input$combine_data_new_vars), {
		if (isTRUE(length(input$combine_data_type_choices)>0)) {
			if (isTRUE(!is.null(rv_current$combine_data_selected_vars)) & isTRUE(!is.null(rv_current$selected_vars)) & (isTRUE(length(input$combine_data_type_choices)>0))) {
				if (isTRUE(length(input$combine_data_match_type)>0)) {
					if ((isTRUE(!is.null(rv_current$combine_data_matched_vars)) & isTRUE(length(rv_current$combine_data_matched_vars)>0)) | (isTRUE(input$combine_data_match_type=="automatic"))) {
						output$combine_data_perform_merging_apply = renderUI({
							actionBttn("combine_data_perform_merging_apply"
								, inline=TRUE
								, block = FALSE
								, color = "success"
								, label = get_rv_labels("apply_selection")
							)
						})
						if (isTRUE(input$combine_data_type_choices=="row")) {
							output$combine_data_create_id_var_input = renderUI({
								 textInput("combine_data_create_id_var_input"
									, label = NULL
									, value = NULL
									, placeholder = get_rv_labels("combine_data_create_id_var_input_ph")
									, width="30%"
								)
							})	
						} else {
							updateTextInput(session = session , inputId = "combine_data_create_id_var_input")
							output$combine_data_create_id_var_input = NULL
						}
					} else {
						updateTextInput(session = session , inputId = "combine_data_create_id_var_input")
						output$combine_data_create_id_var_input = NULL
						output$combine_data_perform_merging_apply = NULL
					}
				} else {
					updateTextInput(session = session , inputId = "combine_data_create_id_var_input")
					output$combine_data_create_id_var_input = NULL
					output$combine_data_perform_merging_apply = NULL
				}
			} else {
				updateTextInput(session = session , inputId = "combine_data_create_id_var_input")
				output$combine_data_create_id_var_input = NULL
				output$combine_data_perform_merging_apply = NULL
			}
		} else {
			updateTextInput(session = session , inputId = "combine_data_create_id_var_input")
			output$combine_data_create_id_var_input = NULL
			output$combine_data_perform_merging_apply = NULL
		}
	})

	### Merge datasets
	observeEvent(input$combine_data_perform_merging_apply, {
		if (isTRUE(length(input$combine_data_type_choices)>0)) {
			if (isTRUE(any(input$combine_data_type_choices %in% c("row", "column")))) {
				temp_out = Rautoml::combine_data(rv_current$working_df
					, rv_current$combine_df
					, type = input$combine_data_type_choices
					, id = input$combine_data_create_id_var_input
				)
				rv_current$working_df = temp_out$df
				rv_current$selected_vars = colnames(rv_current$working_df)

				rv_current$combine_data_row_wise_values_log = c(rv_current$combine_data_row_wise_values_log
					, paste0("Rows: ", temp_out$dim[1], "; ", "Columns: ", temp_out$dim[2])
				)
				output$combine_data_row_wise_values_log_ui = renderUI({
					p( hr()
						, HTML(paste0("<b>", get_rv_labels("combine_data_row_wise_values_log"), "</b>"))
					)
				})
				output$combine_data_row_wise_values_log = renderPrint({
					cat(rv_current$combine_data_row_wise_values_log, sep="\n")
				})	
			}
		}
		
		## Reset
		output$combine_data_type_choices = NULL
		updateRadioButtons(session = session, inputId = "combine_data_type_choices", selected = character(0))

		output$combine_data_match_type = NULL
		updateRadioButtons(session = session, inputId="combine_data_match_type", selected = character(0))

		output$combine_data_base_vars = NULL
		updateSelectInput(session = session, "combine_data_base_vars", selected = NULL, choices = NULL)
		output$combine_data_new_vars = NULL
		rv_current$combine_data_new_vars = NULL
		updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
		output$combine_data_matched_vars = NULL
		rv_current$combine_data_matched_vars = NULL
		updateSelectInput(session = session, "combine_data_matched_vars", selected = NULL, choices = NULL)
		output$combine_data_matched_vars_manual_ui = NULL
		output$combine_data_manual_match_apply = NULL
		output$combine_data_perform_merging_apply = NULL
		updateTextInput(session = session , inputId = "combine_data_create_id_var_input")
		output$combine_data_create_id_var_input = NULL
	})
}

#### ---- Reset combine data ----------------------------------------------------- ####

combine_data_reset = function() {
	## RESET fields 
	observeEvent(input$combine_data_list_datasets, {
		rv_current$combine_data_selected_vars = NULL
		
		output$combine_data_type_choices = NULL
		updateRadioButtons(session = session, inputId = "combine_data_type_choices", selected = character(0))
	
		output$combine_data_match_type = NULL
		updateRadioButtons(session = session, inputId="combine_data_match_type", selected = character(0))

		output$combine_data_base_vars = NULL
		updateSelectInput(session = session, "combine_data_base_vars", selected = NULL, choices = NULL)
		output$combine_data_new_vars = NULL
		rv_current$combine_data_new_vars = NULL
		updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
		output$combine_data_matched_vars = NULL
		rv_current$combine_data_matched_vars = NULL
		updateSelectInput(session = session, "combine_data_matched_vars", selected = NULL, choices = NULL)
		output$combine_data_matched_vars_manual_ui = NULL
		output$combine_data_manual_match_apply = NULL
		output$combine_data_perform_merging_apply = NULL
		updateTextInput(session = session , inputId = "combine_data_create_id_var_input")
		output$combine_data_create_id_var_input = NULL
	})
	
	## FIXME: There should be a better way
	observe({
		if (!isTRUE(length(input$combine_data_type_choices)>0)) {
			output$combine_data_match_type = NULL
			updateRadioButtons(session = session, inputId="combine_data_match_type", selected = character(0))

			output$combine_data_base_vars = NULL
			updateSelectInput(session = session, "combine_data_base_vars", selected = NULL, choices = NULL)
			output$combine_data_new_vars = NULL
			rv_current$combine_data_new_vars = NULL
			updateSelectInput(session = session, "combine_data_new_vars", selected = NULL, choices = NULL)
			output$combine_data_matched_vars = NULL
			rv_current$combine_data_matched_vars = NULL
			updateSelectInput(session = session, "combine_data_matched_vars", selected = NULL, choices = NULL)
			output$combine_data_matched_vars_manual_ui = NULL
			output$combine_data_manual_match_apply = NULL
			output$combine_data_perform_merging_apply = NULL
			updateTextInput(session = session , inputId = "combine_data_create_id_var_input")
			output$combine_data_create_id_var_input = NULL
		}
	})
}
