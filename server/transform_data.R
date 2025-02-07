##### ---- Select variables ------------------------------------------####
transform_data_select_variables_server = function() {
	observeEvent(input$manage_data_apply, {
		rv_current$transform_data_select_vars = renderUI({
		  p(
			 HTML(paste0("<b>", get_rv_labels("transform_data_select_vars"), "</b>"))
			 , selectInput('transform_data_select_vars'
				, label = NULL
				, rv_current$selected_vars
				, selectize=FALSE
				, multiple=FALSE
				, selected = ""
				, size = min(10, length(rv_current$selected_vars))
				, width = "100%"
			 )
		  )
		})
		output$transform_data_select_vars = rv_current$transform_data_select_vars
	})
}


###### ----- rename variable -------------------------------------------- ####

transform_data_rename_variables_server = function() {
	observe({ 
		if (isTRUE(input$transform_data_select_vars!="")) {
		  output$transform_data_rename_variable = renderUI({
				materialSwitch(
						inputId = "transform_data_rename_variable_check",
						label = get_rv_labels("rename_variable"),
						status = "success",
						right = TRUE
				 )
		  })
		} else {
			output$transform_data_rename_variable = NULL
		}
	})

	observe({
		if (isTRUE(input$transform_data_rename_variable_check)) {
			output$transform_data_rename_variable_input = renderUI({
          textInput("transform_data_rename_variable_input"
            , label = NULL
				, value=""
            , placeholder = paste0(input$transform_data_select_vars, "_new")
            , width = "100%"
          )
			})
		} else {
			output$transform_data_rename_variable_input = NULL
		}
	})

}

###### ----- Change data type -------------------------------------------- ####
transform_data_change_type_server = function() {
	observe({
		if (isTRUE(input$transform_data_select_vars=="") | isTRUE(is.null(input$transform_data_select_vars))) {
			rv_current$selected_var = NULL 
		} else {
			rv_current$selected_var = input$transform_data_select_vars
		}
		if (isTRUE(!is.null(rv_current$selected_var))) {
			rv_current$vartype = get_type(rv_current$working_df[[rv_current$selected_var]])
		  output$transform_data_change_type = renderUI({
				materialSwitch(
						inputId = "transform_data_change_type_check",
						label = get_rv_labels("change_variable_type"),
						status = "success",
						right = TRUE
				 )
		  })
		} else {
			output$transform_data_change_type = NULL
		}
	})

	observe({
		if (isTRUE(input$transform_data_change_type_check)) {
			old_vartype = get_named_choices(input_choices_file, input$change_language, "transform_data_change_type_choices")
			output$transform_data_change_type_choices = renderUI({
				selectInput("transform_data_change_type_choices"
					, label = NULL
					, choices = old_vartype[!old_vartype %in% rv_current$vartype]
					, multiple=FALSE
				)
			})
		} else {
			output$transform_data_change_type_choices = NULL
		}
	})

	observe({
		if (isTRUE(!is.null(rv_current$selected_var))) {
			output$transform_data_variable_type_ui = renderUI({
				HTML(paste0("<b>", get_rv_labels("variable_type"), "</b>"))
			})
			output$transform_data_variable_type = renderPrint({
				cat(paste0(rv_current$selected_var , " ----> ", rv_current$vartype))
			})
		} else {
			output$transform_data_variable_type = NULL
			output$transform_data_variable_type_ui = NULL
		}
	})
 	
	#### FIXME: ADD ALL CONDITIONS WHICH DEPENDS ON APPLY
	observe({
		if (isTRUE(!is.null(rv_current$selected_var))) {
			if ((isTRUE(input$transform_data_change_type_check) & isTRUE(!grepl("---", input$transform_data_change_type_choices)))| (isTRUE(!is.null(input$transform_data_rename_variable_input)) & isTRUE(input$transform_data_rename_variable_check)  & isTRUE(input$transform_data_rename_variable_input!="")) | (isTRUE(input$transform_data_recode_variable_check) & isTRUE(!is.null(input$transform_data_recode_variable_input)) & isTRUE(input$transform_data_recode_variable_input!="")) | (isTRUE(input$transform_data_create_missing_values_check) & isTRUE(length(input$transform_data_create_missing_values_options)>0) & any(input$transform_data_create_missing_values_options %in% c("Use patterns", "Use categories")) & ((isTRUE(input$transform_data_create_missing_values_input_numeric!="") & isTRUE(!is.null(input$transform_data_create_missing_values_input_numeric))) | (isTRUE(input$transform_data_create_missing_values_input!="") & (isTRUE(!is.null(input$transform_data_create_missing_values_input)))))) | ((isTRUE(any(input$transform_data_handle_outliers_choices %in% c("drop", "nothing"))) | (isTRUE(any(input$transform_data_handle_outliers_correct_options %in% c("mean", "median")))) | (isTRUE(!is.na(input$transform_data_handle_outliers_correct_options_input)))) & (isTRUE(length(rv_current$outlier_values)>0)) ) | (((isTRUE(!is.null(input$transform_data_handle_missing_values_new_category)) & isTRUE(input$transform_data_handle_missing_values_new_category!="")) | (isTRUE(any(input$transform_data_handle_missing_values_choices %in% c("Drop missing (for this)", "Drop missing (for all)"))))) & isTRUE(rv_current$has_missing_data_check))   ) {
				output$transform_data_apply = renderUI({
				p(
						actionBttn("transform_data_apply"
							, inline = TRUE
							, color = "success"
							, label=get_rv_labels("transform_data_apply")
						)
					)
				})
			} else {
				output$transform_data_apply = NULL
			}
		}
	})
	
	### Change variable types
	#### FIXME: Better way to do this?
	observeEvent(input$transform_data_apply, {
		transfun = function(var, old, new) {
			paste0("{", var, ": ", old, " ----> ", new, "}")
		}
		if (isTRUE(input$transform_data_change_type_check)) {
			if (isTRUE(rv_current$vartype=="numeric") & isTRUE(input$transform_data_change_type_choices=="factor")) {
				rv_current$working_df[[rv_current$selected_var]] = numeric_factor(rv_current$working_df[[rv_current$selected_var]])
			} else if (isTRUE(rv_current$vartype=="numeric") & isTRUE(input$transform_data_change_type_choices=="character")) {
				rv_current$working_df[[rv_current$selected_var]] = numeric_character(rv_current$working_df[[rv_current$selected_var]])
			} else if (isTRUE(rv_current$vartype=="factor") & isTRUE(input$transform_data_change_type_choices=="numeric")) {
				rv_current$working_df[[rv_current$selected_var]] = factor_numeric(rv_current$working_df[[rv_current$selected_var]])
			} else if (isTRUE(rv_current$vartype=="factor") & isTRUE(input$transform_data_change_type_choices=="character")) {
				rv_current$working_df[[rv_current$selected_var]] = sjlabelled::as_character(rv_current$working_df[[rv_current$selected_var]])
			} else if (isTRUE(rv_current$vartype=="character") & isTRUE(input$transform_data_change_type_choices=="factor")) {
				rv_current$working_df[[rv_current$selected_var]] = sjlabelled::as_factor(rv_current$working_df[[rv_current$selected_var]])
			} else if (isTRUE(rv_current$vartype=="character") & isTRUE(input$transform_data_change_type_choices=="numeric")) {
				rv_current$working_df[[rv_current$selected_var]] = factor_numeric(rv_current$working_df[[rv_current$selected_var]])
			}
			rv_current$data = rv_current$working_df
			rv_current$changed_variable_type_log = c(rv_current$changed_variable_type_log, transfun(rv_current$selected_var, rv_current$vartype, input$transform_data_change_type_choices))
			if (isTRUE(!is.null(rv_current$changed_variable_type_log))) {
				output$transform_data_variable_type_log_ui = renderUI({
					p( hr()
						, HTML(paste0("<b>", get_rv_labels("variable_type_log"), "</b>"))
					)
				})
				output$transform_data_variable_type_log = renderPrint({
					cat(rv_current$changed_variable_type_log, sep="\n")
				})
			} else {
				output$transform_data_variable_type_log = NULL
				output$transform_data_variable_type_log_ui = NULL
			}
		}
	})

	#### Reset on language change
	observeEvent(c(input$change_language, input$dataset_id), {
		updateSelectInput(session = session,  "transform_data_select_vars", selected = "", choices = NULL)
		rv_current$selected_var = NULL
	})
}

#### ----- Rename variables -------------------------------------------- ####

transform_data_rename_variables_server = function() {
	observe({ 
		if (isTRUE(!is.null(rv_current$selected_var))) {
		  output$transform_data_rename_variable = renderUI({
				materialSwitch(
						inputId = "transform_data_rename_variable_check",
						label = get_rv_labels("rename_variable"),
						status = "success",
						right = TRUE
				 )
		  })
		} else {
			output$transform_data_rename_variable = NULL
		}
	})

	observe({
		if (isTRUE(input$transform_data_rename_variable_check)) {
			output$transform_data_rename_variable_input = renderUI({
          textInput("transform_data_rename_variable_input"
            , label = NULL
				, value=""
            , placeholder = paste0(input$transform_data_select_vars, "_new")
            , width = "100%"
          )
			})
		} else {
			output$transform_data_rename_variable_input = NULL
		}
	})

	observeEvent(input$transform_data_apply, {
		if (isTRUE(!is.null(input$transform_data_rename_variable_input)) & isTRUE(input$transform_data_rename_variable_input!="") & isTRUE(input$transform_data_rename_variable_check)) {
			rv_current$renamed_variable_log = c(rv_current$renamed_variable_log
				, paste0("{ ", input$transform_data_select_vars, " ----> ", input$transform_data_rename_variable_input, " }")
			)
			output$transform_data_renamed_variable_log_ui = renderUI({
				p( hr()
					, HTML(paste0("<b>", get_rv_labels("renamed_variable_log"), "</b>"))
				)
			})
			output$transform_data_renamed_variable_log = renderPrint({
				cat(rv_current$renamed_variable_log, sep="\n")
			})
			rv_current$working_df = (rv_current$working_df
				|> Rautoml::rename_vars(input$transform_data_select_vars, input$transform_data_rename_variable_input)
			)
			rv_current$data = rv_current$working_df
			rv_current$selected_var = input$transform_data_rename_variable_input
			rv_current$selected_vars = colnames(rv_current$working_df)
			updateSelectInput(session = session,  "transform_data_select_vars", selected = rv_current$selected_var, choices = rv_current$selected_vars)
			updateTextAreaInput(session=session, "transform_data_rename_variable_input", value="")
		}
	})
}


#### ---- Change value labels -------------------------------------------

transform_data_quick_explore_recode_server = function() {
	observe({ 
		if (isTRUE(input$transform_data_select_vars!="") & any(rv_current$vartype %in% recode_var_types)) {
		  output$transform_data_recode_variable = renderUI({
				materialSwitch(
						inputId = "transform_data_recode_variable_check",
						label = get_rv_labels("recode_variable"),
						status = "success",
						right = TRUE
				 )
		  })
		} else {
			output$transform_data_recode_variable = NULL
		}
	})

	observe({
		if (isTRUE(input$transform_data_select_vars!="") & isTRUE(input$transform_data_recode_variable_check) & any(rv_current$vartype %in% recode_var_types)) {
			old_choices = extract_value_labels(rv_current$working_df, rv_current$selected_var)
			output$transform_data_recode_variable_choices = renderUI({
				selectInput("transform_data_recode_variable_choices"
					, label = get_rv_labels("recode_variable_old_label") 
					, choices = old_choices
					, multiple=TRUE
				)
			})
		} else {
			output$transform_data_recode_variable_choices = NULL
		}
	})

	observe({
		if (isTRUE(input$transform_data_select_vars!="") & isTRUE(input$transform_data_recode_variable_check) & any(rv_current$vartype %in% recode_var_types)) {
			output$transform_data_recode_variable_input = renderUI({
			 textInput("transform_data_recode_variable_input"
				, label = get_rv_labels("recode_variable_new_label")
				, value=NULL
				, placeholder = get_rv_labels("recode_variable_new_label_ph")
				, width = "100%"
			 )
			})
		} else {
			output$transform_data_recode_variable_input = NULL
		}
	})

	observeEvent(input$transform_data_apply, {
		if (isTRUE(input$transform_data_recode_variable_check) & any(rv_current$vartype %in% recode_var_types)) {
			req(input$transform_data_recode_variable_input)
			rv_current$recoded_variable_labels_log = c(rv_current$recoded_variable_labels_log
				, paste0("{ ", rv_current$selected_var, ": ", input$transform_data_recode_variable_choices, " ----> ", input$transform_data_recode_variable_input, " }")
			)
			output$transform_data_recoded_variable_labels_log_ui = renderUI({
				p( hr()
					, HTML(paste0("<b>", get_rv_labels("recoded_variable_labels_log"), "</b>"))
				)
			})
			output$transform_data_recoded_variable_labels_log = renderPrint({
				cat(rv_current$recoded_variable_labels_log, sep="\n")
			})
			rv_current$working_df = (rv_current$working_df
				|> mutate_at(rv_current$selected_var, function(x){
					x = recode_value_labels(x
						, old = input$transform_data_recode_variable_choices
						, new = input$transform_data_recode_variable_input
					)
				})
			)
			rv_current$data = rv_current$working_df
			updateCheckboxInput(session = session, "transform_data_recode_variable_check", value = FALSE)
			updateTextAreaInput(session=session, "transform_data_recode_variable_input", value=NULL)
		}
	})
}



#### ----- Create missing values ---------------------------------------
transform_data_create_missing_values_server = function() {

	## Switch for creating missing values
	observe({
		if (isTRUE(!is.null(rv_current$selected_var))) {
			output$transform_data_create_missing_values = renderUI({
				materialSwitch(
						inputId = "transform_data_create_missing_values_check",
						label = get_rv_labels("create_missing_values"),
						status = "success",
						right = TRUE
				 )
			})
		} else {
			output$transform_data_create_missing_values = NULL
			updateMaterialSwitch(session=session, "transform_data_create_missing_values_check", value=FALSE)
		}
	})

	## Options for entire dataset or single variable
	observe({
		if (isTRUE(input$transform_data_create_missing_values_check) & isTRUE(!is.null(rv_current$selected_var))) {
 			output$transform_data_create_missing_values_choices = renderUI({
 				empty_lab = ""
 				names(empty_lab) = get_rv_labels("transform_data_create_missing_values_choices_ph")
 				selectInput(
 					inputId = "transform_data_create_missing_values_choices"
 					, label = NULL
 					, selected = ""
 					, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language, "transform_data_create_missing_values_choices"))
 				)
 			})
		} else {
 			output$transform_data_create_missing_values_choices = NULL
			updateSelectInput(session = session, inputId = "transform_data_create_missing_values_choices", selected = "")
		}
	})


	## Options for creating missing values
	observe({
		if (isTRUE(input$transform_data_create_missing_values_choices!="") & (isTRUE(!is.null(input$transform_data_create_missing_values_choices))) & isTRUE(input$transform_data_create_missing_values_check) & isTRUE(!is.null(rv_current$selected_var))) {
  				output$transform_data_create_missing_values_options = renderUI({
  					radioButtons("transform_data_create_missing_values_options"
  						, label = get_rv_labels("transform_data_create_missing_values_options")
  						, choices = get_named_choices(input_choices_file, input$change_language, "transform_data_create_missing_values_options")
  						, selected = character(0)
  						, inline = TRUE
  					)
  				})
		} else {
			output$transform_data_create_missing_values_options = NULL
			updateRadioButtons(session = session, inputId = "transform_data_create_missing_values_options", selected = character(0))
		}
	})

	## Fields for inputing missing value patterns
	observe({
		if (isTRUE(input$transform_data_create_missing_values_check) & isTRUE(length(input$transform_data_create_missing_values_options)>0)) {
			if (isTRUE(input$transform_data_create_missing_values_options=="Use patterns")) {
				output$transform_data_create_missing_values_input = renderUI({
					 #### FIXME: Default value is set ""
					 textInput("transform_data_create_missing_values_input"
						, label = NULL
						, value=""
						, placeholder = get_rv_labels("transform_data_create_missing_values_input_pattern")
						, width = "100%"
					 )
				})
				output$transform_data_create_missing_values_input_numeric = NULL
				updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input_numeric", selected = NULL)
				output$transform_data_create_missing_values_input_range = NULL
				updateCheckboxInput(session = session, inputId = "transform_data_create_missing_values_input_range", value = FALSE)
			} else if (isTRUE(input$transform_data_create_missing_values_options=="Use categories")) {
					if (isTRUE(input$transform_data_create_missing_values_choices=="For this variable")) {
						if (isTRUE(rv_current$vartype=="numeric")) {
							output$transform_data_create_missing_values_input = renderUI({
								textInput(inputId = "transform_data_create_missing_values_input"
									, label = get_rv_labels("transform_data_create_missing_values_input_numeric")
									, placeholder = get_rv_labels("transform_data_create_missing_values_input_range_ph")
									, value = NULL
								)
							})
							
							output$transform_data_create_missing_values_input_range = renderUI({
								checkboxInput("transform_data_create_missing_values_input_range"
									, label = get_rv_labels("transform_data_create_missing_values_input_range")
									, value = FALSE
								)
							})
							output$transform_data_create_missing_values_input_numeric = NULL
							updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input_numeric", selected = NULL)
						} else if (isTRUE(any(rv_current$vartype %in% recode_var_types))) {
							empty_lab = ""
							names(empty_lab) = get_rv_labels("transform_data_create_missing_values_input_categories_ph")
							output$transform_data_create_missing_values_input = renderUI({
								selectInput(
									inputId = "transform_data_create_missing_values_input"
									, label = NULL
									, selected = NULL
									, choices = c(empty_lab, extract_value_labels(rv_current$working_df, rv_current$selected_var))
									, multiple = TRUE
								)
							})
							output$transform_data_create_missing_values_input_numeric = NULL
							updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input_numeric", selected = NULL)
						} else {
							output$transform_data_create_missing_values_input = NULL
							updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input", selected = NULL)
							output$transform_data_create_missing_values_input_numeric = NULL
							updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input_numeric", selected = NULL)
							
							output$transform_data_create_missing_values_input_range = NULL
							updateCheckboxInput(session = session, inputId = "transform_data_create_missing_values_input_range", value = FALSE)
						}
					} else if (isTRUE(input$transform_data_create_missing_values_choices=="For all variables")) {
						output$transform_data_create_missing_values_input = renderUI({
							textInput(inputId = "transform_data_create_missing_values_input"
								, label = get_rv_labels("transform_data_create_missing_values_input")
								, placeholder = get_rv_labels("transform_data_create_missing_values_input_ph")
								, value = NULL
							)
						})

						output$transform_data_create_missing_values_input_numeric = renderUI({
							textInput(inputId = "transform_data_create_missing_values_input_numeric"
								, label = get_rv_labels("transform_data_create_missing_values_input_numeric")
								, placeholder = get_rv_labels("transform_data_create_missing_values_input_range_ph")
								, value = NULL
							)
						})
					
						output$transform_data_create_missing_values_input_range = renderUI({
							checkboxInput("transform_data_create_missing_values_input_range"
								, label = get_rv_labels("transform_data_create_missing_values_input_range")
								, value = FALSE
							)
						})
					} else {
						output$transform_data_create_missing_values_input = NULL
						updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input", selected = NULL)
						output$transform_data_create_missing_values_input_numeric = NULL
						updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input_numeric", selected = NULL)

						output$transform_data_create_missing_values_input_range = NULL
						updateCheckboxInput(session = session, inputId = "transform_data_create_missing_values_input_range", value = FALSE)
					}
			} else {
				output$transform_data_create_missing_values_input = NULL
				updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input", selected = NULL)
				output$transform_data_create_missing_values_input_numeric = NULL
				updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input_numeric", selected = NULL)
						output$transform_data_create_missing_values_input_range = NULL
						updateCheckboxInput(session = session, inputId = "transform_data_create_missing_values_input_range", value = FALSE)
			}
		} else {
			output$transform_data_create_missing_values_input = NULL
			updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input", selected = NULL)
			output$transform_data_create_missing_values_input_numeric = NULL
			updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input_numeric", selected = NULL)
						output$transform_data_create_missing_values_input_range = NULL
						updateCheckboxInput(session = session, inputId = "transform_data_create_missing_values_input_range", value = FALSE)
		}
	})

	## Activate range check for numeric values
	observe({
	
	})

	#### Apply Creation of missing values

	observeEvent(input$transform_data_apply, {

		if (isTRUE(input$transform_data_create_missing_values_check)) {
			if (isTRUE(input$transform_data_create_missing_values_options=="Use patterns")) {
				if (isTRUE(!is.null(input$transform_data_create_missing_values_input))) {
					if (isTRUE(input$transform_data_create_missing_values_choices=="For this variable")) {
						current_var = rv_current$selected_var
						rv_current$working_df = na_if_pattern(rv_current$working_df
							, rv_current$selected_var
							, pattern = input$transform_data_create_missing_values_input
							, replacement = NA_character_
						)
						recode_result = input$transform_data_create_missing_values_input
					} else {
						rv_current$working_df = na_if_pattern(rv_current$working_df
							, rv_current$selected_var
							, pattern = input$transform_data_create_missing_values_input
							, replacement = NA_character_
							, apply_all = TRUE
						)
						recode_result = input$transform_data_create_missing_values_input
						current_var = colnames(rv_current$working_df)
					}
				}
			} else if (isTRUE(input$transform_data_create_missing_values_options=="Use categories")) {
				if (isTRUE(!is.null(input$transform_data_create_missing_values_input))) {
					if (isTRUE(input$transform_data_create_missing_values_choices=="For this variable")) {
						current_var = rv_current$selected_var
						rv_current$working_df = na_if_category_single(rv_current$working_df
							, rv_current$selected_var
							, input$transform_data_create_missing_values_input
							, range = input$transform_data_create_missing_values_input_range
						)
						recode_result = input$transform_data_create_missing_values_input
					} else {
						rv_current$working_df = na_if_category_multiple(rv_current$working_df
							, numeric_labels = input$transform_data_create_missing_values_input_numeric
							, categorical_labels = input$transform_data_create_missing_values_input
							, range = isTRUE(input$transform_data_create_missing_values_input_range)
						)
						recode_result = paste0(input$transform_data_create_missing_values_input, "; ", input$transform_data_create_missing_values_input_numeric)
						current_var = colnames(rv_current$working_df)
					}
				}
			}
			
			rv_current$created_missing_values_log = c(rv_current$created_missing_values_log
				, paste0("{ ", current_var, ": {", recode_result, "} ----> ", NA, " }")
			)
			output$transform_data_created_missing_values_log_ui = renderUI({
				p( hr()
					, HTML(paste0("<b>", get_rv_labels("created_missing_values_log"), "</b>"))
				)
			})
			output$transform_data_created_missing_values_log = renderPrint({
				cat(rv_current$created_missing_values_log, sep="\n")
			})	
		}
		rv_current$missing_prop = missing_prop(rv_current$working_df)
		updateMaterialSwitch(session=session, "transform_data_create_missing_values_check", value=FALSE)

		updateSelectInput(session = session, inputId = "transform_data_create_missing_values_choices", selected = "")
		output$transform_data_create_missing_values_choices = NULL

		updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input", selected = NULL)
		output$transform_data_create_missing_values_input = NULL

		updateRadioButtons(session = session, inputId = "transform_data_create_missing_values_options", selected = character(0))
		output$transform_data_create_missing_values_options = NULL
		
		updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input", selected = NULL)
		output$transform_data_create_missing_values_input = NULL

		updateCheckboxInput(session = session, inputId = "transform_data_create_missing_values_input_range", value = FALSE)
		output$transform_data_create_missing_values_input_range = NULL


		updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input_numeric", selected = NULL)
		output$transform_data_create_missing_values_input_numeric = NULL

	})

}

###### ----- Identify outliers -------------------------------------------- ####

transform_data_identify_outliers_server = function() {
	
	observe({
		if (isTRUE(!is.null(rv_current$selected_var)) & isTRUE(rv_current$vartype=="numeric")) {
 			output$transform_data_identify_outliers = renderUI({
 				materialSwitch(
 						inputId = "transform_data_identify_outliers_check",
 						label = get_rv_labels("identify_outliers"),
 						status = "success",
 						right = TRUE
 				 )
 			})
		} else {
			output$transform_data_identify_outliers = NULL
			updateMaterialSwitch(session=session, "transform_data_identify_outliers_check", value=FALSE)
			## README
			updateSelectInput(session = session, inputId = "transform_data_handle_outliers_choices", selected = "")
			output$transform_data_handle_outliers_choices = NULL
		}
	})
	
	observe({
		if (isTRUE(!is.null(rv_current$selected_var))) {
			if (isTRUE(input$transform_data_identify_outliers_check)) {
				rv_current$outlier_values = get_outliers(rv_current$working_df, rv_current$selected_var)
				if (isTRUE(length(rv_current$outlier_values)>0)) {
					output$transform_data_handle_outliers_choices = renderUI({
						empty_lab = ""
						names(empty_lab) = get_rv_labels("transform_data_handle_outliers_choices_ph")
						selectInput(
							inputId = "transform_data_handle_outliers_choices"
							, label = NULL
							, selected = ""
							, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language, "transform_data_handle_outliers_choices"))
						)
					})

				} else {
					updateSelectInput(session = session, inputId = "transform_data_handle_outliers_choices", selected = "")
					output$transform_data_handle_outliers_choices = NULL
				}
			} else {
				
				updateSelectInput(session = session, inputId = "transform_data_handle_outliers_choices", selected = "")
				output$transform_data_handle_outliers_choices = NULL
			}
		}
	})

	observe({

		if (isTRUE(input$transform_data_identify_outliers_check)) {
			if (isTRUE(rv_current$vartype=="numeric")) {
				output$transform_data_handle_outliers_log_ui = renderUI({
					p( hr()
						, HTML(paste0("<b>", get_rv_labels("outlier_values_log"), "</b>"))
					)
				})
				if (isTRUE(length(rv_current$outlier_values)>0)) {
					output$transform_data_handle_outliers_log = renderPrint({
						cat(rv_current$selected_var, ":", paste0(rv_current$outlier_values, sep=","))
					})
				} else {
					output$transform_data_handle_outliers_log = renderPrint({
						cat(rv_current$selected_var, ":", get_rv_labels("transform_data_handle_outliers_log_none"))
					})
				}
			} else {
				output$transform_data_handle_outliers_log_ui = NULL
				output$transform_data_handle_outliers_log = NULL
			}
		} else {
			output$transform_data_handle_outliers_log_ui = NULL
			output$transform_data_handle_outliers_log = NULL
		}
	})

	observe({
		if (isTRUE(input$transform_data_identify_outliers_check) & (isTRUE(length(rv_current$outlier_values)>0))) {
			if (isTRUE(isTRUE(input$transform_data_handle_outliers_choices=="correct"))) {
			correct_options = get_named_choices(input_choices_file, input$change_language, "transform_data_handle_outliers_correct_options")
				output$transform_data_handle_outliers_correct_options = renderUI({
					radioButtons(
						inputId = "transform_data_handle_outliers_correct_options"
						, label = get_rv_labels("transform_data_handle_outliers_correct_options")
						, selected = correct_options[1]
						, choices = correct_options 
						, inline =TRUE
					)
				})
			} else {
				output$transform_data_handle_outliers_correct_options = NULL
				updateRadioButtons(session = session, inputId = "transform_data_handle_outliers_correct_options", selected  = character(0)) 
			}
		} else {
			output$transform_data_handle_outliers_correct_options = NULL
			updateRadioButtons(session = session, inputId = "transform_data_handle_outliers_correct_options", selected  = character(0)) 
		}
	})

	observe({
		if (isTRUE(input$transform_data_identify_outliers_check) & (isTRUE(length(rv_current$outlier_values)>0))) {
			if (isTRUE(input$transform_data_handle_outliers_correct_options=="input value")) {
				output$transform_data_handle_outliers_correct_options_input = renderUI({
					numericInput("transform_data_handle_outliers_correct_options_input"
						, label = get_rv_labels("transform_data_handle_outliers_correct_options_input")
						, value = NULL
					)
				})
			} else {
				output$transform_data_handle_outliers_correct_options_input = NULL
				updateNumericInput(session = session, inputId = "transform_data_handle_outliers_correct_options_input", value = NULL)
			}
		} else {
				output$transform_data_handle_outliers_correct_options_input = NULL
				updateNumericInput(session = session, inputId = "transform_data_handle_outliers_correct_options_input", value = NULL)
		}
	})

	observeEvent(input$transform_data_apply, {
		if (isTRUE(input$transform_data_handle_outliers_choices=="drop")) {
			rv_current$working_df = (rv_current$working_df
				|> handle_outliers(var=rv_current$selected_var
					, outliers = rv_current$outlier_values
					, action = input$transform_data_handle_outliers_choices
				)
			)
			outliers_result = paste0(rv_current$outlier_values, " ----> ", input$transform_data_handle_outliers_choices)
		} else if (isTRUE(input$transform_data_handle_outliers_choices=="correct")) {
			if (isTRUE(any(input$transform_data_handle_outliers_correct_options %in% c("mean", "median")))) {
				rv_current$working_df = handle_outliers(rv_current$working_df
					, var=rv_current$selected_var
					, outliers = rv_current$outlier_values
					, action = input$transform_data_handle_outliers_choices
					, new_values = NULL
					, fun = input$transform_data_handle_outliers_correct_options
				)
				outliers_result = paste0(rv_current$outlier_values, " ----> ", input$transform_data_handle_outliers_correct_options)
			} else if (isTRUE(input$transform_data_handle_outliers_correct_options=="input value")) {
				if (isTRUE(!is.null(input$transform_data_handle_outliers_correct_options_input))) {
					rv_current$working_df = handle_outliers(rv_current$working_df
						, var=rv_current$selected_var
						, outliers = rv_current$outlier_values
						, action = input$transform_data_handle_outliers_choices
						, new_values = input$transform_data_handle_outliers_correct_options_input
						, fun = NULL 
					)
					outliers_result = paste0(rv_current$outlier_values, " ----> ", input$transform_data_handle_outliers_correct_options_input)
				}
			}
		} else if (isTRUE(input$transform_data_handle_outliers_choices=="nothing")) {
			outliers_result = paste0(rv_current$outlier_values, " ----> ", input$transform_data_handle_outliers_choices)
		}
		
		if (isTRUE(any(input$transform_data_handle_outliers_choices %in% c("drop", "correct", "nothing")))) {
			rv_current$handle_outlier_values_log = c(rv_current$handle_outlier_values_log
				, paste0("{ ", rv_current$selected_var, ": {", outliers_result, " } }")
			)
			output$transform_data_handle_outlier_values_log_ui = renderUI({
				p( hr()
					, HTML(paste0("<b>", get_rv_labels("handle_outlier_values_log"), "</b>"))
				)
			})
			output$transform_data_handle_outlier_values_log = renderPrint({
				cat(rv_current$handle_outlier_values_log, sep="\n")
			})
		}
		rv_current$outlier_values = NULL
		updateMaterialSwitch(session=session, "transform_data_identify_outliers_check", value=FALSE)
		output$transform_data_handle_outliers_log_ui = NULL
		output$transform_data_handle_outliers_log = NULL
		updateSelectInput(session = session, inputId = "transform_data_handle_outliers_choices", selected = "")
		output$transform_data_handle_outliers_choices = NULL
		output$transform_data_handle_outliers_correct_options = NULL
		updateRadioButtons(session = session, inputId = "transform_data_handle_outliers_correct_options", selected  = character(0)) 
		output$transform_data_handle_outliers_correct_options_input = NULL
		updateNumericInput(session = session, inputId = "transform_data_handle_outliers_correct_options_input", value = NULL)
	})
}


#### ---- Handle missing values -----------------------------------------

transform_data_handle_missing_values_server = function() {
	
	## Generate missing data summary
	observe({
		if (isTRUE(!is.null(rv_current$selected_var)) & (isTRUE(!is.null(rv_current$working_df)))) {
			
			if (!isTRUE(input$explore_data_missingness_check)) {
				rv_current$missing_prop = missing_prop(rv_current$working_df)
			}
			
			if (NROW(rv_current$missing_prop)) {
				rv_current$missing_prop_df = (rv_current$missing_prop
					|> filter_missing_values_df(column="missing"
						, var_column="variable"
						, var=rv_current$selected_var
					)
				)
			}
		}
	})

	## Create a conditional switch for missing data
	observe({
		if (isTRUE(any(rv_current$selected_var %in% rv_current$missing_prop_df$variable))) {
				output$transform_data_handle_missing_values = renderUI({
					materialSwitch(
							inputId = "transform_data_handle_missing_values_check",
							label = get_rv_labels("handle_missing_values"),
							status = "success",
							right = TRUE
					 )
				})
				rv_current$has_missing_data_check = TRUE
				output$transform_data_handle_missing_values_ui = renderUI({
				 HTML(paste0("<b>", get_rv_labels("handle_missing_values_ui"), "</b>"))
				})
				output$transform_data_handle_missing_values_out = renderPrint({
					print(rv_current$missing_prop_df)
				})	
		} else {
			rv_current$has_missing_data_check = FALSE
			updateMaterialSwitch(session=session, "transform_data_handle_missing_values_check", value=FALSE)
			output$transform_data_handle_missing_values = NULL
			output$transform_data_handle_missing_values_ui = NULL
			output$transform_data_handle_missing_values_out = NULL
		}
	})
	
	## Options to handle missing data
	observe({
		if (isTRUE(input$transform_data_handle_missing_values_check) & isTRUE(rv_current$has_missing_data_check)) {
			empty_lab = ""
			names(empty_lab) = get_rv_labels("transform_data_handle_missing_values_choices_ph")
			output$transform_data_handle_missing_values_choices = renderUI({
				selectInput(
					inputId = "transform_data_handle_missing_values_choices"
					, label = NULL
					, selected = ""
					, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language, "transform_data_handle_missing_values_choices"))
				)
			})
		} else {
			updateSelectInput(session = session,  "transform_data_handle_missing_values_choices", selected = "")
			output$transform_data_handle_missing_values_choices = NULL
		}
	})

	## Options to identify missing values
	observe({
		if (isTRUE(input$transform_data_handle_missing_values_check) & isTRUE(any(input$transform_data_handle_missing_values_choices %in% c("Create new category (for all)",  "Create new category (for this)"))) & isTRUE(rv_current$has_missing_data_check)) {
			missing_indicators = get_named_choices(input_choices_file, input$change_language, "transform_data_handle_missing_values_options")
			output$transform_data_handle_missing_values_options = renderUI({
				checkboxGroupInput(
					"transform_data_handle_missing_values_options"
					, label = get_rv_labels("transform_data_handle_missing_values_options")
					, choices = missing_indicators
					, selected = character(0)
					, inline = TRUE
				)
			})
		} else {
			output$transform_data_handle_missing_values_options = NULL
			updateCheckboxGroupInput(session = session, "transform_data_handle_missing_values_options", selected=character(0))
		}
	})

	## Handle categorical variables
	observe({
		if (isTRUE(rv_current$has_missing_data_check) & isTRUE(input$transform_data_handle_missing_values_check) & isTRUE(length(input$transform_data_handle_missing_values_options)>0)) {
			
			if (isTRUE(any(input$transform_data_handle_missing_values_choices %in% c("Create new category (for this)")))) {
				
				if (isTRUE(any(rv_current$vartype %in% recode_var_types))) {
					output$transform_data_handle_missing_values_new_category = transform_data_handle_missing_values_categoricalUI(label = get_rv_labels("transform_data_handle_missing_values_new_category")
						, value=""
						, placeholder = get_rv_labels("transform_data_handle_missing_values_new_category_ph")
					)
				} else {
					output$transform_data_handle_missing_values_new_category = NULL
					updateTextInput(session = session, inputId = "transform_data_handle_missing_values_new_category", value = "")
				}

				if (isTRUE(any(rv_current$vartype %in% "numeric"))) {
					output$transform_data_handle_missing_values_new_numeric = transform_data_handle_missing_values_numericUI(label = get_rv_labels("transform_data_handle_missing_values_new_numeric")
						, value=get_rv_labels("transform_data_handle_missing_values_new_numeric_ph")
					)
				} else {
					output$transform_data_handle_missing_values_new_numeric = NULL
					updateNumericInput(session = session, inputId = "transform_data_handle_missing_values_new_numeric", value = NULL)
				}
				
			} else if (isTRUE(any(input$transform_data_handle_missing_values_choices %in% c("Create new category (for all)")))) {
					output$transform_data_handle_missing_values_new_category = transform_data_handle_missing_values_categoricalUI(label = get_rv_labels("transform_data_handle_missing_values_new_category")
						, value=""
						, placeholder = get_rv_labels("transform_data_handle_missing_values_new_category_ph")
					)
					
					output$transform_data_handle_missing_values_new_numeric = transform_data_handle_missing_values_numericUI(label = get_rv_labels("transform_data_handle_missing_values_new_numeric")
						, value=get_rv_labels("transform_data_handle_missing_values_new_numeric_ph")
					)
				
			} else {
				output$transform_data_handle_missing_values_new_category = NULL
				updateTextInput(session = session, inputId = "transform_data_handle_missing_values_new_category", value = "")
					
				output$transform_data_handle_missing_values_new_numeric = NULL
				updateNumericInput(session = session, inputId = "transform_data_handle_missing_values_new_numeric", value = NULL)
			}

		} else {
			output$transform_data_handle_missing_values_new_category = NULL
			updateTextInput(session = session, inputId = "transform_data_handle_missing_values_new_category", value = "")
				
			output$transform_data_handle_missing_values_new_numeric = NULL
			updateNumericInput(session = session, inputId = "transform_data_handle_missing_values_new_numeric", value = NULL)
		}
	})


	## Apply handling missing data changes
	observeEvent(input$transform_data_apply, {
		if (isTRUE(rv_current$has_missing_data_check) & isTRUE(input$transform_data_handle_missing_values_check)) {
			if (isTRUE(input$transform_data_handle_missing_values_choices=="Create new category (for this)")) {
			
				current_var = rv_current$selected_var
				if (isTRUE(!is.null(input$transform_data_handle_missing_values_new_category)) & isTRUE(input$transform_data_handle_missing_values_new_category!="") & (isTRUE(any(rv_current$vartype %in% recode_var_types)))) {
					rv_current$working_df = (rv_current$working_df
						|> Rautoml::na_to_values(var=rv_current$selected_var
							, na_pattern = input$transform_data_handle_missing_values_options
							, new_category = input$transform_data_handle_missing_values_new_category
							, apply_all = FALSE
						)
					)
					recode_result = input$transform_data_handle_missing_values_new_category
				} else if (isTRUE(!is.null(input$transform_data_handle_missing_values_new_numeric)) & isTRUE(input$transform_data_handle_missing_values_new_numeric!="") & (isTRUE(rv_current$vartype=="numeric"))) {
					rv_current$working_df = (rv_current$working_df
						|> Rautoml::na_to_values(var=rv_current$selected_var
							, na_pattern = input$transform_data_handle_missing_values_options
							, new_category = input$transform_data_handle_missing_values_new_numeric
							, apply_all = FALSE
						)
					)
				recode_result = input$transform_data_handle_missing_values_new_numeric
				}
				
			} else if (isTRUE(input$transform_data_handle_missing_values_choices=="Create new category (for all)")) {
				rv_current$working_df = (rv_current$working_df
					|> Rautoml::na_to_values(var=rv_current$selected_var
						, na_pattern = input$transform_data_handle_missing_values_options
						, new_category = input$transform_data_handle_missing_values_new_category
						, new_numeric = input$transform_data_handle_missing_values_new_numeric
						, apply_all = TRUE
					)
				)
				recode_result = paste0("categorical: ", input$transform_data_handle_missing_values_new_category, "; ", "numeric: ", input$transform_data_handle_missing_values_new_numeric)
				current_var = colnames(rv_current$working_df)
			} else if (isTRUE(input$transform_data_handle_missing_values_choices=="Drop missing (for all)")) {
				rv_current$working_df = (rv_current$working_df
					|> Rautoml::drop_missing_values()
				)
				current_var = colnames(rv_current$working_df)
				recode_result = input$transform_data_handle_missing_values_choices 
			} else if (isTRUE(input$transform_data_handle_missing_values_choices=="Drop missing (for this)")) {
				rv_current$working_df = (rv_current$working_df
					|> Rautoml::drop_missing_values(rv_current$selected_var)
				)
				current_var = rv_current$selected_var 
				recode_result = input$transform_data_handle_missing_values_choices 
			}
		
			rv_current$handle_missing_values_log = c(rv_current$handle_missing_values_log
				, paste0("{ ", current_var, ": {", "NA ----> ", recode_result, "}", " }")
			)
			output$transform_data_handle_missing_values_log_ui = renderUI({
				p( hr()
					, HTML(paste0("<b>", get_rv_labels("handle_missing_values_log"), "</b>"))
				)
			})
			output$transform_data_handle_missing_values_log = renderPrint({
				cat(rv_current$handle_missing_values_log, sep="\n")
			})	
		}	

#		rv_current$has_missing_data_check = FALSE
		updateMaterialSwitch(session=session, "transform_data_handle_missing_values_check", value=FALSE)
		
		updateSelectInput(session = session,  "transform_data_handle_missing_values_choices", selected = "")
		output$transform_data_handle_missing_values_choices = NULL

		updateCheckboxGroupInput(session = session, "transform_data_handle_missing_values_options", selected=character(0))
		output$transform_data_handle_missing_values_options = NULL
				
		updateTextInput(session = session, inputId = "transform_data_handle_missing_values_new_category", value = "")
		updateTextInput(session = session, inputId = "transform_data_handle_missing_values_new_category", value = NULL)
		output$transform_data_handle_missing_values_new_category = NULL
	})
}


#### ---- Plot single variables in transform data --------------------------------
transform_data_quick_explore_plot_server = function() {
		
		observe({
			req(rv_current$selected_var)
			dd = try(
				(rv_current$working_df
				 |> select(all_of(rv_current$selected_var))
			  )
			  , silent=TRUE
			 )
			if (!is.data.frame(dd) | is.null(dd) | any(class(dd) %in% "try-error") | !NROW(dd)) {
				if (isTRUE(is.null(rv_current$selected_var)) & isTRUE(input$transform_data_apply==1)) {
				dd = try(
					(rv_current$working_df
					 |> select(all_of(rv_current$selected_var))
				  )
				  , silent=TRUE
				 )
				} 
			}

			if (isTRUE(!is.null(dd)) & is.data.frame(dd) & !any(class(dd) %in% "try-error")) {
				rv_current$transform_data_plot_df = dd
				rv_current$transform_data_quick_explore_out = generate_data_summary(rv_current$transform_data_plot_df)
				output$transform_data_quick_explore_ui = renderUI({
				 HTML(paste0("<b>", get_rv_labels("quick_explore_ui"), "</b>"))
				})
				output$transform_data_quick_explore_out = renderPrint({
				 print(rv_current$transform_data_quick_explore_out)
				})
				rv_current$transform_data_quick_plot_out = ggunivariate(rv_current$transform_data_plot_df, rv_current$vartype)
				output$transform_data_quick_plot_out = renderPlotly({
				 rv_current$transform_data_quick_plot_out
				})
			}
		})
}


transform_data_plot_missing_data_server = function() {
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			rv_current$transform_data_plot_missing_data_out = missing_data_plot(rv_current$working_df)
				output$transform_data_plot_missing_data_out_ui = renderUI({
				 HTML(paste0("<b>", get_rv_labels("transform_data_plot_missing_data_out"), "</b>"))
				 })
				output$transform_data_plot_missing_data_out = renderPlot({
					rv_current$transform_data_plot_missing_data_out
				})
			
		}
	})
}
