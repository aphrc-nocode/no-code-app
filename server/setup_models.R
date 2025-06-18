#### ---- AI/ML UIs ---------------------------- ####
setup_models_ui = function() {
	
	## Session name
	observeEvent(input$manage_data_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			output$setup_models_analysis_session_name = renderUI({
				textInput("setup_models_analysis_session_name"
					, label = get_rv_labels("setup_models_analysis_session_name")
					, value = ""
					, width = NULL
					, placeholder = get_rv_labels("setup_models_analysis_session_name_ph")
				)	
			})	
		} else {
			output$setup_models_analysis_session_name = NULL
			updateTextInput(session , "setup_models_analysis_session_name", value="")
		}
	})

	## Target variable
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
				rv_current$vartype_all = Rautoml::get_types(rv_current$working_df)
				output$setup_models_analysis_target_variable_options = renderUI({
					prettyRadioButtons(
						inputId = "setup_models_analysis_target_variable_check"
							, label = get_rv_labels("setup_models_analysis_target_variable_options_check")
							, choices = get_named_choices(input_choices_file, input$change_language, "setup_models_analysis_target_variable_options_check")
							, selected = "yes"
							, inline = TRUE
							, status = "success"
					)
				})

			} else {
				output$setup_models_analysis_target_variable_options = NULL
				updatePrettyRadioButtons(session=session, inputId="setup_models_analysis_target_variable_options_check", selected=character(0))
			}
		} else {
			output$setup_models_analysis_target_variable_options = NULL	
			updatePrettyRadioButtons(session=session, inputId="setup_models_analysis_target_variable_options_check", selected=character(0))
		}
	})

	## Seed value
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
				output$setup_models_analysis_session_seed = renderUI({
					numericInput("setup_models_analysis_session_seed"
						, label = get_rv_labels("setup_models_analysis_session_seed")
						, value = 123
						, min = 1
						, max = 99999999
						, width = NULL
					)	
				})	
				
			} else {
				output$setup_models_analysis_session_seed = NULL
				updateTextInput(session , "setup_models_analysis_session_seed", value="")
			}
		} else {
			output$setup_models_analysis_session_seed = NULL
			updateTextInput(session , "setup_models_analysis_session_seed", value="")
		}
	})

	## Select outcome variable
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
				if (isTRUE(input$setup_models_analysis_target_variable_check=="yes")) {
					output$setup_models_analysis_target_variable = renderUI({
						empty_lab = ""
						names(empty_lab) = get_rv_labels("setup_models_analysis_target_variable_ph")
						
						if (isTRUE(!is.null(rv_current$outcome))) {
							temp_labs = rv_current$selected_vars
							temp_selected = rv_current$outcome
						} else {
							temp_labs = c(empty_lab, rv_current$selected_vars)
							temp_selected = NULL
						}
						selectInput("setup_models_analysis_target_variable"
							, label = NULL 
							, choices = temp_labs
							, multiple=FALSE
							, selected = temp_selected
						)
					})
				} else {
					updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
					output$setup_models_analysis_target_variable=NULL
					
				}
			} else {
				updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
				output$setup_models_analysis_target_variable=NULL
			}
		} else {
			updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
			output$setup_models_analysis_target_variable=NULL
			
		}
	})

	## Analysis type
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
				if (isTRUE(input$setup_models_analysis_target_variable_check=="yes") & (isTRUE(input$setup_models_analysis_target_variable!="") & isTRUE(!is.null(input$setup_models_analysis_target_variable))) | isTRUE(input$setup_models_analysis_target_variable_check=="no")) {
					rv_current$vartype_all = Rautoml::get_types(rv_current$working_df)
				
					output$setup_models_analysis_type = renderUI({
						empty_lab = ""
						names(empty_lab) = get_rv_labels("setup_models_analysis_type_ph")
						
						if (isTRUE(input$setup_models_analysis_target_variable_check=="yes")) {
							temp_labs =  c(empty_lab, get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_choices_supervised"))
						} else {
							temp_labs =  c(empty_lab, get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_choices_unsupervised"))
							
						}

						selectInput("setup_models_analysis_type"
							, label = get_rv_labels("setup_models_analysis_type") 
							, choices = temp_labs 
							, multiple=FALSE
						)
					})
				} else {
					updateSelectInput(session, "setup_models_analysis_type", selected="")
					output$setup_models_analysis_type = NULL
				}
			} else {
				updateSelectInput(session, "setup_models_analysis_type", selected="")
				output$setup_models_analysis_type = NULL
			}
		} else {
			updateSelectInput(session, "setup_models_analysis_type", selected="")
			output$setup_models_analysis_type = NULL
		}
	})


	## Model types
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
				if (isTRUE(input$setup_models_analysis_type!="") & isTRUE(!is.null(input$setup_models_analysis_type))) {
					output$setup_models_analysis_model_type = renderUI({
						if (isTRUE(any(input$setup_models_analysis_type %in% c("Supervised learning", "Semi-supervised learning")))) {
							temp_labs=get_named_choices(input_choices_file, input$change_language, "setup_models_analysis_model_type_check_supervised")
						} else if (isTRUE(input$setup_models_analysis_type=="Clustering")) {
							temp_labs=get_named_choices(input_choices_file, input$change_language, "setup_models_analysis_model_type_check_clustering")
						} else if (isTRUE(input$setup_models_analysis_type=="Anomaly detection")) {
							temp_labs=get_named_choices(input_choices_file, input$change_language, "setup_models_analysis_model_type_check_anomaly")
						}
						prettyCheckboxGroup(
							inputId = "setup_models_analysis_model_type_check"
								, label = get_rv_labels("setup_models_analysis_model_type_check")
								, choices = temp_labs
								, inline = FALSE
								, status = "success"
						)
					})			
				} else {
					output$setup_models_analysis_model_type = NULL
					updatePrettyCheckboxGroup(session=session, inputId="setup_models_analysis_model_type_check", selected = character(0))
				}
			} else {
				output$setup_models_analysis_model_type = NULL	
				updatePrettyCheckboxGroup(session=session, inputId="setup_models_analysis_model_type_check", selected = character(0))
			}

		} else {
			output$setup_models_analysis_model_type = NULL	
			updatePrettyCheckboxGroup(session=session, inputId="setup_models_analysis_model_type_check", selected = character(0))
			
		}
		
	})
	
}

setup_models_ui_old_new = function() {

	## ML/AI tasks
	observe({
		if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
			if (isTRUE(!is.null(rv_current$working_df)) & isTRUE(!is.null(input$setup_models_analysis_type))) {
				if (isTRUE(input$setup_models_analysis_type=="Supervised learning")) {
					output$setup_models_analysis_type_specifics = renderUI({
						empty_lab = ""
						names(empty_lab) = get_rv_labels("setup_models_analysis_type_ph")
						selectInput("setup_models_analysis_type_specifics"
							, label = get_rv_labels("setup_models_analysis_type_supervised") 
							, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_supervised_choices"))
							, multiple=FALSE
						)
					})
				} else if (isTRUE(input$setup_models_analysis_type=="Unsupervised learning")) {
					output$setup_models_analysis_type_specifics = renderUI({
						empty_lab = ""
						names(empty_lab) = get_rv_labels("setup_models_analysis_type_ph")
						selectInput("setup_models_analysis_type_specifics"
							, label = get_rv_labels("setup_models_analysis_type_unsupervised") 
							, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_unsupervised_choices"))
							, multiple=FALSE
						)
					})			
				} else if (isTRUE(input$setup_models_analysis_type=="Time series")) {
					# FIXME: HERE
					 shinyalert("", get_rv_labels("future_analysis_type_ts"), type = "info", inputId="future_analysis_type_ts")
					updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
					output$setup_models_analysis_type_specifics = NULL
				} else if (isTRUE(input$setup_models_analysis_type=="Inferential statistics")) {
					# FIXME: HERE
					 shinyalert("", get_rv_labels("future_analysis_type_inference"), type = "info", inputId="future_analysis_type_inference")
					updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
					output$setup_models_analysis_type_specifics = NULL
				} else {
					updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
					output$setup_models_analysis_type_specifics = NULL
				}
			} else {
				updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
				output$setup_models_analysis_type_specifics = NULL
			}
		} else {
			updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
			output$setup_models_analysis_type_specifics = NULL
		}
	})


	## Target/Outcome variable
	observe({
		if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
			if (isTRUE(input$setup_models_analysis_type_specifics=="Regression")) {
				if (isTRUE(length(rv_current$vartype_all$numeric)>0)) {
					output$setup_models_analysis_target_variable = renderUI({
						empty_lab = ""
						names(empty_lab) = get_rv_labels("setup_models_analysis_target_variable_ph")
						selectInput("setup_models_analysis_target_variable"
							, label = get_rv_labels("setup_models_analysis_target_variable") 
							, choices = c(empty_lab, rv_current$vartype_all$numeric)
							, multiple=FALSE
						)
					})
				} else {
					shinyalert("", get_rv_labels("setup_models_analysis_target_numeric_alert"), type = "info", inputId="setup_models_analysis_target_numeric_alert")
				}
			} else if (isTRUE(input$setup_models_analysis_type_specifics=="Classification")){
				if (isTRUE(length(rv_current$vartype_all$categorical)>0)) {
					output$setup_models_analysis_target_variable = renderUI({
						empty_lab = ""
						names(empty_lab) = get_rv_labels("setup_models_analysis_target_variable_ph")
						selectInput("setup_models_analysis_target_variable"
							, label = get_rv_labels("setup_models_analysis_target_variable") 
							, choices = c(empty_lab, rv_current$vartype_all$categorical)
							, multiple=FALSE
						)
					})
				} else {
					shinyalert("", get_rv_labels("setup_models_analysis_target_categorical_alert"), type = "info", inputId="setup_models_analysis_target_categorical_alert")
				}	
			} else {
				updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
				output$setup_models_analysis_target_variable=NULL
			}
		} else {
			updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
			output$setup_models_analysis_target_variable=NULL
		}
	})


	## Train/test ratio
	observe({
		if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
			if (isTRUE(!is.null(input$setup_models_analysis_target_variable)) & isTRUE(input$setup_models_analysis_target_variable!="")) {
				output$setup_models_analysis_partition_ratio = renderUI({
						sliderInput("setup_models_analysis_partition_ratio"
							, get_rv_labels("setup_models_analysis_partition_ratio")
							, min = 0
							, max = 1
							, value = 0.75
							, step = NULL
						)
				})
				
				## Exclude vars
				output$setup_models_analysis_exclude_variables = renderUI({
						empty_lab = ""
						names(empty_lab) = get_rv_labels("setup_models_analysis_exclude_variables_ph")
						selectInput("setup_models_analysis_exclude_variables"
							, label = get_rv_labels("setup_models_analysis_exclude_variables") 
							, choices = c(empty_lab, setdiff(colnames(rv_current$working_df), input$setup_models_analysis_target_variable))
							, multiple=TRUE
						)
				})
			} else {
				updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
				output$setup_models_analysis_partition_ratio = NULL
				output$setup_models_analysis_exclude_variables=NULL
				updateSelectInput(session, "setup_models_analysis_exclude_variables", selected="")
			}
		} else {
			updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
			output$setup_models_analysis_partition_ratio = NULL
			output$setup_models_analysis_exclude_variables=NULL
			updateSelectInput(session, "setup_models_analysis_exclude_variables", selected="")
		}
	})

	observe({
		if (isTRUE(!is.null(input$setup_models_analysis_session_name)) & isTRUE(input$setup_models_analysis_session_name!="")) {
				if (isTRUE(!is.null(input$setup_models_analysis_type)) & (isTRUE(input$setup_models_analysis_type!="")) & isTRUE(!is.null(input$setup_models_analysis_type_specifics)) & isTRUE(input$setup_models_analysis_type_specifics != "")) {
					if (isTRUE(input$setup_models_analysis_target_variable!="")) {
						output$setup_models_analysis_apply = renderUI({
							actionBttn("setup_models_analysis_apply"
								, inline=TRUE
								, block = FALSE
								, color = "success"
								, label = get_rv_labels("setup_models_analysis_apply"))
						})
					} else {
						output$setup_models_analysis_apply = NULL	
					}
				} else {
					output$setup_models_analysis_apply = NULL	
				}	
			} else {
			output$setup_models_analysis_apply = NULL	
		}
	})



	observeEvent(input$setup_models_analysis_apply, {
		if (isTRUE(!is.null(input$setup_models_analysis_session_name)) & isTRUE(input$setup_models_analysis_session_name!="")) {
			rv_ml_ai$session_id = input$setup_models_analysis_session_name
			rv_ml_ai$seed_value = input$setup_models_analysis_session_seed
			rv_ml_ai$dataset_id = rv_current$dataset_id
			rv_ml_ai$analysis_type = input$setup_models_analysis_type
			rv_ml_ai$task = input$setup_models_analysis_type_specifics
			rv_ml_ai$outcome = input$setup_models_analysis_target_variable
			rv_ml_ai$partition_ratio = input$setup_models_analysis_partition_ratio
			rv_ml_ai$predictors = setdiff(colnames(rv_current$working_df), input$setup_models_analysis_target_variable)
			rv_ml_ai$excluded_predictors = input$setup_models_analysis_exclude_variables
			
			if (isTRUE(input$setup_models_analysis_exclude_variables!="") & isTRUE(!is.null(input$setup_models_analysis_exclude_variables))) {
				rv_current$working_df = Rautoml::drop_variables(rv_current$working_df, input$setup_models_analysis_exclude_variables)
			}

			rv_ml_ai$ml_ai_setup_result = paste0(
				"<b>", "Session ID: ", "</b>", rv_ml_ai$session_id
				, "<br>"
				, "<b>", "Seed value: ", "</b>", rv_ml_ai$seed_value
				, "<br>"
				, "<b>", "Dataset ID: ", "</b>", rv_ml_ai$dataset_id
				, "<br>"
				, "<b>", "Analysis Type: ", "</b>", rv_ml_ai$analysis_type
				, "<br>"
				, "<b>", "Task: ", "</b>", rv_ml_ai$task
				, "<br>"
				, "<b>", "Outcome: ", "</b>", rv_ml_ai$outcome
				, "<br>"
				, "<b>", "Train/Test ratio ", "</b>", paste0(rv_ml_ai$partition_ratio*100, "% : ", (1-rv_ml_ai$partition_ratio)*100, "%")
				, "<br>"
				, "<b>", "Predictors: ", "</b>", paste0(rv_ml_ai$predictors, collapse=", ")
				, "<br>"
				, "<b>", "Excluded Predictors: ", "</b>", paste0(rv_ml_ai$excluded_predictors, collapse=", ")
			)

			output$setup_models_analysis_results = renderUI({
				p(
					HTML(
						paste0("<b>Initialization Output</b><br>"
							, rv_ml_ai$ml_ai_setup_result
						)
					)
				)
			})
#			updateTextInput(session , "setup_models_analysis_session_name", value="")
			
			output$setup_models_analysis_session_seed = NULL
			updateTextInput(session , "setup_models_analysis_session_seed", value=123)
			
			updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
			output$setup_models_analysis_type_specifics = NULL
			
			updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
			output$setup_models_analysis_target_variable=NULL
			
			updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
			output$setup_models_analysis_partition_ratio = NULL
				
			output$setup_models_analysis_exclude_variables=NULL
			updateSelectInput(session, "setup_models_analysis_exclude_variables", selected="")
			updateTextInput(session , "setup_models_analysis_session_name", value="")
					
		}
		
	})


	
## 	## Reset if task is updated
## 	observeEvent(input$setup_models_analysis_type, {
## 		
## 		updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
## 		output$setup_models_analysis_type_specifics = NULL
## 		
## 		updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
## 		output$setup_models_analysis_target_variable=NULL
## 		
## 		updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
## 		output$setup_models_analysis_partition_ratio = NULL
## 			
## 		output$setup_models_analysis_exclude_variables=NULL
## 		updateSelectInput(session, "setup_models_analysis_exclude_variables", selected="")
## 				
## 		output$setup_models_analysis_session_seed = NULL
## 		updateTextInput(session , "setup_models_analysis_session_seed", value="")
## 		
## 	})

}


setup_models_ui_old = function() {

## 	## Analytics type
## 	observeEvent(input$manage_data_apply, {
## 		if (isTRUE(!is.null(rv_current$working_df))) {
## 			rv_current$vartype_all = Rautoml::get_types(rv_current$working_df)
## 
## 			output$setup_models_analysis_type = renderUI({
## 				empty_lab = ""
## 				names(empty_lab) = get_rv_labels("setup_models_analysis_type_ph")
## 				selectInput("setup_models_analysis_type"
## 					, label = get_rv_labels("setup_models_analysis_type") 
## 					, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_choices"))
## 					, multiple=FALSE
## 				)
## 			})
## 		} else {
## 			output$setup_models_analysis_type = NULL
## 			updateSelectInput(session, "setup_models_analysis_type", selected="")
## 		}
## 	})
## 
## 
## 	## ML/AI tasks
## 	observe({
## 		if (isTRUE(!is.null(rv_current$working_df)) & isTRUE(!is.null(input$setup_models_analysis_type))) {
## 			if (isTRUE(input$setup_models_analysis_type=="Supervised learning")) {
## 				output$setup_models_analysis_type_specifics = renderUI({
## 					empty_lab = ""
## 					names(empty_lab) = get_rv_labels("setup_models_analysis_type_ph")
## 					selectInput("setup_models_analysis_type_specifics"
## 						, label = get_rv_labels("setup_models_analysis_type_supervised") 
## 						, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_supervised_choices"))
## 						, multiple=FALSE
## 					)
## 				})
## 			} else if (isTRUE(input$setup_models_analysis_type=="Unsupervised learning")) {
## 				output$setup_models_analysis_type_specifics = renderUI({
## 					empty_lab = ""
## 					names(empty_lab) = get_rv_labels("setup_models_analysis_type_ph")
## 					selectInput("setup_models_analysis_type_specifics"
## 						, label = get_rv_labels("setup_models_analysis_type_unsupervised") 
## 						, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_unsupervised_choices"))
## 						, multiple=FALSE
## 					)
## 				})			
## 			} else if (isTRUE(input$setup_models_analysis_type=="Time series")) {
## 				# FIXME: HERE
## 				 shinyalert("", get_rv_labels("future_analysis_type_ts"), type = "info", inputId="future_analysis_type_ts")
## 				updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
## 				output$setup_models_analysis_type_specifics = NULL
## 			} else if (isTRUE(input$setup_models_analysis_type=="Inferential statistics")) {
## 				# FIXME: HERE
## 				 shinyalert("", get_rv_labels("future_analysis_type_inference"), type = "info", inputId="future_analysis_type_inference")
## 				updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
## 				output$setup_models_analysis_type_specifics = NULL
## 			} else {
## 				updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
## 				output$setup_models_analysis_type_specifics = NULL
## 			}
## 		}
## 	})
## 
## 	## Target/Outcome variable
## 	observe({
## 		if (isTRUE(input$setup_models_analysis_type_specifics=="Regression")) {
## 			if (isTRUE(length(rv_current$vartype_all$numeric)>0)) {
## 				output$setup_models_analysis_target_variable = renderUI({
## 					empty_lab = ""
## 					names(empty_lab) = get_rv_labels("setup_models_analysis_target_variable_ph")
## 					selectInput("setup_models_analysis_target_variable"
## 						, label = get_rv_labels("setup_models_analysis_target_variable") 
## 						, choices = c(empty_lab, rv_current$vartype_all$numeric)
## 						, multiple=FALSE
## 					)
## 				})
## 			} else {
## 				shinyalert("", get_rv_labels("setup_models_analysis_target_numeric_alert"), type = "info", inputId="setup_models_analysis_target_numeric_alert")
## 			}
## 		} else if (isTRUE(input$setup_models_analysis_type_specifics=="Classification")){
## 			if (isTRUE(length(rv_current$vartype_all$categorical)>0)) {
## 				output$setup_models_analysis_target_variable = renderUI({
## 					empty_lab = ""
## 					names(empty_lab) = get_rv_labels("setup_models_analysis_target_variable_ph")
## 					selectInput("setup_models_analysis_target_variable"
## 						, label = get_rv_labels("setup_models_analysis_target_variable") 
## 						, choices = c(empty_lab, rv_current$vartype_all$categorical)
## 						, multiple=FALSE
## 					)
## 				})
## 			} else {
## 				shinyalert("", get_rv_labels("setup_models_analysis_target_categorical_alert"), type = "info", inputId="setup_models_analysis_target_categorical_alert")
## 			}	
## 		} else {
## 			updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
## 			output$setup_models_analysis_target_variable=NULL
## 		}
## 	})
	
## 	## Train/test ratio
## 	observe({
## 		if (isTRUE(!is.null(input$setup_models_analysis_target_variable)) & isTRUE(input$setup_models_analysis_target_variable!="")) {
## 			output$setup_models_analysis_partition_ratio = renderUI({
## 					sliderInput("setup_models_analysis_partition_ratio"
## 						, get_rv_labels("setup_models_analysis_partition_ratio")
## 						, min = 0
## 						, max = 1
## 						, value = 0.75
## 						, step = NULL
## 					)
## 			})
## 			
## 			## Exclude vars
## 			output$setup_models_analysis_exclude_variables = renderUI({
## 					empty_lab = ""
## 					names(empty_lab) = get_rv_labels("setup_models_analysis_exclude_variables_ph")
## 					selectInput("setup_models_analysis_exclude_variables"
## 						, label = get_rv_labels("setup_models_analysis_exclude_variables") 
## 						, choices = c(empty_lab, setdiff(colnames(rv_current$working_df), input$setup_models_analysis_target_variable))
## 						, multiple=TRUE
## 					)
## 			})
## 
## 			output$setup_models_analysis_session_name = renderUI({
## 				textInput("setup_models_analysis_session_name"
## 					, label = get_rv_labels("setup_models_analysis_session_name")
## 					, value = ""
## 					, width = NULL
## 					, placeholder = get_rv_labels("setup_models_analysis_session_name_ph")
## 				)	
## 			})
## 		} else {
## 			updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
## 			output$setup_models_analysis_partition_ratio = NULL
## 			output$setup_models_analysis_session_name = NULL
## 			updateTextInput(session , "setup_models_analysis_session_name", value="")
## 			output$setup_models_analysis_exclude_variables=NULL
## 			updateSelectInput(session, "setup_models_analysis_exclude_variables", selected="")
## 		}
## 	})
## 
## 	observe({
## 		if (isTRUE(!is.null(input$setup_models_analysis_session_name)) & isTRUE(input$setup_models_analysis_session_name!="")) {
## 				output$setup_models_analysis_apply = renderUI({
## 					actionBttn("setup_models_analysis_apply"
## 						, inline=TRUE
## 						, block = FALSE
## 						, color = "success"
## 						, label = get_rv_labels("setup_models_analysis_apply"))
## 				})
## 			
## 		} else {
## 			output$setup_models_analysis_apply = NULL	
## 		}
## 	})
## 
## 	observeEvent(input$setup_models_analysis_apply, {
## 		if (isTRUE(!is.null(input$setup_models_analysis_session_name)) & isTRUE(input$setup_models_analysis_session_name!="")) {
## 			rv_ml_ai$session_id = input$setup_models_analysis_session_name
## 			rv_ml_ai$seed_value = input$setup_models_analysis_session_seed
## 			rv_ml_ai$dataset_id = rv_current$dataset_id
## 			rv_ml_ai$analysis_type = input$setup_models_analysis_type
## 			rv_ml_ai$task = input$setup_models_analysis_type_specifics
## 			rv_ml_ai$outcome = input$setup_models_analysis_target_variable
## 			rv_ml_ai$partition_ratio = input$setup_models_analysis_partition_ratio
## 			rv_ml_ai$predictors = setdiff(colnames(rv_current$working_df), input$setup_models_analysis_target_variable)
## 			rv_ml_ai$excluded_predictors = input$setup_models_analysis_exclude_variables
## 			
## 			if (isTRUE(input$setup_models_analysis_exclude_variables!="")) {
## 				rv_current$working_df = Rautoml::drop_variables(rv_current$working_df, input$setup_models_analysis_exclude_variables)
## 			}
## 
## 			rv_ml_ai$ml_ai_setup_result = paste0(
## 				"<b>", "Session ID: ", "</b>", rv_ml_ai$session_id
## 				, "<br>"
## 				, "<b>", "Seed value: ", "</b>", rv_ml_ai$seed_value
## 				, "<br>"
## 				, "<b>", "Dataset ID: ", "</b>", rv_ml_ai$dataset_id
## 				, "<br>"
## 				, "<b>", "Analysis Type: ", "</b>", rv_ml_ai$analysis_type
## 				, "<br>"
## 				, "<b>", "Task: ", "</b>", rv_ml_ai$task
## 				, "<br>"
## 				, "<b>", "Outcome: ", "</b>", rv_ml_ai$outcome
## 				, "<br>"
## 				, "<b>", "Train/Test ratio ", "</b>", paste0(rv_ml_ai$partition_ratio*100, "% : ", (1-rv_ml_ai$partition_ratio)*100, "%")
## 				, "<br>"
## 				, "<b>", "Predictors: ", "</b>", paste0(rv_ml_ai$predictors, collapse=", ")
## 				, "<br>"
## 				, "<b>", "Excluded Predictors: ", "</b>", paste0(rv_ml_ai$excluded_predictors, collapse=", ")
## 			)
## 
## 			output$setup_models_analysis_results = renderUI({
## 				p(
## 					HTML(
## 						paste0("<b>Initialization Output</b><br>"
## 							, rv_ml_ai$ml_ai_setup_result
## 						)
## 					)
## 				)
## 			})
## 		}
## 		
## 	})
## 	
## 	## Reset if task is updated
## 	observeEvent(input$setup_models_analysis_type, {
## 		
## 		updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
## 		output$setup_models_analysis_type_specifics = NULL
## 		
## 		updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
## 		output$setup_models_analysis_target_variable=NULL
## 		
## 		updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
## 		output$setup_models_analysis_partition_ratio = NULL
## 			
## 		output$setup_models_analysis_exclude_variables=NULL
## 		updateSelectInput(session, "setup_models_analysis_exclude_variables", selected="")
## 		
## 		updateTextInput(session , "setup_models_analysis_session_name", value="")
## 	})
}

