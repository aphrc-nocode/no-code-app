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
					updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
					output$setup_models_analysis_target_variable=NULL
					
				output$setup_models_analysis_exclude_variables=NULL
				updateSelectInput(session, "setup_models_analysis_exclude_variables", selected="")
				}
			} else {
				updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
				output$setup_models_analysis_target_variable=NULL
				
				output$setup_models_analysis_exclude_variables=NULL
				updateSelectInput(session, "setup_models_analysis_exclude_variables", selected="")
			}
		} else {
			updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
			output$setup_models_analysis_target_variable=NULL
			
			output$setup_models_analysis_exclude_variables=NULL
			updateSelectInput(session, "setup_models_analysis_exclude_variables", selected="")
		}
	})

	## Analysis type
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
				if (isTRUE(input$setup_models_analysis_target_variable_check=="yes") & (isTRUE(input$setup_models_analysis_target_variable!="") & isTRUE(!is.null(input$setup_models_analysis_target_variable))) | isTRUE(input$setup_models_analysis_target_variable_check=="no")) {
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
						if (isTRUE(input$setup_models_analysis_type=="Supervised learning")) {
							temp_labs=get_named_choices(input_choices_file, input$change_language, "setup_models_analysis_model_type_check_supervised")
						} else if (isTRUE(input$setup_models_analysis_type=="Clustering")) {
							temp_labs=get_named_choices(input_choices_file, input$change_language, "setup_models_analysis_model_type_check_clustering")
						} else if (isTRUE(input$setup_models_analysis_type=="Anomaly detection")) {
							temp_labs=get_named_choices(input_choices_file, input$change_language, "setup_models_analysis_model_type_check_anomaly")
						} else if (isTRUE(input$setup_models_analysis_type=="Semi-supervised learning")) {
							temp_labs=get_named_choices(input_choices_file, input$change_language, "setup_models_analysis_model_type_check_semisupervised")
							
						}
						prettyCheckboxGroup(
							inputId = "setup_models_analysis_model_type_check"
								, label = get_rv_labels("setup_models_analysis_model_type_check")
								, choices = temp_labs
								, selected = character(0)#temp_labs[[1]]
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


	## Test/ratio slide
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
				if (isTRUE(length(input$setup_models_analysis_model_type_check)>0)) {
					output$setup_models_analysis_partition_ratio = renderUI({
							sliderInput("setup_models_analysis_partition_ratio"
								, get_rv_labels("setup_models_analysis_partition_ratio")
								, min = 0
								, max = 1
								, value = 0.75
								, step = NULL
							)
					})
				} else {
					updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
					output$setup_models_analysis_partition_ratio = NULL
				}

			} else {
				updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
				output$setup_models_analysis_partition_ratio = NULL
			}
		} else {
			updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
			output$setup_models_analysis_partition_ratio = NULL
		}
		
	}) 
		
	
	## Apply changes
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
				if (isTRUE(length(input$setup_models_analysis_model_type_check)>0)) {
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
		
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
				if (isTRUE(length(input$setup_models_analysis_model_type_check)>0)) {
			

			rv_ml_ai$session_id = input$setup_models_analysis_session_name
			rv_ml_ai$seed_value = input$setup_models_analysis_session_seed
			rv_ml_ai$dataset_id = rv_current$dataset_id
			rv_ml_ai$analysis_type = input$setup_models_analysis_type
			rv_ml_ai$outcome = input$setup_models_analysis_target_variable

			# Pycaret way
			rv_ml_ai$status <- NULL        # "Running...", "Finished", "Failed: ..."
			rv_ml_ai$leaderboard <- NULL   # table CV (train) renvoyée par /automl
			rv_ml_ai$models <- NULL        # nom/id des modèles pour la sélection
			rv_ml_ai$eval_metrics <- NULL  # résultats test d’un modèle
			rv_ml_ai$eval_plots <- NULL    # images base64 (ROC, confusion, etc.)

			
			if (isTRUE(any(rv_ml_ai$outcome %in% rv_current$vartype_all$categorical))) {
				rv_ml_ai$task = get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_classification")
			} else if (isTRUE(any(rv_ml_ai$outcome %in% rv_current$vartype_all$numeric))) {
				outcome_nlevels = Rautoml::get_nlevels(rv_current$working_df, rv_ml_ai$outcome)
				if (isTRUE(outcome_nlevels<2)) {
					shinyalert::shinyalert("Error: ", get_rv_labels("outcome_variable_level_error"), type = "error")
					return()
				} else if (isTRUE(outcome_nlevels==2)) {
					df_temp = rv_current$working_df
					df_temp[[rv_ml_ai$outcome]] = Rautoml::numeric_factor(df_temp[[rv_ml_ai$outcome]])
					rv_current$working_df = df_temp
					rv_ml_ai$task = get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_classification")
					shinyalert::shinyalert("Info: ", get_rv_labels("outcome_variable_level_change_info"), type = "info")
				} else {
					rv_ml_ai$task = get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_supervised_regression")
				}
			} else {
				rv_ml_ai$task = get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_unsupervised")
			}

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
			
			shinyalert::shinyalert("Done!", get_rv_labels("setup_models_analysis_success"), type = "success")

			output$setup_models_analysis_results = renderUI({
				p(
					HTML(
						paste0("<b>Initialization Output</b><br>"
							, rv_ml_ai$ml_ai_setup_result
						)
					)
				)
			})

##				updateTextInput(session , "setup_models_analysis_session_name", value="")
## 			
## 			output$setup_models_analysis_session_seed = NULL
## 			updateTextInput(session , "setup_models_analysis_session_seed", value=123)
## 			
## 			updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
## 			output$setup_models_analysis_type_specifics = NULL
## 			
## 			updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
## 			output$setup_models_analysis_target_variable=NULL
## 			
## 			updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
## 			output$setup_models_analysis_partition_ratio = NULL
## 				
## 			output$setup_models_analysis_exclude_variables=NULL
## 			updateSelectInput(session, "setup_models_analysis_exclude_variables", selected="")
## 			updateTextInput(session , "setup_models_analysis_session_name", value="")
					
				}
			}
		}
	})
	
	
observeEvent(input$modelling_framework_choices, {
	  req(input$modelling_framework_choices)
	  rv_ml_ai$modelling_framework <- input$modelling_framework_choices
#	  message("Modelling framework sélectionné : ", rv_ml_ai$modelling_framework)
	})
	

observe({
  req(!is.null(rv_ml_ai$modelling_framework))  # Check if value exist
  
  if (tolower(rv_ml_ai$modelling_framework) == "pycaret") {
    output$automl_module_ui <- renderUI({
      automl_ui("automl_module")
    })
  } else {
    output$automl_module_ui <- renderUI({
      h4("")
    })
  }
})

}

