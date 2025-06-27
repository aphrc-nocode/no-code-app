#### ---- LM and GLM models -----	---------------------------- ####

model_training_caret_models_ols_server = function() {
	
	observeEvent(input$feature_engineering_apply, {
		
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				output$model_training_caret_models_ui = renderUI({
					p(
						hr()	
						, HTML(paste0("<b>", get_rv_labels("model_training_caret_models_ui"), "</b>"))
					)
				})
				
				if (isTRUE(any(rv_ml_ai$task %in% c("Classification", "Regression")))) {
					## GLM and LM models
					output$model_training_caret_models_ols = renderUI({
						if (isTRUE(rv_ml_ai$task=="Classification")) {
							temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_ols_logistic")	
						} else if (isTRUE(rv_ml_ai$task=="Regression")) {
							temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_ols_linear")	
						}
						rv_training_results$ols_name = temp_label 
						prettyCheckbox(
							"model_training_caret_models_ols_check"
							, label = names(temp_label)
							, status = "success"
							, outline = FALSE
							, inline = TRUE
						)	
					})
				} else {
					rv_training_results$ols_name = NULL
					output$model_training_caret_models_ols = NULL
					updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=NULL)
				}
				
			} else {
				output$model_training_caret_models_ui = NULL	
			}
		} else {
			output$model_training_caret_models_ui = NULL	
		}
	})

	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				## Hyperparameters for OLS
				if (isTRUE(rv_ml_ai$task=="Classification")) {
					if (isTRUE(input$model_training_caret_models_ols_check)) {
						
						output$model_training_caret_models_ols_advance_control = renderUI({
							actionButton("ols_advance_control_apply"
								, get_rv_labels("advance_control_apply") 
								, icon = icon("cog")
								, width="50%"
							)
						})

					} else {
						output$model_training_caret_models_ols_advance_control = NULL
					}
				}
			}
		}
	})

	observeEvent(input$ols_advance_control_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				## Hyperparameters for OLS
				if (isTRUE(rv_ml_ai$task=="Classification")) {
					if (isTRUE(input$model_training_caret_models_ols_check)) {
						output$model_training_caret_models_ols_advance = renderUI({
							prettySwitch("model_training_caret_models_ols_advance_intercept"
								, get_rv_labels("model_training_caret_models_ols_advance_intercept")
								, value = TRUE
							)
						})

						 showModal(
							 modalDialog(
								title = get_rv_labels("model_training_caret_models_advance_options"),
								size = "m",
								footer = tagList(
								  modalButton(get_rv_labels("customize_train_control_apply_cancel")),
								  actionButton("ols_advance_control_apply_save", get_rv_labels("customize_train_control_apply_save" ))
								),
								tagList(
									uiOutput("model_training_caret_models_ols_advance")
								)
							 )
						)
					} else {
 						output$model_training_caret_models_ols_advance = NULL
 						updatePrettySwitch(session, inputId="model_training_caret_models_ols_advance_intercept" , value=TRUE)
					}
				}
			}
		}
	})

	## Update the advance options
	observeEvent(input$ols_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$modelling_framework_choices=="Caret")) {
					if (isTRUE(input$model_training_caret_models_ols_check)) {
						rv_training_results$ols_param = TRUE
					} else {
						rv_training_results$ols_param = FALSE
					}
				} else {
					rv_training_results$ols_param = FALSE
				}
			}
		}
		removeModal()
	})

	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$modelling_framework_choices=="Caret")) {
					if (isTRUE(input$model_training_caret_models_ols_check)) {
						model = Rautoml::setup_caret( 
							rv_training_results$ols_name
							, param=rv_training_results$ols_param
							, param_set=list(intercept=input$model_training_caret_models_ols_advance_intercept)
						)
					} else {
						model = Rautoml::setup_caret(
							rv_training_results$ols_name
							, param=FALSE
						)
					}
					print(model)
				}
			}
		}
		
	})

}




model_training_caret_models_ols_server_old = function() {
	
	observeEvent(input$feature_engineering_apply, {
		
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				
				output$model_training_caret_models_ui = renderUI({
					p(
						hr()	
						, HTML(paste0("<b>", get_rv_labels("model_training_caret_models_ui"), "</b>"))
					)
				})
				
				if (isTRUE(any(rv_ml_ai$task %in% c("Classification", "Regression")))) {
					## GLM and LM models
					output$model_training_caret_models_ols = renderUI({
						if (isTRUE(rv_ml_ai$task=="Classification")) {
							temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_ols_logistic")	
						} else if (isTRUE(rv_ml_ai$task=="Regression")) {
							temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_ols_linear")	
						}
						rv_training_results$ols_param$name = temp_label 
						prettyCheckbox(
							"model_training_caret_models_ols_check"
							, label = names(temp_label)
							, status = "success"
							, outline = FALSE
							, inline = TRUE
						)	
					})
				} else {
					rv_training_results$ols_param$name = NULL
					
					output$model_training_caret_models_ols = NULL
					updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=NULL)
				}

			} else {
				output$model_training_caret_models_ols = NULL
				updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=NULL)
			}
		} else {	
			output$model_training_caret_models_ols = NULL
			updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=NULL)
		}
	})
	
	## Advance options
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				## Hyperparameters for OLS
				if (isTRUE(rv_ml_ai$task=="Classification")) {
					if (isTRUE(input$model_training_caret_models_ols_check)) {
						
						output$model_training_caret_models_ols_advance_control = renderUI({
							actionButton("ols_advance_control_apply"
								, get_rv_labels("advance_control_apply") 
								, icon = icon("cog")
								, width="50%"
							)
						})
						
						
						output$model_training_caret_models_ols_advance = renderUI({
							prettySwitch("model_training_caret_models_ols_advance_intercept"
								, get_rv_labels("model_training_caret_models_ols_advance_intercept")
								, value = TRUE
							)
						})
					} else {
						output$model_training_caret_models_ols_advance_control = NULL

						output$model_training_caret_models_ols_advance = NULL
						updatePrettySwitch(session, inputId="model_training_caret_models_ols_advance_intercept" , value=FALSE)
					}
				} else {
					output$model_training_caret_models_ols_advance_control = NULL
					
					output$model_training_caret_models_ols_advance = NULL
					updatePrettySwitch(session, inputId="model_training_caret_models_ols_advance_intercept" , value=FALSE)
				}
			} else {
				output$model_training_caret_models_ols_advance_control = NULL
				
				output$model_training_caret_models_ols_advance = NULL
				updatePrettySwitch(session, inputId="model_training_caret_models_ols_advance_intercept" , value=FALSE)
				
			}
		} else {
			output$model_training_caret_models_ols_advance_control = NULL
			
			output$model_training_caret_models_ols_advance = NULL
			updatePrettySwitch(session, inputId="model_training_caret_models_ols_advance_intercept" , value=FALSE)
		}
		
	})

	observeEvent(input$ols_advance_control_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$modelling_framework_choices=="Caret")) {
					if (isTRUE(input$model_training_caret_models_ols_check)) {
						 showModal(
							 modalDialog(
								title = get_rv_labels("model_training_caret_models_advance_options"),
								size = "m",
								footer = tagList(
								  modalButton(get_rv_labels("customize_train_control_apply_cancel")),
								  actionButton("ols_advance_control_apply_save", get_rv_labels("customize_train_control_apply_save" ))
								),
								tagList(
									uiOutput("model_training_caret_models_ols_advance")
								)
							 )
						)
					}
				} else {
					NULL # TODO
				}
			} else {
				NULL # TODO
			}
		} else {
			NULL # TODO
		}
	})
	
	## Update the advance options
	observeEvent(input$ols_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$modelling_framework_choices=="Caret")) {
					if (isTRUE(input$model_training_caret_models_ols_check)) {
						rv_training_results$ols_param$param = TRUE
						removeModal()
					} else {
						rv_training_results$ols_param$param = FALSE
					}
				} else {
					rv_training_results$ols_param$param = FALSE
				}
			} else {
				rv_training_results$ols_param$param = FALSE
			}
		} else {
			rv_training_results$ols_param$param = FALSE
		}
	})

	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$modelling_framework_choices=="Caret")) {
					if (isTRUE(input$model_training_caret_models_ols_check)) {
						rv_ml_ai$at_least_one_model = TRUE
						print(rv_training_results$ols_param$param)
## 						if (isTRUE(rv_training_results$ols_param$param)) {
## 							model = Rautoml::setup_caret(rv_training_results$ols_param$name
## 								, param=TRUE
## 								, param_set=list(intercept=model_training_caret_models_ols_advance_intercept)
## 							)
## 						} else {
## 							model = Rautoml::setup_caret(rv_training_results$ols_param$name
## 								, param=FALSE
## 							)
## 							
## 						}
## 						rv_training_results$models = c(rv_training_results$models, model)
			#			print(rv_training_results$models)
					}
				}
			}
		}
		
	})



}


#### ---- Train all models ------------------------------------------- #####
## 
## model_training_caret_train_all_server = function() {
## 	
## 	## Model training apply
## 	observe({
## 		if (isTRUE(!is.null(rv_current$working_df))) {
## 			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
## 				if (isTRUE(rv_ml_ai$at_least_one_model)) {
## 					output$model_training_apply = renderUI({
## 						actionBttn("model_training_apply"
## 							, inline=TRUE
## 							, block = FALSE
## 							, color = "success"
## 							, label = get_rv_labels("model_training_apply"))
## 					})
## 				} else {
## 					output$model_training_apply = NULL
## 				}
## 			} else {
## 				output$model_training_apply = NULL
## 			}
## 		} else {
## 			output$model_training_apply = NULL
## 		}
## 	})
## 
## 	## Ttrain 
## 	
## }
