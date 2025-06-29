#### ---- LM and GLM models -----	---------------------------- ####
model_training_caret_models_ols_server = function() {
	
	observeEvent(input$feature_engineering_apply, {
		
				output$model_training_caret_models_ols_check = renderUI({
					if (isTRUE(!is.null(rv_current$working_df))) {
						if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
								if (isTRUE(any(rv_ml_ai$task %in% c("Classification", "Regression")))) {
									if ((isTRUE(isTRUE(rv_ml_ai$task=="Classification")) & isTRUE(rv_ml_ai$preprocessed$outcome_nlevels==2)) | isTRUE(rv_ml_ai$task=="Regression")) {
										if (isTRUE(rv_ml_ai$task=="Classification")) {
											temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_ols_logistic")
										} else {
											temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_ols_linear")	
										}
										rv_training_models$ols_name = temp_label 
										prettyCheckbox(
											"model_training_caret_models_ols_check"
											, label = names(temp_label)
											, status = "success"
											, outline = FALSE
											, inline = TRUE
											, value=FALSE
										)
									}
								}
							}
						}
				 })
	})
	
	
	## LM/GLM Model parameters
	output$model_training_caret_models_ols_advance_intercept = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_ols_check)) {
					if (rv_ml_ai$task=="Regression") {
						box(title = get_rv_labels("model_training_caret_models_advance_options")
							, status = "teal"
							, solidHeader = TRUE
							, collapsible = TRUE
							, collapsed = TRUE
							, width = 6
							, selectInput("model_training_caret_models_ols_advance_intercept"
								, get_rv_labels("model_training_caret_models_ols_advance_intercept")
								, choices = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_ols_advance_intercept")
								, selected = TRUE
								, multiple = TRUE
							) 
							, actionButton("ols_advance_control_apply_save"
								, get_rv_labels("customize_train_control_apply_save") 
							)
						)		
						
					}
					
				}
			}
		}
		
	})
	
}

#### ---- Random forest --------------------------------- ####
model_training_caret_models_rf_server = function() {
	
	observeEvent(input$feature_engineering_apply, {
		
				output$model_training_caret_models_rf_check = renderUI({
					if (isTRUE(!is.null(rv_current$working_df))) {
						if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
								temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_rf")
								rv_training_models$rf_name = temp_label 
								prettyCheckbox(
									"model_training_caret_models_rf_check"
									, label = names(temp_label)
									, status = "success"
									, outline = FALSE
									, inline = TRUE
									, value=FALSE
								)
							}
						}
				 })
	})
	
	
	## RF Model parameters
	output$model_training_caret_models_rf_advance_params = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_rf_check)) {
					box(title = get_rv_labels("model_training_caret_models_advance_options")
						, status = "teal"
						, solidHeader = TRUE
						, collapsible = TRUE
						, collapsed = TRUE
						, width = 6
						, numericInput("model_training_caret_models_rf_advance_params"
							, get_rv_labels("model_training_caret_models_rf_advance_params")
							, value = 1 
						) 
						, actionButton("rf_advance_control_apply_save"
							, get_rv_labels("customize_train_control_apply_save") 
						)
					)			
				}
			}
		}
		
	})
	
}

#### ---- Train all models ----------------------------------- ####
model_training_caret_train_all_server = function() {

	
	## Check hyperparameter list
	observeEvent(input$ols_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_ols_check)) {
					if (rv_ml_ai$task=="Regression") {
						rv_training_models$ols_param = TRUE			
					} else {
						rv_training_models$ols_param = FALSE
					}
				}
			}
		}
		
	})

## 	## Check selected models
## 	observe({
## 		if (isTRUE(!is.null(rv_current$working_df))) {
## 			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
## 				
## 				## LM/GLM
## 				if (isTRUE(!is.null(rv_training_models$ols_name))) {
## 					if (isTRUE(input$model_training_caret_models_ols_check)) {
## 						rv_training_models$ols_model = Rautoml::setup_caret( 
## 							unname(rv_training_models$ols_name)
## 							, param=rv_training_models$ols_param
## 							, param_set=list(intercept=input$model_training_caret_models_ols_advance_intercept)
## 						)
## 						rv_ml_ai$at_least_one_model = TRUE
## 					} else {
## 						rv_training_models$ols_model = NULL
## 						rv_ml_ai$at_least_one_model = FALSE
## 					}
## 				}
## 
## 				## RF
## 				if (isTRUE(!is.null(rv_training_models$rf_name))) {
## 					if (isTRUE(input$model_training_caret_models_rf_check)) {
## 						rv_training_models$rf_model = Rautoml::setup_caret( 
## 							unname(rv_training_models$rf_name)
## 							, param=rv_training_models$rf_param
## 							, param_set=list(mtry=input$model_training_caret_models_rf_advance_params)
## 						)
## 						rv_ml_ai$at_least_one_model = TRUE
## 					} else {
## 						rv_training_models$rf_model = NULL
## 						rv_ml_ai$at_least_one_model = FALSE
## 					}
## 				}
## 				
## 			}
## 		}
## 
## 		
## 	})
	
	## Check selected models
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				
				## LM/GLM
				if (isTRUE(input$model_training_caret_models_ols_check)) {
					rv_training_models$ols_model = Rautoml::setup_caret( 
						unname(rv_training_models$ols_name)
						, param=rv_training_models$ols_param
						, param_set=list(intercept=input$model_training_caret_models_ols_advance_intercept)
					)
					rv_ml_ai$at_least_one_model = TRUE
				}

				if (isTRUE(input$model_training_caret_models_rf_check)) {
					rv_training_models$rf_model = Rautoml::setup_caret( 
						unname(rv_training_models$rf_name)
						, param=rv_training_models$rf_param
						, param_set=list(mtry=input$model_training_caret_models_rf_advance_params)
					)
					rv_ml_ai$at_least_one_model = TRUE
				}


				## Update this for every model
				if (!isTRUE(input$model_training_caret_models_ols_check) & !isTRUE(input$model_training_caret_models_rf_check)) {
					rv_ml_ai$at_least_one_model = FALSE
				}
			}
		}

		
	})

   ##	Train model action
	observe({
		output$model_training_apply = renderUI({
			if (isTRUE(!is.null(rv_current$working_df))) {
				if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
					if (isTRUE(rv_ml_ai$at_least_one_model)) {
						p(br()
							, actionBttn("model_training_apply"
								, inline=TRUE
								, block = FALSE
								, color = "success"
								, label = get_rv_labels("model_training_apply")
							)
						)
					}
				}
			}
		})
	})

	## Model list
	output$model_training_caret_models_ui = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				p(br()
				  , hr()
				  , box(
						 title = get_rv_labels("model_training_caret_models_ui"),
						 status = "success",
						 solidHeader = TRUE,
						 collapsible = TRUE,
						 collapsed = TRUE,
						 width = 12,
						 fluidRow (
							column(width=6
								, uiOutput("model_training_caret_models_ols_check")
								, uiOutput("model_training_caret_models_ols_advance_intercept")
							)
							, column(width=6
								, uiOutput("model_training_caret_models_rf_check")
								, uiOutput("model_training_caret_models_rf_advance_params")
							)
						 )
						 , uiOutput("model_training_apply")
				  )
				)
			}
		}
	})

	## Train models
	observeEvent(input$model_training_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(rv_ml_ai$at_least_one_model)) {
					all_model_params = c(rv_training_models$ols_model
						, rv_training_models$rf_model
					)
					xxx = Rautoml::train_caret_models(
						df=rv_ml_ai$preprocessed$train_df
						, model_form=rv_ml_ai$model_formula
						, ctrl=reactiveValuesToList(rv_train_control_caret)
						, model_list=all_model_params
						, metric=input$model_training_setup_eval_metric
					)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_rf_check", value=FALSE)
					rv_ml_ai$at_least_one_model = FALSE
					rv_training_models$ols_model = NULL
					rv_training_models$rf_model = NULL
				}
			}
		}
	})
	
	
}

model_training_caret_models_ols_server_old12 = function() {
	observeEvent(input$feature_engineering_apply, {
		
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				

				## GLM and LM models
				if (isTRUE(rv_ml_ai$task=="Classification")) {
					temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_ols_logistic")	
						if (isTRUE(!is.null(rv_ml_ai$preprocessed$outcome_nlevels))) {
							if (isTRUE(rv_ml_ai$preprocessed$outcome_nlevels!=2)) {
								rv_training_results$ols_name = temp_label 
								model_training_caret_models_ols_check = prettyCheckbox(
									"model_training_caret_models_ols_check"
									, label = names(temp_label)
									, status = "success"
									, outline = FALSE
									, inline = TRUE
								)	
								
							} else {
								model_training_caret_models_ols_check = NULL	
								output$model_training_caret_models_ols = NULL
								updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=NULL)
							}
						} else {
							model_training_caret_models_ols_check = NULL	
							output$model_training_caret_models_ols = NULL
							updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=NULL)
						}
				} else if (isTRUE(rv_ml_ai$task=="Regression")) {
					temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_ols_linear")	
						rv_training_results$ols_name = temp_label 
						model_training_caret_models_ols_check = prettyCheckbox(
							"model_training_caret_models_ols_check"
							, label = names(temp_label)
							, status = "success"
							, outline = FALSE
							, inline = TRUE
						)	
				} else {
					model_training_caret_models_ols_check = NULL
					output$model_training_caret_models_ols = NULL
					updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=NULL)
				}
				rv_ui_models$model_training_caret_models_ols_check = model_training_caret_models_ols_check	


			} else {
				output$model_training_caret_models_ui = NULL	
				model_training_caret_models_ols_check = NULL
				output$model_training_caret_models_ols = NULL
				updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=NULL)
				rv_ui_models$model_training_caret_models_ols_check = NULL	
			}
		} else {
			output$model_training_caret_models_ui = NULL	
			model_training_caret_models_ols_check = NULL
			output$model_training_caret_models_ols = NULL
			updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=NULL)
			rv_ui_models$model_training_caret_models_ols_check = NULL	
		}
	})


	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				## Hyperparameters for OLS
				if (isTRUE(input$model_training_caret_models_ols_check)) {
					rv_ml_ai$at_least_one_model = TRUE
					if (isTRUE(rv_ml_ai$task=="Regression")) {
						rv_ui_models$model_training_caret_models_ols_advance_control = selectInput(
							"test"
							, "Test"
							, c("A", "B")
						
						)
						print(rv_ui_models$model_training_caret_models_ols_advance_control)
					} else {
				#		rv_ui_models$model_training_caret_models_ols_advance_control = NULL
					}
				} else {
				#	rv_ui_models$model_training_caret_models_ols_advance_control = NULL
				}
			}
		}
	})

				
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				output$model_training_caret_models_ui = renderUI({
					p(br()
					  , hr()
					  , box(
							 title = get_rv_labels("model_training_caret_models_ui"),
							 status = "success",
							 solidHeader = TRUE,
							 collapsible = TRUE,
							 collapsed = TRUE,
							 width = 12,
							 fluidRow (
								column(width=4
							 		, rv_ui_models$model_training_caret_models_ols_check
								)
							 )
					  )
					)
				})
				
			} else {
				output$model_training_caret_models_ui = NULL
			}
		} else {
			output$model_training_caret_models_ui = NULL
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
						rv_training_results$ols_name = temp_label 
						prettyCheckbox(
							"model_training_caret_models_ols_check"
							, label = names(temp_label)
							, status = "success"
							, outline = FALSE
							, inline = TRUE
						)	
					})
					print(rv_ml_ai$preprocessed$outcome_nlevels)
					if (isTRUE(rv_ml_ai$task=="Classification")) {
						if (isTRUE(!is.null(rv_ml_ai$preprocessed$outcome_nlevels))) {
							if (isTRUE(rv_ml_ai$preprocessed$outcome_nlevels!=2)) {
					print("What")
					print(rv_ml_ai$preprocessed$outcome_nlevels)
								rv_training_results$ols_name = NULL
								output$model_training_caret_models_ols = NULL
								updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=NULL)
							}
						}
					}
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
				if (isTRUE(input$model_training_caret_models_ols_check)) {
					rv_ml_ai$at_least_one_model = TRUE
					if (isTRUE(rv_ml_ai$task=="Regression")) {
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
				} else {
					rv_ml_ai$at_least_one_model = FALSE
				}
			}
		}
	})

	observeEvent(input$ols_advance_control_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				## Hyperparameters for OLS
				if (isTRUE(rv_ml_ai$task=="Regression")) {
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

}



#### ---- Train all models ------------------------------------------- #####

model_training_caret_train_all_server_old = function() {
	
	## Model training apply
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(rv_ml_ai$at_least_one_model)) {
					output$model_training_apply = renderUI({
						p(br()
							, actionBttn("model_training_apply"
								, inline=TRUE
								, block = FALSE
								, color = "success"
								, label = get_rv_labels("model_training_apply")
							)
						)
					})
				} else {
					output$model_training_apply = NULL
				}
			} else {
				output$model_training_apply = NULL
			}
		} else {
			output$model_training_apply = NULL
		}
	})

	## Ttrain 
	observeEvent(input$model_training_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$modelling_framework_choices=="Caret")) {
					if (isTRUE(rv_ml_ai$at_least_one_model)) {
						if (isTRUE(input$model_training_caret_models_ols_check)) {
							model = Rautoml::setup_caret( 
								unname(rv_training_results$ols_name)
								, param=rv_training_results$ols_param
								, param_set=list(intercept=input$model_training_caret_models_ols_advance_intercept)
							)
							xxx = Rautoml::train_caret_models(
								df=rv_ml_ai$preprocessed$train_df
								, model_form=rv_ml_ai$model_formula
								, ctrl=reactiveValuesToList(rv_train_control_caret)
								, model_list=model
								, metric=input$model_training_setup_eval_metric
							)
							print(xxx)
						}
					}
				}
			}
		}
		
	})

	
}
