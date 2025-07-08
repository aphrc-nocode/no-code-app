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
						, selectInput("model_training_caret_models_rf_advance_params"
							, get_rv_labels("model_training_caret_models_rf_advance_params")
							, choices = 1:100
							, selected = 5
							, multiple = TRUE
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



#### ---- GBM --------------------------------- ####
model_training_caret_models_gbm_server = function() {
	
	observeEvent(input$feature_engineering_apply, {
		
				output$model_training_caret_models_gbm_check = renderUI({
					if (isTRUE(!is.null(rv_current$working_df))) {
						if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
								temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_gbm")
								rv_training_models$gbm_name = temp_label 
								prettyCheckbox(
									"model_training_caret_models_gbm_check"
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
	
	
	## GBM Model parameters
	output$model_training_caret_models_gbm_advance_params = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_gbm_check)) {
					box(title = get_rv_labels("model_training_caret_models_advance_options")
						, status = "teal"
						, solidHeader = TRUE
						, collapsible = TRUE
						, collapsed = TRUE
						, width = 6
						, selectInput("model_training_caret_models_gbm_advance_n.trees"
							, get_rv_labels("model_training_caret_models_gbm_advance_n.trees")
							, choices = c(50, 100, 500, 1000, 1500, 2000)
							, selected = 100
							, multiple = TRUE
						)

						, selectInput("model_training_caret_models_gbm_advance_interaction.depth"
							, get_rv_labels("model_training_caret_models_gbm_advance_interaction.depth")
							, choices = c(5, 10, 15, 20, 25)
							, selected = 10
							, multiple=TRUE
						)
						
						, selectInput("model_training_caret_models_gbm_advance_n.minobsinnode"
							, get_rv_labels("model_training_caret_models_gbm_advance_n.minobsinnode")
							, choices = c(1, 2, 3, 4, 5)
							, selected = 1
							, multiple=TRUE
						)

	  					, sliderInput("model_training_caret_models_gbm_advance_shrinkage"
							, get_rv_labels("model_training_caret_models_gbm_advance_shrinkage")
							, min = 0.001
							, max = 0.1
							, value = c(0.001, 0.1)
						)
						, actionButton("gbm_advance_control_apply_save"
							, get_rv_labels("customize_train_control_apply_save") 
						)
					)			
				}
			}
		}
		
	})
	
}


#### ---- xgbTree --------------------------------- ####
model_training_caret_models_xgbTree_server = function() {
	
	observeEvent(input$feature_engineering_apply, {
		
				output$model_training_caret_models_xgbTree_check = renderUI({
					if (isTRUE(!is.null(rv_current$working_df))) {
						if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
								temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_xgbTree")
								rv_training_models$xgbTree_name = temp_label 
								prettyCheckbox(
									"model_training_caret_models_xgbTree_check"
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
	
	
	## xgbTree Model parameters
	output$model_training_caret_models_xgbTree_advance_params = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_xgbTree_check)) {
					box(title = get_rv_labels("model_training_caret_models_advance_options")
						, status = "teal"
						, solidHeader = TRUE
						, collapsible = TRUE
						, collapsed = TRUE
						, width = 6
						, selectInput("model_training_caret_models_xgbTree_advance_nrounds"
							, get_rv_labels("model_training_caret_models_xgbTree_advance_nrounds")
							, choices = c(50, 100, 500, 1000, 1500, 2000)
							, selected = 100
							, multiple = TRUE
						)

						, selectInput("model_training_caret_models_xgbTree_advance_max_depth"
							, get_rv_labels("model_training_caret_models_xgbTree_advance_max_depth")
							, choices = c(5, 10, 15, 20, 25)
							, selected = 10
							, multiple=TRUE
						)
	  					, sliderInput("model_training_caret_models_xgbTree_advance_eta"
							, get_rv_labels("model_training_caret_models_xgbTree_advance_eta")
							, min = 0
							, max = 1
							, value = c(0, 1)
						)
						, selectInput("model_training_caret_models_xgbTree_advance_gamma"
							, get_rv_labels("model_training_caret_models_xgbTree_advance_gamma")
							, choices = c(0.1, 0.2, 0.3, 0.5, 0, 1, 2, 3, 4, 5)
							, selected = 1
							, multiple=TRUE
						)
	  					, selectInput("model_training_caret_models_xgbTree_advance_colsample_bytree"
							, get_rv_labels("model_training_caret_models_xgbTree_advance_colsample_bytree")
							, choices = c(0.1, 0.2, 0.5, 1, 2, 3, 5)
							, selected = 1
							, multiple=TRUE
						)
	  					, selectInput("model_training_caret_models_xgbTree_advance_min_child_weight"
							, get_rv_labels("model_training_caret_models_xgbTree_advance_min_child_weight")
							, choices = c(0.1, 0.2, 0.5, 1, 2, 3, 5)
							, selected = 1
							, multiple=TRUE
						)
	  					, selectInput("model_training_caret_models_xgbTree_advance_subsample"
							, get_rv_labels("model_training_caret_models_xgbTree_advance_subsample")
							, choices = seq(0.1, 1, length.out=10)
							, selected = 1
							, multiple=TRUE
						)

						, actionButton("xgbTree_advance_control_apply_save"
							, get_rv_labels("customize_train_control_apply_save") 
						)
					)			
				}
			}
		}
		
	})
	
}


#### ---- xgbLinear --------------------------------- ####
model_training_caret_models_xgbLinear_server = function() {
	
	observeEvent(input$feature_engineering_apply, {
		
				output$model_training_caret_models_xgbLinear_check = renderUI({
					if (isTRUE(!is.null(rv_current$working_df))) {
						if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
								temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_xgbLinear")
								rv_training_models$xgbLinear_name = temp_label 
								prettyCheckbox(
									"model_training_caret_models_xgbLinear_check"
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
	
	
	## xgbLinear Model parameters
	output$model_training_caret_models_xgbLinear_advance_params = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_xgbLinear_check)) {
					box(title = get_rv_labels("model_training_caret_models_advance_options")
						, status = "teal"
						, solidHeader = TRUE
						, collapsible = TRUE
						, collapsed = TRUE
						, width = 6
						, selectInput("model_training_caret_models_xgbLinear_advance_nrounds"
							, get_rv_labels("model_training_caret_models_xgbLinear_advance_nrounds")
							, choices = c(10, 50, 100, 500, 1000, 1500, 2000)
							, selected = 100
							, multiple = TRUE
						)
						, selectInput("model_training_caret_models_xgbLinear_advance_lambda"
							, get_rv_labels("model_training_caret_models_xgbLinear_advance_lambda")
							, choices = c(0, 0.01, 0.05, 0.1, 0.1, 0.5, 0.8)
							, selected = 0
							, multiple=TRUE
						)
						, selectInput("model_training_caret_models_xgbLinear_advance_alpha"
							, get_rv_labels("model_training_caret_models_xgbLinear_advance_alpha")
							, choices = c(0, 0.01, 0.05, 0.1, 0.1, 0.5, 0.8)
							, selected = 0
							, multiple=TRUE
						)
	  					, sliderInput("model_training_caret_models_xgbLinear_advance_eta"
							, get_rv_labels("model_training_caret_models_xgbLinear_advance_eta")
							, min = 0
							, max = 1
							, value = c(0, 1)
						)
						, actionButton("xgbLinear_advance_control_apply_save"
							, get_rv_labels("customize_train_control_apply_save") 
						)
					)			
				}
			}
		}
		
	})
	
}



#### ---- svmRadial --------------------------------- ####
model_training_caret_models_svmRadial_server = function() {
	
	observeEvent(input$feature_engineering_apply, {
		
				output$model_training_caret_models_svmRadial_check = renderUI({
					if (isTRUE(!is.null(rv_current$working_df))) {
						if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
								temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_svmRadial")
								rv_training_models$svmRadial_name = temp_label 
								prettyCheckbox(
									"model_training_caret_models_svmRadial_check"
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
	
	
	## svmRadial Model parameters
	output$model_training_caret_models_svmRadial_advance_params = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_svmRadial_check)) {
					box(title = get_rv_labels("model_training_caret_models_advance_options")
						, status = "teal"
						, solidHeader = TRUE
						, collapsible = TRUE
						, collapsed = TRUE
						, width = 6
	  					, sliderInput("model_training_caret_models_svmRadial_advance_sigma"
							, get_rv_labels("model_training_caret_models_svmRadial_advance_sigma")
							, min = 0
							, max = 1
							, value = c(0.001, 0.9999)
						)
						, selectInput("model_training_caret_models_svmRadial_advance_C"
							, get_rv_labels("model_training_caret_models_svmRadial_advance_C")
							, choices =  2^(-2:7) 
							, selected = 1
							, multiple = TRUE
						)
						, actionButton("svmRadial_advance_control_apply_save"
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
	
	### LM/GLM
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

	### LM/GLM
	observeEvent(input$rf_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_rf_check)) {
					rv_training_models$rf_param = TRUE			
				}
			}
		}
		
	})
	
	### GBM
	observeEvent(input$gbm_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_gbm_check)) {
					rv_training_models$gbm_param = TRUE			
				}
			}
		}
	})
	
	### xgbTree
	observeEvent(input$xgbTree_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_xgbTree_check)) {
					rv_training_models$xgbTree_param = TRUE			
				}
			}
		}
	})
	
	### xgbLinear
	observeEvent(input$xgbLinear_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_xgbLinear_check)) {
					rv_training_models$xgbLinear_param = TRUE			
				}
			}
		}
	})
	
	### svmRadial
	observeEvent(input$svmRadial_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_svmRadial_check)) {
					rv_training_models$svmRadial_param = TRUE			
				}
			}
		}
	})


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

				
				## RF
				if (isTRUE(input$model_training_caret_models_rf_check)) {
					rv_training_models$rf_model = Rautoml::setup_caret( 
						unname(rv_training_models$rf_name)
						, param=rv_training_models$rf_param
						, param_set=list(
							mtry=as.numeric(input$model_training_caret_models_gbm_advance_params)
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				}

				## GBM
				if (isTRUE(input$model_training_caret_models_gbm_check)) {
					if (isTRUE(!is.null(input$model_training_caret_models_gbm_advance_shrinkage))) {
						shrinkage = input$model_training_caret_models_gbm_advance_shrinkage
						shrinkage = seq(shrinkage[1], shrinkage[2], length.out=3)
					} else {
						shrinkage = input$model_training_caret_models_gbm_advance_shrinkage
					}
					rv_training_models$gbm_model = Rautoml::setup_caret( 
						unname(rv_training_models$gbm_name)
						, param=rv_training_models$gbm_param
						, param_set=list(
							n.trees=as.numeric(input$model_training_caret_models_gbm_advance_n.trees)
							, interaction.depth = as.numeric(input$model_training_caret_models_gbm_advance_interaction.depth)
							, n.minobsinnode = as.numeric(input$model_training_caret_models_gbm_advance_n.minobsinnode)
							, shrinkage = shrinkage 
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				}

				## xgbTree
				if (isTRUE(input$model_training_caret_models_xgbTree_check)) {
					if (isTRUE(!is.null(input$model_training_caret_models_xgbTree_advance_shrinkage))) {
						eta = input$model_training_caret_models_xgbTree_advance_eta
						eta = seq(eta[1], eta[2], length.out=3)
					} else {
						eta = input$model_training_caret_models_xgbTree_advance_eta
					}
					rv_training_models$xgbTree_model = Rautoml::setup_caret( 
						unname(rv_training_models$xgbTree_name)
						, param=rv_training_models$xgbTree_param
						, param_set=list(
							nrounds=as.numeric(input$model_training_caret_models_xgbTree_advance_nrounds)
							, max_depth=as.numeric(input$model_training_caret_models_xgbTree_advance_max_depth)
							, eta=eta
							, gamma=as.numeric(input$model_training_caret_models_xgbTree_advance_gamma)
							, colsample_bytree=as.numeric(input$model_training_caret_models_xgbTree_advance_colsample_bytree)
							, min_child_weight=as.numeric(input$model_training_caret_models_xgbTree_advance_min_child_weight)
							, subsample=as.numeric(input$model_training_caret_models_xgbTree_advance_subsample)
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				}


				## xgbLinear
				if (isTRUE(input$model_training_caret_models_xgbLinear_check)) {
					if (isTRUE(!is.null(input$model_training_caret_models_xgbLinear_advance_shrinkage))) {
						eta = input$model_training_caret_models_xgbLinear_advance_eta
						eta = seq(eta[1], eta[2], length.out=3)
					} else {
						eta = input$model_training_caret_models_xgbLinear_advance_eta
					}
					rv_training_models$xgbLinear_model = Rautoml::setup_caret( 
						unname(rv_training_models$xgbLinear_name)
						, param=rv_training_models$xgbLinear_param
						, param_set=list(
							nrounds=as.numeric(input$model_training_caret_models_xgbLinear_advance_nrounds)
							, lambda=as.numeric(input$model_training_caret_models_xgbLinear_advance_lambda)
							, alpha=as.numeric(input$model_training_caret_models_xgbLinear_advance_alpha)
							, eta=eta
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				}

				## svmRadial
				if (isTRUE(input$model_training_caret_models_svmRadial_check)) {
					if (isTRUE(!is.null(input$model_training_caret_models_svmRadial_advance_shrinkage))) {
						C_ = input$model_training_caret_models_svmRadial_advance_C
						C_ = seq(C_[1], C_[2], length.out=3)
					} else {
						C_ = input$model_training_caret_models_svmRadial_advance_C
					}
					rv_training_models$svmRadial_model = Rautoml::setup_caret( 
						unname(rv_training_models$svmRadial_name)
						, param=rv_training_models$svmRadial_param
						, param_set=list(
							sigma=as.numeric(input$model_training_caret_models_svmRadial_advance_sigma)
							, C=C_
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				}




				## Update this for every model
				if (!isTRUE(input$model_training_caret_models_ols_check) & !isTRUE(input$model_training_caret_models_rf_check) & !isTRUE(input$model_training_caret_models_gbm_check) & !isTRUE(input$model_training_caret_models_xgbTree_check) & !isTRUE(input$model_training_caret_models_xgbLinear_check) & !isTRUE(input$model_training_caret_models_svmRadial_check)   ) {
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
							column(width=4
								, uiOutput("model_training_caret_models_ols_check")
								, uiOutput("model_training_caret_models_ols_advance_intercept")
							)
							, column(width=4
								, uiOutput("model_training_caret_models_rf_check")
								, uiOutput("model_training_caret_models_rf_advance_params")
							)
							, column(width=4
								, uiOutput("model_training_caret_models_gbm_check")
								, uiOutput("model_training_caret_models_gbm_advance_params")
							)
						 )
						 , fluidRow (
							column(width=4
								, uiOutput("model_training_caret_models_xgbTree_check")
								, uiOutput("model_training_caret_models_xgbTree_advance_params")
							)
							, column(width=4
								, uiOutput("model_training_caret_models_xgbLinear_check")
								, uiOutput("model_training_caret_models_xgbLinear_advance_params")
							)
							, column(width=4
								, uiOutput("model_training_caret_models_svmRadial_check")
								, uiOutput("model_training_caret_models_svmRadial_advance_params")
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
						, rv_training_models$gbm_model
						, rv_training_models$xgbTree_model
						, rv_training_models$xgbLinear_model
						, rv_training_models$svmRadial_model
					)
					set.seed(rv_ml_ai$seed_value)
					if (isTRUE(input$model_training_setup_start_clusters_check)) {
						Rautoml::start_cluster()
					}
					rv_training_results$models = Rautoml::train_caret_models(
						df=rv_ml_ai$preprocessed$train_df
						, model_form=rv_ml_ai$model_formula
						, ctrl=reactiveValuesToList(rv_train_control_caret)
						, model_list=all_model_params
						, metric=input$model_training_setup_eval_metric
					)
					if (isTRUE(input$model_training_setup_start_clusters_check)) {
						Rautoml::stop_cluster()
					}
					if (isTRUE(input$model_training_setup_include_ensemble_check)) {
						rv_training_results$models = create_ensemble(
							all.models = rv_training_results$models
							, ctrl=reactiveValuesToList(rv_train_control_caret)
							, metric=input$model_training_setup_eval_metric
						)
					}
					rv_training_results$train_metrics_df=Rautoml::extract_summary(rv_training_results$models)

						rv_training_results$test_metrics_objs=Rautoml::boot_estimates_multiple(
							models=rv_training_results$models
							, df=rv_ml_ai$preprocessed$test_df
							, outcome_var=rv_ml_ai$outcome
							, problem_type=rv_ml_ai$task
							, nreps=100
							, model_name=NULL
							, type="prob"
							, report= input$model_training_setup_eval_metric
							, summary_fun=Rautoml::quantile_summary
						)

					updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_rf_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_gbm_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_xgbTree_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_xgbLinear_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_svmRadial_check", value=FALSE)
					rv_ml_ai$at_least_one_model = FALSE
					rv_training_models$ols_model = NULL
					rv_training_models$rf_model = NULL
					rv_training_models$gbm_model = NULL
					rv_training_models$xgbTree_model = NULL
					rv_training_models$xgbLinear_model = NULL
					rv_training_models$svmRadial_model = NULL
					
					rv_train_control_caret$method = "cv"
					rv_train_control_caret$number = 5
					rv_train_control_caret$repeats = NA
					rv_train_control_caret$search = "grid"
					rv_train_control_caret$verboseIter = FALSE
					rv_train_control_caret$savePredictions = FALSE
					rv_train_control_caret$classProbs = TRUE

				} else {
					rv_training_results$models = NULL
					rv_training_results$train_metrics_df = NULL
					rv_training_results$test_metrics_objs = NULL
				}
			} else {
				rv_training_results$models = NULL
				rv_training_results$train_metrics_df = NULL
				rv_training_results$test_metrics_objs = NULL
			}
		} else {
			rv_training_results$models = NULL
			rv_training_results$train_metrics_df = NULL
			rv_training_results$test_metrics_objs = NULL
		}
	})	
}



#### ---- Train all models ----------------------------------- ####
model_training_caret_train_metrics_server = function() {
	
	
	observeEvent(input$model_training_apply, {
		
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(!is.null(rv_training_results$train_metrics_df))) {

					## Training data
					output$model_training_caret_train_metrics_plot = renderPlot({
						plot(rv_training_results$train_metrics_df)	
					})

					output$model_training_caret_train_metrics_df = DT::renderDT({
					  df = rv_training_results$train_metrics_df

					  DT::datatable(
						 df,
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
					  ) %>%
						 DT::formatRound(columns = c("lower", "estimate", "upper"), digits = 4)
					})
					
					if (isTRUE(!is.null(rv_training_results$test_metrics_objs))) {
						## Test data
						test_plots = plot(rv_training_results$test_metrics_objs)
						output$model_training_caret_test_metrics_plot_specifics = renderPlot({
							test_plots$specifics
						})
						
						output$model_training_caret_test_metrics_plot_all = renderPlot({
							test_plots$all	
						})

						output$model_training_caret_test_metrics_plot_roc = renderPlot({
							test_plots$roc
						})
						
						output$model_training_caret_test_metrics_df = DT::renderDT({
						  df = rv_training_results$test_metrics_objs$all

						  DT::datatable(
							 df,
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
						  ) %>%
							 DT::formatRound(columns = c("lower", "estimate", "upper"), digits = 4)
						})
					} else {
						output$model_training_caret_test_metrics_plot_specifics = NULL
						output$model_training_caret_test_metrics_plot_all = NULL
						output$model_training_caret_test_metrics_plot_roc = NULL
						output$model_training_caret_test_metrics_df = NULL
					}

				} else {
					output$model_training_caret_train_metrics_plot = NULL
					output$model_training_caret_train_metrics_df = NULL
						
					output$model_training_caret_test_metrics_plot_specifics = NULL
					output$model_training_caret_test_metrics_plot_all = NULL
					output$model_training_caret_test_metrics_plot_roc = NULL
					output$model_training_caret_test_metrics_df = NULL
				}
			} else {
				output$model_training_caret_train_metrics_plot = NULL
				output$model_training_caret_train_metrics_df = NULL
					
				output$model_training_caret_test_metrics_plot_specifics = NULL
				output$model_training_caret_test_metrics_plot_all = NULL
				output$model_training_caret_test_metrics_plot_roc = NULL
				output$model_training_caret_test_metrics_df = NULL
			}
		} else {
			output$model_training_caret_train_metrics_plot = NULL
			output$model_training_caret_train_metrics_df = NULL
				
			output$model_training_caret_test_metrics_plot_specifics = NULL
			output$model_training_caret_test_metrics_plot_all = NULL
			output$model_training_caret_test_metrics_plot_roc = NULL
			output$model_training_caret_test_metrics_df = NULL
		}
	})
	
	
	output$model_training_caret_train_metrics = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(!is.null(rv_training_results$train_metrics_df))) {

						p(br()
							, hr()
							, HTML(paste0("<b>", get_rv_labels("model_training_caret_train_metrics"), ":</b> <br/>"))
							, tabsetPanel(
								tabPanel(get_rv_labels("model_training_caret_train_metrics_training")
									, p(
										br()
										, box(title = NULL 
											, status = "success"
											, solidHeader = TRUE
											, collapsible = TRUE
											, collapsed = FALSE
											, width = 12
											, fluidRow(
												column(width = 12
													, DT::DTOutput("model_training_caret_train_metrics_df", width="100%", fill=TRUE)
												)
											)
											, hr()
											, fluidRow(
												 column(width=12
													, plotOutput("model_training_caret_train_metrics_plot")
												)
											)
										)
									)
								)
								, tabPanel(get_rv_labels("model_training_caret_train_metrics_test")
									, p(
										br()
										, box(title = NULL 
											, status = "success"
											, solidHeader = TRUE
											, collapsible = TRUE
											, collapsed = FALSE
											, width = 12
											, fluidRow(
												column(width = 6
													, DT::DTOutput("model_training_caret_test_metrics_df", width="100%", fill=TRUE)
												)
												 , column(width=6
													, plotOutput("model_training_caret_test_metrics_plot_specifics")
												)
											)
											, hr()
											, fluidRow(
												 column(width=12
													, plotOutput("model_training_caret_test_metrics_plot_all")
												)
											)
											, hr()
											, fluidRow(
												column(width=12
													, plotOutput("model_training_caret_test_metrics_plot_roc")
												)
											)
										)
									)
								)
							)
						)



				}
			}
		}
	})
	 
}
