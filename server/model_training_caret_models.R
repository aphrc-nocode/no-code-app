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
					rv_training_models$ols_trained_model = rv_training_models$ols_name
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
					rv_training_models$rf_trained_model = rv_training_models$rf_name
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
							, value = c(0.001, 0.9999)
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
							, value = c(0.001, 0.9999)
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



#### ---- svmLinear --------------------------------- ####
model_training_caret_models_svmLinear_server = function() {
	
	observeEvent(input$feature_engineering_apply, {
		
				output$model_training_caret_models_svmLinear_check = renderUI({
					if (isTRUE(!is.null(rv_current$working_df))) {
						if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
								temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_svmLinear")
								rv_training_models$svmLinear_name = temp_label 
								prettyCheckbox(
									"model_training_caret_models_svmLinear_check"
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
	
	
	## svmLinear Model parameters
	output$model_training_caret_models_svmLinear_advance_params = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_svmLinear_check)) {
					box(title = get_rv_labels("model_training_caret_models_advance_options")
						, status = "teal"
						, solidHeader = TRUE
						, collapsible = TRUE
						, collapsed = TRUE
						, width = 6
						, selectInput("model_training_caret_models_svmLinear_advance_C"
							, get_rv_labels("model_training_caret_models_svmLinear_advance_C")
							, choices =  2^(-2:7) 
							, selected = 1
							, multiple = TRUE
						)
						, actionButton("svmLinear_advance_control_apply_save"
							, get_rv_labels("customize_train_control_apply_save") 
						)
					)			
				}
			}
		}
		
	})
	
}



#### ---- svmPoly --------------------------------- ####
model_training_caret_models_svmPoly_server = function() {
	
	observeEvent(input$feature_engineering_apply, {
		
				output$model_training_caret_models_svmPoly_check = renderUI({
					if (isTRUE(!is.null(rv_current$working_df))) {
						if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
								temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_svmPoly")
								rv_training_models$svmPoly_name = temp_label 
								prettyCheckbox(
									"model_training_caret_models_svmPoly_check"
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
	
	
	## svmPoly Model parameters
	output$model_training_caret_models_svmPoly_advance_params = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_svmPoly_check)) {
					box(title = get_rv_labels("model_training_caret_models_advance_options")
						, status = "teal"
						, solidHeader = TRUE
						, collapsible = TRUE
						, collapsed = TRUE
						, width = 6
						, selectInput("model_training_caret_models_svmPoly_advance_degree"
							, get_rv_labels("model_training_caret_models_svmPoly_advance_degree")
							, choices =  1:10 
							, selected = 1
							, multiple = TRUE
						)
	  					, sliderInput("model_training_caret_models_svmPoly_advance_scale"
							, get_rv_labels("model_training_caret_models_svmPoly_advance_scale")
							, min = 0
							, max = 1
							, value = c(0.001, 0.9999)
						)
						, selectInput("model_training_caret_models_svmPoly_advance_C"
							, get_rv_labels("model_training_caret_models_svmPoly_advance_C")
							, choices =  2^(-2:7) 
							, selected = 1
							, multiple = TRUE
						)
						, actionButton("svmPoly_advance_control_apply_save"
							, get_rv_labels("customize_train_control_apply_save") 
						)
					)			
				}
			}
		}
	})
	
}


#### ---- glmnet --------------------------------- ####
model_training_caret_models_glmnet_server = function() {
	
	observeEvent(input$feature_engineering_apply, {
		
				output$model_training_caret_models_glmnet_check = renderUI({
					if (isTRUE(!is.null(rv_current$working_df))) {
						if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
								temp_label = get_named_choices(input_choices_file, input$change_language,"model_training_caret_models_glmnet")
								rv_training_models$glmnet_name = temp_label 
								prettyCheckbox(
									"model_training_caret_models_glmnet_check"
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
	
	
	## glmnet Model parameters
	output$model_training_caret_models_glmnet_advance_params = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_glmnet_check)) {
					box(title = get_rv_labels("model_training_caret_models_advance_options")
						, status = "teal"
						, solidHeader = TRUE
						, collapsible = TRUE
						, collapsed = TRUE
						, width = 6
						, selectInput("model_training_caret_models_glmnet_advance_alpha"
							, get_rv_labels("model_training_caret_models_glmnet_advance_alpha")
							, choices = seq(0.1, 1, length.out=10) 
							, selected = 1
							, multiple = TRUE
						)
						, actionButton("glmnet_advance_control_apply_save"
							, get_rv_labels("customize_train_control_apply_save") 
						)
					)			
				}
			}
		}
	})
	
}






