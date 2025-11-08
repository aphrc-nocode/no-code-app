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

	
	### svmLinear
	observeEvent(input$svmLinear_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_svmLinear_check)) {
					rv_training_models$svmLinear_param = TRUE			
				}
			}
		}
	})


	### svmPoly
	observeEvent(input$svmPoly_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_svmPoly_check)) {
					rv_training_models$svmPoly_param = TRUE			
				}
			}
		}
	})


	### glmnet
	observeEvent(input$glmnet_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_glmnet_check)) {
					rv_training_models$glmnet_param = TRUE			
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
					if (isTRUE(!is.null(input$model_training_caret_models_xgbTree_advance_eta))) {
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
					if (isTRUE(!is.null(input$model_training_caret_models_xgbLinear_advance_eta))) {
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
					if (isTRUE(!is.null(input$model_training_caret_models_svmRadial_advance_sigma))) {
						sigma = input$model_training_caret_models_svmRadial_advance_sigma
						sigma = seq(sigma[1], sigma[2], length.out=3)
					} else {
						sigma = input$model_training_caret_models_svmRadial_advance_sigma
					}
					rv_training_models$svmRadial_model = Rautoml::setup_caret( 
						unname(rv_training_models$svmRadial_name)
						, param=rv_training_models$svmRadial_param
						, param_set=list(
							C=as.numeric(input$model_training_caret_models_svmRadial_advance_C)
							, sigma=sigma
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				}

				## svmLinear
				if (isTRUE(input$model_training_caret_models_svmLinear_check)) {
					rv_training_models$svmLinear_model = Rautoml::setup_caret( 
						unname(rv_training_models$svmLinear_name)
						, param=rv_training_models$svmLinear_param
						, param_set=list(
							C=as.numeric(input$model_training_caret_models_svmLinear_advance_C)
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				}


				## svmPoly
				if (isTRUE(input$model_training_caret_models_svmPoly_check)) {
					if (isTRUE(!is.null(input$model_training_caret_models_svmPoly_advance_scale))) {
						scale_ = input$model_training_caret_models_svmPoly_advance_scale
						scale_ = seq(scale_[1], scale_[2], length.out=3)
					} else {
						scale_ = input$model_training_caret_models_svmPoly_advance_scale
					}
					rv_training_models$svmPoly_model = Rautoml::setup_caret( 
						unname(rv_training_models$svmPoly_name)
						, param=rv_training_models$svmPoly_param
						, param_set=list(
							degree=as.numeric(input$model_training_caret_models_svmPoly_advance_degree)
							, C = as.numeric(input$model_training_caret_models_svmPoly_advance_C)
							, scale=scale_
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				}


				## glmnet
				if (isTRUE(input$model_training_caret_models_glmnet_check)) {
					rv_training_models$glmnet_model = Rautoml::setup_caret( 
						unname(rv_training_models$glmnet_name)
						, param=rv_training_models$glmnet_param
						, param_set=list(
							degree=as.numeric(input$model_training_caret_models_glmnet_advance_alpha)
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				}

				
				## Update this for every model
				if (!isTRUE(input$model_training_caret_models_ols_check) & !isTRUE(input$model_training_caret_models_rf_check) & !isTRUE(input$model_training_caret_models_gbm_check) & !isTRUE(input$model_training_caret_models_xgbTree_check) & !isTRUE(input$model_training_caret_models_xgbLinear_check) & !isTRUE(input$model_training_caret_models_svmRadial_check) & !isTRUE(input$model_training_caret_models_svmLinear_check) & !isTRUE(input$model_training_caret_models_svmPoly_check) & !isTRUE(input$model_training_caret_models_glmnet_check) ) {
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
						 , fluidRow (
							column(width=4
								, uiOutput("model_training_caret_models_svmLinear_check")
								, uiOutput("model_training_caret_models_svmLinear_advance_params")
							)
							, column(width=4
								, uiOutput("model_training_caret_models_svmPoly_check")
								, uiOutput("model_training_caret_models_svmPoly_advance_params")
							)
							, column(width=4
								, uiOutput("model_training_caret_models_glmnet_check")
								, uiOutput("model_training_caret_models_glmnet_advance_params")
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

					start_progress_bar(att_new_obj=att_new_obj, text=get_rv_labels("model_training_apply_progress_bar"))

					all_model_params = c(rv_training_models$ols_model
						, rv_training_models$rf_model
						, rv_training_models$gbm_model
						, rv_training_models$xgbTree_model
						, rv_training_models$xgbLinear_model
						, rv_training_models$svmRadial_model
						, rv_training_models$svmLinear_model
						, rv_training_models$svmPoly_model
						, rv_training_models$glmnet_model
					)
					set.seed(rv_ml_ai$seed_value)
					if (isTRUE(input$model_training_setup_start_clusters_check)) {
						Rautoml::start_cluster()
					}
					rv_training_results$models = tryCatch({
						Rautoml::train_caret_models(
							df=rv_ml_ai$preprocessed$train_df
							, model_form=rv_ml_ai$model_formula
							, ctrl=reactiveValuesToList(rv_train_control_caret)
							, model_list=all_model_params
							, metric=input$model_training_setup_eval_metric
						)
					}, error = function(e) {
						shinyalert("Error: ", paste0(get_rv_labels("model_training_error"), "\n", e$message), type = "error")
						if (isTRUE(input$model_training_setup_start_clusters_check)) Rautoml::stop_cluster()
						close_progress_bar(att_new_obj=att_new_obj)
						return(NULL)
					})
					
					if (isTRUE(is.null(rv_training_results$models))) return()
					
					if (isTRUE(input$model_training_setup_start_clusters_check)) {
						Rautoml::stop_cluster()
					}
					if (isTRUE(input$model_training_setup_include_ensemble_check)) {
						rv_training_results$models = tryCatch({
							create_ensemble(
								all.models = rv_training_results$models
								, ctrl=reactiveValuesToList(rv_train_control_caret)
								, metric=input$model_training_setup_eval_metric
							)
						}, error = function(e) {
							shinyalert("Error: ", paste0(get_rv_labels("model_training_error"), "\n", e$message), type = "error")
							close_progress_bar(att_new_obj=att_new_obj)
							return(NULL)
						})
						print(rv_training_results$models)
					}
					
					if (isTRUE(is.null(rv_training_results$models))) return()
					
					rv_training_results$train_metrics_df=tryCatch({
						Rautoml::extract_summary(rv_training_results$models, summary_fun=Rautoml::student_t_summary)
					}, error = function(e) {
						shinyalert("Error: ", paste0(get_rv_labels("model_train_metrics_error"), "\n", e$message), type = "error")
						close_progress_bar(att_new_obj=att_new_obj)
						return(NULL)
					})

					if (is.null(rv_training_results$train_metrics_df)) return()

					## Test metrics
					rv_training_results$test_metrics_objs=tryCatch({
						Rautoml::boot_estimates_multiple(
							models=rv_training_results$models
							, df=rv_ml_ai$preprocessed$test_df
							, outcome_var=rv_ml_ai$outcome
							, problem_type=rv_ml_ai$task
							, nreps=100
							, model_name=NULL
							, type="prob"
							, report= input$model_training_setup_eval_metric
							, summary_fun=Rautoml::student_t_summary
							, save_model = TRUE
							, model_folder = "models"
							, preprocesses = rv_ml_ai$preprocessed
						)
					}, error = function(e) {
						shinyalert("Error: ", paste0(get_rv_labels("model_test_metrics_error"), "\n", e$message), type = "error")
						close_progress_bar(att_new_obj=att_new_obj)
						return(NULL)
					})

					if (is.null(rv_training_results$test_metrics_objs)) return()

					## Generate logs
					Rautoml::create_model_logs(
						df_name=rv_ml_ai$dataset_id
						, session_name=rv_ml_ai$session_id
						, outcome=rv_ml_ai$outcome
						, framework=input$modelling_framework_choices
						, train_result=rv_training_results$test_metrics_objs$all
						, timestamp=Sys.time()
						, path=".log_files"
					)

					## More metrics 
					### Post metrics
					rv_training_results$post_model_metrics_objs=tryCatch({
						Rautoml::post_model_metrics(
							models=rv_training_results$models
							, outcome=rv_ml_ai$outcome
							, df=rv_ml_ai$preprocessed$test_df
							, task=rv_ml_ai$task
						)
					}, error = function(e) {
						shinyalert("Error: ", paste0(get_rv_labels("model_post_metrics_error"), "\n", e$message), type = "error")
						close_progress_bar(att_new_obj=att_new_obj)
						return(NULL)
					})

					if (is.null(rv_training_results$post_model_metrics_objs)) return()


					updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_rf_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_gbm_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_xgbTree_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_xgbLinear_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_svmRadial_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_svmLinear_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_svmPoly_check", value=FALSE)
					updatePrettyCheckbox(session, inputId="model_training_caret_models_glmnet_check", value=FALSE)
					rv_ml_ai$at_least_one_model = FALSE
					rv_training_models$ols_model = NULL
					rv_training_models$rf_model = NULL
					rv_training_models$gbm_model = NULL
					rv_training_models$xgbTree_model = NULL
					rv_training_models$xgbLinear_model = NULL
					rv_training_models$svmRadial_model = NULL
					rv_training_models$svmLinear_model = NULL
					rv_training_models$svmPoly_model = NULL
					rv_training_models$glmnet_model = NULL
					
					rv_train_control_caret$method = "cv"
					rv_train_control_caret$number = 5
					rv_train_control_caret$repeats = NA
					rv_train_control_caret$search = "grid"
					rv_train_control_caret$verboseIter = FALSE
					rv_train_control_caret$savePredictions = FALSE
					rv_train_control_caret$classProbs = TRUE
				
					close_progress_bar(att_new_obj=att_new_obj)
				} else {
					rv_training_results$models = NULL
					rv_training_results$train_metrics_df = NULL
					rv_training_results$test_metrics_objs = NULL
					rv_training_results$post_model_metrics_objs = NULL
				}
			} else {
				rv_training_results$models = NULL
				rv_training_results$train_metrics_df = NULL
				rv_training_results$test_metrics_objs = NULL
				rv_training_results$post_model_metrics_objs = NULL
			}
		} else {
			rv_training_results$models = NULL
			rv_training_results$train_metrics_df = NULL
			rv_training_results$test_metrics_objs = NULL
			rv_training_results$post_model_metrics_objs = NULL
		}
	})	
}

