redis_jobs_loaded <- FALSE
if (file.exists("R/redis_jobs.R")) {
  tryCatch({
    source("R/redis_jobs.R", local=TRUE)
    redis_jobs_loaded <- TRUE
  }, error = function(e) {
    warning("Failed to load redis_jobs.R: ", e$message)
  })
} else if (file.exists("/usr/no-code-app/R/redis_jobs.R")) {
  tryCatch({
    source("/usr/no-code-app/R/redis_jobs.R", local=TRUE)
    redis_jobs_loaded <- TRUE
  }, error = function(e) {
    warning("Failed to load redis_jobs.R: ", e$message)
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

	### RF
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
	

	### lasso
	observeEvent(input$lasso_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_lasso_check)) {
					rv_training_models$lasso_param = TRUE			
				}
			}
		}
	})


	### ridge
	observeEvent(input$ridge_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_ridge_check)) {
					rv_training_models$ridge_param = TRUE			
				}
			}
		}
	})


	### knn
	observeEvent(input$knn_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_knn_check)) {
					rv_training_models$knn_param = TRUE			
				}
			}
		}
	})


	### nnet
	observeEvent(input$nnet_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_nnet_check)) {
					rv_training_models$nnet_param = TRUE			
				}
			}
		}
	})

	### avNNet
	observeEvent(input$avNNet_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_avNNet_check)) {
					rv_training_models$avNNet_param = TRUE			
				}
			}
		}
	})

	### pls
	observeEvent(input$pls_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_pls_check)) {
					rv_training_models$pls_param = TRUE			
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
					if (isTRUE(rv_ml_ai$preprocessed$outcome_nlevels>2)) {
						param_set = list(intercept=as.numeric(input$model_training_caret_models_ols_advance_decay))
					} else {
						param_set=list(intercept=input$model_training_caret_models_ols_advance_intercept)
					}
					rv_training_models$ols_model = Rautoml::setup_caret( 
						rv_training_models$ols_name
						, param=rv_training_models$ols_param
						, param_set=param_set
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$ols_model = NULL
				}

				
				## RF
				if (isTRUE(input$model_training_caret_models_rf_check)) {
					rv_training_models$rf_model = Rautoml::setup_caret( 
						rv_training_models$rf_name
						, param=rv_training_models$rf_param
						, param_set=list(
							mtry=as.numeric(input$model_training_caret_models_rf_advance_params_mtry)
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$rf_model = NULL
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
						rv_training_models$gbm_name
						, param=rv_training_models$gbm_param
						, param_set=list(
							n.trees=as.numeric(input$model_training_caret_models_gbm_advance_n.trees)
							, interaction.depth = as.numeric(input$model_training_caret_models_gbm_advance_interaction.depth)
							, n.minobsinnode = as.numeric(input$model_training_caret_models_gbm_advance_n.minobsinnode)
							, shrinkage = shrinkage 
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$gbm_model = NULL
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
						rv_training_models$xgbTree_name
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
				} else {
					rv_training_models$xgbTree_model = NULL
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
						rv_training_models$xgbLinear_name
						, param=rv_training_models$xgbLinear_param
						, param_set=list(
							nrounds=as.numeric(input$model_training_caret_models_xgbLinear_advance_nrounds)
							, lambda=as.numeric(input$model_training_caret_models_xgbLinear_advance_lambda)
							, alpha=as.numeric(input$model_training_caret_models_xgbLinear_advance_alpha)
							, eta=eta
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$xgbLinear_model = NULL
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
						rv_training_models$svmRadial_name
						, param=rv_training_models$svmRadial_param
						, param_set=list(
							C=as.numeric(input$model_training_caret_models_svmRadial_advance_C)
							, sigma=sigma
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$svmRadial_model = NULL
				}

				## svmLinear
				if (isTRUE(input$model_training_caret_models_svmLinear_check)) {
					rv_training_models$svmLinear_model = Rautoml::setup_caret( 
						rv_training_models$svmLinear_name
						, param=rv_training_models$svmLinear_param
						, param_set=list(
							C=as.numeric(input$model_training_caret_models_svmLinear_advance_C)
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$svmLinear_model = NULL
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
						rv_training_models$svmPoly_name
						, param=rv_training_models$svmPoly_param
						, param_set=list(
							degree=as.numeric(input$model_training_caret_models_svmPoly_advance_degree)
							, C = as.numeric(input$model_training_caret_models_svmPoly_advance_C)
							, scale=scale_
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$svmPoly_model = NULL
				}


				## glmnet
				if (isTRUE(input$model_training_caret_models_glmnet_check)) {
					rv_training_models$glmnet_model = Rautoml::setup_caret( 
						rv_training_models$glmnet_name
						, param=rv_training_models$glmnet_param
						, param_set=list(
							alpha=as.numeric(input$model_training_caret_models_glmnet_advance_alpha)
							, lambda = c(seq(0.001, 0.1, length.out=10), seq(0.1, 2, by =0.1) ,  seq(2, 5, 0.5) , seq(5, 25, 1))
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$glmnet_model = NULL
				}

				
				## lasso
				if (isTRUE(input$model_training_caret_models_lasso_check)) {
					rv_training_models$lasso_param = TRUE
					rv_training_models$lasso_model = Rautoml::setup_caret( 
						rv_training_models$lasso_name
						, param=TRUE#rv_training_models$lasso_param
						, param_set=list(
							alpha=as.numeric(input$model_training_caret_models_lasso_advance_alpha)
							, lambda = c(seq(0.001, 0.1, length.out=10), seq(0.1, 2, by =0.1) ,  seq(2, 5, 0.5) , seq(5, 25, 1))
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$lasso_model = NULL
				}

				
				## ridge
				if (isTRUE(input$model_training_caret_models_ridge_check)) {
					rv_training_models$ridge_param = TRUE
					rv_training_models$ridge_model = Rautoml::setup_caret( 
						rv_training_models$ridge_name
						, param=TRUE#rv_training_models$ridge_param
						, param_set=list(
							alpha=as.numeric(input$model_training_caret_models_ridge_advance_alpha)
							, lambda = c(seq(0.001, 0.1, length.out=10), seq(0.1, 2, by =0.1) ,  seq(2, 5, 0.5) , seq(5, 25, 1))
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$ridge_model = NULL
				}

				
				## knn
				if (isTRUE(input$model_training_caret_models_knn_check)) {
					rv_training_models$knn_model = Rautoml::setup_caret( 
						rv_training_models$knn_name
						, param=rv_training_models$knn_param
						, param_set=list(
							k=as.numeric(input$model_training_caret_models_knn_advance_k)
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$knn_model = NULL
				}

				
				## nnet
				if (isTRUE(input$model_training_caret_models_nnet_check)) {
					rv_training_models$nnet_model = Rautoml::setup_caret( 
						rv_training_models$nnet_name
						, param=rv_training_models$nnet_param
						, param_set=list(
							size=as.numeric(input$model_training_caret_models_nnet_advance_size)
							, decay=as.numeric(input$model_training_caret_models_nnet_advance_decay)
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$nnet_model = NULL
				}

				
				## treebag
				if (isTRUE(input$model_training_caret_models_treebag_check)) {
					rv_training_models$treebag_trained_model = rv_training_models$treebag_name
					rv_training_models$treebag_model = Rautoml::setup_caret( 
						rv_training_models$treebag_name
						, param=FALSE
						, param_set=NULL
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$treebag_model = NULL
				}

				## avNNet
				if (isTRUE(input$model_training_caret_models_avNNet_check)) {
					rv_training_models$avNNet_model = Rautoml::setup_caret( 
						rv_training_models$avNNet_name
						, param=rv_training_models$avNNet_param
						, param_set=list(
							bag = input$model_training_caret_models_avNNet_advance_bag
							, size=as.numeric(input$model_training_caret_models_avNNet_advance_size)
							, decay=as.numeric(input$model_training_caret_models_avNNet_advance_decay)
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$avNNet_model = NULL
				}

				## pls
				if (isTRUE(input$model_training_caret_models_pls_check)) {
					rv_training_models$pls_model = Rautoml::setup_caret( 
						rv_training_models$pls_name
						, param=rv_training_models$pls_param
						, param_set=list(
							ncomp=as.numeric(input$model_training_caret_models_pls_advance_ncomp)
						)
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$pls_model = NULL
				}

				
				## gam
				if (isTRUE(input$model_training_caret_models_gam_check)) {
					rv_training_models$gam_trained_model = rv_training_models$gam_name
					rv_training_models$gam_model = Rautoml::setup_caret( 
						rv_training_models$gam_name
						, param=FALSE
						, param_set=NULL
					)
					rv_ml_ai$at_least_one_model = TRUE
				} else {
					rv_training_models$gam_model = NULL
				}


				## Update this for every model
				if (!isTRUE(input$model_training_caret_models_ols_check) & !isTRUE(input$model_training_caret_models_rf_check) & !isTRUE(input$model_training_caret_models_gbm_check) & !isTRUE(input$model_training_caret_models_xgbTree_check) & !isTRUE(input$model_training_caret_models_xgbLinear_check) & !isTRUE(input$model_training_caret_models_svmRadial_check) & !isTRUE(input$model_training_caret_models_svmLinear_check) & !isTRUE(input$model_training_caret_models_svmPoly_check) & !isTRUE(input$model_training_caret_models_glmnet_check) & !isTRUE(input$model_training_caret_models_lasso_check) & !isTRUE(input$model_training_caret_models_ridge_check) & !isTRUE(input$model_training_caret_models_knn_check) & !isTRUE(input$model_training_caret_models_nnet_check) & !isTRUE(input$model_training_caret_models_treebag_check) & !isTRUE(input$model_training_caret_models_avNNet_check) & !isTRUE(input$model_training_caret_models_pls_check) & !isTRUE(input$model_training_caret_models_gam_check) ) {
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
	observe({
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
								, column(width=4
									, uiOutput("model_training_caret_models_lasso_check")
									, uiOutput("model_training_caret_models_lasso_advance_params")
								)
								, column(width=4
									, uiOutput("model_training_caret_models_ridge_check")
									, uiOutput("model_training_caret_models_ridge_advance_params")
								)
								, column(width=4
									, uiOutput("model_training_caret_models_knn_check")
									, uiOutput("model_training_caret_models_knn_advance_params")
								)
								, column(width=4
									, uiOutput("model_training_caret_models_nnet_check")
									, uiOutput("model_training_caret_models_nnet_advance_params")
								)
								, column(width=4
									, uiOutput("model_training_caret_models_treebag_check")
								)
								, column(width=4
									, uiOutput("model_training_caret_models_avNNet_check")
									, uiOutput("model_training_caret_models_avNNet_advance_params")
								)
								, column(width=4
									, uiOutput("model_training_caret_models_pls_check")
									, uiOutput("model_training_caret_models_pls_advance_params")
								)
								, column(width=4
									, uiOutput("model_training_caret_models_gam_check")
								)
							 )
							 , uiOutput("model_training_apply")
					  )
					)
				}
			}
		})
	})

	## Train models
	observeEvent(input$model_training_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(rv_ml_ai$at_least_one_model)) {

					start_progress_bar(id="model_training_caret_pb", att_new_obj=model_training_caret_pb, text=get_rv_labels("model_training_apply_progress_bar"))
 
					all_model_params = c(rv_training_models$ols_model
						, rv_training_models$rf_model
						, rv_training_models$gbm_model
						, rv_training_models$xgbTree_model
						, rv_training_models$xgbLinear_model
						, rv_training_models$svmRadial_model
						, rv_training_models$svmLinear_model
						, rv_training_models$svmPoly_model
						, rv_training_models$glmnet_model
						, rv_training_models$lasso_model
						, rv_training_models$ridge_model
						, rv_training_models$knn_model
						, rv_training_models$nnet_model
						, rv_training_models$treebag_model
						, rv_training_models$avNNet_model
						, rv_training_models$pls_model
						, rv_training_models$gam_model
					)

					train_df <- isolate(rv_ml_ai$preprocessed$train_df)
					test_df <- isolate(rv_ml_ai$preprocessed$test_df)
					preprocessed_data <- isolate(rv_ml_ai$preprocessed)
					model_form <- isolate(rv_ml_ai$model_formula)
					ctrl_list <- isolate(reactiveValuesToList(rv_train_control_caret))
					metric_val <- isolate(input$model_training_setup_eval_metric)
					seed_val <- isolate(rv_ml_ai$seed_value)
					use_cluster <- isTRUE(isolate(input$model_training_setup_start_clusters_check))
					include_ensemble <- isTRUE(isolate(input$model_training_setup_include_ensemble_check))
					dataset_id_val <- isolate(rv_ml_ai$dataset_id)
					session_id_val <- isolate(rv_ml_ai$session_id)
					outcome_val <- isolate(rv_ml_ai$outcome)
					task_val <- isolate(rv_ml_ai$task)
					app_username_val <- isolate(app_username)
					framework_val <- isolate(input$modelling_framework_choices)

					use_async <- redis_jobs_loaded && exists("create_job_id") && exists("store_job_status")
					
					if (use_async) {
						job_id <- create_job_id()
						rv_ml_ai$caret_job_id <- job_id
						rv_training_results$training_completed <- FALSE
						if (exists("store_job_status")) {
							store_job_status(job_id, "queued", 0, "Training job queued")
							store_job_status(job_id, "running", 5, "Starting training")
						}
					} else {
						job_id <- NULL
					}

					safe_store_status <- function(job_id, status, progress, message, error = NULL) {
						if (!is.null(job_id) && use_async && exists("store_job_status")) {
							store_job_status(job_id, status, progress, message, error)
						}
					}

					training_future <- future::future({
						set.seed(seed_val)
						
						on.exit({
							gc(verbose = FALSE)
						}, add = TRUE)
						
						if (use_cluster) {
							Rautoml::start_cluster()
							on.exit(Rautoml::stop_cluster(), add = TRUE)
						}

						safe_store_status(job_id, "running", 10, "Training models")

						models_result <- tryCatch({
							Rautoml::train_caret_models(
								df = train_df
								, model_form = model_form
								, ctrl = ctrl_list
								, model_list = all_model_params
								, metric = metric_val
							)
						}, error = function(e) {
							safe_store_status(job_id, "failed", 0, "Training failed", error = e$message)
							return(NULL)
						})

						if (is.null(models_result)) {
							return(NULL)
						}

						safe_store_status(job_id, "running", 40, "Creating ensemble if needed")

						if (include_ensemble) {
							models_result <- tryCatch({
								Rautoml::create_ensemble(
									all.models = models_result
									, ctrl = ctrl_list
									, metric = metric_val
								)
							}, error = function(e) {
								safe_store_status(job_id, "running", 45, "Ensemble creation failed, continuing")
								return(models_result)
							})
						}

						safe_store_status(job_id, "running", 50, "Extracting tuned parameters")

						tuned_params <- tryCatch({
							Rautoml::get_tuned_params(models_result)
						}, error = function(e) {
							return(NULL)
						})

						safe_store_status(job_id, "running", 55, "Extracting control parameters")

						control_params <- tryCatch({
							Rautoml::get_ctl_params(
								models = models_result
								, items = names(ctrl_list)
							)
						}, error = function(e) {
							return(NULL)
						})

						safe_store_status(job_id, "running", 60, "Calculating training metrics")

						train_metrics_df <- tryCatch({
							Rautoml::extract_summary(models_result, summary_fun = Rautoml::student_t_summary)
						}, error = function(e) {
							return(NULL)
						})

						safe_store_status(job_id, "running", 65, "Saving training metrics")

						if (!is.null(train_metrics_df)) {
							tryCatch({
								Rautoml::save_rautoml_csv(
									object = train_metrics_df
									, name = "training_performance_metrics"
									, dataset_id = dataset_id_val
									, session_name = session_id_val
									, timestamp = Sys.time()
									, output_dir = paste0(app_username_val, "/outputs")
								)
							}, error = function(e) NULL)
						}

						safe_store_status(job_id, "running", 70, "Calculating test metrics")

						test_metrics_objs <- tryCatch({
							Rautoml::boot_estimates_multiple(
								models = models_result
								, df = test_df
								, outcome_var = outcome_val
								, problem_type = task_val
								, nreps = 100
								, model_name = NULL
								, type = "prob"
								, report = metric_val
								, summary_fun = Rautoml::student_t_summary
								, save_model = TRUE
								, model_folder = paste0(app_username_val, "/models")
								, recipe_folder = paste0(app_username_val, "/recipes")
								, preprocesses = preprocessed_data
							)
						}, error = function(e) {
							return(NULL)
						})

						safe_store_status(job_id, "running", 80, "Saving test metrics")

						if (!is.null(test_metrics_objs)) {
							tryCatch({
								Rautoml::save_boot_estimates(
									boot_list = test_metrics_objs
									, dataset_id = dataset_id_val
									, session_name = session_id_val
									, timestamp = Sys.time()
									, output_dir = paste0(app_username_val, "/outputs")
									, sub_dir = "test_metrics"
								)
							}, error = function(e) NULL)
						}

						safe_store_status(job_id, "running", 85, "Generating logs")

						tryCatch({
							Rautoml::create_model_logs(
								df_name = dataset_id_val
								, session_name = session_id_val
								, outcome = outcome_val
								, framework = framework_val
								, train_result = test_metrics_objs$all
								, timestamp = Sys.time()
								, path = paste0(app_username_val, "/.log_files")
							)
						}, error = function(e) NULL)

						safe_store_status(job_id, "running", 90, "Calculating post-model metrics")

						post_model_metrics_objs <- tryCatch({
							Rautoml::post_model_metrics(
								models = models_result
								, outcome = outcome_val
								, df = test_df
								, task = task_val
							)
						}, error = function(e) {
							return(NULL)
						})

						safe_store_status(job_id, "running", 95, "Saving post-model metrics")

						if (!is.null(post_model_metrics_objs)) {
							tryCatch({
								Rautoml::save_post_metrics_plots(
									metric_list = post_model_metrics_objs
									, dataset_id = dataset_id_val
									, session_name = session_id_val
									, timestamp = Sys.time()
									, output_dir = paste0(app_username_val, "/outputs")
								)
							}, error = function(e) NULL)
						}

						result <- list(
							models = models_result
							, tuned_parameters = tuned_params
							, control_parameters = control_params
							, train_metrics_df = train_metrics_df
							, test_metrics_objs = test_metrics_objs
							, post_model_metrics_objs = post_model_metrics_objs
						)

						store_job_result(job_id, result)
						safe_store_status(job_id, "completed", 100, "Training completed successfully")

						result
					})

					poll_timer <- reactiveTimer(2000)
					poll_count <- reactiveVal(0)
					max_polls <- 1800

					poll_status <- observe({
						poll_timer()
						
						current_count <- poll_count()
						if (current_count >= max_polls) {
							safe_store_status(job_id, "failed", 0, "Training timeout")
							close_progress_bar(att_new_obj = model_training_caret_pb)
							poll_status$destroy()
							return()
						}

						poll_count(current_count + 1)

						status_data <- get_job_status(job_id)

						if (status_data$status == "completed") {
							result <- get_job_result(job_id)
							if (!is.null(result)) {
								rv_training_results$models <- result$models
								rv_training_results$tuned_parameters <- result$tuned_parameters
								rv_training_results$control_parameters <- result$control_parameters
								rv_training_results$train_metrics_df <- result$train_metrics_df
								rv_training_results$test_metrics_objs <- result$test_metrics_objs
								rv_training_results$post_model_metrics_objs <- result$post_model_metrics_objs
								rv_training_results$training_completed <- TRUE
								rv_ml_ai$at_least_one_model <- FALSE
							}
							close_progress_bar(att_new_obj = model_training_caret_pb)
							poll_status$destroy()
							return()
						}

						if (status_data$status == "failed") {
							error_msg <- status_data$error %||% "Unknown error"
							shinyalert::shinyalert("Error: ", paste0(get_rv_labels("model_training_error"), "\n", error_msg), type = "error")
							close_progress_bar(att_new_obj = model_training_caret_pb)
							poll_status$destroy()
							return()
						}
					})
						
					
					if (isTRUE(input$model_training_setup_start_clusters_check)) {
						Rautoml::stop_cluster()
					}
					
					
					if (isTRUE(is.null(rv_training_results$models))) return()
					
					if (isTRUE(input$model_training_setup_include_ensemble_check)) {
						rv_training_results$models = tryCatch({
							Rautoml::create_ensemble(
								all.models = rv_training_results$models
								, ctrl=reactiveValuesToList(rv_train_control_caret)
								, metric=input$model_training_setup_eval_metric
							)
						}, error = function(e) {
							shinyalert::shinyalert("Error: ", paste0(get_rv_labels("model_training_error"), "\n", e$message), type = "error")
							close_progress_bar(att_new_obj=model_training_caret_pb)
							return(NULL)
						})
					}

					rv_training_results$training_completed = TRUE
					rv_ml_ai$at_least_one_model = FALSE
				
## 					rv_training_models$ols_model = NULL
## 					rv_training_models$rf_model = NULL
## 					rv_training_models$gbm_model = NULL
## 					rv_training_models$xgbTree_model = NULL
## 					rv_training_models$xgbLinear_model = NULL
## 					rv_training_models$svmRadial_model = NULL
## 					rv_training_models$svmLinear_model = NULL
## 					rv_training_models$svmPoly_model = NULL
## 					rv_training_models$glmnet_model = NULL
## 					rv_training_models$lasso_model = NULL
## 					rv_training_models$ridge_model = NULL
## 					rv_training_models$knn_model = NULL
## 					rv_training_models$nnet_model = NULL
## 
## 					rv_train_control_caret$method = "cv"
## 					rv_train_control_caret$number = 5
## 					rv_train_control_caret$repeats = NA
## 					rv_train_control_caret$search = "grid"
## 					rv_train_control_caret$verboseIter = FALSE
## 					rv_train_control_caret$savePredictions = FALSE
## 					rv_train_control_caret$classProbs = TRUE
## 				
					
					if (isTRUE(is.null(rv_training_results$models))) return()
					
					rv_training_results$tuned_parameters = tryCatch({
						Rautoml::get_tuned_params(rv_training_results$models)
					}, error=function(e) {
						shinyalert::shinyalert("Error: ", paste0(get_rv_labels("model_train_metrics_error"), "\n", e$message), type = "error")
						close_progress_bar(att_new_obj=model_training_caret_pb)
						return(NULL)
					})
					
					if (is.null(rv_training_results$tuned_parameters)) return()

					rv_training_results$control_parameters = tryCatch({
						Rautoml::get_ctl_params(models=rv_training_results$models
							, items=names(reactiveValuesToList(rv_train_control_caret))
						)
					}, error=function(e) {
						shinyalert::shinyalert("Error: ", paste0(get_rv_labels("model_train_metrics_error"), "\n", e$message), type = "error")
						close_progress_bar(att_new_obj=model_training_caret_pb)
						return(NULL)
					})
					
					if (is.null(rv_training_results$control_parameters)) return()
				
					rv_training_results$train_metrics_df=tryCatch({
						Rautoml::extract_summary(rv_training_results$models, summary_fun=Rautoml::student_t_summary)
					}, error = function(e) {
						shinyalert::shinyalert("Error: ", paste0(get_rv_labels("model_train_metrics_error"), "\n", e$message), type = "error")
						close_progress_bar(att_new_obj=model_training_caret_pb)
						return(NULL)
					})
					

					if (is.null(rv_training_results$train_metrics_df)) return()

					## Save training performance metrics locally
					
					save_training = tryCatch({
						Rautoml::save_rautoml_csv(object=rv_training_results$train_metrics_df
							, name="training_performance_metrics"
							, dataset_id=rv_ml_ai$dataset_id
							, session_name=rv_ml_ai$session_id
							, timestamp=Sys.time()
							, output_dir=paste0(app_username, "/outputs")
						)
						invisible(TRUE)
					}, error=function(e) {
						shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
						return(NULL)
					})

					if (is.null(save_training)) {
						close_progress_bar(att_new_obj=model_training_caret_pb)	
						return()
					}
					
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
							, model_folder = paste0(app_username, "/models")
							, recipe_folder = paste0(app_username, "/recipes")
							, preprocesses = rv_ml_ai$preprocessed
						)
					}, error = function(e) {
						shinyalert::shinyalert("Error: ", paste0(get_rv_labels("model_test_metrics_error"), "\n", e$message), type = "error")
						close_progress_bar(att_new_obj=model_training_caret_pb)
						return(NULL)
					})

					if (is.null(rv_training_results$test_metrics_objs)) return()
					
					## Save test performance metrics
					
					save_test = tryCatch({
						Rautoml::save_boot_estimates(boot_list=rv_training_results$test_metrics_objs
							, dataset_id=rv_ml_ai$dataset_id
							, session_name=rv_ml_ai$session_id
							, timestamp=Sys.time()
							, output_dir=paste0(app_username, "/outputs")
							, sub_dir="test_metrics"
						)
						invisible(TRUE)
					}, error=function(e) {
						shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
						return(NULL)
					})

					if (is.null(save_test)) {
						close_progress_bar(att_new_obj=model_training_caret_pb)
						return()
					}
					
					## Generate logs
					save_logs = tryCatch({
						Rautoml::create_model_logs(
							df_name=rv_ml_ai$dataset_id
							, session_name=rv_ml_ai$session_id
							, outcome=rv_ml_ai$outcome
							, framework=input$modelling_framework_choices
							, train_result=rv_training_results$test_metrics_objs$all
							, timestamp=Sys.time()
							, path=paste0(app_username, "/.log_files")
						)
						invisible(TRUE)
					}, error=function(e) {
						shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
						return(NULL)
					})

					if (is.null(save_logs)) {
						close_progress_bar(att_new_obj=model_training_caret_pb)
						return()
					}


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
						shinyalert::shinyalert("Error: ", paste0(get_rv_labels("model_post_metrics_error"), "\n", e$message), type = "error")
						close_progress_bar(att_new_obj=model_training_caret_pb)
						return(NULL)
					})

					if (is.null(rv_training_results$post_model_metrics_objs)) return()
					
					## Save post model objects
					save_post = tryCatch({
						Rautoml::save_post_metrics_plots(metric_list=rv_training_results$post_model_metrics_objs
							, dataset_id=rv_ml_ai$dataset_id
							, session_name=rv_ml_ai$session_id
							, timestamp=Sys.time()
							, output_dir=paste0(app_username, "/outputs")
						)
						invisible(TRUE)
					}, error=function(e) {
						shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
						return(NULL)
					})

					if (is.null(save_post)) {
						close_progress_bar(att_new_obj=model_training_caret_pb)
						return()
					}

					close_progress_bar(att_new_obj=model_training_caret_pb)
				} else {
					rv_training_results$models = NULL
					rv_training_results$train_metrics_df = NULL
					rv_training_results$test_metrics_objs = NULL
					rv_training_results$post_model_metrics_objs = NULL
					rv_training_results$tuned_parameters = NULL
					rv_training_results$control_parameters = NULL

					close_progress_bar(att_new_obj=model_training_caret_pb)
				}
			} else {
				rv_training_results$models = NULL
				rv_training_results$train_metrics_df = NULL
				rv_training_results$test_metrics_objs = NULL
				rv_training_results$post_model_metrics_objs = NULL
				rv_training_results$tuned_parameters = NULL
				rv_training_results$control_parameters = NULL
				close_progress_bar(att_new_obj=model_training_caret_pb)
			}
		} else {
			rv_training_results$models = NULL
			rv_training_results$train_metrics_df = NULL
			rv_training_results$test_metrics_objs = NULL
			rv_training_results$post_model_metrics_objs = NULL
			rv_training_results$tuned_parameters = NULL
			rv_training_results$control_parameters = NULL
			close_progress_bar(att_new_obj=model_training_caret_pb)
		}
	})	

## 	observe({
## 		req(!isTRUE(rv_training_results$training_completed), isTRUE(!is.null(rv_training_results$training_completed)))
## 		req(isTRUE(!is.null(rv_current$working_df)))
## 		req(isTRUE(!is.null(rv_ml_ai$preprocessed)))
## 		req(!isTRUE(rv_training_results$training_completed))
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_ols_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_rf_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_gbm_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_xgbTree_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_xgbLinear_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_svmRadial_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_svmLinear_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_svmPoly_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_glmnet_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_lasso_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_ridge_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_knn_check", value=FALSE)
## 		updatePrettyCheckbox(session, inputId="model_training_caret_models_nnet_check", value=FALSE)
## 		rv_ml_ai$at_least_one_model = FALSE
## 		rv_training_models$ols_model = NULL
## 		rv_training_models$rf_model = NULL
## 		rv_training_models$gbm_model = NULL
## 		rv_training_models$xgbTree_model = NULL
## 		rv_training_models$xgbLinear_model = NULL
## 		rv_training_models$svmRadial_model = NULL
## 		rv_training_models$svmLinear_model = NULL
## 		rv_training_models$svmPoly_model = NULL
## 		rv_training_models$glmnet_model = NULL
## 		rv_training_models$lasso_model = NULL
## 		rv_training_models$ridge_model = NULL
## 		rv_training_models$knn_model = NULL
## 		rv_training_models$nnet_model = NULL
## 					
## 	})
}

