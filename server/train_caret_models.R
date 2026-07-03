source("server/caret_job_manager.R", local = TRUE)

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

	
	### rpart
	observeEvent(input$rpart_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_rpart_check)) {
					rv_training_models$rpart_param = TRUE			
				}
			}
		}
	})

	### mlpWeightDecayML
	observeEvent(input$mlpWeightDecayML_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_mlpWeightDecayML_check)) {
					rv_training_models$mlpWeightDecayML_param = TRUE			
				}
			}
		}
	})
	
	### naive_bayes
	observeEvent(input$naive_bayes_advance_control_apply_save, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(input$model_training_caret_models_naive_bayes_check)) {
					rv_training_models$naive_bayes_param = TRUE			
				}
			}
		}
	})

	## Build model specs — fires on preprocessing complete OR any advance option save.
	## Does NOT depend on checkbox inputs so checkbox ticks are instant (fast observer).
	observeEvent(
		list(
			rv_ml_ai$preprocessed,
			input$ols_advance_control_apply_save,
			input$rf_advance_control_apply_save,
			input$gbm_advance_control_apply_save,
			input$xgbTree_advance_control_apply_save,
			input$xgbLinear_advance_control_apply_save,
			input$svmRadial_advance_control_apply_save,
			input$svmLinear_advance_control_apply_save,
			input$svmPoly_advance_control_apply_save,
			input$glmnet_advance_control_apply_save,
			input$lasso_advance_control_apply_save,
			input$ridge_advance_control_apply_save,
			input$knn_advance_control_apply_save,
			input$nnet_advance_control_apply_save,
			input$avNNet_advance_control_apply_save,
			input$pls_advance_control_apply_save,
			input$rpart_advance_control_apply_save,
			input$mlpWeightDecayML_advance_control_apply_save,
			input$naive_bayes_advance_control_apply_save
		),
		{
		req(!is.null(rv_current$working_df))
		req(!is.null(rv_ml_ai$preprocessed))
		req(!is.null(rv_training_models$ols_name))
		{
				
				## LM/GLM
				if (!is.null(rv_training_models$ols_name)) {
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
				}

				
				## RF
				if (!is.null(rv_training_models$rf_name)) {
					rv_training_models$rf_model = Rautoml::setup_caret(
						rv_training_models$rf_name
						, param=rv_training_models$rf_param
						, param_set=list(mtry=as.numeric(input$model_training_caret_models_rf_advance_params_mtry))
					)
				}

				## GBM
				if (!is.null(rv_training_models$gbm_name)) {
					shrinkage = input$model_training_caret_models_gbm_advance_shrinkage
					if (!is.null(shrinkage)) shrinkage = seq(shrinkage[1], shrinkage[2], length.out=20)
					rv_training_models$gbm_model = Rautoml::setup_caret(
						rv_training_models$gbm_name
						, param=rv_training_models$gbm_param
						, param_set=list(
							n.trees=as.numeric(input$model_training_caret_models_gbm_advance_n.trees)
							, interaction.depth=as.numeric(input$model_training_caret_models_gbm_advance_interaction.depth)
							, n.minobsinnode=as.numeric(input$model_training_caret_models_gbm_advance_n.minobsinnode)
							, shrinkage=shrinkage
						)
					)
				}

				## xgbTree
				if (!is.null(rv_training_models$xgbTree_name)) {
					eta = input$model_training_caret_models_xgbTree_advance_eta
					if (!is.null(eta)) eta = seq(eta[1], eta[2], length.out=20)
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
				}

				## xgbLinear
				if (!is.null(rv_training_models$xgbLinear_name)) {
					eta = input$model_training_caret_models_xgbLinear_advance_eta
					if (!is.null(eta)) eta = seq(eta[1], eta[2], length.out=20)
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
				}

				## svmRadial
				if (!is.null(rv_training_models$svmRadial_name)) {
					sigma = input$model_training_caret_models_svmRadial_advance_sigma
					if (!is.null(sigma)) sigma = seq(sigma[1], sigma[2], length.out=20)
					rv_training_models$svmRadial_model = Rautoml::setup_caret(
						rv_training_models$svmRadial_name
						, param=rv_training_models$svmRadial_param
						, param_set=list(C=as.numeric(input$model_training_caret_models_svmRadial_advance_C), sigma=sigma)
					)
				}

				## svmLinear
				if (!is.null(rv_training_models$svmLinear_name)) {
					rv_training_models$svmLinear_model = Rautoml::setup_caret(
						rv_training_models$svmLinear_name
						, param=rv_training_models$svmLinear_param
						, param_set=list(C=as.numeric(input$model_training_caret_models_svmLinear_advance_C))
					)
				}

				## svmPoly
				if (!is.null(rv_training_models$svmPoly_name)) {
					scale_ = input$model_training_caret_models_svmPoly_advance_scale
					if (!is.null(scale_)) scale_ = seq(scale_[1], scale_[2], length.out=20)
					rv_training_models$svmPoly_model = Rautoml::setup_caret(
						rv_training_models$svmPoly_name
						, param=rv_training_models$svmPoly_param
						, param_set=list(
							degree=as.numeric(input$model_training_caret_models_svmPoly_advance_degree)
							, C=as.numeric(input$model_training_caret_models_svmPoly_advance_C)
							, scale=scale_
						)
					)
				}

				## glmnet
				if (!is.null(rv_training_models$glmnet_name)) {
					rv_training_models$glmnet_model = Rautoml::setup_caret(
						rv_training_models$glmnet_name
						, param=rv_training_models$glmnet_param
						, param_set=list(
							alpha=as.numeric(input$model_training_caret_models_glmnet_advance_alpha)
							, lambda=c(seq(0.001,0.1,length.out=10),seq(0.1,2,by=0.1),seq(2,5,0.5),seq(5,25,1))
						)
					)
				}

				## lasso
				if (!is.null(rv_training_models$lasso_name)) {
					rv_training_models$lasso_param = TRUE
					rv_training_models$lasso_model = Rautoml::setup_caret(
						rv_training_models$lasso_name
						, param=TRUE
						, param_set=list(
							alpha=as.numeric(input$model_training_caret_models_lasso_advance_alpha)
							, lambda=c(seq(0.001,0.1,length.out=10),seq(0.1,2,by=0.1),seq(2,5,0.5),seq(5,25,1))
						)
					)
				}

				## ridge
				if (!is.null(rv_training_models$ridge_name)) {
					rv_training_models$ridge_param = TRUE
					rv_training_models$ridge_model = Rautoml::setup_caret(
						rv_training_models$ridge_name
						, param=TRUE
						, param_set=list(
							alpha=as.numeric(input$model_training_caret_models_ridge_advance_alpha)
							, lambda=c(seq(0.001,0.1,length.out=10),seq(0.1,2,by=0.1),seq(2,5,0.5),seq(5,25,1))
						)
					)
				}

				## knn
				if (!is.null(rv_training_models$knn_name)) {
					rv_training_models$knn_model = Rautoml::setup_caret(
						rv_training_models$knn_name
						, param=rv_training_models$knn_param
						, param_set=list(k=as.numeric(input$model_training_caret_models_knn_advance_k))
					)
				}

				## nnet
				if (!is.null(rv_training_models$nnet_name)) {
					rv_training_models$nnet_model = Rautoml::setup_caret(
						rv_training_models$nnet_name
						, param=rv_training_models$nnet_param
						, param_set=list(
							size=as.numeric(input$model_training_caret_models_nnet_advance_size)
							, decay=as.numeric(input$model_training_caret_models_nnet_advance_decay)
						)
					)
				}

				## treebag
				if (!is.null(rv_training_models$treebag_name)) {
					rv_training_models$treebag_trained_model = rv_training_models$treebag_name
					rv_training_models$treebag_model = Rautoml::setup_caret(rv_training_models$treebag_name, param=FALSE, param_set=NULL)
				}

				## avNNet
				if (!is.null(rv_training_models$avNNet_name)) {
					rv_training_models$avNNet_model = Rautoml::setup_caret(
						rv_training_models$avNNet_name
						, param=rv_training_models$avNNet_param
						, param_set=list(
							bag=input$model_training_caret_models_avNNet_advance_bag
							, size=as.numeric(input$model_training_caret_models_avNNet_advance_size)
							, decay=as.numeric(input$model_training_caret_models_avNNet_advance_decay)
						)
					)
				}

				## pls
				if (!is.null(rv_training_models$pls_name)) {
					rv_training_models$pls_model = Rautoml::setup_caret(
						rv_training_models$pls_name
						, param=rv_training_models$pls_param
						, param_set=list(ncomp=as.numeric(input$model_training_caret_models_pls_advance_ncomp))
					)
				}

				## gam
				if (!is.null(rv_training_models$gam_name)) {
					rv_training_models$gam_trained_model = rv_training_models$gam_name
					rv_training_models$gam_model = Rautoml::setup_caret(rv_training_models$gam_name, param=FALSE, param_set=NULL)
				}

				## rpart
				if (!is.null(rv_training_models$rpart_name)) {
					cp_ = input$model_training_caret_models_rpart_advance_cp
					if (!is.null(cp_)) cp_ = runif(cp_, n=20)
					rv_training_models$rpart_model = Rautoml::setup_caret(
						rv_training_models$rpart_name
						, param=rv_training_models$rpart_param
						, param_set=list(cp=cp_)
					)
				}

				## mlpWeightDecayML
				if (!is.null(rv_training_models$mlpWeightDecayML_name)) {
					layer1_ = input$model_training_caret_models_mlpWeightDecayML_advance_layer1
					if (!is.null(layer1_)) layer1_ = floor(runif(layer1_, n=20))
					layer2_ = input$model_training_caret_models_mlpWeightDecayML_advance_layer2
					if (!is.null(layer2_)) layer2_ = floor(runif(layer2_, n=20))
					layer3_ = input$model_training_caret_models_mlpWeightDecayML_advance_layer3
					if (!is.null(layer3_)) layer3_ = floor(runif(layer3_, n=20))
					decay_ = input$model_training_caret_models_mlpWeightDecayML_advance_decay
					if (!is.null(decay_)) decay_ = runif(decay_, n=20)
					rv_training_models$mlpWeightDecayML_model = Rautoml::setup_caret(
						rv_training_models$mlpWeightDecayML_name
						, param=rv_training_models$mlpWeightDecayML_param
						, param_set=list(layer1=layer1_, layer2=layer2_, layer3=layer3_, decay=decay_)
					)
				}
				
				
				## naive_bayes
				if (!is.null(rv_training_models$naive_bayes_name)) {
					laplace_ = input$model_training_caret_models_naive_bayes_advance_laplace
					if (!is.null(laplace_)) {
						laplace_ = runif(laplace_, n=20)
					}
					adjust_ = input$model_training_caret_models_naive_bayes_advance_adjust
					if (!is.null(adjust_)) {
						adjust_ = runif(adjust_, n=20)
					}
					usekernel_ = input$model_training_caret_models_naive_bayes_advance_usekernel
					rv_training_models$naive_bayes_model = Rautoml::setup_caret(
						rv_training_models$naive_bayes_name
						, param=rv_training_models$naive_bayes_param
						, param_set=list(laplace=laplace_, adjust=adjust_, usekernel=usekernel_)
					)
				}

				## Track models for PB
				rv_training_models$CARET_MODEL_IDS = gsub("_model$", "", grep("(?<!trained)_model$", names(rv_training_models), value = TRUE, perl = TRUE))
		}
	},
	ignoreInit = TRUE,
	ignoreNULL = FALSE
	)

	## Register placeholders so outputOptions can be called before the real renderUI
	output$model_training_caret_models_ui <- renderUI({ NULL })
	output$model_training_apply           <- renderUI({ NULL })
	outputOptions(output, "model_training_caret_models_ui", suspendWhenHidden = FALSE)
	outputOptions(output, "model_training_apply",           suspendWhenHidden = FALSE)

	## After feature engineering fires, pre-warm all model checkbox/advance outputs
	## so navigating to Train model does not trigger 40+ sequential round-trips.
	observeEvent(input$feature_engineering_apply, {
		req(!is.null(rv_ml_ai$preprocessed))
		later::later(function() {
			# Model IDs — match the keys used in model_training_caret_models.R
			all_ids     <- c("ols","rf","gbm","xgbTree","xgbLinear","svmRadial",
			                 "svmLinear","svmPoly","glmnet","lasso","ridge","knn",
			                 "nnet","treebag","avNNet","pls","gam","rpart",
			                 "mlpWeightDecayML","naive_bayes")
			no_advance  <- c("treebag", "gam")  # these have no advance options panel
			ols_special <- "ols"                 # ols uses a different suffix

			check_ids   <- paste0("model_training_caret_models_", all_ids, "_check")
			adv_std     <- paste0("model_training_caret_models_",
			                      setdiff(all_ids, c(no_advance, ols_special)),
			                      "_advance_params")
			adv_ols     <- "model_training_caret_models_ols_advance_intercept"

			for (id in c(check_ids, adv_std, adv_ols)) {
				tryCatch(
					outputOptions(output, id, suspendWhenHidden = FALSE),
					error = function(e) NULL
				)
			}
		}, delay = 0.25)
	}, ignoreInit = TRUE)

	observe({
		req(!is.null(rv_ml_ai$preprocessed))
		all_ids <- c("ols","rf","gbm","xgbTree","xgbLinear","svmRadial",
		             "svmLinear","svmPoly","glmnet","lasso","ridge","knn",
		             "nnet","treebag","avNNet","pls","gam","rpart",
		             "mlpWeightDecayML","naive_bayes")
		any_checked <- any(vapply(all_ids, function(id) {
			isTRUE(input[[paste0("model_training_caret_models_", id, "_check")]])
		}, logical(1)))
		rv_ml_ai$at_least_one_model <- any_checked
	})

   ##	Train model action
	output$model_training_apply = renderUI({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$preprocessed))) {
				if (isTRUE(rv_ml_ai$at_least_one_model)) {
					button_label <- if (isTRUE(rv_training_results$training_busy) || isTRUE(rv_training_results$training_completed)) {
						add_label <- tryCatch(as.character(get_rv_labels("caret_jobs_add_selected")), error = function(e) NULL)
						if (is.null(add_label) || !length(add_label) || is.na(add_label[[1]]) || !nzchar(add_label[[1]])) {
							"Add selected models to queue"
						} else {
							add_label[[1]]
						}
					} else {
						get_rv_labels("model_training_apply")
					}
					p(br()
						, actionBttn("model_training_apply"
							, inline=TRUE
							, block = FALSE
							, color = "success"
							, label = button_label
						)
					)
				}
			}
		}
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
								, column(width=4
									, uiOutput("model_training_caret_models_rpart_check")
									, uiOutput("model_training_caret_models_rpart_advance_params")
								)
								, column(width=4
									, uiOutput("model_training_caret_models_mlpWeightDecayML_check")
									, uiOutput("model_training_caret_models_mlpWeightDecayML_advance_params")
								)
								, column(width=4
									, uiOutput("model_training_caret_models_naive_bayes_check")
									, uiOutput("model_training_caret_models_naive_bayes_advance_params")
								)
							 )
							 , uiOutput("model_training_apply")
					  )
					)
				}
			}
	})
	outputOptions(output, "model_training_caret_models_ui", suspendWhenHidden = FALSE)
	outputOptions(output, "model_training_apply",           suspendWhenHidden = FALSE)

		caret_job_manager_server(
			input = input,
			output = output,
			session = session,
			rv_current = rv_current,
			rv_ml_ai = rv_ml_ai,
			rv_training_models = rv_training_models,
			rv_train_control_caret = rv_train_control_caret,
			rv_training_results = rv_training_results,
			app_username = app_username,
			get_rv_labels = get_rv_labels
		)

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
