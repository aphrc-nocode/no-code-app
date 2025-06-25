#### ---- Preprocessing ------------------------ ####

##### ---- Recipe ------------------------ ####
feature_engineering_recipe_server = function() {
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(length(input$modelling_framework_choices)>0)) {
				if (isTRUE(!is.null(rv_ml_ai$outcome)) & isTRUE(rv_ml_ai$outcome!="")) {
					rv_ml_ai$model_formula = as.formula(paste0("`", rv_ml_ai$outcome, "`", "~."))
				} else {
					rv_ml_ai$model_formula = as.formula("~.")
				}
			} else {
				rv_ml_ai$model_formula = NULL
			}
		} else {
			rv_ml_ai$model_formula = NULL
		}
	})
}

##### ---- Perform feature engineering ------------------------ ####

feature_engineering_perform_preprocess_server = function() {
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(length(input$modelling_framework_choices)>0)) {
				
				## Data partitioning
				output$feature_engineering_perform_partition = renderUI({
					
					selectInput("feature_engineering_perform_partition"
						, label = get_rv_labels("feature_engineering_perform_partition") 
						, choices = get_named_choices(input_choices_file, input$change_language,"feature_engineering_perform_partition_choices")
						, selected = "single"
						, multiple=FALSE
					)
				})
				
				## Preprocess
				output$feature_engineering_perform_preprocess = renderUI({
					p(
						HTML("<b>", get_rv_labels("feature_engineering_perform_preprocess"), ": </b>")
						, materialSwitch(
							inputId = "feature_engineering_perform_preprocess_check",
							label = get_rv_labels("feature_engineering_perform_preprocess_check"), 
							status = "success",
							right = TRUE,
							value = TRUE
						 )
					)
				})
			} else {
				output$feature_engineering_perform_preprocess = NULL
				updateMaterialSwitch(session , inputId="feature_engineering_perform_preprocess_check" , value=FALSE)
				
				output$feature_engineering_perform_partition = NULL
				updateSelectInput(session, "feature_engineering_perform_partition", selected="")
			}
		} else {
			output$feature_engineering_perform_preprocess = NULL
			updateMaterialSwitch(session , inputId="feature_engineering_perform_preprocess_check" , value=FALSE)
				
			output$feature_engineering_perform_partition = NULL
			updateSelectInput(session, "feature_engineering_perform_partition", selected="")
		}
	})
}


##### ---- Impute missing values ------------------------ ####
feature_engineering_impute_missing_server = function() {
	
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(length(input$modelling_framework_choices)>0)) {
				if (isTRUE(input$feature_engineering_perform_preprocess_check)) {
					
					## Missing values input
					rv_current$missing_prop_impute = missing_prop(rv_current$working_df, return_exact=TRUE)
					if (isTRUE(NROW(rv_current$missing_prop_impute)>0)) {
						output$feature_engineering_perform_missing_impute = renderUI({
							materialSwitch(
								inputId = "feature_engineering_perform_missing_impute_check",
								label = get_rv_labels("feature_engineering_perform_missing_impute_check"), 
								status = "success",
								right = TRUE,
								value = TRUE
							 )
						})
					} else {
						output$feature_engineering_perform_missing_impute = NULL
						updateMaterialSwitch(session , inputId="feature_engineering_perform_missing_impute_check" , value=FALSE)
					}

					
					## Feature engineering steps
					output$feature_engineering_perform_fe_steps = renderUI({
						materialSwitch(
							inputId = "feature_engineering_perform_fe_steps_check",
							label = get_rv_labels("feature_engineering_perform_fe_steps_check"), 
							status = "success",
							right = TRUE,
							value = TRUE
						 )
					})
					
					## Correlated predictors
					output$feature_engineering_perform_corr_steps = renderUI({
						materialSwitch(
							inputId = "feature_engineering_perform_corr_steps_check",
							label = get_rv_labels("feature_engineering_perform_corr_steps_check"), 
							status = "success",
							right = TRUE,
							value = TRUE
						 )
					})

					## PCA
					output$feature_engineering_perform_pca_steps = renderUI({
						materialSwitch(
							inputId = "feature_engineering_perform_pca_steps_check",
							label = get_rv_labels("feature_engineering_perform_pca_steps_check"), 
							status = "success",
							right = TRUE,
							value = TRUE
						 )
					})

					## Up sample
					output$feature_engineering_perform_upsample_steps = renderUI({
						materialSwitch(
							inputId = "feature_engineering_perform_upsample_steps_check",
							label = get_rv_labels("feature_engineering_perform_upsample_steps_check"), 
							status = "success",
							right = TRUE,
							value = TRUE
						 )
					})
				
				} else {
					output$feature_engineering_perform_missing_impute = NULL
					updateMaterialSwitch(session , inputId="feature_engineering_perform_missing_impute_check" , value=FALSE)
					
					output$feature_engineering_perform_fe_steps = NULL
					updateMaterialSwitch(session , inputId="feature_engineering_perform_fe_steps_check" , value=FALSE)
					
					output$feature_engineering_perform_corr_steps = NULL
					updateMaterialSwitch(session , inputId="feature_engineering_perform_corr_steps_check" , value=FALSE)
					
					output$feature_engineering_perform_pca_steps = NULL
					updateMaterialSwitch(session , inputId="feature_engineering_perform_pca_steps_check" , value=FALSE)
					
					output$feature_engineering_perform_upsample_steps = NULL
					updateMaterialSwitch(session , inputId="feature_engineering_perform_upsample_steps_check" , value=FALSE)
				}
			} else {
				output$feature_engineering_perform_missing_impute = NULL
				updateMaterialSwitch(session , inputId="feature_engineering_perform_missing_impute_check" , value=FALSE)
				output$feature_engineering_perform_fe_steps = NULL
				updateMaterialSwitch(session , inputId="feature_engineering_perform_fe_steps_check" , value=FALSE)
				output$feature_engineering_perform_corr_steps = NULL
				updateMaterialSwitch(session , inputId="feature_engineering_perform_corr_steps_check" , value=FALSE)
				
				output$feature_engineering_perform_pca_steps = NULL
				updateMaterialSwitch(session , inputId="feature_engineering_perform_pca_steps_check" , value=FALSE)
				
				output$feature_engineering_perform_upsample_steps = NULL
				updateMaterialSwitch(session , inputId="feature_engineering_perform_upsample_steps_check" , value=FALSE)
			}
		} else {
			output$feature_engineering_perform_missing_impute = NULL
			updateMaterialSwitch(session , inputId="feature_engineering_perform_missing_impute_check" , value=FALSE)
				
			output$feature_engineering_perform_fe_steps = NULL
			updateMaterialSwitch(session , inputId="feature_engineering_perform_fe_steps_check" , value=FALSE)
				
			output$feature_engineering_perform_corr_steps = NULL
			updateMaterialSwitch(session , inputId="feature_engineering_perform_corr_steps_check" , value=FALSE)
				
			output$feature_engineering_perform_pca_steps = NULL
			updateMaterialSwitch(session , inputId="feature_engineering_perform_pca_steps_check" , value=FALSE)
			
			output$feature_engineering_perform_upsample_steps = NULL
			updateMaterialSwitch(session , inputId="feature_engineering_perform_upsample_steps_check" , value=FALSE)
		}
	})
	
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(length(input$modelling_framework_choices)>0)) {
				if (isTRUE(input$feature_engineering_perform_missing_impute_check)) {
					output$feature_engineering_impute_missing_impute = renderUI({
						empty_lab = ""
						names(empty_lab) = get_rv_labels("impute_missing_options_ph")
						selectInput("feature_engineering_impute_missing_impute"
							, label = NULL#get_rv_labels("impute_missing_options") 
							, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"impute_missing_options_choices"))
							, selected = "omit"
							, multiple=FALSE
						)
					})
				} else {
					output$feature_engineering_impute_missing_impute = NULL
					updateSelectInput(session, "feature_engineering_impute_missing_impute", selected="")
					
				}

				if (isTRUE(input$feature_engineering_perform_upsample_steps_check)) {
					output$feature_engineering_perform_upsample_steps_choices = renderUI({
						empty_lab = ""
						selectInput("feature_engineering_perform_upsample_steps_choices"
							, label = NULL
							, choices = get_named_choices(input_choices_file, input$change_language,"feature_engineering_perform_upsample_steps_choices")
							, selected = "random"
							, multiple=FALSE
						)
					})
				} else {
					output$feature_engineering_perform_upsample_steps_choices = NULL
					updateSelectInput(session, "feature_engineering_perform_upsample_steps_choices", selected="")
				}

				if (isTRUE(input$feature_engineering_perform_corr_steps_check)) {
					output$feature_engineering_perform_corr_steps_value = renderUI({
						sliderInput("feature_engineering_perform_corr_steps_value"
							, label = NULL
							, min = 0
							, max = 1
							, value = 0.8
							, step = NULL
						)
					})
				} else {
					output$feature_engineering_perform_corr_steps_value = NULL
					updateSliderInput(session, "feature_engineering_perform_corr_steps_value", value = NULL)
				}

			} else {
				output$feature_engineering_impute_missing_impute = NULL
				updateSelectInput(session, "feature_engineering_impute_missing_impute", selected="")
				
				output$feature_engineering_perform_upsample_steps_choices = NULL
				updateSelectInput(session, "feature_engineering_perform_upsample_steps_choices", selected="")
					
				output$feature_engineering_perform_corr_steps_value = NULL
				updateSliderInput(session, "feature_engineering_perform_corr_steps_value", value = NULL)
			}
		} else {
			output$feature_engineering_impute_missing_imputes = NULL
			updateSelectInput(session, "feature_engineering_impute_missing_impute", selected="")
				
			output$feature_engineering_perform_upsample_steps_choices = NULL
			updateSelectInput(session, "feature_engineering_perform_upsample_steps_choices", selected="")
				
			output$feature_engineering_perform_corr_steps_value = NULL
			updateSliderInput(session, "feature_engineering_perform_corr_steps_value", value = NULL)
		}
	})


	## Apply the FE selections
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(length(input$modelling_framework_choices)>0)) {
				output$feature_engineering_apply = renderUI({
					actionBttn("feature_engineering_apply"
						, inline=TRUE
						, block = FALSE
						, color = "success"
						, label = get_rv_labels("feature_engineering_apply"))
				})
			} else {
				output$feature_engineering_apply = NULL	
			}
		} else {
			output$feature_engineering_apply = NULL	
		}
	})


	## Apply all the changes
	observeEvent(input$feature_engineering_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(length(input$modelling_framework_choices)>0)) {
				partition_objs = Rautoml::train_test_split(
					data=rv_current$working_df
					, type = input$feature_engineering_perform_partition
					, prop=rv_ml_ai$partition_ratio
				)
				rv_ml_ai$split = partition_objs$split
				rv_ml_ai$train_df = partition_objs$train_df
				rv_ml_ai$test_df = partition_objs$test_df
				if (isTRUE(input$feature_engineering_perform_preprocess_check)) {
					rv_ml_ai$preprocessed = Rautoml::preprocess(
						df = rv_ml_ai$train_df
						, model_form = rv_ml_ai$model_formula
						, outcome_var = rv_ml_ai$outcome
						, corr = input$feature_engineering_perform_corr_steps_value
						, impute = input$feature_engineering_perform_missing_impute_check
						, impute_methods=input$feature_engineering_impute_missing_impute
						, perform_fe = input$feature_engineering_perform_fe_steps_check
						, perform_pca = input$feature_engineering_perform_pca_steps_check
						, up_sample = input$feature_engineering_perform_upsample_steps_check
						, df_test = rv_ml_ai$test_df
					)
					rv_ml_ai$feature_engineering_preprocessed_log = rv_ml_ai$preprocessed$preprocess_steps
					output$feature_engineering_preprocessed_log_ui = renderUI({
						p(
							
							HTML(paste0("<b>", get_rv_labels("feature_engineering_preprocessed_log"), "</b>"))
						)
					})
					output$feature_engineering_preprocessed_log = renderPrint({
						cat(rv_ml_ai$feature_engineering_preprocessed_log, sep="\n")
					})
				} else {
					rv_ml_ai$preprocessed = NULL
					rv_ml_ai$feature_engineering_preprocessed_log = NULL
				} 
			} else {
				rv_ml_ai$preprocessed = NULL
				rv_ml_ai$split = NULL
				rv_ml_ai$train_df = NULL
				rv_ml_ai$test_df = NULL
				rv_ml_ai$feature_engineering_preprocessed_log = NULL
			}
		} else {
			rv_ml_ai$preprocessed = NULL
			rv_ml_ai$split = NULL
			rv_ml_ai$train_df = NULL
			rv_ml_ai$test_df = NULL
			rv_ml_ai$feature_engineering_preprocessed_log = NULL
		}
	})
}


