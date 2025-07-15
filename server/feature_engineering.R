#### ---- Framework ------------------------ ####




#### ---- Preprocessing ------------------------ ####

##### ---- Recipe ------------------------ ####
setup_recipe_server = function() {
	observeEvent(input$setup_models_analysis_apply, {
		if (isTRUE(!is.null(rv_ml_ai$outcome))) {
			rv_ml_ai$model_formula = as.formula(paste0("`", rv_ml_ai$outcome, "`", "~."))
		} else {
			rv_ml_ai$model_formula = NULL
		}
	})
	
}


##### ---- Impute missing values ------------------------ ####
impute_missing_server = function() {
	observeEvent(input$setup_models_analysis_apply, {
		if (isTRUE(!is.null(input$setup_models_analysis_session_name))) {
			if (isTRUE(is.null(rv_current$missing_prop))) {
				rv_current$missing_prop = missing_prop(rv_current$working_df)
			} 
			if (isTRUE(NROW(rv_current$working_df) > 0)) {
				output$impute_missing_options = renderUI({
					empty_lab = ""
					names(empty_lab) = get_rv_labels("impute_missing_options_ph")
					selectInput("impute_missing_options"
						, label = get_rv_labels("impute_missing_options") 
						, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"impute_missing_options_choices"))
						, multiple=FALSE
					)
				})	
			} else {
				output$impute_missing_options = NULL
				updateSelectInput(session, "impute_missing_options", selected="")
			}
		} else {
			output$impute_missing_options = NULL
			updateSelectInput(session, "impute_missing_options", selected="")
		}
		print(rv_current$missing_prop)
		print("tests\n")
		print(rv_current$missing_prop_df)
	})
## 	observeEvent(input$setup_models_analysis_apply, {
## 		if (!isTRUE(NROW(rv_current$missing_prop))) {
## 			rv_current$missing_prop = missing_prop(rv_current$working_df)
## 		}
## 		if (isTRUE(NROW(rv_current$missing_prop))) {
## 			output$impute_missing_options = renderUI({
## 				empty_lab = ""
## 				names(empty_lab) = get_rv_labels("impute_missing_options_ph")
## 				selectInput("impute_missing_options"
## 					, label = "Test tes"##get_rv_labels("impute_missing_options") 
## 					, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"impute_missing_options_choices"))
## 					, multiple=FALSE
## 				)
## 			})
## 			print(rv_current$missing_prop)
## 		}
## 			print(rv_current$missing_prop)
## 
## ## 		if (isTRUE(!is.null(rv_current$working_df))) {
## ## 			if (!isTRUE(NROW(rv_current$missing_prop))) {
## ## 				rv_current$missing_prop = missing_prop(rv_current$working_df)
## ## 			}
## ## 			if (isTRUE(NROW(rv_current$missing_prop))) {
## ## 				output$impute_missing_options = renderUI({
## ## 					empty_lab = ""
## ## 					names(empty_lab) = get_rv_labels("impute_missing_options_ph")
## ## 					selectInput("impute_missing_options"
## ## 						, label = get_rv_labels("impute_missing_options") 
## ## 						, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"impute_missing_options_choices"))
## ## 						, multiple=FALSE
## ## 					)
## ## 				})				
## ## 			} else {
## ## 				output$impute_missing_options = NULL
## ## 				updateSelectInput(session, "impute_missing_options", selected="")
## ## 			}
## ## 		} else {
## ## 			output$impute_missing_options = NULL
## ## 			updateSelectInput(session, "impute_missing_options", selected="")
## ## 		}
## 	})
}
