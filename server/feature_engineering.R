#### ---- Preprocessing ------------------------ ####

##### ---- Recipe ------------------------ ####
setup_recipe_server = function() {
	observeEvent(input$manage_data_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$outcome))) {
				rv_ml_ai$model_formula = as.formula(paste0(rv_ml_ai$outcome, "~."))
			} else {
				rv_ml_ai$model_formula = NULL
			}
		} else {
			rv_ml_ai$model_formula = NULL
		}
	})
	
}

impute_missing_server = function() {
	observeEvent(input$manage_data_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
						
		}
	})
}
