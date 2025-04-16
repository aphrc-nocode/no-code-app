#### ---- Preprocessing ------------------------ ####

##### ---- Recipe ------------------------ ####
setup_recipe_server = function() {
	observeEvent(input$manage_data_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(rv_ml_ai$outcome))) {
				model
			}
		}
	})
	
}

impute_missing_server = function() {
	observeEvent(input$manage_data_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
						
		}
	})
}
