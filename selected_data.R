#### ---- Currently selected dataset ------------------------------------ ####

currently_selected_data_server = function() {
	observeEvent(input$manage_data_apply, {
		file_path = get_data_class(paste0("datasets/", input$dataset_id))
		df = upload_data(file_path)
		if (any(class(file_path) %in% c("spss", "dta"))) {
			df = (df
				|> as_factor()
			)
		}
		rv_current$working_df = rv_current$data = as.data.frame(df)
		rv_current$selected_vars = colnames(rv_current$data)
	})
}


### Reset if new language is selected in the current session
reset_currently_selected_data_server = function() {
	observeEvent(input$change_language, {
		rv_current$data = NULL
	})
}
