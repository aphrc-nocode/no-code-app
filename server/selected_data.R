#### ---- Currently selected dataset ------------------------------------ ####

currently_selected_data_server = function() {
	observeEvent(input$manage_data_apply, {
		file_path = get_data_class(paste0(app_username, "/datasets/", input$dataset_id))
		df = tryCatch({
			upload_data(file_path)
		}, error = function(e) {
      	shinyalert::shinyalert("Error: ", paste0(get_rv_labels("general_error_alert"), "\n", e$message), type = "error")
         return(NULL)

		})

		if (is.null(df)) return()

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
