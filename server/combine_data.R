##### ---- List datasets ------------------------------------------####
combine_data_list_datasets = function() {
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(length(rv_metadata$dataset_ids)>1)) {
				combine_data_ids = rv_metadata$dataset_ids[!rv_metadata$dataset_ids %in% input$dataset_id]
				empty_lab = ""
				names(empty_lab) = get_rv_labels("combine_data_list_datasets_ph")
				output$combine_data_list_datasets = renderUI({
					selectInput("combine_data_list_datasets"
						, label = get_rv_labels("combine_data_list_datasets") 
						, choices = c(empty_lab, combine_data_ids)
						, multiple=FALSE
					)
				})
			} else {
				output$combine_data_apply = NULL
				output$combine_data_list_datasets = renderText(paste0(get_rv_labels("combine_no_second_data"), "<b> Source data </b>"))
			}
		} else {
			output$combine_data_source_choices = NULL
			output$combine_data_apply = NULL
		}
	})
	
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(!is.null(input$combine_data_list_datasets)) & isTRUE(input$combine_data_list_datasets!="")) {
				output$combine_data_apply = renderUI({
					actionBttn("combine_data_apply"
						, inline=TRUE
						, block = FALSE
						, color = "success"
						, label = get_rv_labels("apply_selection"))
				})
			} else {
				output$combine_data_apply = NULL
			}
		} else {
				output$combine_data_apply = NULL
		}
	})
}


##### ---- Match columns ------------------------------------------####

combine_data_match_columns = function() {
	observeEvent(input$combine_data_apply, {
		if (isTRUE(!is.null(input$combine_data_list_datasets))) {
			file_path = get_data_class(paste0("datasets/", input$combine_data_list_datasets))
			combine_df = upload_data(file_path)
			if (any(class(file_path) %in% c("spss", "dta"))) {
				combine_df = (combine_df
					|> as_factor()
				)
			}
			rv_current$combine_df = as.data.frame(combine_df)
			rv_current$combine_data_selected_vars = colnames(combine_df)
		} else {
			rv_current$combine_df = NULL 
		}
	})
	
	observe({
		if (isTRUE(!is.null(rv_current$combine_df)) & isTRUE(!is.null(rv_current$working_df))) {
			combine_data_matched = intersect(rv_current$selected_vars, rv_current$combine_data_selected_vars)
			print(combine_data_matched)
		}
	})

}
