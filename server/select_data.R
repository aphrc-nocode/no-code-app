  #### ---- Manage Data ----------------------------------------------------####

select_data_server = function(){
	observe({
		if (NROW(rv_metadata$upload_logs)) {
			## Uniquely identify current dataset
			id_names = as.list(rv_metadata$upload_logs$file_name)
			names(id_names) = paste0(rv_metadata$upload_logs$study_name, " (", rv_metadata$upload_logs$study_country, ", ", rv_metadata$upload_logs$upload_time, ")")
			rv_metadata$dataset_ids = id_names

			output$dataset_id = renderUI({
			  selectInput("dataset_id"
				 , label = get_rv_labels("select_data")
				 , choices = rv_metadata$dataset_ids
				 , selected = rv_metadata$dataset_ids[1]
				 , width = "100%"
			  )
			})
			output$manage_data_apply = renderUI({
				actionBttn("manage_data_apply"
					, inline=TRUE
      			, block = FALSE
      			, color = "success"
					, label = get_rv_labels("apply_selection"))
			})
		} else {
			rv_metadata$dataset_ids = NULL
			output$manage_data_apply = NULL
			output$dataset_id = renderText(paste0(get_rv_labels("no_data"), "<b> Source data </b>"))
			output$manage_data_show = NULL
			output$manage_data_title = NULL
		}
	})
}

manage_data_show_server = function() {
	observeEvent(input$manage_data_apply, {
		output$manage_data_show = renderUI({
		  radioButtons("manage_data_show"
			 , paste0(get_rv_labels("data_summary"), ": ")
			 , choices = c("str", "summary","skim","summarytools")
			 , inline = TRUE
		  )
		})
	})
	
	observeEvent(input$change_language, {
		output$manage_data_show = NULL
	})
}
