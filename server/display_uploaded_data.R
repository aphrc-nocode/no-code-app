
##### ---- Display uploaded files ---------------------------------------####
display_uploaded_data_server = function() {
	observeEvent(c(input$upload_ok, input$show_uploaded), {
	 if ((!is.null(rv_metadata$upload_logs) & NROW(rv_metadata$upload_logs))) {
	 	output$upload_info = renderUI(h4(strong(get_rv_labels("uploaded_datasets"))))
		rv_metadata$upload_logs = dplyr::arrange(rv_metadata$upload_logs, desc(upload_time))
		output$upload_logs = DT::renderDT(rv_metadata$upload_logs
		  , escape = FALSE
		  , rownames = FALSE
		  , options = list(processing = FALSE
		  	, autoWidth = TRUE
			, scrollX = TRUE
		  )
		)
		updateCheckboxInput(session, "show_uploaded", value = TRUE)
	 } else {
		output$upload_info = NULL
		output$upload_logs = NULL
		shinyalert::shinyalert("", get_rv_labels("no_data_error_msg"), type = "error", inputId="non_data_ok")
	 }
	})
}

