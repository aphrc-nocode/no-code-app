##### ---- Update logfiles based on existing datasets -------------------####
update_logs_server = function() {
	observe({
	if (file.exists(".log_files/.automl-shiny-upload.main.log")) {
		 logs = try(read.table(".log_files/.automl-shiny-upload.main.log", header = TRUE), silent = TRUE)
		 if (!any(class(logs) %in% "try-error") & NROW(logs)) {
		 	rv_metadata$upload_logs = logs
			if (length(input$show_uploaded)) {
				if (input$show_uploaded) {
					output$upload_info = renderUI(h4(strong(get_rv_labels("uploaded_datasets"))))
					output$upload_logs = DT::renderDT(rv_metadata$upload_logs
						, escape = FALSE
						, rownames = FALSE
						, options = list(processing = FALSE
							, autoWidth = TRUE
							, scrollX = TRUE
						)
					)
				} else {
					output$upload_info = NULL
					output$upload_logs = NULL
				}
			}
		 }
		}
	})
}

