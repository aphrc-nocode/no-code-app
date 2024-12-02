
##### ----- Detete uploaded datasets ------------------------------------#####
delete_uploaded_data_server = function() {
	observeEvent(input$current_id, {
	 req(!is.null(input$current_id))
	 req(!is.null(rv_metadata$upload_logs))
	 req(stringi::stri_detect_regex(input$current_id, pattern = "ytxxdeletezzyt_"))
	 all_ids = str_replace_all(rv_metadata$upload_logs$delete, "[^[:alnum:]]|[[:punct:]]", "")
	 id = str_replace_all(input$current_id, "[^[:alnum:]]|[[:punct:]]", "")
	 dt_rw = stringi::stri_detect_regex(all_ids, pattern=id)
	 rv_current$current_id = gsub("ytxxdeletezzyt_", "", input$current_id)
	 rv_metadata$upload_logs = rv_metadata$upload_logs[dt_rw==FALSE, ]
	 current_log = paste0(".log_files/", rv_current$current_id, "-upload.main.log")
	 current_file = paste0("datasets/", rv_current$current_id)
	 if (file.exists(current_log)) {
		try(file.remove(current_log), silent = TRUE)
	 }
	 if (file.exists(current_file)) {
		try(file.remove(current_file), silent = TRUE)
	 }
	 write.table(rv_metadata$upload_logs, file=".log_files/.automl-shiny-upload.main.log", row.names = FALSE)
	 if (isTRUE(!NROW(rv_metadata$upload_logs))) {
	 	updateCheckboxInput(session, "show_uploaded", value = FALSE)
	 } 
	})
}
