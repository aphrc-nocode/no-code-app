###### ---- Data Upload logfile -------------------------------------------####
collect_logs_server = function(){
	observeEvent(c(input$upload_ok, input$show_uploaded), {
		upload_logs_current = collect_logs(paste0(app_username, "/.log_files"), "*.upload.main.log")
		if (NROW(upload_logs_current)) {
			rv_metadata$upload_logs = upload_logs_current 
			rv_metadata$upload_logs$delete = create_btns(rv_metadata$upload_logs$file_name)
			write.table(rv_metadata$upload_logs, file=paste0(app_username, "/.log_files/.automl-shiny-upload.main.log"), row.names = FALSE)
		}
	})
}

