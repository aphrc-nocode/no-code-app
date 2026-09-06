###### ---- Data Upload logfile -------------------------------------------####
collect_logs_server = function(){
	observeEvent(c(input$upload_ok, input$show_uploaded), {
		## Scan and rewrite inside the lock. Scanning first and writing later
		## would rewrite the summary from a listing taken before a delete ran,
		## putting a deleted dataset back into it.
		collected = with_upload_lock(app_username, {
			upload_logs_current = collect_logs(paste0(app_username, "/.log_files"), "*.upload.main.log")
			if (!NROW(upload_logs_current)) {
				NULL
			} else {
				upload_logs_current$delete = create_btns(upload_logs_current$file_name)
				## Only hand the listing back once it is actually saved, so the
				## screen never shows state that did not reach the disk
				if (write_upload_summary(app_username, upload_logs_current)) {
					upload_logs_current
				} else {
					NULL
				}
			}
		})
		if (!is.null(collected)) rv_metadata$upload_logs = collected
	})
}

