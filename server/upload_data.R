##### ---- Data Upload Type and Submit ------------------------------------####
upload_data_server = function(){
	observeEvent(input$submit_upload, {
	  start_progress_bar(id="data_upload_id_pb", att_new_obj=data_upload_id_pb, text=get_rv_labels("data_upload_id_progres_bar"))
	 db_df = NULL
	 if (input$upload_type=="Local") {
		req(iv$is_valid())
		req(input$files_with_ext)
		if (is.null(input$files_with_ext)) return()
		file_name_full = input$files_with_ext$name
	 } else if (input$upload_type=="URL") {
		req(iv_url$is_valid())
		req(input$url_upload)
		file_name_full = input$url_upload
	 }else if(input$upload_type == "Database connection"){
	   file_ext = "csv"
	   ## The table is only held in memory here. It used to be written straight
	   ## to its final path at this point, under a name built from a different
	   ## timestamp than the log written later, so the file and its log entry
	   ## could disagree - and a crash in between left a dataset nothing knew
	   ## about. It is now staged and moved into place under the lock below.
	   if(input$option_picked == "use a table"){
	     req(rv_database$df_table_str)
	     file_name_full = paste0(rv_database$schema_selected,rv_database$table_selected,".csv")
	     db_df = data.frame(rv_database$df_table_str)
	   }else{
	     req(rv_database$df_table)
	     file_name_full = paste0(rv_database$query_table_name,".csv")
	     db_df = data.frame(rv_database$df_table)
	   }
	   if (!NROW(db_df)) {
	     shinyalert::shinyalert("", "Table save failed.", type = "info")
	     close_progress_bar(att_new_obj=data_upload_id_pb)
	     return()
	   }
	 }
	 file_name = get_file_name(file_name_full)
	 file_ext = get_file_ext(file_name_full)
	 supported_files_temp = gsub("\\.", "", supported_files)

	 if (input$upload_type=="URL" & tolower(file_ext) == "xls") {
		shinyalert::shinyalert("", get_rv_labels("xls_error_msg"), type = "error", inputId="upload_error")
		reset("upload_form")
	 } else {
		if (any(!tolower(file_ext) %in% supported_files_temp)) {
		  shinyalert::shinyalert("", get_rv_labels("supported_files_msg"), type = "error", inputId="upload_error")
		  if (input$upload_type=="Local") {
			 file.remove(input$files_with_ext$datapath)
		  }
		  reset("upload_form")
		} else {
		  upload_time = Sys.time()
		  temp_name = paste0(file_name, "-", format_date_time(upload_time, "%d%m%Y%H%M%S"), ".", file_ext)
		  upload_time = format_date_time(upload_time)

		  ## Everything is staged under a name the app ignores, and only moved
		  ## to the real one inside the lock. A crash before that point leaves a
		  ## stray staging file rather than a dataset with no log entry, which
		  ## neither the uploads table nor the cleanup worker would ever see.
		  final_path = paste0(app_username, "/datasets", "/", temp_name)
		  staging_path = paste0(app_username, "/datasets", "/.incoming-", temp_name)

		  if (input$upload_type=="Local") {
			 if (file.exists(input$files_with_ext$datapath)) {
				file.copy(input$files_with_ext$datapath, staging_path)
				file.remove(input$files_with_ext$datapath)
			 }
			 read_from = get_data_class(staging_path)
		  } else if (input$upload_type=="URL") {
			 read_from = get_data_class(file_name_full)
		  } else {
			 read_from = NULL
		  }

		  df = if (is.null(read_from)) db_df else try(upload_data(read_from), silent = TRUE)

		  if (!is.data.frame(df) | is.null(df) | any(class(df) %in% "try-error")) {
			 if (file.exists(staging_path)) {
				file.remove(staging_path)
			 }
			 shinyalert::shinyalert("", get_rv_labels("uploaded_data_error_msg"), type = "error", inputId="data_error")
		  } else {

			 ## Generate metadata
			 meta_data = Rautoml::create_df_metadata(data=df
				, filename=temp_name
				, study_name=input$study_name
				, study_country=input$study_country
				, additional_info=input$additional_info
				, upload_time=upload_time
				, last_modified = upload_time
				, user = app_username
			 )

			 log_file_main = paste0(app_username, "/.log_files/", temp_name, "-upload.main.log")
			 ## The move into place, the log and the summary all happen together
			 ## under the same lock the delete paths use, so an upload cannot
			 ## land in the middle of a deletion or leave one part behind.
			 written = with_upload_lock(app_username, {
				tryCatch({
				  ## URL and database data have no uploaded temp file to move,
				  ## so create their staging file here. Keeping this inside the
				  ## handler guarantees a failed write is cleaned up and the
				  ## progress observer returns a normal error to the user.
				  if (input$upload_type!="Local") {
					 write_data(get_data_class(staging_path), df)
				  }
				  if (!file.rename(staging_path, final_path)) stop("could not move the uploaded file into place")
				  write.csv(meta_data, log_file_main, row.names = FALSE)
				  upload_logs_current = collect_logs(paste0(app_username, "/.log_files"), "*.upload.main.log")
				  if (!NROW(upload_logs_current)) stop("the upload log could not be read back")
				  upload_logs_current$delete = create_btns(upload_logs_current$file_name)
				  ## An upload that is not in the summary is invisible to the
				  ## uploads table and to retention, so a summary that will not
				  ## save means the whole upload is rolled back rather than
				  ## reported as a success
				  if (!write_upload_summary(app_username, upload_logs_current)) {
					 stop("the upload summary could not be written")
				  }
				  TRUE
				}, error = function(e) {
				  message(sprintf("Upload of '%s' failed: %s", temp_name, conditionMessage(e)))
				  ## Undone here, while the lock is still held. Rolling back
				  ## after it is released leaves a window in which another
				  ## session or the cleanup worker sees a half-written upload
				  ## and rebuilds the summary around it.
				  for (p in c(log_file_main, final_path, staging_path)) {
					 if (file.exists(p)) file.remove(p)
				  }
				  FALSE
				})
			 })
			 if (!isTRUE(written)) {
				## A NULL means the lock was never taken, so nothing but the
				## staging file can exist; a FALSE has already cleaned up above
				if (file.exists(staging_path)) file.remove(staging_path)
				shinyalert::shinyalert("", get_rv_labels("general_error_alert"), type = "error", inputId="data_error")
			 } else {
				shinyalert::shinyalert("", get_rv_labels("data_upload_successful_msg"), type = "success", inputId="upload_ok")
			 }
		  }
		}
		reset("upload_form")
	 }

	 close_progress_bar(att_new_obj=data_upload_id_pb)

	})
}
