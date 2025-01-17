##### ---- Data Upload Type and Submit ------------------------------------####
upload_data_server = function(){
	observeEvent(input$submit_upload, {
	 if (input$upload_type=="Local") {
		req(iv$is_valid())
		req(input$files_with_ext)
		if (is.null(input$files_with_ext)) return()
		file_name_full = input$files_with_ext$name
	 } else if (input$upload_type=="URL") { 
		req(iv_url$is_valid())
		req(input$url_upload)
		file_name_full = input$url_upload
	 }  else if(input$upload_type == "Database connection"){
	   req(rv_database$df_table)
	   file_name_full = paste0(rv_database$schema_selected,rv_database$table_selected,".csv")
	   file_ext = "csv"
	   file_name = paste0(rv_database$schema_selected,rv_database$table_selected)
	   temp_name = paste0(file_name, "-", format_date_time(Sys.time(), "%d%m%Y%H%M%S"), ".", "csv")
	   file_path = get_data_class(paste0("datasets", "/", temp_name))
	   df = data.frame(rv_database$df_table)
	   if(length(rv_database$df_table > 0)){
	       write_data(file_path, df)
	     }else{shinyalert("", "Table save failed.", type = "info")}
	     
	 }

	 file_name = get_file_name(file_name_full)
	 file_ext = get_file_ext(file_name_full)
	 supported_files_temp = gsub("\\.", "", supported_files)
	 
	 if (input$upload_type=="URL" & tolower(file_ext) == "xls") {
		shinyalert("", get_rv_labels("xls_error_msg"), type = "error", inputId="upload_error")
		reset("upload_form")
	 } else {
		if (any(!tolower(file_ext) %in% supported_files_temp)) {
		  shinyalert("", get_rv_labels("supported_files_msg"), type = "error", inputId="upload_error")
		  if (input$upload_type=="Local") {
			 file.remove(input$files_with_ext$datapath)
		  }
		  reset("upload_form")
		} else {
		  upload_time = Sys.time()
		  temp_name = paste0(file_name, "-", format_date_time(upload_time, "%d%m%Y%H%M%S"), ".", file_ext)
		  upload_time = format_date_time(upload_time)

		  if (input$upload_type=="Local") {
			 file_path = paste0("datasets", "/", temp_name)
			 if (file.exists(input$files_with_ext$datapath)) {
				file.copy(input$files_with_ext$datapath, file_path)
				file.remove(input$files_with_ext$datapath)
			 }
		  } else if (input$upload_type=="URL") {
			 file_path = file_name_full
		  }
		  
		  file_path = get_data_class(file_path)
		  df = try(upload_data(file_path), silent = TRUE)
		  
		  if (!is.data.frame(df) | is.null(df) | any(class(df) %in% "try-error")) {
			 if (file.exists(file_path)) {
				file.remove(file_path)
			 }
			 shinyalert("", get_rv_labels("uploaded_data_error_msg"), type = "error", inputId="data_error")
		  } else {
			 
			 if (input$upload_type=="URL") {
				file_path = get_data_class(paste0("datasets", "/", temp_name))
				write_data(file_path, df)
			 }
			 
			 ## Generate metadata
			 meta_data = Rautoml::create_df_metadata(data=df
				, filename=temp_name
				, study_name=input$study_name
				, study_country=input$study_country
				, additional_info=input$additional_info
				, upload_time=upload_time
				, last_modified = upload_time
			 )
	 
			 log_file_main = paste0(".log_files/", temp_name, "-upload.main.log")
			 write.csv(meta_data, log_file_main, row.names = FALSE)
			 shinyalert("", get_rv_labels("data_upload_successful_msg"), type = "success", inputId="upload_ok")
		  }
		}
		reset("upload_form")
	 }
	 
	})
}



