#### ---- Upload datasets/files UI --------------------------------####
input_files = renderUI({
	 if (isTRUE(input$upload_type=="Local")) {
		fileInput("files_with_ext", get_rv_labels("upload_file")
		  , buttonLabel = get_rv_labels("browse_files")
		  , placeholder = paste0(get_rv_labels("drop_files"), ": ", paste0(supported_files, collapse = ", "))
		  , accept = supported_files
		  , width = "50%"
		)
	 } else if (isTRUE(input$upload_type=="URL")) {
		textInput("url_upload"
	  	, "URL"
	  	, placeholder = paste0(get_rv_labels("supported_urls_msg")
		 	, ": ", paste0(supported_files, collapse = ", ") 
			)
	  	, width = "50%"
		)
 	} else if (isTRUE(input$upload_type=="Database connection")) {
		textInput("db_host_url", label = get_rv_labels("db_host_url")
			, placeholder = get_rv_labels("db_host_url_ph")
			, width = "50%"
		)	
	}
})

