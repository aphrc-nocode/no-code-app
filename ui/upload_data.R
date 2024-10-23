#### ---- Upload data UI ----------------------------------####
observe({
	if (!is.null(input$upload_type)) {
		upload_type_choices = get_named_choices(input_choices_file
			, rv_lang$selected_language
			, "upload_type"
		)
		rv_lang$upload_type_choices = upload_type_choices
	}
})

upload_type = renderUI({
	selectInput("upload_type"
		, label = get_rv_labels("upload_type")
		, choices = rv_lang$upload_type_choices 
	)
})

show_uploaded = renderUI({
	p(
		h5(HTML(paste0("<b>", get_rv_labels("show_uploaded"), ":</b>")))
   	, checkboxInput("show_uploaded", get_rv_labels("uploaded_datasets"))
	)
})

#### FIXME: FUTURE functionality

db_api_con_future = observeEvent(input$upload_type, {
	if (!is.null(input$upload_type)) {
		if (input$upload_type %in% c("API connection")) {
			shinyalert("", get_rv_labels("future_upload_msg"), type="warning", inputId="future_upload")
		}
	}
})
