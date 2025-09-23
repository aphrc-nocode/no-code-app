#### ---- Display meta data related to current selection ------------------ ####

display_selected_metadata_server = function() {
  observeEvent(input$manage_data_apply, {
  	 rv_current$dataset_id = input$dataset_id
    metadata = rv_metadata$upload_logs[rv_metadata$upload_logs$file_name==rv_current$dataset_id, ,drop=FALSE]
    rv_current$metadata_id = list(study_name = paste0("<b>", get_rv_labels("study_name"),  ": </b>", metadata$study_name)
      , upload_time = paste0("<b>", get_rv_labels("upload_time"),  ": </b>", metadata$upload_time)
      , coutry = paste0("<b>", get_rv_labels("study_country"), ": </b>", metadata$study_country)
    )
    
	 rv_current$metadata_id = paste0(rv_current$metadata_id, sep = "<br/>")
    output$manage_data_title = renderUI({
      HTML(rv_current$metadata_id)
    })
  })
}

### Reset if new language is selected in the current session
reset_display_selected_metadata_server = function() {
	observeEvent(input$change_language, {
		output$manage_data_title = NULL
		rv_current$dataset_id = NULL
	})
}
