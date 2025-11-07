change_language_update = function() {
	observeEvent(input$change_language, {
		if (!is.null(input$change_language)) {
			labelling_file_df = (labelling_file
				|> filter(input_language==input$change_language)
			)
			rv_lang$selected_language = input$change_language
			rv_lang$language_label = labelling_file_df$language_label
			rv_lang$labelling_file_df = labelling_file_df
		}
	})
}
