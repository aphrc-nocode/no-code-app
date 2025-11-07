#### ---- Change UI language --------------------------------####
rv_lang = reactiveValues(selected_language=language_choices[1]
	, language_label=language_labels[1]
	, labelling_file_df = labelling_file |> filter(input_language==language_choices[1])
	, upload_type_choices = get_named_choices(input_choices_file, language_choices[1], "upload_type")
)

change_language = renderUI({
	selectInput("change_language"
		, label = rv_lang$language_label
		, choices=language_choices
		, selected = rv_lang$selected_language
	)
})

## observeEvent(input$change_language, {
## 	if (!is.null(input$change_language)) {
## 		labelling_file_df = (labelling_file
## 			|> filter(input_language==input$change_language)
## 		)
## 		rv_lang$selected_language = input$change_language 
## 		rv_lang$language_label = labelling_file_df$language_label
## 		rv_lang$labelling_file_df = labelling_file_df
## 	}
## })

