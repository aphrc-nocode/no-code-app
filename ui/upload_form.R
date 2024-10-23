#### ---- Study name ---------------------------------------#####

study_name = renderUI({
	textInput("study_name", get_rv_labels("study_name"), width = "50%")
})

#### ---- Study country ---------------------------------------#####
study_country = renderUI({
	selectInput("study_country", get_rv_labels("study_country"), choices = countries::list_countries(), multiple = TRUE)
})

#### ---- Additional info ---------------------------------------#####
additional_info = renderUI({
	textAreaInput("additional_info", get_rv_labels("additional_info"), placeholder = get_rv_labels("additional_info_ph"), width = "50%")
})

#### ----- Submit upload ----------------------------------------####
submit_upload = renderUI({
	actionBttn("submit_upload", get_rv_labels("submit_upload"), width = "25%"
		, inline=TRUE
		, block = FALSE
		, color = "success"
	)
})
