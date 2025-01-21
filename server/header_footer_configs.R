app_title = function() {
	output$app_title = renderUI({
		h3(get_rv_labels("app_title"), style = "text-align: center;")
	})
}

