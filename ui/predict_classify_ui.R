predict_classify_ui = function() {
	tabItem(tabName = "predictClassify",
		fluidRow(
			column(width=12
				, uiOutput("predict_trained_caret_models_box_ui"),
				predict_pycaret_ui("predict_pycaret")
			)
		)
	)
}
