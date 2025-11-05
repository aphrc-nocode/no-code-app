train_all_model_ui = function() {
	tabItem(tabName = "trainModel",
		fluidRow(
			column(width = 12
				, uiOutput("model_training_setup_presetup")

			)
			, column(width=12
				, uiOutput("model_training_caret_models_ui")
			)
			, column(width=12
				, uiOutput("model_training_caret_train_metrics")
			)
			, column(width=12
				, train_model_ui("train_model")
			)
		)
	)
}
