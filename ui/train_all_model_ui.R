train_all_model_ui = function() {
	tabItem(tabName = "trainModel",
		fluidRow(
			conditionalPanel(
				condition = "input.modelling_framework_choices == 'Caret'",
				column(width = 12
					, uiOutput("model_training_setup_presetup")

				)
				, column(width=12
					, uiOutput("model_training_caret_models_ui")
				)
				, column(width=12
					, uiOutput("model_training_caret_train_metrics")
				)
			),
			conditionalPanel(
				condition = "input.modelling_framework_choices == 'Pycaret'",		
				column(width=12
					, train_model_ui("train_model")
				)
			)
		)
	)
}
