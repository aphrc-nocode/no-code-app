setup_models_ui = function() {
	tabItem(tabName = "setupModels",
		fluidRow(
			column(width=3
				, uiOutput("setup_models_analysis_session_name")
				, uiOutput("setup_models_analysis_session_seed")
				, uiOutput("setup_models_analysis_target_variable_options")
				, uiOutput("setup_models_analysis_target_variable")
				, uiOutput("setup_models_analysis_exclude_variables")
				, uiOutput("setup_models_analysis_type")
				, uiOutput("setup_models_analysis_model_type")
				, uiOutput("setup_models_analysis_partition_ratio")

				, uiOutput("setup_models_analysis_apply")
			)

			, column(width = 9
				, htmlOutput("setup_models_analysis_results")
			)
		)
	)
}
