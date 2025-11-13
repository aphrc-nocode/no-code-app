feature_engineering_ui = function() {
	tabItem(tabName = "featureEngineering",
		fluidRow(
			column(width = 3,
				uiOutput("modelling_framework_choices"),
				## --- Bloc commun ou dynamique ---
				conditionalPanel(
					condition = "input.modelling_framework_choices == 'Caret'",
					uiOutput("feature_engineering_perform_partition"),
					uiOutput("feature_engineering_perform_preprocess"),
					uiOutput("feature_engineering_perform_missing_impute"),
					uiOutput("feature_engineering_impute_missing_impute"),
					uiOutput("feature_engineering_perform_fe_steps"),
					uiOutput("feature_engineering_perform_corr_steps"),
					uiOutput("feature_engineering_perform_corr_steps_value"),
					uiOutput("feature_engineering_perform_pca_steps"),
					uiOutput("feature_engineering_perform_upsample_steps"),
					uiOutput("feature_engineering_perform_upsample_steps_choices"),
					uiOutput("feature_engineering_apply")
				),
				conditionalPanel(
					condition = "input.modelling_framework_choices == 'Pycaret'",
					#uiOutput("automl_module_ui")
					automl_controls_ui("automl_controls")
				)
			),
			column(width = 9,
				uiOutput("feature_engineering_preprocessed_log_ui"),
				verbatimTextOutput("feature_engineering_preprocessed_log")
			)
		)
	)
}

