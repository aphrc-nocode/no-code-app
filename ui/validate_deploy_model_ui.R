validate_deploy_model_ui = function() {
	tabItem(tabName = "validateDeployModel",
	  fluidRow(
		column(width=12
			, uiOutput("deploy_trained_caret_models_box_ui")
		)
		, column(width=12,
			deployment_ui("deploy")
		)
	  )
	)
}
