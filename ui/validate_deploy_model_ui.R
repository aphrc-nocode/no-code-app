validate_deploy_model_ui = function() {
	tabItem(tabName = "validateDeployModel",
	  fluidRow(
## 		conditionalPanel(
## 			condition = "input.modelling_framework_choices == 'Caret'",
			column(width=12
				, uiOutput("deploy_trained_caret_models_box_ui")
			)
## 		)
		,
## 		conditionalPanel(
## 			condition = "input.modelling_framework_choices == 'Pycaret'",
			column(width=12,
				#deployment_ui("deploy")
        uiOutput("deploy_container")
			)
## 		)
	  )
	)
}

