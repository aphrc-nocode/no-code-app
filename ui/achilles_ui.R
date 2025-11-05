achilles_ui = function() {
	tabItem(
		tabName = "achilles",
		fluidRow(
			box(title = "Schema Selection"
				 ,status = "success"
				 ,solidHeader = TRUE
				 ,width = 12
				 ,collapsible = FALSE
				 ,uiOutput("schema_selectors")
				 ,uiOutput("run_achilles")
			)
		)
	)
}
