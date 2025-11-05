evidence_quality_ui = function() {
	tabItem(
	  tabName = "evidenceQuality",
		fluidRow(
		 p("OMOP Data Quality Check and Characterization"),
		 uiOutput ("global_source_redirect")
		 , uiOutput("omop_connection")
		 , uiOutput("existing_connection")
		 , uiOutput("omop_quality_type")
		 , uiOutput("schemas")
		 , uiOutput("generate_dqd")
		 ,verbatimTextOutput("stderr_log")
		 ,uiOutput("view_dqd")
		 ,br()
		 ,uiOutput("open_link")
	  )
	)
}
