overview_ui = function() {
	tabItem(tabName = "Overview"
		, fluidRow(id = "OverViewMenu"
			, column(width = 3
				, uiOutput("dataset_id")
				, uiOutput("manage_data_apply")
				, br()
				, uiOutput("manage_data_show")
			)
			, column(width=9
				, htmlOutput("manage_data_title")
				, conditionalPanel(
					condition = "input.manage_data_show == 'summarytools'"
					, style = "display: none;"
					, htmlOutput("data_summary_summarytools")
				)
				, verbatimTextOutput("data_summary")
			  )
		)
	)
}



