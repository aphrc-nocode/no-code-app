explore_ui = function() {
	tabItem(tabName = "Explore",

		fluidRow(
			column(
				width = 3
				, htmlOutput("manage_data_title_explore")
				, uiOutput("explore_data_show_data")
				, uiOutput("explore_data_show_data_type")
				, uiOutput("explore_data_missingness")
				, uiOutput("explore_data_filter")
				, uiOutput("explore_data_filter_rules")
				, uiOutput("manage_data_explore_filter_apply")
				, uiOutput("manage_data_explore_filter_reset")
				, uiOutput("explore_data_select_variables")
				, uiOutput("manage_data_select_vars")
				, uiOutput("explore_data_quick_explore")
				, uiOutput("explore_data_update_data")
			)

			, column(
				width = 9
				, fluidRow(
					column(width = 8,
						conditionalPanel(
							condition = "input.explore_data_show_data_check == 1"
							, htmlOutput("current_dataset_text")
						)
						, conditionalPanel(
							condition = "input.explore_data_show_data_check == 1 & input.explore_data_show_data_type_check=='All'"
							, DT::DTOutput("working_df_all", width = "100%", fill=TRUE)
						)

						, conditionalPanel(
							condition = "input.explore_data_show_data_check == 1 & input.explore_data_show_data_type_check != 'All'"
							, verbatimTextOutput("working_df")
						)
					)

					, column(width=4
						, htmlOutput("data_explore_filter_applied")
						, verbatimTextOutput("data_explore_filter_applied_out")
						, htmlOutput("explore_missing_data_out")
						, verbatimTextOutput("explore_missing_data")
					)
				)
				, fluidRow(
					column(width=12
						, htmlOutput("explore_data_quick_explore_ui")
						, verbatimTextOutput("explore_data_quick_explore_out")
					)
				)
			)
		)
	)
}
         
	
