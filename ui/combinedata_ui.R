combinedata_ui = function() {
	tabItem(tabName = "combineData"
		, fluidRow(
			column(width = 3
				, htmlOutput("combine_data_title")
			)
			, column(width=9
				, uiOutput("combine_data_list_datasets")
				, uiOutput("combine_data_apply")
				, uiOutput("combine_data_type_choices")
				, uiOutput("combine_data_match_type")
				, htmlOutput("combine_data_matched_vars_manual_ui")
				, column(width=4
					, uiOutput("combine_data_base_vars")
				)
				, column(width=4
					, uiOutput("combine_data_new_vars")
				)
				, column(width=4
					, htmlOutput("combine_data_matched_vars")
				)
				, br()
				, uiOutput("combine_data_manual_match_apply")
				, br()
				, uiOutput("combine_data_create_id_var_input")
				, br()
				, uiOutput("combine_data_perform_merging_apply")
				, htmlOutput("combine_data_row_wise_values_log_ui")
				, verbatimTextOutput("combine_data_row_wise_values_log")
			)
		)
	)
}
