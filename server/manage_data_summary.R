#### ---- Generate a summary of the current data ----------------------------- ####

generate_data_summary_server = function() {
	observeEvent(input$manage_data_apply, {
		if (!is.null(rv_current$data)) {
 ## 			progressSweetAlert(
 ## 				session = session, id = "summary_statistics_progress"
 ## 				, title = get_rv_labels("summary_statistics_progress")
 ## 				, display_pct = TRUE, value = 0
 ## 			)
 			strff = function(){
				rv_current$data_summary_str = renderPrint({
					str(rv_current$data)
				})
			}
			skimff = function() {
				rv_current$data_summary_skim = renderPrint({
					skimr::skim(rv_current$data)
				})
			}
			sumff = function() {
				rv_current$data_summary_summary = renderPrint({
					summary(rv_current$data)
				})
			}
			sumtoolsff = function() {
				rv_current$data_summary_summarytools = renderUI({
				 summarytools::view(
				   summarytools::dfSummary(rv_current$data, style= "grid")
					, method = "render"
					, silent=TRUE
					, max.tbl.height = 600
				 )
				})
			}
			funs = list(strff, skimff, sumff, sumtoolsff)
			i = 1
			for (ff in funs) {
				do.call(ff, list())
 ## 				updateProgressBar(
 ## 				  session = session,
 ## 				  id = "summary_statistics_progress",
 ## 				  value = i*(100/length(funs)),
 ## 				)
				i = i + 1
		#		Sys.sleep(0.05)
			}
 ## 			closeSweetAlert(session = session)
		}
	})
}

display_data_summary_server = function() {
	observe({
		if (!is.null(rv_current$data) & !is.null(input$manage_data_show)) {
			if (!is.null(rv_current$data_summary_str) & input$manage_data_show=="str") {
				output$data_summary = rv_current$data_summary_str
			} else if (!is.null(rv_current$data_summary_skim) & input$manage_data_show=="skim") {
				output$data_summary = rv_current$data_summary_skim
			} else if (!is.null(rv_current$data_summary_summary) & input$manage_data_show=="summary") {
				output$data_summary = rv_current$data_summary_summary
			} else if (!is.null(rv_current$data_summary_summarytools) & input$manage_data_show=="summarytools") {
			  output$data_summary = NULL
			  output$data_summary_summarytools = rv_current$data_summary_summarytools
			} 
		} 
	})
}

