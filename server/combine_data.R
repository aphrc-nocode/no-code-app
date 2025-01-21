##### ---- Data source ------------------------------------------####

combine_data_combine_type = function() {
## 	observeEvent(input$manage_data_apply, {
## 		rv_current$transform_data_select_vars = renderUI({
## 		  p(
## 			 HTML(paste0("<b>", get_rv_labels("transform_data_select_vars"), "</b>"))
## 			 , selectInput('transform_data_select_vars'
## 				, label = NULL
## 				, rv_current$selected_vars
## 				, selectize=FALSE
## 				, multiple=FALSE
## 				, selected = ""
## 				, size = min(10, length(rv_current$selected_vars))
## 				, width = "100%"
## 			 )
## 		  )
## 		})
## 		output$transform_data_select_vars = rv_current$transform_data_select_vars
## 	})
	observeEvent(input$manage_data_apply, {
		output$combine_data_source_choices = renderUI({
			selectInput("combine_data_source_choices"
				, label = "Source of data"#get_rv_labels("recode_variable_old_label") 
				, choices = c("Internal", "External")
				, multiple=FALSE
			)
		})
	})

	observeEvent(input$manage_data_apply, {
		print(rv_current$merge_data_title_merge)
		print(rv_current$manage_data_title_transform)

	})
}


