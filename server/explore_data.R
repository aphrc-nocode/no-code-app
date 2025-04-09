#### ---- Helper function to alter the contents of UI rendered ---------- #####

alter_renderUI_title = function() {
		observeEvent(input$manage_data_apply, {
			rv_current$manage_data_title_transform = alter_renderUI(ui=rv_current$manage_data_title_transform, session=session, replacement="<hr/>")
			output$manage_data_title_transform = rv_current$manage_data_title_transform
			
			rv_current$visualize_data_title=rv_current$combine_data_title = alter_renderUI(ui=rv_current$combine_data_title, session=session, replacement="<hr/>")
			output$combine_data_title = rv_current$combine_data_title
			output$visualize_data_title = rv_current$visualize_data_title
		})
}

#### ---- Explore data tab ----------------------------------------------- ####

explore_data_server = function() {
	observeEvent(input$manage_data_apply, {
		req(input$manage_data_apply)
		if (isTRUE(!is.null(input$dataset_id))) {
			 rv_current$visualize_data_title=rv_current$combine_data_title=rv_current$manage_data_title_transform=rv_current$manage_data_title_explore = renderUI({
				p(
					  HTML(
						 c(paste0("<b>", get_rv_labels("you_selected"), ":</b> <br/>")
							, rv_current$metadata_id
							, paste0("<br/>", get_rv_labels("you_can_select"), " <b>Manage data > Overview</b><br/>")
						 )
					)
					, hr()
					, HTML("<b>", get_rv_labels("toggle_actions"), ": </b>")
				)
			})
			output$manage_data_title_explore = rv_current$manage_data_title_explore
			alter_renderUI_title()
		}
	})

	observe({
		if (NROW(rv_metadata$upload_logs)) {
			if (is.null(input$dataset_id)) {
				rv_current$visualize_data_title=rv_current$combine_data_title=rv_current$manage_data_title_transform=rv_current$manage_data_title_explore = renderText(
					paste0(get_rv_labels("no_data_selected"),  " <b>Manage data > Overview</b>")
				)
				output$manage_data_title_explore = rv_current$manage_data_title_explore
				output$manage_data_title_transform = rv_current$manage_data_title_transform
				output$combine_data_title = rv_current$combine_data_title
				output$visualize_data_title = rv_current$visualize_data_title
			} 
		} else {
			rv_current$visualize_data_title=rv_current$combine_data_title=rv_current$manage_data_title_transform=rv_current$manage_data_title_explore = renderText(
				paste0(get_rv_labels("no_data"), "<b> Source data </b>")
			)
			output$manage_data_title_explore = rv_current$manage_data_title_explore
			output$manage_data_title_transform = rv_current$manage_data_title_transform
			output$combine_data_title = rv_current$combine_data_title
			output$visualize_data_title = rv_current$visualize_data_title
		}
	})

	observeEvent(c(input$change_language, input$dataset_id), {
		rv_current$visualize_data_title=rv_current$combine_data_title=rv_current$manage_data_title_transform=rv_current$manage_data_title_explore = renderText(
			paste0(get_rv_labels("no_data_selected"),  " <b>Manage data > Overview</b>")
		)
		output$manage_data_title_explore = rv_current$manage_data_title_explore
		output$manage_data_title_transform = rv_current$manage_data_title_transform
		output$combine_data_title = rv_current$combine_data_title
		output$visualize_data_title = rv_current$visualize_data_title
	})
}


#### ----- Explore data actions ------------------------------------------ ####

explore_data_actions_server = function() {
	observeEvent(input$manage_data_apply, {
	  output$explore_data_missingness = renderUI({
		 materialSwitch(
			inputId = "explore_data_missingness_check",
			label = get_rv_labels("missingness_check"), 
			status = "success",
			right = TRUE
		 )
	  })
	  output$explore_data_filter = renderUI({
		 materialSwitch(
			inputId = "explore_data_filter_check",
			label = get_rv_labels("filter_check"), 
			status = "success",
			right = TRUE
		 )
	  })
	  output$explore_data_show_data = renderUI({
		 materialSwitch(
			inputId = "explore_data_show_data_check",
			label = get_rv_labels("show_data_check"), 
			status = "success",
			right = TRUE
		 )
	  })
	  output$explore_data_select_variables = renderUI({
		 materialSwitch(
			inputId = "explore_data_select_variables_check",
			label = get_rv_labels("select_variables_check"), 
			status = "success",
			right = TRUE
		 )
	  })
	  output$explore_data_quick_explore = renderUI({
		 materialSwitch(
			inputId = "explore_data_quick_explore_check",
			label = get_rv_labels("quick_explore_check"), 
			status = "success",
			right = TRUE
		 )
	  })
	
	})
}

explore_data_subactions_server = function() {
	observe({
	  if (isTRUE(input$explore_data_show_data_check)) {
		  output$explore_data_show_data_type = renderUI({
				prettyRadioButtons(
					inputId = "explore_data_show_data_type_check"
						, label = NULL
						, choices = get_named_choices(input_choices_file, input$change_language, "explore_data_show_data_type_check")
						, inline = FALSE
						, status = "success"
				)
		  })
	  } else {
	     output$explore_data_show_data_type = NULL
	  }
	})
	
	observe({
	  if (isTRUE(input$explore_data_quick_explore_check)) {
	  	output$explore_data_quick_explore_ui = renderUI({
			p(hr()
				, HTML(paste0("<b>", get_rv_labels("quick_explore_ui"), "</b>"))
			)
		})
	  	output$explore_data_quick_explore_out = renderPrint({
			generate_data_summary(rv_current$working_df)
		})
	  } else {
	  	output$explore_data_quick_explore_ui = NULL
	  	output$explore_data_quick_explore_out = NULL
	  }
	})
}

explore_data_filter_server = function() {
	observe({
		if (isTRUE(input$explore_data_filter_check) & !is.null(input$explore_data_filter_check)) {
			output$explore_data_filter_rules = renderUI({
				textAreaInput("explore_data_filter_rules"
				  , label = NULL
				  , value = ""
				  , placeholder = paste0(get_rv_labels("filter_data_ph"), " var1=='M' & var2 >= 40")
				)
			})
		} else {
			output$explore_data_filter_rules = NULL
			output$manage_data_explore_filter_apply = NULL
			output$manage_data_explore_filter_reset = NULL
		}
	})
}

explore_data_apply_filter_server = function() {
		observe({
 			if (isTRUE(input$explore_data_filter_rules != "")) {
 				output$manage_data_explore_filter_apply = renderUI({
				p(
						actionBttn("manage_data_explore_filter_apply"
							, inline = TRUE
							, color = "success"
							, label=get_rv_labels("apply_filter")
						)
						, br()
					)
 				})
 			} else {
				output$manage_data_explore_filter_apply = NULL
 			}
		})
		
		observe({
 			if (isTRUE(input$explore_data_filter_rules != "") | isTRUE(!is.null(rv_current$current_filter))) {
				output$manage_data_explore_filter_reset = renderUI({
 					p(
						actionBttn("manage_data_explore_filter_reset"
							, inline = TRUE
							, color = "success"
							, label=get_rv_labels("reset_filter")
						)
						, br()
						, br()
					)
 				})
 			} else {
 				output$manage_data_explore_filter_reset = NULL
 			}
		})
}

#### ---- Filter data, based on the filter rules -------------
explore_data_current_filter_server = function() {
	observeEvent(input$manage_data_explore_filter_apply, {
		working_df = try(filter_data(rv_current$working_df, input$explore_data_filter_rules), silent = TRUE)
		if (!is.data.frame(working_df) | is.null(working_df) | any(class(working_df) %in% "try-error") | !NROW(working_df)) {
			rv_current$working_df = rv_current$working_df
			shinyalert("", paste0(get_rv_labels("filter_data_error"), " <br><b>", input$explore_data_filter_rules, "</b></br>"), html=TRUE, type = "error", inputId="filter_data_error")
		} else {
			rv_current$working_df = working_df
			rv_current$current_filter = c(rv_current$current_filter, input$explore_data_filter_rules)
			output$data_explore_filter_applied = renderUI({
				 p(
					HTML(paste0("<b>", get_rv_labels("current_filter"), ": </b>"))
				 )
			})
		  	output$data_explore_filter_applied_out = renderPrint({
				cat(rv_current$current_filter, sep = "\n")
			})
			rv_current$current_filter_reset = FALSE
		  	updateTextAreaInput(session, "explore_data_filter_rules", value = "")
		}
	})
	observe({
		if (isTRUE(input$filter_data_error)) {
			updateTextAreaInput(session, "explore_data_filter_rules", value = "")
		}
	})
}

explore_data_reset_current_filter_server = function() {
	observeEvent(input$manage_data_explore_filter_reset, {
		rv_current$current_filter = NULL
		rv_current$working_df = rv_current$data
		output$data_explore_filter_applied = NULL
		output$data_explore_filter_applied_out = NULL
		rv_current$current_filter_reset = NULL
	})
}

#### ---- Display uploaded data --------------------------------------####
explore_show_data_server = function() {
	observe({
		if (isTRUE(input$explore_data_show_data_check) & isTRUE(!is.null(rv_current$working_df))) {
			output$current_dataset_text = renderUI({
				 p(
					HTML(paste0("<b>", get_rv_labels("current_dataset_text"), ": </b>"))
				 )
			})
			if (isTRUE(input$explore_data_show_data_type_check=="Head")) {
				output$working_df = renderPrint({
					head(rv_current$working_df)
				})
			} else if (isTRUE(input$explore_data_show_data_type_check=="Tail")) {
				output$working_df = renderPrint({
					tail(rv_current$working_df)
				})
			} else {
 				output$working_df_all = DT::renderDT(rv_current$working_df
 					, escape = FALSE
 					, rownames = FALSE
 					, options = list(processing = FALSE
 						, autoWidth = TRUE
 						, scrollX = TRUE
 						, columnDefs = list(list(className = 'dt-center', targets='_all'))
 					)
 				)
			}
		} else {
			output$current_dataset_text = NULL
			output$working_df = NULL
			output$working_df_all = NULL
		}
	})
}

##### ---- Compute proportion of missing data ------------------------
explore_missing_data_server = function() {
	observe({
		if (isTRUE(input$explore_data_missingness_check) & isTRUE(!is.null(rv_current$working_df))) {
			rv_current$missing_prop = missing_prop(rv_current$working_df)
			if (isTRUE(rv_current$current_filter!="") & isTRUE(!is.null(rv_current$current_filter))) {
				output$explore_missing_data_out = renderUI({
					p(
						hr()
						, HTML("<b>", get_rv_labels("missing_proportion"), "</b>")
					)
				})
				output$explore_missing_data = renderPrint({
					rv_current$missing_prop
				})
			} else {
				output$explore_missing_data_out = renderUI({
					p(
						HTML("<b>", get_rv_labels("missing_proportion"), "</b>")
					)
				})
				output$explore_missing_data = renderPrint({
					rv_current$missing_prop
				})
				
			}
		} else {
			output$explore_missing_data_out = NULL
			output$explore_missing_data = NULL
		}
	})
}

##### ---- Select variables ------------------------------------------####
explore_data_select_variables_server = function() {
	observe({
		if (isTRUE(input$explore_data_select_variables_check)) {
			output$manage_data_select_vars = renderUI({
			  p(
			  	hr()
				 , HTML(paste0("<b>", get_rv_labels("manage_data_select_vars"), "</b>"))
				 , helpText(get_rv_labels("manage_data_select_vars_ht"))
				 , selectInput('manage_data_select_vars'
					, label = NULL
					, rv_current$selected_vars
					, selectize=FALSE
					, multiple=TRUE
					, size = min(10, length(rv_current$selected_vars))
					, width = "100%"
				 )
			  )
			})
		} else {
			output$manage_data_select_vars = NULL
			rv_current$selected_vars = colnames(rv_current$data)
		}
	})
}

### FIXME: Is there an efficient way for this?
explore_data_selected_variables_server = function() {
	observeEvent(c(input$manage_data_select_vars, input$explore_data_select_variables_check), {
		df = rv_current$data
		if (isTRUE(!is.null(rv_current$current_filter)) & !isTRUE(rv_current$current_filter_reset)) {
			for (f in 1:length(rv_current$current_filter)) {
				df = filter_data(df, rv_current$current_filter[f])
			}
		}
		if (isTRUE(!is.null(input$manage_data_select_vars)) & isTRUE(any(input$manage_data_select_vars %in% rv_current$selected_vars)) & isTRUE(input$explore_data_select_variables_check)) {
  			df = dplyr::select(df, input$manage_data_select_vars)
 			rv_current$working_df = df
  		}
		rv_current$working_df = df
	})
}

explore_data_update_data_server = function() {
	observe({
		if ((isTRUE(!is.null(input$manage_data_select_vars)) & isTRUE(input$explore_data_select_variables_check)) | (isTRUE(!is.null(rv_current$current_filter))) & isTRUE(input$explore_data_filter_check)) {
			output$explore_data_update_data = renderUI({
				p(
					hr()
				 , helpText(get_rv_labels("update_overwrite_ht"))
				 , actionBttn(
				 	inputId = "explore_data_update_data_apply"
						, label = get_rv_labels("update_overwrite")
						, style = "jelly"
						, color = "warning"
						, inline=TRUE
					)
				)
			})
		} else {
			output$explore_data_update_data = NULL
		}
	})

	observeEvent(input$explore_data_update_data_apply, {
		write_data(get_data_class(paste0("datasets/", rv_current$dataset_id)), rv_current$working_df)
		rv_current$data = rv_current$working_df
		rv_current$selected_vars = colnames(rv_current$data)
		## Update logs
		log_file_main = paste0(".log_files/", rv_current$dataset_id, "-upload.main.log")
		meta_data = read.csv(log_file_main)
		meta_data$last_modified = format_date_time(Sys.time())
		meta_data$observations = NROW(rv_current$data)
		meta_data$features = NCOL(rv_current$data)
		meta_data$size = object.size(rv_current$data)
		write.csv(meta_data, log_file_main, row.names = FALSE)
		upload_logs_current = collect_logs(".log_files", "*.upload.main.log")
		rv_metadata$upload_logs = upload_logs_current
		rv_metadata$upload_logs$delete = create_btns(rv_metadata$upload_logs$file_name)
		write.table(rv_metadata$upload_logs, file=".log_files/.automl-shiny-upload.main.log", row.names = FALSE)
		updateSelectInput(session=session, "manage_data_select_vars", choices=colnames(rv_current$data))
		rv_current$current_filter_reset = TRUE
		shinyalert("", get_rv_labels("updated_overwriten"), type = "success", inputId="manage_data_explore_update_data_alert")
	})
}
