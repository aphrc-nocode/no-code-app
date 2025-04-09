#### ---- Functions to reset various componets of the app ----------------- ####


reset_data_server = function() {
	observeEvent(c(input$change_language, input$dataset_id), {
		rv_current$data = NULL
		rv_current$dataset_id = NULL
		rv_current$metadata_id = NULL
		rv_current$selected_vars = NULL
		rv_current$working_df = NULL
		rv_current$current_filter = NULL
		output$data_explore_filter_applied = NULL
		output$data_summary = NULL
		rv_current$data_summary_str = NULL
		rv_current$data_summary_skim = NULL
		rv_current$data_summary_summary = NULL
		rv_current$vartype = NULL
		output$data_summary_summarytools = NULL
		output$manage_data_title = NULL
		output$manage_data_show = NULL
		output$explore_data_missingness = NULL
		rv_current$missing_prop = NULL
		output$transform_data_handle_missing_values = NULL
		output$explore_data_filter = NULL
		output$explore_data_show_data = NULL
		output$explore_data_select_variables = NULL
		output$explore_data_quick_explore = NULL
		output$explore_data_filter_rules = NULL
		output$manage_data_explore_filter_apply = NULL
		output$manage_data_explore_filter_reset = NULL
		output$manage_data_select_vars = NULL
		output$explore_data_show_data_type = NULL
		output$explore_data_quick_explore_ui = NULL
      output$explore_data_quick_explore_out = NULL
		output$explore_data_update_data = NULL
		rv_current$transform_data_select_vars = NULL
		rv_current$selected_var = NULL
		output$transform_data_select_vars = NULL
		output$transform_data_change_type = NULL
		output$transform_data_change_type_choices = NULL
		output$transform_data_variable_type = NULL
		output$transform_data_variable_type_ui = NULL
		rv_current$changed_variable_type_log = NULL
		output$transform_data_variable_type_log = NULL
		output$transform_data_variable_type_log_ui = NULL
		rv_current$transform_data_quick_explore_out = NULL
		output$transform_data_quick_explore_ui = NULL
		output$transform_data_quick_explore_out = NULL
		output$transform_data_quick_plot_out = NULL
		output$transform_data_rename_variable = NULL
		output$renamed_variable_log = NULL
		rv_current$vartype_change_check = NULL
		rv_current$transform_data_plot_df = NULL
		output$transform_data_renamed_variable_log_ui = NULL
		output$transform_data_renamed_variable_log = NULL
		rv_current$recoded_variable_labels_log = NULL
		output$transform_data_recoded_variable_labels_log_ui = NULL
		output$transform_data_recoded_variable_labels_log = NULL
		rv_current$missing_prop_df = NULL
		output$transform_data_handle_missing_values_choices = NULL
		updateSelectInput(session = session, "transform_data_select_vars", selected = "", choices = NULL)
		output$transform_data_identify_outliers = NULL
		updateMaterialSwitch(session=session, "transform_data_identify_outliers_check", value=FALSE)
		rv_current$outlier_values = NULL
		output$transform_data_handle_outliers_log_ui = NULL
		output$transform_data_handle_outliers_log = NULL
		updateSelectInput(session = session, inputId = "transform_data_handle_outliers_choices", selected = "")
		output$transform_data_handle_outliers_choices = NULL
		output$transform_data_handle_outliers_correct_options = NULL
		updateRadioButtons(session = session, inputId = "transform_data_handle_outliers_correct_options", selected = character(0))
		output$transform_data_handle_outliers_correct_options_input = NULL
		updateNumericInput(session = session, inputId = "transform_data_handle_outliers_correct_options_input", value = NULL)

		updateMaterialSwitch(session=session, "transform_data_create_missing_values_check", value=FALSE)
		updateSelectInput(session = session, inputId = "transform_data_create_missing_values_choices", selected = "")
		updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input", selected = NULL)
		updateRadioButtons(session = session, inputId = "transform_data_create_missing_values_options", selected = character(0))
		updateTextInput(session = session, inputId = "transform_data_create_missing_values_input", value = "")
		updateNumericRangeInput(session = session, inputId = "transform_data_create_missing_values_input", value = NULL)
		updateCheckboxInput(session = session, inputId = "transform_data_create_missing_values_input_range", value = FALSE)
		updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input_numeric", selected = NULL)

	
		updateCheckboxInput(session = session, "transform_data_recode_variable_check", value = FALSE)
		updateTextAreaInput(session=session, "transform_data_recode_variable_input", value=NULL)

		output$transform_data_handle_missing_values = NULL
		updateMaterialSwitch(session=session, "transform_data_handle_missing_values_check", value=FALSE)
		output$transform_data_handle_missing_values_ui = NULL
		output$transform_data_handle_missing_values_out = NULL
         
		output$transform_data_handle_missing_values_choices = NULL      
		updateSelectInput(session = session, "transform_data_handle_missing_values_choices", selected = "")

		output$combine_data_matched_vars = NULL
		rv_current$combine_df = NULL 
		rv_current$combine_data_selected_vars = NULL
		updateSelectInput(session = session, "combine_data_list_datasets", selected = "", choices = NULL)
			
		output$generate_research_questions_choices = NULL
		updateRadioButtons(session, "generate_research_questions_choices", selected=character(0))
		output$generate_research_questions_gemini = NULL
		output$generate_research_question_gemini_suggest_analysis = NULL

		rv_generative_ai$history = NULL
	})

	observeEvent(c(input$current_id, input$non_data_ok), {
		if (isTRUE(gsub("ytxxdeletezzyt_", "", input$current_id)==rv_current$dataset_id)) {
			rv_current$data = NULL
			rv_current$dataset_id = NULL
			output$dataset_id = NULL
			rv_current$current_id = NULL
			rv_current$metadata_id = NULL
			rv_current$selected_vars = NULL
			rv_current$working_df = NULL
			rv_current$current_filter = NULL
			rv_current$vartype = NULL
			output$data_explore_filter_applied = NULL
			output$manage_data_title = NULL
			output$data_summary = NULL
			output$data_summary_summarytools = NULL
			rv_current$data_summary_summarytools = NULL
			rv_current$data_summary_str = NULL
			rv_current$data_summary_skim = NULL
			output$manage_data_show = NULL
			output$manage_data_title_explore = NULL
			output$explore_data_missingness = NULL
			rv_current$missing_prop = NULL
			output$transform_data_handle_missing_values = NULL
			output$explore_data_select_variables = NULL
			output$explore_data_quick_explore = NULL
			output$explore_data_filter = NULL
			output$explore_data_show_data = NULL
			output$explore_data_filter_rules = NULL
			output$manage_data_explore_filter_apply = NULL
			output$manage_data_explore_filter_reset = NULL
			output$manage_data_select_vars = NULL
			output$explore_data_show_data_type = NULL
			output$explore_data_quick_explore_ui = NULL
			output$explore_data_quick_explore_out = NULL
			output$manage_data_title_transform = NULL
			rv_current$manage_data_title_transform = NULL
			output$explore_data_update_data = NULL
			rv_current$transform_data_select_vars = NULL
			output$transform_data_select_vars = NULL
			rv_current$selected_var = NULL
			output$transform_data_change_type = NULL
			output$transform_data_change_type_choices = NULL
         output$transform_data_variable_type = NULL
         output$transform_data_variable_type_ui = NULL
			rv_current$changed_variable_type_log = NULL
			output$transform_data_variable_type_log = NULL
			output$transform_data_variable_type_log_ui = NULL
			rv_current$transform_data_quick_explore_out = NULL
			output$transform_data_quick_explore_ui = NULL
			output$transform_data_quick_explore_out = NULL
			output$transform_data_quick_plot_out = NULL
			output$transform_data_rename_variable = NULL
			output$renamed_variable_log = NULL
			rv_current$vartype_change_check = NULL
			rv_current$transform_data_plot_df = NULL
			output$transform_data_renamed_variable_log_ui = NULL
			output$transform_data_renamed_variable_log = NULL
			rv_current$recoded_variable_labels_log = NULL
			output$transform_data_recoded_variable_labels_log_ui = NULL
			output$transform_data_recoded_variable_labels_log = NULL
			rv_current$missing_prop_df = NULL
			output$transform_data_handle_missing_values_choices = NULL
			updateSelectInput(session = session, "transform_data_select_vars", selected = "", choices = NULL)
			output$transform_data_identify_outliers = NULL
			updateMaterialSwitch(session=session, "transform_data_identify_outliers_check", value=FALSE)
			rv_current$missing_prop_df = NULL
			output$transform_data_handle_missing_values_choices = NULL
			updateSelectInput(session = session, "transform_data_select_vars", selected = "", choices = NULL)
			output$transform_data_identify_outliers = NULL
			updateMaterialSwitch(session=session, "transform_data_identify_outliers_check", value=FALSE)
			rv_current$outlier_values = NULL
			output$transform_data_handle_outliers_log_ui = NULL
			output$transform_data_handle_outliers_log = NULL
			updateSelectInput(session = session, inputId = "transform_data_handle_outliers_choices", selected = "")
			output$transform_data_handle_outliers_choices = NULL
			output$transform_data_handle_outliers_correct_options = NULL
			updateRadioButtons(session = session, inputId = "transform_data_handle_outliers_correct_options", selected = character(0))
			output$transform_data_handle_outliers_correct_options_input = NULL
			updateNumericInput(session = session, inputId = "transform_data_handle_outliers_correct_options_input", value = NULL)
		
			updateMaterialSwitch(session=session, "transform_data_create_missing_values_check", value=FALSE)
			updateSelectInput(session = session, inputId = "transform_data_create_missing_values_choices", selected = "")
			updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input", selected = NULL)
			updateRadioButtons(session = session, inputId = "transform_data_create_missing_values_options", selected = character(0))
			updateTextInput(session = session, inputId = "transform_data_create_missing_values_input", value = "")
			updateNumericRangeInput(session = session, inputId = "transform_data_create_missing_values_input", value = NULL)
			updateCheckboxInput(session = session, inputId = "transform_data_create_missing_values_input_range", value = FALSE)
			updateSelectInput(session = session, inputId = "transform_data_create_missing_values_input_numeric", selected = NULL)

			updateCheckboxInput(session = session, "transform_data_recode_variable_check", value = FALSE)
         updateTextAreaInput(session=session, "transform_data_recode_variable_input", value=NULL)
			
			output$transform_data_handle_missing_values = NULL
			updateMaterialSwitch(session=session, "transform_data_handle_missing_values_check", value=FALSE)
			output$transform_data_handle_missing_values_ui = NULL
			output$transform_data_handle_missing_values_out = NULL

         output$transform_data_handle_missing_values_choices = NULL      
         updateSelectInput(session = session, "transform_data_handle_missing_values_choices", selected = "")
		
			output$combine_data_matched_vars = NULL
			rv_current$combine_df = NULL 
			rv_current$combine_data_selected_vars = NULL
			updateSelectInput(session = session, "combine_data_list_datasets", selected = "", choices = NULL)
			
			output$generate_research_questions_choices = NULL
			updateRadioButtons(session, "generate_research_questions_choices", selected=character(0))
			output$generate_research_questions_gemini = NULL
			output$generate_research_question_gemini_suggest_analysis = NULL
			
			rv_generative_ai$history = NULL


			if (NROW(rv_metadata$upload_logs)) {
				rv_current$manage_data_title_explore = renderText(
            	paste0(get_rv_labels("no_data_selected"),  " <b>Manage data > Overview</b>")
         	)
				output$manage_data_title_transform = output$manage_data_title_explore = rv_current$manage_data_title_transform = rv_current$manage_data_title_explore
			}
		}
	})

	observe({
		if (isTRUE(!is.null(rv_current$selected_var))) {
			if (!isTRUE(input$transform_data_create_missing_values_check)) {
				updateRadioButtons(session = session, inputId = "transform_data_create_missing_values_options", selected = character(0))
				updateSelectInput(session = session, inputId = "transform_data_create_missing_values_choices", selected = "")
				updateTextInput(session = session, inputId = "transform_data_create_missing_values_input", value = NULL)
			}
		}
	})
}
