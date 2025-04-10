#### ---- AI/ML UIs ---------------------------- ####
setup_models_ui = function() {

	## Analytics type
	observeEvent(input$manage_data_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			rv_current$vartype_all = Rautoml::get_types(rv_current$working_df)

			output$setup_models_analysis_type = renderUI({
				empty_lab = ""
				names(empty_lab) = get_rv_labels("setup_models_analysis_type_ph")
				selectInput("setup_models_analysis_type"
					, label = get_rv_labels("setup_models_analysis_type") 
					, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_choices"))
					, multiple=FALSE
				)
			})
		} else {
			output$setup_models_analysis_type = NULL
			updateSelectInput(session, "setup_models_analysis_type", selected="")
		}
	})


	## ML/AI tasks
	observe({
		if (isTRUE(!is.null(rv_current$working_df)) & isTRUE(!is.null(input$setup_models_analysis_type))) {
			if (isTRUE(input$setup_models_analysis_type=="Supervised learning")) {
				output$setup_models_analysis_type_specifics = renderUI({
					empty_lab = ""
					names(empty_lab) = get_rv_labels("setup_models_analysis_type_ph")
					selectInput("setup_models_analysis_type_specifics"
						, label = get_rv_labels("setup_models_analysis_type_supervised") 
						, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_supervised_choices"))
						, multiple=FALSE
					)
				})
			} else if (isTRUE(input$setup_models_analysis_type=="Unsupervised learning")) {
				output$setup_models_analysis_type_specifics = renderUI({
					empty_lab = ""
					names(empty_lab) = get_rv_labels("setup_models_analysis_type_ph")
					selectInput("setup_models_analysis_type_specifics"
						, label = get_rv_labels("setup_models_analysis_type_unsupervised") 
						, choices = c(empty_lab, get_named_choices(input_choices_file, input$change_language,"setup_models_analysis_type_unsupervised_choices"))
						, multiple=FALSE
					)
				})			
			} else if (isTRUE(input$setup_models_analysis_type=="Time series")) {
				# FIXME: HERE
				 shinyalert("", get_rv_labels("future_analysis_type_ts"), type = "info", inputId="future_analysis_type_ts")
				updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
				output$setup_models_analysis_type_specifics = NULL
			} else if (isTRUE(input$setup_models_analysis_type=="Inferential statistics")) {
				# FIXME: HERE
				 shinyalert("", get_rv_labels("future_analysis_type_inference"), type = "info", inputId="future_analysis_type_inference")
				updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
				output$setup_models_analysis_type_specifics = NULL
			} else {
				updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
				output$setup_models_analysis_type_specifics = NULL
			}
		}
	})

	## Target/Outcome variable
	observe({
		if (isTRUE(input$setup_models_analysis_type_specifics=="Regression")) {
			if (isTRUE(length(rv_current$vartype_all$numeric)>0)) {
				output$setup_models_analysis_target_variable = renderUI({
					empty_lab = ""
					names(empty_lab) = get_rv_labels("setup_models_analysis_target_variable_ph")
					selectInput("setup_models_analysis_target_variable"
						, label = get_rv_labels("setup_models_analysis_target_variable") 
						, choices = c(empty_lab, rv_current$vartype_all$numeric)
						, multiple=FALSE
					)
				})
			} else {
				shinyalert("", get_rv_labels("setup_models_analysis_target_numeric_alert"), type = "info", inputId="setup_models_analysis_target_numeric_alert")
			}
		} else if (isTRUE(input$setup_models_analysis_type_specifics=="Classification")){
			if (isTRUE(length(rv_current$vartype_all$categorical)>0)) {
				output$setup_models_analysis_target_variable = renderUI({
					empty_lab = ""
					names(empty_lab) = get_rv_labels("setup_models_analysis_target_variable_ph")
					selectInput("setup_models_analysis_target_variable"
						, label = get_rv_labels("setup_models_analysis_target_variable") 
						, choices = c(empty_lab, rv_current$vartype_all$categorical)
						, multiple=FALSE
					)
				})
			} else {
				shinyalert("", get_rv_labels("setup_models_analysis_target_categorical_alert"), type = "info", inputId="setup_models_analysis_target_categorical_alert")
			}	
		} else {
			updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
			output$setup_models_analysis_target_variable=NULL
		}
	})
	
	## Train/test ratio
	observe({
		if (isTRUE(!is.null(input$setup_models_analysis_target_variable)) & isTRUE(input$setup_models_analysis_target_variable!="")) {
			output$setup_models_analysis_partition_ratio = renderUI({
					sliderInput("setup_models_analysis_partition_ratio"
						, get_rv_labels("setup_models_analysis_partition_ratio")
						, min = 0
						, max = 1
						, value = 0.75
						, step = NULL
					)
			})

			output$setup_models_analysis_session_name = renderUI({
				textInput("setup_models_analysis_session_name"
					, label = get_rv_labels("setup_models_analysis_session_name")
					, value = ""
					, width = NULL
					, placeholder = get_rv_labels("setup_models_analysis_session_name_ph")
				)	
			})
		} else {
			updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
			output$setup_models_analysis_partition_ratio = NULL
			output$setup_models_analysis_session_name = NULL
			updateTextInput(session , "setup_models_analysis_session_name", value="")
		}
	})

	observe({
		if (isTRUE(!is.null(input$setup_models_analysis_session_name)) & isTRUE(input$setup_models_analysis_session_name!="")) {
				output$setup_models_analysis_apply = renderUI({
					actionBttn("setup_models_analysis_apply"
						, inline=TRUE
						, block = FALSE
						, color = "success"
						, label = get_rv_labels("setup_models_analysis_apply"))
				})
			
		} else {
			output$setup_models_analysis_apply = NULL	
		}
	})
	
	## Reset if task is updated
	observeEvent(input$setup_models_analysis_type, {
		updateSelectInput(session, "setup_models_analysis_type_specifics", selected="")
		output$setup_models_analysis_type_specifics = NULL
		
		updateSelectInput(session, "setup_models_analysis_target_variable", selected="")
		output$setup_models_analysis_target_variable=NULL
		
		updateSliderInput(session, "setup_models_analysis_partition_ratio", value = NULL)
		output$setup_models_analysis_partition_ratio = NULL
		
	})
}

#### ---- Preprocessing ------------------------ ####
