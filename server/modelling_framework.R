#### ----- Modelling framework --------------------------------- ####

modelling_framework_choices = function() {
	observeEvent(input$setup_models_analysis_apply, {
		
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
				if (isTRUE(length(input$setup_models_analysis_model_type_check)>0)) {
					output$modelling_framework_choices = renderUI({
						choices =   get_named_choices(input_choices_file, input$change_language,"modelling_framework_choices")
						prettyRadioButtons(
							inputId = "modelling_framework_choices"
								, label = get_rv_labels("modelling_framework_choices")
								, choices = choices
								, selected = character(0)
								, status = "success"
						)
					})
				} else {
					output$modelling_framework_choices = NULL
					prettyRadioButtons(session=session, inputId="modelling_framework_choices", selected = character(0))
				}
			} else {
				output$modelling_framework_choices = NULL
				prettyRadioButtons(session=session, inputId="modelling_framework_choices", selected = character(0))
			}
		} else {
			output$modelling_framework_choices = NULL
			prettyRadioButtons(session=session, inputId="modelling_framework_choices", selected = character(0))
			
		}
		
	})
}
