#### ----- Modelling framework --------------------------------- ####

modelling_framework_choices = function() {
	observeEvent(input$setup_models_analysis_apply, {
		
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(input$setup_models_analysis_session_name!="") & isTRUE(!is.null(input$setup_models_analysis_session_name))) {
				if (isTRUE(length(input$setup_models_analysis_model_type_check)>0)) {
					output$modelling_framework_choices = renderUI({
						selectInput("modelling_framework_choices"
							, label = get_rv_labels("modelling_framework_choices")
							, choices =   get_named_choices(input_choices_file, input$change_language,"modelling_framework_choices")
							, multiple=FALSE
							, selected = "Caret"
						)
					})
				} else {
					output$modelling_framework_choices = NULL
					updateSelectInput(session, "modelling_framework_choices", selected="")

				}
			} else {
				output$modelling_framework_choices = NULL
				updateSelectInput(session, "modelling_framework_choices", selected="")
				
			}
		} else {
			output$modelling_framework_choices = NULL
			updateSelectInput(session, "modelling_framework_choices", selected="")
			
		}
		
	})
}
