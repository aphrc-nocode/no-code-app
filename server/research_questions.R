##### ---- Generate research question ------------------ ####

generate_research_questions_choices = function() {
	
	observeEvent(input$manage_data_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			output$generate_research_questions_choices = renderUI({
				  radioButtons("generate_research_questions_choices"
						, label = get_rv_labels("generate_research_questions_choices")
						, choices = get_named_choices(input_choices_file, input$change_language, "generate_research_questions_choices")
						, selected = character(0)
						, inline = TRUE
				  )
			})
		}
	})

	## Reset in case no data
	observe({
		if (isTRUE(is.null(rv_current$working_df))) {
			output$generate_research_questions_choices = NULL
			updateRadioButtons(session, "generate_research_questions_choices", selected=character(0))
		}
	})
}


##### ---- API Token ------------------ ####

generate_research_questions_api_token = function() {
	observe({
		if (isTRUE(input$generate_research_questions_choices=="yes")) {
			if (isTRUE(Rautoml::check_api("GEMINE_API_KEY"))) {
			 	shinyalert("", get_rv_labels("api_stored_ok"), type = "success", inputId="api_stored_ok")
				output$generate_research_questions_api_token = NULL
			} else {
				output$generate_research_questions_api_token = renderUI({
					p(
						hr()
						, HTML(paste0("<b>", get_rv_labels("generate_research_questions_api_token"), "</b>"))
						, helpText(paste0(get_rv_labels("generate_research_questions_api_token_ht")), " ", a(get_rv_labels("generate_research_questions_api_token_ht_h"), href="https://aistudio.google.com/app/apikey", target="_blank"))
						, passwordInput("generate_research_questions_api_token"
							, label = NULL 
							, value = ""
							, width = "100%"
							, placeholder = get_rv_labels("generate_research_questions_api_token_ph")
						)
					)
				})	
				output$generate_research_questions_api_token_apply = renderUI({
					actionBttn("generate_research_questions_api_token_apply"
						, inline=TRUE
						, block = FALSE
						, color = "success"
						, label = get_rv_labels("generate_research_questions_api_token_apply"))
				})
			}
		} else {
			output$generate_research_questions_api_token_apply = NULL
			output$generate_research_questions_api_token = NULL
		}
	})
}


##### ---- Store API Token ------------------ ####

generate_research_questions_api_store = function() {
	observeEvent(input$generate_research_questions_api_token_apply, {
		if (!isTRUE(Rautoml::check_api("GEMINE_API_KEY"))) {
			if (isTRUE(input$generate_research_questions_api_token != "")) {
				Rautoml::set_api("GEMINE_API_KEY", input$generate_research_questions_api_token)
			 	shinyalert("", get_rv_labels("api_stored_success"), type = "success", inputId="api_stored_success")
			}
		}
	})
}


#### ---- Addional prompts --------------- ####

generate_research_questions_additional = function() {
	observe({
		if (isTRUE(isTRUE(input$generate_research_questions_choices=="yes"))) {
			if (isTRUE(Rautoml::check_api("GEMINE_API_KEY"))) {
				output$generate_research_questions_additional = renderUI({
					p(
						HTML("<b>", get_rv_labels("generate_research_questions_additional"), ": </b>")
					)
				})
			  
			  output$generate_research_questions_additional_analysis_ui = renderUI({
				 materialSwitch(
					inputId = "generate_research_questions_additional_analysis",
					label = get_rv_labels("generate_research_questions_additional_analysis"), 
					status = "success",
					right = TRUE
				 )
			  })

				output$generate_research_questions_apply = renderUI({
					actionBttn("generate_research_questions_apply"
						, inline=TRUE
						, block = FALSE
						, color = "success"
						, label = get_rv_labels("generate_research_questions_apply"))
				})
			} else {
				output$generate_research_questions_additional = NULL
				output$generate_research_questions_additional_analysis_ui = NULL
				updateMaterialSwitch(session, inputId="generate_research_questions_additional_analysis", value = FALSE)
				output$generate_research_questions_apply = NULL
			}
		} else {
			output$generate_research_questions_additional = NULL
			output$generate_research_questions_additional_analysis_ui = NULL
			updateMaterialSwitch(session, inputId="generate_research_questions_additional_analysis", value = FALSE)
			output$generate_research_questions_apply = NULL
		}
	})
}


#### ---- Generate insights using Gemini --------------- ####

generate_research_questions_gemini = function() {
	observeEvent(input$generate_research_questions_apply, {
		if (isTRUE(isTRUE(input$generate_research_questions_choices=="yes"))) {
			if (isTRUE(Rautoml::check_api("GEMINE_API_KEY"))) {
				if (isTRUE(is.null(rv_current$quick_explore_summary))) {
					rv_current$quick_explore_summary = generate_data_summary(rv_current$working_df)
				}
				explore_logs = Rautoml::create_prompts(
					desc=get_prompts("generate_research_questions_gemini_base")
					, log=Rautoml::extract_list_logs(rv_current$quick_explore_summary)
					, add_info=get_prompts("generate_research_questions_gemini_add")
				)
				research_question_chat = tryCatch({
					gemini.R::gemini_chat(explore_logs
						, maxOutputTokens = rv_current$max_tockens
					)
				}, error=function(e){
					list(outputs = "Error occured"
						, history = ""
					)
				})
				output$generate_research_questions_gemini = renderUI({
					p(br()
						, h2(get_rv_labels("generate_research_questions"))
						, hr()
						, shiny::markdown(research_question_chat$outputs)
					)
				})
			}
		}
	})
}

