##### ---- Generate research question ------------------ ####

generate_research_questions_choices = function() {
	
	observeEvent(input$manage_data_apply, {
		if (isTRUE(!is.null(rv_current$working_df))) {
			output$generate_research_questions_outcome = renderUI({
				  radioButtons("generate_research_questions_outcome"
						, label = get_rv_labels("generate_research_questions_outcome")
						, choices = get_named_choices(input_choices_file, input$change_language, "generate_research_questions_choices")
						, selected = character(0)
						, inline = TRUE
				  )
			})
		}
	})
	
	observe({
		if (isTRUE(!is.null(rv_current$working_df))) {
			if (isTRUE(length(input$generate_research_questions_outcome)>0)) {
				output$generate_research_questions_choices = renderUI({
					  radioButtons("generate_research_questions_choices"
							, label = get_rv_labels("generate_research_questions_choices")
							, choices = get_named_choices(input_choices_file, input$change_language, "generate_research_questions_choices")
							, selected = character(0)
							, inline = TRUE
					  )
				})
			} else {
				output$generate_research_questions_choices = NULL
				updateRadioButtons(session, "generate_research_questions_choices", selected=character(0))
			}
		}
	})

	observe({
		if (isTRUE(input$generate_research_questions_outcome=="yes")) {
 			output$generate_research_questions_outcome_selected = renderUI({
 				empty_lab = ""
 				names(empty_lab) = get_rv_labels("generate_research_questions_outcome_selected_ph")
 				selectInput(
 					inputId = "generate_research_questions_outcome_selected"
 					, label = NULL
 					, selected = ""
 					, choices = c(empty_lab, rv_current$selected_vars)
 				)
 			})
		} else {
			output$generate_research_questions_outcome_selected = NULL
			updateSelectInput(session, inputId = "generate_research_questions_outcome_selected", selected = "")
		}
	})

	observe({
		if (isTRUE(!is.null(input$generate_research_questions_outcome_selected)) & isTRUE(input$generate_research_questions_outcome_selected!="")) {
			rv_current$outcome = input$generate_research_questions_outcome_selected
		} else {
			rv_current$outcome = NULL
		}
	})

	## Reset in case no data
	observe({
		if (isTRUE(is.null(rv_current$working_df))) {
			output$generate_research_questions_choices = NULL
			updateRadioButtons(session, "generate_research_questions_choices", selected=character(0))
			output$generate_research_questions_outcome = NULL
			updateRadioButtons(session, "generate_research_questions_outcome", selected=character(0))
			rv_current$outcome = NULL
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

				output$generate_research_questions_apply = renderUI({
					actionBttn("generate_research_questions_apply"
						, inline=TRUE
						, block = FALSE
						, color = "success"
						, label = get_rv_labels("generate_research_questions_apply"))
				})
			} else {
				output$generate_research_questions_apply = NULL
			}
		} else {
			output$generate_research_questions_apply = NULL
		}
	})
	
	observeEvent(input$generate_research_questions_apply, {
		if (isTRUE(isTRUE(input$generate_research_questions_choices=="yes"))) {
			if (isTRUE(Rautoml::check_api("GEMINE_API_KEY"))) {
				output$generate_research_questions_additional = renderUI({
					p(
						br()
						, hr()
						, HTML("<b>", get_rv_labels("generate_research_questions_additional"), ": </b>")
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

			} else {
				output$generate_research_questions_additional = NULL
				output$generate_research_questions_additional_analysis_ui = NULL
				updateMaterialSwitch(session, inputId="generate_research_questions_additional_analysis", value = FALSE)
			}
		} else {
				output$generate_research_questions_additional = NULL
				output$generate_research_questions_additional_analysis_ui = NULL
				updateMaterialSwitch(session, inputId="generate_research_questions_additional_analysis", value = FALSE)
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

## 				 waiter_show( # show the waiter
## 					html = spin_loaders(id = 15, color = "green", style = "spin")
## 					, color = NULL
## 				 )
				showPageSpinner()
				if (isTRUE(!is.null(rv_current$outcome))) {
					outcome_prompt = paste0(get_prompts("generate_research_questions_outcome"), " ", rv_current$outcome)
				} else {
					outcome_prompt = ""
				}

				explore_logs = Rautoml::create_prompts(
					desc=get_prompts("generate_research_questions_gemini_base")
					, log=Rautoml::extract_list_logs(rv_current$quick_explore_summary)
					, add_info=paste0(get_prompts("generate_research_questions_gemini_add"), ".", outcome_prompt)
				)
				research_question_chat = tryCatch({
					gemini.R::gemini_chat(explore_logs
						, maxOutputTokens = rv_current$max_tockens
						, seed = rv_current$seed
					)
				}, error=function(e){
					list(outputs = get_rv_labels("generate_research_questions_error")
						, history = list()
					)
				})
				rv_generative_ai$history = research_question_chat$history
				output$generate_research_questions_gemini = renderUI({
					p(br()
						, h2(get_rv_labels("generate_research_questions"))
						, hr()
						, shiny::markdown(research_question_chat$outputs)
					)
				})
## 			 waiter_hide()
				hidePageSpinner()
			}
		}
	})

	observe({
		if (isTRUE(isTRUE(input$generate_research_questions_choices=="yes"))) {
			if (isTRUE(Rautoml::check_api("GEMINE_API_KEY"))) {
				if (isTRUE(input$generate_research_questions_additional_analysis)) {
## 				 waiter_show( # show the waiter
## 					html = spin_loaders(id = 15, color = "green", style = "spin")
## 					, color = NULL
## 				 )
						showPageSpinner()
						analysis_prompt = get_prompts("generate_research_questions_additional_analysis")
					suggest_analysis_chat = tryCatch({
						gemini.R::gemini_chat(prompt = analysis_prompt
							, history = rv_generative_ai$history
							, maxOutputTokens = rv_current$max_tockens
							, seed = rv_current$seed
						)
					}, error=function(e){
						list(outputs = get_rv_labels("generate_research_questions_error")
							, history = list()
						)
					})
					output$generate_research_question_gemini_suggest_analysis = renderUI({
						p(br()
							, h2(get_rv_labels("generate_research_questions_suggest_analysis"))
							, hr()
							, shiny::markdown(suggest_analysis_chat$outputs)
						)
					})
## 					waiter_hide()
					hidePageSpinner()
				}
				
			}
		}
	})
}

