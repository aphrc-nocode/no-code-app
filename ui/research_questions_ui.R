research_questions_ui = function() {
	tabItem(tabName = "researchQuestions"
		, fluidRow(
			column(width = 3
				, htmlOutput("research_questions_title")
				, uiOutput("generate_research_questions_outcome")
				, uiOutput("generate_research_questions_outcome_selected")
				, uiOutput("generate_research_questions_choices")
				, uiOutput("generate_research_questions_api_token")
				, uiOutput("generate_research_questions_api_token_apply")
				, uiOutput("generate_research_questions_apply")
				, htmlOutput("generate_research_questions_additional")
				, uiOutput("generate_research_questions_additional_analysis_ui")
			)
			, column(width = 9
				, htmlOutput("generate_research_questions_gemini") 
				, htmlOutput("generate_research_question_gemini_suggest_analysis")
			)
		)
	)
}
