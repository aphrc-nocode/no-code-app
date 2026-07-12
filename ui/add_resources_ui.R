# ui/add_resources_ui.R


feedback_form_ui <- function() {
  
  tabItem(
    tabName = "feedbackForm",
    
    div(
      style = "padding: 25px;",
      
      div(
        style = "
          background: #ffffff;
          border-radius: 20px;
          padding: 28px;
          box-shadow: 0 10px 30px rgba(0,0,0,0.08);
          border: 1px solid #e8eef7;
          width: 100%;
        ",
        
        uiOutput("feedback_form_title_ui"),
        uiOutput("feedback_form_description_ui"),
        
        tags$hr(),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("feedback_role_ui")
          ),
          column(
            width = 6,
            uiOutput("feedback_use_frequency_ui")
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("feedback_module_ui")
          ),
          column(
            width = 6,
            uiOutput("feedback_task_ui")
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("feedback_ease_ui")
          ),
          column(
            width = 6,
            uiOutput("feedback_expected_results_ui")
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("feedback_speed_ui")
          ),
          column(
            width = 6,
            uiOutput("feedback_recommend_ui")
          )
        ),
        
        uiOutput("feedback_issues_ui"),
        
        uiOutput("feedback_issue_description_ui"),
        uiOutput("feedback_additional_comments_ui"),
        
        fluidRow(
          column(
            width = 6,
            uiOutput("feedback_contact_permission_ui")
          ),
          column(
            width = 6,
            uiOutput("feedback_email_ui")
          )
        ),
        
        br(),
        
        actionButton(
          inputId = "submit_feedback",
          label = NULL,
          icon = icon("paper-plane"),
          class = "btn-primary",
          style = "
            padding: 12px 22px;
            border-radius: 12px;
            font-weight: 700;
            background: #2c7be5;
            border: none;
          "
        ),
        
        br(),
        br(),
        
        uiOutput("feedback_status_ui")
      )
    )
  )
}


user_manual_ui <- function() {
  
  tabItem(
    tabName = "userManual",
    
    div(
      style = "padding: 25px;",
      
      div(
        style = "
          background: #ffffff;
          border-radius: 20px;
          padding: 28px;
          box-shadow: 0 10px 30px rgba(0,0,0,0.08);
          border: 1px solid #e8eef7;
        ",
        
        uiOutput("user_manual_title_ui"),
        uiOutput("user_manual_description_ui"),
        
        tags$iframe(
          src = "manuals/user_manual.pdf",
          width = "100%",
          height = "850px",
          style = "
            border: none;
            border-radius: 16px;
          ",
          loading = "lazy"
        ),
        
        br(),
        
        uiOutput("user_manual_button_ui")
      )
    )
  )
}


add_resources_translation <- function(output, session) {
  
  lbl <- function(key) {
    
    value <- tryCatch(
      get_rv_labels(key),
      error = function(e) NULL
    )
    
    if (
      is.null(value) ||
      length(value) == 0 ||
      is.na(value[1]) ||
      !nzchar(trimws(as.character(value[1])))
    ) {
      return(key)
    }
    
    as.character(value[1])
  }
  
  
  output$feedback_form_title_ui <- renderUI({
    
    h2(
      icon("comment-dots"),
      lbl("feedback_form_title"),
      style = "
        font-weight: 800;
        color: #243447;
        margin-top: 0;
      "
    )
  })
  
  
  output$feedback_form_description_ui <- renderUI({
    
    p(
      lbl("feedback_form_description"),
      style = "
        font-size: 16px;
        color: #5f6b7a;
      "
    )
  })
  
  
  output$feedback_role_ui <- renderUI({
    
    selectInput(
      inputId = "feedback_role",
      label = lbl("feedback_role"),
      choices = c(
        lbl("feedback_role_researcher"),
        lbl("feedback_role_data_scientist"),
        lbl("feedback_role_statistician"),
        lbl("feedback_role_epidemiologist"),
        lbl("feedback_role_public_health"),
        lbl("feedback_role_monitoring_evaluation"),
        lbl("feedback_role_student"),
        lbl("feedback_role_policy_maker"),
        lbl("feedback_other")
      ),
      selected = character(0),
      width = "100%"
    )
  })
  
  
  output$feedback_use_frequency_ui <- renderUI({
    
    selectInput(
      inputId = "feedback_use_frequency",
      label = lbl("feedback_use_frequency"),
      choices = c(
        lbl("feedback_frequency_first_time"),
        lbl("feedback_frequency_occasionally"),
        lbl("feedback_frequency_monthly"),
        lbl("feedback_frequency_weekly"),
        lbl("feedback_frequency_daily")
      ),
      selected = character(0),
      width = "100%"
    )
  })
  
  
  output$feedback_module_ui <- renderUI({
    
    checkboxGroupInput(
      inputId = "feedback_module",
      label = lbl("feedback_module"),
      choices = c(
        lbl("feedback_module_data_management"),
        lbl("feedback_module_data_visualization"),
        lbl("feedback_module_data_anonymization"),
        lbl("feedback_module_omop"),
        lbl("feedback_module_research_questions"),
        lbl("feedback_module_machine_learning"),
        lbl("feedback_module_deep_learning"),
        lbl("feedback_module_overall"),
        lbl("feedback_other")
      ),
      selected = character(0),
      width = "100%"
    )
  })
  
  
  output$feedback_task_ui <- renderUI({
    
    textAreaInput(
      inputId = "feedback_task",
      label = lbl("feedback_task"),
      placeholder = lbl("feedback_your_answer"),
      width = "100%",
      height = "120px"
    )
  })
  
  
  output$feedback_ease_ui <- renderUI({
    
    tagList(
      tags$label(
        lbl("feedback_ease"),
        class = "control-label"
      ),
      
      div(
        style = "
          display: flex;
          justify-content: space-between;
          margin-bottom: 4px;
          color: #5f6b7a;
          font-size: 13px;
        ",
        span(lbl("feedback_ease_very_difficult")),
        span(lbl("feedback_ease_very_easy"))
      ),
      
      sliderInput(
        inputId = "feedback_ease",
        label = NULL,
        min = 1,
        max = 5,
        value = 3,
        step = 1,
        width = "100%"
      )
    )
  })
  
  
  output$feedback_expected_results_ui <- renderUI({
    
    radioButtons(
      inputId = "feedback_expected_results",
      label = lbl("feedback_expected_results"),
      choices = c(
        lbl("feedback_expected_completely"),
        lbl("feedback_expected_mostly"),
        lbl("feedback_expected_partially"),
        lbl("feedback_expected_not_at_all")
      ),
      selected = character(0)
    )
  })
  
  
  output$feedback_speed_ui <- renderUI({
    
    tagList(
      tags$label(
        lbl("feedback_speed"),
        class = "control-label"
      ),
      
      div(
        style = "
          display: flex;
          justify-content: space-between;
          margin-bottom: 4px;
          color: #5f6b7a;
          font-size: 13px;
        ",
        span(lbl("feedback_speed_very_poor")),
        span(lbl("feedback_speed_excellent"))
      ),
      
      sliderInput(
        inputId = "feedback_speed",
        label = NULL,
        min = 1,
        max = 5,
        value = 3,
        step = 1,
        width = "100%"
      )
    )
  })
  
  
  output$feedback_issues_ui <- renderUI({
    
    checkboxGroupInput(
      inputId = "feedback_issues",
      label = lbl("feedback_issues"),
      choices = c(
        lbl("feedback_issue_slow_loading"),
        lbl("feedback_issue_errors"),
        lbl("feedback_issue_incorrect_outputs"),
        lbl("feedback_issue_visualization"),
        lbl("feedback_issue_navigation"),
        lbl("feedback_issue_missing_functionality"),
        lbl("feedback_issue_none"),
        lbl("feedback_other")
      ),
      selected = character(0),
      width = "100%"
    )
  })
  
  
  output$feedback_issue_description_ui <- renderUI({
    
    textAreaInput(
      inputId = "feedback_issue_description",
      label = lbl("feedback_issue_description"),
      placeholder = lbl("feedback_your_answer"),
      width = "100%",
      height = "130px"
    )
  })
  
  
  output$feedback_recommend_ui <- renderUI({
    
    tagList(
      tags$label(
        lbl("feedback_recommend"),
        class = "control-label"
      ),
      
      div(
        style = "
          display: flex;
          justify-content: space-between;
          margin-bottom: 4px;
          color: #5f6b7a;
          font-size: 13px;
        ",
        span(lbl("feedback_recommend_not_likely")),
        span(lbl("feedback_recommend_extremely_likely"))
      ),
      
      sliderInput(
        inputId = "feedback_recommend",
        label = NULL,
        min = 1,
        max = 5,
        value = 3,
        step = 1,
        width = "100%"
      )
    )
  })
  
  output$feedback_additional_comments_ui <- renderUI({
    
    textAreaInput(
      inputId = "feedback_additional_comments",
      label = lbl("feedback_additional_comments"),
      placeholder = lbl("feedback_your_answer"),
      width = "100%",
      height = "130px"
    )
  })
  
  output$feedback_contact_permission_ui <- renderUI({
    
    radioButtons(
      inputId = "feedback_contact_permission",
      label = lbl("feedback_contact_permission"),
      choices = c(
        lbl("yes"),
        lbl("no")
      ),
      selected = character(0)
    )
  })
  
  
  output$feedback_email_ui <- renderUI({
    
    textInput(
      inputId = "feedback_email",
      label = lbl("feedback_email"),
      placeholder = lbl("feedback_your_answer"),
      width = "100%"
    )
  })
  
  
  observe({
    
    updateActionButton(
      session = session,
      inputId = "submit_feedback",
      label = lbl("submit_feedback")
    )
  })
  
  
  output$user_manual_title_ui <- renderUI({
    
    h2(
      icon("book-open"),
      lbl("user_manual_title"),
      style = "
        font-weight: 800;
        color: #243447;
        margin-top: 0;
      "
    )
  })
  
  
  output$user_manual_description_ui <- renderUI({
    
    p(
      lbl("user_manual_description"),
      style = "
        font-size: 16px;
        color: #5f6b7a;
      "
    )
  })
  
  
  output$user_manual_button_ui <- renderUI({
    
    tags$a(
      lbl("open_user_manual"),
      href = "manuals/user_manual.pdf",
      target = "_blank",
      style = "
        display: inline-block;
        margin-top: 18px;
        padding: 12px 20px;
        border-radius: 12px;
        background: #2c7be5;
        color: white;
        text-decoration: none;
        font-weight: 700;
      "
    )
  })
}