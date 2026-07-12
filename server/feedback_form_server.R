# server/feedback_form_server.R

feedback_form_server <- function(input, output, session, USER = NULL) {
  
  feedback_status <- reactiveVal(NULL)
  submission_in_progress <- reactiveVal(FALSE)
  
  
  # Retrieve translated labels from the platform translation system.
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
  
  
  # Safely convert input values to a single character value.
  safe_character <- function(value) {
    
    if (is.null(value) || length(value) == 0) {
      return("")
    }
    
    value <- as.character(value[1])
    
    if (is.na(value)) {
      return("")
    }
    
    trimws(value)
  }
  
  
  output$feedback_status_ui <- renderUI({
    
    status <- feedback_status()
    
    if (is.null(status)) {
      return(NULL)
    }
    
    if (identical(status$type, "success")) {
      
      div(
        style = "
          padding: 14px 18px;
          border-radius: 12px;
          background: #e8f7ee;
          color: #176b3a;
          font-weight: 700;
          border: 1px solid #bce5c8;
        ",
        icon("circle-check"),
        status$message
      )
      
    } else if (identical(status$type, "warning")) {
      
      div(
        style = "
          padding: 14px 18px;
          border-radius: 12px;
          background: #fff8e1;
          color: #8a5a00;
          font-weight: 700;
          border: 1px solid #f0d98c;
        ",
        icon("triangle-exclamation"),
        status$message
      )
      
    } else {
      
      div(
        style = "
          padding: 14px 18px;
          border-radius: 12px;
          background: #fdecea;
          color: #b42318;
          font-weight: 700;
          border: 1px solid #f5c2c0;
        ",
        icon("circle-exclamation"),
        status$message
      )
    }
  })
  
  
  # Disable the submit button while a submission is being processed.
  observe({
    
    shinyjs::toggleState(
      id = "submit_feedback",
      condition = !submission_in_progress()
    )
  })
  
  
  observeEvent(input$submit_feedback, {
    
    if (submission_in_progress()) {
      return()
    }
    
    feedback_status(NULL)
    
    
    # Read and clean selected form values.
    task <- safe_character(input$feedback_task)
    
    contact_permission <- safe_character(
      input$feedback_contact_permission
    )
    
    email_address <- safe_character(
      input$feedback_email
    )
    
    
    # Validate required task description.
    if (!nzchar(task)) {
      
      feedback_status(list(
        type = "error",
        message = lbl("feedback_required_message")
      ))
      
      return()
    }
    
    
    # Validate email when the user asks to be contacted.
    yes_label <- lbl("yes")
    
    if (
      identical(contact_permission, yes_label) &&
      !nzchar(email_address)
    ) {
      
      feedback_status(list(
        type = "error",
        message = lbl("feedback_email_required_message")
      ))
      
      return()
    }
    
    
    # Validate email format when an email is entered.
    if (
      nzchar(email_address) &&
      !grepl(
        "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
        email_address
      )
    ) {
      
      feedback_status(list(
        type = "error",
        message = lbl("feedback_invalid_email_message")
      ))
      
      return()
    }
    
    
    submission_in_progress(TRUE)
    
    on.exit(
      submission_in_progress(FALSE),
      add = TRUE
    )
    
    
    # Local feedback storage directory.
    feedback_dir <- file.path("feedback")
    
    if (!dir.exists(feedback_dir)) {
      
      dir.create(
        feedback_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
    }
    
    
    feedback_file <- file.path(
      feedback_dir,
      "feedback_responses.csv"
    )
    
    
    # Logged-in username.
    username <- tryCatch({
      
      if (!is.null(USER) && !is.null(USER$username)) {
        safe_character(USER$username)
      } else {
        ""
      }
      
    }, error = function(e) {
      ""
    })
    
    
    # Current application language.
    current_language <- tryCatch({
      
      safe_character(rv_lang$selected_language)
      
    }, error = function(e) {
      ""
    })
    
    
    # Current active application tab.
    active_tab <- tryCatch({
      
      safe_character(input$tabs)
      
    }, error = function(e) {
      ""
    })
    
    
    # Multi-select issue responses.
    issues <- input$feedback_issues
    
    if (is.null(issues) || length(issues) == 0) {
      
      issues <- ""
      
    } else {
      
      issues <- paste(
        as.character(issues),
        collapse = "; "
      )
    }
    
    
    # Create one submission row.
    feedback_row <- data.frame(
      
      timestamp = format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S %Z"
      ),
      
      username = username,
      
      language = current_language,
      
      active_tab = active_tab,
      
      role = safe_character(
        input$feedback_role
      ),
      
      use_frequency = safe_character(
        input$feedback_use_frequency
      ),
      
      module = safe_character(
        input$feedback_module
      ),
      
      task = task,
      
      ease_of_use = as.integer(
        input$feedback_ease
      ),
      
      expected_results = safe_character(
        input$feedback_expected_results
      ),
      
      speed_responsiveness = as.integer(
        input$feedback_speed
      ),
      
      issues_experienced = issues,
      
      issue_description = safe_character(
        input$feedback_issue_description
      ),
      
      recommend_likelihood = as.integer(
        input$feedback_recommend
      ),
      
      contact_permission = contact_permission,
      
      email_address = email_address,
      
      session_token = safe_character(
        session$token
      ),
      
      stringsAsFactors = FALSE
    )
    
    
    # Save a local backup before sending to Google Sheets.
    local_save_success <- FALSE
    
    tryCatch({
      
      if (file.exists(feedback_file)) {
        
        existing <- read.csv(
          feedback_file,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          fileEncoding = "UTF-8"
        )
        
        expected_columns <- names(feedback_row)
        
        # If an older CSV has a different structure, preserve it as a backup
        # and start a new file using the current structure.
        if (!identical(names(existing), expected_columns)) {
          
          backup_name <- file.path(
            feedback_dir,
            paste0(
              "feedback_responses_old_",
              format(Sys.time(), "%Y%m%d_%H%M%S"),
              ".csv"
            )
          )
          
          file.copy(
            feedback_file,
            backup_name,
            overwrite = FALSE
          )
          
          existing <- data.frame()
        }
        
        
        combined <- if (nrow(existing) > 0) {
          rbind(existing, feedback_row)
        } else {
          feedback_row
        }
        
        
        write.csv(
          combined,
          feedback_file,
          row.names = FALSE,
          na = "",
          fileEncoding = "UTF-8"
        )
        
      } else {
        
        write.csv(
          feedback_row,
          feedback_file,
          row.names = FALSE,
          na = "",
          fileEncoding = "UTF-8"
        )
      }
      
      local_save_success <- TRUE
      
    }, error = function(e) {
      
      warning(
        paste(
          lbl("feedback_local_save_warning"),
          conditionMessage(e)
        )
      )
    })
    
    
    # Append the same submission to Google Sheets.
    sheet_result <- tryCatch({
      
      append_feedback_to_google_sheet(
        feedback_row
      )
      
      list(
        success = TRUE,
        error = NULL
      )
      
    }, error = function(e) {
      
      warning(
        paste(
          lbl("feedback_google_sheet_warning"),
          conditionMessage(e)
        )
      )
      
      list(
        success = FALSE,
        error = conditionMessage(e)
      )
    })
    
    
    # Handle Google Sheets failure.
    if (!sheet_result$success) {
      
      if (local_save_success) {
        
        feedback_status(list(
          type = "warning",
          message = paste(
            lbl("feedback_sheet_sync_warning"),
            sheet_result$error
          )
        ))
        
      } else {
        
        feedback_status(list(
          type = "error",
          message = paste(
            lbl("feedback_error_message"),
            sheet_result$error
          )
        ))
      }
      
      return()
    }
    
    
    # Reset form fields after successful submission.
    updateSelectInput(
      session,
      "feedback_role",
      selected = character(0)
    )
    
    updateSelectInput(
      session,
      "feedback_use_frequency",
      selected = character(0)
    )
    
    updateSelectInput(
      session,
      "feedback_module",
      selected = character(0)
    )
    
    updateTextAreaInput(
      session,
      "feedback_task",
      value = ""
    )
    
    updateSliderInput(
      session,
      "feedback_ease",
      value = 3
    )
    
    updateRadioButtons(
      session,
      "feedback_expected_results",
      selected = character(0)
    )
    
    updateSliderInput(
      session,
      "feedback_speed",
      value = 3
    )
    
    updateCheckboxGroupInput(
      session,
      "feedback_issues",
      selected = character(0)
    )
    
    updateTextAreaInput(
      session,
      "feedback_issue_description",
      value = ""
    )
    
    updateSliderInput(
      session,
      "feedback_recommend",
      value = 3
    )
    
    updateRadioButtons(
      session,
      "feedback_contact_permission",
      selected = character(0)
    )
    
    updateTextInput(
      session,
      "feedback_email",
      value = ""
    )
    
    
    feedback_status(list(
      type = "success",
      message = lbl("feedback_success_message")
    ))
  })
}