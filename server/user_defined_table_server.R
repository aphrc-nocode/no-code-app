user_defined_table_server <- function(input, output, session, rv_current, plots_custom_rv) {
  
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  
  select_placeholder <- function(label = rv_label("select_variable")) {
    out <- list("")
    names(out) <- label
    out
  }

  default_rv_choices <- function(key) {
    defaults <- list(
      user_output_type = c(Chart = "Chart", Table = "Table"),
      yes_no_boolean = c(Yes = "TRUE", No = "FALSE"),
      user_report_numeric = c(Mean = "mean", Median = "median"),
      user_numeric_summary = c(`Standard deviation` = "sd", `Min-max` = "min-max")
    )
    defaults[[key]] %||% character(0)
  }

  rv_choices <- function(key) {
    if (exists("get_rv_choices", mode = "function", inherits = TRUE)) {
      vals <- get_rv_choices(key)
      if (!is.null(vals) && length(vals) > 0) return(vals)
    }
    if (exists("get_named_choices", mode = "function", inherits = TRUE) &&
        exists("input_choices_file", inherits = TRUE) &&
        !is.null(input$change_language)) {
      vals <- get_named_choices(input_choices_file, input$change_language, key)
      if (!is.null(vals) && length(vals) > 0) return(vals)
    }
    default_rv_choices(key)
  }


  is_blank <- function(x) {
    is.null(x) || length(x) == 0 || identical(x, "") || (length(x) == 1 && is.na(x))
  }
  
  non_blank <- function(x) !is_blank(x)
  
  safe_scalar <- function(x, default = NULL) {
    if (is.null(x) || length(x) == 0) return(default)
    x1 <- x[1]
    if (is.na(x1) || identical(x1, "")) return(default)
    x1
  }
  
  safe_bool <- function(x, default = FALSE) {
    if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) return(default)
    
    if (is.logical(x)) {
      if (length(x) == 0 || is.na(x[1])) return(default)
      return(isTRUE(x[1]))
    }
    
    x_chr <- tolower(trimws(as.character(x[1])))
    if (x_chr %in% c("true", "t", "1", "yes", "y")) return(TRUE)
    if (x_chr %in% c("false", "f", "0", "no", "n")) return(FALSE)
    
    default
  }
  
  keep_valid_single <- function(selected, choices) {
    if (is.null(selected) || length(selected) == 0 || selected == "" || !selected %in% choices) "" else selected
  }
  
  keep_valid_multi <- function(selected, choices) {
    if (is.null(selected) || length(selected) == 0) character(0) else intersect(selected, choices)
  }
  
  rv_table <- reactiveValues(
    table_ready = FALSE,
    table_error = NULL,
    table_status = rv_label("choose_table_variables_to_begin"),
    last_request_key = NULL
  )
  
  observe({
    shinyjs::runjs("
      var cssId = 'custom-table-smooth-panels';
      if (!document.getElementById(cssId)) {
        var style = document.createElement('style');
        style.id = cssId;
        style.innerHTML = `
          #tabmoreoption {
            overflow: hidden;
            max-height: 0;
            opacity: 0;
            transform: translateY(-4px);
            transition: max-height 0.35s ease, opacity 0.25s ease, transform 0.25s ease;
            pointer-events: none;
          }
          #tabmoreoption.open-panel {
            max-height: 74vh;
            opacity: 1;
            transform: translateY(0);
            overflow-y: auto;
            overflow-x: hidden;
            padding-right: 8px;
            pointer-events: auto;
          }
        `;
        document.head.appendChild(style);
      }
    ")
  })

  observe({
    is_chart <- identical(input$cboOutput %||% "Chart", "Chart")
    
    if (is_chart) {
      try(shinyjs::hide("tableOptionsPanel"), silent = TRUE)
      try(shinyjs::hide("tableTopPanel"), silent = TRUE)
      try(shinyjs::hide("tableStatusPanel"), silent = TRUE)
      shinyjs::hide("tabOutputs")
      shinyjs::hide("tabSummaries")
      shinyjs::hide("tabmore")
      close_tab_more()
    } else {
      try(shinyjs::show("tableOptionsPanel"), silent = TRUE)
      try(shinyjs::show("tableTopPanel"), silent = TRUE)
      try(shinyjs::show("tableStatusPanel"), silent = TRUE)
      shinyjs::show("tabOutputs")
      shinyjs::show("tabSummaries")
      
      if (!isTRUE(is_chart) && !is.null(rv_current$working_df)) {
        shinyjs::show("tabmore")
      } else {
        shinyjs::hide("tabmore")
        close_tab_more()
      }
    }
  })
  
  is_chart_mode <- reactive({
    identical(input$cboOutput %||% "Chart", "Chart")
  })
  
  has_table_available <- reactive({
    !isTRUE(is_chart_mode()) &&
      isTRUE(rv_table$table_ready) &&
      !is.null(plots_custom_rv$tab_rv)
  })
  
  table_has_minimum_inputs <- reactive({
    selected_vars <- input$cboCalcVar %||% character(0)
    selected_vars <- selected_vars[selected_vars != ""]
    length(selected_vars) >= 1 && length(selected_vars) <= 5
  })
  
  close_tab_more <- function() {
    if ("tabmore" %in% names(input) && isTRUE(input$tabmore)) {
      updateSwitchInput(session, "tabmore", value = FALSE)
    }
    shinyjs::runjs("$('#tabmoreoption').removeClass('open-panel');")
  }
  
  output$user_tab_more_out <- renderUI({
    if (!isTRUE(is_chart_mode()) &&
        isTRUE(rv_table$table_ready) &&
        !is.null(plots_custom_rv$tab_rv)) {
      shinyWidgets::switchInput(
        inputId = "tabmore",
        label = NULL,
        value = isTRUE(input$tabmore),
        onLabel = rv_label("hide_details"),
        offLabel = rv_label("show_more_details"),
        onStatus = "success",
        offStatus = "default",
        size = "normal",
        labelWidth = "90px",
        handleWidth = "35px"
      )
    } else {
      NULL
    }
  })
  
  output$custom_table_status <- renderUI({
    div(style = "margin: 6px 0 10px 0; color:#5f6b76;", rv_table$table_status)
  })
  
  observeEvent(rv_current$working_df, {
    req(rv_current$working_df)
    nms <- names(rv_current$working_df)
    
    updateSelectInput(
      session,
      "cboColVar",
      choices = c(select_placeholder(rv_label("select_variable")), nms),
      selected = ""
    )
    
    updateSelectInput(
      session,
      "cboCalcVar",
      choices = c(select_placeholder(rv_label("select_variable")), nms),
      selected = character(0)
    )
    
    rv_table$table_ready <- FALSE
    rv_table$table_error <- NULL
    rv_table$table_status <- rv_label("choose_table_variables_to_begin")
    plots_custom_rv$tab_rv <- NULL
    
    close_tab_more()
  }, ignoreInit = FALSE)
  
  observe({
    selected_vars <- input$cboCalcVar %||% character(0)
    selected_vars <- selected_vars[selected_vars != ""]
    if (length(selected_vars) == 0 || length(selected_vars) > 5) {
      shinyjs::disable("btnCreatetable")
    } else {
      shinyjs::enable("btnCreatetable")
    }
  })
  
  observeEvent(input$cboCalcVar, {
    req(isTRUE(!is.null(rv_current$working_df)))
    
    valid_choices <- setdiff(names(rv_current$working_df), input$cboCalcVar %||% character(0))
    updateSelectInput(
      session,
      "cboColVar",
      choices = c(select_placeholder(rv_label("select_variable")), valid_choices),
      selected = keep_valid_single(input$cboColVar, valid_choices)
    )
    
    selected_vars <- input$cboCalcVar %||% character(0)
    selected_vars <- selected_vars[selected_vars != ""]
    if (length(selected_vars) > 5) {
      showModal(modalDialog(
        h3(get_rv_labels("max_var_limit")),
        style = "text-align: center;color: red",
        h5(get_rv_labels("max_var_limit_description")),
        footer = tagList(actionButton("dismissBtn", rv_label("close")))
      ))
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$cboColVar, {
    req(isTRUE(!is.null(rv_current$working_df)))
    
    valid_choices <- setdiff(names(rv_current$working_df), input$cboColVar %||% "")
    updateSelectInput(
      session,
      "cboCalcVar",
      choices = c(select_placeholder(rv_label("select_variable")), valid_choices),
      selected = keep_valid_multi(input$cboCalcVar, valid_choices)
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$dismissBtn, {
    removeModal()
    updateSelectInput(session, "cboCalcVar", selected = character(0))
  }, ignoreInit = TRUE)
  
 
  
  observe({
    if (!isTRUE(is_chart_mode()) && !is.null(rv_current$working_df)) {
      shinyjs::show("tabmore")
      shinyjs::show("tabmoreoption")
      
      if (isTRUE(input$tabmore)) {
        shinyjs::runjs("$('#tabmoreoption').addClass('open-panel');")
      } else {
        shinyjs::runjs("$('#tabmoreoption').removeClass('open-panel');")
      }
      
    } else {
      shinyjs::hide("tabmore")
      shinyjs::hide("tabmoreoption")
      shinyjs::runjs("$('#tabmoreoption').removeClass('open-panel');")
    }
  })
  
  observe({
    has_data <- !is.null(rv_current$working_df)
    
    if (has_data) {
      try(shinyjs::show("tableOptionsPanel"), silent = TRUE)
      try(shinyjs::show("tableTopPanel"), silent = TRUE)
    } else {
      try(shinyjs::hide("tableOptionsPanel"), silent = TRUE)
      try(shinyjs::hide("tableTopPanel"), silent = TRUE)
      try(shinyjs::hide("tableStatusPanel"), silent = TRUE)
      shinyjs::hide("tabOutputs")
      shinyjs::hide("tabSummaries")
      close_tab_more()
    }
  })
  
  validate_crosstab_inputs <- function(df, vars, by = NULL) {
    vars <- vars[vars != ""]
    
    if (length(vars) == 0) {
      return(list(ok = FALSE, msg = rv_label("please_select_at_least_one_table_variable")))
    }
    
    if (length(vars) > 5) {
      return(list(ok = FALSE, msg = rv_label("select_maximum_5_cross_tab_variables")))
    }
    
    valid_names <- names(df)
    if (!all(vars %in% valid_names)) {
      return(list(ok = FALSE, msg = rv_label("selected_variables_do_not_exist")))
    }
    
    if (!is.null(by) && by != "" && !by %in% valid_names) {
      return(list(ok = FALSE, msg = rv_label("selected_grouping_variable_does_not_exist")))
    }
    
    if (!is.null(by) && by != "" && by %in% vars) {
      return(list(ok = FALSE, msg = rv_label("grouping_variable_must_be_different")))
    }
    
    supported_var <- function(x) {
      is.factor(x) || is.character(x) || is.logical(x) ||
        inherits(x, "Date") || is.numeric(x) || is.integer(x)
    }
    
    if (!all(sapply(df[, vars, drop = FALSE], supported_var))) {
      return(list(ok = FALSE, msg = rv_label("selected_variables_not_supported_cross_tab")))
    }
    
    if (!is.null(by) && by != "" && !supported_var(df[[by]])) {
      return(list(ok = FALSE, msg = rv_label("grouping_variable_not_supported_cross_tab")))
    }
    
    list(ok = TRUE, msg = NULL)
  }
  
  current_table_request_key <- reactive({
    list(
      vars = sort((input$cboCalcVar %||% character(0))[(input$cboCalcVar %||% character(0)) != ""]),
      by = input$cboColVar %||% "",
      add_p = safe_bool(input$rdoAddTabPValue, FALSE),
      add_ci = safe_bool(input$rdoAddTabCI, FALSE),
      report_numeric = safe_scalar(input$chkReportNumeric, "mean"),
      numeric_summary = safe_scalar(input$chkNumericSummary, "sd"),
      drop_na = safe_bool(input$rdoDropTabMissingValues, TRUE),
      caption = safe_scalar(input$txtTabCaption, ""),
      df_nrow = if (is.null(rv_current$working_df)) 0 else nrow(rv_current$working_df),
      df_names = if (is.null(rv_current$working_df)) character(0) else names(rv_current$working_df)
    )
  })
  
  build_table <- function() {
    req(isTRUE(!is.null(rv_current$working_df)))
    
    df <- rv_current$working_df
    selected_vars <- input$cboCalcVar %||% character(0)
    selected_vars <- selected_vars[selected_vars != ""]
    by_var <- input$cboColVar %||% ""
    if (identical(by_var, "")) by_var <- NULL
    
    validation <- validate_crosstab_inputs(df, selected_vars, by_var)
    if (!isTRUE(validation$ok)) {
      return(list(ok = FALSE, msg = validation$msg, table = NULL))
    }
    
    tryCatch({
      tab <- Rautoml::custom_crosstab(
        df = df,
        vars = selected_vars,
        by = by_var,
        add.p = safe_bool(input$rdoAddTabPValue, FALSE),
        add.ci = safe_bool(input$rdoAddTabCI, FALSE),
        report_numeric = safe_scalar(input$chkReportNumeric, "mean"),
        numeric_summary = safe_scalar(input$chkNumericSummary, "sd"),
        drop_na = safe_bool(input$rdoDropTabMissingValues, TRUE),
        caption = safe_scalar(input$txtTabCaption, "")
      )
      
      final_tab <- if (inherits(tab, "flextable")) {
        tab
      } else {
        tryCatch(as_flex_table(tab), error = function(e) NULL)
      }
      
      if (is.null(final_tab)) {
        list(
          ok = FALSE,
          msg = rv_label("cross_tab_generated_not_rendered"),
          table = NULL
        )
      } else {
        list(ok = TRUE, msg = NULL, table = final_tab)
      }
    }, error = function(e) {
      list(ok = FALSE, msg = paste(rv_label("cross_tab_could_not_be_generated"), e$message), table = NULL)
    })
  }
  
  observeEvent(
    list(
      input$cboOutput,
      input$cboCalcVar,
      input$cboColVar,
      input$rdoAddTabPValue,
      input$rdoAddTabCI,
      input$chkReportNumeric,
      input$chkNumericSummary,
      input$rdoDropTabMissingValues,
      input$txtTabCaption,
      rv_current$working_df
    ),
    {
      if (!isTRUE(!is.null(rv_current$working_df))) {
        plots_custom_rv$tab_rv <- NULL
        rv_table$table_ready <- FALSE
        rv_table$table_error <- NULL
        rv_table$table_status <- rv_label("choose_table_variables_to_begin")
        rv_table$last_request_key <- NULL
        close_tab_more()
        return()
      }
      
      if (!isTRUE(table_has_minimum_inputs())) {
        if (!isTRUE(is_chart_mode())) {
          plots_custom_rv$tab_rv <- NULL
          rv_table$table_ready <- FALSE
          rv_table$table_error <- NULL
          rv_table$table_status <- rv_label("choose_at_least_one_table_variable")
          rv_table$last_request_key <- NULL
          close_tab_more()
        }
        return()
      }
      
      request_key <- current_table_request_key()
      same_request <- identical(rv_table$last_request_key, request_key)
      
      if (isTRUE(is_chart_mode())) {
        if (isTRUE(rv_table$table_ready) && !is.null(plots_custom_rv$tab_rv) && same_request) {
          rv_table$table_status <- rv_label("table_ready")
        }
        close_tab_more()
        return()
      }
      
      if (isTRUE(rv_table$table_ready) && !is.null(plots_custom_rv$tab_rv) && same_request) {
        rv_table$table_status <- rv_label("table_ready")
        return()
      }
      
      rv_table$table_status <- rv_label("generating_table")
      
      shiny::withProgress(
        message = rv_label("generating_table"),
        detail = rv_label("building_cross_tabulation"),
        value = 0,
        {
          shiny::incProgress(0.25)
          result <- build_table()
          shiny::incProgress(0.95)
          
          if (isTRUE(result$ok) && !is.null(result$table)) {
            plots_custom_rv$tab_rv <- result$table
            rv_table$table_ready <- TRUE
            rv_table$table_error <- NULL
            rv_table$table_status <- rv_label("table_ready")
            rv_table$last_request_key <- request_key
          } else {
            plots_custom_rv$tab_rv <- NULL
            rv_table$table_ready <- FALSE
            rv_table$table_error <- result$msg
            rv_table$table_status <- result$msg %||% rv_label("table_could_not_be_generated")
            rv_table$last_request_key <- NULL
            close_tab_more()
          }
        }
      )
    },
    ignoreInit = FALSE
  )
  
  output$tabSummaries <- renderUI({
    if (isTRUE(rv_table$table_ready) && !is.null(plots_custom_rv$tab_rv)) {
      div(
        style = "overflow-x:auto; width:100%;",
        flextable::htmltools_value(plots_custom_rv$tab_rv)
      )
    } else if (!isTRUE(is_chart_mode()) && non_blank(rv_table$table_error)) {
      div(
        style = "color:#d9534f; background:#fff5f5; border:1px solid #f5c2c7; padding:12px; border-radius:6px; margin-top:10px;",
        strong(rv_label("cross_tabulation_error")),
        span(rv_table$table_error)
      )
    } else {
      NULL
    }
  })
  
  output$btnDownloadTable <- downloadHandler(
    filename = function() {
      paste0("summary_table_", format(Sys.Date(), "%B %d %Y"), ".docx")
    },
    content = function(file) {
      req(plots_custom_rv$tab_rv)
      doc <- officer::read_docx() %>%
        flextable::body_add_flextable(value = plots_custom_rv$tab_rv)
      print(doc, target = file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
}