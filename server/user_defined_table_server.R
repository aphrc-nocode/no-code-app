user_defined_table_server <- function(input, output, session, rv_current, plots_custom_rv) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  blank <- function(x) is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(as.character(x[1]))
  yes <- function(x, default = FALSE) {
    if (blank(x)) return(default)
    tolower(as.character(x[1])) %in% c("true", "t", "1", "yes", "y")
  }
  placeholder <- function(label = "Select variable") stats::setNames("", label)
  supported <- function(x) is.factor(x) || is.character(x) || is.logical(x) ||
    inherits(x, c("Date", "POSIXct", "POSIXt")) || is.numeric(x) || is.integer(x)

  state <- reactiveValues(
    ready = FALSE, generated = FALSE, error = NULL,
    base_table = NULL, cache = list(), cache_order = character(0),
    data_revision = 0L,
    status = "Choose table variables, then click Generate table."
  )
  mode <- reactive(as.character(input$cboTableMode %||% "summary")[1])
  is_table <- reactive(identical(input$cboOutput %||% "Chart", "Table"))
  all_names <- reactive({
    df <- rv_current$working_df
    if (is.null(df)) character(0) else names(df)[vapply(df, supported, logical(1))]
  })
  categorical_names <- reactive({
    df <- rv_current$working_df
    if (is.null(df)) return(character(0))
    nms <- names(df)[vapply(df, function(x) {
      is.factor(x) || is.character(x) || is.logical(x) ||
        ((is.numeric(x) || is.integer(x)) && length(unique(x[!is.na(x)])) <= 20)
    }, logical(1))]
    nms[vapply(df[nms], function(x) length(unique(x[!is.na(x)])) <= 50, logical(1))]
  })

  output$user_table_mode <- renderUI({
    req(!is.null(rv_current$working_df))
    radioButtons("cboTableMode", "Table type:",
      choices = c("Summary table" = "summary", "Cross-tabulation" = "cross"),
      selected = "summary", inline = TRUE)
  })
  output$user_calc_var <- renderUI({
    req(!is.null(rv_current$working_df))
    if (identical(mode(), "cross")) {
      selectInput("cboCalcVar", "Row variable:",
        choices = c(placeholder(), categorical_names()), selected = "")
    } else {
      selectizeInput("cboCalcVar", "Variables to summarize:", choices = all_names(),
        selected = character(0), multiple = TRUE,
        options = list(placeholder = "Select one or more variables", plugins = list("remove_button")))
    }
  })
  output$user_row_var <- renderUI({
    req(!is.null(rv_current$working_df))
    if (identical(mode(), "cross")) {
      selectInput("cboColVar", "Column variable:",
        choices = c(placeholder(), categorical_names()), selected = "")
    } else {
      selectInput("cboColVar", "Group table by (optional):",
        choices = c(placeholder(), categorical_names()), selected = "")
    }
  })
  output$user_table_strata <- renderUI({
    req(!is.null(rv_current$working_df))
    selectizeInput("cboTableStrata", "Stratify by (optional):",
      choices = categorical_names(), selected = character(0), multiple = TRUE,
      options = list(maxItems = 3, plugins = list("remove_button"),
                     placeholder = "Up to three strata variables"))
  })
  output$user_table_percentage <- renderUI({
    if (!identical(mode(), "cross")) return(NULL)
    radioButtons("rdoTablePercent", "Percentage denominator:",
      choices = c("Column" = "column", "Row" = "row", "Overall" = "cell"),
      selected = "column", inline = TRUE)
  })
  output$user_show_binary_levels <- renderUI({
    if (!identical(mode(), "summary")) return(NULL)
    radioButtons("rdoShowBinaryLevels", "Show both levels of Yes/No variables:",
      choices = c("Yes" = "TRUE", "No" = "FALSE"), selected = "TRUE", inline = TRUE)
  })
  output$usr_create_cross_tab <- renderUI({
    req(!is.null(rv_current$working_df))
    actionButton("btnCreateTable", "Generate table", class = "btn-success", icon = icon("table"))
  })
  output$user_tab_more_out <- renderUI({
    req(!is.null(rv_current$working_df))
    shinyWidgets::switchInput("tabmore", label = NULL, value = isTRUE(input$tabmore),
      onLabel = "Hide details", offLabel = "Show more details",
      onStatus = "success", offStatus = "default")
  })
  output$custom_table_status <- renderUI({
    colour <- if (!blank(state$error)) "#b42318" else "#5f6b76"
    div(style = paste0("margin:8px 0;color:", colour, ";"), state$error %||% state$status)
  })

  observeEvent(input$tabmore, {
    shinyjs::runjs(if (isTRUE(input$tabmore))
      "$('#tabmoreoption').addClass('open-panel');" else
      "$('#tabmoreoption').removeClass('open-panel');")
  }, ignoreInit = FALSE)

  observe({
    summary_mode <- identical(mode(), "summary")
    shinyjs::toggle("user_report_numeric", condition = summary_mode)
    shinyjs::toggle("user_numeric_summary", condition = summary_mode)
    shinyjs::toggle("user_add_confidence_interval", condition = summary_mode)
  })

  observeEvent(rv_current$working_df, {
    state$data_revision <- state$data_revision + 1L
    state$cache <- list()
    state$cache_order <- character(0)
    state$ready <- FALSE
    state$generated <- FALSE
    state$base_table <- NULL
    state$error <- NULL
    state$status <- "Choose table variables, then click Generate table."
    plots_custom_rv$tab_rv <- NULL
  }, ignoreInit = FALSE)

  observeEvent(input$cboTableMode, {
    if (isTRUE(state$generated)) {
      state$ready <- FALSE
      state$generated <- FALSE
      state$base_table <- NULL
      state$error <- NULL
      state$status <- "Table type changed. Select variables and click Generate table."
      plots_custom_rv$tab_rv <- NULL
    }
  }, ignoreInit = TRUE)

  observeEvent(list(input$cboCalcVar, input$cboColVar, input$cboTableStrata), {
    if (isTRUE(state$generated)) {
      state$ready <- FALSE
      state$generated <- FALSE
      state$base_table <- NULL
      state$error <- NULL
      state$status <- "Table variables changed. Click Generate table to create the new table."
      plots_custom_rv$tab_rv <- NULL
    }
  }, ignoreInit = TRUE)

  validate_request <- function(df, table_mode, vars, by, strata) {
    if (is.null(df) || nrow(df) == 0) return("The dataset contains no observations.")
    if (identical(table_mode, "summary")) {
      vars <- vars[vars != ""]
      if (!length(vars)) return("Select at least one variable to summarize.")
      if (!all(vars %in% names(df))) return("One or more selected variables are no longer available.")
      if (!blank(by) && by %in% vars) return("The grouping variable must differ from the summary variables.")
    } else {
      row <- as.character(vars %||% "")[1]
      if (blank(row) || blank(by)) return("Select both a row variable and a column variable.")
      if (identical(row, by)) return("The row and column variables must be different.")
      if (!all(c(row, by) %in% names(df))) return("The selected row or column variable is unavailable.")
    }
    strata <- strata[strata != ""]
    if (length(strata) > 3) return("Use no more than three strata variables.")
    if (!all(strata %in% names(df))) return("One or more strata variables are unavailable.")
    used <- c(as.character(vars), by, strata)
    used <- used[!is.na(used) & used != ""]
    if (anyDuplicated(used)) return("Summary, grouping, row/column, and strata variables must be distinct.")
    NULL
  }

  summary_table <- function(df, vars, by, show_all_binary, drop_na,
                            add_p, add_ci, report_numeric, numeric_summary) {
    type_arg <- if (show_all_binary) list(gtsummary::all_dichotomous() ~ "categorical") else NULL
    continuous_stat <- if (identical(report_numeric, "median")) {
      if (identical(numeric_summary, "min-max")) "{median} ({min}, {max})" else "{median} ({p25}, {p75})"
    } else {
      if (identical(numeric_summary, "min-max")) "{mean} ({min}, {max})" else "{mean} ({sd})"
    }
    if (blank(by)) {
      tab <- gtsummary::tbl_summary(
        data = df, include = tidyselect::all_of(vars), type = type_arg,
        statistic = list(gtsummary::all_continuous() ~ continuous_stat),
        missing = if (drop_na) "no" else "ifany"
      )
    } else {
      tab <- gtsummary::tbl_summary(
        data = df, by = tidyselect::all_of(by),
        include = tidyselect::all_of(vars), type = type_arg,
        statistic = list(gtsummary::all_continuous() ~ continuous_stat),
        missing = if (drop_na) "no" else "ifany"
      )
    }
    if (add_p && !blank(by)) tab <- gtsummary::add_p(tab)
    if (add_ci) tab <- gtsummary::add_ci(tab)
    tab
  }

  cross_table <- function(df, row, col, percent, drop_na, add_p) {
    tab <- gtsummary::tbl_cross(
      data = df, row = tidyselect::all_of(row), col = tidyselect::all_of(col),
      percent = percent, missing = if (drop_na) "no" else "ifany"
    )
    if (add_p) tab <- gtsummary::add_p(tab)
    tab
  }

  calculation_key <- function() {
    summary_mode <- identical(mode(), "summary")
    values <- c(
      paste0("data=", state$data_revision),
      paste0("mode=", mode()),
      paste0("vars=", paste(as.character(input$cboCalcVar %||% character(0)), collapse = "\u001f")),
      paste0("by=", as.character(input$cboColVar %||% "")[1]),
      paste0("strata=", paste(as.character(input$cboTableStrata %||% character(0)), collapse = "\u001f")),
      paste0("percent=", if (summary_mode) "" else
        as.character(input$rdoTablePercent %||% "column")[1]),
      paste0("binary=", if (summary_mode)
        yes(input$rdoShowBinaryLevels, TRUE) else ""),
      paste0("drop_na=", yes(input$rdoDropTabMissingValues, TRUE)),
      paste0("p=", yes(input$rdoAddTabPValue, FALSE)),
      paste0("ci=", if (summary_mode) yes(input$rdoAddTabCI, FALSE) else FALSE),
      paste0("numeric=", if (summary_mode)
        as.character(input$chkReportNumeric %||% "mean")[1] else ""),
      paste0("summary=", if (summary_mode)
        as.character(input$chkNumericSummary %||% "sd")[1] else "")
    )
    paste(values, collapse = "\u001e")
  }

  cache_table <- function(key, table) {
    cache <- state$cache
    order <- c(setdiff(state$cache_order, key), key)
    cache[[key]] <- table
    if (length(order) > 5) {
      remove_keys <- head(order, length(order) - 5)
      for (old_key in remove_keys) cache[[old_key]] <- NULL
      order <- tail(order, 5)
    }
    state$cache <- cache
    state$cache_order <- order
    invisible(table)
  }

  calculate_table <- function(use_cache = TRUE) {
    df <- rv_current$working_df
    table_mode <- mode()
    vars <- input$cboCalcVar %||% character(0)
    by <- as.character(input$cboColVar %||% "")[1]
    strata <- as.character(input$cboTableStrata %||% character(0))
    strata <- strata[strata != ""]
    error <- validate_request(df, table_mode, vars, by, strata)
    if (!is.null(error)) {
      return(list(ok = FALSE, error = error, base_table = NULL, cached = FALSE))
    }

    key <- calculation_key()
    if (isTRUE(use_cache) && !is.null(state$cache[[key]])) {
      return(list(ok = TRUE, error = NULL, base_table = state$cache[[key]], cached = TRUE))
    }

    drop_na <- yes(input$rdoDropTabMissingValues, TRUE)
    add_p <- yes(input$rdoAddTabPValue, FALSE)
    add_ci <- yes(input$rdoAddTabCI, FALSE)
    show_all <- yes(input$rdoShowBinaryLevels, TRUE)
    report_numeric <- as.character(input$chkReportNumeric %||% "mean")[1]
    numeric_summary <- as.character(input$chkNumericSummary %||% "sd")[1]
    percent <- as.character(input$rdoTablePercent %||% "column")[1]

    needed_columns <- unique(c(as.character(vars), by, strata))
    needed_columns <- needed_columns[!is.na(needed_columns) & needed_columns != ""]
    table_df <- df[, needed_columns, drop = FALSE]

    build_one <- function(d) {
      if (identical(table_mode, "summary")) {
        summary_table(d, as.character(vars), by, show_all, drop_na,
          add_p, add_ci, report_numeric, numeric_summary)
      } else {
        cross_table(d, as.character(vars)[1], by, percent, drop_na, add_p)
      }
    }
    tab <- if (length(strata)) {
      gtsummary::tbl_strata(
        data = table_df, strata = tidyselect::all_of(strata),
        .tbl_fun = function(data, ...) build_one(data),
        .combine_with = "tbl_stack"
      )
    } else build_one(table_df)

    cache_table(key, tab)
    list(ok = TRUE, error = NULL, base_table = tab, cached = FALSE)
  }

  format_table <- function(base_table) {
    if (is.null(base_table)) stop("There is no calculated table to format.")
    caption <- as.character(input$txtTabCaption %||% "")[1]
    display_table <- base_table
    if (!blank(caption)) {
      display_table <- gtsummary::modify_caption(display_table, caption)
    }
    gtsummary::as_flex_table(display_table)
  }

  build_table_result <- function(use_cache = TRUE) {
    calculated <- calculate_table(use_cache = use_cache)
    if (!isTRUE(calculated$ok)) {
      return(list(ok = FALSE, error = calculated$error, table = NULL,
                  base_table = NULL, cached = FALSE))
    }
    flex <- format_table(calculated$base_table)
    list(ok = TRUE, error = NULL, table = flex,
         base_table = calculated$base_table, cached = calculated$cached)
  }

  observeEvent(input$btnCreateTable, {
    req(is_table())
    shinyjs::disable("btnCreateTable")
    on.exit(shinyjs::enable("btnCreateTable"), add = TRUE)
    state$ready <- FALSE
    state$error <- NULL
    state$status <- "Generating table..."

    result <- shiny::withProgress(
      message = "Generating table",
      detail = "Checking variables and table settings...",
      value = 0,
      {
        shiny::setProgress(value = 0.10,
          detail = "Checking variables and table settings...")
        result <- tryCatch({
          shiny::setProgress(value = 0.30,
            detail = if (identical(mode(), "cross"))
              "Calculating counts and percentages..." else
              "Calculating summary statistics...")
          built <- build_table_result(use_cache = TRUE)
          shiny::setProgress(value = 0.85,
            detail = if (isTRUE(built$cached))
              "Using a cached calculation and formatting the table..." else
              "Formatting the table...")
          built
        }, error = function(e) {
          list(ok = FALSE, error = conditionMessage(e), table = NULL)
        })
        shiny::setProgress(value = 1,
          detail = if (isTRUE(result$ok)) "Table ready." else "Table generation stopped.")
        result
      }
    )
    if (isTRUE(result$ok)) {
      plots_custom_rv$tab_rv <- result$table
      state$ready <- TRUE
      state$generated <- TRUE
      state$base_table <- result$base_table
      state$error <- NULL
      state$status <- if (isTRUE(result$cached))
        "Table ready (reused a recent calculation)." else "Table ready."
    } else {
      plots_custom_rv$tab_rv <- NULL
      state$ready <- FALSE
      state$generated <- FALSE
      state$base_table <- NULL
      state$error <- paste("Table could not be generated:", result$error)
    }
  }, ignoreInit = TRUE)

  calculation_request <- shiny::debounce(reactive({
    list(
      percentage = input$rdoTablePercent,
      binary_levels = input$rdoShowBinaryLevels,
      drop_missing = input$rdoDropTabMissingValues,
      add_p = input$rdoAddTabPValue,
      add_ci = input$rdoAddTabCI,
      numeric_report = input$chkReportNumeric,
      numeric_summary = input$chkNumericSummary
    )
  }), 650)

  observeEvent(calculation_request(), {
    if (!isTRUE(state$generated) || !isTRUE(is_table())) return()

    previous_table <- plots_custom_rv$tab_rv
    previous_base <- state$base_table
    shinyjs::disable("btnCreateTable")
    on.exit(shinyjs::enable("btnCreateTable"), add = TRUE)
    state$error <- NULL
    state$status <- "Recalculating table automatically..."

    result <- shiny::withProgress(
      message = "Updating table",
      detail = "Applying the revised statistical options...",
      value = 0,
      {
        shiny::setProgress(value = 0.15,
          detail = "Checking the revised options...")
        result <- tryCatch({
          shiny::setProgress(value = 0.35,
            detail = "Recalculating table statistics...")
          built <- build_table_result(use_cache = TRUE)
          shiny::setProgress(value = 0.88,
            detail = if (isTRUE(built$cached))
              "Reusing a recent calculation..." else "Formatting the updated table...")
          built
        }, error = function(e) {
          list(ok = FALSE, error = conditionMessage(e), table = NULL,
               base_table = NULL, cached = FALSE)
        })
        shiny::setProgress(value = 1,
          detail = if (isTRUE(result$ok)) "Table updated." else "Update stopped.")
        result
      }
    )

    if (isTRUE(result$ok)) {
      plots_custom_rv$tab_rv <- result$table
      state$base_table <- result$base_table
      state$ready <- TRUE
      state$error <- NULL
      state$status <- if (isTRUE(result$cached))
        "Table updated automatically using a recent calculation." else
        "Table recalculated and updated automatically."
    } else {
      plots_custom_rv$tab_rv <- previous_table
      state$base_table <- previous_base
      state$ready <- !is.null(previous_table)
      state$error <- paste("The table option could not be applied:", result$error)
      state$status <- "The previous table has been retained."
    }
  }, ignoreInit = TRUE)

  caption_request <- shiny::debounce(reactive({
    input$txtTabCaption %||% ""
  }), 350)

  observeEvent(caption_request(), {
    if (!isTRUE(state$generated) || !isTRUE(is_table()) ||
        is.null(state$base_table)) return()
    previous_table <- plots_custom_rv$tab_rv
    result <- tryCatch(
      list(ok = TRUE, table = format_table(state$base_table), error = NULL),
      error = function(e) list(ok = FALSE, table = NULL,
                               error = conditionMessage(e))
    )
    if (isTRUE(result$ok)) {
      plots_custom_rv$tab_rv <- result$table
      state$ready <- TRUE
      state$error <- NULL
      state$status <- "Table caption updated automatically."
    } else {
      plots_custom_rv$tab_rv <- previous_table
      state$ready <- !is.null(previous_table)
      state$error <- paste("The caption could not be applied:", result$error)
      state$status <- "The previous table has been retained."
    }
  }, ignoreInit = TRUE)

  output$tabSummaries <- renderUI({
    if (!is_table()) return(NULL)
    if (state$ready && !is.null(plots_custom_rv$tab_rv)) {
      div(style = "overflow-x:auto;width:100%;", flextable::htmltools_value(plots_custom_rv$tab_rv))
    } else if (!blank(state$error)) {
      div(style = "color:#b42318;background:#fff5f5;border:1px solid #f5c2c7;padding:12px;border-radius:6px;", state$error)
    }
  })
  output$btnDownloadTable <- downloadHandler(
    filename = function() paste0("summary_table_", format(Sys.Date(), "%Y%m%d"), ".docx"),
    content = function(file) {
      req(state$ready, plots_custom_rv$tab_rv)
      doc <- officer::read_docx()
      doc <- flextable::body_add_flextable(doc, value = plots_custom_rv$tab_rv)
      print(doc, target = file)
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
}
