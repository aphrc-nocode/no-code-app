history_visualize_custom_actions_server <- function(input, output, session, rv_current, plots_sec_rv, get_rv_labels = function(x) x) {
  `%||%` <- function(x, y) if (is.null(x)) y else x

  show_msg <- function(txt, type = c("message", "warning", "error")) {
    showNotification(txt, type = match.arg(type), duration = 4)
  }

  if (is.null(rv_current$history_active_section)) {
    rv_current$history_active_section <- NULL
  }

  rv_history <- shiny::reactiveValues(
    undo = list(),
    redo = list(),
    baseline = NULL,
    pending = NULL,
    restoring = FALSE
  )

  visualize_custom_snapshot <- function() {
    list(
      plot_rv = plots_sec_rv$plot_rv,
      tab_rv = plots_sec_rv$tab_rv
    )
  }

  same_visualize_custom_state <- function(a, b) {
    identical(
      list(
        plot_rv = a$plot_rv,
        tab_rv = a$tab_rv
      ),
      list(
        plot_rv = b$plot_rv,
        tab_rv = b$tab_rv
      )
    )
  }

  refresh_visualize_custom_outputs <- function() {
    output$GeneratedPlot <- renderPlot({
      plots_sec_rv$plot_rv
    })

    if (!is.null(plots_sec_rv$tab_rv)) {
      output$tabSummaries <- renderUI({
        div(
          style = "overflow-x:auto; width:100%;",
          flextable::htmltools_value(plots_sec_rv$tab_rv)
        )
      })
    } else {
      output$tabSummaries <- NULL
    }
  }

  restore_visualize_custom_snapshot <- function(snap) {
    if (is.null(snap)) return(FALSE)

    rv_history$restoring <- TRUE
    on.exit({ rv_history$restoring <- FALSE }, add = TRUE)

    plots_sec_rv$plot_rv <- snap$plot_rv
    plots_sec_rv$tab_rv <- snap$tab_rv

    refresh_visualize_custom_outputs()
    TRUE
  }

  push_history <- function(before, after, action) {
    if (is.null(before) || is.null(after)) return(invisible(FALSE))
    if (same_visualize_custom_state(before, after)) return(invisible(FALSE))

    entry <- list(
      action = action,
      before = before,
      after = after
    )

    rv_history$undo <- c(rv_history$undo, list(entry))
    if (length(rv_history$undo) > 20) rv_history$undo <- tail(rv_history$undo, 20)
    rv_history$redo <- list()

    rv_current$history_active_section <- "visualize_custom"
    invisible(TRUE)
  }

  init_baseline <- function(force = FALSE) {
    if (is.null(rv_current$working_df)) return(invisible(FALSE))

    if (isTRUE(force) || is.null(rv_history$baseline)) {
      rv_history$baseline <- visualize_custom_snapshot()
    }

    if (isTRUE(force)) {
      rv_history$undo <- list()
      rv_history$redo <- list()
      rv_history$pending <- NULL
    }

    invisible(TRUE)
  }

  observeEvent(rv_current$working_df, {
    if (isTRUE(rv_history$restoring)) return()
    if (!is.null(rv_current$working_df) && is.null(rv_history$baseline)) init_baseline(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$manage_data_apply, {
    init_baseline(force = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$btnchartOut, {
    if (isTRUE(rv_history$restoring)) return()
    if (is.null(rv_history$baseline)) init_baseline(FALSE)

    rv_history$pending <- list(
      action = "generate_custom_chart",
      before = isolate(visualize_custom_snapshot())
    )
    rv_current$history_active_section <- "visualize_custom"
  }, ignoreInit = TRUE, priority = 1000)

  observeEvent(input$btnchartOut, {
    if (isTRUE(rv_history$restoring)) return()

    pending <- isolate(rv_history$pending)
    rv_history$pending <- NULL
    if (is.null(pending)) return()

    after <- isolate(visualize_custom_snapshot())
    push_history(pending$before, after, pending$action)
  }, ignoreInit = TRUE, priority = -1000)

  observeEvent(input$btnCreatetable, {
    if (isTRUE(rv_history$restoring)) return()
    if (is.null(rv_history$baseline)) init_baseline(FALSE)

    rv_history$pending <- list(
      action = "generate_custom_table",
      before = isolate(visualize_custom_snapshot())
    )
    rv_current$history_active_section <- "visualize_custom"
  }, ignoreInit = TRUE, priority = 1000)

  observeEvent(input$btnCreatetable, {
    if (isTRUE(rv_history$restoring)) return()

    pending <- isolate(rv_history$pending)
    rv_history$pending <- NULL
    if (is.null(pending)) return()

    after <- isolate(visualize_custom_snapshot())
    push_history(pending$before, after, pending$action)
  }, ignoreInit = TRUE, priority = -1000)

  run_action <- function(action) {
    if (identical(action, "undo")) {
      if (length(rv_history$undo) == 0) {
        return(invisible(FALSE))
      }

      entry <- rv_history$undo[[length(rv_history$undo)]]
      ok <- tryCatch(restore_visualize_custom_snapshot(entry$before), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }

      rv_history$undo <- rv_history$undo[-length(rv_history$undo)]
      rv_history$redo <- c(rv_history$redo, list(entry))
      rv_current$history_active_section <- "visualize_custom"
      return(invisible(TRUE))
    }

    if (identical(action, "redo")) {
      if (length(rv_history$redo) == 0) {
        return(invisible(FALSE))
      }

      entry <- rv_history$redo[[length(rv_history$redo)]]
      ok <- tryCatch(restore_visualize_custom_snapshot(entry$after), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }

      rv_history$redo <- rv_history$redo[-length(rv_history$redo)]
      rv_history$undo <- c(rv_history$undo, list(entry))
      rv_current$history_active_section <- "visualize_custom"
      return(invisible(TRUE))
    }

    if (identical(action, "reset")) {
      if (is.null(rv_history$baseline)) {
        return(invisible(FALSE))
      }

      current <- visualize_custom_snapshot()
      if (same_visualize_custom_state(current, rv_history$baseline)) {
        rv_history$undo <- list()
        rv_history$redo <- list()
        rv_history$pending <- NULL
        return(invisible(FALSE))
      }

      ok <- tryCatch(restore_visualize_custom_snapshot(rv_history$baseline), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }

      rv_history$undo <- list()
      rv_history$redo <- list()
      rv_history$pending <- NULL
      rv_current$history_active_section <- "visualize_custom"
      return(invisible(TRUE))
    }

    invisible(FALSE)
  }

  observeEvent(input$history_sidebar_action, {
    payload <- input$history_sidebar_action
    action <- NULL

    if (is.list(payload)) {
      if (!is.null(payload$action)) action <- as.character(payload$action)[1]
    } else if (is.character(payload) && length(payload) > 0) {
      action <- payload[[1]]
    }

    if (!identical(isolate(rv_current$history_active_section), "visualize_custom")) return()
    if (is.null(action) || !nzchar(action)) return()

    run_action(action)
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
