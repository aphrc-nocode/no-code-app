history_visualize_auto_actions_server <- function(input, output, session, rv_current, plots_auto_rv, get_rv_labels = function(x) x) {
  `%||%` <- function(x, y) if (is.null(x)) y else x

  deep_clone <- function(x) {
    tryCatch(unserialize(serialize(x, NULL)), error = function(e) x)
  }

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

  visualize_auto_snapshot <- function() {
    list(
      plot_corr = deep_clone(plots_auto_rv$plot_corr),
      plot_bivariate_auto = deep_clone(plots_auto_rv$plot_bivariate_auto),
      inputs = list(
        cboCorrFeatures = deep_clone(input$cboCorrFeatures),
        cboColorBrewerCorrplot = input$cboColorBrewerCorrplot %||% "",
        cboBivariateOutcome = input$cboBivariateOutcome %||% "",
        cboBivariateFeatures = deep_clone(input$cboBivariateFeatures),
        cboColorBrewerBivariate = input$cboColorBrewerBivariate %||% "",
        txtPlotBivariateTitle = input$txtPlotBivariateTitle %||% ""
      )
    )
  }

  same_visualize_auto_state <- function(a, b) {
    identical(
      list(
        plot_corr = a$plot_corr,
        plot_bivariate_auto = a$plot_bivariate_auto
      ),
      list(
        plot_corr = b$plot_corr,
        plot_bivariate_auto = b$plot_bivariate_auto
      )
    )
  }

  refresh_visualize_auto_outputs <- function() {
    output$CorrPlotOutput <- renderPlot({ plots_auto_rv$plot_corr })
    output$BivariatePlotOutput <- renderPlot({ plots_auto_rv$plot_bivariate_auto })
  }

  restore_visualize_auto_snapshot <- function(snap) {
    if (is.null(snap)) return(FALSE)

    rv_history$restoring <- TRUE
    on.exit({ rv_history$restoring <- FALSE }, add = TRUE)

    plots_auto_rv$plot_corr <- deep_clone(snap$plot_corr)
    plots_auto_rv$plot_bivariate_auto <- deep_clone(snap$plot_bivariate_auto)

    try(updateSelectInput(session, "cboCorrFeatures", selected = snap$inputs$cboCorrFeatures %||% NULL), silent = TRUE)
    try(updateSelectInput(session, "cboColorBrewerCorrplot", selected = snap$inputs$cboColorBrewerCorrplot %||% ""), silent = TRUE)
    try(updateSelectInput(session, "cboBivariateOutcome", selected = snap$inputs$cboBivariateOutcome %||% ""), silent = TRUE)
    try(updateSelectInput(session, "cboBivariateFeatures", selected = snap$inputs$cboBivariateFeatures %||% NULL), silent = TRUE)
    try(updateSelectInput(session, "cboColorBrewerBivariate", selected = snap$inputs$cboColorBrewerBivariate %||% ""), silent = TRUE)
    try(updateTextInput(session, "txtPlotBivariateTitle", value = snap$inputs$txtPlotBivariateTitle %||% ""), silent = TRUE)

    refresh_visualize_auto_outputs()
    TRUE
  }

  push_history <- function(before, after, action) {
    if (is.null(before) || is.null(after)) return(invisible(FALSE))
    if (same_visualize_auto_state(before, after)) return(invisible(FALSE))

    entry <- list(
      action = action,
      before = deep_clone(before),
      after = deep_clone(after)
    )

    rv_history$undo <- c(rv_history$undo, list(entry))
    if (length(rv_history$undo) > 20) rv_history$undo <- tail(rv_history$undo, 20)
    rv_history$redo <- list()

    rv_current$history_active_section <- "visualize_automatic"
    invisible(TRUE)
  }

  init_baseline <- function(force = FALSE) {
    if (is.null(rv_current$working_df)) return(invisible(FALSE))

    rv_history$baseline <- visualize_auto_snapshot()

    if (isTRUE(force)) {
      rv_history$undo <- list()
      rv_history$redo <- list()
      rv_history$pending <- NULL
    }

    invisible(TRUE)
  }

  observeEvent(input$manage_data_apply, {
    init_baseline(force = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(rv_current$working_df, {
    if (isTRUE(rv_history$restoring)) return()
    if (!is.null(rv_current$working_df) && is.null(rv_history$baseline)) init_baseline(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$btnGenerateBivariate, {
    if (isTRUE(rv_history$restoring)) return()
    if (is.null(rv_history$baseline)) init_baseline(FALSE)

    rv_history$pending <- list(
      action = "generate_bivariate",
      before = visualize_auto_snapshot()
    )
    rv_current$history_active_section <- "visualize_automatic"
  }, ignoreInit = TRUE, priority = 1000)

  observeEvent(input$btnGenerateBivariate, {
    if (isTRUE(rv_history$restoring)) return()

    pending <- isolate(rv_history$pending)
    rv_history$pending <- NULL
    if (is.null(pending)) return()

    push_history(pending$before, visualize_auto_snapshot(), pending$action)
  }, ignoreInit = TRUE, priority = -1000)

  run_action <- function(action) {
    if (identical(action, "undo")) {
      if (length(rv_history$undo) == 0) {
        return(invisible(FALSE))
      }

      entry <- rv_history$undo[[length(rv_history$undo)]]
      ok <- tryCatch(restore_visualize_auto_snapshot(entry$before), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }

      rv_history$undo <- rv_history$undo[-length(rv_history$undo)]
      rv_history$redo <- c(rv_history$redo, list(entry))
      rv_current$history_active_section <- "visualize_automatic"
      return(invisible(TRUE))
    }

    if (identical(action, "redo")) {
      if (length(rv_history$redo) == 0) {
        return(invisible(FALSE))
      }

      entry <- rv_history$redo[[length(rv_history$redo)]]
      ok <- tryCatch(restore_visualize_auto_snapshot(entry$after), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }

      rv_history$redo <- rv_history$redo[-length(rv_history$redo)]
      rv_history$undo <- c(rv_history$undo, list(entry))
      rv_current$history_active_section <- "visualize_automatic"
      return(invisible(TRUE))
    }

    if (identical(action, "reset")) {
      if (is.null(rv_history$baseline)) {
        return(invisible(FALSE))
      }

      current <- visualize_auto_snapshot()
      if (same_visualize_auto_state(current, rv_history$baseline)) {
        rv_history$undo <- list()
        rv_history$redo <- list()
        rv_history$pending <- NULL
        return(invisible(FALSE))
      }

      ok <- tryCatch(restore_visualize_auto_snapshot(rv_history$baseline), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }

      rv_history$undo <- list()
      rv_history$redo <- list()
      rv_history$pending <- NULL
      rv_current$history_active_section <- "visualize_automatic"
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

    if (!identical(isolate(rv_current$history_active_section), "visualize_automatic")) return()
    if (is.null(action) || !nzchar(action)) return()

    run_action(action)
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
