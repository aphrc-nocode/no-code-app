history_transform_actions_server <- function(input, output, session, rv_current, get_rv_labels = function(x) x) {
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

  transform_snapshot <- function() {
    list(
      data = deep_clone(rv_current$data),
      working_df = deep_clone(rv_current$working_df),
      selected_vars = deep_clone(rv_current$selected_vars),
      selected_var = deep_clone(rv_current$selected_var),
      vartype = deep_clone(rv_current$vartype),

      changed_variable_type_log = deep_clone(rv_current$changed_variable_type_log),
      renamed_variable_log = deep_clone(rv_current$renamed_variable_log),
      recoded_variable_labels_log = deep_clone(rv_current$recoded_variable_labels_log),
      handle_missing_values_log = deep_clone(rv_current$handle_missing_values_log),
      handle_outlier_values_log = deep_clone(rv_current$handle_outlier_values_log),

      inputs = list(
        select_vars = input$transform_data_select_vars %||% "",
        change_type_check = isTRUE(input$transform_data_change_type_check),
        rename_check = isTRUE(input$transform_data_rename_variable_check),
        recode_check = isTRUE(input$transform_data_recode_variable_check)
      )
    )
  }

  same_transform_state <- function(a, b) {
    identical(
      list(
        data = a$data,
        working_df = a$working_df,
        selected_vars = a$selected_vars,
        selected_var = a$selected_var,
        vartype = a$vartype,
        changed_variable_type_log = a$changed_variable_type_log,
        renamed_variable_log = a$renamed_variable_log,
        recoded_variable_labels_log = a$recoded_variable_labels_log,
        handle_missing_values_log = a$handle_missing_values_log,
        handle_outlier_values_log = a$handle_outlier_values_log
      ),
      list(
        data = b$data,
        working_df = b$working_df,
        selected_vars = b$selected_vars,
        selected_var = b$selected_var,
        vartype = b$vartype,
        changed_variable_type_log = b$changed_variable_type_log,
        renamed_variable_log = b$renamed_variable_log,
        recoded_variable_labels_log = b$recoded_variable_labels_log,
        handle_missing_values_log = b$handle_missing_values_log,
        handle_outlier_values_log = b$handle_outlier_values_log
      )
    )
  }

  refresh_transform_outputs <- function() {
    output$transform_data_variable_type_log <- if (!is.null(rv_current$changed_variable_type_log) && length(rv_current$changed_variable_type_log) > 0) {
      renderPrint({ cat(rv_current$changed_variable_type_log, sep = "\n") })
    } else NULL

    output$transform_data_renamed_variable_log <- if (!is.null(rv_current$renamed_variable_log) && length(rv_current$renamed_variable_log) > 0) {
      renderPrint({ cat(rv_current$renamed_variable_log, sep = "\n") })
    } else NULL

    output$transform_data_recoded_variable_labels_log <- if (!is.null(rv_current$recoded_variable_labels_log) && length(rv_current$recoded_variable_labels_log) > 0) {
      renderPrint({ cat(rv_current$recoded_variable_labels_log, sep = "\n") })
    } else NULL

    output$transform_data_handle_missing_values_log <- if (!is.null(rv_current$handle_missing_values_log) && length(rv_current$handle_missing_values_log) > 0) {
      renderPrint({ cat(rv_current$handle_missing_values_log, sep = "\n") })
    } else NULL

    output$transform_data_handle_outlier_values_log <- if (!is.null(rv_current$handle_outlier_values_log) && length(rv_current$handle_outlier_values_log) > 0) {
      renderPrint({ cat(rv_current$handle_outlier_values_log, sep = "\n") })
    } else NULL
  }

  restore_transform_snapshot <- function(snap) {
    if (is.null(snap)) return(FALSE)

    rv_history$restoring <- TRUE
    on.exit({ rv_history$restoring <- FALSE }, add = TRUE)

    rv_current$data <- deep_clone(snap$data)
    rv_current$working_df <- deep_clone(snap$working_df)
    rv_current$selected_vars <- deep_clone(snap$selected_vars)
    rv_current$selected_var <- deep_clone(snap$selected_var)
    rv_current$vartype <- deep_clone(snap$vartype)

    rv_current$changed_variable_type_log <- deep_clone(snap$changed_variable_type_log)
    rv_current$renamed_variable_log <- deep_clone(snap$renamed_variable_log)
    rv_current$recoded_variable_labels_log <- deep_clone(snap$recoded_variable_labels_log)
    rv_current$handle_missing_values_log <- deep_clone(snap$handle_missing_values_log)
    rv_current$handle_outlier_values_log <- deep_clone(snap$handle_outlier_values_log)

    try(
      updateSelectInput(
        session,
        "transform_data_select_vars",
        selected = snap$inputs$select_vars %||% snap$selected_var %||% "",
        choices = snap$selected_vars %||% character(0)
      ),
      silent = TRUE
    )
    try(
      shinyWidgets::updateMaterialSwitch(
        session, "transform_data_change_type_check",
        value = isTRUE(snap$inputs$change_type_check)
      ),
      silent = TRUE
    )
    try(
      shinyWidgets::updateMaterialSwitch(
        session, "transform_data_rename_variable_check",
        value = isTRUE(snap$inputs$rename_check)
      ),
      silent = TRUE
    )
    try(
      shinyWidgets::updateMaterialSwitch(
        session, "transform_data_recode_variable_check",
        value = isTRUE(snap$inputs$recode_check)
      ),
      silent = TRUE
    )

    refresh_transform_outputs()
    TRUE
  }

  push_history <- function(before, after, action) {
    if (is.null(before) || is.null(after)) return(invisible(FALSE))
    if (same_transform_state(before, after)) return(invisible(FALSE))

    entry <- list(
      action = action,
      before = deep_clone(before),
      after = deep_clone(after)
    )

    rv_history$undo <- c(rv_history$undo, list(entry))
    if (length(rv_history$undo) > 20) rv_history$undo <- tail(rv_history$undo, 20)
    rv_history$redo <- list()

    rv_current$history_active_section <- "Transform"
    invisible(TRUE)
  }

  init_baseline <- function(force = FALSE) {
    if (is.null(rv_current$data) && is.null(rv_current$working_df)) return(invisible(FALSE))

    if (isTRUE(force) || is.null(rv_history$baseline)) {
      rv_history$baseline <- transform_snapshot()
    }

    if (isTRUE(force)) {
      rv_history$undo <- list()
      rv_history$redo <- list()
      rv_history$pending <- NULL
    }

    invisible(TRUE)
  }

  observeEvent(rv_current$data, {
    if (isTRUE(rv_history$restoring)) return()
    if (!is.null(rv_current$data) && is.null(rv_history$baseline)) init_baseline(force = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$manage_data_apply, {
    init_baseline(force = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$transform_data_apply, {
    if (isTRUE(rv_history$restoring)) return()
    if (is.null(rv_history$baseline)) init_baseline(FALSE)

    rv_history$pending <- list(
      action = "transform_apply",
      before = transform_snapshot()
    )
    rv_current$history_active_section <- "Transform"
  }, ignoreInit = TRUE, priority = 1000)

  observeEvent(input$transform_data_apply, {
    if (isTRUE(rv_history$restoring)) return()

    pending <- isolate(rv_history$pending)
    rv_history$pending <- NULL
    if (is.null(pending)) return()

    push_history(pending$before, transform_snapshot(), pending$action)
  }, ignoreInit = TRUE, priority = -1000)

  run_action <- function(action) {
    if (identical(action, "undo")) {
      if (length(rv_history$undo) == 0) {
        return(invisible(FALSE))
      }

      entry <- rv_history$undo[[length(rv_history$undo)]]
      ok <- tryCatch(restore_transform_snapshot(entry$before), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }

      rv_history$undo <- rv_history$undo[-length(rv_history$undo)]
      rv_history$redo <- c(rv_history$redo, list(entry))
      rv_current$history_active_section <- "Transform"
      return(invisible(TRUE))
    }

    if (identical(action, "redo")) {
      if (length(rv_history$redo) == 0) {
        return(invisible(FALSE))
      }

      entry <- rv_history$redo[[length(rv_history$redo)]]
      ok <- tryCatch(restore_transform_snapshot(entry$after), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }

      rv_history$redo <- rv_history$redo[-length(rv_history$redo)]
      rv_history$undo <- c(rv_history$undo, list(entry))
      rv_current$history_active_section <- "Transform"
      return(invisible(TRUE))
    }

    if (identical(action, "reset")) {
      if (is.null(rv_history$baseline)) {
        return(invisible(FALSE))
      }

      current <- transform_snapshot()
      if (same_transform_state(current, rv_history$baseline)) {
        rv_history$undo <- list()
        rv_history$redo <- list()
        rv_history$pending <- NULL
        return(invisible(FALSE))
      }

      ok <- tryCatch(restore_transform_snapshot(rv_history$baseline), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }

      rv_history$undo <- list()
      rv_history$redo <- list()
      rv_history$pending <- NULL
      rv_current$history_active_section <- "Transform"
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

    if (!identical(isolate(rv_current$history_active_section), "Transform")) return()
    if (is.null(action) || !nzchar(action)) return()

    run_action(action)
  }, ignoreInit = TRUE)

  invisible(TRUE)
}
