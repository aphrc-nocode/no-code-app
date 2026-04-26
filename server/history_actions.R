history_actions_server <- function(input, output, session, rv_current, get_rv_labels = function(x) x) {
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
  
  explore_snapshot <- function() {
    list(
      working_df = deep_clone(rv_current$working_df),
      current_filter = deep_clone(rv_current$current_filter),
      current_filter_reset = deep_clone(rv_current$current_filter_reset),
      filter_rules_input = input$explore_data_filter_rules %||% ""
    )
  }
  
  same_explore_state <- function(a, b) {
    identical(
      list(
        working_df = a$working_df,
        current_filter = a$current_filter,
        current_filter_reset = a$current_filter_reset
      ),
      list(
        working_df = b$working_df,
        current_filter = b$current_filter,
        current_filter_reset = b$current_filter_reset
      )
    )
  }
  
  refresh_explore_outputs <- function() {
    if (!is.null(rv_current$current_filter) && length(rv_current$current_filter) > 0) {
      output$data_explore_filter_applied <- renderUI({
        p(HTML(paste0("<b>", get_rv_labels("current_filter"), ": </b>")))
      })
      output$data_explore_filter_applied_out <- renderPrint({
        cat(rv_current$current_filter, sep = "\n")
      })
    } else {
      output$data_explore_filter_applied <- NULL
      output$data_explore_filter_applied_out <- NULL
    }
  }
  
  restore_explore_snapshot <- function(snap) {
    if (is.null(snap)) return(FALSE)
    
    rv_history$restoring <- TRUE
    on.exit({ rv_history$restoring <- FALSE }, add = TRUE)
    
    rv_current$working_df <- deep_clone(snap$working_df)
    rv_current$current_filter <- deep_clone(snap$current_filter)
    rv_current$current_filter_reset <- deep_clone(snap$current_filter_reset)
    
    try(
      updateTextAreaInput(
        session,
        "explore_data_filter_rules",
        value = snap$filter_rules_input %||% ""
      ),
      silent = TRUE
    )
    
    refresh_explore_outputs()
    TRUE
  }
  
  push_history <- function(before, after, action) {
    if (is.null(before) || is.null(after)) return(invisible(FALSE))
    if (same_explore_state(before, after)) return(invisible(FALSE))
    
    entry <- list(
      action = action,
      before = deep_clone(before),
      after = deep_clone(after)
    )
    
    rv_history$undo <- c(rv_history$undo, list(entry))
    if (length(rv_history$undo) > 20) rv_history$undo <- tail(rv_history$undo, 20)
    rv_history$redo <- list()
    
    rv_current$history_active_section <- "explore"
    invisible(TRUE)
  }
  
  init_baseline <- function(force = FALSE) {
    if (is.null(rv_current$data) && is.null(rv_current$working_df)) return(invisible(FALSE))
    
    rv_history$baseline <- explore_snapshot()
    
    if (isTRUE(force)) {
      rv_history$undo <- list()
      rv_history$redo <- list()
      rv_history$pending <- NULL
    }
    
    invisible(TRUE)
  }
  
  observeEvent(rv_current$data, {
    if (!is.null(rv_current$data)) init_baseline(force = TRUE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$manage_data_apply, {
    init_baseline(force = TRUE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$manage_data_explore_filter_apply, {
    if (isTRUE(rv_history$restoring)) return()
    
    rules <- trimws(input$explore_data_filter_rules %||% "")
    if (!nzchar(rules)) return()
    
    if (is.null(rv_history$baseline)) init_baseline(FALSE)
    
    rv_history$pending <- list(
      action = "apply_filter",
      before = explore_snapshot()
    )
  }, ignoreInit = TRUE, priority = 1000)
  
  observeEvent(input$manage_data_explore_filter_apply, {
    if (isTRUE(rv_history$restoring)) return()
    
    pending <- isolate(rv_history$pending)
    rv_history$pending <- NULL
    if (is.null(pending)) return()
    
    push_history(pending$before, explore_snapshot(), pending$action)
  }, ignoreInit = TRUE, priority = -1000)
  
  observeEvent(input$manage_data_explore_filter_reset, {
    if (isTRUE(rv_history$restoring)) return()
    
    had_filter <- !is.null(rv_current$current_filter) && length(rv_current$current_filter) > 0
    changed_df <- !identical(rv_current$working_df, rv_current$data)
    if (!had_filter && !changed_df) return()
    
    if (is.null(rv_history$baseline)) init_baseline(FALSE)
    
    rv_history$pending <- list(
      action = "reset_filter",
      before = explore_snapshot()
    )
  }, ignoreInit = TRUE, priority = 1000)
  
  observeEvent(input$manage_data_explore_filter_reset, {
    if (isTRUE(rv_history$restoring)) return()
    
    pending <- isolate(rv_history$pending)
    rv_history$pending <- NULL
    if (is.null(pending)) return()
    
    push_history(pending$before, explore_snapshot(), pending$action)
  }, ignoreInit = TRUE, priority = -1000)
  
  run_action <- function(action) {
    if (identical(action, "undo")) {
      if (length(rv_history$undo) == 0) {
        return(invisible(FALSE))
      }
      
      entry <- rv_history$undo[[length(rv_history$undo)]]
      ok <- tryCatch(restore_explore_snapshot(entry$before), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }
      
      rv_history$undo <- rv_history$undo[-length(rv_history$undo)]
      rv_history$redo <- c(rv_history$redo, list(entry))
      rv_current$history_active_section <- "explore"
      return(invisible(TRUE))
    }
    
    if (identical(action, "redo")) {
      if (length(rv_history$redo) == 0) {
        return(invisible(FALSE))
      }
      
      entry <- rv_history$redo[[length(rv_history$redo)]]
      ok <- tryCatch(restore_explore_snapshot(entry$after), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }
      
      rv_history$redo <- rv_history$redo[-length(rv_history$redo)]
      rv_history$undo <- c(rv_history$undo, list(entry))
      rv_current$history_active_section <- "explore"
      return(invisible(TRUE))
    }
    
    if (identical(action, "reset")) {
      if (is.null(rv_history$baseline)) {
        return(invisible(FALSE))
      }
      
      current <- explore_snapshot()
      if (same_explore_state(current, rv_history$baseline)) {
        rv_history$undo <- list()
        rv_history$redo <- list()
        rv_history$pending <- NULL
        return(invisible(FALSE))
      }
      
      ok <- tryCatch(restore_explore_snapshot(rv_history$baseline), error = function(e) FALSE)
      if (!isTRUE(ok)) {
        return(invisible(FALSE))
      }
      
      rv_history$undo <- list()
      rv_history$redo <- list()
      rv_history$pending <- NULL
      rv_current$history_active_section <- "explore"
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
    
    if (!identical(isolate(rv_current$history_active_section), "explore")) return()
    if (is.null(action) || !nzchar(action)) return()
    
    run_action(action)
  }, ignoreInit = TRUE)
  
  invisible(TRUE)
}