# server/session_persistence.R
# ============================================================================
# Session State Persistence — auto-save & restore user workspace
# ============================================================================
#
# Saves key reactive values to <username>/session_state.rds so that if the
# browser is closed the user's workspace is restored on re-login.
#
# PERFORMANCE NOTES:
# - Large objects (data, working_df) are saved lazily (5 min interval)
# - Small metadata is saved more frequently (event-driven)
# - All saves are debounced to prevent stacking
# ============================================================================

# ---------------------------------------------------------------------------
# Helper: collect the serialisable subset of a reactiveValues object
# ---------------------------------------------------------------------------
.rv_to_list <- function(rv, keys = NULL) {
  all_names <- isolate(names(rv))
  if (!is.null(keys)) all_names <- intersect(all_names, keys)
  out <- list()
  for (nm in all_names) {
    val <- tryCatch(isolate(rv[[nm]]), error = function(e) NULL)
    if (inherits(val, c("DBIConnection", "Pool", "environment", "R6"))) next
    out[[nm]] <- val
  }
  out
}

# ---------------------------------------------------------------------------
# Helper: restore values into a reactiveValues object
# ---------------------------------------------------------------------------
.list_to_rv <- function(rv, saved, keys = NULL) {
  if (is.null(saved)) return()
  nms <- names(saved)
  if (!is.null(keys)) nms <- intersect(nms, keys)
  for (nm in nms) {
    tryCatch(rv[[nm]] <- saved[[nm]], error = function(e) {
      warning("[session-persistence] Could not restore rv$", nm, ": ", e$message)
    })
  }
}

# ===========================================================================
# PUBLIC API — called from server.R
# ===========================================================================

session_persistence_server <- function(session, app_username,
                                       rv_current, rv_metadata,
                                       rv_ml_ai, rv_train_control_caret,
                                       input) {

  # ---- Paths ---------------------------------------------------------------
  state_dir  <- file.path(getwd(), app_username)
  state_file <- file.path(state_dir, "session_state.rds")

  # Keys — LIGHT (small scalars, saved on events)
  rv_current_light_keys <- c(
    "dataset_id", "metadata_id",
    "selected_vars", "selected_var",
    "current_filter", "current_filter_reset",
    "manage_data_title_explore", "manage_data_title_transform",
    "missing_prop", "has_missing_data_check",
    "transform_data_select_vars", "vartype",
    "changed_variable_type_log", "renamed_variable_log",
    "recoded_variable_labels_log", "missing_prop_df",
    "created_missing_values_log", "outlier_values",
    "handle_missing_values_log", "handle_outlier_values_log",
    "outcome", "vartype_all"
  )

  # Keys — HEAVY (data frames, saved only on timer or session end)
  rv_current_heavy_keys <- c("data", "working_df", "quick_explore_summary")

  rv_ml_ai_keys <- c(
    "session_id", "seed_value", "dataset_id",
    "analysis_type", "task", "outcome",
    "model_formula", "partition_ratio",
    "predictors", "excluded_predictors",
    "at_least_one_model"
  )

  rv_train_control_keys <- c(
    "method", "number", "repeats", "search",
    "verboseIter", "savePredictions", "classProbs"
  )

  rv_metadata_keys <- c(
    "upload_logs", "dataset_ids",
    "data_summary_str", "data_summary_skim",
    "data_summary_summary", "data_summary_summarytools"
  )

  # ---- Internal save guard (debounce) --------------------------------------
  .last_save_time <- reactiveVal(Sys.time() - 10)

  # ---- SAVE (light = metadata only, full = includes heavy keys) ------------
  .do_save <- function(include_heavy = FALSE, silent = TRUE) {
    # Debounce: skip if last save was < 5 seconds ago
    elapsed <- as.numeric(difftime(Sys.time(), isolate(.last_save_time()), units = "secs"))
    if (elapsed < 5) return(invisible())

    tryCatch({
      if (!dir.exists(state_dir)) dir.create(state_dir, recursive = TRUE)

      # Always save light keys
      current_keys <- rv_current_light_keys
      if (include_heavy) current_keys <- c(current_keys, rv_current_heavy_keys)

      snapshot <- list(
        saved_at       = Sys.time(),
        active_tab     = isolate(input$tabs),
        rv_current     = .rv_to_list(rv_current, current_keys),
        rv_metadata    = .rv_to_list(rv_metadata, rv_metadata_keys),
        rv_ml_ai       = .rv_to_list(rv_ml_ai, rv_ml_ai_keys),
        rv_train_ctrl  = .rv_to_list(rv_train_control_caret, rv_train_control_keys),
        has_heavy      = include_heavy
      )

      saveRDS(snapshot, file = state_file)
      .last_save_time(Sys.time())

      if (!silent) {
        showNotification(
          paste0("\u2705 Session saved (", format(Sys.time(), "%H:%M:%S"), ")"),
          type = "message", duration = 3
        )
      }
    }, error = function(e) {
      warning("[session-persistence] Save failed: ", e$message)
    })
  }

  # ---- RESTORE -------------------------------------------------------------
  .do_restore <- function() {
    if (!file.exists(state_file)) return(invisible(FALSE))
    tryCatch({
      snapshot <- readRDS(state_file)

      # Don't restore ancient snapshots (> 7 days)
      if (difftime(Sys.time(), snapshot$saved_at, units = "days") > 7) {
        message("[session-persistence] Snapshot too old, skipping restore.")
        return(invisible(FALSE))
      }

      # Restore light + heavy keys from rv_current
      all_current_keys <- c(rv_current_light_keys, rv_current_heavy_keys)
      .list_to_rv(rv_current, snapshot$rv_current, all_current_keys)
      .list_to_rv(rv_metadata, snapshot$rv_metadata, rv_metadata_keys)
      .list_to_rv(rv_ml_ai, snapshot$rv_ml_ai, rv_ml_ai_keys)
      .list_to_rv(rv_train_control_caret, snapshot$rv_train_ctrl, rv_train_control_keys)

      # Restore sidebar tab — use JavaScript to click the menu item directly
      # (updateTabItems doesn't work with sidebarMenuOutput/renderMenu)
      saved_tab <- snapshot$active_tab
      saved_time <- format(snapshot$saved_at, "%b %d, %H:%M")

      if (!is.null(saved_tab) && saved_tab != "homePage") {
        shinyjs::delay(2500, {
          # Click the sidebar link whose data-value matches the saved tab
          shinyjs::runjs(sprintf(
            "var el = $('a[data-value=\"%s\"]'); if(el.length){ el.click(); }",
            saved_tab
          ))
        })
        showNotification(
          paste0("\u267b\ufe0f Session restored from ", saved_time,
                 " \u2014 navigating back to your workspace..."),
          type = "message", duration = 5
        )
      } else {
        showNotification(
          paste0("\u267b\ufe0f Session restored from ", saved_time),
          type = "message", duration = 5
        )
      }
      return(invisible(TRUE))
    }, error = function(e) {
      warning("[session-persistence] Restore failed: ", e$message)
      return(invisible(FALSE))
    })
  }

  # ---- AUTO-SAVE TIMER (every 5 minutes for heavy data) --------------------
  observe({
    invalidateLater(300000, session)  # 5 minutes instead of 60 seconds
    if (!is.null(isolate(rv_current$dataset_id))) {
      .do_save(include_heavy = TRUE, silent = TRUE)
    }
  })

  # ---- SAVE ON KEY EVENTS (light saves — fast) -----------------------------
  # After dataset upload
  observeEvent(rv_current$dataset_id, {
    if (!is.null(rv_current$dataset_id)) .do_save(include_heavy = TRUE, silent = FALSE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # After ML setup changes (light only — fast)
  observeEvent(rv_ml_ai$outcome, {
    .do_save(include_heavy = FALSE, silent = TRUE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ---- SAVE ON SESSION END (full save) -------------------------------------
  session$onSessionEnded(function() {
    isolate({
      tryCatch(.do_save(include_heavy = TRUE, silent = TRUE), error = function(e) NULL)
    })
  })

  # ---- INITIAL RESTORE (called once after login) ---------------------------
  .do_restore()
}
