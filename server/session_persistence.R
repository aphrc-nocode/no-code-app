# server/session_persistence.R
# ============================================================================
# Session State Persistence — auto-save & restore user workspace
# ============================================================================
#
# Saves key reactive values to <username>/session_state.rds so that if the
# browser is closed (power failure, accidental tab close, etc.) the user's
# workspace is restored on re-login.
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
    # Skip non-serialisable objects (DB connections, environments, etc.)
    if (inherits(val, c("DBIConnection", "Pool", "environment", "R6"))) next
    out[[nm]] <- val
  }
  out
}

# ---------------------------------------------------------------------------
# Helper: restore values into a reactiveValues object
# ---------------------------------------------------------------------------
.list_to_rv <- function(rv, saved, keys = NULL) {
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

# ---------------------------------------------------------------------------
# session_persistence_server()
#
# Arguments:
#   session       – Shiny session
#   app_username  – character, the logged-in user name
#   rv_current    – reactiveValues for the current dataset / transform state
#   rv_metadata   – reactiveValues for upload logs & summaries
#   rv_ml_ai      – reactiveValues for ML/AI setup
#   rv_train_control_caret – reactiveValues for caret train control
#   input         – Shiny input (to read active sidebar tab)
# ---------------------------------------------------------------------------
session_persistence_server <- function(session, app_username,
                                       rv_current, rv_metadata,
                                       rv_ml_ai, rv_train_control_caret,
                                       input) {

  # ---- Paths ---------------------------------------------------------------
  state_dir  <- file.path(getwd(), app_username)
  state_file <- file.path(state_dir, "session_state.rds")

  # Keys we care about (avoid persisting huge/transient objects)
  rv_current_keys <- c(
    "dataset_id", "metadata_id", "data", "working_df",
    "selected_vars", "selected_var",
    "current_filter", "current_filter_reset",
    "manage_data_title_explore", "manage_data_title_transform",
    "missing_prop", "has_missing_data_check",
    "transform_data_select_vars", "vartype",
    "changed_variable_type_log", "renamed_variable_log",
    "recoded_variable_labels_log", "missing_prop_df",
    "created_missing_values_log", "outlier_values",
    "handle_missing_values_log", "handle_outlier_values_log",
    "quick_explore_summary", "outcome", "vartype_all"
  )

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

  # ---- SAVE ----------------------------------------------------------------
  .do_save <- function(silent = TRUE) {
    tryCatch({
      if (!dir.exists(state_dir)) dir.create(state_dir, recursive = TRUE)

      snapshot <- list(
        saved_at       = Sys.time(),
        active_tab     = isolate(input$dynamic_meinu_aphrc),
        rv_current     = .rv_to_list(rv_current, rv_current_keys),
        rv_metadata    = .rv_to_list(rv_metadata, rv_metadata_keys),
        rv_ml_ai       = .rv_to_list(rv_ml_ai, rv_ml_ai_keys),
        rv_train_ctrl  = .rv_to_list(rv_train_control_caret, rv_train_control_keys)
      )

      saveRDS(snapshot, file = state_file)

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

      # Sanity check — don't restore ancient snapshots (> 7 days)
      if (difftime(Sys.time(), snapshot$saved_at, units = "days") > 7) {
        message("[session-persistence] Snapshot too old, skipping restore.")
        return(invisible(FALSE))
      }

      .list_to_rv(rv_current, snapshot$rv_current, rv_current_keys)
      .list_to_rv(rv_metadata, snapshot$rv_metadata, rv_metadata_keys)
      .list_to_rv(rv_ml_ai, snapshot$rv_ml_ai, rv_ml_ai_keys)
      .list_to_rv(rv_train_control_caret, snapshot$rv_train_ctrl, rv_train_control_keys)

      # Restore sidebar tab
      if (!is.null(snapshot$active_tab)) {
        shinydashboard::updateTabItems(
          session, "dynamic_meinu_aphrc", selected = snapshot$active_tab
        )
      }

      saved_time <- format(snapshot$saved_at, "%b %d, %H:%M")
      showNotification(
        paste0("\u267b\ufe0f Session restored from ", saved_time),
        type = "message", duration = 5
      )
      return(invisible(TRUE))
    }, error = function(e) {
      warning("[session-persistence] Restore failed: ", e$message)
      return(invisible(FALSE))
    })
  }

  # ---- AUTO-SAVE TIMER (every 60 seconds) ----------------------------------
  observe({
    invalidateLater(60000, session)
    # Only save if there is data loaded
    if (!is.null(isolate(rv_current$dataset_id))) {
      .do_save(silent = TRUE)
    }
  })

  # ---- SAVE ON KEY EVENTS --------------------------------------------------
  # After dataset upload
  observeEvent(rv_current$dataset_id, {
    if (!is.null(rv_current$dataset_id)) .do_save(silent = FALSE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # After data transformation (working_df changes)
  observeEvent(rv_current$working_df, {
    .do_save(silent = TRUE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # After ML setup changes
  observeEvent(rv_ml_ai$outcome, {
    .do_save(silent = TRUE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ---- SAVE ON SESSION END -------------------------------------------------
  session$onSessionEnded(function() {
    isolate({
      tryCatch(.do_save(silent = TRUE), error = function(e) NULL)
    })
  })

  # ---- INITIAL RESTORE (called once after login) ---------------------------
  .do_restore()
}
