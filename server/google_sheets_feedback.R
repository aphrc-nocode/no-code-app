# server/google_sheets_feedback.R
#
# Handles Google Sheets configuration, authentication,
# validation, and feedback submission.

get_google_feedback_label <- function(key) {
  
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


get_feedback_sheet_config <- function() {
  
  sheet_id <- trimws(
    Sys.getenv(
      "GOOGLE_FEEDBACK_SHEET_ID",
      unset = ""
    )
  )
  
  google_account <- trimws(
    Sys.getenv(
      "GOOGLE_FEEDBACK_ACCOUNT",
      unset = ""
    )
  )
  
  token_cache <- trimws(
    Sys.getenv(
      "GOOGLE_FEEDBACK_TOKEN_CACHE",
      unset = ".secrets"
    )
  )
  
  worksheet <- trimws(
    Sys.getenv(
      "GOOGLE_FEEDBACK_WORKSHEET",
      unset = "1"
    )
  )
  
  
  if (!nzchar(sheet_id)) {
    stop(
      get_google_feedback_label(
        "feedback_sheet_id_not_configured"
      ),
      call. = FALSE
    )
  }
  
  
  if (!nzchar(google_account)) {
    stop(
      get_google_feedback_label(
        "feedback_google_account_not_configured"
      ),
      call. = FALSE
    )
  }
  
  
  if (!nzchar(token_cache)) {
    stop(
      get_google_feedback_label(
        "feedback_token_cache_not_configured"
      ),
      call. = FALSE
    )
  }
  
  
  if (!nzchar(worksheet)) {
    stop(
      get_google_feedback_label(
        "feedback_worksheet_not_configured"
      ),
      call. = FALSE
    )
  }
  
  
  list(
    sheet_id = sheet_id,
    google_account = google_account,
    token_cache = token_cache,
    worksheet = worksheet
  )
}


authenticate_feedback_sheet <- function() {
  
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop(
      get_google_feedback_label(
        "feedback_googlesheets_package_missing"
      ),
      call. = FALSE
    )
  }
  
  
  config <- get_feedback_sheet_config()
  
  
  app_root <- normalizePath(
    ".",
    winslash = "/",
    mustWork = TRUE
  )
  
  
  token_cache_path <- if (
    grepl(
      "^(?:[A-Za-z]:[/\\\\]|/)",
      config$token_cache
    )
  ) {
    config$token_cache
  } else {
    file.path(
      app_root,
      config$token_cache
    )
  }
  
  
  if (!dir.exists(token_cache_path)) {
    
    created <- dir.create(
      token_cache_path,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    if (!created && !dir.exists(token_cache_path)) {
      stop(
        get_google_feedback_label(
          "feedback_token_cache_creation_failed"
        ),
        call. = FALSE
      )
    }
  }
  
  
  authentication_result <- tryCatch({
    
    googlesheets4::gs4_auth(
      email = config$google_account,
      cache = token_cache_path,
      scopes = "https://www.googleapis.com/auth/spreadsheets"
    )
    
    TRUE
    
  }, error = function(e) {
    
    stop(
      paste(
        get_google_feedback_label(
          "feedback_google_authentication_failed"
        ),
        conditionMessage(e)
      ),
      call. = FALSE
    )
  })
  
  
  if (!isTRUE(authentication_result)) {
    stop(
      get_google_feedback_label(
        "feedback_google_authentication_failed"
      ),
      call. = FALSE
    )
  }
  
  
  authenticated_user <- tryCatch(
    googlesheets4::gs4_user(),
    error = function(e) NULL
  )
  
  
  if (is.null(authenticated_user)) {
    stop(
      get_google_feedback_label(
        "feedback_google_user_not_authenticated"
      ),
      call. = FALSE
    )
  }
  
  
  config$token_cache <- token_cache_path
  
  invisible(config)
}


normalise_feedback_for_sheet <- function(feedback_row) {
  
  if (!is.data.frame(feedback_row)) {
    stop(
      get_google_feedback_label(
        "feedback_row_not_dataframe"
      ),
      call. = FALSE
    )
  }
  
  
  if (nrow(feedback_row) != 1L) {
    stop(
      get_google_feedback_label(
        "feedback_row_invalid_count"
      ),
      call. = FALSE
    )
  }
  
  
  if (ncol(feedback_row) == 0L) {
    stop(
      get_google_feedback_label(
        "feedback_row_has_no_columns"
      ),
      call. = FALSE
    )
  }
  
  
  if (
    is.null(names(feedback_row)) ||
    any(!nzchar(names(feedback_row)))
  ) {
    stop(
      get_google_feedback_label(
        "feedback_row_missing_column_names"
      ),
      call. = FALSE
    )
  }
  
  
  feedback_row[] <- lapply(
    feedback_row,
    function(column) {
      
      if (
        inherits(
          column,
          c(
            "POSIXct",
            "POSIXlt",
            "Date"
          )
        )
      ) {
        return(as.character(column))
      }
      
      
      if (is.factor(column)) {
        return(as.character(column))
      }
      
      
      if (is.list(column)) {
        return(
          vapply(
            column,
            function(value) {
              
              if (
                is.null(value) ||
                length(value) == 0
              ) {
                return("")
              }
              
              paste(
                as.character(value),
                collapse = "; "
              )
            },
            character(1)
          )
        )
      }
      
      
      column
    }
  )
  
  
  feedback_row[] <- lapply(
    feedback_row,
    function(column) {
      
      if (is.character(column)) {
        column[is.na(column)] <- ""
      }
      
      if (is.numeric(column)) {
        column[is.na(column)] <- NA_real_
      }
      
      if (is.logical(column)) {
        column[is.na(column)] <- FALSE
      }
      
      column
    }
  )
  
  
  feedback_row
}


append_feedback_to_google_sheet <- function(feedback_row) {
  
  config <- authenticate_feedback_sheet()
  
  feedback_row <- normalise_feedback_for_sheet(
    feedback_row
  )
  
  
  worksheet <- if (
    grepl(
      "^[0-9]+$",
      config$worksheet
    )
  ) {
    as.integer(config$worksheet)
  } else {
    config$worksheet
  }
  
  
  append_result <- tryCatch({
    
    googlesheets4::sheet_append(
      ss = config$sheet_id,
      data = feedback_row,
      sheet = worksheet
    )
    
    TRUE
    
  }, error = function(e) {
    
    stop(
      paste(
        get_google_feedback_label(
          "feedback_google_sheet_append_failed"
        ),
        conditionMessage(e)
      ),
      call. = FALSE
    )
  })
  
  
  if (!isTRUE(append_result)) {
    stop(
      get_google_feedback_label(
        "feedback_google_sheet_append_failed"
      ),
      call. = FALSE
    )
  }
  
  
  invisible(TRUE)
}