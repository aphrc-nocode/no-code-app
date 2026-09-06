#### ---- Data privacy consent modal ---- ####

## Retention period comes from .env, never from the translated text
get_privacy_label <- function(key) {
  gsub("{months}", app_privacy_config$retention_months, get_rv_labels(key), fixed = TRUE)
}

data_privacy_modal_ui <- function() {
  modalDialog(
    title = get_privacy_label("data_privacy_modal_title"),
    footer = tagList(
      actionButton("privacy_decline_btn", get_privacy_label("data_privacy_modal_decline_btn"),
        class = "btn btn-outline-secondary"),
      shinyjs::disabled(
        actionButton("privacy_accept_btn", get_privacy_label("data_privacy_modal_accept_btn"),
          class = "btn btn-success")
      )
    ),
    size = "m",
    easyClose = FALSE,
    div(class = "privacy-modal-body",
      p(get_privacy_label("data_privacy_modal_intro")),
      tags$ul(
        tags$li(get_privacy_label("data_privacy_modal_point_storage")),
        tags$li(get_privacy_label("data_privacy_modal_point_access")),
        tags$li(get_privacy_label("data_privacy_modal_point_retention")),
        tags$li(get_privacy_label("data_privacy_modal_point_control"))
      )
    ),
    # Kept outside the scrollable body so it stays visible with longer policies
    div(class = "privacy-modal-consent",
      checkboxInput("privacy_agree_chk", get_privacy_label("data_privacy_modal_agree_label"),
        value = FALSE)
    )
  )
}

## Returns a reactive that is TRUE once consent for the current policy version
## exists, so downstream onboarding steps can wait for it.
data_privacy_server <- function(USER) {

  consent_ok <- reactiveVal(FALSE)

  # Show modal after login if the current policy version was not accepted yet
  observeEvent(USER$logged_in, {
    req(isTRUE(USER$logged_in))
    con <- DBI::dbConnect(RSQLite::SQLite(), 'users_db/users.sqlite')
    row <- DBI::dbGetQuery(con,
      "SELECT privacy_version FROM users WHERE username = ?",
      params = list(USER$username))
    DBI::dbDisconnect(con)
    accepted <- nrow(row) > 0 && !is.na(row$privacy_version[1]) &&
      identical(trimws(row$privacy_version[1]), app_privacy_config$policy_version)
    if (accepted) {
      consent_ok(TRUE)
      return()
    }
    modal_ui <- data_privacy_modal_ui()
    session$onFlushed(function() {
      waiter::waiter_hide()
      showModal(modal_ui)
    }, once = TRUE)
  })

  # Consent has to be deliberate: the checkbox unlocks the accept button
  observeEvent(input$privacy_agree_chk, {
    shinyjs::toggleState("privacy_accept_btn", condition = isTRUE(input$privacy_agree_chk))
  }, ignoreInit = TRUE)

  observeEvent(input$privacy_accept_btn, {
    req(isTRUE(USER$logged_in))
    req(isTRUE(input$privacy_agree_chk))
    accepted_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    tryCatch({
      con <- DBI::dbConnect(RSQLite::SQLite(), 'users_db/users.sqlite')
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExecute(con,
        "UPDATE users SET privacy_version = ?, privacy_accepted_at = ? WHERE username = ?",
        params = list(app_privacy_config$policy_version, accepted_at, USER$username))
      DBI::dbExecute(con,
        "INSERT INTO users_activity (username, action, timestamp) VALUES (?, ?, ?)",
        params = list(
          USER$username,
          paste0("privacy: accepted v", app_privacy_config$policy_version),
          accepted_at
        )
      )
      message(sprintf("Data privacy notice accepted by '%s' (v%s)",
        USER$username, app_privacy_config$policy_version))
      removeModal()
      # Let the dialog finish closing before the next onboarding modal opens,
      # otherwise Bootstrap strips the backdrop of the modal that follows
      shinyjs::delay(450, {
        shinyjs::runjs("
          $('.modal-backdrop').remove();
          $('body').removeClass('modal-open').css('padding-right', '');
        ")
        consent_ok(TRUE)
      })
    }, error = function(e) {
      message(sprintf("Data privacy consent failed for user '%s': %s",
        USER$username, conditionMessage(e)))
      shinyalert::shinyalert("Error", paste0(get_rv_labels("general_error_alert"), "\n", conditionMessage(e)), type = "error")
    })
  })

  # Declining means the session ends, same path as the header logout button
  observeEvent(input$privacy_decline_btn, {
    showNotification(get_privacy_label("data_privacy_modal_decline_msg"),
      type = "warning", duration = 4)
    removeModal()
    shinyjs::delay(2000, {
      shinyjs::runjs(
        "document.cookie = 'aphrc=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;'"
      )
      session$reload()
    })
  })

  reactive(consent_ok())
}
