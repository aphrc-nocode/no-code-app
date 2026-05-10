#### ---- Country capture modal + page visit tracking ---- ####

country_modal_ui <- function() {
  modalDialog(
    title = get_rv_labels("country_modal_title"),
    footer = tagList(
      actionButton("save_country_btn", get_rv_labels("country_modal_save_btn"),
        class = "btn btn-success")
    ),
    size  = "s",
    easyClose = FALSE,
    selectInput("user_country", get_rv_labels("country_modal_label"),
      choices = c(
        "--- Select ---",
        "Kenya", "Uganda", "Senegal", "Ethiopia", "Ghana", "Nigeria",
        "Europe",
        "Other"
      )
    ),
    conditionalPanel(
      condition = "input.user_country == 'Other'",
      textInput("user_country_other", get_rv_labels("country_modal_other_label"),
        placeholder = get_rv_labels("country_modal_other_ph"))
    )
  )
}

location_modal_server <- function(USER) {

  # Show modal after login if country not yet set
  observeEvent(USER$logged_in, {
    req(isTRUE(USER$logged_in))
    con <- DBI::dbConnect(RSQLite::SQLite(), 'users_db/users.sqlite')
    row <- DBI::dbGetQuery(con,
      "SELECT country FROM users WHERE username = ?",
      params = list(USER$username))
    DBI::dbDisconnect(con)
    country_set <- nrow(row) > 0 && !is.na(row$country[1]) && nzchar(trimws(row$country[1]))
    if (!country_set) showModal(country_modal_ui())
  })

  # Save country on submit
  observeEvent(input$save_country_btn, {
    req(isTRUE(USER$logged_in))
    chosen <- input$user_country
    if (chosen == "Other") chosen <- trimws(input$user_country_other)
    if (is.null(chosen) || !nzchar(chosen) || chosen == "--- Select ---") return()
    con <- DBI::dbConnect(RSQLite::SQLite(), 'users_db/users.sqlite')
    DBI::dbExecute(con,
      "UPDATE users SET country = ? WHERE username = ?",
      params = list(chosen, USER$username))
    DBI::dbDisconnect(con)
    removeModal()
  })

  # Log page visits using actual sidebar click (bypasses input$tabs re-render bug)
  observeEvent(input$actual_page_visit, {
    req(isTRUE(USER$logged_in))
    tab <- input$actual_page_visit$tab
    req(!is.null(tab) && nzchar(tab))
    con <- DBI::dbConnect(RSQLite::SQLite(), 'users_db/users.sqlite')
    DBI::dbExecute(con,
      "INSERT INTO users_activity (username, action, timestamp) VALUES (?, ?, ?)",
      params = list(
        USER$username,
        paste0("page: ", tab),
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    )
    DBI::dbDisconnect(con)
  }, ignoreInit = TRUE)
}
