#### ---- Country capture modal + page visit tracking ---- ####

country_modal_ui <- function() {
  # Featured countries on top, then the full list from the countries package
  featured <- c("Kenya", "Uganda", "Senegal", "Ethiopia", "Ghana", "Nigeria")
  all_countries <- tryCatch(countries::list_countries(), error = function(e) character(0))
  remaining <- setdiff(all_countries, featured)
  full_list <- c("--- Select ---", featured, sort(remaining))

  modalDialog(
    title = get_rv_labels("country_modal_title"),
    footer = tagList(
      actionButton("save_country_btn", get_rv_labels("country_modal_save_btn"),
        class = "btn btn-success")
    ),
    size  = "s",
    easyClose = FALSE,
    selectInput("user_country", get_rv_labels("country_modal_label"),
      choices = full_list
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
    if (is.null(chosen) || !nzchar(chosen) || chosen == "--- Select ---") return()
    con <- DBI::dbConnect(RSQLite::SQLite(), 'users_db/users.sqlite')
    DBI::dbExecute(con,
      "UPDATE users SET country = ? WHERE username = ?",
      params = list(chosen, USER$username))
    DBI::dbDisconnect(con)
    removeModal()
  })

  # Log page visits using the sidebar tab input (server-side, no JS needed)
  last_logged_tab <- reactiveVal(NULL)
  observeEvent(input$tabs, {
    req(isTRUE(USER$logged_in))
    req(!is.null(input$tabs) && nzchar(input$tabs))
    # Avoid duplicate consecutive entries (e.g., from menu re-renders)
    if (isTRUE(input$tabs == last_logged_tab())) return()
    last_logged_tab(input$tabs)
    con <- DBI::dbConnect(RSQLite::SQLite(), 'users_db/users.sqlite')
    DBI::dbExecute(con,
      "INSERT INTO users_activity (username, action, timestamp) VALUES (?, ?, ?)",
      params = list(
        USER$username,
        paste0("page: ", input$tabs),
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    )
    DBI::dbDisconnect(con)
  }, ignoreInit = TRUE)
}
