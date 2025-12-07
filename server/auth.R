# server/auth.R

user_auth <- function(input, output, session) {
  # Guard: only initialize once per session
  if (isTRUE(session$userData$user_auth_initialized)) {
    return(invisible(NULL))
  }
  session$userData$user_auth_initialized <- TRUE
  
  # ---- Login module ----
  USER <- login::login_server(
    id        = app_login_config$APP_ID,
    db_conn   = DBI::dbConnect(RSQLite::SQLite(), 'users.sqlite'),
    emailer   = login::emayili_emailer(
      email_host     = app_login_config$email_host,
      email_port     = app_login_config$email_port,
      email_username = app_login_config$email_username,
      email_password = app_login_config$email_password,
      from_email     = app_login_config$from_email
    ),
    additional_fields = c(
      "first_name" = "First Name",
      "last_name"  = "Last Name"
    ),
    cookie_name     = "aphrc",
    cookie_password = "aphrcpass1"
  )
  
  # Show name somewhere if you want (e.g. in header)
  output$userName <- renderText({
    paste0(USER$first_name, " ", USER$last_name)
  })
  
  # Logout button: clear cookie and reload app
  observeEvent(input$logoutID, {
    shinyjs::runjs(
      "document.cookie = 'aphrc=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;'"
    )
    session$reload()
  }, ignoreInit = TRUE)
  
  # Toggle between login / signup / reset forms
  observeEvent(input$show_login, {
    shinyjs::show("login_form",  anim = TRUE, animType = "fade", time = 0.4)
    shinyjs::hide("signup_form")
    shinyjs::hide("reset_form")
  }, ignoreInit = TRUE)
  
  observeEvent(input$show_signup, {
    shinyjs::show("signup_form", anim = TRUE, animType = "fade", time = 0.4)
    shinyjs::hide("login_form")
    shinyjs::hide("reset_form")
  }, ignoreInit = TRUE)
  
  observeEvent(input$show_reset, {
    shinyjs::show("reset_form",  anim = TRUE, animType = "fade", time = 0.4)
    shinyjs::hide("login_form")
    shinyjs::hide("signup_form")
  }, ignoreInit = TRUE)
  
  # Show login form on first load
  observe({
    shinyjs::show("login_form",  anim = TRUE, animType = "fade", time = 0.4)
    shinyjs::hide("signup_form")
    shinyjs::hide("reset_form")
  })
  
  #return USER so server() can see USER$logged_in
  USER
}
