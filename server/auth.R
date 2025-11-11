user_auth <- function(input, output, session) {
  
  USER <- login::login_server(
    id = app_login_config$APP_ID,
    db_conn = DBI::dbConnect(RSQLite::SQLite(), 'users.sqlite'),
    emailer = login::emayili_emailer(
      email_host = app_login_config$email_host,
      email_port = app_login_config$email_port,
      email_username = app_login_config$email_username,
      email_password = app_login_config$email_password,
      from_email = app_login_config$from_email
    ),
    additional_fields = c('first_name' = 'First Name',
                          'last_name' = 'Last Name'),
    cookie_name = "aphrc",
    cookie_password = "aphrcpass1"
  )
  
  output$userName <- renderText({
    paste0(USER$first_name, " ", USER$last_name)
  })
  
  observeEvent(input$logoutID, {
    shinyjs::runjs("document.cookie = 'aphrc=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;'")
    session$reload()
  })
  
  observeEvent(input$show_login, {
    shinyjs::show("login_form")
    shinyjs::hide("signup_form")
    shinyjs::hide("reset_form")
    output$form_title <- renderText("APHRC Nocode Platform")
  })
  
  observeEvent(input$show_signup, {
    shinyjs::hide("login_form")
    shinyjs::show("signup_form")
    shinyjs::hide("reset_form")
    output$form_title <- renderText("Create an Account")
  })
  
  observeEvent(input$show_reset, {
    shinyjs::hide("login_form")
    shinyjs::hide("signup_form")
    shinyjs::show("reset_form")
    output$form_title <- renderText("Reset Password")
  })
  
}
