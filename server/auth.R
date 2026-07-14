# server/auth.R

user_auth <- function(input, output, session) {
  # The users database is created on first run by login::login_server and is
  # NOT tracked in git. Ensure its directory exists before we connect.
  dir.create("users_db", showWarnings = FALSE, recursive = TRUE)

  # Add the 'country' column if the users table already exists. On a fresh
  # database the table has not been created yet (login_server creates it), so
  # we must not ALTER it here or startup fails with "no such table: users".
  local({
    con <- DBI::dbConnect(RSQLite::SQLite(), 'users_db/users.sqlite')
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    if ("users" %in% DBI::dbListTables(con)) {
      cols <- DBI::dbGetQuery(con, "PRAGMA table_info(users)")$name
      if (!'country' %in% cols) {
        DBI::dbExecute(con, "ALTER TABLE users ADD COLUMN country TEXT DEFAULT ''")
      }
    }
  })
  # Guard: only initialize once per session
  if (isTRUE(session$userData$user_auth_initialized)) {
    return(invisible(NULL))
  }
  session$userData$user_auth_initialized <- TRUE

  # Cookie-signing secret. MUST be set via the COOKIE_PASSWORD env var in any
  # real deployment: with a known value, session cookies can be forged. The
  # insecure fallback keeps local dev working but warns loudly.
  cookie_password <- Sys.getenv("COOKIE_PASSWORD")
  if (!nzchar(cookie_password)) {
    warning("COOKIE_PASSWORD is not set - using an insecure default. ",
            "Set COOKIE_PASSWORD before deploying.", call. = FALSE)
    cookie_password <- "aphrcpass1"
  }

  # Hold the DB connection so we can close it when the session ends, instead of
  # leaking one connection per session (the source of the "call dbDisconnect()"
  # warnings).
  auth_db_conn <- DBI::dbConnect(RSQLite::SQLite(), 'users_db/users.sqlite')
  session$onSessionEnded(function() {
    try(DBI::dbDisconnect(auth_db_conn), silent = TRUE)
  })

  # ---- Login module ----
  USER <- login::login_server(
    id        = app_login_config$APP_ID,
    db_conn   = auth_db_conn,
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
    cookie_password = cookie_password
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
