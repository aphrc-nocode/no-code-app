readRenviron(".env")
app_login_config <- list(
  email_host =  Sys.getenv("email_host"),
  email_port = Sys.getenv("email_port"),
  email_username = Sys.getenv("email_username"),
  email_password = Sys.getenv("email_password"),
  from_email = Sys.getenv("from_email"),
  APP_ID = Sys.getenv("APP_ID")
)