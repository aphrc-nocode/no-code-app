readRenviron(".env")
app_login_config <- list(
  email_host =  Sys.getenv("email_host"),
  email_port = Sys.getenv("email_port"),
  email_username = Sys.getenv("email_username"),
  email_password = Sys.getenv("email_password"),
  from_email = Sys.getenv("from_email"),
  APP_ID = Sys.getenv("APP_ID")
)

## Data privacy / retention settings
app_privacy_config <- list(
  # Bumping the version re-prompts every user for consent
  policy_version = Sys.getenv("privacy_policy_version", "1.0"),
  retention_months = suppressWarnings(
    as.integer(Sys.getenv("data_retention_months", "3"))
  )
)
if (is.na(app_privacy_config$retention_months)) {
  app_privacy_config$retention_months <- 3L
}