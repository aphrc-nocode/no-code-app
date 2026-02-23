if (!file.exists("server.R")) {
    cat("Error: server.R not found. Please run this script from the no-code-app directory.\n")
    cat("Current directory:", getwd(), "\n")
    stop("Wrong directory")
}

R_USER_LIB <- Sys.getenv("R_USER_LIB", paste0(Sys.getenv("HOME"), "/R/library"))
if (dir.exists(R_USER_LIB)) {
    .libPaths(c(R_USER_LIB, .libPaths()))
}

Sys.setenv("FASTAPI_BASE" = Sys.getenv("FASTAPI_BASE", "http://localhost:8000"))
Sys.setenv("R_CARET_MAX_WORKERS" = Sys.getenv("R_CARET_MAX_WORKERS", "15"))

cat("Starting R Shiny App...\n")
cat("Working directory:", getwd(), "\n")
cat("FastAPI URL:", Sys.getenv("FASTAPI_BASE"), "\n")
cat("R Caret Workers:", Sys.getenv("R_CARET_MAX_WORKERS"), "\n")
cat("App will be available at: http://localhost:3838\n")
cat("\nPress Ctrl+C to stop the app\n\n")

shiny::runApp(port = 3838, host = "0.0.0.0")
