libraries <- c(
   "remotes",
	"plyr",
	"e1071",
	"nnet",
	"pls",
	"mgcv",
   "gsheet",
   "dygraphs",
   "jsonlite",
   "httr2",
   "shiny",
   "shinyjs",
   "shinyvalidate",
   "shinyalert",
   "shinyWidgets",
   "shinydashboard",
   "shinydashboardPlus",
   "shinycssloaders",
   "sjlabelled",
   "tidyr",
   "dplyr",
   "stringr",
   "stringi",
   "officer",
   "readr",
   "readxl",
   "openxlsx",
   "haven",
   "forcats",
   "skimr",
   "summarytools",
   "countries",
   "plotly",
   "RPostgreSQL",
   "DT",
   "bslib",
   "gt",
   "lubridate",
   "gtsummary",
   "webshot",
   "webshot2",
   "ggplot2",
   "shinyFiles",
   "flextable",
   "RSQLite",
   "sjmisc",
   "DBI",
   "RMySQL",
   "Achilles",
   "DatabaseConnector",
   "doParallel",
   "GGally",
   "DataExplorer",
   "htmltools",
   "promises",
   "future",
   "CodelistGenerator",
   "CDMConnector",
   "CohortConstructor",
   "RColorBrewer",
   "caret",
   "cli",
   "fastshap",
   "gemini.R",
   "naniar",
   "recipes",
   "rlang",
   "rsample",
   "shapviz",
   "callr",
   "caretEnsemble",
   "cvms",
   "digest",
   "foreach",
   "ggthemes",
   "grid",
   "httpuv",
   "httr",
   "patchwork",
   "pins",
   "plumber",
   "scales",
   "themis",
   "vetiver",
   "waiter",
   "glue",
   "zip",
   "htmlwidgets",
	"duckdb",
	"gbm",
	"MLmetrics",
	"fs"
)


# Install missing CRAN packages
missing <- setdiff(libraries, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing, repos='https://cloud.r-project.org', dependencies = TRUE)

safe_library <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    library(pkg, character.only = TRUE)
  } else {
    warning("Package '", pkg, "' not available. Some features may not work.")
  }
}

invisible(lapply(libraries, safe_library))

if (requireNamespace("summarytools", quietly = TRUE)) {
  tryCatch({
    summarytools::st_options(footnote=NA, headings = FALSE)
  }, error = function(e) {
    warning("Could not set summarytools options: ", e$message)
  })
}

install_github_if_missing <- function(pkg, repo) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    tryCatch({
      remotes::install_github(repo, upgrade="never", quiet=TRUE)
      if (requireNamespace(pkg, quietly = TRUE)) {
        library(pkg, character.only = TRUE)
      } else {
        warning("Package '", pkg, "' installation from GitHub failed. Some features may not work.")
      }
    }, error = function(e) {
      warning("Failed to install '", pkg, "' from GitHub: ", e$message, ". Some features may not work.")
    })
  } else {
    library(pkg, character.only = TRUE)
  }
}

tryCatch({
  install_github_if_missing("DataQualityDashboard", "OHDSI/DataQualityDashboard")
}, error = function(e) {
  warning("DataQualityDashboard installation failed: ", e$message)
})

tryCatch({
  install_github_if_missing("login", "jbryer/login")
}, error = function(e) {
  warning("login package installation failed: ", e$message)
})

tryCatch({
  install_github_if_missing("Andromeda", "OHDSI/Andromeda")
}, error = function(e) {
  warning("Andromeda installation failed: ", e$message)
})

tryCatch({
  install_github_if_missing("FeatureExtraction", "OHDSI/FeatureExtraction")
}, error = function(e) {
  warning("FeatureExtraction installation failed: ", e$message)
})





