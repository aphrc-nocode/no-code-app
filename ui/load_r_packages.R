libraries <- c(
   "remotes",
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
   "htmlwidgets"
)


# Install missing CRAN packages
missing <- setdiff(libraries, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing, repos='https://cloud.r-project.org', dependencies = TRUE)

invisible(lapply(libraries, library, character.only = TRUE))

st_options(footnote=NA, headings = FALSE)

install_github_if_missing <- function(pkg, repo) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
	 remotes::install_github(repo)
	 library(pkg, character.only = TRUE)
  }
}

install_github_if_missing("DataQualityDashboard", "OHDSI/DataQualityDashboard")
install_github_if_missing("login", "jbryer/login")

tryCatch({
	pkg = "duckdb"
	if (!requireNamespace(pkg, quietly = TRUE)) {
	 install.packages(pkg, lib=NULL, repos = "https://cloud.r-project.org")
	 library(pkg, character.only = TRUE)
	}

	install_github_if_missing("Andromeda", "OHDSI/Andromeda")
	install_github_if_missing("FeatureExtraction", "OHDSI/FeatureExtraction")
}, error=function(e) {
	return(NULL)
})






