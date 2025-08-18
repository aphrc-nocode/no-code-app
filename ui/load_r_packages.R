libraries <- c(
  "shiny"
  , "shinyjs"
  , "shinyvalidate"
  , "shinyalert"
  , "shinyWidgets"
  , "shinydashboard"
  , "shinydashboardPlus"
  , "shinycssloaders"
  , "sjlabelled"
  , "dplyr"
  , "stringr"
  , "stringi"
  , "officer"
  , "readr"
  , "readxl"
  , "openxlsx"
  , "haven"
  , "forcats"
  , "skimr"
  , "summarytools"
  , "countries"
  , "plotly"
  , "ggplot2"
  , "RPostgreSQL"
  , "DT"
  , "bslib"
  , "gt"
  , "lubridate"
  , "gtsummary"
  , "webshot"
  , "webshot2"
  , "shinyFiles"
  , "flextable"
  , "RSQLite"
  ,"sjmisc"
  ,"DBI"
  ,"RMySQL"
  , "remotes"
  ,"Achilles"
  , "DatabaseConnector"
  , "doParallel"
  , "doParallel"
  ,"GGally"
  ,"DataExplorer"
  ,"htmltools"
)


#Check if libraies have been isntalled before loading them.
for(lib in libraries){
  if(!require(lib, character.only = TRUE)){
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE)
}

st_options(footnote=NA, headings = FALSE)



install_github_if_missing <- function(pkg, repo) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    remotes::install_github(repo)
    library(pkg)
  }
}

install_github_if_missing("DataQualityDashboard", "OHDSI/DataQualityDashboard")
install_github_if_missing("login", "jbryer/login")







