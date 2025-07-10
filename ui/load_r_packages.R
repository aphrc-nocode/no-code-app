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
  , "DBI"
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
  , "login"
  , "RSQLite"
  ,"sjmisc"
  ,"DBI"
  ,"RMySQL"
)


#Check if libraies have been isntalled before loading them.
for(lib in libraries){
  if(!require(lib, character.only = TRUE)){
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE)
}

st_options(footnote=NA, headings = FALSE)
