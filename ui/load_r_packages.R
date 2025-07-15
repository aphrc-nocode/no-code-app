# library(shiny)
# library(shinyjs)
# library(shinyvalidate)
# library(shinyalert)
# library(shinyWidgets)
# library(shinydashboard)
# library(shinydashboardPlus)
# # library(waiter)
# library(shinycssloaders)
# 
# library(sjlabelled)
# library(dplyr)
# library(stringr)
# library(stringi)
# 
# library(officer)
# library(readr)
# library(readxl)
# library(openxlsx)
# library(haven)
# library(forcats)
# 
# library(skimr)
# library(summarytools); st_options(footnote=NA, headings = FALSE)
# 
# library(countries)
# 
# library(plotly)
# library(ggplot2)
# library(DBI)
# library(RPostgreSQL)
# library(DT)
# library(bslib)
# library(gt)
# library(lubridate)
# library(dplyr)
# library(gtsummary)
# library(webshot)
# library(webshot2)
# library(shinyFiles)
# library(flextable)


libraries <- c(
  "shiny", "shinyjs", "shinyvalidate", "shinyalert", "shinyWidgets",
  "shinydashboard", "shinydashboardPlus", "shinycssloaders",
  "sjlabelled", "dplyr", "stringr", "stringi",
  "officer", "readr", "readxl", "openxlsx", "haven", "forcats",
  "skimr", "summarytools",
  "countries",
  "plotly", "ggplot2", "DBI", "RPostgreSQL", "DT", "bslib", "gt",
  "lubridate", "gtsummary", "webshot", "webshot2", "shinyFiles", "flextable","sjmisc"
  # MySQL connector package
  ,"DBI","RMySQL"
  
)

#Check if libraies have been isntalled before loading them.
for(lib in libraries){
  if(!require(lib, character.only = TRUE)){
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE)
}

st_options(footnote=NA, headings = FALSE)
