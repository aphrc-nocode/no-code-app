automl_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("AutoML with PyCaret"),
    uiOutput(ns("target_selector")),
    actionButton(ns("launch_automl"), "Launch AutoML"),
    br(), br(),
    h4("Best models"),
    br(), br(),
    
    ## Add a dynamic container that show button only if there is  a table
    uiOutput(ns("download_ui")),
    br(), br(),

    DT::dataTableOutput(ns("automl_results"))
  )
}
