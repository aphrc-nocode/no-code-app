predict_pycaret_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("predict_content"),
      uiOutput(ns("predict_pycaret_content_ui"))
    )
  )
}
