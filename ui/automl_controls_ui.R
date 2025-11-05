# ui/automl_controls_ui.R
automl_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("AutoML with PyCaret"),
    actionButton(ns("launch_automl"), "Launch AutoML")
  )
}
