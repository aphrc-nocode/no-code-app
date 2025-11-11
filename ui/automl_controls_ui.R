# ui/automl_controls_ui.R
automl_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("AutoML"),
    actionButton(ns("launch_automl"), "Launch AutoML")
  )
}
