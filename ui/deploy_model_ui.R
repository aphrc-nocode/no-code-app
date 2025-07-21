deploy_model_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Choose a model to deploy"),
    selectInput(ns("selected_model_for_deploy"), "Model to be deployed", choices = NULL),  # Choices
    actionButton(ns("deploy_model"), "Deploying this model"),
    actionButton(ns("reload_models"), "Reloading models")
    
  )
}
