deploy_model_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Choisissez un modèle à déployer"),
    selectInput(ns("selected_model_for_deploy"), "Modèle à déployer", choices = NULL),  # Choices remplis dynamiquement
    actionButton(ns("deploy_model"), "Deployer ce modèle"),
    actionButton(ns("reload_models"), "🔄 Recharger les modèles")
    
  )
}
