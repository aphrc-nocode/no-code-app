deploy_model_server <- function(id, rv_automl) {
  moduleServer(id, function(input, output, session) {
    print("✅ Module deploy_model_server lancé")  # Debug
    ns <- session$ns
    
    # Fonction pour lister les modèles disponibles
    get_saved_models <- function(models_dir = "models") {
      print(paste("🔍 getwd():", getwd()))
      if (!dir.exists(models_dir)) {
        print("❌ Dossier 'models' non trouvé")
        return(character(0))
      }
      files <- list.files(models_dir, pattern = "\\.pkl$", full.names = FALSE)
      print(paste("📂 Fichiers modèles détectés :", toString(files)))
      model_names <- gsub("\\.pkl$", "", files)  # Retirer l'extension
      return(model_names)
    }
    
    # Mise à jour dynamique de la liste
    observeEvent(1, {
      models <- get_saved_models()
      print(paste("📋 Modèles affichés dans le selectInput :", toString(models)))
      updateSelectInput(session, "selected_model_for_deploy", choices = models)
    }, once = TRUE)
    
    
    # Action bouton "Déployer"
    observeEvent(input$deploy_model, {
      req(input$selected_model_for_deploy)
      
      res <- httr::POST("http://127.0.0.1:8000/deploy_model", body = list(
        model_id = input$selected_model_for_deploy
      ))
      
      msg <- httr::content(res, "parsed", encoding = "UTF-8")
      showNotification(msg$message %||% msg$error, type = if (!is.null(msg$error)) "error" else "message")
    })
    
    # Rechargement manuel via bouton
    observeEvent(input$reload_models, {
      models <- get_saved_models()
      print(paste("🔁 Rechargement manuel - modèles :", toString(models)))
      updateSelectInput(session, "selected_model_for_deploy", choices = models)
    })
    
    
  })
}
