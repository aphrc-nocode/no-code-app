deploy_model_server <- function(id, rv_automl) {
  moduleServer(id, function(input, output, session) {
    print("Module deploy_model_server launched")  # Debug
    ns <- session$ns
    
    # Function to list available models
    get_saved_models <- function(models_dir = "models") {
      print(paste("getwd():", getwd()))
      if (!dir.exists(models_dir)) {
        print("Folder 'models' not found")
        return(character(0))
      }
      files <- list.files(models_dir, pattern = "\\.pkl$", full.names = FALSE)
      print(paste("Models folder detected :", toString(files)))
      model_names <- gsub("\\.pkl$", "", files)  # Remove extension
      return(model_names)
    }
    
    # Dynamic list update
    observeEvent(1, {
      models <- get_saved_models()
      print(paste("Models showed :", toString(models)))
      updateSelectInput(session, "selected_model_for_deploy", choices = models)
    }, once = TRUE)
    
    
    # Deploy button action
    observeEvent(input$deploy_model, {
      req(input$selected_model_for_deploy)
      
      res <- httr::POST("http://127.0.0.1:8000/deploy_model", body = list(
        model_id = input$selected_model_for_deploy
      ))
      
      msg <- httr::content(res, "parsed", encoding = "UTF-8")
      showNotification(msg$message %||% msg$error, type = if (!is.null(msg$error)) "error" else "message")
    })
    
    # Manual reload via button
    observeEvent(input$reload_models, {
      models <- get_saved_models()
      print(paste("Manual reloading - models :", toString(models)))
      updateSelectInput(session, "selected_model_for_deploy", choices = models)
    })
    
    
  })
}
