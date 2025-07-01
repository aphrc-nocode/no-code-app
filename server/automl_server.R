library(httr)
library(readr)

automl_server <- function(id, rv_current, rv_ml_ai) {
  moduleServer(id, function(input, output, session) {
    
    # start evaluate model Review position
    # Reactive for evaluation results
    evaluation_results <- reactiveVal(NULL)
    evaluation_images <- reactiveVal(NULL)
    prediction_results <- reactiveVal(NULL)
    #automl_test_results <- reactiveVal(NULL)
    
    
    
    observeEvent(input$selected_model, {
      req(rv_current$working_df, input$target, input$selected_model)
      
      tmpfile <- tempfile(fileext = ".csv")
      write.csv(rv_current$working_df, tmpfile, row.names = FALSE)
      
      model_code <- rv_ml_ai$model_mapping[[input$selected_model]]
      
      res <- httr::POST(
        url = "http://127.0.0.1:8000/evaluate_model",
        body = list(
          file = upload_file(tmpfile),
          target = input$target,
          #model_name = input$selected_model,
          model_name = model_code,
          session_id = input$session_id
        ),
        encode = "multipart"
      )
      
      if (res$status_code == 200) {
        content_raw <- httr::content(res, as = "text")
        result_json <- jsonlite::fromJSON(content_raw, flatten = TRUE)
        
        evaluation_results(result_json$metrics)
        evaluation_images(result_json$plots)
        
        print("Model evaluation completed.")
        print(result_json$model_name)
      } else {
        showNotification("Error during model evaluation", type = "error")
        evaluation_results(NULL)
        evaluation_images(NULL)
      }
    })
    
    # end evaluate model
    
    # start prediction step
    
    observeEvent(input$predict_model_btn, {
      req(rv_current$working_df, input$target, input$selected_model, input$predict_file)
      
      # fichier test uploadé
      test_path <- input$predict_file$datapath
      
      # train : jeu de données courant
      tmp_train <- tempfile(fileext = ".csv")
      write.csv(rv_current$working_df, tmp_train, row.names = FALSE)
      
      # code du modèle sélectionné
      model_code <- rv_ml_ai$model_mapping[[input$selected_model]]
      
      res <- httr::POST(
        url = "http://127.0.0.1:8000/predict_model",
        body = list(
          train_file = upload_file(tmp_train),
          test_file = upload_file(test_path),
          target = input$target,
          model_name = model_code,
          session_id = input$session_id
        ),
        encode = "multipart"
      )
      
      if (res$status_code == 200) {
        # Récupération en tant que fichier CSV
        tf <- tempfile(fileext = ".csv")
        writeBin(httr::content(res, "raw"), tf)
        print("Raw content of file received :")
        print(readLines(tf))  # affiche le fichier tel quel
        
        tryCatch({
          library(readr)
          
          df_pred <- tryCatch({
            read_csv(tf, show_col_types = FALSE)
          }, error = function(e1) {
            tryCatch({
              read_csv(tf, delim = ";", show_col_types = FALSE)
            }, error = function(e2) {
              showNotification("Cannot read CSV file (even if fallback)", type = "error")
              print("Reading error CSV :")
              print(e1$message)
              prediction_results(NULL)
              return(NULL)
            })
          })
          prediction_results(df_pred)
          showNotification("Successful predictions generated", type = "message")
        }, error = function(e) {
          showNotification(paste("Erreur lecture du fichier prédiction :", e$message), type = "error")
          prediction_results(NULL)
        })
        
      } else {
        showNotification("Prediciton request failed", type = "error")
        prediction_results(NULL)
      }
    })
    # end prediction step
    
    ns <- session$ns
    
    # Reactive value to store AutoML result
    automl_results <- reactiveVal(NULL)
    
    # Reactive value to store model name from leaderboard
    available_models <- reactiveVal(NULL) # U1
    
    
    # UI: Sélection de la variable cible
    output$target_selector <- renderUI({
      req(rv_current$working_df)
      selectInput(ns("target"),
                  "Choose target",
                  choices = names(rv_current$working_df))
    })
    
    # Lancement du POST vers FastAPI
    observeEvent(input$launch_automl, {
      req(rv_current$working_df, input$target)
      
      tmpfile <- tempfile(fileext = ".csv")
      write.csv(rv_current$working_df, tmpfile, row.names = FALSE)
      
      res <- httr::POST(
        url = "http://127.0.0.1:8000/automl",
        body = list(
          target = input$target,
          session_id = input$session_id,
          analysis_type = input$analysis_type,
          file = upload_file(tmpfile)
        ),
        encode = "multipart"
      )
      
      if (res$status_code == 200) {
        content_raw <- httr::content(res, as = "text")
        result_json <- jsonlite::fromJSON(content_raw, flatten = TRUE)
        
        print("API response result:")
        print(str(result_json))
        
        if (input$analysis_type == "supervised" && !is.null(result_json$leaderboard)) {
          df <- as.data.frame(result_json$leaderboard)
          
          print("Columns of leaderboard :")
          print(names(df))
          
          print("Overview of leaderboard :")
          print(head(df))
          automl_results(df)
          available_models(df$Model)
          # Sauvegarde du mapping global
          if ("model_id" %in% names(df) && "Model" %in% names(df)) {
            rv_ml_ai$model_mapping <- setNames(df$model_id, df$Model)
          } else {
            showNotification("Erreur : 'model_id' ou 'Model' missed in the leaderboard", type = "error")
            rv_ml_ai$model_mapping <- NULL
          }
          
          
        } else if (input$analysis_type == "unsupervised" && !is.null(result_json$clustering_result)) {
          df <- as.data.frame(result_json$clustering_result)
          print("Overview of clusters :")
          print(head(df))
          automl_results(df)
          
        } else {
          showNotification("No valid result returned from AutoML", type = "warning")
          automl_results(NULL)
        }
      } else {
        showNotification("Error while calling the AutoML API", type = "error")
      }
    })
    
    # Affichage du tableau
    output$automl_results <- DT::renderDataTable({
      df <- automl_results()
      available_models(unique(df$Model))  # U2 Store models's names
      req(df, input$analysis_type)
      
      if (!is.data.frame(df)) {
        showNotification("Bad Résultat : Not a 2D table", type = "error")
        return(NULL)
      }
      
      caption_txt <- if (input$analysis_type == "supervised") {
        "Leaderboard of supervised models"
      } else {
        "Clustering result"
      }
      
      DT::datatable(df, caption = caption_txt, options = list(pageLength = 10))
    })
    
    # Donwload button
    output$download_ui <- renderUI({
      req(automl_results())
      downloadButton(ns("download_automl_results"), "Download result (.csv)")
    })
    
    # U3 start and UI for show models dropdown list
    
    output$model_selector <- renderUI({
      req(available_models())
      selectInput(ns("selected_model"),
                  "Choose a model",
                  choices = available_models(),
                  selected = available_models()[1])
    })
    # U3 end
    
    
    output$download_automl_results <- downloadHandler(
      filename = function() {
        paste0("AutoML_Results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(automl_results(), file, row.names = FALSE)
      }
    )
    # U4 start
    observe({
      req(input$selected_model)
      if (exists("rv_ml_ai")) {
        rv_ml_ai$selected_model <- input$selected_model
      }
    })
    # U4 end
    # U5 start
    # U5 start
    output$model_plots <- renderUI({
      req(evaluation_images())
      
      tags$div(
        h5("Confusion Matrix"),
        tags$img(src = paste0("data:image/png;base64,", evaluation_images()$confusion_matrix), width = "100%"),
        h5("ROC Curve"),
        tags$img(src = paste0("data:image/png;base64,", evaluation_images()$roc_curve), width = "100%"),
        h5("Feature Importance"),
        tags$img(src = paste0("data:image/png;base64,", evaluation_images()$feature_importance), width = "100%")
      )
        })
    # U5 end
    

    
    output$evaluation_metrics <- DT::renderDataTable({
      req(evaluation_results())
      DT::datatable(as.data.frame(evaluation_results()), options = list(pageLength = 5))
    })
    
    # U6 for prediction
    output$prediction_results <- DT::renderDataTable({
      req(prediction_results())
      DT::datatable(prediction_results(), options = list(pageLength = 10))
    })
    
    # U6 for prediction end
    
    # U7 for download button prediction start
    output$download_prediction_ui <- renderUI({
      req(prediction_results())
      if (nrow(prediction_results()) > 0) {
        downloadButton(ns("download_prediction"), "Download predictions (.csv)")
      }
    })
    
    
    output$download_prediction <- downloadHandler(
      filename = function() {
        paste0("Predictions_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(prediction_results(), file, row.names = FALSE)
      }
    )
    
    # U7 for download button prediction end
    
    
  })
}
