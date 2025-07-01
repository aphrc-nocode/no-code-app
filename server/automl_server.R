library(httr)
library(readr)

automl_server <- function(id, rv_current, rv_ml_ai) {
  moduleServer(id, function(input, output, session) {
    
    # start evaluate model Review position
    # Reactive for evaluation results
    evaluation_results <- reactiveVal(NULL)
    evaluation_images <- reactiveVal(NULL)
    prediction_results <- reactiveVal(NULL)
    # New for ggplot evaluation
    evaluation_raw_data <- reactiveVal(NULL)
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
        
        #evaluation_results(result_json$metrics)
        #evaluation_images(result_json$plots)
        
        evaluation_results(result_json$metrics)
        evaluation_raw_data(result_json$plots_data)  # <- plots_data instead of plots
        
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
      
      # test file uploaded
      test_path <- input$predict_file$datapath
      
      # train : working dataset
      tmp_train <- tempfile(fileext = ".csv")
      write.csv(rv_current$working_df, tmp_train, row.names = FALSE)
      
      # Choosen model code
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
        # Get it as a CSV file
        tf <- tempfile(fileext = ".csv")
        writeBin(httr::content(res, "raw"), tf)
        print("Raw content of file received :")
        print(readLines(tf))  # print file
        
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
          showNotification(paste("Reading error of prediction file :", e$message), type = "error")
          prediction_results(NULL)
        })
        
      } else {
        showNotification("Prediction request failed", type = "error")
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
    
    # Launch POST request to FastAPI
    observeEvent(input$launch_automl, {
      req(rv_current$working_df, input$target)
      
      tmpfile <- tempfile(fileext = ".csv")
      write.csv(rv_current$working_df, tmpfile, row.names = FALSE)
      print(paste("Valeur de input$n_models :", input$n_models))
      res <- httr::POST(
        url = "http://127.0.0.1:8000/automl",
        body = list(
          target = input$target,
          session_id = input$session_id,
          analysis_type = input$analysis_type,
          file = upload_file(tmpfile),
          n_models = input$n_models
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
          # Save global mapping
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
    
    # Show table
    output$automl_results <- DT::renderDataTable({
      df <- automl_results()
      available_models(unique(df$Model))  # U2 Store models's names
      req(df, input$analysis_type)
      
      if (!is.data.frame(df)) {
        showNotification("Bad Result : Not a 2D table", type = "error")
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
      req(evaluation_raw_data())
      tagList(
        h5("Confusion Matrix"),
        plotOutput(ns("plot_conf_matrix")),
        h5("ROC Curve"),
        plotOutput(ns("plot_roc")),
        h5("Feature Importance"),
        plotOutput(ns("plot_feature_importance")),
        # Shape values
        h5("SHAP Values"),
        plotOutput(ns("plot_shap_values"))
        
      )
    })
    # U5 end
    
    output$evaluation_metrics <- DT::renderDataTable({
      req(evaluation_results())
      DT::datatable(as.data.frame(evaluation_results()), options = list(pageLength = 5))
    })
    
    ## GGPLOT for confusion Matrix
    output$plot_conf_matrix <- renderPlot({
      req(evaluation_raw_data())
      cm <- evaluation_raw_data()$confusion_matrix
      if (is.null(cm)) return(NULL)
      df_cm <- as.data.frame(cm)
      colnames(df_cm) <- paste0("Pred_", seq_len(ncol(df_cm)))
      df_cm$Actual <- paste0("Actual_", seq_len(nrow(df_cm)))
      df_long <- reshape2::melt(df_cm, id.vars = "Actual")
      
      ggplot(df_long, aes(x = variable, y = Actual, fill = value)) +
        geom_tile() +
        geom_text(aes(label = value), color = "white", size = 6) +
        scale_fill_gradient(low = "blue", high = "red") +
        labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
        theme_minimal()
    })
    
    ## GGPLOT For ROC Curve
    
    output$plot_roc <- renderPlot({
      req(evaluation_raw_data())
      roc <- evaluation_raw_data()$roc_curve
      if (is.null(roc) || length(roc$fpr) == 0 || length(roc$tpr) == 0) return(NULL)
      
      # Convert in vectors
      df_roc <- data.frame(
        FPR = unlist(roc$fpr),
        TPR = unlist(roc$tpr)
      )
      
      # Extract AUC if possible
      auc_value <- if (!is.null(roc$auc)) round(as.numeric(roc$auc), 3) else NA
      auc_label <- if (!is.na(auc_value)) paste(" - AUC:", auc_value) else ""
      
      ggplot(df_roc, aes(x = FPR, y = TPR)) +
        geom_line(color = "blue", size = 1.2) +
        geom_abline(linetype = "dashed", color = "gray") +
        labs(title = paste("ROC Curve", auc_label),
             x = "False Positive Rate",
             y = "True Positive Rate") +
        theme_minimal()
    })
    
    
    ## GGPLOT FOR Feature importance
    
    output$plot_feature_importance <- renderPlot({
      req(evaluation_raw_data())
      feature_data <- evaluation_raw_data()$feature_importance
      if (is.null(feature_data) || length(feature_data) == 0) return(NULL)
      
      df_feat <- as.data.frame(feature_data)
      if (!("Feature" %in% colnames(df_feat) && "Importance" %in% colnames(df_feat))) {
        showNotification("Feature importance data missing expected columns", type = "error")
        return(NULL)
      }
      
      ggplot(df_feat, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(title = "Feature Importance", x = "Feature", y = "Importance") +
        theme_minimal()
    })
    
    ## Shape values
    output$plot_shap_values <- renderPlot({
      req(evaluation_raw_data())
      shap <- evaluation_raw_data()$shap_values
      if (is.null(shap) || length(shap) == 0) return(NULL)
      
      df_shap <- as.data.frame(shap)
      if (!("Feature" %in% colnames(df_shap) && "Importance" %in% colnames(df_shap))) return(NULL)
      
      ggplot(df_shap, aes(x = reorder(Feature, Importance), y = Importance)) +
        geom_bar(stat = "identity", fill = "darkorange") +
        coord_flip() +
        labs(title = "SHAP Summary (Mean Absolute Value)", x = "Feature", y = "SHAP Importance") +
        theme_minimal()
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
