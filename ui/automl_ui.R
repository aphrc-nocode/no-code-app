automl_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("AutoML with PyCaret"),
    # Update 
    numericInput(ns("n_models"), "No of models for comparison (AutoML)", value = 10, min = 1, max = 50),
    actionButton(ns("launch_automl"), "Launch AutoML"),
    br(), br(),
    h4("Train performance (Cross-validation)"),
    #br(), br(),
    DT::dataTableOutput(ns("automl_results")),
    ## New bloc 1 start 
    br(),
    h4("Select a model for detailed evaluation"),
    uiOutput(ns("model_selector")),  # <--- Dynamic UI  for dropdown
    ## New bloc 1 end 
    
    tabsetPanel(
      tabPanel("Confusion Matrix", plotOutput(ns("plot_conf_matrix"))),
      tabPanel("ROC Curve", plotOutput(ns("plot_roc"))),
      tabPanel("Feature Importance", plotOutput(ns("plot_feature_importance"))),
      tabPanel("Shape values", plotOutput(ns("plot_shap_values"))),
    ),
    
    ## Add a dynamic container that show button only if there is  a table
    uiOutput(ns("download_ui")),
    # New bloc 2 start
    br(), br(),
    #uiOutput(ns("model_plots")),
    #br(),
    h4("Test performance (Test set)"),
    DT::dataTableOutput(ns("evaluation_metrics")),
    # New bloc 2 end
    
    h4("Upload new data for prediction"),
    fileInput(ns("predict_file"), "Upload test dataset (.csv)"),
    actionButton(ns("predict_model_btn"), "Run Prediction"),
    br(), br(),
    DT::dataTableOutput(ns("prediction_results")),
    uiOutput(ns("download_prediction_ui")),
    br(), br(),
    DTOutput(ns("leaderboard_train_table")),
    DTOutput(ns("leaderboard_test_table")),
    DT::dataTableOutput(ns("deployed_predictions"))

    # Deploy
    
    
    
    #,h4("Train performance (Cross-validation)")
    #,DT::dataTableOutput(ns("leaderboard_table"))
    #,h4("Test performance (Holdout/Test set)")
    #,DT::dataTableOutput(ns("evaluation_metrics_table"))
  )
}
