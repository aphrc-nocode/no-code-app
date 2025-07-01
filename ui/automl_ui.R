automl_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("AutoML with PyCaret"),
    numericInput(ns("session_id"), "Session ID", value = 123, min = 1),
    selectInput(ns("analysis_type"), "Analysis type",
                choices = c("Supervised" = "supervised", "Unsupervised" = "unsupervised")),
    uiOutput(ns("target_selector")),
    # Update 
    numericInput(ns("n_models"), "No of models for comparison (AutoML)", value = 10, min = 1, max = 50),
    actionButton(ns("launch_automl"), "Launch AutoML"),
    br(), br(),
    h4("Best models"),
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
    h4("Evaluation Results for Selected Model"),
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
    DTOutput(ns("leaderboard_test_table"))
    
    
    
  )
}
