automl_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("AutoML with PyCaret"),
    numericInput(ns("session_id"), "Session ID", value = 123, min = 1),
    selectInput(ns("analysis_type"), "Analysis type",
                choices = c("Supervised" = "supervised", "Unsupervised" = "unsupervised")),
    uiOutput(ns("target_selector")),
    actionButton(ns("launch_automl"), "Launch AutoML"),
    br(), br(),
    h4("Best models"),
    #br(), br(),
    DT::dataTableOutput(ns("automl_results")),
    ## New bloc 1 start 
    br(),
    h4("Select a model for detailed evaluation"),
    uiOutput(ns("model_selector")),  # <--- UI dynamique pour le dropdown
    ## New bloc 1 end 
    
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
