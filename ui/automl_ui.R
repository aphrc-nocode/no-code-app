automl_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(), br(),
    br(), br(),
    h4("Test performance (Test set)"),
    
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

  )
}
