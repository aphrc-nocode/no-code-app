# ui/train_model_ui.R
train_model_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Train performance & Evaluation"),
    strong("Status: "), textOutput(ns("job_status")),
    br(),

    # === When PyCaret was launched and completed ===
    conditionalPanel(
      condition = sprintf("output['%s'] == true", ns("train_ready")),
      tagList(
        h4("Train performance (Cross-validation)"),
        # Filter Top-N (for TRAIN & TEST)
        sliderInput(ns("top_n"), "Show top N models:", min = 1, max = 50, value = 10, step = 1),
        DT::dataTableOutput(ns("leaderboard_train_table")),
        br(),
        shiny::downloadButton("leaderboard_train_tabledown", label = "donwload"),
        br(),
        h4("Test performance"),
        DT::dataTableOutput(ns("test_leaderboard_table")),
        br(),
        shiny::downloadButton("test_leaderboard_tabledown", label = "donwload"),
        br(),
        h4("Select a model for detailed evaluation"),
        uiOutput(ns("model_selector")),
        br(),
        shiny::downloadButton("model_selectordonw", label = "donwload"),
        br(),
        h4("Metrics (Test set)"),
        uiOutput(ns("metrics_table")),
        br(),
        shiny::downloadButton("metrics_tabledown", label = "donwload"),
        br(),
        h4("Evaluation plots"),
        tabsetPanel(
          id = ns("plots_tabs"), type = "tabs",
          tabPanel("ROC Curve",uiOutput(ns("roc_ui")),
                   br(),shiny::downloadButton("roc_uidown", label = "donwload")),
          tabPanel("Confusion Matrix",   uiOutput(ns("cm_ui")),
                   br(),shiny::downloadButton("cm_uidown", label = "donwload")),
          tabPanel("Feature Importance", uiOutput(ns("fi_ui")),
                   br(),shiny::downloadButton("fi_uidown", label = "donwload")),
          tabPanel("SHAP Values", uiOutput(ns("shap_ui")),
                   br(),shiny::downloadButton("shap_uidown", label = "donwload"))
        )
      )
    ),
    # === Waiting screen until PyCaret/AutoML is ready ===
    conditionalPanel(
      condition = sprintf("output['%s'] != true", ns("train_ready")),
      div(
        style = "padding:16px;border:1px dashed #ddd;background:#fafafa;border-radius:8px;",
        h4("Train – on hold"),
        p(
          "Select the option ", strong("PyCaret"),
          " On the Feature Engineering page, click ",
          strong("Launch AutoML"),
          " then launch the ", strong("Training"), "."
        )
      )
    )
  )
}
