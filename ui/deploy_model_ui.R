deployment_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Light styling for UI
    tags$head(tags$style(HTML("
      .deploy-card .form-group { margin-bottom: 8px; }
      .deploy-card .selectize-control { margin-bottom: 0; }
      .deploy-card .dataTables_wrapper { margin-top: 12px; }
      .deploy-toolbar { gap: 12px; }
      @media (max-width: 992px) {
        .deploy-toolbar { flex-direction: column; align-items: stretch; }
      }
    "))),

    div(class = "card p-3 shadow-sm rounded-2xl deploy-card",

      # Titre
      h3(class = "h3 mb-3", "Validate & History"),

      # Filter (visible)
      fluidRow(
        column(6, uiOutput(ns("metric_picker"))),   # selectInput Metric (server)
        column(6, uiOutput(ns("session_filter")))   # selectInput Session (server)
      ),

      # Line: model(s) + actions
      fluidRow(
        column(
          width = 8,
          # Server-side rendered multiple selector (uiOutput -> selectizeInput)
          uiOutput(ns("bulk_selector"))
        ),
      ),

      # Main panel (below the selectors)
      div(class = "mt-2", DT::dataTableOutput(ns("validate_table")),
          br(),
          downloadBttn("validate_tabledown", label = "Download", color = "success")),

      # Info banner (optional)
      div(class = "mt-3", htmlOutput(ns("prereq_status"))),

      # Logs (if used)
      div(class = "mt-3", uiOutput(ns("logs_filters"))),
      div(class = "mt-2", DT::dataTableOutput("logs_table"),
          br(),
          downloadBttn("logs_tabledown", label = "Download", color = "success"))
    )
  )
}
