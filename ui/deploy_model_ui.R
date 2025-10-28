deployment_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "card p-3 shadow-sm rounded-2xl",
      h2(class = "h5 mb-2", "Model Deployment"),

      # ── 0) History & Validation (always visible)
      div(class = "card p-3 mb-3",
        h3(class = "h6 mb-2", "Validate & History"),
        # ── 1) Deployed models (actions)
        div(class = "card p-3 mt-3",
          h3(class = "h6 mb-2", "Deployed models"),
          div(DT::dataTableOutput(ns("validate_table")),

          )
        ),


        uiOutput(ns("logs_filters")),                 # dynamic filters (session / metric / framework)
        div(class = "mt-2", DT::dataTableOutput(ns("logs_table")))  # log table
      ),

      # ── 1) Info banner if prerequisites are not met
      conditionalPanel(
        condition = sprintf("!output['%s']", ns("ready_flag")),
        div(class = "alert alert-info", htmlOutput(ns("prereq_status")))
      ),

      # ── 2) Main body (when ready)
      conditionalPanel(
        condition = sprintf("output['%s']", ns("ready_flag")),
        fluidRow(
          # Left column: VALIDATE & DEPLOY + selection/deployment
          column(12,
            # ===== Validate & Deploy (metric-driven) =====
            div(class = "card p-3 mb-3",
              # metric picker (provided/updated by the server)
              # -> if you prefer a static selectInput: selectInput(ns(“deploy_metric”), “Metric,” choices = NULL)
              # fields side by side
              fluidRow(
                column(
                  width = 6,
                  uiOutput(ns("metric_picker"))
                ),
                column(
                  width = 6,
                  uiOutput(ns("session_filter"))
                )
              ),

              # dynamic table (columns: date_trained, dataset_id, model, metric, estimate, …)
              div(class = "mt-2", DT::dataTableOutput(ns("deployed_table")))
            )
          )
        )
      )
    )
  )
}
