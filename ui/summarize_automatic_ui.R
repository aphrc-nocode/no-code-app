summarize_automatic_ui <- function() {
  tabItem(
    tabName = "summarizeAutomatic",
    
    tags$head(
      tags$style(HTML("
        .auto-viz-scroll {
          overflow-x: auto;
          overflow-y: auto;
          max-height: 700px;
          width: 100%;
          border: 1px solid #dcdcdc;
          border-radius: 6px;
          background: #ffffff;
          padding: 14px;
        }
        .auto-viz-main-panel {
          min-height: 650px;
        }
        .auto-viz-report-section {
          margin-bottom: 28px;
        }
        .auto-viz-report-section h3 {
          margin-top: 0;
          font-weight: 700;
        }
        .auto-viz-table-wrap {
          overflow-x: auto;
          margin-bottom: 14px;
        }
        .auto-viz-table-wrap table {
          width: 100%;
          border-collapse: collapse;
        }
        .auto-viz-table-wrap th,
        .auto-viz-table-wrap td {
          padding: 8px 10px;
          border: 1px solid #e5e7eb;
          text-align: left;
        }
      "))
    ),
    
    fluidRow(
      box(
        title = uiOutput("bivariate_header_label"),
        status = "success",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        
        fluidRow(
          column(
            width = 3,
            uiOutput("user_select_bivariate_outcome"),
            uiOutput("user_select_Bivariate_features"),
            uiOutput("user_select_color_parlet_bivariate"),
            uiOutput("bivariate_plot_title"),
            br()
          ),
          column(
            width = 9,
            div(
              class = "auto-viz-scroll auto-viz-main-panel",
              uiOutput("auto_visualization_report_view")
            )
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = uiOutput("corrplot_header_label"),
        status = "success",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,
        
        fluidRow(
          column(
            width = 3,
            uiOutput("user_select_corr_features"),
            uiOutput("user_select_color_parlet_corrplot")
          ),
          column(
            width = 9,
            div(
              class = "auto-viz-scroll",
              plotOutput("CorrPlotOutputDedicated", height = "650px", width = "100%")
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        offset = 8,
        uiOutput("user_download_autoreport")
      )
    )
  )
}