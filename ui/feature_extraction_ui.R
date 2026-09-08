feature_extraction_ui <- function() {
  tabItem(
    tabName = "FeatureExtraction",
    fluidPage(
      tags$style(
        HTML("
          table thead tr th {
            background-color: #4CAF50;
            color: white;
          }
          .btn-success {
            background-color: #228B22 !important;
            border-color: #228B22 !important;
            color: white !important;
            font-weight: bold;
          }
        ")
      ),
      
      titlePanel("Feature Extraction Tool"),
      
      conditionalPanel(
        condition = "output.dbConnected == false",
        uiOutput("schema_feature")
      ),
      
      conditionalPanel(
        condition = "output.dbConnected == true",
        sidebarLayout(
          sidebarPanel(
            uiOutput("cdm_schema_ui"),
            uiOutput("results_schema_ui"),
            uiOutput("cohort_table_ui"),
            uiOutput("domain_choices_ui"),
            
            textInput(
              "output_csv",
              "Output CSV File Name:",
              value = "features.csv",
              placeholder = "Enter file name, e.g. features.csv"
            ),
            
            actionButton(
              "extract_features",
              "Extract Features",
              class = "btn-success"
            )
          ),
          
          mainPanel(
            h4("Instructions"),
            p("1. Select CDM schema, results schema, and cohort table."),
            p("2. Review the domain record summary."),
            p("3. Select domains and run feature extraction."),
            h4("CDM Table Record Summary"),
            
            conditionalPanel(
              condition = "output.featureSummaryAvailable == true",
              downloadButton(
                "download_cdm_summary",
                "Download CSV",
                class = "btn-success"
              )
            ),
            
            br(), br(),
            DT::dataTableOutput("domain_summary"),
            br(),
            verbatimTextOutput("feature_extract_log")
          )
        )
      )
    )
  )
}