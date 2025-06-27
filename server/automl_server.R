library(httr)

automl_server <- function(id, rv_current) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value for store AutoML output
    automl_results <- reactiveVal(NULL)
    
    # Target selector
    output$target_selector <- renderUI({
      req(rv_current$working_df)
      selectInput(ns("target"),
                  "Choose target",
                  choices = names(rv_current$working_df))
    })
    
    # Launch AutoML with API
    observeEvent(input$launch_automl, {
      req(rv_current$working_df, input$target)
      
      tmpfile <- tempfile(fileext = ".csv") # Create a temporary csv file generated from dataframe in input
      write.csv(rv_current$working_df, tmpfile, row.names = FALSE)
      
      res <- httr::POST(
        url = "http://127.0.0.1:8000/automl",
        body = list(
          target = input$target,
          file = upload_file(tmpfile) #File needed for FastPI has been taken from temporary file
        ),
        encode = "multipart"
      )
      
      if (res$status_code == 200) {
        result_df <- jsonlite::fromJSON(httr::content(res, as = "text"), flatten = TRUE)
        
    #Update result
        automl_results(result_df)
        
      } else {
        showNotification("Error while calling the AutoML API", type = "error")
      }
    })
    
    #Show AutoML table
    output$automl_results <- DT::renderDataTable({
      req(automl_results())
      DT::datatable(automl_results())
    })
    
    # Create button only if results exist
    output$download_ui <- renderUI({
      req(automl_results())
      downloadButton(ns("download_automl_results"), "Download result (.csv)")
    })
    
    # Download Handler
    output$download_automl_results <- downloadHandler(
      filename = function() {
        paste0("AutoML_Results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(automl_results(), file, row.names = FALSE)
      }
    )
  })
}
