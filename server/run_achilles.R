### ---- Achilles Integration ---- ###
achilles_integration_server <- function() {
  
  achilles_conn <- reactiveValues(
    details = NULL,
    conn = NULL,
    schemas = NULL,
    is_connected = FALSE
  )
  
  # Reactive to track connection from rv_database
  observe({
    if (!is.null(rv_database$conn) && DBI::dbIsValid(rv_database$conn) && !achilles_conn$is_connected) {
      tryCatch({
        # Reuse the existing connection
        achilles_conn$conn <- rv_database$conn
        achilles_conn$details <- rv_database$details
        achilles_conn$is_connected <- TRUE
        
        # Get available schemas
        schema_query <- "
          SELECT schema_name 
          FROM information_schema.schemata 
          WHERE schema_name NOT IN ('pg_catalog', 'information_schema') 
          AND schema_name NOT LIKE 'pg_%'"
        
        schemas <- DatabaseConnector::querySql(achilles_conn$conn, schema_query)$SCHEMA_NAME
        achilles_conn$schemas <- schemas
        
        # Update UI with schema selectors
        output$schema_selectors <- renderUI({
          tagList(
            selectInput("cdm_schema",
                        get_rv_labels("cdm_schema"),
                        choices = schemas,
                        selected = ""),
            
            selectInput("results_schema",
                        get_rv_labels("results_schema"),
                        choices = schemas,
                        selected =""),
            
            selectInput("vocab_schema",
                        get_rv_labels("vocab_schema"),
                        choices = c("", achilles_conn$schemas),
                        selected = ""),
            
            selectInput("cdm_version",
                        get_rv_labels("cdm_version"),
                        choices = c("5.3", "5.4"),
                        selected = "5.4")
          )
        })
        
        # Enable Achilles run button
        output$run_achilles <- renderUI({
          actionButton("run_achilles",
                       get_rv_labels("run_achilles"),
                       class = "btn-success")
        })
        

        
      }, error = function(e) {
        achilles_conn$is_connected <- FALSE
        shinyalert::shinyalert("Connection Error", 
                   paste("Failed to use connection:", e$message), 
                   type = "error")
      })
    }
  })
  
  
  #-------- Redirection to source ------------------#
  
  output$schema_selectors <- renderUI({
    if (!achilles_conn$is_connected) {
      actionButton("go_to_source",
                   "Click to connect to a database in the Source page first",
                   style = "background-color: #7bc148",
                   icon = icon("arrow-right"))
    }
  })
  
  # Navigate to source page
  observeEvent(input$go_to_source, {
    updateTabItems(session, "tabs", "sourcedata")
  })
  
  #-------------------------------------------------#
  
  
  # Run Achilles analysis
  observeEvent(input$run_achilles, {
    req(achilles_conn$conn, 
        input$cdm_schema, 
        input$results_schema,
        achilles_conn$is_connected)
    
    tryCatch({
      showModal(modalDialog("Running Achilles... Please wait.", footer = NULL))
      
      # Determine vocab schema
      vocab_schema <- if (nzchar(input$vocab_schema)) {
        input$vocab_schema
      } else {
        input$cdm_schema
      }
      
      # Execute Achilles using rv_database connection details
      Achilles::achilles(
        connectionDetails = achilles_conn$details,
        cdmDatabaseSchema = input$cdm_schema,
        resultsDatabaseSchema = input$results_schema,
        vocabDatabaseSchema = vocab_schema,
        cdmVersion = input$cdm_version,
        createTable = TRUE,
        outputFolder = tempdir(),
        numThreads = 1,
        createIndices = FALSE,
        smallCellCount = 5
      )
      
      removeModal()
      shinyalert::shinyalert("Success", "Achilles analysis completed!", type = "success")
      
    }, error = function(e) {
      removeModal()
      shinyalert::shinyalert("Error", paste("Achilles failed:", e$message), type = "error")
    })
  })
}
