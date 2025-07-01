#### ---- Achilles Integration ----###

achilles_integration_server <- function() {
  
  achilles_conn <- reactiveValues(details = NULL,
                                  conn = NULL,
                                  schemas = NULL)
  
      output$cbodatabasetype <-  renderUI(selectInput("achilles_dbms", get_rv_labels("achilles_dbms"), 
                                                        choices = c("", "postgresql", "mysql"), 
                                                        selected = "postgresql"))
        
      output$cbodbhost<- renderUI(textInput("achilles_db_host",
                                            get_rv_labels("achilles_db_host"),
                                            placeholder = "e.g., localhost or IP"))
        
      output$cbodbport <- renderUI(numericInput("achilles_db_port",
                                                 get_rv_labels("achilles_db_port"),
                                                 value = 5432))
       
      output$cbodbname<- renderUI(textInput("achilles_db_name",
                                             get_rv_labels("achilles_db_name"),
                                             placeholder = "Required"))
      
      output$cbodbuser<- renderUI(textInput("achilles_db_user",
                                            get_rv_labels("achilles_db_user"),
                                            placeholder = "Required"))
     
  observeEvent(input$achilles_db_connect, {
    req(input$achilles_dbms,
        input$achilles_db_host,
        input$achilles_db_port,
        input$achilles_db_name,
        input$achilles_db_user,
        input$achilles_db_pwd)
    
    tryCatch({
      achilles_conn$details <- DatabaseConnector::createConnectionDetails(
        dbms = input$achilles_dbms,
        server = paste0(input$achilles_db_host, "/", input$achilles_db_name),
        port = input$achilles_db_port,
        user = input$achilles_db_user,
        password = input$achilles_db_pwd,
        pathToDriver = "./static_files"
      )
      
      # Try connecting
      conn <- DatabaseConnector::connect(achilles_conn$details)
      
      # Get schemas
      schema_query <- ("SELECT schema_name FROM information_schema.schemata 
                       WHERE schema_name NOT LIKE 'pg_%' AND schema_name <> 'information_schema'")
      
      achilles_conn$schemas <- DatabaseConnector::querySql(conn, schema_query)$SCHEMA_NAME
      
      DatabaseConnector::disconnect(conn)
      
      shinyalert("Success", "Connected successfully!", type = "success")
      
      
      # Populate schema dropdowns
      
      output$schema_selectors <- renderUI({
        tagList(
          selectInput("cdm_schema",
                      get_rv_labels("cdm_schema"),
                      choices = achilles_conn$schemas),
          
          selectInput("results_schema",
                      get_rv_labels("results_schema"),
                      choices = achilles_conn$schemas),
          
          selectInput("vocab_schema",
                      get_rv_labels("vocab_schema"),
                      choices = c("", achilles_conn$schemas)),
          
          selectInput("cdm_version",
                      get_rv_labels("cdm_version"),
                      choices = c("5.3", "5.4"),
                      selected = "5.4")
        )
      })
      
      
      
      output$run_achilles <- renderUI({
        actionButton("run_achilles",
                     get_rv_labels("run_achilles"),
                     class = "btn-success")
        
      })
      
    }, error = function(e) {
      shinyalert("Connection Error", e$message, type = "error")
    })
  })
  
  observeEvent(input$run_achilles, {
    req(achilles_conn$details,
        input$cdm_schema,
        input$results_schema)
    
    tryCatch({
      showModal(modalDialog("Running Achilles... Please wait.", footer = NULL))
      
      Achilles::achilles(
        connectionDetails = achilles_conn$details,
        cdmDatabaseSchema = input$cdm_schema,
        resultsDatabaseSchema = input$results_schema,
        vocabDatabaseSchema = ifelse(input$vocab_schema == "", input$cdm_schema, input$vocab_schema),
        cdmVersion = input$cdm_version,
        createTable = TRUE,
        outputFolder = tempdir(),
        numThreads = 1,
        createIndices = FALSE,
        smallCellCount = 0
      )
      
      removeModal()
      shinyalert("Success", "Achilles analysis completed!", type = "success")
      
    }, error = function(e) {
      removeModal()
      shinyalert("Achilles Error", e$message, type = "error")
    })
  })
}
