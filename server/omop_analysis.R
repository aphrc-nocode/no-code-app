### OMOP analysis - DQD ACHILLES CohortConstructor
omop_analysis_server <- function(){
  
  observe({
    if (isTRUE(!is.null(rv_database$conn))) {
      rv_database$database_host <- input$db_host
      rv_database$database_name <- input$db_name
      rv_database$database_user <- input$db_user
      rv_database$database_pass <- input$db_pwd
      
      
     
    }
  })
  
  observeEvent(input$generate_dqd, {
    
  file_path <- file.path(getwd(), "static_files")
  
  # Extract raw values from reactives first
  database_host <- isolate(rv_database$database_host)
  database_name <- isolate(rv_database$database_name)
  database_user <- isolate(rv_database$database_user)
  database_pass <- isolate(rv_database$database_pass)
  
  connectionDetails <- rv_database$details  #Call to earlier connection details with persistent con

  # connectionDetails <- createConnectionDetails(
  #   dbms = "postgresql",
  #   server = paste(database_host, database_name, sep="/"),
  #   user = database_user,
  #   password = database_pass,
  #   pathToDriver = file_path
  # )
  # 

  
  
  #Setting up the variable
  
  cdmDatabaseSchema <- input$cdm_schema
  resultsDatabaseSchema <- input$results_schema
  cdmSourceName <- input$cdmSourceName
  numThreads <- 1 # on Redshift, 3 seems to work well
  sqlOnly <- FALSE # set to TRUE if you just want to get the SQL scripts and not actually run the queries
  outputFolder <- "output"
  verboseMode <- TRUE # set to TRUE if you want to see activity written to the console
  writeToTable <- TRUE # set to FALSE if you want to skip writing to results table
  checkLevels <- c("TABLE", "FIELD", "CONCEPT")
  checkNames <- c() #Names can be found in inst/csv/OMOP_CDM_v5.3.1_Check_Desciptions.csv
  
  print(paste("db-omop:", input$cdm_schema))
  
  folder <- file.path(getwd(),"output")
  
  # To run DQD on the bg
  # Step 2: Launch in background
  p<-callr::r_bg(
    func = function(db_host, db_name, db_user, db_pass, driver_path,
                    cdmSchema, resultsSchema, sourceName, threads, sqlFlag,
                    outFolder, verbose, writeFlag, levels, names) {
      
      library(DataQualityDashboard)
      library(DatabaseConnector)
      
      connectionDetails <- createConnectionDetails(
        dbms = "postgresql",
        server = paste(db_host, db_name, sep = "/"),
        user = db_user,
        password = db_pass,
        pathToDriver = driver_path
      )
      
      executeDqChecks(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmSchema,
        resultsDatabaseSchema = resultsSchema,
        cdmSourceName = sourceName,
        numThreads = threads,
        sqlOnly = sqlFlag,
        outputFolder = outFolder,
        verboseMode = verbose,
        writeToTable = writeFlag,
        checkLevels = levels,
        checkNames = names
      )
      Sys.sleep(900)
    },
    args = list(
      db_host = database_host,
      db_name = database_name,
      db_user = database_user,
      db_pass = database_pass,
      driver_path = file_path,
      cdmSchema = cdmDatabaseSchema,
      resultsSchema = resultsDatabaseSchema,
      sourceName = cdmSourceName,
      threads = numThreads,
      sqlFlag = sqlOnly,
      outFolder = outputFolder,
      verbose = verboseMode,
      writeFlag = writeToTable,
      levels = checkLevels,
      names = checkNames
    ),
    stdout = file.path(outputFolder, "dq_stdout.txt"),
    stderr = file.path(outputFolder, "dq_stderr.txt"),
    supervise = TRUE
  )
  
  #p()   Not running on bg
  
  # Running interactively
  tryCatch({
    showModal(modalDialog("Generating DQD ...", footer = NULL))
    DataQualityDashboard::executeDqChecks(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          resultsDatabaseSchema = resultsDatabaseSchema,
                                          cdmSourceName = cdmSourceName,
                                          numThreads = numThreads,
                                          sqlOnly = sqlOnly,
                                          outputFolder = outputFolder,
                                          verboseMode = verboseMode,
                                          writeToTable = writeToTable,
                                          checkLevels = checkLevels,
                                          checkNames = checkNames)
    removeModal()
    
  },
  error = function(e){
    shinyalert::shinyalert("", "Error running DQD", type = "error")
  })

  
  #showNotification("DQD check running in background...", type = "message")
  
  
  })
  
  
  observeEvent(input$view_dqd, {
   
    
    # To run shiny report dashboard bg
    
    get_latest_json <- function(folder_path) {
      json_files <- list.files(path = folder_path, pattern = "\\.json$", full.names = TRUE)
      
      timestamps <- regmatches(
        basename(json_files),
        regexpr("[0-9]{14}", basename(json_files))
      )
      
      valid <- !is.na(timestamps)
      if (!any(valid)) stop("No JSON files with valid timestamp found.")
      
      datetime <- as.POSIXct(timestamps[valid], format = "%Y%m%d%H%M%S", tz = "UTC")
      
      return(json_files[valid][which.max(datetime)])
    }
    
    folder <- file.path(getwd(),"output")
    
    path_to_json <- get_latest_json(folder)

    jsonPath <-path_to_json
    
    
    callr::r_bg(
      function(jsonPath) {

        Sys.setenv("jsonPath" = jsonPath)
        app_dir <- system.file("shinyApps", package = "DataQualityDashboard")

        if (app_dir == "") {
          app_dir <- "~/library/DataQualityDashboard/shinyApps"
        }

        shiny::runApp(app_dir, host = "127.0.0.1", launch.browser = TRUE)
        Sys.sleep(Inf) 
      },
      args = list(jsonPath),
      stdout = file.path(folder, "dq_stdout.txt"),
      stderr = file.path(folder, "dq_stderr.txt"),
      supervise = TRUE
    )

    showNotification("DQD dashboard launched in browser", type = "message")

  })
  
  observeEvent(input$existed_conn, {
    if(input$existed_conn == "Postgres Connection"){
      updateSelectInput(session,inputId = "cdm_schema", choices = rv_database$schema_list,selected = rv_database$schema_list[1] )
      updateSelectInput(session,inputId = "results_schema", choices = rv_database$schema_list,selected = rv_database$schema_list[1] )
      
      }
    
  })  
  

  
}


create_log_reader <- function(stderr_file_path) {
  reactiveFileReader(
    intervalMillis = 2000,   # check every 2 seconds
    session = session,
    filePath = stderr_file_path,
    readFunc = function(path) {
      if (file.exists(path)) {
        paste(readLines(path, warn = FALSE), collapse = "\n")
        
        content <- paste(readLines(path, warn = FALSE), collapse = "\n")
        
        match <- regmatches(content, regexpr("http[s]?://[^\\s]+", content))
        
        if (length(match) > 0) {
          rv_omop$url<-as.character(match)
          return(match)
        } else {
          return("No URL found.")
        }
        
      } 
    }
  )
}






  
