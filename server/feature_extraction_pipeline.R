feature_extraction_pipeline <- function() {
  
  cohort_conn <- reactiveValues(
    schema_names = NULL,
    tables       = NULL,
    domain_summary_data = NULL
  )
  
  # --- 1. Schema fetch ---
  observe({
    req(rv_database$conn)
    tryCatch({
      cohort_conn$schema_names <- DBI::dbGetQuery(
        rv_database$conn,
        "SELECT schema_name 
         FROM information_schema.schemata 
         WHERE schema_name NOT LIKE 'pg_%' 
           AND schema_name <> 'information_schema'"
      )$schema_name
      
      output$cdm_schema_ui <- renderUI({
        selectInput("cdm_schema", "CDM Schema:", choices = cohort_conn$schema_names)
      })
      
      output$results_schema_ui <- renderUI({
        selectInput("results_schema", "Results Schema:", choices = cohort_conn$schema_names)
      })
      
      output$domain_choices_ui <- renderUI({
        checkboxGroupInput(
          inputId = "domain_choices",
          label = "Select Domains for Feature Extraction:",
          choices = list(
            "Demographics (Gender, Age, Race, Ethnicity)" = "demographics",
            "Condition Occurrence" = "condition",
            "Drug Exposure" = "drug",
            "Measurement" = "measurement",
            "Procedure Occurrence" = "procedure",
            "Observation" = "observation"
          ),
          selected = c("demographics", "condition", "drug")
        )
      })
      
    }, error = function(e) {
      showNotification(paste("Schema Fetch Error:", e$message), type = "error")
    })
  })
  
  # --- 2. Schema connection prompt ---
  output$schema_feature <- renderUI({
    if (is.null(rv_database$conn)) {
      actionButton("go_to_source_global",
                   "Click to connect to a database in the Source page first",
                   style = "background-color: #7bc148; font-weight: bold;",
                   icon = icon("arrow-right"))
    }
  })
  
  observeEvent(input$go_to_source_global, {
    updateTabItems(session, "tabs", "sourcedata")
  })
  
  # --- 3. Populate cohort tables when schema selected ---
  observeEvent(input$results_schema, {
    req(rv_database$conn, input$results_schema)
    tryCatch({
      cohort_conn$tables <- DBI::dbGetQuery(
        rv_database$conn,
        glue::glue("SELECT table_name 
                    FROM information_schema.tables 
                    WHERE table_schema = '{input$results_schema}'")
      )$table_name
      
      output$cohort_table_ui <- renderUI({
        selectInput("cohort_table", "Cohort Table:", choices = cohort_conn$tables)
      })
    }, error = function(e) {
      showNotification(paste("Table Fetch Error:", e$message), type = "error")
    })
  })
  
  # --- 4. Generate record summary per domain ---
  observeEvent(input$cohort_table, {
    req(rv_database$conn, input$cdm_schema, input$results_schema, input$cohort_table)
    tryCatch({
      conn <- rv_database$conn
      cdmSchema <- input$cdm_schema
      resultsSchema <- input$results_schema
      cohortTableName <- input$cohort_table
      
      cohort_table <- DBI::dbGetQuery(conn, glue::glue(
        "SELECT * FROM {resultsSchema}.{cohortTableName}"
      )) %>% dplyr::rename(person_id = subject_id)
      
      cohort_persons <- cohort_table %>% dplyr::select(person_id) %>% distinct()
      if (nrow(cohort_persons) == 0) {
        output$domain_summary <- DT::renderDataTable({ data.frame(Domain = character(), Records = numeric()) })
        showNotification("No subjects found in the selected cohort.", type = "warning")
        return(NULL)
      }
      
      domain_tables <- c("condition_occurrence", "drug_exposure", "measurement", "procedure_occurrence", "observation")
      counts <- purrr::map_df(domain_tables, function(tbl) {
        domain_data <- DBI::dbGetQuery(conn, glue::glue("SELECT * FROM {cdmSchema}.{tbl}"))
        joined <- dplyr::inner_join(domain_data, cohort_persons, by = "person_id")
        tibble::tibble(Domain = tbl, Records = nrow(joined))
      })
      
      counts$Records <- formatC(counts$Records, format = "d", big.mark = ",")
      cohort_conn$domain_summary_data <- counts  # store table for conditional display
      
      output$domain_summary <- DT::renderDataTable({
        DT::datatable(counts, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
      })
      showNotification("✅ Domain record summary generated successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error generating domain summary:", e$message), type = "error")
    })
  })
  
  # --- 5. Feature extraction process with upload registration ---
  observeEvent(input$extract_features, {
    req(input$domain_choices, input$cdm_schema, input$results_schema, input$cohort_table)
    
    tryCatch({
      connDetails <- DatabaseConnector::createConnectionDetails(
        dbms = "postgresql",
        server = paste0(input$db_host, "/", input$db_name),
        user = input$db_user,
        password = input$db_pwd,
        port = as.integer(input$db_port),
        pathToDriver = "static_files"
      )
      
      conn_dc <- DatabaseConnector::connect(connDetails)
      
      covariateSettings <- FeatureExtraction::createCovariateSettings(
        useDemographicsGender = "demographics" %in% input$domain_choices,
        useDemographicsAge = "demographics" %in% input$domain_choices,
        useDemographicsRace = "demographics" %in% input$domain_choices,
        useDemographicsEthnicity = "demographics" %in% input$domain_choices,
        useConditionOccurrenceAnyTimePrior = "condition" %in% input$domain_choices,
        useDrugExposureAnyTimePrior = "drug" %in% input$domain_choices,
        useMeasurementAnyTimePrior = "measurement" %in% input$domain_choices,
        useProcedureOccurrenceAnyTimePrior = "procedure" %in% input$domain_choices,
        useObservationAnyTimePrior = "observation" %in% input$domain_choices
      )
      
      covariateData <- FeatureExtraction::getDbCovariateData(
        connection = conn_dc,
        cdmDatabaseSchema = input$cdm_schema,
        cohortDatabaseSchema = input$results_schema,
        cohortTable = input$cohort_table,
        covariateSettings = covariateSettings
      )
      
      DatabaseConnector::disconnect(conn_dc)
      
      cov_df <- covariateData$covariates %>% collect()
      cov_ref <- covariateData$covariateRef %>% collect()
      
      person_map <- DBI::dbGetQuery(rv_database$conn, glue::glue(
        "SELECT subject_id AS person_id,
              ROW_NUMBER() OVER (ORDER BY subject_id) - 1 AS rowid
         FROM {input$results_schema}.{input$cohort_table}"
      )) %>% dplyr::mutate(rowid = as.integer(rowid))
      
      cov_named <- cov_df %>%
        dplyr::rename(rowid = rowId) %>%
        dplyr::mutate(rowid = as.integer(rowid)) %>%
        dplyr::left_join(person_map, by = "rowid") %>%
        dplyr::left_join(cov_ref, by = "covariateId") %>%
        dplyr::select(person_id, covariateName, covariateValue)
      
      dt <- data.table::as.data.table(cov_named)
      cov_wide <- data.table::dcast(
        dt, person_id ~ covariateName,
        value.var = "covariateValue",
        fun.aggregate = sum,
        fill = 0
      )
      
      # --- Save dataset ---
      file_name <- paste0("feature_extracted_", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv")
      file_path <- file.path("datasets", file_name)
      readr::write_csv(cov_wide %>% dplyr::filter(!is.na(person_id)), file_path)
      
      # --- Register as upload ---
      upload_time <- Sys.time()
      meta_data <- Rautoml::create_df_metadata(
        data = cov_wide,
        filename = file_name,
        study_name = "Feature Extraction Output",
        study_country = "N/A",
        additional_info = paste0("Extracted from ", input$cohort_table),
        upload_time = upload_time,
        last_modified = upload_time
      )
      
      log_file_main <- paste0(".log_files/", file_name, "-upload.main.log")
      write.csv(meta_data, log_file_main, row.names = FALSE)
      
      # Refresh uploads list
      if (exists("refresh_uploaded_data")) {
        refresh_uploaded_data()
      }
      
      shinyalert::shinyalert("", "✅ Feature-extracted dataset saved and added to uploads!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("\u274c Error during extraction:", e$message), type = "error")
    })
  })
  
  # --- 6. Download handler for domain summary ---
  output$download_cdm_summary <- downloadHandler(
    filename = function() {
      paste0("domain_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(cohort_conn$domain_summary_data, file, row.names = FALSE)
    }
  )
  
  # --- 7. REACTIVE FLAGS for conditionalPanels ---
  output$dbConnected <- reactive({
    !is.null(rv_database$conn)
  })
  outputOptions(output, "dbConnected", suspendWhenHidden = FALSE)
  
  output$summaryAvailable <- reactive({
    !is.null(cohort_conn$domain_summary_data)
  })
  outputOptions(output, "summaryAvailable", suspendWhenHidden = FALSE)
}
