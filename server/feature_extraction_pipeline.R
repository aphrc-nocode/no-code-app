feature_extraction_pipeline <- function() {
  
  cohort_conn <- reactiveValues(
    schema_names = NULL,
    tables = NULL,
    domain_summary_data = NULL
  )
  
  # --- 1. Fetch schemas ---
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
        selectInput(
          "cdm_schema",
          "CDM Schema:",
          choices = cohort_conn$schema_names
        )
      })
      
      output$results_schema_ui <- renderUI({
        selectInput(
          "results_schema",
          "Results Schema:",
          choices = cohort_conn$schema_names
        )
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
      showNotification(
        paste("Schema Fetch Error:", e$message),
        type = "error"
      )
    })
  })
  
  # --- 2. Connection prompt ---
  output$schema_feature <- renderUI({
    if (is.null(rv_database$conn)) {
      actionButton(
        "go_to_source_global",
        "Click to connect to a database in the Source page first",
        style = "background-color: #7bc148; font-weight: bold;",
        icon = icon("arrow-right")
      )
    }
  })
  
  observeEvent(input$go_to_source_global, {
    updateTabItems(session, "tabs", "sourcedata")
  })
  
  # --- 3. Populate cohort tables ---
  observeEvent(input$results_schema, {
    req(rv_database$conn, input$results_schema)
    
    tryCatch({
      cohort_conn$tables <- DBI::dbGetQuery(
        rv_database$conn,
        glue::glue("
          SELECT table_name
          FROM information_schema.tables
          WHERE table_schema = '{input$results_schema}'
        ")
      )$table_name
      
      output$cohort_table_ui <- renderUI({
        selectInput(
          "cohort_table",
          "Cohort Table:",
          choices = cohort_conn$tables
        )
      })
      
    }, error = function(e) {
      showNotification(
        paste("Table Fetch Error:", e$message),
        type = "error"
      )
    })
  })
  
  # --- 4. Domain summary using SQL counts only ---
  observeEvent(input$cohort_table, {
    req(rv_database$conn, input$cdm_schema, input$results_schema, input$cohort_table)
    
    tryCatch({
      conn <- rv_database$conn
      cdmSchema <- input$cdm_schema
      resultsSchema <- input$results_schema
      cohortTableName <- input$cohort_table
      
      cohort_n <- DBI::dbGetQuery(
        conn,
        glue::glue("
          SELECT COUNT(DISTINCT subject_id) AS n
          FROM {resultsSchema}.{cohortTableName}
        ")
      )$n
      
      if (length(cohort_n) == 0 || is.na(cohort_n) || cohort_n == 0) {
        cohort_conn$domain_summary_data <- data.frame(
          Domain = character(),
          Records = numeric()
        )
        
        output$domain_summary <- DT::renderDataTable({
          DT::datatable(
            cohort_conn$domain_summary_data,
            rownames = FALSE,
            options = list(pageLength = 10, scrollX = TRUE)
          )
        })
        
        showNotification(
          "No subjects found in the selected cohort.",
          type = "warning"
        )
        return(NULL)
      }
      
      domain_tables <- c(
        "condition_occurrence",
        "drug_exposure",
        "measurement",
        "procedure_occurrence",
        "observation"
      )
      
      counts <- purrr::map_df(domain_tables, function(tbl) {
        q <- glue::glue("
          SELECT COUNT(*) AS records
          FROM {cdmSchema}.{tbl} d
          INNER JOIN (
            SELECT DISTINCT subject_id AS person_id
            FROM {resultsSchema}.{cohortTableName}
          ) c
          ON d.person_id = c.person_id
        ")
        
        n <- DBI::dbGetQuery(conn, q)$records
        
        tibble::tibble(
          Domain = tbl,
          Records = as.numeric(n)
        )
      })
      
      counts$Records <- formatC(counts$Records, format = "d", big.mark = ",")
      
      cohort_conn$domain_summary_data <- counts
      
      output$domain_summary <- DT::renderDataTable({
        DT::datatable(
          counts,
          rownames = FALSE,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            autoWidth = TRUE
          )
        )
      })
      
      showNotification(
        "Domain record summary generated successfully!",
        type = "message"
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error generating domain summary:", e$message),
        type = "error"
      )
    })
  })
  
  # --- 5. Feature extraction ---
  observeEvent(input$extract_features, {
    req(
      rv_database$conn,
      input$domain_choices,
      input$cdm_schema,
      input$results_schema,
      input$cohort_table,
      input$output_csv
    )
    
    output$feature_extract_log <- renderText("Running feature extraction...")
    
    tryCatch({
      conn <- rv_database$conn
      
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
        connection = conn,
        cdmDatabaseSchema = input$cdm_schema,
        cohortDatabaseSchema = input$results_schema,
        cohortTable = input$cohort_table,
        covariateSettings = covariateSettings
      )
      
      cov_df <- covariateData$covariates %>% dplyr::collect()
      cov_ref <- covariateData$covariateRef %>% dplyr::collect()
      
      person_map <- DBI::dbGetQuery(
        conn,
        glue::glue("
          SELECT subject_id AS person_id,
                 ROW_NUMBER() OVER (ORDER BY subject_id) - 1 AS rowid
          FROM {input$results_schema}.{input$cohort_table}
        ")
      ) %>%
        dplyr::mutate(rowid = as.integer(rowid))
      
      cov_named <- cov_df %>%
        dplyr::rename(rowid = rowId) %>%
        dplyr::mutate(rowid = as.integer(rowid)) %>%
        dplyr::left_join(person_map, by = "rowid") %>%
        dplyr::left_join(cov_ref, by = "covariateId") %>%
        dplyr::select(person_id, covariateName, covariateValue)
      
      dt <- data.table::as.data.table(cov_named)
      
      cov_wide <- data.table::dcast(
        dt,
        person_id ~ covariateName,
        value.var = "covariateValue",
        fun.aggregate = sum,
        fill = 0
      )
      
      file_name <- input$output_csv
      if (!grepl("\\.csv$", file_name, ignore.case = TRUE)) {
        file_name <- paste0(file_name, ".csv")
      }
      
      file_path <- file.path(paste0(app_username, "/datasets"), file_name)
      
      readr::write_csv(
        cov_wide %>% dplyr::filter(!is.na(person_id)),
        file_path
      )
      
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
      
      log_file_main <- paste0(
        app_username,
        "/.log_files/",
        file_name,
        "-upload.main.log"
      )
      
      write.csv(meta_data, log_file_main, row.names = FALSE)
      
      if (exists("refresh_uploaded_data")) {
        refresh_uploaded_data()
      }
      
      output$feature_extract_log <- renderText(
        paste("Feature extraction completed successfully.\nSaved to:", file_path)
      )
      
      shinyalert::shinyalert(
        "",
        "Feature-extracted dataset saved and added to uploads!",
        type = "success"
      )
      
    }, error = function(e) {
      output$feature_extract_log <- renderText(
        paste("Error during extraction:", e$message)
      )
      
      showNotification(
        paste("Error during extraction:", e$message),
        type = "error"
      )
    })
  })
  
  # --- 6. Download domain summary ---
  output$download_cdm_summary <- downloadHandler(
    filename = function() {
      paste0("domain_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(cohort_conn$domain_summary_data)
      write.csv(cohort_conn$domain_summary_data, file, row.names = FALSE)
    }
  )
  
  # --- 7. Reactive flags ---
  output$dbConnected <- reactive({
    !is.null(rv_database$conn)
  })
  outputOptions(output, "dbConnected", suspendWhenHidden = FALSE)
  
  output$featureSummaryAvailable <- reactive({
    !is.null(cohort_conn$domain_summary_data)
  })
  outputOptions(output, "featureSummaryAvailable", suspendWhenHidden = FALSE)
}
