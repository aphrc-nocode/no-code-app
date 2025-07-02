feature_extraction_pipeline <- function() {
  cohort_conn <- reactiveValues(details = NULL, conn = NULL, schema_names = NULL, tables = NULL)
  
  # --- 1. Connect and fetch schemas ---
  observeEvent(input$ConnectCohortID, {
    req(input$dbmsID, input$dbmsServerID, input$UserID, input$UserPswdID, input$PortID)
    
    tryCatch({
      cohort_conn$details <- DatabaseConnector::createConnectionDetails(
        dbms = input$dbmsID,
        server = input$dbmsServerID,
        user = input$UserID,
        password = input$UserPswdID,
        port = input$PortID,
        pathToDriver = "./static_files"
      )
      cohort_conn$conn <- DatabaseConnector::connect(cohort_conn$details)
      
      cohort_conn$schema_names <- DBI::dbGetQuery(
        cohort_conn$conn,
        "SELECT schema_name FROM information_schema.schemata 
         WHERE schema_name NOT LIKE 'pg_%' AND schema_name <> 'information_schema'"
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
      showNotification(paste("Connection Error:", e$message), type = "error")
    })
  })
  
  # --- 2. Load cohort tables after result schema is selected ---
  observeEvent(input$results_schema, {
    req(cohort_conn$conn)
    tryCatch({
      cohort_conn$tables <- DBI::dbGetQuery(
        cohort_conn$conn,
        glue::glue("SELECT table_name FROM information_schema.tables 
                    WHERE table_schema = '{input$results_schema}'")
      )$table_name
      
      output$cohort_table_ui <- renderUI({
        selectInput("cohort_table", "Cohort Table:", choices = cohort_conn$tables)
      })
    }, error = function(e) {
      showNotification(paste("Table Fetch Error:", e$message), type = "error")
    })
  })
  
  # --- 3. Feature extraction execution ---
  observeEvent(input$extract_features, {
    req(input$cdm_schema, input$results_schema, input$cohort_table, input$cohort_id, input$output_csv)
    
    if (is.null(input$domain_choices) || length(input$domain_choices) == 0) {
      showNotification("⚠️ Please select at least one domain.", type = "warning")
      return(NULL)
    }
    
    tryCatch({
      conn <- cohort_conn$conn
      cdmSchema <- input$cdm_schema
      resultsSchema <- input$results_schema
      cohortTable <- input$cohort_table
      cohortId <- as.integer(input$cohort_id)
      output_csv <- input$output_csv
      
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
        oracleTempSchema = NULL,
        cdmDatabaseSchema = cdmSchema,
        cohortDatabaseSchema = resultsSchema,
        cohortTable = cohortTable,
        cohortIds = cohortId,
        covariateSettings = covariateSettings
      )
      
      cov_df <- covariateData$covariates %>% collect()
      cov_ref <- covariateData$covariateRef %>% collect()
      
      person_map <- DBI::dbGetQuery(conn, glue::glue(
        "SELECT subject_id AS person_id,
                ROW_NUMBER() OVER (ORDER BY subject_id) - 1 AS rowid
         FROM {resultsSchema}.{cohortTable}
         WHERE cohort_definition_id = {cohortId}"
      ))
      
      cov_named <- cov_df %>%
        rename(rowid = rowId) %>%
        left_join(person_map, by = "rowid") %>%
        left_join(cov_ref, by = "covariateId") %>%
        select(person_id, covariateName, covariateValue)
      
      dt <- data.table::as.data.table(cov_named)
      cov_wide <- data.table::dcast(dt, person_id ~ covariateName, 
                              value.var = "covariateValue", 
                              fun.aggregate = sum, 
                              fill = 0)

      
      person_ids <- paste(na.omit(unique(cov_wide$person_id)), collapse = ",")
      
      person_data <- DBI::dbGetQuery(conn, glue::glue(
        "SELECT p.person_id,
                c.concept_name AS gender_concept_name,
                p.year_of_birth,
                p.ethnicity_source_value,
                p.race_source_value
         FROM {cdmSchema}.person p
         LEFT JOIN {cdmSchema}.concept c ON p.gender_concept_id = c.concept_id
         WHERE p.person_id IN ({person_ids})"
      )) %>%
        mutate(age = lubridate::year(Sys.Date()) - year_of_birth) %>%
        select(person_id, gender_concept_name, age, ethnicity_source_value, race_source_value)
      
      cov_demo <- left_join(cov_wide, person_data, by = "person_id")
      
      condition_data <- DBI::dbGetQuery(conn, glue::glue(
        "SELECT person_id, COUNT(*) AS n_conditions
         FROM {cdmSchema}.condition_occurrence
         WHERE person_id IN ({person_ids})
         GROUP BY person_id"
      ))
      
      final_df <- left_join(cov_demo, condition_data, by = "person_id")
      readr::write_csv(final_df %>% filter(!is.na(person_id)), output_csv)
      
      
      showNotification("✅ Feature-enriched dataset saved!", type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })
}
