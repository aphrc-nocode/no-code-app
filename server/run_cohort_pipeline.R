run_cohort_pipeline <- function() {
  cohort_conn <- reactiveValues(
    details = NULL,
    conn = NULL,
    schema_names = NULL,
    cdm = NULL,
    results = NULL,
    person_summary = NULL
  )
  
  # --- UI Inputs ---
  output$dbmsID <- renderUI({ selectInput("dbmsID", "DBMS", choices = c("postgresql", "sql server")) })
  output$dbmsServerID <- renderUI({ textInput("dbmsServerID", "Server") })
  output$UserID <- renderUI({ textInput("UserID", "Username") })
  output$UserPswdID <- renderUI({ passwordInput("UserPswdID", "Password") })
  output$PortID <- renderUI({ textInput("PortID", "Port", value = "5432") })
  output$PathID <- renderUI({ textInput("PathID", "Path to Driver", value = normalizePath("./static_files", mustWork = FALSE)) })
  output$ConnectCohortID <- renderUI({ actionButton("ConnectCohortID", "Connect", class = "btn-success") })
  
  # --- Connect to Database ---
  observeEvent(input$ConnectCohortID, {
    req(input$dbmsID, input$dbmsServerID, input$UserID, input$UserPswdID, input$PortID, input$PathID)
    tryCatch({
      if (!rJava::.jniInitialized) rJava::.jinit()
      
      cohort_conn$details <- DatabaseConnector::createConnectionDetails(
        dbms = input$dbmsID,
        server = input$dbmsServerID,
        user = input$UserID,
        password = input$UserPswdID,
        port = input$PortID,
        pathToDriver = normalizePath(input$PathID, mustWork = TRUE)
      )
      cohort_conn$conn <- DatabaseConnector::connect(cohort_conn$details)
      
      cohort_conn$schema_names <- DBI::dbGetQuery(cohort_conn$conn, "SELECT schema_name FROM information_schema.schemata")[["schema_name"]]
      
      output$CDMSchemaName <- renderUI({ selectInput("CDMSchemaName", "CDM Schema", choices = cohort_conn$schema_names) })
      output$ResultSchemaName <- renderUI({ selectInput("ResultSchemaName", "Results Schema", choices = cohort_conn$schema_names) })
      output$CDMConnName <- renderUI({ textInput("CDMConnName", "CDM Connection Name", value = "my_cdm") })
      output$achillesSchemaName <- renderUI({ textInput("achillesSchemaName", "Achilles Schema (optional)") })
      output$CreateCDMID <- renderUI({ actionButton("CreateCDMID", "Create CDM", class = "btn-success") })
      
      shinyalert::shinyalert("Success", "Database connected successfully!", type = "success")
    }, error = function(e) {
      shinyalert::shinyalert("Error", e$message, type = "error")
    })
  })
  
  # --- Create CDM Reference ---
  observeEvent(input$CreateCDMID, {
    req(cohort_conn$conn, input$CDMConnName, input$CDMSchemaName, input$ResultSchemaName)
    tryCatch({
      cohort_conn$cdm <- CDMConnector::cdmFromCon(
        con = cohort_conn$conn,
        cdmName = input$CDMConnName,
        cdmSchema = input$CDMSchemaName,
        writeSchema = input$ResultSchemaName,
        achillesSchema = if (nzchar(input$achillesSchemaName)) input$achillesSchemaName else NULL
      )
      
      if (is.null(cohort_conn$cdm) || !inherits(cohort_conn$cdm, "cdm_reference")) {
        shinyalert::shinyalert("CDM Creation Failed", "CDM reference was not created successfully.", type = "error")
        return(NULL)
      }
      
      shinyalert::shinyalert("CDM Reference Created", type = "success")
      
    }, error = function(e) {
      shinyalert::shinyalert("CDM Creation Failed", e$message, type = "error")
    })
  })
  
  # --- Cohort Inputs ---
  output$CohortcreationName <- renderUI({ textInput("CohortcreationName", "Cohort Label") })
  output$ConceptKeyword <- renderUI({ textInput("ConceptKeyword", "Keywords (comma-separated)") })
  output$CohortNameID <- renderUI({ textInput("CohortNameID", "Cohort Table Name") })
  output$CohortDateID <- renderUI({ selectInput("CohortDateID", "Cohort Exit Date", choices = c("event_start_date", "event_end_date")) })
  output$GenerateCohortID <- renderUI({ actionButton("run_cohort", "Run Cohort Pipeline", class ="btn-success") })
  
  # --- Cohort Creation Logic ---
  observeEvent(input$run_cohort, {
    req(cohort_conn$cdm, input$ConceptKeyword, input$CohortNameID, input$CohortDateID)
    showModal(modalDialog("Running cohort pipeline... Please wait.", footer = NULL))
    
    tryCatch({
      keywords <- trimws(unlist(strsplit(input$ConceptKeyword, ",")))
      
      valid_keywords <- c()
      invalid_keywords <- c()
      
      for (kw in keywords) {
        concept_codes <- CodelistGenerator::getCandidateCodes(cohort_conn$cdm, kw)
        if (nrow(concept_codes) == 0) {
          invalid_keywords <- c(invalid_keywords, kw)
        } else {
          valid_keywords <- c(valid_keywords, kw)
        }
      }
      
      if (length(valid_keywords) == 0) {
        removeModal()
        shinyalert::shinyalert("No Valid Keywords", "None of the provided keywords were found.", type = "error")
        return(NULL)
      }
      
      if (length(invalid_keywords) > 0) {
        shinyalert::shinyalert("Some Keywords Skipped", paste("Skipped:", paste(invalid_keywords, collapse = ", ")), type = "warning")
      }
      
      codes_list <- list()
      for (kw in valid_keywords) {
        codes_list[[paste0(kw, "_codes")]] <- CodelistGenerator::getCandidateCodes(cohort_conn$cdm, kw)
      }
      
      codes_list_id <- list()
      for (kw in names(codes_list)) {
        codes_list_id[[kw]] <- codes_list[[kw]]$concept_id
      }
      
      study_codes <- CodelistGenerator::newCodelist(codes_list_id)
      
      cohort_conn$cdm[[input$CohortNameID]] <- cohort_conn$cdm %>%
        CohortConstructor::conceptCohort(
          conceptSet = study_codes,
          name = input$CohortNameID,
          exit = input$CohortDateID
        )
      
      # --- FIX: Manually query cohort from results schema ---
      cohort_table_sql <- glue::glue(
        "SELECT * FROM {input$ResultSchemaName}.{input$CohortNameID}"
      )
      cohort_df <- DBI::dbGetQuery(cohort_conn$conn, cohort_table_sql)
      
      person_df <- cohort_conn$cdm$person %>% dplyr::collect()
      
      cohort_conn$person_summary <- cohort_df %>%
        dplyr::inner_join(person_df, by = c("subject_id" = "person_id")) %>%
        dplyr::mutate(
          birth_date = as.Date(paste(year_of_birth, month_of_birth, day_of_birth, sep = "-"), format = "%Y-%m-%d"),
          age = floor(lubridate::interval(birth_date, Sys.Date()) / lubridate::years(1))
        )
      
      removeModal()
      shinyalert::shinyalert("Success", "Cohort created and summary generated!", type = "success")
      
      output$cohort_demographics <- renderTable({
        req(cohort_conn$person_summary)
        cohort_conn$person_summary %>%
          dplyr::summarise(
            count = dplyr::n(),
            mean_age = round(mean(age, na.rm = TRUE), 1),
            median_age = median(age, na.rm = TRUE),
            male = sum(gender_concept_id == 8507, na.rm = TRUE),
            female = sum(gender_concept_id == 8532, na.rm = TRUE),
            male_pct = round(100 * male / count, 1),
            female_pct = round(100 * female / count, 1)
          )
      })
      
    }, error = function(e) {
      removeModal()
      shinyalert::shinyalert("Pipeline Error", as.character(e), type = "error")
    })
  })
}
