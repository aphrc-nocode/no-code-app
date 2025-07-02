run_cohort_pipeline <- function() {
  cohort_conn <- reactiveValues(
    details = NULL,
    conn = NULL,
    schema_names = NULL,
    cdm = NULL,
    results = NULL,
    person_summary = NULL,
    covariateData = NULL,
    summary_table = NULL
  )
  
  # --- UI Inputs ---
  output$dbmsID <- renderUI({ selectInput("dbmsID", "DBMS", choices = c("postgresql", "sql server")) })
  output$dbmsServerID <- renderUI({ textInput("dbmsServerID", "Server") })
  output$UserID <- renderUI({ textInput("UserID", "Username") })
  output$UserPswdID <- renderUI({ passwordInput("UserPswdID", "Password") })
  output$PortID <- renderUI({ textInput("PortID", "Port", value = "5432") })
  output$ConnectCohortID <- renderUI({ actionButton("ConnectCohortID", "Connect", class = "btn-success") })
  
  # --- Connect to Database ---
  observeEvent(input$ConnectCohortID, {
    req(input$dbmsID, input$dbmsServerID, input$UserID, input$UserPswdID, input$PortID)
    tryCatch({
      if (!rJava::.jniInitialized) rJava::.jinit()
      
      cohort_conn$details <- DatabaseConnector::createConnectionDetails(
        dbms = input$dbmsID,
        server = input$dbmsServerID,
        user = input$UserID,
        password = input$UserPswdID,
        port = input$PortID,
        pathToDriver = "./static_files"
      )
      cohort_conn$conn <- DatabaseConnector::connect(cohort_conn$details)
      
      cohort_conn$schema_names <- DBI::dbGetQuery(cohort_conn$conn,
                                                  "SELECT schema_name FROM information_schema.schemata 
                                                    WHERE schema_name NOT LIKE 'pg_%' AND schema_name <> 'information_schema'")
      
      output$CDMSchemaName <- renderUI({ selectInput("CDMSchemaName", "CDM Schema", choices = cohort_conn$schema_names) })
      output$ResultSchemaName <- renderUI({ selectInput("ResultSchemaName", "Results Schema", choices = cohort_conn$schema_names) })
      output$CDMConnName <- renderUI({ textInput("CDMConnName", "CDM Connection Name", value = "my_cdm") })
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
        achillesSchema = NULL
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
  observe({
    req(cohort_conn$cdm)
    output$ConceptKeyword <- renderUI({ textInput("ConceptKeyword", "Keywords (e.g:word1,word2,word3,...)") })
    output$CohortNameID <- renderUI({ textInput("CohortNameID", "Cohort Table Name") })
    output$CohortDateID <- renderUI({ selectInput("CohortDateID", "Cohort Exit Date", choices = c("event_start_date", "event_end_date")) })
    output$GenerateCohortID <- renderUI({ actionButton("run_cohort", "Run Pipeline", class = "btn-success") })
  })
  
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
      
      codes_list_id <- lapply(codes_list, function(x) x$concept_id)
      study_codes <- CodelistGenerator::newCodelist(codes_list_id)
      
      cohort_conn$cdm[[input$CohortNameID]] <- cohort_conn$cdm %>%
        CohortConstructor::conceptCohort(
          conceptSet = study_codes,
          name = input$CohortNameID,
          exit = input$CohortDateID
        )
      
      cohort_table_sql <- glue::glue("SELECT * FROM {input$ResultSchemaName}.{input$CohortNameID}")
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
      
      # -- Render a UI block with a dynamic title and a table --
      output$cohort_demographics <- renderUI({
        req(cohort_conn$person_summary)
        
        # Extract keywords from input
        title_keywords <- paste(trimws(unlist(strsplit(input$ConceptKeyword, ","))), collapse = ", ")
        df <- cohort_conn$person_summary
        
        # Gender counts
        male <- sum(df$gender_concept_id == 8507, na.rm = TRUE)
        female <- sum(df$gender_concept_id == 8532, na.rm = TRUE)
        other_gender <- sum(!df$gender_concept_id %in% c(8507, 8532), na.rm = TRUE)
        
        # Race, Ethnicity, etc.
        top_race <- df %>%
          dplyr::filter(!is.na(race_source_value)) %>%
          dplyr::count(race_source_value, sort = TRUE) %>%
          dplyr::slice_head(n = 3) %>%
          dplyr::mutate(pct = round(100 * n / sum(n), 1))
        
        top_ethnicity <- df %>%
          dplyr::filter(!is.na(ethnicity_source_value)) %>%
          dplyr::count(ethnicity_source_value, sort = TRUE) %>%
          dplyr::slice_head(n = 3) %>%
          dplyr::mutate(pct = round(100 * n / sum(n), 1))
        
        top_location <- df %>% dplyr::count(location_id, sort = TRUE) %>% dplyr::slice_head(n = 3) %>% dplyr::mutate(pct = round(100 * n / sum(n), 1))
        top_care_site <- df %>% dplyr::count(care_site_id, sort = TRUE) %>% dplyr::slice_head(n = 3) %>% dplyr::mutate(pct = round(100 * n / sum(n), 1))
        top_provider <- df %>% dplyr::count(provider_id, sort = TRUE) %>% dplyr::slice_head(n = 3) %>% dplyr::mutate(pct = round(100 * n / sum(n), 1))
        
        # Main summary
        summary_table <- data.frame(
          Metric = c("Total Individuals", "Mean Age", "Median Age", "Male (%)", "Female (%)", "Other Gender (%)"),
          Value = c(
            nrow(df),
            round(mean(df$age, na.rm = TRUE), 1),
            median(df$age, na.rm = TRUE),
            paste0(round(100 * male / nrow(df), 1), "%"),
            paste0(round(100 * female / nrow(df), 1), "%"),
            paste0(round(100 * other_gender / nrow(df), 1), "%")
          )
        )
        
        # Helper to append top groups
        append_summary <- function(table, name_prefix, data, id_col = 1) {
          for (i in seq_len(nrow(data))) {
            table <- rbind(table, data.frame(
              Metric = paste0(name_prefix, " ", data[[id_col]][i]),
              Value = paste0(data$n[i], " (", data$pct[i], "%)")
            ))
          }
          table
        }
        
        summary_table <- append_summary(summary_table, "Race", top_race, id_col = "race_source_value")
        summary_table <- append_summary(summary_table, "Ethnicity", top_ethnicity, id_col = "ethnicity_source_value")
        summary_table <- append_summary(summary_table, "Location ID", top_location)
        summary_table <- append_summary(summary_table, "Care Site ID", top_care_site)
        summary_table <- append_summary(summary_table, "Provider ID", top_provider)
        
        cohort_conn$summary_table <- summary_table
        
        tagList(
          h4(paste("Demographics for:", title_keywords)),
          tableOutput("summary_table_output")
        )
      })
      
      # Actual table rendering
      output$summary_table_output <- renderTable({
        req(cohort_conn$summary_table)
        cohort_conn$summary_table
      })
      
      
      # --- Plots after cohort summary ---
      output$gender_plot <- plotly::renderPlotly({
        req(cohort_conn$person_summary)
        df <- cohort_conn$person_summary
        gender_df <- df %>%
          dplyr::mutate(gender = dplyr::case_when(
            gender_concept_id == 8507 ~ "Male",
            gender_concept_id == 8532 ~ "Female",
            TRUE ~ "Other"
          )) %>%
          dplyr::count(gender)
        
        plot_ly(gender_df, x = ~gender, y = ~n, type = 'bar', marker = list(color = 'steelblue')) %>%
          layout(title = "Gender Distribution", yaxis = list(title = "Count"), xaxis = list(title = "Gender"))
      })
      
      output$age_group_plot <- plotly::renderPlotly({
        req(cohort_conn$person_summary)
        df <- cohort_conn$person_summary %>%
          dplyr::mutate(age_group = dplyr::case_when(
            age <= 14 ~ "Children",
            age >= 15 & age <= 24 ~ "15-24",
            age >= 25 & age <= 44 ~ "25-44",
            age >= 45 & age <= 59 ~ "45-59",
            age >= 60 & age <= 74 ~ "60-74",
            age >= 75 ~ "75+",
            TRUE ~ NA_character_
          )) %>%
          dplyr::count(age_group)
        
        plot_ly(df, x = ~age_group, y = ~n, type = 'bar', marker = list(color = 'coral')) %>%
          layout(title = "Age Group Distribution", yaxis = list(title = "Count"), xaxis = list(title = "Age Group"))
      })
      
      output$race_plot <- plotly::renderPlotly({
        req(cohort_conn$person_summary)
        df <- cohort_conn$person_summary %>%
          dplyr::filter(!is.na(race_source_value)) %>%
          dplyr::count(race_source_value, sort = TRUE) %>%
          dplyr::slice_head(n = 5)
        
        plot_ly(df, x = ~race_source_value, y = ~n, type = 'bar', marker = list(color = 'darkcyan')) %>%
          layout(title = "Top Race Source Values", yaxis = list(title = "Count"), xaxis = list(title = "Race"))
      })
      
      output$ethnicity_plot <- plotly::renderPlotly({
        req(cohort_conn$person_summary)
        df <- cohort_conn$person_summary %>%
          dplyr::filter(!is.na(ethnicity_source_value)) %>%
          dplyr::count(ethnicity_source_value, sort = TRUE) %>%
          dplyr::slice_head(n = 5)
        
        plot_ly(df, x = ~ethnicity_source_value, y = ~n, type = 'bar', marker = list(color = 'orchid')) %>%
          layout(title = "Top Ethnicity Source Values", yaxis = list(title = "Count"), xaxis = list(title = "Ethnicity"))
      })
    },
      error = function(e) {
      removeModal()
      shinyalert::shinyalert("Pipeline Error", as.character(e), type = "error")
    })
  })

  
  # --- Feature Extraction UI Elements ---
  output$SelectCohortTable <- renderUI({
    req(input$CohortNameID)
    selectInput("SelectedCohort", "Select Cohort Table", choices = c(input$CohortNameID))
  })
  
  output$ExtractFeaturesID <- renderUI({
    req(cohort_conn$cdm, input$CohortNameID)
    actionButton("ExtractFeaturesID", "Extract Features", class = "btn-primary")
  })
  
  output$CovariateTableUI <- renderUI({
    req(cohort_conn$covariateData)
    tagList(
      h4("Sample Extracted Features"),
      tableOutput("covariate_table"),
      downloadButton("DownloadCovariates", "Download Covariates")
    )
  })
  
  output$covariate_table <- renderTable({
    req(cohort_conn$covariateData)
    if (is.null(cohort_conn$covariateData$covariates)) {
      return(data.frame(message = "No covariates found."))
    }
    head(cohort_conn$covariateData$covariates %>% dplyr::collect(), 10)
  })
  
  output$DownloadCovariates <- downloadHandler(
    filename = function() paste0("covariates-", Sys.Date(), ".csv"),
    content = function(file) {
      df <- cohort_conn$covariateData$covariates %>% dplyr::collect()
      readr::write_csv(df, file)
    }
  )
  
  
}
