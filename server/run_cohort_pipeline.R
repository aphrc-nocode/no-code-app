run_cohort_pipeline <- function() {
  
  library(dplyr)
  library(lubridate)
  library(glue)
  library(plotly)
  library(CDMConnector)
  library(CodelistGenerator)
  library(CohortConstructor)
  library(shinyalert)
  library(tidyr)
  library(zip)
  library(DT)
  
  
  cohort_conn <- reactiveValues(
    schema_names   = NULL,
    cdm            = NULL,
    person_summary = NULL,
    summary_stats  = NULL
  )
  
  # --- 1. Fetch available schemas ---
  observe({
    req(rv_database$conn)
    tryCatch({
      cohort_conn$schema_names <- DBI::dbGetQuery(
        rv_database$conn,
        "SELECT schema_name FROM information_schema.schemata 
         WHERE schema_name NOT LIKE 'pg_%' AND schema_name <> 'information_schema'"
      )$schema_name
      
      output$CDMSchemaName    <- renderUI({ selectInput("CDMSchemaName", "CDM Schema", choices = cohort_conn$schema_names) })
      output$ResultSchemaName <- renderUI({ selectInput("ResultSchemaName", "Results Schema", choices = cohort_conn$schema_names) })
      output$CDMConnName      <- renderUI({ textInput("CDMConnName", "CDM Connection Name", value = "my_cdm") })
      output$CreateCDMID      <- renderUI({ actionButton("CreateCDMID", "Create CDM", class = "btn-success") })
      
    }, error = function(e) {
      shinyalert::shinyalert("Error", e$message, type = "error")
    })
  })
  
  # --- 2. Create CDM reference ---
  observeEvent(input$CreateCDMID, {
    req(rv_database$conn, input$CDMConnName, input$CDMSchemaName, input$ResultSchemaName)
    tryCatch({
      cohort_conn$cdm <- CDMConnector::cdmFromCon(
        con           = rv_database$conn,
        cdmName       = input$CDMConnName,
        cdmSchema     = input$CDMSchemaName,
        writeSchema   = input$ResultSchemaName,
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
  
  # --- 3. Render cohort creation inputs ---
  observe({
    req(cohort_conn$cdm)
    output$ConceptKeyword   <- renderUI({ textInput("ConceptKeyword", "Keywords (comma separated)") })
    output$CohortNameID     <- renderUI({ textInput("CohortNameID", "Cohort Table Name") })
    output$CohortDateID     <- renderUI({ selectInput("CohortDateID", "Cohort Exit Date", choices = c("event_start_date", "event_end_date")) })
    output$GenerateCohortID <- renderUI({ actionButton("run_cohort", "Run Pipeline", class = "btn-success") })
  })
  
  # --- 4. Cohort creation and summary ---
  observeEvent(input$run_cohort, {
    req(cohort_conn$cdm, input$ConceptKeyword, input$CohortNameID, input$CohortDateID)
    showModal(modalDialog("Running cohort pipeline... Please wait.", footer = NULL))
    
    tryCatch({
      # Split keywords
      keywords <- trimws(unlist(strsplit(input$ConceptKeyword, ",")))
      codes_list <- list()
      valid_keywords <- c()
      invalid_keywords <- c()
      
      for (kw in keywords) {
        concept_codes <- CodelistGenerator::getCandidateCodes(cohort_conn$cdm, kw)$concept_id
        concept_codes <- concept_codes[!is.na(concept_codes)]
        concept_codes <- as.integer(concept_codes)
        if (length(concept_codes) == 0) {
          invalid_keywords <- c(invalid_keywords, kw)
        } else {
          valid_keywords <- c(valid_keywords, kw)
          codes_list[[kw]] <- concept_codes
        }
      }
      
      if (length(valid_keywords) == 0) {
        removeModal()
        shinyalert::shinyalert("No Valid Keywords", "None of the keywords were found.", type = "error")
        return(NULL)
      }
      
      if (length(invalid_keywords) > 0) {
        shinyalert::shinyalert("Some Keywords Skipped", paste("Skipped:", paste(invalid_keywords, collapse = ", ")), type = "warning")
      }
      
      # Create codelist and cohort
      study_codes <- CodelistGenerator::newCodelist(codes_list)
      cohort_conn$cdm[[input$CohortNameID]] <- cohort_conn$cdm %>%
        CohortConstructor::conceptCohort(
          conceptSet = study_codes,
          name       = input$CohortNameID,
          exit       = input$CohortDateID
        )
      
      # Fetch cohort table
      cohort_table <- DBI::dbGetQuery(rv_database$conn,
                                      glue::glue("SELECT * FROM {input$ResultSchemaName}.{input$CohortNameID}"))
      
      # Fetch person table and calculate age
      person_df <- cohort_conn$cdm$person %>% dplyr::collect() %>%
        mutate(
          birth_date = make_date(year = year_of_birth, month = month_of_birth, day = day_of_birth),
          age = floor(interval(birth_date, Sys.Date()) / years(1)),
          age_group = cut(age, breaks = seq(0, 100, 10), right = FALSE, include.lowest = TRUE)
        )
      
      # Join with cohort
      cohort_full <- cohort_table %>%
        inner_join(person_df, by = c("subject_id" = "person_id"))
      
      cohort_conn$person_summary <- cohort_full
      
      # --- SUMMARY STATS (VERTICAL TABLE) ---
      stats_list <- list(
        "Total N" = nrow(cohort_full),
        "Mean Age" = mean(cohort_full$age, na.rm = TRUE),
        "Median Age" = median(cohort_full$age, na.rm = TRUE),
        "Min Age" = min(cohort_full$age, na.rm = TRUE),
        "Max Age" = max(cohort_full$age, na.rm = TRUE)
      )
      
      gender_counts <- cohort_full %>%
        count(Gender = gender_source_value) %>%
        mutate(Gender = paste("Gender:", Gender)) %>%
        rename(Value = n)
      
      race_counts <- cohort_full %>%
        count(Race = race_source_value) %>%
        mutate(Race = paste("Race:", Race)) %>%
        rename(Value = n)
      
      ethnicity_counts <- cohort_full %>%
        count(Ethnicity = ethnicity_source_value) %>%
        mutate(Ethnicity = paste("Ethnicity:", Ethnicity)) %>%
        rename(Value = n)
      
      age_group_counts <- cohort_full %>%
        count(Age_Group = age_group) %>%
        mutate(Age_Group = paste("Age group:", Age_Group)) %>%
        rename(Value = n)
      
      summary_df <- bind_rows(
        tibble(Statistic = names(stats_list), Value = unlist(stats_list)),
        tibble(Statistic = gender_counts$Gender, Value = gender_counts$Value),
        tibble(Statistic = race_counts$Race, Value = race_counts$Value),
        tibble(Statistic = ethnicity_counts$Ethnicity, Value = ethnicity_counts$Value),
        tibble(Statistic = age_group_counts$Age_Group, Value = age_group_counts$Value)
      )
      
      cohort_conn$summary_stats <- summary_df
      
      removeModal()
      shinyalert::shinyalert("Success", "Cohort created and summary statistics generated!", type = "success")
      
    }, error = function(e) {
      removeModal()
      shinyalert::shinyalert("Pipeline Error", as.character(e), type = "error")
    })
  })
  
  # --- 5. Render summary stats table ---
  output$cohort_summary <- DT::renderDataTable({
    req(cohort_conn$summary_stats)
    datatable(
      cohort_conn$summary_stats,
      options = list(
        pageLength = 10,          # show 10 rows at a time
        scrollX = TRUE,           # horizontal scrolling
        scrollY = "400px",        # vertical scroll height
        dom = 't<"bottom"lip>'    # show table, info, and pagination below
      ),
      rownames = FALSE
    )
  })
  
  
  # --- 6. Render interactive cohort plots ---
  output$Gender_plot <- renderPlotly({
    req(cohort_conn$person_summary)
    df <- cohort_conn$person_summary %>% filter(!is.na(gender_source_value))
    plot_ly(df, x = ~gender_source_value, type = "histogram", marker = list(color = 'green')) %>%
      layout(title = "Gender Distribution", xaxis = list(title = "Gender"), yaxis = list(title = "Count"))
  })
  
  output$age_group_plot <- renderPlotly({
    req(cohort_conn$person_summary)
    df <- cohort_conn$person_summary %>% filter(!is.na(age_group))
    plot_ly(df, x = ~age_group, type = "histogram", marker = list(color = 'green')) %>%
      layout(title = "Age Group Distribution", xaxis = list(title = "Age Group"), yaxis = list(title = "Count"))
  })
  
  output$Race_plot <- renderPlotly({
    req(cohort_conn$person_summary)
    df <- cohort_conn$person_summary %>% filter(!is.na(race_source_value))
    plot_ly(df, x = ~race_source_value, type = "histogram", marker = list(color = 'green')) %>%
      layout(title = "Race Distribution", xaxis = list(title = "Race"), yaxis = list(title = "Count"))
  })
  
  output$Ethnicity_plot <- renderPlotly({
    req(cohort_conn$person_summary)
    df <- cohort_conn$person_summary %>% filter(!is.na(ethnicity_source_value))
    ethnicity_counts <- df %>% count(ethnicity_source_value) %>%
      mutate(ethnicity_group = ifelse(n < 100, "Other", ethnicity_source_value))
    
    plot_ly(ethnicity_counts, x = ~ethnicity_group, y = ~n, type = 'bar',
            marker = list(color = 'green')) %>%
      layout(title = "Ethnicity Distribution", xaxis = list(title = "Ethnicity"), yaxis = list(title = "Count"))
  })
  
  # --- 7. DOWNLOAD HANDLERS ---
  
  # Download summary table
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("cohort_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(cohort_conn$summary_stats, file, row.names = FALSE)
    }
  )
  
  # Download plots as ZIP (PNG)
  # --- 7. DOWNLOAD HANDLERS ---
  
  # Download summary table
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("cohort_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(cohort_conn$summary_stats, file, row.names = FALSE)
    }
  )
  
  # Download plots as ZIP (using webshot2 instead of orca)
  # Download plots as ZIP (using webshot2 safely)
  output$download_plots <- downloadHandler(
    filename = function() {
      paste0("cohort_plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      library(webshot2)
      library(plotly)
      library(htmlwidgets)
      library(zip)
      
      tmpdir <- tempdir()
      setwd(tmpdir)
      
      # Rebuild the same plots from data
      df <- cohort_conn$person_summary
      
      # 1. Gender plot
      gender_plot <- plot_ly(df, x = ~gender_source_value, type = "histogram", marker = list(color = 'green')) %>%
        layout(title = "Gender Distribution", xaxis = list(title = "Gender"), yaxis = list(title = "Count"))
      htmlwidgets::saveWidget(gender_plot, "Gender_plot.html", selfcontained = TRUE)
      webshot2::webshot("Gender_plot.html", "Gender_plot.png", vwidth = 1200, vheight = 800)
      
      # 2. Age group plot
      age_plot <- plot_ly(df, x = ~age_group, type = "histogram", marker = list(color = 'green')) %>%
        layout(title = "Age Group Distribution", xaxis = list(title = "Age Group"), yaxis = list(title = "Count"))
      htmlwidgets::saveWidget(age_plot, "Age_group_plot.html", selfcontained = TRUE)
      webshot2::webshot("Age_group_plot.html", "Age_group_plot.png", vwidth = 1200, vheight = 800)
      
      # 3. Race plot
      race_plot <- plot_ly(df, x = ~race_source_value, type = "histogram", marker = list(color = 'green')) %>%
        layout(title = "Race Distribution", xaxis = list(title = "Race"), yaxis = list(title = "Count"))
      htmlwidgets::saveWidget(race_plot, "Race_plot.html", selfcontained = TRUE)
      webshot2::webshot("Race_plot.html", "Race_plot.png", vwidth = 1200, vheight = 800)
      
      # 4. Ethnicity plot
      ethnicity_counts <- df %>% count(ethnicity_source_value) %>%
        mutate(ethnicity_group = ifelse(n < 100, "Other", ethnicity_source_value))
      ethnicity_plot <- plot_ly(ethnicity_counts, x = ~ethnicity_group, y = ~n, type = 'bar',
                                marker = list(color = 'green')) %>%
        layout(title = "Ethnicity Distribution", xaxis = list(title = "Ethnicity"), yaxis = list(title = "Count"))
      htmlwidgets::saveWidget(ethnicity_plot, "Ethnicity_plot.html", selfcontained = TRUE)
      webshot2::webshot("Ethnicity_plot.html", "Ethnicity_plot.png", vwidth = 1200, vheight = 800)
      
      # Zip all PNGs
      zip::zip(file, c("Gender_plot.png", "Age_group_plot.png", "Race_plot.png", "Ethnicity_plot.png"))
    }
  )
  
  
}

