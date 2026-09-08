run_cohort_pipeline <- function() {
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
      
      output$CDMSchemaName    <- renderUI({
        selectInput("CDMSchemaName", "CDM Schema", choices = cohort_conn$schema_names)
      })
      output$ResultSchemaName <- renderUI({
        selectInput("ResultSchemaName", "Results Schema", choices = cohort_conn$schema_names)
      })
      output$CDMConnName      <- renderUI({
        textInput("CDMConnName", "CDM Connection Name", value = "my_cdm")
      })
      output$CreateCDMID      <- renderUI({
        actionButton("CreateCDMID", "Create CDM", class = "btn-success")
      })
      
    }, error = function(e) {
      shinyalert::shinyalert("Error", e$message, type = "error")
    })
  })
  
  #-------- Redirection to source ------------------#
  output$schema_cohort <- renderUI({
    if (is.null(rv_database$conn)) {
      actionButton(
        "go_to_source_global",
        "Click to connect to a database in the Source page first",
        style = "background-color: #7bc148",
        icon = icon("arrow-right")
      )
    }
  })
  
  observeEvent(input$go_to_source_global, {
    updateTabItems(session, "tabs", "sourcedata")
  })
  #-------------------------------------------------#
  
  # --- 2. Create CDM reference ---
  observeEvent(input$CreateCDMID, {
    req(rv_database$conn, input$CDMConnName, input$CDMSchemaName, input$ResultSchemaName)
    tryCatch({
      cohort_conn$cdm <- CDMConnector::cdmFromCon(
        con            = rv_database$conn,
        cdmName        = input$CDMConnName,
        cdmSchema      = input$CDMSchemaName,
        writeSchema    = input$ResultSchemaName,
        achillesSchema = NULL
      )
      
      if (is.null(cohort_conn$cdm) || !inherits(cohort_conn$cdm, "cdm_reference")) {
        shinyalert::shinyalert(
          "CDM Creation Failed",
          "CDM reference was not created successfully.",
          type = "error"
        )
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
    output$ConceptKeyword   <- renderUI({
      textInput("ConceptKeyword", "Keywords (comma separated)")
    })
    output$CohortNameID     <- renderUI({
      textInput("CohortNameID", "Cohort Table Name")
    })
    output$CohortDateID     <- renderUI({
      selectInput("CohortDateID", "Cohort Exit Date",
                  choices = c("event_start_date", "event_end_date"))
    })
    output$GenerateCohortID <- renderUI({
      actionButton("run_cohort", "Run Pipeline", class = "btn-success")
    })
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
        shinyalert::shinyalert(
          "Some Keywords Skipped",
          paste("Skipped:", paste(invalid_keywords, collapse = ", ")),
          type = "warning"
        )
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
      cohort_table <- DBI::dbGetQuery(
        rv_database$conn,
        glue::glue("SELECT * FROM {input$ResultSchemaName}.{input$CohortNameID}")
      )
      
      # Fetch person table and calculate age
      person_df <- cohort_conn$cdm$person %>%
        dplyr::collect() %>%
        dplyr::mutate(
          birth_date = lubridate::make_date(
            year = year_of_birth,
            month = month_of_birth,
            day = day_of_birth
          ),
          age = floor(lubridate::interval(birth_date, Sys.Date()) / lubridate::years(1)),
          age_group = cut(
            age,
            breaks = seq(0, 100, 10),
            right = FALSE,
            include.lowest = TRUE
          )
        )
      
      # Join with cohort
      cohort_full <- cohort_table %>%
        dplyr::inner_join(person_df, by = c("subject_id" = "person_id"))
      
      cohort_conn$person_summary <- cohort_full
      
      # detect ethnicity column safely
      ethnicity_col <- NULL
      if ("ethnicity_source_value" %in% names(cohort_full)) {
        ethnicity_col <- "ethnicity_source_value"
      } else if ("ethnicity_source_concept_id" %in% names(cohort_full)) {
        ethnicity_col <- "ethnicity_source_concept_id"
      } else if ("ethnicity_concept_id" %in% names(cohort_full)) {
        ethnicity_col <- "ethnicity_concept_id"
      }
      
      # --- SUMMARY STATS ---
      stats_list <- list(
        "Total N"    = nrow(cohort_full),
        "Mean Age"   = mean(cohort_full$age, na.rm = TRUE),
        "Median Age" = median(cohort_full$age, na.rm = TRUE),
        "Min Age"    = min(cohort_full$age, na.rm = TRUE),
        "Max Age"    = max(cohort_full$age, na.rm = TRUE)
      )
      
      gender_counts <- cohort_full %>%
        dplyr::count(Gender = gender_source_value) %>%
        dplyr::mutate(Gender = paste("Gender:", ifelse(is.na(Gender), "Unknown", as.character(Gender)))) %>%
        dplyr::rename(Value = n)
      
      race_counts <- cohort_full %>%
        dplyr::count(Race = race_source_value) %>%
        dplyr::mutate(Race = paste("Race:", ifelse(is.na(Race), "Unknown", as.character(Race)))) %>%
        dplyr::rename(Value = n)
      
      if (!is.null(ethnicity_col)) {
        ethnicity_counts <- cohort_full %>%
          dplyr::mutate(Ethnicity = .data[[ethnicity_col]]) %>%
          dplyr::count(Ethnicity) %>%
          dplyr::mutate(Ethnicity = paste("Ethnicity:", ifelse(is.na(Ethnicity), "Unknown", as.character(Ethnicity)))) %>%
          dplyr::rename(Value = n)
      } else {
        ethnicity_counts <- tibble::tibble(
          Ethnicity = "Ethnicity: Not available",
          Value = NA_real_
        )
      }
      
      age_group_counts <- cohort_full %>%
        dplyr::count(Age_Group = age_group) %>%
        dplyr::mutate(Age_Group = paste("Age group:", ifelse(is.na(Age_Group), "Unknown", as.character(Age_Group)))) %>%
        dplyr::rename(Value = n)
      
      summary_df <- dplyr::bind_rows(
        tibble::tibble(
          Statistic = names(stats_list),
          Value = as.numeric(unlist(stats_list))
        ),
        gender_counts %>% dplyr::rename(Statistic = Gender),
        race_counts %>% dplyr::rename(Statistic = Race),
        ethnicity_counts %>% dplyr::rename(Statistic = Ethnicity),
        age_group_counts %>% dplyr::rename(Statistic = Age_Group)
      )
      
      cohort_conn$summary_stats <- summary_df
      
      removeModal()
      shinyalert::shinyalert(
        "Success",
        "Cohort created and summary statistics generated!",
        type = "success"
      )
      
    }, error = function(e) {
      removeModal()
      shinyalert::shinyalert("Pipeline Error", as.character(e), type = "error")
    })
  })
  
  # --- 5. Render summary stats table ---
  output$cohort_summary <- DT::renderDataTable({
    req(cohort_conn$summary_stats)
    
    DT::datatable(
      cohort_conn$summary_stats,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 't<"bottom"lip>'
      ),
      rownames = FALSE
    )
  })
  
  # --- 6. Render interactive cohort plots ---
  output$Gender_plot <- renderPlotly({
    req(cohort_conn$person_summary)
    df <- cohort_conn$person_summary %>% dplyr::filter(!is.na(gender_source_value))
    
    plotly::plot_ly(
      df,
      x = ~gender_source_value,
      type = "histogram",
      marker = list(color = "green")
    ) %>%
      plotly::layout(
        title = "Gender Distribution",
        xaxis = list(title = "Gender"),
        yaxis = list(title = "Count")
      )
  })
  
  output$age_group_plot <- renderPlotly({
    req(cohort_conn$person_summary)
    df <- cohort_conn$person_summary %>% dplyr::filter(!is.na(age_group))
    
    plotly::plot_ly(
      df,
      x = ~age_group,
      type = "histogram",
      marker = list(color = "green")
    ) %>%
      plotly::layout(
        title = "Age Group Distribution",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Count")
      )
  })
  
  output$Race_plot <- renderPlotly({
    req(cohort_conn$person_summary)
    df <- cohort_conn$person_summary %>% dplyr::filter(!is.na(race_source_value))
    
    plotly::plot_ly(
      df,
      x = ~race_source_value,
      type = "histogram",
      marker = list(color = "green")
    ) %>%
      plotly::layout(
        title = "Race Distribution",
        xaxis = list(title = "Race"),
        yaxis = list(title = "Count")
      )
  })
  
  output$Ethnicity_plot <- renderPlotly({
    req(cohort_conn$person_summary)
    
    df <- cohort_conn$person_summary
    
    # detect available ethnicity column
    ethnicity_col <- NULL
    if ("ethnicity_source_value" %in% names(df)) {
      ethnicity_col <- "ethnicity_source_value"
    } else if ("ethnicity_source_concept_id" %in% names(df)) {
      ethnicity_col <- "ethnicity_source_concept_id"
    } else if ("ethnicity_concept_id" %in% names(df)) {
      ethnicity_col <- "ethnicity_concept_id"
    }
    
    # fallback if no column
    if (is.null(ethnicity_col)) {
      return(
        plotly::plot_ly() %>%
          plotly::layout(
            title = "Ethnicity Distribution",
            annotations = list(
              list(
                text = "No ethnicity column found in the data",
                x = 0.5,
                y = 0.5,
                showarrow = FALSE,
                xref = "paper",
                yref = "paper"
              )
            )
          )
      )
    }
    
    ethnicity_counts <- df %>%
      dplyr::mutate(
        ethnicity_value = .data[[ethnicity_col]],
        ethnicity_value = ifelse(is.na(ethnicity_value), "Unknown", as.character(ethnicity_value))
      ) %>%
      dplyr::count(ethnicity_value, name = "Count") %>%
      dplyr::mutate(
        ethnicity_group = ifelse(Count < 100, "Other", ethnicity_value)
      ) %>%
      dplyr::group_by(ethnicity_group) %>%
      dplyr::summarise(Count = sum(Count), .groups = "drop")
    
    plotly::plot_ly(
      data = ethnicity_counts,
      x = ~ethnicity_group,
      y = ~Count,
      type = "bar",
      marker = list(color = "green")  # ✅ green like other plots
    ) %>%
      plotly::layout(
        title = "Ethnicity Distribution",
        xaxis = list(title = "Ethnicity"),
        yaxis = list(title = "Count")
      )
  })
  
  # --- 7. DOWNLOAD HANDLERS ---
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("cohort_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(cohort_conn$summary_stats)
      write.csv(cohort_conn$summary_stats, file, row.names = FALSE)
    }
  )
  
  output$download_plots <- downloadHandler(
    filename = function() {
      paste0("cohort_plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(cohort_conn$person_summary)
      
      tmpdir <- tempdir()
      oldwd <- getwd()
      on.exit(setwd(oldwd), add = TRUE)
      setwd(tmpdir)
      
      df <- cohort_conn$person_summary
      
      gender_plot <- plotly::plot_ly(
        df %>% dplyr::filter(!is.na(gender_source_value)),
        x = ~gender_source_value,
        type = "histogram",
        marker = list(color = "green")
      ) %>%
        plotly::layout(
          title = "Gender Distribution",
          xaxis = list(title = "Gender"),
          yaxis = list(title = "Count")
        )
      htmlwidgets::saveWidget(gender_plot, "Gender_plot.html", selfcontained = TRUE)
      webshot2::webshot("Gender_plot.html", "Gender_plot.png", vwidth = 1200, vheight = 800)
      
      age_plot <- plotly::plot_ly(
        df %>% dplyr::filter(!is.na(age_group)),
        x = ~age_group,
        type = "histogram",
        marker = list(color = "green")
      ) %>%
        plotly::layout(
          title = "Age Group Distribution",
          xaxis = list(title = "Age Group"),
          yaxis = list(title = "Count")
        )
      htmlwidgets::saveWidget(age_plot, "Age_group_plot.html", selfcontained = TRUE)
      webshot2::webshot("Age_group_plot.html", "Age_group_plot.png", vwidth = 1200, vheight = 800)
      
      race_plot <- plotly::plot_ly(
        df %>% dplyr::filter(!is.na(race_source_value)),
        x = ~race_source_value,
        type = "histogram",
        marker = list(color = "green")
      ) %>%
        plotly::layout(
          title = "Race Distribution",
          xaxis = list(title = "Race"),
          yaxis = list(title = "Count")
        )
      htmlwidgets::saveWidget(race_plot, "Race_plot.html", selfcontained = TRUE)
      webshot2::webshot("Race_plot.html", "Race_plot.png", vwidth = 1200, vheight = 800)
      
      ethnicity_col <- NULL
      if ("ethnicity_source_value" %in% names(df)) {
        ethnicity_col <- "ethnicity_source_value"
      } else if ("ethnicity_source_concept_id" %in% names(df)) {
        ethnicity_col <- "ethnicity_source_concept_id"
      } else if ("ethnicity_concept_id" %in% names(df)) {
        ethnicity_col <- "ethnicity_concept_id"
      }
      
      if (!is.null(ethnicity_col)) {
        ethnicity_counts <- df %>%
          dplyr::mutate(ethnicity_value = .data[[ethnicity_col]]) %>%
          dplyr::mutate(
            ethnicity_value = ifelse(is.na(ethnicity_value), "Unknown", as.character(ethnicity_value))
          ) %>%
          dplyr::count(ethnicity_value) %>%
          dplyr::mutate(ethnicity_group = ifelse(n < 100, "Other", ethnicity_value)) %>%
          dplyr::group_by(ethnicity_group) %>%
          dplyr::summarise(n = sum(n), .groups = "drop")
        
        ethnicity_plot <- plotly::plot_ly(
          ethnicity_counts,
          x = ~ethnicity_group,
          y = ~n,
          type = "bar",
          marker = list(color = "green")
        ) %>%
          plotly::layout(
            title = "Ethnicity Distribution",
            xaxis = list(title = "Ethnicity"),
            yaxis = list(title = "Count")
          )
        
        htmlwidgets::saveWidget(ethnicity_plot, "Ethnicity_plot.html", selfcontained = TRUE)
        webshot2::webshot("Ethnicity_plot.html", "Ethnicity_plot.png", vwidth = 1200, vheight = 800)
        
        zip::zip(
          file,
          c("Gender_plot.png", "Age_group_plot.png", "Race_plot.png", "Ethnicity_plot.png")
        )
      } else {
        zip::zip(
          file,
          c("Gender_plot.png", "Age_group_plot.png", "Race_plot.png")
        )
      }
    }
  )
  
  # --- 8. REACTIVE FLAGS FOR CONDITIONAL PANELS ---
  output$cdmCreated <- reactive({
    !is.null(cohort_conn$cdm)
  })
  outputOptions(output, "cdmCreated", suspendWhenHidden = FALSE)
  
  output$summaryAvailable <- reactive({
    !is.null(cohort_conn$summary_stats)
  })
  outputOptions(output, "summaryAvailable", suspendWhenHidden = FALSE)
  
  output$plotsAvailable <- reactive({
    !is.null(cohort_conn$person_summary)
  })
  outputOptions(output, "plotsAvailable", suspendWhenHidden = FALSE)
}