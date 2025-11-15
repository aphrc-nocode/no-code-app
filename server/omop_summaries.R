omopVizServer <- function() {
  omop_conn <- reactiveValues(
    details = NULL,
    conn = NULL,
    schemas = NULL,
    cdm = NULL,
    cdm_tables = NULL
  )
  
  #------------ Reuse existing connection if available ------------------------#
  observe({
  	req(isTRUE(!is.null(rv_database$conn)), DBI::dbIsValid(rv_database$conn), isTRUE(is.null(omop_conn$conn)))
    if (!is.null(rv_database$conn) && DBI::dbIsValid(rv_database$conn) && is.null(omop_conn$conn)) {
      tryCatch({
        omop_conn$conn <- rv_database$conn
        omop_conn$details <- rv_database$details
        
        schema_query <- "
          SELECT schema_name 
          FROM information_schema.schemata 
          WHERE schema_name NOT LIKE 'pg_%' 
            AND schema_name <> 'information_schema'
        "
        omop_conn$schemas <- DatabaseConnector::querySql(omop_conn$conn, schema_query)$schema_name
        
        output$schema_selection <- renderUI({
          tagList(
            selectInput("cdm_schema",
                        get_rv_labels("cdm_schema"),
                        choices = omop_conn$schemas,
                        selected = NULL),
            
            selectInput("results_schema",
                        get_rv_labels("results_schema"),
                        choices = omop_conn$schemas,
                        selected = NULL),
            
            selectInput("vocab_schema",
                        get_rv_labels("vocab_schema"),
                        choices = c("CDM schema if none" = "", omop_conn$schemas),
                        selected = ""),
            
            selectInput("cdm_version",
                        get_rv_labels("cdm_version"),
                        choices = c("5.3", "5.4"),
                        selected = "5.4")
          )
        })
        
        output$generate_summaries <- renderUI({
            actionButton(
              "generate_summaries",
              "Generate Summaries",
              icon = icon("play"),
              class = "btn-success")
            })
        
      }, error = function(e) {
        shinyalert::shinyalert("Connection Error", e$message, type = "error")
      })
    }
  })
  
  
  #------------ Redirection to source page ------------------------------------#
  
  output$schema_selection <- renderUI({
    if (is.null(omop_conn$conn) || !DBI::dbIsValid(omop_conn$conn)) {
        actionButton("go_to_source_omop"
                     ,"Click to connect to a database in the Source page first"
                     ,style = "background-color: #7bc148"
                     ,icon = icon("arrow-right"))
      
    }
  })
  
  # Navigate to Source page
  observeEvent(input$go_to_source_omop, {
    updateTabItems(session, "tabs", "sourcedata")
  })
  
  
  
  #----------------------------------------------------------------------------#
  #                    General Summaries                          
  #----------------------------------------------------------------------------#
  
  
  # Record counts ----
  
  observeEvent(input$generate_summaries,{
    req(input$cdm_schema
        ,input$results_schema
        ,omop_conn$conn)
    

    tryCatch({
      tables_query <- glue::glue("
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = '{input$cdm_schema}'
      ")
      table_list <- DatabaseConnector::querySql(omop_conn$conn, tables_query)$table_name

      output$cdm_table_summaries <- DT::renderDataTable({
        tbl_counts <- purrr::map_dfr(table_list, function(tbl) {
          count_query <- glue::glue("SELECT COUNT(*) AS COUNT FROM {input$cdm_schema}.\"{tbl}\"")
          tryCatch({
            res <- DatabaseConnector::querySql(omop_conn$conn, count_query)
            tibble::tibble(Table = tbl, Records = res$count[1])
          }, error = function(e) {
            tibble::tibble(Table = tbl, Records = NA_integer_)
          })
        })
        DT::datatable(
          tbl_counts,
          options = list(pageLength = 10, order = list(list(1, 'desc'))),
          rownames = FALSE
        )
      })

      # Store non-empty CDM tables for later dropdown
      tbl_counts <- purrr::map_dfr(table_list, function(tbl) {
        count_query <- glue::glue("SELECT COUNT(*) AS COUNT FROM {input$cdm_schema}.\"{tbl}\"")
        res <- tryCatch(DatabaseConnector::querySql(omop_conn$conn, count_query), error = function(e) NULL)
        tibble::tibble(Table = tbl, Records = ifelse(is.null(res), 0, res$count[1]))
      })

      omop_conn$cdm_tables <- tbl_counts |>
        dplyr::filter(Records > 0) |>
        dplyr::pull(Table)

    }, error = function(e) {
      shinyalert::shinyalert("Record Count Error", e$message, type = "error")
    })
  })

  
 
  ##======== Populate table dropdown with non-empty CDM tables ======#
  
  observeEvent(input$generate_summaries, {
    req(input$cdm_schema,input$results_schema, omop_conn$conn)

    tryCatch({
      output$omop_cdm_tables <- renderUI({
        if (!is.null(omop_conn$cdm_tables) && length(omop_conn$cdm_tables) > 0) {
          selectInput("selected_cdm_table", "Select a CDM Table", choices = omop_conn$cdm_tables)
        } else {
          p("No non-empty CDM tables found in this schema.")
        }
      })

    }, error = function(e) {
      shinyalert::shinyalert("Table Load Error", e$message, type = "error")
    })
  })

  
  # CDM Source summary counts ----
 
    observeEvent(input$generate_summaries, {
      req(input$results_schema, omop_conn$conn)
      
    con <- omop_conn$conn
    
    
    output$cdm_source_info <- renderUI({
      src <- tryCatch(dbGetQuery(con, glue::glue("SELECT cdm_source_name,
                                         cdm_source_abbreviation,
                                         cdm_release_date,
                                         vocabulary_version
                                          FROM {input$cdm_schema}.cdm_source
                                        "),
                                 error = function(e) NULL))
      
      person_count <- dbGetQuery(con,
                                 glue::glue("SELECT COUNT(*) AS n 
                                            FROM {input$cdm_schema}.person"))
      obs_period <- dbGetQuery(con,
      glue::glue("SELECT MIN(observation_period_start_date) AS min_start,
                  MAX(observation_period_end_date)   AS max_end
                  FROM {input$cdm_schema}.observation_period
                 "))
      
      tagList(
        div(style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 1px 1px 3px #ccc;",
            p(strong("CDM Source:"),src$cdm_source_name),
            p(strong("Abbreviation:"),src$cdm_source_abbreviation),
            p(strong("Release Date:"),src$cdm_release_date),
            p(strong("Vocabulary Version:"),src$vocabulary_version),
            p(strong("Number of Persons:"),person_count$n),
            p(strong("Observation Period:"), paste(obs_period$min_start, obs_period$max_end, sep = " → "))
            ))
    })
    
 

    # Non-standard / Unmapped Concepts (dynamic) ----
    
    output$nonstandard_concepts <- DT::renderDataTable({
  
      cdm_tables <- dbGetQuery(con,
                               glue::glue("
                            SELECT DISTINCT  c1.table_name,
                            c1.column_name AS concept_col,
                            c2.column_name AS source_col
                            FROM information_schema.columns c1
                            JOIN information_schema.columns c2
                            ON c1.table_schema = c2.table_schema
                            AND c1.table_name = c2.table_name
                            WHERE c1.table_schema = '{input$cdm_schema}'
                            AND c1.column_name = (c1.table_name || '_concept_id')
                            AND c2.column_name = (c1.table_name || '_source_value')
                            AND c2.column_name NOT LIKE '%value_source_value%'
                            AND c1.table_name NOT IN ('cdm_source')
                                          ")
      )
      
        queries <- purrr::map(seq_len(nrow(cdm_tables)), function(i) {
        tbl <- cdm_tables$table_name[i]
        concept_col <- cdm_tables$concept_col[i]
        source_col <- cdm_tables$source_col[i]
        
        glue::glue("SELECT DISTINCT
                    '{tbl}' AS table_name,
                    t.{concept_col} AS concept_id,
                    t.{source_col} AS source_value,
                    COUNT(*) AS n
                    FROM {input$cdm_schema}.\"{tbl}\" t
                    WHERE t.{concept_col} <> 0 
                    AND (t.{concept_col} > 2000000000 OR t.{concept_col} IS NULL)
                    GROUP BY t.{concept_col}, t.{source_col}
                   ")
      })
      
      if (length(queries) == 0) {
        return(data.frame(message = "No tables with *_concept_id + *_source_value found"))
      }
      
      full_q <- paste(queries, collapse = " UNION ALL ")
      res <- dbGetQuery(con, full_q)
      
      # keep only rows with counts > 0
      res <- res[res$n > 0, ]
      
      res
    })
   
     
    # ---- Domain Distribution ----
    
    output$domain_distribution <- renderPlotly({
    cdm_tables <- dbGetQuery(con, glue::glue("SELECT table_name,
                                              column_name
                                              FROM information_schema.columns
                                              WHERE table_schema = '{input$cdm_schema}'
                                              AND column_name LIKE '%_concept_id'
                                             "))
      
      # Step 2: loop through and collect counts
      results <- purrr::map_dfr(seq_len(nrow(cdm_tables)), function(i) {
        tbl <- cdm_tables$table_name[i]
        col <- cdm_tables$column_name[i]
        
        qry <- glue::glue("SELECT c.domain_id, COUNT(*) AS n
                          FROM {input$cdm_schema}.\"{tbl}\" t
                          JOIN {input$vocab_schema}.concept c
                          ON t.{col} = c.concept_id
                          WHERE t.{col} <> 0
                          GROUP BY c.domain_id
                          ")
        
        out <- tryCatch(dbGetQuery(con, qry), error = function(e) NULL)
        if (!is.null(out) && nrow(out) > 0) out else NULL
      })
      
      # Step 3: aggregate across all tables
      dom <- results %>%
        dplyr::group_by(domain_id) %>%
        dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(desc(n))
      
      # Step 4: horizontal bar plot
      plot_ly(dom,
              x = ~n,
              y = ~domain_id,
              type = 'bar',
              orientation = 'h',
              marker = list(color = 'steelblue')) %>%
        layout(
          title = "Concept Domain Distribution",
          xaxis = list(title = "Record Count"),
          yaxis = list(title = "Domain", categoryorder = "total ascending")
        )
    })
  
    
    })
    
  
    #----------------------------------------------------------------------------#
    #                   Table-specific analysis                          
    #----------------------------------------------------------------------------#

    
    # Gender filter dropdown(to edit for translation) ---
    output$gender_filter_ui <- renderUI({
      selectInput(
        inputId = "gender_filter",
        label = "Filter by Gender",
        choices = c("All", "Male", "Female"),
        selected = "All"
      )
    })
  
  observe({
    req(omop_conn$conn,
        input$selected_cdm_table,
        input$cdm_schema,
        input$vocab_schema)
    
    con <- omop_conn$conn
    
    ## Person table ----
    if (input$selected_cdm_table == "person") {
      
      gender_query <- glue::glue("SELECT gender_concept_id, concept_name, COUNT(*) AS N
                                  FROM {input$cdm_schema}.person 
                                  JOIN {input$vocab_schema}.concept ON person.gender_concept_id = concept.concept_id
                                  GROUP BY gender_concept_id,
                                  concept_name
                                 ")
      
      race_query <- glue::glue("SELECT race_concept_id, concept_name, COUNT(*) AS N
                                FROM {input$cdm_schema}.person 
                                JOIN {input$vocab_schema}.concept ON person.race_concept_id = concept.concept_id
                                GROUP BY race_concept_id,
                                concept_name
                               ")
      
      age_query <- glue::glue("SELECT MIN(EXTRACT(YEAR FROM observation_date) - year_of_birth) AS min_age,
                                  MAX(EXTRACT(YEAR FROM observation_date) - year_of_birth) AS max_age,
                                  ROUND(AVG(EXTRACT(YEAR FROM observation_date) - year_of_birth), 2) AS mean_age
                                  FROM {input$cdm_schema}.person 
                                  JOIN {input$cdm_schema}.observation ON person.person_id = observation.person_id
                              ")
      
      # gender_data <- DBI::dbGetQuery(con, gender_query)
      # race_data  <- DBI::dbGetQuery(con, race_query)
      # age_data   <- DBI::dbGetQuery(con, age_query)
      
      gender_data <- tryCatch({
        DBI::dbGetQuery(con, gender_query)
      }, error = function(e) NULL)
      
      race_data <- tryCatch({
        DBI::dbGetQuery(con, race_query)
      }, error = function(e) NULL)
      
      age_data <- tryCatch({
        DBI::dbGetQuery(con, age_query)
      }, error = function(e) NULL)
      
     
      ## Gender plot
      output$gender_plot <- renderPlotly({
        plot_ly(gender_data
                ,x = ~concept_name
                ,y = ~n
                ,type = 'bar'
                ,name = "Gender")
      })
      
      ## Race plot
      output$race_plot <- renderPlotly({
        plot_ly(race_data
                ,x = ~concept_name
                ,y = ~n, type = 'bar'
                ,name = "Race")
      })
      
      ## Age summary table
      output$age_summary <- renderTable({ age_data })
    }
    
    ## Observation table -----
    else if (input$selected_cdm_table == "observation") {
      
      obs_query <- glue::glue("WITH concept_counts AS (
                              SELECT observation_concept_id,
                              COUNT(DISTINCT person_id) AS n
                              FROM {input$cdm_schema}.observation
                              GROUP BY observation_concept_id
                              )
                              SELECT 
                              cc.observation_concept_id,
                              COALESCE(c.concept_name, MIN(obs.observation_source_value)) AS concept_name,
                              CASE WHEN c.concept_id IS NOT NULL THEN 'Standard' ELSE 'Source' END AS vocabulary_type,
                              cc.n
                              FROM concept_counts cc
                              LEFT JOIN {input$vocab_schema}.concept c ON cc.observation_concept_id = c.concept_id
                              LEFT JOIN {input$cdm_schema}.observation obs ON cc.observation_concept_id = obs.observation_concept_id
                              GROUP BY cc.observation_concept_id, c.concept_name, c.concept_id, cc.n
                              ORDER BY cc.n DESC
                              LIMIT 20
                              ")
      #obs_data <- DBI::dbGetQuery(con, obs_query)
      
      obs_data <- tryCatch({
        DBI::dbGetQuery(con, obs_query)
      }, error = function(e) {
        NULL
      })
      
      output$observation_table <- renderDataTable({
        datatable(obs_data, selection = 'single')
      })
      
      output$observation_value_ui <- renderUI({
        req(input$observation_table_rows_selected)
        selected_id <- obs_data$observation_concept_id[input$observation_table_rows_selected]
        
        val_query <- glue::glue("SELECT COALESCE(vc.concept_name, o.value_as_string) AS value_label,
                                COUNT(*) AS n
                                FROM {input$cdm_schema}.observation o
                                LEFT JOIN {input$vocab_schema}.concept vc ON o.value_as_concept_id = vc.concept_id
                                WHERE o.observation_concept_id = {selected_id}
                                AND (o.value_as_concept_id IS NOT NULL OR o.value_as_string IS NOT NULL)
                                GROUP BY value_label
                                ORDER BY n DESC
                                ")
        
        #val_data <- DBI::dbGetQuery(con, val_query)
        val_data <- tryCatch({
          DBI::dbGetQuery(con, val_query)
        }, error = function(e) {
          NULL
        })
        
        output$value_as_concept_plot <- renderPlotly({
          if (nrow(val_data) > 0) {
            plot_ly(val_data
                    ,x = ~value_label
                    ,y = ~n
                    ,type = 'bar')|>
                    layout(title = "Value Distribution")
          } else {
            plotly_empty(type = "scatter"
                         , mode = "markers")|>
                          layout(title = "No data available")
          }
        })
        plotlyOutput("value_as_concept_plot")
      })
    }
    
    ## Location table ----
    else if (input$selected_cdm_table == "location") {
        
     location_query <- glue::glue("SELECT l.address_1, l.location_source_value, l.country_source_value,
                                  COUNT(p.person_id) AS n_persons
                                  FROM {input$cdm_schema}.location l
                                  JOIN {input$cdm_schema}.person p
                                  ON l.location_id = p.location_id
                                  GROUP BY 
                                  l.address_1,
                                  l.location_source_value,
                                  l.country_source_value
                                  ORDER BY n_persons DESC;
                                  ")
     #loc_data <- DBI::dbGetQuery(con, location_query)
     loc_data<- tryCatch({
       DBI::dbGetQuery(con, location_query)
     }, error = function(e) {
       NULL
     })
    
    output$location_table <- renderDataTable({
      datatable(loc_data, selection = 'single')
    })
    }
    
    ## Care site table ----
    else if (input$selected_cdm_table == "care_site"){
    
      care_query<- glue::glue("SELECT c.care_site_name,
                                c.care_site_source_value,
                                v.concept_name AS place_of_service,
                                l.location_source_value,
                                l.address_1,
                                COUNT (p.person_id) AS n_person
                                FROM {input$cdm_schema}.care_site c
                                JOIN {input$cdm_schema}.location l ON c.location_id = l.location_id
                                JOIN {input$vocab_schema}.concept v ON v.concept_id = c.place_of_service_concept_id JOIN
                                {input$cdm_schema}.person p ON c.location_id = p.location_id
                                GROUP BY c.care_site_name,
                                care_site_source_value,
                                place_of_service,
                                location_source_value,
                                address_1
                              ")
      
      #care_data<- DBI::dbGetQuery(con, care_query)
     
       care_data<- tryCatch({
        DBI::dbGetQuery(con, care_query)
      }, error = function(e) {
        NULL
      })
      
      output$care_table <- renderDataTable({
        datatable(care_data, selection = 'single')
      })
    }
    
    ## Provider table ----
    else if (input$selected_cdm_table == "provider") {
      summary_query <- glue::glue("SELECT COUNT(*) AS n_providers,
                                  COUNT(DISTINCT CASE WHEN specialty_concept_id <> 0 THEN specialty_concept_id END) AS n_specialties,
                                  COUNT(DISTINCT care_site_id) AS n_care_sites_linked,
                                  COUNT(DISTINCT CASE WHEN gender_concept_id <> 0 THEN gender_concept_id END) AS n_genders
                                  FROM {input$cdm_schema}.provider
                                  ")
      
      #summary_data <- DBI::dbGetQuery(con, summary_query)
      
      summary_data<- tryCatch({
        DBI::dbGetQuery(con, summary_query)
      }, error = function(e) {
        NULL
      })
      
      output$summary_table <- renderDataTable({
        datatable(summary_data, options = list(dom = 't'))
      })
    }
    
   
    ## Visit Occurrence table ----
    else if (input$selected_cdm_table == "visit_occurrence") {
              
      # --- Summary table
      visit_query <- glue::glue("SELECT COUNT(*) AS n_visits,
                                  COUNT(DISTINCT person_id) AS n_persons,
                                  COUNT(DISTINCT visit_concept_id) AS n_visit_types,
                                  MIN(visit_start_date) AS first_visit,
                                  MAX(visit_end_date) AS last_visit
                                  FROM {input$cdm_schema}.visit_occurrence
                                  ")
  
    #visit_data <- DBI::dbGetQuery(con, visit_query)
    visit_data<- tryCatch({
      DBI::dbGetQuery(con, visit_query)
    }, error = function(e) {
      NULL
    })
      output$summary_table <- renderDataTable({
        datatable(visit_data, options = list(dom = 't'))
      })
  
  # --- Visits over time
  visit_time_query <- glue::glue("SELECT TO_CHAR(visit_start_date, 'YYYY-MM-DD') AS date,
                                COUNT(person_id) AS n_persons
                                FROM {input$cdm_schema}.visit_occurrence
                                GROUP BY TO_CHAR(visit_start_date, 'YYYY-MM-DD')
                                ORDER BY date;
                                 ")
  
  #visit_time <- DBI::dbGetQuery(con, visit_time_query)
  visit_time<- tryCatch({
    DBI::dbGetQuery(con, visit_time_query)
  }, error = function(e) {
    NULL
  })
  
  
  output$visit_time_plot <- renderPlotly({
    plot_ly(
      data = visit_time,
      x = ~date,
      y = ~n_persons,
      type = 'scatter',
      mode = 'lines+markers'
    ) %>%
      layout(
        title = "Visits Over Time",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Number of persons")
      )
  })
  
  # --- Visits by type (with concept name instead of ID)
  visit_type_query <- glue::glue("SELECT c.concept_name AS visit_type,
                                  COUNT(*) AS n_visits
                                  FROM {input$cdm_schema}.visit_occurrence v
                                  LEFT JOIN {input$cdm_schema}.concept c
                                  ON v.visit_concept_id = c.concept_id
                                  GROUP BY c.concept_name
                                  ORDER BY n_visits DESC
                                 ")
  
  #visit_type <- DBI::dbGetQuery(con, visit_type_query)
  visit_type<- tryCatch({
    DBI::dbGetQuery(con, visit_type_query)
  }, error = function(e) {
    NULL
  })
  
      output$visit_type_plot <- renderPlotly({
        plot_ly(data = visit_type,
          x = ~n_visits,
          y = ~visit_type,
          type = "bar",
          orientation = "h"
        ) %>%
          layout(
            title = "Visits by Type",
            xaxis = list(title = "Number of Visits"),
            yaxis = list(title = "Visit Type")
          )
      })
}

    
    ## Measurement table---- 
  
    else if (input$selected_cdm_table == "measurement") {
    
      meas_query <- glue::glue("WITH concept_counts AS (SELECT measurement_concept_id,
                               COUNT(*) AS n
                                FROM {input$cdm_schema}.measurement
                                GROUP BY measurement_concept_id)
                                SELECT cc.measurement_concept_id,
                                COALESCE(c.concept_name, MIN(meas.measurement_source_value)) AS concept_name,
                                CASE WHEN c.concept_id IS NOT NULL THEN 'Standard' ELSE 'Source' END AS vocabulary_type,
                                cc.n
                                FROM concept_counts cc
                                LEFT JOIN {input$vocab_schema}.concept c 
                                ON cc.measurement_concept_id = c.concept_id
                                LEFT JOIN {input$cdm_schema}.measurement meas 
                                ON cc.measurement_concept_id = meas.measurement_concept_id
                                GROUP BY cc.measurement_concept_id, c.concept_name, c.concept_id, cc.n
                                ORDER BY cc.n DESC
                                LIMIT 20
                               ")
      
      #meas_data <- DBI::dbGetQuery(con, meas_query)
      meas_data<- tryCatch({
        DBI::dbGetQuery(con, meas_query)
      }, error = function(e) {
        NULL
      })
      
      # Main measurement table
      output$measurement_table <- renderDataTable({
        datatable(meas_data, selection = 'single')
      })
      
      # UI placeholders for drill-down (plot + table)
      output$measurement_value_ui <- renderUI({
        req(input$measurement_table_rows_selected)
        
        tagList(
          plotlyOutput("measurement_categorical_plot"),
          dataTableOutput("measurement_numeric_summary")
        )
      })
      
      # Observe row selection and fetch details
        observeEvent(input$measurement_table_rows_selected, {
        selected_id <- meas_data$measurement_concept_id[input$measurement_table_rows_selected]
        
        #Numeric summary
        val_query <- glue::glue("SELECT 'Numeric' AS value_type,
                                NULL AS value_label,
                                COUNT(m.value_as_number) AS n,
                                MIN(m.value_as_number) AS min_value,
                                MAX(m.value_as_number) AS max_value,
                                AVG(m.value_as_number) AS avg_value
                                FROM {input$cdm_schema}.measurement m
                                WHERE m.measurement_concept_id = {selected_id}
                                AND m.value_as_number IS NOT NULL

                                UNION ALL
                          
                                -- Categorical breakdown
                                SELECT
                                'Categorical' AS value_type,
                                COALESCE(vc.concept_name, 'Unknown') AS value_label,
                                COUNT(*) AS n,
                                NULL AS min_value,
                                NULL AS max_value,
                                NULL AS avg_value
                                FROM {input$cdm_schema}.measurement m
                                LEFT JOIN {input$vocab_schema}.concept vc 
                                ON m.value_as_concept_id = vc.concept_id
                                WHERE m.measurement_concept_id = {selected_id}
                                AND m.value_as_concept_id IS NOT NULL
                                GROUP BY vc.concept_name
                                ORDER BY value_type, n DESC
                                ")
        
       # val_data <- DBI::dbGetQuery(con, val_query)
        val_data<- tryCatch({
          DBI::dbGetQuery(con, val_query)
        }, error = function(e) {
          NULL
        })
        
        # Categorical plot
        output$measurement_categorical_plot <- renderPlotly({
          if ("Categorical" %in% val_data$value_type) {
            plot_ly(
              val_data[val_data$value_type == "Categorical", ],
              x = ~value_label
              ,y = ~n
              ,type = 'bar')|>
              layout(title = "Categorical Value Distribution")
          } else {
            plotly_empty(type = "scatter", mode = "markers") |>
              layout(title = "No categorical data")
          }
        })
        
        # Numeric summary table
        output$measurement_numeric_summary <- renderDataTable({
          if ("Numeric" %in% val_data$value_type) {
            val_data[val_data$value_type == "Numeric",
                     c("min_value", "max_value", "avg_value"), drop = FALSE] |>
              transform(
                min_value = round(min_value, 2),
                max_value = round(max_value, 2),
                avg_value = round(avg_value, 2)
              )
          } else {
            data.frame(Note = "No numeric values available")
          }
        })
      })
    }
    
    ## Condition_Occurrence table-----
    else if (input$selected_cdm_table == "condition_occurrence") {
     
      gender_sql <- ""
      if (!is.null(input$gender_filter) && input$gender_filter != "All") {
        gender_sql <- glue::glue("AND p.gender_concept_id = 
                                CASE 
                                  WHEN '{input$gender_filter}' = 'Male' THEN 8507
                                  WHEN '{input$gender_filter}' = 'Female' THEN 8532
                                END
                                 ")
      }
      
      # Main condition concept query with gender filter
      con_query <- glue::glue("WITH concept_counts AS (
                              SELECT condition_concept_id, COUNT(*) AS n
                              FROM {input$cdm_schema}.condition_occurrence con
                              JOIN {input$cdm_schema}.person p ON con.person_id = p.person_id
                              WHERE 1=1
                              {gender_sql}
                              GROUP BY condition_concept_id)
                              SELECT 
                              cc.condition_concept_id,
                              COALESCE(c.concept_name, MIN(con.condition_source_value)) AS concept_name,
                              CASE WHEN c.concept_id IS NOT NULL THEN 'Standard' ELSE 'Source' END AS vocabulary_type,
                              cc.n
                              FROM concept_counts cc
                              LEFT JOIN {input$vocab_schema}.concept c ON cc.condition_concept_id = c.concept_id
                              LEFT JOIN {input$cdm_schema}.condition_occurrence con ON cc.condition_concept_id = con.condition_concept_id
                              GROUP BY cc.condition_concept_id, c.concept_name, c.concept_id, cc.n
                              ORDER BY cc.n DESC
                              LIMIT 20
                              ")
      
     # con_data <- DBI::dbGetQuery(con, con_query)
      con_data<- tryCatch({
        DBI::dbGetQuery(con, con_query)
      }, error = function(e) {
        NULL
      })
      
      output$condition_table <- renderDataTable({
        datatable(con_data, selection = 'single')
      })
      
      # Value-as-concept distribution, also with gender filter
      output$condition_value_ui <- renderUI({
        req(input$condition_table_rows_selected)
        selected_id <- con_data$condition_concept_id[input$condition_table_rows_selected]
        
        val_query <- glue::glue("SELECT COALESCE(vc.concept_name, con.condition_source_value) AS value_label,
                                COUNT(*) AS n
                                FROM {input$cdm_schema}.condition_occurrence con
                                JOIN {input$cdm_schema}.person p ON con.person_id = p.person_id
                                LEFT JOIN {input$vocab_schema}.concept vc 
                                ON con.condition_type_concept_id = vc.concept_id
                                WHERE con.condition_concept_id = {selected_id}
                                {gender_sql}
                                AND (con.condition_type_concept_id IS NOT NULL OR con.condition_source_value IS NOT NULL)
                                GROUP BY value_label
                                ORDER BY n DESC
                                ")
        #val_data <- DBI::dbGetQuery(con, val_query)
        val_data<- tryCatch({
          DBI::dbGetQuery(con, val_query)
        }, error = function(e) {
          NULL
        })
        
        output$value_as_concept_plot <- renderPlotly({
          if (nrow(val_data) > 0) {
            plot_ly(val_data, x = ~value_label, y = ~n, type = 'bar') |>
              layout(title = "Value Distribution")
          } else {
            plotly_empty(type = "scatter", mode = "markers") |>
              layout(title = "No data available")
          }
        })
        
        plotlyOutput("value_as_concept_plot")
      })
    }
    
  })
  
  session$onSessionEnded(function() {
    if (!is.null(isolate(omop_conn$conn))) {
      DatabaseConnector::disconnect(isolate(omop_conn$conn))
    }
  })
}
