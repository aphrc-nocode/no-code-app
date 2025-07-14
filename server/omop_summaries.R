omopVizServer <- function() {
  omop_conn <- reactiveValues(
    details = NULL,
    conn = NULL,
    schemas = NULL,
    cdm = NULL
  )
  
  observeEvent(input$omop_db_connect, {
    req(
      input$omop_dbms == "postgresql",
      input$omop_db_host,
      input$omop_db_port,
      input$omop_db_name,
      input$omop_db_user,
      input$omop_db_pwd
    )
    
    tryCatch({
      omop_conn$details <- DatabaseConnector::createConnectionDetails(
        dbms = input$omop_dbms,
        server = paste0(input$omop_db_host, "/", input$omop_db_name),
        port = input$omop_db_port,
        user = input$omop_db_user,
        password = input$omop_db_pwd,
        pathToDriver = "./static_files"
      )
      
      omop_conn$conn <- DatabaseConnector::connect(omop_conn$details)
      
      schema_query <- "
        SELECT schema_name 
        FROM information_schema.schemata 
        WHERE schema_name NOT LIKE 'pg_%' AND schema_name <> 'information_schema'
      "
      omop_conn$schemas <- DatabaseConnector::querySql(omop_conn$conn, schema_query)$SCHEMA_NAME
      
      shinyalert("Success", "Connected successfully!", type = "success")
      
      output$omop_cdm_schema <- renderUI({
        selectInput("cdm_schema", "CDM Schema", choices = omop_conn$schemas)
      })
      
      output$omop_results_schema <- renderUI({
        selectInput("results_schema", "Results Schema", choices = omop_conn$schemas)
      })
      
      output$omop_vocabulary_schema <- renderUI({
        selectInput("vocab_schema", "Vocabulary Schema", choices = omop_conn$schemas)
      })
      
    }, error = function(e) {
      shinyalert("Connection Error", e$message, type = "error")
    })
  })
  
  observeEvent(input$generate_summary, {
    req(omop_conn$conn, input$cdm_schema, input$results_schema)
    
    shinybusy::show_modal_spinner(text = "Generating summaries...")
    
    tryCatch({
      omop_conn$cdm <- CDMConnector::cdmFromCon(
        con = omop_conn$conn,
        cdmSchema = input$cdm_schema,
        cdmVersion = "5.4",
        writeSchema = input$results_schema
      )
      
      snap <- OmopSketch::summariseOmopSnapshot(omop_conn$cdm)
      summary_table <- OmopSketch::tableOmopSnapshot(snap, type = "gt")
      
      
      output$omop_snapshot_summary <- gt::render_gt({
        summary_table})
        
      tables_query <- glue::glue("
        SELECT table_name 
        FROM information_schema.tables 
        WHERE table_schema = '{input$cdm_schema}'
      ")
      table_list <- DatabaseConnector::querySql(omop_conn$conn, tables_query)$TABLE_NAME
      
      tbl_counts <- purrr::map_dfr(table_list, function(tbl) {
        count_query <- glue::glue("SELECT COUNT(*) AS COUNT FROM {input$cdm_schema}.\"{tbl}\"")
        tryCatch({
          res <- DatabaseConnector::querySql(omop_conn$conn, count_query)
          tibble::tibble(Table = tbl, Records = res$COUNT[1])
        }, error = function(e) {
          tibble::tibble(Table = tbl, Records = NA_integer_)
        })
      })
      
      output$cdm_table_summaries <- DT::renderDataTable({
        DT::datatable(
          tbl_counts, 
          options = list(pageLength = 10, order = list(list(1, 'desc'))), 
          rownames = FALSE
        )
      })
      
    }, error = function(e) {
      shinyalert("Visualization Error", e$message, type = "error")
    }, finally = {
      shinybusy::remove_modal_spinner()
    })
  })
  
  observeEvent(input$cdm_schema, {
    req(omop_conn$conn, input$cdm_schema)
    
    tryCatch({
      tables_query <- glue::glue("
        SELECT table_name 
        FROM information_schema.tables 
        WHERE table_schema = '{input$cdm_schema}'
      ")
      table_names <- DatabaseConnector::querySql(omop_conn$conn, tables_query)$TABLE_NAME
      
      output$omop_cdm_tables <- renderUI({
        selectInput("selected_cdm_table", "Select a CDM Table", choices = table_names)
      })
      
    }, error = function(e) {
      shinyalert("Table Load Error", e$message, type = "error")
    })
  })
  
  observe({
    req(omop_conn$cdm, input$selected_cdm_table, input$cdm_schema,input$vocab_schema)
    
    con <- attr(omop_conn$cdm, "dbcon")
    
    if (input$selected_cdm_table == "person") {
      # Gender
      gender_query <- glue::glue("
        SELECT gender_concept_id, concept_name, COUNT(*) AS N
        FROM {input$cdm_schema}.person 
        JOIN {input$vocab_schema}.concept ON person.gender_concept_id = concept.concept_id
        GROUP BY gender_concept_id, concept_name
      ")
      
      # Race
      race_query <- glue::glue("
        SELECT race_concept_id, concept_name, COUNT(*) AS N
        FROM {input$cdm_schema}.person 
        JOIN {input$vocab_schema}.concept ON person.race_concept_id = concept.concept_id
        GROUP BY race_concept_id, concept_name
      ")
      
      # Age
      age_query <- glue::glue("
        SELECT 
          MIN(EXTRACT(YEAR FROM observation_date) - year_of_birth) AS min_age,
          MAX(EXTRACT(YEAR FROM observation_date) - year_of_birth) AS max_age,
          ROUND(AVG(EXTRACT(YEAR FROM observation_date) - year_of_birth), 2) AS mean_age
        FROM {input$cdm_schema}.person 
        JOIN {input$cdm_schema}.observation ON person.person_id = observation.person_id
      ")
      
      gender_data <- DBI::dbGetQuery(con, gender_query) |> dplyr::rename(N = n)
      race_data  <- DBI::dbGetQuery(con, race_query)  |> dplyr::rename(N = n)
      age_data   <- DBI::dbGetQuery(con, age_query)
      
      output$gender_plot <- renderPlotly({
        plot_ly(gender_data, x = ~concept_name, y = ~N, type = 'bar', name = "Gender")
      })
      
      output$race_plot <- renderPlotly({
        plot_ly(race_data, x = ~concept_name, y = ~N, type = 'bar', name = "Race")
      })
      
      output$age_summary <- renderTable({
        age_data
      })
    }
    
    else if (input$selected_cdm_table == "observation") {
      obs_query <- glue::glue("
        WITH concept_counts AS (
          SELECT observation_concept_id,
          COUNT(*) AS n
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
      
      obs_data <- DBI::dbGetQuery(con, obs_query)
      
      output$observation_table <- renderDataTable({
        datatable(obs_data, selection = 'single')
      })
      
      output$observation_value_ui <- renderUI({
        req(input$observation_table_rows_selected)
        selected_id <- obs_data$observation_concept_id[input$observation_table_rows_selected]
        
        val_query <- glue::glue("
          SELECT 
            COALESCE(vc.concept_name, o.value_as_string) AS value_label,
            COUNT(*) AS n
          FROM {input$cdm_schema}.observation o
          LEFT JOIN {input$vocab_schema}.concept vc ON o.value_as_concept_id = vc.concept_id
          WHERE o.observation_concept_id = {selected_id}
            AND (o.value_as_concept_id IS NOT NULL OR o.value_as_string IS NOT NULL)
          GROUP BY value_label
          ORDER BY n DESC
        ")
        
        val_data <- DBI::dbGetQuery(con, val_query)
        
        output$value_as_concept_plot <- renderPlotly({
          if (nrow(val_data) > 0) {
            plot_ly(val_data, x = ~value_label, y = ~n, type = 'bar') |> layout(title = "Value Distribution")
          } else {
            plotly_empty(type = "scatter", mode = "markers") |> layout(title = "No data available")
          }
        })
        
        plotlyOutput("value_as_concept_plot")
      })
    }
    
    else if (input$selected_cdm_table == "condition_occurrence") {
      con_query <- glue::glue("
        WITH concept_counts AS (
          SELECT condition_concept_id, COUNT(*) AS n
          FROM {input$cdm_schema}.condition_occurrence
          GROUP BY condition_concept_id
        )
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
      
      con_data <- DBI::dbGetQuery(con, con_query)
      
      output$condition_table <- renderDataTable({
        datatable(con_data, selection = 'single')
      })
      
      output$condition_value_ui <- renderUI({
        req(input$condition_table_rows_selected)
        selected_id <- con_data$condition_concept_id[input$condition_table_rows_selected]
        
        val_query <- glue::glue("
             SELECT 
          COALESCE(vc.concept_name, con.condition_source_value) AS value_label,
          COUNT(*) AS n
        FROM {input$cdm_schema}.condition_occurrence con
        LEFT JOIN {input$vocab_schema}.concept vc 
          ON con.condition_type_concept_id = vc.concept_id
        WHERE con.condition_concept_id = {selected_id}
          AND (con.condition_type_concept_id IS NOT NULL OR con.condition_source_value IS NOT NULL)
        GROUP BY value_label
        ORDER BY n DESC"
                                )
        
        val_data <- DBI::dbGetQuery(con, val_query)
        
        output$value_as_concept_plot <- renderPlotly({
          if (nrow(val_data) > 0) {
            plot_ly(val_data, x = ~value_label, y = ~n, type = 'bar') |> layout(title = "Value Distribution")
          } else {
            plotly_empty(type = "scatter", mode = "markers") |> layout(title = "No data available")
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
