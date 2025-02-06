### Possubmit_uploadtgreSQL connection logic
database_integration_server <- function(){
  
  observeEvent(input$db_connect, {
    database_host <- input$db_host 
    database_name <- input$db_name
    database_user <- input$db_user
    database_pass <- input$db_pwd
    #db_port <- "5432" ## FIXME: Transfer to UI
    db_port <- input$db_port
    #drv <- RPostgres::Postgres()
    drv <- dbDriver("PostgreSQL")
    
    if(input$db_type == "PostgreSQL"){
      tryCatch({
        conn <- dbConnect(drv, 
                          dbname = database_name,
                          host = database_host, 
                          port = db_port,
                          user = database_user, 
                          password = database_pass)
        
        rv_database$conn <- conn
        shinyalert("", get_rv_labels("db_connect_success"), type = "success")
        query_schemas <-"SELECT schema_name FROM information_schema.schemata WHERE schema_name NOT IN ('pg_catalog', 'information_schema') AND schema_name NOT LIKE 'pg_%' "
        schemas <- dbGetQuery(conn, query_schemas)
        schema_list <- c(schemas)
        rv_database$schema_list <- schema_list
        updateSelectInput(session,inputId = "db_schema_list", choices = rv_database$schema_list,selected = rv_database$schema_list[1] )
      }, 
      error = function(e){
        shinyalert("", "database connection failed, Confirm Host,Username and Password are correct", type = "error")
        
      })
      
    }else{
      shinyalert("", "MySQL database integration not yet implemented", type = "info")
    }
    
    
  })
  
  observeEvent(input$option_picked, {
    if(input$option_picked == "use a table"){
      updateSelectInput(session,inputId = "db_schema_list", choices = rv_database$schema_list,selected = rv_database$schema_list[1] )
    }
    
  })  
  
  
  observeEvent(input$db_schema_list, {
    req(rv_database$conn, input$db_schema_list)
    schema_selected <- input$db_schema_list
    rv_database$schema_selected <- schema_selected
    query_tables <- paste("SELECT table_name FROM information_schema.tables WHERE table_schema = '", schema_selected, "';", sep = "")
    conn <- rv_database$conn
    tables <- dbGetQuery(conn, query_tables)
    table_list <-c(tables)
    rv_database$table_list <- table_list
    updateSelectInput(session,inputId = "db_table_list", choices = rv_database$table_list,selected = rv_database$table_list[1] )
    
  })
  
  observeEvent(input$db_table_list,{
    req(rv_database$conn, input$db_table_list)
    table_selected <- input$db_table_list
    rv_database$table_selected <- table_selected
    schema_selected <- rv_database$schema_selected
    conn <- rv_database$conn
    query_table_data <-  paste("SELECT * FROM ",schema_selected,".",table_selected, " ;", sep = "")
    df_table_str <- dbGetQuery(conn, query_table_data)
    rv_database$df_table_str <- df_table_str
    


  })
  
  output$db_table_str <- renderPrint({ 
    if(!is.null(rv_database$df_table_str)){
      if(input$option_picked == "use a table"){
        str(rv_database$df_table_str)

      }else{
        rv_database$df_table_str = NULL
      }
      
    }
    
  })
  
  
  output$db_table_view<- renderDT({
    df_table <- rv_database$df_table
    if(input$option_picked == "use SQL query"){
      datatable(
        head(df_table,5),
        options = list(pageLength =10)
      )
    }else{
      rv_database$df_table = NULL
    }

    
    
    
  })
  
  observeEvent(input$db_run_query, {
    if (is.null(rv_database$conn)) {
      shinyalert(
        title = "",
        text = "Please establish a database connection before running a query.",
        type = "error"
      )
      return()
    }
    
    result <- tryCatch({
      
      query_string <- input$db_custom_query
      table_name <- sub("(?i).*\\bfrom\\s+([\\w.]+).*", "\\1", query_string, perl = TRUE)
      query_table_name <- gsub("\\.","_",table_name)
      conn <- rv_database$conn
      df_table <- dbGetQuery(conn, query_string)
      rv_database$query_table_name <- query_table_name
      rv_database$df_table <- df_table
      df_table
    },
    error = function(e) {
      df_table <- NULL
      query_table_name <- NULL
      
    })
    
    if(is.null(result)){
      shinyalert(
        title = "",
        text = "Check your SQL Syntax.",
        type = "error"
      )
    }
    
  })
  
  observeEvent(input$db_disconnect, {
    dbDisconnect(rv_database$conn)
    rv_database$conn = NULL
    rv_database$schema_list = NULL
    rv_database$table_list = NULL
    rv_database$df_table = NULL
    rv_database$df_table_str = NULL
  })
  
}
