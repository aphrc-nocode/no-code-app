### Possubmit_uploadtgreSQL connection logic
database_integration_server <- function(){

observeEvent(input$db_connect, {
  database_host <- input$db_host 
  database_name <- input$db_name
  database_user <- input$db_user
  database_pass <- input$db_pwd
  db_port <- "5432" ## FIXME: Transfer to UI
  drv <- RPostgres::Postgres()
  
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
      shinyalert("", "database connection failed", type = "error")
      
    })
    
  }else{
    shinyalert("", "MySQL database integration in progress", type = "info")
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
  query_table_data <-  paste("SELECT * FROM ",schema_selected,".",table_selected, ";", sep = "")
  df_table <- dbGetQuery(conn, query_table_data)
  rv_database$df_table <- df_table
})


output$db_table_view<- renderDT({
  df_table <- rv_database$df_table
  
  datatable(
    df_table,
    options = list(pageLength =10)
  )
})

}
