#### ---- Study name ---------------------------------------#####

study_name = renderUI({
	textInput("study_name", get_rv_labels("study_name"), width = "50%")
})

#### ---- Study country ---------------------------------------#####
study_country = renderUI({
	selectInput("study_country", get_rv_labels("study_country"), choices = countries::list_countries(), multiple = TRUE)
})

#### ---- Additional info ---------------------------------------#####
additional_info = renderUI({
	textAreaInput("additional_info", get_rv_labels("additional_info"), placeholder = get_rv_labels("additional_info_ph"), width = "50%")
})

## #### ----- Submit upload ----------------------------------------####
## submit_upload = renderUI({
## 	actionBttn("submit_upload", get_rv_labels("submit_upload"), width = "25%"
## 		, inline=TRUE
## 		, block = FALSE
## 		, color = "success"
## 	)
## })



#### ----- Submit upload ----------------------------------------####
submit_upload <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) {
    actionBttn("submit_upload", get_rv_labels("submit_upload"),
               width = "25%" 
               , inline = TRUE 
               , block = FALSE 
               , color = "success" 
    ) 
  } else if (isTRUE(any(input$upload_type %in% c("Local", "URL")))) { 
    actionBttn("submit_upload", get_rv_labels("submit_upload"),
               width = "25%" 
               , inline = TRUE 
               , block = FALSE 
               , color = "success" 
    ) 
  } else { 
    NULL 
  } 
})

### --- Additional form details for database section --- ###

#### ----- Connect database ----------------------------------------####
db_connect <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) { 
    if(is.null(rv_database$conn)){
      actionBttn("db_connect",
                 label = get_rv_labels("db_connect"), width = "25%" 
                 , inline = TRUE 
                 , block = FALSE 
                 , color = "success" 
      )
    }  
    
    
  } else { 
    NULL 
  } 
})


### ----------OMOP Schema Views---------------------------------------####
db_schema_list <- renderUI({
  if (!is.null(input$db_type) &&  isTRUE(input$upload_type == "Database connection") && input$db_type == "PostgreSQL") { #U1
    if(!is.null(rv_database$conn) && !is.null(input$option_picked) &&  input$option_picked == "use a table"){
      selectInput("db_schema_list", get_rv_labels("db_schema_list"), choices = NULL, multiple = FALSE)
    }
  }
  else {#U2
    NULL
  }
})
#U for checking
observe({
  if (!is.null(rv_database$table_list)) {
    message(">>> Tables availables : ", paste(rv_database$table_list, collapse = ", "))
  }
})


### ----------OMOP Table Views---------------------------------------####
db_table_list <- renderUI({
  if (!is.null(rv_database$conn) && isTRUE(input$upload_type == "Database connection")) {
    if (!is.null(input$option_picked) && input$option_picked == "use a table") {
      
      # Maj Force to Convert into vector
      choices <- rv_database$table_list
      if (is.data.frame(choices)) {
        choices <- choices[[1]]
      }
      
      if (!is.null(choices) && length(choices) > 0) {
        # Maj Label : Fix get_rv_labels()
        label_raw <- get_rv_labels("db_table_list")
        label_txt <- if (is.null(label_raw)) {
          "Choose a table"
        } else if (length(label_raw) > 1) {
          paste(label_raw, collapse = " ")  # concatenation if there contains several text
        } else {
          as.character(label_raw)
        }
        
        return(selectInput("db_table_list", label = label_txt, choices = choices, multiple = FALSE))
      }
    }
  }
  return(NULL)
})



### ----------OMOP database type---------------------------------------####

db_type <- renderUI({
    if (isTRUE(input$upload_type == "Database connection")) {
      selectInput("db_type", get_rv_labels("db_type"), choices = c("PostgreSQL","MySQL"), selected = "PostgreSQL" , multiple = FALSE)
    }
  })

### --- Database host ---###
db_host <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) {
    if(is.null(rv_database$conn)){
      textInput("db_host",
                label = get_rv_labels("db_host")
                , placeholder = get_rv_labels("db_host_placeholder")
                , width = "50%"
      )
    }
  } else {
    NULL
  }
})

#### ---- Database name ---------------------------------------#####
db_name <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) {
    if(is.null(rv_database$conn)){  
      textInput("db_name",
                label = get_rv_labels("db_name")
                , placeholder = get_rv_labels("db_name_placeholder")
                , width = "50%"
      )
    }
  } else {
    NULL
  }
})

#### ---- Database user ---------------------------------------#####
db_user <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) {
    if(is.null(rv_database$conn)){  
      textInput("db_user",
                label = get_rv_labels("db_user")
                , placeholder = get_rv_labels("db_user_placeholder")
                , width = "50%"
      )
    }
  } else {
    NULL
  }
})

#### ---- Database password ---------------------------------------#####
db_pwd <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) {
    if(is.null(rv_database$conn)){  
       maskedPasswordInput("db_pwd",
                    label = get_rv_labels("db_pwd")
                    , placeholder = get_rv_labels("db_pwd_placeholder"),
                    width="50%"
                    
      )
    }
  } else {
    NULL
  }
})


#### ---- Database port ---------------------------------------#####
db_port <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) {
    if(is.null(rv_database$conn)){
      textInput("db_port",
                label = get_rv_labels("db_port")
                , placeholder = get_rv_labels("db_port_placeholder")
                , width = "50%"
                , value ="5432"
      )
    }
  } else {
    NULL
  }
})


### ---------- Custom Query ---------------------------------------####

db_custom_query <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) { 
    if(!is.null(rv_database$conn) && !is.null(input$option_picked) && input$option_picked == "use SQL query"){
      textAreaInput("db_custom_query", get_rv_labels("db_custom_query"), placeholder = get_rv_labels("db_write_query_placeholder"), width = "50%")
    }
    
  }
})


db_run_query<- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) { 
    if(!is.null(rv_database$conn) && !is.null(input$option_picked) && input$option_picked == "use SQL query"){
      actionBttn("db_run_query",
                 label = get_rv_labels("db_run_query"), width = "25%" 
                 , inline = TRUE 
                 , block = FALSE 
                 , color = "success" 
      ) 
    }
    
  } else { 
    NULL 
  } 
})


#### ----- Disconnect database ----------------------------------------####
db_disconnect <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) { 
    if(!is.null(rv_database$conn)){
      actionBttn("db_disconnect",
                 label = get_rv_labels("db_disconnect"), width = "25%" 
                 , inline = TRUE 
                 , block = FALSE 
                 , color = "success" 
      )
      
    }
    
  } else { 
    NULL 
  } 
})

db_tab_query <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) { 
    if(!is.null(rv_database$conn)){
      radioButtons("option_picked", get_rv_labels("db_tab_query"), 
                   choices = get_named_choices(input_choices_file, input$change_language, "ui_radio_button"),
                   selected = get_named_choices(input_choices_file, input$change_language, "ui_radio_button")[1])
                   
      
    }
    
  }else { 
    NULL 
  }
})


# -- New database connection

output$omop_connection <- renderUI({
  radioButtons(
    inputId = "omop_db",
    label = "Database Connect:",
    choices = c("New Connection", "Existing Connection"),
    selected = "Existing Connection"
  )
})





existing_connection <- renderUI({
  if (!is.null(rv_database$conn) && isTRUE(input$omop_db == "Existing Connection")) {
      selectInput("existed_conn", "Use Connection", choices = "Postgres Connection", multiple = FALSE)
  
  }
})
  


output$omop_quality_type <- renderUI({
  if (isTRUE(input$omop_db == "Existing Connection")) {
    if(!is.null(rv_database$conn)){
      radioButtons(
        inputId = "omop_quality",
        label = "",
        choices = c("Data Quality Dashboard", "ACHILLES"),
        selected = "Data Quality Dashboard"
      )
    }else{
      shinyalert("", "Please create a connection using Source data page", type = "info")
    }
    
  }else{
    shinyalert("", "Please create a connection using Source data page", type = "info")
  }
  
})


output$schemas <- renderUI({
  if (!is.null(rv_database$conn) && input$omop_quality == "Data Quality Dashboard") {
    tagList(
      selectInput("cdm_schema", "CDM schema", choices = "demo_cdm", multiple = FALSE),
      selectInput("results_schema", "Results schema", choices = "demo_cdm_results", multiple = FALSE),
      textInput("cdmSourceName", "CDM Source Name", placeholder = "NO-CODE CDM", width = "50%")
    )
  } else {
    NULL
  }
})
  
  
output$generate_dqd <- renderUI({
  if (isTRUE(input$omop_quality == "Data Quality Dashboard")) {
    
    if(isTRUE(!is.null(rv_database$conn))){
      
      actionBttn("generate_dqd",
                 label = "Generate DQD", width = "25%" 
                 , inline = TRUE 
                 , block = FALSE 
                 , color = "success" 
      )
      
    } else {  NULL }
    
  }else{ NULL }
    
  
})


output$view_dqd <- renderUI({
  if (isTRUE(input$omop_quality == "Data Quality Dashboard")) {
    
    if(isTRUE(!is.null(rv_database$conn))){
      
      actionBttn("view_dqd",
                 label = "Run DQD", width = "25%" 
                 , inline = TRUE 
                 , block = FALSE 
                 , color = "success" 
      )
      
    } else {  NULL }
    
  }else{ NULL }
  
  
})


output$stderr_log <- renderText({
  
  if (isTRUE(input$omop_quality == "Data Quality Dashboard")) {
    
    if(isTRUE(!is.null(rv_database$conn))){
      stderr_content()
    }

}
  


})


output$open_link <- renderUI({
  
  if (isTRUE(input$omop_quality == "Data Quality Dashboard")) {
    
    if(isTRUE(!is.null(rv_database$conn))){
      url <- rv_omop$url  # Assuming this is a reactive value
      
      # Make sure it's a non-empty character string
      if (!is.null(url) && is.character(url) && grepl("^http", url)) {
        tags$a(href = url, "View Dashboard", target = "_blank")
      } else {
        tags$span("No valid URL yet.")
      }
    }else {
  
}
  }else{}
  
})

