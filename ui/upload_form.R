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

#### ----- Connect database ----------------------------------------####
db_connect <- renderUI({
    if (isTRUE(input$upload_type == "Database connection")) { 
        actionBttn("db_connect",
            label = "Connect", width = "25%" 
            , inline = TRUE 
            , block = FALSE 
            , color = "success" 
        ) 
    } else { 
        NULL 
    } 
})


### ----------OMOP Schema Views---------------------------------------####
db_schema_list <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) {
    selectInput("db_schema_list", "List of Schemas", choices = NULL, multiple = FALSE)
  }
})


### ----------OMOP Table Views---------------------------------------####
db_table_list <- renderUI({
    if (isTRUE(input$upload_type == "Database connection")) {
        selectInput("db_table_list", "List of Tables", choices = NULL, multiple = FALSE)
    }
})

### --- Additional form details for database section --- ###

### ----------OMOP Schema Views---------------------------------------####
db_type <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) {
    selectInput("db_type", "Database Type", choices = c("PostgreSQL","MySQL"), selected = "PostgreSQL" , multiple = FALSE)
  }
})

### --- Database host ---###
db_host <- renderUI({
  if (isTRUE(input$upload_type == "Database connection")) {
    textInput("db_host",
              label = "Database Host"
              , placeholder = "Enter your database host/IP address"
              , width = "50%"
    )
  } else {
    NULL
  }
})

#### ---- Database name ---------------------------------------#####
db_name <- renderUI({
    if (isTRUE(input$upload_type == "Database connection")) {
        textInput("db_name",
            label = "Database name"
            , placeholder = "Enter your database name"
            , width = "50%"
        )
    } else {
        NULL
    }
})

#### ---- Database user ---------------------------------------#####
db_user <- renderUI({
    if (isTRUE(input$upload_type == "Database connection")) {
        textInput("db_user",
            label = "Database User"
            , placeholder = "Enter your database User"
            , width = "50%"
        )
    } else {
        NULL
    }
})

#### ---- Database password ---------------------------------------#####
db_pwd <- renderUI({
    if (isTRUE(input$upload_type == "Database connection")) {
        passwordInput("db_pwd",
            label = "Database password"
            , placeholder = "Enter your database password"
            , width = "50%"
        )
    } else {
        NULL
    }
})
