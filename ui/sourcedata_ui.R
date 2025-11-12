
# Helper function to create a collapsible section
collapsible_panel <- function(title, ..., open = FALSE) {
  tags$details(
    open = if (open) NA else NULL,
    tags$summary(title, style = "font-weight: bold; cursor: pointer; margin-top: 15px;"),
    div(style = "padding: 10px; border: 1px solid #ddd; border-radius: 5px; margin-top: 5px;", ...
    )
  )
}


## Source data tab
sourcedata_ui = function() {
	tabItem(tabName = "sourcedata",
		  
	 fluidRow(class  = "aphrc-row",
			 column(
				width = 3
				#Upload data types
				, uiOutput("upload_type")),
			 column(
				width = 3,
				uiOutput("show_uploaded")
			 )
			 
		  )
		  , hr()
		  , fluidRow(
			 div(id = "upload_form"
				  , uiOutput("study_name")
				  , uiOutput("study_country")
				  , uiOutput("additional_info")
				  , uiOutput("input_files")
				  
				  , uiOutput("db_type")
				  , uiOutput("db_host")
				  , uiOutput("db_name")
				  , uiOutput("db_user")
				  , uiOutput("db_pwd")
				  , uiOutput("db_port")
				  , uiOutput("db_connect")
				  , uiOutput("db_disconnect")
				  , hr()
				  , uiOutput("db_tab_query")
				  , uiOutput("db_schema_list")
				  , uiOutput("db_table_list")
				  , conditionalPanel(
					 condition = "input.upload_type == 'Database connection'",
					 verbatimTextOutput("db_table_str")
				  )
				  , br()
				  , uiOutput("db_custom_query")
				  , uiOutput("db_run_query")
				  , conditionalPanel(
					 condition = "input.upload_type == 'Database connection'",
					 DT::DTOutput("db_table_view", width = "100%")
				  ) 
				  
				  
				  ## This is my thing
				  , uiOutput("submit_upload")
			 )
			 , br()
			 , uiOutput("upload_info")
			 , hr()
			 , div(id  = "aphrc-row1",
				style = "margin-top: 10px;"
				, DT::DTOutput("upload_logs", width = "100%")
			 )
		  )
	)
}

