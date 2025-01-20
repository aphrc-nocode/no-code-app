library(shiny)
library(shinyjs)
library(shinyvalidate)
library(shinyalert)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

library(sjlabelled)
library(dplyr)
library(stringr)
library(stringi)

library(readr)
library(readxl)
library(openxlsx)
library(haven)
library(forcats)

library(skimr)
library(summarytools); st_options(footnote=NA, headings = FALSE)

library(countries)

library(plotly)
library(ggplot2)
library(DBI)
library(RPostgreSQL)
library(DT)
library(bslib)


source(paste0(getwd(), "/ui/headertag.R"))
source(paste0(getwd(), "/ui/appTheme.R"))
source(paste0(getwd(), "/ui/header.R"))
source(paste0(getwd(), "/ui/footer.R"))

#Sidebar
aphrcSiderbar <- dashboardSidebar(
  width = "20%",
  
  sidebarMenu(
    menuItem(text = HTML("<span class='menu-label'> Home</span>"), tabName = "homePage", icon = icon("house")),
    menuItem(text = HTML("<span class='menu-label'> Source data</span>"), tabName = "sourcedata", icon = icon("file-import",lib="font-awesome")),
    
    
    menuItem(HTML("<span class='menu-label'> Manage data</span>"), tabName = "manageData", icon = icon("glyphicon glyphicon-tasks",lib="glyphicon"),
             menuSubItem(text = HTML("<span class='menu-label'> Overview</span>"), tabName = "Overview",icon = icon("table-columns",lib="font-awesome"),selected = TRUE),
             menuSubItem(text =  HTML("<span class='menu-label'> Explore</span>"), tabName = "Explore", icon = icon("object-ungroup",lib="font-awesome")),
             menuSubItem(text =  HTML("<span class='menu-label'> Transform</span>"), tabName = "Transform", icon = icon("table-columns",llib="font-awesome"))),
    
    
    menuItem( HTML("<span class='menu-label'> Visualize data</span>"), tabName = "visualizeData", icon = icon("glyphicon glyphicon-stats", lib="glyphicon"),
              menuSubItem(text =  HTML("<span class='menu-label'> Summarize categorical</span>"), tabName = "summarizeCategorical",icon = icon("glyphicon glyphicon-stats",lib="glyphicon"),selected = TRUE),
              menuSubItem(text =  HTML("<span class='menu-label'> Summarize numerical</span>"), tabName = "summarizeNumerical", icon = icon("glyphicon glyphicon-stats",lib="glyphicon"))
              
    ),
    
    menuItem( HTML("<span class='menu-label'> Machine learning</span>"), tabName = "machineLearning", icon = icon("code-merge",lib="font-awesome"),
              menuSubItem(text = HTML("<span class='menu-label'> Data partitioning</span>"), tabName = "dataPartitioning",icon = icon("arrows-split-up-and-left", lib="font-awesome"),selected = TRUE),
              menuSubItem(text = HTML("<span class='menu-label'> Feature Engineering</span>"), tabName = "featureEngineering", icon = icon("sitemap",lib="font-awesome")),
              menuSubItem(text = HTML("<span class='menu-label'> Train Model</span>"), tabName = "trainModel", icon = icon("gear", lib="font-awesome")),
              menuSubItem(text = HTML("<span class='menu-label'> Validate and deploy model</span>"), tabName = "validateDeployModel", icon = icon("server", lib="font-awesome")),
              menuSubItem(text = HTML("<span class='menu-label'> Predict/classify</span>"), tabName = "predictClassify", icon = icon("layer-group", lib="font-awesome"))
              
    ),
    
    menuItem(HTML("<span class='menu-label'>Additional resources</span>"), tabName = "addResources", icon = icon("book"))
  )
  
)

#Body
aphrcBody <- dashboardBody(
  headertag,
  useShinyjs(),
  theme = appTheme,
  tabItems(tabItem(tabName = "homePage",class = "active",
                   fluidRow()),
           
           tabItem(tabName = "sourcedata",
                   fluidRow(
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
                         , uiOutput("db_connect")
                         , hr()
                         , uiOutput("db_schema_list")
                         , uiOutput("db_table_list")
                         ,br()
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
                     , div(
                       style = "margin-top: 10px;"
                       , DT::DTOutput("upload_logs", width = "100%")
                     )
                   )
           ),
           tabItem(tabName = "manageData",
                   fluidRow(
                     column(
                       style = "height:90vh; overflow-y: auto;",
                       width = 3
                       , uiOutput("dataset_id")
                       , uiOutput("manage_data_apply")
                       , br()
                       , uiOutput("manage_data_show")
                     )
                     , column(width=9
                              , htmlOutput("manage_data_title")
                              , conditionalPanel(
                                condition = "input.manage_data_show == 'summarytools'"
                                , style = "display: none;"
                                , htmlOutput("data_summary_summarytools")
                              )
                              , verbatimTextOutput("data_summary")
                     )
                   )),
           
           tabItem(tabName = "Overview",
                   
                   fluidRow(id = "OverViewMenu",
                            column(width = 3
                                   , uiOutput("dataset_id")
                                   , uiOutput("manage_data_apply")
                                   , br()
                                   , uiOutput("manage_data_show")
                            )
                            , column(width=9
                                     , htmlOutput("manage_data_title")
                                     , conditionalPanel(
                                       condition = "input.manage_data_show == 'summarytools'"
                                       , style = "display: none;"
                                       , htmlOutput("data_summary_summarytools")
                                     )
                                     , verbatimTextOutput("data_summary")
                            )
                   )),
           
           tabItem(tabName = "Explore",
                   
                   fluidRow(
                     column(
                       width = 3
                       , htmlOutput("manage_data_title_explore")
                       , uiOutput("explore_data_show_data")
                       , uiOutput("explore_data_show_data_type")
                       , uiOutput("explore_data_missingness")
                       , uiOutput("explore_data_filter")
                       , uiOutput("explore_data_filter_rules")
                       , uiOutput("manage_data_explore_filter_apply")
                       , uiOutput("manage_data_explore_filter_reset")
                       , uiOutput("explore_data_select_variables")
                       , uiOutput("manage_data_select_vars")
                       , uiOutput("explore_data_quick_explore")
                       , uiOutput("explore_data_update_data")
                     ),
                     
                     column(
                       width = 9,
                       fluidRow(
                         column(width = 8,
                                conditionalPanel(
                                  condition = "input.explore_data_show_data_check == 1"
                                  , htmlOutput("current_dataset_text")
                                )
                                , conditionalPanel(
                                  condition = "input.explore_data_show_data_check == 1 & input.explore_data_show_data_type_check=='All'"
                                  , DT::DTOutput("working_df_all", width = "100%", fill=TRUE)
                                )
                                
                                , conditionalPanel(
                                  condition = "input.explore_data_show_data_check == 1 & input.explore_data_show_data_type_check != 'All'"
                                  , verbatimTextOutput("working_df")
                                )
                         )
                         
                         , column(width=4
                                  , htmlOutput("data_explore_filter_applied")
                                  , verbatimTextOutput("data_explore_filter_applied_out")
                                  , htmlOutput("explore_missing_data_out")
                                  , verbatimTextOutput("explore_missing_data")
                         ))
                       , fluidRow(column(width=12
                                         , htmlOutput("explore_data_quick_explore_ui")
                                         , verbatimTextOutput("explore_data_quick_explore_out")
                       )
                       )))),
           
           tabItem(tabName = "Transform",
                   fluidRow(
                     column(width = 3
                            , htmlOutput("manage_data_title_transform")
                            , htmlOutput("transform_data_select_vars")
                            , htmlOutput("transform_data_change_type")
                            , uiOutput("transform_data_change_type_choices")
                            , htmlOutput("transform_data_rename_variable")
                            , uiOutput("transform_data_rename_variable_input")
                            , uiOutput("transform_data_recode_variable")
                            , uiOutput("transform_data_recode_variable_choices")
                            , uiOutput("transform_data_recode_variable_input")
                            , uiOutput("transform_data_create_missing_values")
                            , uiOutput("transform_data_create_missing_values_choices")
                            , uiOutput("transform_data_create_missing_values_options")
                            , uiOutput("transform_data_create_missing_values_input")
                            , uiOutput("transform_data_create_missing_values_input_numeric")
                            , uiOutput("transform_data_create_missing_values_input_range")
                            , uiOutput("transform_data_identify_outliers")
                            , uiOutput("transform_data_handle_outliers_choices")
                            , uiOutput("transform_data_handle_outliers_correct_options")
                            , uiOutput("transform_data_handle_outliers_correct_options_input")
                            , uiOutput("transform_data_handle_missing_values")
                            , uiOutput("transform_data_handle_missing_values_choices")
                            , uiOutput("transform_data_handle_missing_values_options")
                            , uiOutput("transform_data_handle_missing_values_new_category")
                            , uiOutput("transform_data_handle_missing_values_new_numeric")
                            , uiOutput("transform_data_apply")
                     )
                     , column(width=9,
                              fluidRow(column(width=4
                                              , htmlOutput("transform_data_variable_type_ui")
                                              , verbatimTextOutput("transform_data_variable_type")
                                              , htmlOutput("transform_data_variable_type_log_ui")
                                              , verbatimTextOutput("transform_data_variable_type_log")
                                              , htmlOutput("transform_data_renamed_variable_log_ui")
                                              , verbatimTextOutput("transform_data_renamed_variable_log")
                                              , htmlOutput("transform_data_recoded_variable_labels_log_ui")
                                              , verbatimTextOutput("transform_data_recoded_variable_labels_log")
                                              , htmlOutput("transform_data_create_missing_values_log_ui")
                                              , verbatimTextOutput("transform_data_create_missing_values_log")
                                              , htmlOutput("transform_data_handle_outliers_log_ui")
                                              , verbatimTextOutput("transform_data_handle_outliers_log")
                              )
                              , column(width=8
                                       , htmlOutput("transform_data_quick_explore_ui")
                                       , verbatimTextOutput("transform_data_quick_explore_out")
                                       , htmlOutput("transform_data_handle_missing_values_ui")
                                       , verbatimTextOutput("transform_data_handle_missing_values_out")
                                       , plotlyOutput("transform_data_quick_plot_out")
                              )
                              )
                     )
                   )),
           
           tabItem(tabName = "summarizeCategorical",
                   fluidRow()),
           tabItem(tabName = "summarizeNumerical",
                   fluidRow()),
           tabItem(tabName = "dataPartitioning",
                   fluidRow()),
           tabItem(tabName = "featureEngineering",
                   fluidRow()),
           tabItem(tabName = "trainModel",
                   fluidRow()),
           tabItem(tabName = "validateDeployModel",
                   fluidRow()),
           tabItem(tabName = "predictClassify",
                   fluidRow()),
           tabItem(tabName = "addResources",
                   fluidRow())
           
           
  ))

fluidPage(
  header,
  aphrcHeader <- dashboardHeader(disable = TRUE),
  
  dashboardPage(aphrcHeader, aphrcSiderbar, aphrcBody,skin = "green"),
  
  
  footer
)
