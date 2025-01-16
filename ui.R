library(shiny)
library(shinyjs)
library(shinyvalidate)
library(shinyalert)
library(shinyWidgets)

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

fluidPage(
  shiny::includeScript("js/script.js"),
  useShinyjs(),
  tabsetPanel(
    #### ---- Data Upload --------------------------------------------------####
    tabPanel("Source data"
      , fluidRow(
        sidebarPanel(
          style = "height:90vh; overflow-y: auto;",
          width = 3
          ## Change language
          , uiOutput("change_language")
          , hr()
          ## Upload data types
          , uiOutput("upload_type")
          , hr()
          , uiOutput("show_uploaded")
        )
        , mainPanel(
            width = 9
            , div(id = "upload_form"
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
              ,br()
              ,conditionalPanel(
                condition = "input.upload_type == 'Database connection'",
                DT::DTOutput("db_table_view", width = "100%")
              )
              ,uiOutput("submit_upload")              
              ,br()
            )
            , br()
            , uiOutput("upload_info")
            , hr()
            , div(
                style = "margin-top: 10px;"
                , DT::DTOutput("upload_logs", width = "100%")
            )
        )
      )
    )
    #### ---- Manage data --------------------------------------------------####
    , navbarMenu("Manage data"
      , tabPanel("Overview"
        , fluidRow(
            sidebarPanel(
              style = "height:90vh; overflow-y: auto;",
              width = 3
              , uiOutput("dataset_id")
                , uiOutput("manage_data_apply")
                , br()
                , uiOutput("manage_data_show")
            )
            , mainPanel(width=9
              , htmlOutput("manage_data_title")
              , conditionalPanel(
                  condition = "input.manage_data_show == 'summarytools'"
                  , style = "display: none;"
                  , htmlOutput("data_summary_summarytools")
              )
              , verbatimTextOutput("data_summary")
            )
        )
      )
      ##### ---- Explore data ----------------------------------------------####
      , "----"
      , tabPanel("Explore"
        , fluidRow(
          sidebarPanel(
            style = "height:90vh; overflow-y: auto;",
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
          )
          , mainPanel(
            conditionalPanel(
              condition = "input.explore_data_show_data_check == 1"
              , column(width = 8
                , htmlOutput("current_dataset_text")
              )
            )
            , conditionalPanel(
              condition = "input.explore_data_show_data_check == 1 & input.explore_data_show_data_type_check=='All'"
              , column(width = 8
                , DT::DTOutput("working_df_all", width = "100%", fill=TRUE)
              )
            )
            , conditionalPanel(
              condition = "input.explore_data_show_data_check == 1 & input.explore_data_show_data_type_check != 'All'"
              , column(width = 8
                , verbatimTextOutput("working_df")
              )
            )
            , column(width=4
               , htmlOutput("data_explore_filter_applied")
               , verbatimTextOutput("data_explore_filter_applied_out")
               , htmlOutput("explore_missing_data_out")
               , verbatimTextOutput("explore_missing_data")
            )
            , column(width=12
              , htmlOutput("explore_data_quick_explore_ui")
              , verbatimTextOutput("explore_data_quick_explore_out")
            )
          )
        )
      )
      ##### ---- Transform data --------------------------------------------####
      , "----"
      , tabPanel("Transform"
        , fluidRow(
          sidebarPanel(
            style = "height:90vh; overflow-y: auto;"
            , width = 3
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
          , mainPanel(
            column(width=4
              , htmlOutput("transform_data_plot_missing_data_out_ui")
              , plotOutput("transform_data_plot_missing_data_out")
            )
            , column(width=8
              , plotlyOutput("transform_data_quick_plot_out")
            )
            # , column(width=4
            #   , htmlOutput("transform_data_variable_type_ui")
            #   , verbatimTextOutput("transform_data_variable_type")
            #   , htmlOutput("transform_data_variable_type_log_ui")
            #   , verbatimTextOutput("transform_data_variable_type_log")
            #   , htmlOutput("transform_data_renamed_variable_log_ui")
            #   , verbatimTextOutput("transform_data_renamed_variable_log")
            #   , htmlOutput("transform_data_recoded_variable_labels_log_ui")
            #   , verbatimTextOutput("transform_data_recoded_variable_labels_log")
            #   , htmlOutput("transform_data_created_missing_values_log_ui")
            #   , verbatimTextOutput("transform_data_created_missing_values_log")
            #   , htmlOutput("transform_data_handle_missing_values_log_ui")
            #   , verbatimTextOutput("transform_data_handle_missing_values_log")
            #   , htmlOutput("transform_data_handle_outlier_values_log_ui")
            #   , verbatimTextOutput("transform_data_handle_outlier_values_log")
            # )
            # , column(width=8
            #   , htmlOutput("transform_data_quick_explore_ui")
            #   , verbatimTextOutput("transform_data_quick_explore_out")
            #   , htmlOutput("transform_data_handle_missing_values_ui")
            #   , verbatimTextOutput("transform_data_handle_missing_values_out")
            #   , htmlOutput("transform_data_handle_outliers_log_ui")
            #   , verbatimTextOutput("transform_data_handle_outliers_log")
            #   # , htmlOutput("transform_data_plot_missing_data_out_ui")
            #   # , plotOutput("transform_data_plot_missing_data_out")
            #   # , plotlyOutput("transform_data_quick_plot_out")
            # )
          )
          , br()
          , hr()
          , mainPanel(
            column(width=4
              , htmlOutput("transform_data_variable_type_ui")
              , verbatimTextOutput("transform_data_variable_type")
              , htmlOutput("transform_data_variable_type_log_ui")
              , verbatimTextOutput("transform_data_variable_type_log")
              , htmlOutput("transform_data_renamed_variable_log_ui")
              , verbatimTextOutput("transform_data_renamed_variable_log")
              , htmlOutput("transform_data_recoded_variable_labels_log_ui")
              , verbatimTextOutput("transform_data_recoded_variable_labels_log")
              , htmlOutput("transform_data_created_missing_values_log_ui")
              , verbatimTextOutput("transform_data_created_missing_values_log")
              , htmlOutput("transform_data_handle_missing_values_log_ui")
              , verbatimTextOutput("transform_data_handle_missing_values_log")
              , htmlOutput("transform_data_handle_outlier_values_log_ui")
              , verbatimTextOutput("transform_data_handle_outlier_values_log")
            )
            , column(width=8
              , htmlOutput("transform_data_quick_explore_ui")
              , verbatimTextOutput("transform_data_quick_explore_out")
              , htmlOutput("transform_data_handle_missing_values_ui")
              , verbatimTextOutput("transform_data_handle_missing_values_out")
              , htmlOutput("transform_data_handle_outliers_log_ui")
              , verbatimTextOutput("transform_data_handle_outliers_log")
              # , htmlOutput("transform_data_plot_missing_data_out_ui")
              # , plotOutput("transform_data_plot_missing_data_out")
              # , plotlyOutput("transform_data_quick_plot_out")
            )
          )
        )
      )
    )
    , tabPanel("Visualise results")
  )
)
