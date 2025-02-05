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
                       , uiOutput("db_port")
                       , uiOutput("db_connect")
                       , uiOutput("db_disconnect")
                       , hr()
                       , conditionalPanel(
                         condition = "input.upload_type == 'Database connection'",
                         radioButtons("option_picked", "Upload database records", choices = c("use a table","use SQL query"), selected = "use a table")
                       )
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
                   , div(
                     style = "margin-top: 10px;"
                     , DT::DTOutput("upload_logs", width = "100%")
                   )
                 )
         ),
         
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
         
         tabItem(tabName = "combineData"
                 , fluidRow(
                   column(width = 3
                          , htmlOutput("combine_data_title")
                   )
 						, column(width=9
							, uiOutput("combine_data_list_datasets")
							, uiOutput("combine_data_apply")
							, hr()
							, htmlOutput("combine_data_matched_vars")
						)
					  )
         ),
         
         
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
