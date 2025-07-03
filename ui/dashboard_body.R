aphrcBody <- dashboardBody(
headertag,
useShinyjs(),
# useWaiter(), #FIXME: Use better one
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
													  , htmlOutput("transform_data_plot_missing_data_out_ui")
													  , plotOutput("transform_data_plot_missing_data_out")
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
							, uiOutput("combine_data_type_choices")
							, uiOutput("combine_data_match_type")
							, htmlOutput("combine_data_matched_vars_manual_ui")
							, column(width=4
								, uiOutput("combine_data_base_vars")
							)
							, column(width=4
								, uiOutput("combine_data_new_vars")
							)
							, column(width=4
								, htmlOutput("combine_data_matched_vars")
							)
							, br()
							, uiOutput("combine_data_manual_match_apply")
							, br()
							, uiOutput("combine_data_create_id_var_input")
							, br()
							, uiOutput("combine_data_perform_merging_apply")
							, htmlOutput("combine_data_row_wise_values_log_ui")
							, verbatimTextOutput("combine_data_row_wise_values_log")
						)
					)
			  ),
     
         
			  tabItem(tabName = "summarizeAutomatic",
			          fluidRow(htmlOutput("visualize_auto_data_title")),
			          fluidRow(uiOutput("bivariate_header_label")),
			          hr(),
			          fluidRow(column(
			            width = 3,
			            uiOutput("user_select_bivariate_outcome"),
			            uiOutput("user_select_Bivariate_features"),
			            uiOutput("user_select_color_parlet_bivariate"),
			            uiOutput("user_select_bivariate_single_color"),
			            uiOutput("bivariate_plot_title")
			          ),
			          column(
			            width = 9,
			            plotOutput("BivariatePlotOutput")
			          )),
			          hr(),
			          fluidRow(uiOutput("corrplot_header_label")),
			          hr(),
			          fluidRow(column(
			            width = 3,
			            uiOutput("user_select_corr_features"),
			            uiOutput("user_select_color_parlet_corrplot")),
			            column(width=9,
			                   plotOutput("CorrPlotOutput")
			            )
			          )),
         tabItem(tabName = "summarizeCustom",
                 fluidRow(
						column(width = 2,
						       htmlOutput("visualize_data_title")
                     , uiOutput("user_output_type"),
						       
						       div(id = "tabOutputs",
						       uiOutput("user_tab_options"),
						       br(),
						       uiOutput("user_calc_var"),
						       uiOutput("user_row_var"),
						       #uiOutput("user_strata_var"),
						       br(),
						       uiOutput("usr_create_cross_tab"),
						       br(),
						       uiOutput("user_download_table")
						),
						
						div(
						  id = "graphOutputs",
						    uiOutput("user_plot_options"),
						    uiOutput("user_select_variable_on_x_axis"),
						    uiOutput("user_select_variable_on_y_axis"),
						    uiOutput("user_plot_title"),
						    uiOutput("user_x_axis_label"),
						    uiOutput("user_y_axis_label"),
						    br(),
						    uiOutput("user_create"),
						    br(),
						    br(),
						    uiOutput("user_download")
						),align = "left"
						
						),
                   column(
                     width = 8,
                     uiOutput("user_chart_type"),
                              uiOutput("tabSummaries"),
                     plotOutput("GeneratedPlot", height = "65vh"),
                     align = "center"),
                       column(
                         width = 2,
                         uiOutput("user_tab_more_out"),
                         uiOutput("user_graph_more_out"),
                         div(id = "tabmoreoption",
                             uiOutput("user_table_options"),
                             br(),
                             uiOutput("user_report_numeric"),
                             uiOutput("user_add_p_value"),
                             uiOutput("user_add_confidence_interval"),
                             uiOutput("user_drop_missing_values"),
                             uiOutput("user_numeric_summary"),
                             uiOutput("user_table_caption")
                         ),
                         
                         div(id = "graphmoreoption",
                         uiOutput("user_more_plot_options"),
                         uiOutput("user_transform_to_doughnut"),
                         uiOutput("user_select_color_variable"),
                         uiOutput("user_select_group_variable"),
                         uiOutput("user_visual_orientation"),
                         uiOutput("user_bar_width"),
                         uiOutput("user_line_size"),
                         uiOutput("user_select_line_type"),
                         uiOutput("user_add_shapes"),
                         uiOutput("user_select_shape"),
                         uiOutput("user_add_smooth"),
                         uiOutput("user_display_confidence_interval"),
                         uiOutput("user_level_of_confidence_interval"),
                         
                         uiOutput("user_select_line_join"),
                         uiOutput("user_add_line_type"),
                         
                         uiOutput("user_add_points"),
                         uiOutput("user_y_variable_summary_type"),
                         uiOutput("user_title_position"),
                         uiOutput("user_size_of_plot_title"),
                         uiOutput("user_axis_title_size"),
                         uiOutput("user_facet_title_size"),
                         uiOutput("user_axis_text_size"),
                         uiOutput("user_data_label_size"),
                         uiOutput("user_x_axis_text_angle"),
                         uiOutput("user_legend_title"),
                         uiOutput("user_stacked"),
                         uiOutput("user_add_density"),
                         uiOutput("user_remove_histogram"),
                         uiOutput("user_select_color_variable_single"),
                         uiOutput("user_select_color_parlet")
                         
                     ),
                     align = "right")
                   ),
						fluidRow(br(),DT::DTOutput("dfPreview"))
					  ), 

					  tabItem(tabName = "researchQuestions"
                     , fluidRow(
                        column(width = 3
                           , htmlOutput("research_questions_title")
                           , uiOutput("generate_research_questions_outcome")
                           , uiOutput("generate_research_questions_outcome_selected")
									, uiOutput("generate_research_questions_choices")
									, uiOutput("generate_research_questions_api_token")
									, uiOutput("generate_research_questions_api_token_apply")
									, uiOutput("generate_research_questions_apply")
									, htmlOutput("generate_research_questions_additional")
									, uiOutput("generate_research_questions_additional_analysis_ui")
                        )
                        , column(width = 9
									, htmlOutput("generate_research_questions_gemini") 
									, htmlOutput("generate_research_question_gemini_suggest_analysis")
                        )
                     )
                  ),
			

						tabItem(tabName = "setupModels",
							fluidRow(
								column(width=3
									, uiOutput("setup_models_analysis_session_name")
									, uiOutput("setup_models_analysis_session_seed")
									, uiOutput("setup_models_analysis_target_variable_options")
									, uiOutput("setup_models_analysis_target_variable")
									, uiOutput("setup_models_analysis_exclude_variables")
									, uiOutput("setup_models_analysis_type")
									, uiOutput("setup_models_analysis_model_type")
									, uiOutput("setup_models_analysis_partition_ratio")

									, uiOutput("setup_models_analysis_apply")
								)

								, column(width = 9
									, htmlOutput("setup_models_analysis_results")
								)
							)
						),
						
						tabItem(tabName = "featureEngineering",
							fluidRow(
								column(width = 3
									, uiOutput("impute_missing_options")
								)
							)
						),
			      tabItem(
			        tabName = "evidenceQuality",
			        fluidRow( 
			          p("OMOP Data Quality Check and Characterization"),
			          uiOutput("omop_connection")
			          , uiOutput("existing_connection")
			          , uiOutput("omop_quality_type")
			          , uiOutput("schemas")
			          , uiOutput("generate_dqd")
			          ,verbatimTextOutput("stderr_log")
			          ,uiOutput("view_dqd")
			          ,br()
			          ,uiOutput("open_link")
			        )
			      ), 
			     tabItem(tabName = "cohortConstruction",
			          fluidRow()),
			  
						tabItem(tabName = "trainModel",
								  fluidRow()),
						tabItem(tabName = "validateDeployModel",
								  fluidRow()),
						tabItem(tabName = "predictClassify",
								  fluidRow()),
						tabItem(tabName = "addResources",
								  fluidRow()),

            tabItem(
              tabName = "CohortConstructor",
              
              # --- DATABASE CONNECTION ---
              box(
                title = "Database Connection",
                width = 12,
                status = "success",
                solidHeader = TRUE,
                collapsible = FALSE,
                fluidRow(
                  column(4, uiOutput("dbmsID")),
                  column(4, uiOutput("dbmsServerID")),
                  column(4, uiOutput("cohort_db_port"))
                ),
                fluidRow(
                  column(4, uiOutput("cohort_db_name")),
                  column(4, uiOutput("UserID")),
                  column(4, uiOutput("UserPswdID"))
                  #column(2, offset = 2, 
                         #br(),uiOutput("ConnectCohortID"), align="right")
                ),
                fluidRow(
                   column(2, offset = 9, uiOutput("ConnectCohortID"))
                 )
              ),
              
              # --- CDM REFERENCE CREATION ---
              box(
                title = "CDM Reference",
                width = 12,
                status = "success",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                fluidRow(
                 # column(4, uiOutput("CDMConn")),
                  column(4, uiOutput("CDMConnName")),
                  column(4, uiOutput("CDMSchemaName")),
                  column(4, uiOutput("ResultSchemaName"))
                  
                ),
                fluidRow(
                  # column(4, uiOutput("ResultSchemaName")),
                  # column(4, uiOutput("achillesSchemaName")),
                  #column(4, uiOutput("achillesSchemaName")),
                  column(3, offset = 9, br(), uiOutput("CreateCDMID"), align="right")
                )
                # ,fluidRow(
                #   column(6, uiOutput("tableDropdown1UI")),
                #   column(6, uiOutput("tableDropdown2UI"))
                # )
              )
              , # --- COHORT CREATION ---
              box(
                title = "Cohort Creation",
                width = 12,
                status = "success",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                fluidRow(
                  #column(4, uiOutput("CohortcreationName")),
                  column(4, uiOutput("ConceptKeyword")),
                  column(4, uiOutput("CohortNameID")),
                  column(4, uiOutput("CohortDateID")
                ),
                fluidRow(
                 # column(4, uiOutput("CohortDateID")),
                  column(4, offset = 8, br(), uiOutput("GenerateCohortID"), br(), align="right")
                ),
                # --- OUTPUT: COHORT SUMMARY & DEMOGRAPHICS ---
                # --- OUTPUT: COHORT SUMMARY & DEMOGRAPHICS ---
                box(
                  title = "Cohort Summary",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  tableOutput("cohort_summary"),
                  uiOutput("cohort_demographics")
                ),
                
                # --- INTERACTIVE PLOTS ---
                box(
                  title = "Cohort Plots",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  fluidRow(
                    column(6, plotlyOutput("Gender_plot")),
                    column(6, plotlyOutput("age_group_plot"))
                  ),
                  fluidRow(
                    column(6, plotlyOutput("Race_plot")),
                    column(6, plotlyOutput("ethnicity_plot"))
                  )#,
                  
                  #uiOutput("SelectCohortTable"),     # dropdown for cohort table name
                  #uiOutput("ExtractFeaturesID"),     # extraction button
                  #br(),
                  #uiOutput("CovariateTableUI")       # table + download
                )
                
                
              )
)),

                   
                   tabItem(
             tabName = "achilles",
             fluidRow(
               box(
                 title = "Create Connection Details",
                 status = "success",
                 solidHeader = TRUE,
                 width = 12,
                 collapsible = FALSE,
                 fluidRow(
                   column(4,
                          uiOutput("cbodatabasetype")),
                   column(4,
                          uiOutput("cbodbhost")), 
                   column(4,
                          uiOutput("cbodbport"))
                 ),
                 fluidRow(
                   column(4,
                          uiOutput("cbodbname")),
                   column(4,
                          uiOutput("cbodbuser")),
                   column(4,
                          uiOutput("cbodbpass"))
                          
                 ),
                 fluidRow(
                   column(12,
                          actionButton("achilles_db_connect",
                                       "Connect",
                                       icon = icon("plug"),
                                       class = "btn btn-primary"))
                 )
               ),
               
             fluidRow(
               box(
                 title = "Schema Selection",
                 status = "success",
                 solidHeader = TRUE,
                 width = 12,
                 collapsible = FALSE,
                 
                 # Shown after connection
                 uiOutput("schema_selectors"),
                 uiOutput("run_achilles")
               )
             )
             )
              ), tabItem(
								    tabName = "omop_visualizations",
								    fluidRow(
								      column(
								        width = 3,
								        box(
								          title = "Database Connection",
								          status = "success",
								          solidHeader = TRUE,
								          width = 12,
								          collapsible = TRUE,
								          column(12,
								                 selectInput("omop_dbms", "Database Type", choices = c("postgresql", "mysql"), selected = "postgresql"),
								                 textInput("omop_db_host", "Host", placeholder = "e.g., localhost or IP"),
								                 numericInput("omop_db_port", "Port", value = 5432),
								                 textInput("omop_db_name", "Database Name"),
								                 textInput("omop_db_user", "Username"),
								                 passwordInput("omop_db_pwd", "Password"),
								                 actionButton("omop_db_connect", "Connect", icon = icon("plug"), class = "btn btn-primary")
								          )
								        ),
								        box(
								          title = "Schema & Version Selection",
								          status = "success",
								          solidHeader = TRUE,
								          width = 12,
								          collapsible = FALSE,
								          uiOutput("omop_cdm_schema"),
								          uiOutput("omop_results_schema"),
								          uiOutput("omop_vocabulary_schema")
								        ),
								        box(
								          title = "CDM Table Selection",
								          status = "success",
								          solidHeader = TRUE,
								          width = 12,
								          collapsible = FALSE,
								          uiOutput("omop_cdm_tables"),
								          actionButton("generate_summary",
								                       "Generate Summary",
								                       icon = icon("play"),
								                       class = "btn btn-success")
								        )
								      ),
								      column(
								        width = 9,
								        tabsetPanel(
								          tabPanel("General",
								                   box(
								                     title = "OMOP Snapshot Summary",
								                     status = "success",
								                     solidHeader = TRUE,
								                     width = 12,
								                     collapsible = TRUE,
								                     uiOutput("omop_snapshot_summary")
								                   ),
								                   box(
								                     title = "CDM Table Record Counts",
								                     status = "success",
								                     solidHeader = TRUE,
								                     width = 12,
								                     collapsible = TRUE,
								                     collapsed = TRUE,
								                     DT::dataTableOutput("cdm_table_summaries")
								                   )
								          ),
								          tabPanel("Table-specific Analysis",
								                   conditionalPanel(
								                     condition = "input.selected_cdm_table == 'person'",
								                     fluidRow(
								                       box(title = "Age Summary",
								                           status = "info", 
								                           solidHeader = TRUE,
								                           width = 12,
								                           tableOutput("age_summary"))
								                     ),
								                     fluidRow(
								                       box(title = "Gender Distribution",
								                           status = "primary",
								                           solidHeader = TRUE,
								                           width = 6,
								                           plotlyOutput("gender_plot")),
								                       box(title = "Race Distribution", 
								                           status = "primary",
								                           solidHeader = TRUE,
								                           width = 6,
								                           plotlyOutput("race_plot"))
								                     )
								                   ),
								                   conditionalPanel(
								                     condition = "input.selected_cdm_table == 'observation'",
								                     fluidRow(
								                       box(title = "Observation Concepts",
								                           status = "info",
								                           solidHeader = TRUE,
								                           width = 12,
								                           dataTableOutput("observation_table"))
								                     )
								                     ,fluidRow(
								                       box(title = "Value as Concept Distribution",
								                           status = "primary",
								                           solidHeader = TRUE, 
								                           width = 12,
								                           uiOutput("observation_value_ui")
								                           
								                       )
								                     )
								                   ),
								                   
								                   conditionalPanel(
								                     condition = "input.selected_cdm_table == 'condition_occurrence'",
								                     fluidRow(
								                       box(title = "Condition Concepts",
								                           status = "info",
								                           solidHeader = TRUE,
								                           width = 12,
								                           dataTableOutput("condition_table"))
								                     )
								                     ,fluidRow(
								                       box(title = "Value as Concept Distribution",
								                           status = "primary",
								                           solidHeader = TRUE, 
								                           width = 12,
								                           uiOutput("condition_value_ui")
								                           
								                       )
								                     )
								                   )
								          )
								        )
								      )
								    )
								  ),
								  
        
                 


tabItem(
  tabName = "FeatureExtraction",
  fluidPage(
    titlePanel("Feature Extraction Tool"),
    
    sidebarLayout(
      sidebarPanel(
        # --- DB connection info ---
        selectInput("dbmsID", "Database Type", 
                    choices = c("", "postgresql", "mysql"), 
                    selected = "postgresql"),
        textInput("dbmsServerID",
                  "Host",
                  placeholder = "e.g., localhost or IP"),
        numericInput("cohort_db_port",
                    "Port",
                     value = 5432),
        textInput("cohort_db_name",
                  "Database Name",
                  placeholder = "Required"),
        textInput("UserID",
                  "User",
                  placeholder = "Required"),
        passwordInput("UserPswdID",
                      "Password",
                      placeholder = "Required"),
        #textInput("jdbc_path", "Path to JDBC Driver:", value = "/path/to/jdbc"),
        
        # --- Connect button ---
        actionButton("ConnectCohortID", "Connect to Database", class = "btn-primary"),
        tags$hr(),
        
        # --- Dynamic dropdowns populated after connection ---
        uiOutput("cdm_schema_ui"),
        uiOutput("results_schema_ui"),
        uiOutput("cohort_table_ui"),
        uiOutput("domain_choices_ui"),
        
        # --- Extraction settings ---
        numericInput("cohort_id", "Cohort ID:", value = ""),
        textInput("output_csv", "Output CSV Path:", value = "name.csv"),
        actionButton("extract_features", "Extract Features", class = "btn-success")
      ),
      
      mainPanel(
        h4("Instructions"),
        p("1. Enter DB details and click 'Connect to Database'."),
        p("2. Then select schema and cohort info."),
        p("3. Run feature extraction and download the CSV."),
        verbatimTextOutput("feature_extract_log")
      )
    )
  )
)







))

