# Helper function to create a collapsible section
collapsible_panel <- function(title, ..., open = FALSE) {
  tags$details(
    open = if (open) NA else NULL,
    tags$summary(title, style = "font-weight: bold; cursor: pointer; margin-top: 15px;"),
    div(style = "padding: 10px; border: 1px solid #ddd; border-radius: 5px; margin-top: 5px;", ...
    )
  )
}

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
			          
			          div(id = "DivvisualizationMenu",
			          box(
			            title = htmlOutput("bivariate_header_label"),
			            status = "success",
			            solidHeader = TRUE,
			            width = 12,
			            collapsible = TRUE,
			            collapsed = FALSE,
  			          hr(),
  			          fluidRow(column(
  			            width = 3,
			            uiOutput("user_select_bivariate_outcome"),
			            uiOutput("user_select_Bivariate_features"),
			            uiOutput("user_select_color_parlet_bivariate"),
			            htmlOutput("bivariate_plot_title"),
			            uiOutput("user_generatebivriate")
			          ),
			          column(
			            width = 9,
			            plotOutput("BivariatePlotOutput", height = "600px")
			          ))),
			          br(),
			          box(
			            title = uiOutput("corrplot_header_label"),
			            status = "success",
			            solidHeader = TRUE,
			            collapsible = TRUE,
			            collapsed = FALSE,
			            width = 12,
			          hr(),
			          fluidRow(column(
			            width = 3,
			            uiOutput("user_select_corr_features"),
			            uiOutput("user_select_color_parlet_corrplot")),
			            column(width=9,
			                   plotOutput("CorrPlotOutput")
			            )
			          )),
			          br(),
			          fluidRow(column(width = 3, offset = 9, uiOutput("user_download_autoreport")))
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
                         uiOutput("user_ggthemes"),
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
  column(width = 9,
    uiOutput("modelling_framework_choices"),

    ## --- Bloc commun ou dynamique ---
    conditionalPanel(
      condition = "input.modelling_framework_choices == 'Caret'",
      uiOutput("feature_engineering_perform_partition"),
      uiOutput("feature_engineering_perform_preprocess"),
      uiOutput("feature_engineering_perform_missing_impute"),
      uiOutput("feature_engineering_impute_missing_impute"),
      uiOutput("feature_engineering_perform_fe_steps"),
      uiOutput("feature_engineering_perform_corr_steps"),
      uiOutput("feature_engineering_perform_corr_steps_value"),
      uiOutput("feature_engineering_perform_pca_steps"),
      uiOutput("feature_engineering_perform_upsample_steps"),
      uiOutput("feature_engineering_perform_upsample_steps_choices"),
      uiOutput("feature_engineering_apply")
    ),

    conditionalPanel(
      condition = "input.modelling_framework_choices == 'Pycaret'",
      #DT::DTOutput("pycaret_results_table"),
      uiOutput("automl_module_ui"),
      #downloadButton("download_pycaret_results", "Download PyCaret results"),
      br(), br(),
      #uiOutput("automl_module_ui")
      #automl_controls_ui("automl_controls")
    )
  ),
  column(width = 9,
    uiOutput("feature_engineering_preprocessed_log_ui"),
    verbatimTextOutput("feature_engineering_preprocessed_log")
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
								  fluidRow(
									column(width = 12
										, uiOutput("model_training_setup_presetup")

									)
								, column(width=12
									, uiOutput("model_training_caret_models_ui")
								)
								, column(width=12
									, uiOutput("model_training_caret_train_metrics")
								)

                , column(width=12
                  , train_model_ui("train_model")
                )

							)
						),
						tabItem(tabName = "validateDeployModel",
								  fluidRow(column(width = 12,
      deployment_ui("deploy")
    ))
                  ),

						tabItem(tabName = "predictClassify",
								  fluidRow()),
            tabItem(tabName = "deeplearning",
								  fluidRow(
                    column(width=4, 
                        h4("Task Configuration"),
                        selectInput("task_selector", "Select Task:",
                                    choices = c("Object Detection", "ASR")),
                        
                        # --- Object Detection Training UI ---
                        shinyjs::hidden(
                            div(
                                id = "obj_panel",
                                h5("Object Detection Training", style="font-weight:bold; margin-top:20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
                                collapsible_panel("Paths & Naming", open = TRUE,
                                    textInput("obj_data_dir", "Data Directory", "/path/to/obj/dataset"),
                                    textInput("obj_model_checkpoint", "Model Checkpoint", "facebook/detr-resnet-50"),
                                    textInput("obj_run_name", "Run Name", "shiny-obj-run"),
                                    textInput("obj_version", "Version", "1.0.0")
                                ),
                                collapsible_panel("Training Parameters",
                                    numericInput("obj_epochs", "Epochs", 5, min = 1),
                                    numericInput("obj_learning_rate", "Learning Rate", 5e-5, step = 1e-6),
                                    numericInput("obj_weight_decay", "Weight Decay", 1e-4, step = 1e-5),
                                    numericInput("obj_train_batch_size", "Train Batch Size", 8, min = 1),
                                    numericInput("obj_eval_batch_size", "Eval Batch Size", 8, min = 1),
                                    numericInput("obj_gradient_accumulation_steps", "Gradient Accumulation", 1, min = 1),
                                    numericInput("obj_max_image_size", "Max Image Size", 600, min = 128)
                                ),
                                collapsible_panel("Saving & Early Stopping",
                                    numericInput("obj_early_stopping_patience", "Early Stopping Patience", 5),
                                    numericInput("obj_early_stopping_threshold", "Early Stopping Threshold", 0.0, step = 1e-4)
                                ),
                                collapsible_panel("Execution & Reproducibility",
                                    numericInput("obj_seed", "Seed", 42),
                                    numericInput("obj_num_proc", "Number of Processes", 4, min = 0)
                                ),
                                collapsible_panel("Hub & Logging",
                                    checkboxInput("obj_push_to_hub", "Push to Hub", FALSE),
                                    checkboxInput("obj_log_to_wandb", "Log to W&B", FALSE),
                                    textInput("obj_wandb_project", "W&B Project", ""),
                                    textInput("obj_wandb_entity", "W&B Entity", "")
                                ),
                                actionButton("start_obj_job", "Start Object Detection Job", class = "btn-primary", style="margin-top: 15px; width: 100%;")
                            )
                        ),
                        
                        # --- ASR Training UI ---
                        shinyjs::hidden(
                            div(
                                id = "asr_panel",
                                h5("ASR Training", style="font-weight:bold; margin-top:20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
                                collapsible_panel("Paths & Naming", open = TRUE,
                                    textInput("asr_data_dir", "Data Directory", "/path/to/asr/dataset"),
                                    selectInput("asr_model_arch", "Model Architecture", 
                                                choices = c("Whisper", "XLS-R", "Wav2Vec2-BERT")),
                                    selectInput("asr_model_checkpoint", "Model Checkpoint", choices = NULL),
                                    textInput("asr_run_name", "Run Name", "shiny-asr-run"),
                                    textInput("asr_version", "Version", "1.0.0")
                                ),
                                collapsible_panel("Dataset & Language",
                                    textInput("asr_language", "Language", "english"),
                                    textInput("asr_language_code", "Language Code", "en"),
                                    textInput("asr_speaker_id_column", "Speaker ID Column", "speaker_id"),
                                    textInput("asr_text_column", "Text Column", "sentence")
                                ),
                                collapsible_panel("Preprocessing & Filtering",
                                    numericInput("asr_target_sampling_rate", "Target Sampling Rate", 16000),
                                    numericInput("asr_min_duration_s", "Min Duration (s)", 1.0),
                                    numericInput("asr_max_duration_s", "Max Duration (s)", 30.0),
                                    numericInput("asr_min_transcript_len", "Min Transcript Length", 10),
                                    numericInput("asr_max_transcript_len", "Max Transcript Length", 300),
                                    checkboxInput("asr_apply_outlier_filtering", "Apply Outlier Filtering", FALSE),
                                    numericInput("asr_outlier_std_devs", "Outlier Std Devs", 2.0)
                                ),
                                collapsible_panel("Data Splitting",
                                    checkboxInput("asr_is_presplit", "Is Presplit?", TRUE),
                                    checkboxInput("asr_speaker_disjointness", "Speaker Disjoint Split", TRUE),
                                    numericInput("asr_train_ratio", "Train Ratio", 0.8, min = 0, max = 1),
                                    numericInput("asr_dev_ratio", "Dev Ratio", 0.1, min = 0, max = 1)
                                ),
                                collapsible_panel("Training Parameters",
                                    numericInput("asr_epochs", "Epochs", 5, min = 1),
                                    numericInput("asr_learning_rate", "Learning Rate", 3e-4, step = 1e-5),
                                    selectInput("asr_lr_scheduler_type", "LR Scheduler", choices = c("linear", "cosine", "constant")),
                                    numericInput("asr_warmup_ratio", "Warmup Ratio", 0.1),
                                    numericInput("asr_train_batch_size", "Train Batch Size", 16),
                                    numericInput("asr_eval_batch_size", "Eval Batch Size", 16),
                                    numericInput("asr_gradient_accumulation_steps", "Gradient Accumulation", 1),
                                    selectInput("asr_optimizer", "Optimizer", choices = c("adamw_torch", "adamw_hf", "adafactor"))
                                ),
                                collapsible_panel("Saving & Early Stopping",
                                    numericInput("asr_early_stopping_patience", "Early Stopping Patience", 5),
                                    numericInput("asr_early_stopping_threshold", "Early Stopping Threshold", 1e-3)
                                ),
                                collapsible_panel("Hub & Logging",
                                    checkboxInput("asr_push_to_hub", "Push to Hub", FALSE),
                                    textInput("asr_hub_model_id", "Hub Model ID", ""),
                                    checkboxInput("asr_hub_private_repo", "Private Hub Repo", TRUE),
                                    checkboxInput("asr_log_to_wandb", "Log to W&B", FALSE),
                                    textInput("asr_wandb_project", "W&B Project", ""),
                                    textInput("asr_wandb_entity", "W&B Entity", "")
                                ),
                                collapsible_panel("Execution & Advanced",
                                    numericInput("asr_seed", "Seed", 42),
                                    numericInput("asr_num_proc", "Number of Processes", 4)
                                ),
                                actionButton("start_asr_job", "Start ASR Job", class = "btn-success", style="margin-top: 15px; width: 100%;")
                            )
                        )
                    ), 
                    
                    column(width=8,
                        tabsetPanel(
                            id = "main_tabs",
                            type = "tabs",
                            tabPanel("Training Status", 
                                h4("Job Status", style="margin-top:20px;"),
                                wellPanel(
                                    strong("Task:"), textOutput("job_task_display", inline = TRUE), br(),
                                    strong("Job ID:"), textOutput("job_id_display", inline = TRUE), br(),
                                    strong("Status:"), textOutput("job_status_display", inline = TRUE)
                                ),
                                h4("Training Progress"),
                                div(
                                    style = "background-color: #e9ecef; border-radius: .25rem; height: 20px; margin-bottom: 1rem;",
                                    div(id = "progress-bar", style = "background-color: #007bff; height: 100%; width: 0%; border-radius: .25rem; transition: width .6s ease;")
                                ),
                                textOutput("progress_text"),
                                hr(),
                                h4("Evaluation Results"),
                                DTOutput("eval_table"),
                                hr(),
                                tags$details(
                                    style = "margin-top: 15px;",
                                    tags$summary("View Full Log", style = "cursor: pointer; font-weight: bold;"),
                                    div(
                                      style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 10px; margin-top: 5px; max-height: 400px; overflow-y: auto; font-family: monospace; white-space: pre-wrap;",
                                      verbatimTextOutput("log_output")
                                    )
                                )
                            ),
                            
                            tabPanel("Inference",
                                conditionalPanel(
                                    condition = "input.task_selector == 'Object Detection'",
                                    h4("Object Detection Inference", style="margin-top:20px;"),
                                    wellPanel(
                                        textInput("infer_run_name", "Enter Run Name to Find Checkpoints", ""),
                                        selectInput("infer_checkpoint_dropdown", "Select Checkpoint", choices = NULL),
                                        fileInput("infer_obj_image_upload", "Upload Image for Detection", accept = c('image/png', 'image/jpeg', 'image/jpg')),
                                        sliderInput("infer_obj_threshold", "Confidence Threshold", min = 0.1, max = 1.0, value = 0.5, step = 0.05),
                                        actionButton("start_obj_inference", "Run Inference", class = "btn-info", style="margin-top: 10px;")
                                    ),
                                    hr(),
                                    h5("Inference Result"),
                                    uiOutput("inference_status_ui"),
                                    imageOutput("inference_image_output", height = "auto")
                                ),
                                conditionalPanel(
                                    condition = "input.task_selector == 'ASR'",
                                    h4("ASR Inference", style="margin-top:20px;"),
                                    wellPanel(
                                        textInput("infer_asr_run_name", "Enter Run Name to Find Checkpoints", ""),
                                        selectInput("infer_asr_checkpoint_dropdown", "Select Checkpoint", choices = NULL),
                                        fileInput("infer_asr_audio_upload", "Upload Audio File", accept = c('audio/wav', 'audio/mp3', 'audio/flac')),
                                        actionButton("start_asr_inference", "Run Inference", class = "btn-info", style="margin-top: 10px;")
                                    ),
                                    hr(),
                                    h5("Transcription Result"),
                                    uiOutput("asr_inference_status_ui"),
                                    div(
                                      style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin-top: 5px; min-height: 100px; font-size: 1.1em;",
                                      textOutput("asr_transcription_output")
                                    )
                                )
                            )
                        )
                    )
                  )),

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
              ), 
              tabItem(
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
								                 maskedPasswordInput("omop_db_pwd", "Password"),
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
                        maskedPasswordInput("UserPswdID",
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
         
 #        			tabItem(tabName = "addResources",
	#							  fluidRow()
   #            )

))
