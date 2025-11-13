library(Rautoml)
options(shiny.maxRequestSize=300*1024^2)

source("R/utils_logging.R")

# ----- FastAPI base URL -----
#api_base <- Sys.getenv("FASTAPI_BASE", "http://127.0.0.1:8000")

# ----- FastAPI base URL -----
api_base <- "http://localhost:3760"

message("[ML API base] ", api_base)


source("server/automl_controls_server.R")
source("server/train_model_server.R")
source("R/utils_api.R")
source("server/deploy_model_server.R", local=TRUE)
source("ui/deploy_model_ui.R", local=TRUE)
source("server/predict_pycaret_server.R", local = TRUE)

function(input, output, session){
  waiter_show(
    html = spin_loaders(id = 2, style="width:56px;height:56px;color:#7BC148;"),
    color = "#FFF"
  )
  
  shinyjs::show("login")
  source("server/auth.R")
  user_auth(input, output, session)


  model_training_caret_pb = Attendant$new("model_training_caret_pb", hide_on_max = TRUE)
  model_metrics_caret_pb = Attendant$new("model_metrics_caret_pb", hide_on_max = TRUE)
  deploy_models_caret_pb = Attendant$new("deploy_models_caret_pb", hide_on_max = TRUE)
  predict_models_caret_pb = Attendant$new("predict_models_caret_pb", hide_on_max = TRUE)
  predict_endpoint_models_caret_pb = Attendant$new("predict_endpoint_models_caret_pb", hide_on_max = TRUE)
  generate_research_questions_outcome_pb = Attendant$new("generate_research_questions_outcome_pb", hide_on_max = TRUE)
  generate_research_questions_additional_analysis_pb = Attendant$new("generate_research_questions_additional_analysis_pb", hide_on_max = TRUE)
  
  #### ---- Input validators ---------------------------------------------------
  source("server/input_validators.R")
  #### ---- Create needed folders for datasets and logs ------------------------
  source("server/create_dirs.R")

  #### ---- Placeholder for reactive values ------------------------------------
  ##### -------- Currently selected dataset ------------------------------------
  rv_current = reactiveValues(
    dataset_id = NULL
    , metadata_id = NULL
    , data = NULL
    , selected_vars = NULL
    , selected_var = NULL
    , working_df = NULL
    , current_filter = NULL
    , current_filter_reset = NULL
    , manage_data_title_explore = NULL
    , missing_prop = NULL
	 , has_missing_data_check=FALSE
    , manage_data_title_transform = NULL
    , merge_data_title_merge = NULL
    , transform_data_select_vars = NULL
    , vartype = NULL
	 , changed_variable_type_log = NULL
    , transform_data_plot_df = NULL
    , renamed_variable_log = NULL
    , transform_data_quick_plot_out = NULL
    , recoded_variable_labels_log = NULL
    , missing_prop_df = NULL
    , created_missing_values_log = NULL
    , outlier_values = NULL
	 , handle_missing_values_log = NULL
    , handle_outlier_values_log = NULL
    , transform_data_plot_missing_data_out = NULL
	 , quick_explore_summary = NULL
	 , max_tockens = 10000
	 , seed = 9991
	 , outcome = NULL
	 , vartype_all = NULL
  )
  
  #####------------------Plots Reactive-------------------

  plots_sec_rv <- reactiveValues(
    plot_rv=NULL
    ,tab_rv=NULL
    ,plot_bivariate_auto=NULL
    ,plot_corr = NULL
  )
  

  ##### --------- Meta data ---------------------------------------------
  rv_metadata = reactiveValues(
    upload_logs = NULL
    , dataset_ids = NULL
    , data_summary_str = NULL
    , data_summary_skim = NULL
    , data_summary_summary = NULL
    , data_summary_summarytools = NULL
  )

	rv_database <- reactiveValues(schema_list = NULL
		, table_list = NULL
		, conn = NULL
		, schema_selected = NULL
		, table_selected = NULL
		, df_table = data.frame()
		, df_table_str = NULL
		, query_table_name = NULL
		, database_host = NULL
		, database_name = NULL
		, database_user = NULL
		, database_pass = NULL
		, details = NULL
	)

	## ---
	
	rv_omop<- reactiveValues(
	  url = NULL )
	
	
	## LLM/GAI
	rv_generative_ai = reactiveValues(
		history = NULL
	)

	## Reactive values for ML/AI module
	rv_ml_ai = reactiveValues(
		session_id = NULL
		, seed_value = NULL
		, dataset_id = NULL 
		, analysis_type = NULL
		, task = NULL
		, outcome = NULL
		, model_formula = NULL
		, partition_ratio = NULL
		, predictors = NULL
		, excluded_predictors = NULL
		, ml_ai_setup_result = NULL
		, history = NULL
		, split = NULL
		, train_df = NULL
		, test_df = NULL
		, preprocessed = NULL
		, feature_engineering_preprocessed_log = NULL
		, at_least_one_model = FALSE
	)

	## RV to hold UIs
	rv_ui_models = reactiveValues(
	   model_training_caret_models_ols_check = NULL
		, model_training_caret_models_ols_advance_control = NULL
	)
		
	## Train control caret
	rv_train_control_caret = reactiveValues(
		method = "cv"
		, number = 5
		, repeats = NA
		, search = "grid"
		, verboseIter = FALSE
		, savePredictions = FALSE
		, classProbs = TRUE
	)
   
	## Trained models
	rv_training_models = reactiveValues(
		ols_model = NULL
		, ols_param = FALSE
		, ols_name = NULL
		, ols_trained_model = NULL
		, rf_model = NULL
		, rf_param = FALSE
		, rf_name = NULL
		, rf_trained_model = NULL
		, all_trained_models = NULL
	)
	
	rv_training_results = reactiveValues(
		models = NULL
		, train_metrics_df = NULL
		, test_metrics_objs = NULL
	)

  # Update training results when a new model is trained
  automl_controls_server(
    id         = "automl_controls",
    rv_current = rv_current,
    rv_ml_ai   = rv_ml_ai,
    api_base   = api_base
  )

  train_model_server(
    id         = "train_model",
    rv_current = rv_current,
    rv_ml_ai   = rv_ml_ai,
    api_base   = api_base
  )

  # End update training
  # ---- (A) Detect if a complete PyCaret run is available (leaderboard displayed) ----
  .can_show_by_train <- reactive({
    curr_ds <- rv_current$dataset_id %||% rv_ml_ai$dataset_id
    isTRUE(rv_ml_ai$status %in% c("Finished","Finished_NoPlots")) &&
    !is.null(rv_ml_ai$leaderboard) && NROW(rv_ml_ai$leaderboard) > 0 &&
    isTRUE(nzchar(rv_ml_ai$trained_dataset_id)) &&
    identical(rv_ml_ai$trained_dataset_id, curr_ds)
  })

  observeEvent(rv_current$dataset_id, {
  # If we change the dataset, we clean up the transient state linked to the previous train.
  if (!identical(rv_current$dataset_id, rv_ml_ai$trained_dataset_id)) {
    rv_ml_ai$leaderboard <- NULL
    rv_ml_ai$leaderboard_full <- NULL
    rv_ml_ai$test_leaderboard <- NULL
    rv_ml_ai$test_leaderboard_full <- NULL
    rv_ml_ai$models <- NULL
    rv_ml_ai$eval_metrics <- NULL
    rv_ml_ai$eval_plots <- NULL
    rv_ml_ai$status <- NULL
  }
}, ignoreInit = FALSE)



# ---- (B) Datasets with pre-trained models (historical) ----
  .get_models_index_csv <- function() file.path(getwd(), "logs", "models", "index.csv")
  dataset_has_history <- reactive({
    idx <- .get_models_index_csv()
    if (!file.exists(idx)) return(FALSE)
    df <- tryCatch(read.csv(idx, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || !"dataset_id" %in% names(df)) return(FALSE)
    ds <- rv_ml_ai$dataset_id %||% rv_current$dataset_id
    if (is.null(ds) || !nzchar(ds)) return(FALSE)
    any(df$dataset_id == ds & (df$framework %in% c("PyCaret","pycaret","Pycaret")))
  })

  # Expose known datasets for the Deploy module (used in its selector)
  observe({
    idx <- .get_models_index_csv()
    if (!file.exists(idx)) return(invisible(NULL))
    df <- tryCatch(read.csv(idx, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || !"dataset_id" %in% names(df)) return(invisible(NULL))
    rv_current$known_datasets <- sort(unique(df$dataset_id))
  })
  .can_show_deploy <- reactive({
    isTRUE(.can_show_by_train()) || isTRUE(dataset_has_history())
  })
  # (keep this if you still use it on the JS side)
  output$can_show_deploy <- reactive({ .can_show_deploy() })
  outputOptions(output, "can_show_deploy", suspendWhenHidden = FALSE)

  # ---- Deploy tab UI container ----
  output$deploy_container <- renderUI({
    if (!isTRUE(.can_show_deploy())) return(NULL)  # => onglet totalement vide
    column(width = 12, deployment_ui("deploy"))
  })

  outputOptions(output, "can_show_deploy", suspendWhenHidden = FALSE)
  ## Deployed models table
	rv_deploy_models = reactiveValues(
		trained_models_table = NULL
	)

	## Deployed models
	rv_deployed_models = reactiveValues()
  
	## Reactive values to stock AutoML leaderboard
	rv_automl <- reactiveValues(
	  leaderboard = NULL
	)

	#### ---- App title ----------------------------------------------------
  source("server/header_footer_configs.R", local=TRUE)
  app_title()
  
  ###-------App Footer--------------------------
  
  footer_language_translation()
  ###-------Menu Translate---------
  
  menu_translation()

  #### ---- Change language ----------------------------------------------------
  output$change_language = change_language

  source("server/change_language_update.R", local = TRUE)
  change_language_update() 
  
  #### ---- Upload data UI --------------------------------------------
  source("ui/upload_data.R", local = TRUE)
  output$upload_type = upload_type
  
  #### ---- Upload dataset/files UI --------------------------------------------
  source("server/input_files.R", local = TRUE)
  output$input_files = input_files
  
  #### ---- Show uploaded datasets UI --------------------------------------------
  output$show_uploaded = show_uploaded
  
  #### ---- Data upload form -----------------------------------------------
  source("ui/upload_form.R", local = TRUE)
  output$study_name = study_name
  output$study_country = study_country
  output$additional_info = additional_info
  output$submit_upload = submit_upload
 
  #### ---- Databse and API connection warning ---------------------
  db_api_con_future
  
  #### ---- Upload datasets ----------------------------------------
  source("server/upload_data.R", local = TRUE)
  upload_data_server()
  
  #### ---- Database integration ----------------------------------------
  source("server/database_integration.R", local = TRUE)
  database_integration_server()
 
  #### --- Database related form elements ---###
  output$db_type = db_type
  output$db_host = db_host
  output$db_name = db_name
  output$db_user = db_user
  output$db_pwd = db_pwd
  output$db_connect = db_connect
  output$db_schema_list = db_schema_list
  output$db_table_list = db_table_list
  output$db_custom_query = db_custom_query
  output$db_run_query = db_run_query
  output$db_port = db_port
  output$db_disconnect = db_disconnect
  output$db_tab_query = db_tab_query
  output$existing_connection = existing_connection
  
  source("server/omop_analysis.R", local = TRUE)
  omop_analysis_server()
  
  stderr_file_path <- file.path(getwd(), "output", "dq_stderr.txt")
  
  stderr_content<-create_log_reader(stderr_file_path)
  

  #### ---- Collect logs ----------------------------------------
  source("server/collect_logs.R", local = TRUE)
  collect_logs_server()
  
  #### ---- Display uploaded datasets ----------------------------------------
  source("server/display_uploaded_data.R", local = TRUE)
  display_uploaded_data_server()

  #### ---- Delete uploaded dadatsets ----------------------------------------
  source("server/delete_uploaded_data.R", local = TRUE)
  delete_uploaded_data_server()
  
  #### ---- Update logfiles based on existing datasets -------------------####
  source("server/update_logs.R", local = TRUE)
  update_logs_server()
  
  #### ---- Manage data ----------------------------------------------
  
  ##### ---- Select data ---------------------------------------------
  source("server/select_data.R", local = TRUE)
  select_data_server()
  manage_data_show_server()
  
  ##### ---- Display meta data for the selected dataset ---------------------------------------------
  source("server/display_metadata.R", local = TRUE)
  display_selected_metadata_server()
  reset_display_selected_metadata_server()

  ##### ---- Currently selected data ---------------------------------------------
  source("server/selected_data.R", local = TRUE)
  currently_selected_data_server()

  ##### ----Generate summary stats for the row data -------------------
  source("server/manage_data_summary.R", local = TRUE)
  generate_data_summary_server()
  display_data_summary_server()

  #### ----- Explore data -----------------------------------------------
  source("server/explore_data.R", local = TRUE)
  explore_data_server()
  explore_data_subactions_server()
  
  ##----User Defined Visualization section-----------------------
  source("ui/user_defined_visualization_header.R", local = TRUE)
  output$user_output_type = user_output_type
  output$user_tab_options = user_tab_options
  output$user_calc_var = user_calc_var
  #output$user_strata_var = user_strata_var
  output$user_row_var = user_row_var
  output$usr_create_cross_tab = usr_create_cross_tab
  output$user_download_table = user_download_table
  
  output$user_table_options = user_table_options
  output$user_report_numeric = user_report_numeric
  output$user_add_p_value = user_add_p_value
  output$user_add_confidence_interval = user_add_confidence_interval
  output$user_drop_missing_values = user_drop_missing_values
  output$user_table_caption = user_table_caption
  
  output$user_plot_options = user_plot_options
  output$user_select_variable_on_x_axis = user_select_variable_on_x_axis
  output$user_select_variable_on_y_axis = user_select_variable_on_y_axis
  output$user_plot_title = user_plot_title
  output$user_x_axis_label = user_x_axis_label
  output$user_y_axis_label = user_y_axis_label
  output$user_create = user_create
  output$user_download = user_download
  
  output$user_more_plot_options = user_more_plot_options
  output$user_transform_to_doughnut = user_transform_to_doughnut
  output$user_select_color_variable = user_select_color_variable
  output$user_select_group_variable = user_select_group_variable
  output$user_visual_orientation = user_visual_orientation
  output$user_bar_width = user_bar_width
  output$user_line_size = user_line_size
  output$user_select_line_type = user_select_line_type
  output$user_add_shapes = user_add_shapes
  
  output$user_select_shape = user_select_shape
  output$user_add_smooth = user_add_smooth
  output$user_display_confidence_interval = user_display_confidence_interval
  output$user_level_of_confidence_interval = user_level_of_confidence_interval
  output$user_select_line_join = user_select_line_join
  output$user_add_line_type = user_add_line_type
  output$user_add_points = user_add_points
  output$user_y_variable_summary_type = user_y_variable_summary_type
  output$user_title_position = user_title_position
  
  output$user_size_of_plot_title = user_size_of_plot_title
  output$user_axis_title_size = user_axis_title_size
  output$user_facet_title_size = user_facet_title_size
  output$user_axis_text_size = user_axis_text_size
  output$user_data_label_size = user_data_label_size
  output$user_x_axis_text_angle = user_x_axis_text_angle
  output$user_legend_title = user_legend_title
  output$user_stacked = user_stacked
  output$user_add_density = user_add_density
  output$user_remove_histogram = user_remove_histogram
  output$user_select_color_variable_single = user_select_color_variable_single
  output$user_select_color_parlet = user_select_color_parlet
  output$user_numeric_summary = user_numeric_summary
  output$user_tab_more_out = user_tab_more_out
  output$user_graph_more_out = user_tab_more_out
  
  output$bivariate_header_label = bivariate_header_label
  output$corrplot_header_label = corrplot_header_label
  
  output$user_select_bivariate_single_color = user_select_bivariate_single_color
  output$user_select_color_parlet_bivariate = user_select_color_parlet_bivariate
  output$user_select_color_parlet_corrplot = user_select_color_parlet_corrplot
  output$bivariate_plot_title = bivariate_plot_title
  output$corrplot_title = corrplot_title
  output$user_download_autoreport = user_download_autoreport
  output$user_generatebivriate = user_generatebivriate

  

  ##### ---- Explore data actions ----------------------------------
  explore_data_actions_server()
  
  ##### ---- Filter data --------------------------------------------
  explore_data_filter_server()
  explore_data_apply_filter_server()
  explore_data_current_filter_server()
  
  ##### ---- Show/display -------------------------------------------------------
  explore_show_data_server()
  explore_data_reset_current_filter_server()
  
  ##### ---- Compute proportion of missing data ---------------------------
  explore_missing_data_server()
  
  ##### ---- Select variables ---------------------------------------------
  explore_data_select_variables_server()
  explore_data_selected_variables_server()
  
  ##### ---- Update data -----------------------------------------------
  explore_data_update_data_server()
  
  #### ---- Transform variables -------------------------------------- ####
  source("server/transform_data.R", local = TRUE)

  ##### ---- Select variables to transform ------------------------------------###
  transform_data_select_variables_server()

  ##### ---- Change type  -----------------------------------------------###
  transform_data_change_type_server()

  ##### ---- Rename variables  -----------------------------------------------###
  transform_data_rename_variables_server()

  ##### ---- Recode/change value labels ---------------------------------------###
  transform_data_quick_explore_recode_server()
  
  ##### ---- Handle missing data ---------------------------------------###
  transform_data_create_missing_values_server()
  
  ##### ---- Identify outliers ---------------------------------------###
  transform_data_identify_outliers_server()
  
  ##### ---- Handle missing values ---------------------------------------###
  transform_data_handle_missing_values_server()
  
  ##### ---- Plot transform data ----------------------------------------------###
  transform_data_quick_explore_plot_server()
  
  ##### ---- Plot missing data ----------------------------------------------###
  transform_data_plot_missing_data_server()
  
  #### ---- Combine datasets with the existing one --------------------------------------####
  source("server/combine_data.R", local = TRUE)
  
  ##### ---- List of internal data ------------------------------------------####
  combine_data_list_datasets()
  
  ##### ---- Combine data options ------------------------------------------####
  combine_data_type()

  ##### ---- Combine data mtch type ------------------------------------------####
  combine_data_match_type()

  ##### ---- Combine data variables matched --------------------####
  combine_data_variable_matching()
  
  #### ----- Perform matching ---------------------------------####
  combine_data_perform_variable_match()

  ##### ---- Combine data perform merging  --------------------####
  combine_data_perform_merging()

  #### ---- Reset combine data --------------------------------####
  combine_data_reset()

  ##### ---- Control Custom visualizations ------------------ #####
  source("server/user_defined_visualization.R", local = TRUE)
  user_defined_server()
  
  ### ------- OMOP ------------------------------------------ #####
  
  #### ----- Cohort Constructor ---------#####
  source("server/run_cohort_pipeline.R", local = TRUE)
  run_cohort_pipeline()
  
  #### ----- Feature Extraction ---------#####
  source("server/feature_extraction_pipeline.R", local = TRUE)
  feature_extraction_pipeline()
  
  #### ---- Achilles Integration -------------------####
  
  source("server/run_achilles.R", local = TRUE)
  achilles_integration_server()
  
  ### ---- OMOP CDM Summaries---------------------------####
  source("server/omop_summaries.R", local = TRUE)
  omopVizServer()

  #### ---- Generate Research Questions --------------------------------------####
  source("server/research_questions.R", local = TRUE)
  generate_research_questions_choices()
  

  ##### ---- API Token ------------------ ####
  generate_research_questions_api_token()
  generate_research_questions_api_store()

  #### ---- Addional prompts --------------- ####
  generate_research_questions_additional()

  #### ---- Generate insights using Gemini --------------- ####
  generate_research_questions_gemini()

  #### ---- Machine learning and AI --------------- ####
  
  ##### ----- Set ML/AI UI ------------------- ####
  source("server/setup_models.R", local=TRUE)
  setup_models_ui()
  
  ##### ----- Preprocessing ------------------- ####
  source("server/feature_engineering.R", local=TRUE)
  
  #### Preprocessing ------------------------------------------- ####
  feature_engineering_perform_preprocess_server()

  #### ------ Missing value imputation -------------------------- ####
  feature_engineering_recipe_server()
  feature_engineering_impute_missing_server()
  
  #### ----- Modelling framework --------------------------------- ####

  source("server/modelling_framework.R", local=TRUE)
  modelling_framework_choices()
  
  #### ----- Model setup ----------------------------------------- ####
  source("server/model_training_setup.R", local=TRUE)
  model_training_setup_server()

  #### ----- Caret models --------------------------------------- ####
  source("server/model_training_caret_models.R", local=TRUE)
  
  ## LM/GLM
  model_training_caret_models_ols_server()

  ## RF
  model_training_caret_models_rf_server()

  ## GBM
  model_training_caret_models_gbm_server()

  ## xgbTree
  model_training_caret_models_xgbTree_server()

  ## xgbLinear
  model_training_caret_models_xgbLinear_server()

  ## svmRadial
  model_training_caret_models_svmRadial_server()
  
  ## svmLinear
  model_training_caret_models_svmLinear_server()

  ## svmPoly
  model_training_caret_models_svmPoly_server()

  ## glmnet
  model_training_caret_models_glmnet_server()
  
  #### ----- Train all models ----------------------------------- ####
  source("server/train_caret_models.R", local=TRUE)
  model_training_caret_train_all_server()

  #### ----- Compare trained models ------------------------------ ####
  source("server/compare_trained_caret_models.R", local=TRUE)
  model_training_caret_train_metrics_server()

  #### ----- Deploy trained models ------------------------------- ####
  source("server/deploy_trained_caret_models.R", local=TRUE)
  deploy_trained_caret_models()

  #### ---- Predict using no-code models ------------------------ ####
  source("server/predict_trained_caret_models.R", local=TRUE)
  predict_trained_caret_models()

  #### ---- PyCaret Integration (API) ----------------------------------------------------

  # New ADD
  rv_ml_ai   <- rv_ml_ai   %||% reactiveValues(target = NULL, outcome = NULL)
  rv_current <- rv_current %||% reactiveValues(target = NULL)

  deployment_server(id="deploy",rv_ml_ai=rv_ml_ai,rv_current = rv_current,api_base=api_base)
  predict_pycaret_server("predict_pycaret", api_base , rv_current, rv_ml_ai)

  # END NEW ADD
  #### ---- Call current dataset for FastAPI ---------------------------------------------------  
  source("server/automl_server.R", local=TRUE)
  automl_server("automl_module", rv_current, rv_ml_ai)

  observe({
    req(!is.null(rv_ml_ai$modelling_framework))  # Check if value exist
    
    if (tolower(rv_ml_ai$modelling_framework) == "pycaret") {
      output$automl_module_ui <- renderUI({
        automl_ui("automl_module")
      })
    } else {
      output$automl_module_ui <- renderUI({
        h4("")
      })
    }
  })
  
  observeEvent(input$modelling_framework_choices, {
    rv_ml_ai$framework <- tolower(input$modelling_framework_choices %||% "")
  }, ignoreInit = FALSE)
  
  #### ---- Deep Learning Server ----- ###
  source("server/deep_learning.R", local=TRUE)
  deep_learning()
  
  #### ---- Reset various components --------------------------------------####
  ## Various components come before this
  source("server/resets.R", local = TRUE)

  ##### ---- Reset on delete or language change ------------------- ####
  reset_data_server()

  #### ---- Activate required fields --------------------------------------####
  iv$enable()
  iv_url$enable()
  iv_ml$enable()

  waiter_hide()
  
}
