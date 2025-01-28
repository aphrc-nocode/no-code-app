library(Rautoml)
options(shiny.maxRequestSize=300*1024^2)
source("R/shinyutilities.R")

function(input, output, session) {
  #### ---- Input validators ---------------------------------------------------
  source("server/input_validators.R")

  #### ---- Create needed folders for datasets and logs -------------------------
  source("server/create_dirs.R")
  
  #### ---- Placeholder for reactive values ------------------------------------
  ##### --------- Currently selected dataset --------------------------
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
	)
 

  #### ---- App title ----------------------------------------------------
  source("server/header_footer_configs.R", local=TRUE)
  app_title()
  
  ###-------App Footer--------------------------
  footer_language_translation()
  ###-------Menu Translate---------
  
  menu_translation()

  #### ---- Change language ----------------------------------------------------
  source("server/change_language.R", local = TRUE)
  output$change_language = change_language
  
  #### Extracts language specific labels
  get_rv_labels = function(var) {
    get_rv_labels_base(rv_lang$labelling_file_df, var)
  }
  
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


  ##### ---- Identify matching colums ------------------------------------------####
  combine_data_match_columns()

  #### ---- Reset various components --------------------------------------####
  ## Various components come before this
  source("server/resets.R", local = TRUE)

  ##### ---- Reset on delete or language change ------------------- ####
  reset_data_server()

  #### ---- Activate required fields --------------------------------------####
  iv$enable()
  iv_url$enable()
  
  
}



