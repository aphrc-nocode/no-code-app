app_title = function() {
	output$app_title = renderUI({
		h3(get_rv_labels("app_title"), style = "text-align: center;")
	})
}


#Footer Language convertion
footer_language_translation = function() {
  output$app_footer_title = renderUI({
    h4(get_rv_labels("footer_org_name"))
  })

  output$app_footer_contact = renderUI({
    h4(HTML(paste0('<a href="mailto:example@aphrc.org" style="color: white;">',get_rv_labels("footer_contact"), ': example@aphrc.org</a>')))
  })

  output$app_footer_all_rights = renderUI({
    h4(get_rv_labels("footer_org_name"))
  })

  output$app_footer_all_rights = renderUI({
    h4(paste0(get_rv_labels("footer_copyright"), " © ", format(Sys.Date(), "%Y"), ", ", get_rv_labels("footer_all_rights")), style = "text-align: right;")
  })
}

#Menu and submenu conversion


menu_translation = function(){
  output$dynamic_meinu_aphrc <- renderMenu({
    sidebarMenu(id = "tabs",
      menuItem(text =  HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_home"), "</span>")), tabName = "homePage", icon = icon("house"), selected = TRUE),
      menuItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_source_data"), "</span>")), tabName = "sourcedata", icon = icon("file-import", lib = "font-awesome")),
      menuItem(
        text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_manage_data"), "</span>")), tabName = "manageData", icon = icon("glyphicon glyphicon-tasks", lib = "glyphicon"),
        menuSubItem(text =  HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_overview"), "</span>")), tabName = "Overview", icon = icon("table-columns", lib = "font-awesome")),
        menuSubItem(text =  HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_explore"), "</span>")), tabName = "Explore", icon = icon("object-ungroup", lib = "font-awesome")),
        menuSubItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_transform"), "</span>")), tabName = "Transform", icon = icon("table-columns", lib = "font-awesome")),
        menuSubItem(text =  HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_combine_data"), "</span>")), tabName = "combineData", icon = icon("table-columns", lib = "font-awesome"))
      ),
        menuItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_visualize_data"), "</span>")), tabName = "visualizeData", icon = icon("glyphicon glyphicon-stats", lib = "glyphicon"),
        menuItem(text =  HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_summarizeAutomatic"), "</span>")), tabName = "summarizeAutomatic", icon = icon("glyphicon glyphicon-stats", lib = "glyphicon")
           
      ),
      menuItem(text =  HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_summarizeCustom"), "</span>")), tabName = "summarizeCustom", icon = icon("chart-line"))),
      
        
      menuItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_research_question"), "</span>")), tabName = "researchQuestions", icon = icon("file-import", lib = "font-awesome"), selected = FALSE),


		menuItem(
        text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_machine_learning"), "</span>")), tabName = "machineLearning", icon = icon("code-merge", lib = "font-awesome"),
        menuSubItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_setup_models"), "</span>")), tabName = "setupModels", icon = icon("arrows-split-up-and-left", lib = "font-awesome")),
        menuSubItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_feature_engineering"), "</span>")), tabName = "featureEngineering", icon = icon("sitemap", lib = "font-awesome")),
        menuSubItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_train_model"), "</span>")), tabName = "trainModel", icon = icon("gear", lib = "font-awesome")),
        menuSubItem(text =HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_validate_model"), "</span>")), tabName = "validateDeployModel", icon = icon("server", lib = "font-awesome")),
        menuSubItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_predict"), "</span>")), tabName = "predictClassify", icon = icon("layer-group", lib = "font-awesome"))
      ),
		  menuItem(text =  HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_omop_data"), "</span>")), tabName = "omopAnalysis", icon = icon("magnifying-glass-chart", lib = "font-awesome"),
		  menuSubItem(text =HTML(paste0("<span class='menu-label'>", get_rv_labels("evidenceQuality"), "</span>")), tabName = "evidenceQuality", icon = icon("server", lib = "font-awesome")),
		  #menuSubItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("cohortConstruction"), "</span>")), tabName = "cohortConstruction", icon = icon("layer-group", lib = "font-awesome")),
		  menuSubItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_achilles"), "</span>")), tabName = "achilles", icon = icon("glyphicon glyphicon-stats", lib = "glyphicon")),
		  menuSubItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_omop"), "</span>")), tabName = "omop_visualizations", icon = icon("glyphicon glyphicon-stats", lib = "glyphicon")),
		  menuSubItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_Cohort_Constructor"), "</span>")), tabName = "CohortConstructor", icon = icon("arrows-split-up-and-left", lib = "font-awesome")),
		  menuSubItem(text = HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_Feature_Extraction"), "</span>")), tabName = "FeatureExtraction", icon = icon("arrows-split-up-and-left", lib = "font-awesome"))
		  
		  
		  ),
		menuItem(
		  HTML(paste0("<span class='menu-label'>", get_rv_labels("deeplearning"), "</span>")),
		  tabName = "deeplearning",
		  icon = icon("gear"),
		  
		  # --- CNN Deep section with its own submenus ---
		  menuItem(
		    HTML(paste0("<span class='menu-label'>", get_rv_labels("cnn_deep"), "</span>")),
		    tabName = "cnntransformers",
		    icon = icon("gear", lib = "font-awesome"),
		    
		    menuSubItem(get_rv_labels("dashboard"), tabName = "dashboard", icon = icon("dashboard")),
		    menuSubItem(get_rv_labels("c_pipeline"), tabName = "create", icon = icon("plus-circle")),
		    menuSubItem(get_rv_labels("t_model"), tabName = "train", icon = icon("cog")),
		    menuSubItem(get_rv_labels("m_predictions"), tabName = "predict", icon = icon("eye")),
		    menuSubItem(get_rv_labels("v_jobs"), tabName = "jobs", icon = icon("list")),
		    menuSubItem(get_rv_labels("v_datasets"), tabName = "datasets", icon = icon("database")),
		    menuSubItem(get_rv_labels("d_jobs"), tabName = "delete", icon = icon("trash"))
		  ),
		  
		  # --- Transformers Deep ---
		  menuItem(
		    HTML(paste0("<span class='menu-label'>", get_rv_labels("transformers_deep"), "</span>")),
		    tabName = "cnndeep",
		    icon = icon("server", lib = "font-awesome")
		  )
		)
		,
		

		menuItem(HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_additional_resources"), "</span>")), tabName = "addResources", icon = icon("book")),
		br(),
		div(
		  style="display: flex; justify-content: flex-start; gap: 10px; margin-top: 5px;",
		  actionBttn("logoutID", label = get_rv_labels("logoutID"), class = "btn")
		  #is_logged_in(
			# id = app_login_config$APP_ID,login::logout_button(id = "logout", label = "Sign out"))
		)
    
	 )
  })
  
  
}

