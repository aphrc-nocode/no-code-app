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
  output$menu_home = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_home"), "</span>"))
  })
  output$menu_source_data = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_source_data"), "</span>"))
  })
  output$menu_manage_data = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_manage_data"), "</span>"))
  })
  output$menu_overview = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_overview"), "</span>"))
  })
  output$menu_explore = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_explore"), "</span>"))
  })
  output$menu_transform = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_transform"), "</span>"))
  })
  output$menu_combine_data = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_combine_data"), "</span>"))
  })
  output$menu_add_rows = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_add_rows"), "</span>"))
  })
  output$menu_visualize_data = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_visualize_data"), "</span>"))
  })
  
  output$menu_visualize_numeric = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_visualize_numeric"), "</span>"))
  })
  
  output$menu_visualize_categorical = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_visualize_categorical"), "</span>"))
  })
  
  output$menu_machine_learning = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_machine_learning"), "</span>"))
  })
  
  output$menu_partition_data = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_partition_data"), "</span>"))
  })
  
  output$menu_feature_engineering = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_feature_engineering"), "</span>"))
  })
  
  output$menu_train_model = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_train_model"), "</span>"))
  })
  
  output$menu_validate_model = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_validate_model"), "</span>"))
  })
  output$menu_predict = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_predict"), "</span>"))
  })
  
  output$menu_additional_resources = renderUI({
    HTML(paste0("<span class='menu-label'>", get_rv_labels("menu_additional_resources"), "</span>"))
  })
  
  
}








