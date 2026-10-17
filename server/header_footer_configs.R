app_title = function() {
  output$app_title = renderUI({
    h3(get_rv_labels("app_title"), style = "text-align: center;")
  })
}


## The platform's own contact address; the footer hides "Contact Us" while it is empty
app_contact_email <- ""

#Footer Language convertion
footer_language_translation = function() {
  output$app_footer_title = renderUI({
    # h4(get_rv_labels("footer_org_name"))
    h4(get_rv_labels("app_version"))
  })
  
  output$app_footer_contact = renderUI({
    if (!nzchar(app_contact_email)) return(NULL)
    h4(tags$a(href = paste0("mailto:", app_contact_email), paste0(get_rv_labels("footer_contact"), ": ", app_contact_email)), style = "text-align: center;")
  })
  
  output$app_footer_all_rights = renderUI({
    h4(get_rv_labels("footer_org_name"))
  })
  
  output$app_footer_all_rights = renderUI({
    h4(paste0(get_rv_labels("footer_copyright"), " © ", format(Sys.Date(), "%Y"), ", ", get_rv_labels("footer_all_rights")), style = "text-align: right;")
  })

  ## Tab links reuse the Home tab's [data-home-tab] click handler
  footer_tab_link <- function(tab, key) {
    tags$a(href = "#", `data-home-tab` = tab, icon("angle-right"), get_rv_labels(key))
  }

  output$app_footer_extended = renderUI({
    tags$div(
      class = "footer-extended-inner"
      , tags$div(
        tags$img(src = "brand/automl-full-logo-light-transparent.svg", alt = "AutoML No-Code Platform")
        , tags$p(get_rv_labels("landing_footer_tagline"))
      )
      , tags$div(
        tags$h4(get_rv_labels("landing_footer_platform"))
        , footer_tab_link("sourcedata", "menu_source_data")
        , footer_tab_link("summarizeAutomatic", "menu_visualize_data")
        , footer_tab_link("setupModels", "menu_machine_learning")
        , footer_tab_link("transfer_learning", "menu_transfer_learning")
      )
      , tags$div(
        tags$h4(get_rv_labels("landing_footer_explore"))
        , footer_tab_link("cnndeep", "deeplearning")
        , footer_tab_link("evidenceQuality", "menu_omop_data")
        , footer_tab_link("anonymization_quant", "menu_data_anonymization")
        , footer_tab_link("addResources", "menu_additional_resources")
        , if (nzchar(app_contact_email)) tags$a(href = paste0("mailto:", app_contact_email), icon("envelope"), get_rv_labels("footer_contact"))
      )
    )
  })
}

#Menu and submenu conversion


# Sidebar helper for non-navigating history commands
history_sidebar_command_item <- function(action, label, icon_tag) {
  tags$li(
    class = "history-command-subitem",
    tags$a(
      href = "#",
      role = "button",
      onclick = sprintf(
        "Shiny.setInputValue('history_sidebar_action', {action: '%s', nonce: Date.now()}, {priority: 'event'}); return false;",
        action
      ),
      icon_tag,
      tags$span(class = "menu-label", label)
    )
  )
}

menu_translation = function(){
  output$dynamic_meinu_aphrc <- renderMenu({
    sidebarMenu(id = "tabs",
                tags$li(
                  class = "aphrc-toggle-item",
                  tags$a(
                    href = "#",
                    id   = "sidebar_toggle_menu",
                    role = "button",
                    
                    # Excel labels (use your keys)
                    `data-label-collapse` = get_rv_labels("sidebar_toggle_collapse_menu"),
                    `data-label-expand`   = get_rv_labels("sidebar_toggle_expand_menu"),
                    
                    # accessibility
                    `aria-label`    = get_rv_labels("sidebar_toggle_collapse_menu"),
                    `aria-expanded` = "true",
                    
                    # icon + label
                    tags$i(class = "fa fa-times aphrc-icon-expanded"),
                    tags$i(class = "fa fa-bars  aphrc-icon-collapsed"),
                    tags$span(
                      class = "menu-label aphrc-toggle-label",
                      get_rv_labels("sidebar_toggle_collapse_menu")
                    )
                  )
                ),
                menuItem(text =  HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_home"), "</span>")), tabName = "homePage", icon = icon("house"), selected = TRUE),
                menuItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_source_data"), "</span>")), tabName = "sourcedata", icon = icon("file-import", lib = "font-awesome")),
                menuItem(
                  text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_manage_data"), "</span>")), tabName = "manageData", icon = icon("glyphicon glyphicon-tasks", lib = "glyphicon"),
                  menuSubItem(text =  HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_overview"), "</span>")), tabName = "Overview", icon = icon("table-columns", lib = "font-awesome")),
                  menuSubItem(text =  HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_explore"), "</span>")), tabName = "Explore", icon = icon("object-ungroup", lib = "font-awesome")),
                  menuSubItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_transform"), "</span>")), tabName = "Transform", icon = icon("table-columns", lib = "font-awesome")),
                  menuSubItem(text =  HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_combine_data"), "</span>")), tabName = "combineData", icon = icon("table-columns", lib = "font-awesome"))
                ),
                menuItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_visualize_data"), "</span>")), tabName = "visualizeData", icon = icon("glyphicon glyphicon-stats", lib = "glyphicon"),
                         menuItem(text =  HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_summarizeAutomatic"), "</span>")), tabName = "summarizeAutomatic", icon = icon("glyphicon glyphicon-stats", lib = "glyphicon")
                                  
                         ),
                         menuItem(text =  HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_summarizeCustom"), "</span>")), tabName = "summarizeCustom", icon = icon("chart-line"))),
                
                menuItem(text =  HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_omop_data"), "</span>")), tabName = "omopAnalysis", icon = icon("magnifying-glass-chart", lib = "font-awesome"),
                         menuSubItem(text =HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("evidenceQuality"), "</span>")), tabName = "evidenceQuality", icon = icon("server", lib = "font-awesome")),
                         #menuSubItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("cohortConstruction"), "</span>")), tabName = "cohortConstruction", icon = icon("layer-group", lib = "font-awesome")),
                         menuSubItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_achilles"), "</span>")), tabName = "achilles", icon = icon("glyphicon glyphicon-stats", lib = "glyphicon")),
                         menuSubItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_omop"), "</span>")), tabName = "omop_visualizations", icon = icon("glyphicon glyphicon-stats", lib = "glyphicon")),
                         menuSubItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_Cohort_Constructor"), "</span>")), tabName = "CohortConstructor", icon = icon("arrows-split-up-and-left", lib = "font-awesome")),
                         menuSubItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_Feature_Extraction"), "</span>")), tabName = "FeatureExtraction", icon = icon("arrows-split-up-and-left", lib = "font-awesome"))
                         
                         
                ),
                
                menuItem(
                  text = HTML(
                    paste0(
                      "<span class='menu-label'>",
                      get_rv_labels("menu_data_anonymization"),
                      "</span>"
                    )
                  ),
                  tabName = "anonymization",
                  icon = icon("user-shield", lib = "font-awesome"),
                  
                  menuSubItem(
                    text = HTML(
                      paste0(
                        "<span class='menu-label'>",
                        get_rv_labels("menu_quant_anonymization"),
                        "</span>"
                      )
                    ),
                    tabName = "anonymization_quant",
                    icon = icon("calculator")
                  ),
                  
                  menuSubItem(
                    text = HTML(
                      paste0(
                        "<span class='menu-label'>",
                        get_rv_labels("menu_qual_anonymization"),
                        "</span>"
                      )
                    ),
                    tabName = "anonymization_qual",
                    icon = icon("align-left")
                  )
                ),
                
                menuItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_research_question"), "</span>")), tabName = "researchQuestions", icon = icon("file-import", lib = "font-awesome"), selected = FALSE),
                
                
                menuItem(
                  text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_machine_learning"), "</span>")), tabName = "machineLearning", icon = icon("code-merge", lib = "font-awesome"),
                  menuSubItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_setup_models"), "</span>")), tabName = "setupModels", icon = icon("arrows-split-up-and-left", lib = "font-awesome")),
                  menuSubItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_feature_engineering"), "</span>")), tabName = "featureEngineering", icon = icon("sitemap", lib = "font-awesome")),
                  menuSubItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_train_model"), "</span>")), tabName = "trainModel", icon = icon("gear", lib = "font-awesome")),
                  menuSubItem(text =HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_validate_model"), "</span>")), tabName = "validateDeployModel", icon = icon("server", lib = "font-awesome")),
                  menuSubItem(text = HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_predict"), "</span>")), tabName = "predictClassify", icon = icon("layer-group", lib = "font-awesome")),
                  menuSubItem(
                    text = HTML(paste0(
                      "<span class='menu-label'>",
                      {
                        x <- get_rv_labels("menu_transfer_learning")
                        if (is.null(x) || length(x) == 0 || !nzchar(x)) "Transfer Learning" else x
                      },
                      "</span>"
                    )),
                    tabName = "transfer_learning",
                    icon = icon("exchange-alt", lib = "font-awesome")
                  )
                ),
                
                menuItem(
                  HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("deeplearning"), "</span>")),
                  tabName = "deeplearning",
                  icon = icon("gear"),
                  
                  # # --- CNN Deep section with its own submenus ---
                  # menuItem(
                  #   HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("cnn_deep"), "</span>")),
                  #   tabName = "cnntransformers",
                  #   icon = icon("gear", lib = "font-awesome"),
                  #   
                  #   menuSubItem(get_rv_labels("dashboard"), tabName = "dashboard", icon = icon("dashboard")),
                  #   menuSubItem(get_rv_labels("c_pipeline"), tabName = "create", icon = icon("plus-circle")),
                  #   menuSubItem(get_rv_labels("t_model"), tabName = "train", icon = icon("cog")),
                  #   menuSubItem(get_rv_labels("m_predictions"), tabName = "predict", icon = icon("eye")),
                  #   menuSubItem(get_rv_labels("v_jobs"), tabName = "jobs", icon = icon("list")),
                  #   menuSubItem(get_rv_labels("v_datasets"), tabName = "datasets", icon = icon("database")),
                  #   menuSubItem(get_rv_labels("d_jobs"), tabName = "delete", icon = icon("trash"))
                  #),
                  
                  # --- Transformers Deep ---
                  menuItem(
                    HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("transformers_deep"), "</span>")),
                    tabName = "cnndeep",
                    icon = icon("server", lib = "font-awesome")
                  )
                ),
                menuItem(
                  HTML(paste0("<span class='menu-label'>", get_rv_labels("action_buttons"), "</span>")),
                  icon = icon("clock-rotate-left", lib = "font-awesome"),
                  startExpanded = FALSE,
                  
                  history_sidebar_command_item("undo",  get_rv_labels("undo"),  icon("rotate-left", lib = "font-awesome")),
                  history_sidebar_command_item("redo",  get_rv_labels("redo"),  icon("rotate-right", lib = "font-awesome")),
                  history_sidebar_command_item("reset", get_rv_labels("reset"), icon("arrows-rotate", lib = "font-awesome")),
                  
                  history_sidebar_command_item("save",  get_rv_labels("save"),  icon("floppy-disk", lib = "font-awesome"))
                ),
                menuItem(HTML(paste0("<span class=\'menu-label\'>", get_rv_labels("menu_additional_resources"), "</span>")), tabName = "addResources", icon = icon("book")),

                if (isTRUE(isolate(USER$username) %in% c("scygu@aphrc.org", "guest565@aphrc.org"))) {
                  menuItem(get_rv_labels("admin_menu_label"), tabName = "adminPanel", icon = icon("shield-halved"))
                },

                br(),
                div(
                  class = "logout-container",
                  actionBttn(
                    inputId = "logoutID",
                    label = HTML(
                      paste0(
                        "<span class='logout-inner'>",
                        "<i class='fa fa-right-from-bracket logout-icon'></i>",
                        "<span class='logout-label'>", get_rv_labels("logoutID"), "</span>",
                        "</span>"
                      )
                    ),
                    class = "btn",
                    title = get_rv_labels("logoutID")   # <- tooltip text
                  )
                )
                
    )
  })
  
  
}
