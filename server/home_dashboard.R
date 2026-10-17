#### ---- Dashboard home ---- ####

home_parse_date <- function(x) {
  x <- substr(as.character(x), 1, 10)
  out <- as.Date(x, format = "%d-%m-%Y")
  iso <- is.na(out)
  out[iso] <- as.Date(x[iso], format = "%Y-%m-%d")
  out
}

home_deletion_date <- function(uploaded, months = app_privacy_config$retention_months) {
  out <- rep(as.Date(NA), length(uploaded))
  ok <- !is.na(uploaded)
  if (any(ok)) {
    out[ok] <- do.call(c, lapply(uploaded[ok], function(d) {
      seq(d, by = paste(months, "months"), length.out = 2)[2]
    }))
  }
  out
}

## English text for Home labels that are not in labelling_file.xlsx yet
home_label_fallbacks <- c(
  home_steps_subtitle = "Four steps take you from raw data to a deployed model."
  , home_step_source_desc = "Load a file from your computer or connect to a database."
  , home_step_explore_desc = "Check data quality, summaries and charts before modelling."
  , home_step_train_desc = "Pick algorithms, tune them and compare the results."
  , home_step_deploy_desc = "Publish your best model so it can serve predictions."
)

home_dashboard_server <- function(USER) {
  home_label <- function(key, ...) {
    value <- get_rv_labels(key)
    if (length(value) != 1 || is.na(value)) value <- ""
    if (!nzchar(value) && key %in% names(home_label_fallbacks)) value <- home_label_fallbacks[[key]]
    tokens <- c(list(months = app_privacy_config$retention_months), list(...))
    for (token in names(tokens)) {
      value <- gsub(paste0("{", token, "}"), tokens[[token]], value, fixed = TRUE)
    }
    value
  }

  count_models <- function() {
    caret <- length(list.files(file.path(app_username, ".log_files"), pattern = "-trained\\.model\\.main\\.log$"))
    index <- file.path(app_username, "logs", "models", "index.csv")
    pycaret <- if (file.exists(index)) NROW(tryCatch(utils::read.csv(index), error = function(e) NULL)) else 0
    caret + pycaret
  }

  has_outputs <- function() {
    length(list.files(file.path(app_username, "outputs"), recursive = TRUE)) > 0
  }

  home_datasets <- reactive({
    logs <- rv_metadata$upload_logs
    if (!NROW(logs) || !"file_name" %in% names(logs)) return(NULL)
    uploaded <- home_parse_date(logs$upload_time)
    study <- if ("study_name" %in% names(logs)) as.character(logs$study_name) else rep(NA_character_, NROW(logs))
    data.frame(
      name = ifelse(is.na(study) | !nzchar(trimws(study)), as.character(logs$file_name), study)
      , size = paste(logs$observations, "\u00d7", logs$features)
      , uploaded = uploaded
      , deletion = home_deletion_date(uploaded)
      , stringsAsFactors = FALSE
    )
  })

  output$home_dashboard <- renderUI({
    # Model files and visualization outputs are not reactive; returning to
    # the Home tab re-reads them.
    input$tabs
    datasets <- home_datasets()
    n_models <- count_models()
    is_new <- NROW(datasets) == 0
    ## needs: index of the step whose result this step depends on
    steps <- list(
      list(label = home_label("home_step_source"), desc = home_label("home_step_source_desc"), tab = "sourcedata", done = NROW(datasets) > 0)
      , list(label = home_label("home_step_explore"), desc = home_label("home_step_explore_desc"), tab = "summarizeAutomatic", done = has_outputs()
        , needs = 1, locked_hint = home_label("home_step_locked_source"))
      , list(label = home_label("home_step_train"), desc = home_label("home_step_train_desc"), tab = "setupModels", done = n_models > 0
        , needs = 1, locked_hint = home_label("home_step_locked_source"))
      , list(label = home_label("home_step_deploy"), desc = home_label("home_step_deploy_desc"), tab = "validateDeployModel"
        , done = NROW(rv_deploy_models$deployed_models_table) > 0
        , needs = 3, locked_hint = home_label("home_step_locked_train"))
    )
    home_dashboard_view(
      name = USER$first_name
      , steps = steps
      , datasets = datasets
      , n_models = n_models
      , label = home_label
      , is_new = is_new
    )
  })

  output$home_recent <- renderUI({
    home_recent_view(home_datasets(), home_label)
  })

  output$home_components <- renderUI({
    home_components_view()
  })

  observeEvent(input$home_go, {
    updateTabItems(session, "tabs", input$home_go)
  })
}
