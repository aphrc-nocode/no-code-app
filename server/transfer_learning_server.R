transfer_learning_server <- function(id, rv_current, rv_ml_ai, app_username, api_base = NULL) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    `%||%` <- function(a, b) {
      if (!is.null(a)) {
        if (is.character(a)) { if (length(a) > 0 && nzchar(a[1])) return(a) }
        else { return(a) }
      }
      b
    }

    clean_metric_name <- function(x) {
      dplyr::case_when(
        x %in% c("roc_auc", "auc")               ~ "ROC AUC",
        x %in% c("accuracy", "acc")               ~ "Accuracy",
        x %in% c("sens", "sensitivity", "recall") ~ "Sensitivity",
        x %in% c("spec", "specificity")           ~ "Specificity",
        x %in% c("precision", "prec")             ~ "Precision",
        x %in% c("f_meas", "f1", "f1_score")      ~ "F1 Score",
        TRUE                                       ~ as.character(x)
      )
    }

    get_prob_col <- function(preds_prob, positive_level) {
      candidate <- paste0(".pred_", positive_level)
      if (candidate %in% names(preds_prob)) return(candidate)
      prob_cols <- setdiff(names(preds_prob)[grepl("^\\.pred_", names(preds_prob))], ".pred_class")
      if (length(prob_cols) == 0) return(NULL)
      prob_cols[1]
    }

    # Some model backends (esp. caret `train` objects with na.action = na.omit, or
    # recipes with a na-removal step) silently drop rows with missing predictor
    # values, returning a prediction vector shorter than nrow(predictor_df). Left
    # unguarded, that blows up downstream with "`.predicted_numeric` must be size
    # N or 1, not <shorter N>" and takes the whole reactive down. Realign to the
    # full row count here -- dropped rows come back as NA and flow through
    # cleanly, since the metric functions already drop NA before computing.
    align_predictions <- function(v, predictor_df) {
      n <- nrow(predictor_df)
      if (is.null(v) || length(v) == n) return(v)
      out <- rep(NA_real_, n)
      nm  <- names(v)
      idx <- suppressWarnings(as.integer(nm))
      if (!is.null(nm) && !anyNA(idx) && length(idx) == length(v) && all(idx >= 1 & idx <= n)) {
        out[idx] <- as.numeric(v)
      } else {
        complete_idx <- which(stats::complete.cases(predictor_df))
        if (length(complete_idx) == length(v)) out[complete_idx] <- as.numeric(v)
      }
      out
    }

    align_predictions_df <- function(df, predictor_df) {
      if (is.null(df) || nrow(df) == nrow(predictor_df)) return(df)
      out <- as.data.frame(lapply(df, align_predictions, predictor_df = predictor_df))
      names(out) <- names(df)
      out
    }

    make_pred_class <- function(prob, outcome_levels, threshold = 0.5) {
      factor(ifelse(prob >= threshold, outcome_levels[2], outcome_levels[1]), levels = outcome_levels)
    }

    make_calibration_df <- function(df, truth_col, prob_col, positive_level, bins = 10) {
      df %>%
        dplyr::transmute(truth = .data[[truth_col]], prob = .data[[prob_col]]) %>%
        dplyr::mutate(truth_num = ifelse(truth == positive_level, 1, 0), bin = dplyr::ntile(prob, bins)) %>%
        dplyr::group_by(bin) %>%
        dplyr::summarise(mean_pred = mean(prob, na.rm = TRUE), observed = mean(truth_num, na.rm = TRUE),
                         n = dplyr::n(), .groups = "drop")
    }

    compute_source_metrics <- function(scored_df, outcome_var, prob_col) {
      metric_once <- function(dat) {
        dplyr::bind_rows(
          yardstick::roc_auc(dat,   truth = !!rlang::sym(outcome_var), !!rlang::sym(prob_col), event_level = "second"),
          yardstick::accuracy(dat,  truth = !!rlang::sym(outcome_var), estimate = predicted_class),
          yardstick::sens(dat,      truth = !!rlang::sym(outcome_var), estimate = predicted_class, event_level = "second"),
          yardstick::spec(dat,      truth = !!rlang::sym(outcome_var), estimate = predicted_class, event_level = "second"),
          yardstick::precision(dat, truth = !!rlang::sym(outcome_var), estimate = predicted_class, event_level = "second"),
          yardstick::f_meas(dat,    truth = !!rlang::sym(outcome_var), estimate = predicted_class, event_level = "second")
        )
      }
      point_est <- metric_once(scored_df)
      set.seed(123)
      boot <- purrr::map_dfr(seq_len(100), function(i) {
        idx <- sample(seq_len(nrow(scored_df)), replace = TRUE)
        tryCatch(metric_once(scored_df[idx, , drop = FALSE]), error = function(e) NULL)
      })
      ci_tbl <- boot %>%
        dplyr::group_by(.metric) %>%
        dplyr::summarise(lower = stats::quantile(.estimate, 0.025, na.rm = TRUE),
                         upper = stats::quantile(.estimate, 0.975, na.rm = TRUE), .groups = "drop")
      point_est %>%
        dplyr::left_join(ci_tbl, by = ".metric") %>%
        dplyr::mutate(model = "Source Model", metric = clean_metric_name(.metric),
                      estimate = round(.estimate, 4), lower = round(lower, 4), upper = round(upper, 4)) %>%
        dplyr::select(model, metric, estimate, lower, upper)
    }

    compute_scored_metrics <- function(scored_df, outcome_var, model_label, n_boot = 100) {
      metric_once <- function(dat) {
        dplyr::bind_rows(
          yardstick::roc_auc(dat,   truth = !!rlang::sym(outcome_var), probability, event_level = "second"),
          yardstick::accuracy(dat,  truth = !!rlang::sym(outcome_var), estimate = predicted_class),
          yardstick::sens(dat,      truth = !!rlang::sym(outcome_var), estimate = predicted_class, event_level = "second"),
          yardstick::spec(dat,      truth = !!rlang::sym(outcome_var), estimate = predicted_class, event_level = "second"),
          yardstick::precision(dat, truth = !!rlang::sym(outcome_var), estimate = predicted_class, event_level = "second"),
          yardstick::f_meas(dat,    truth = !!rlang::sym(outcome_var), estimate = predicted_class, event_level = "second")
        )
      }
      point_est <- tryCatch(metric_once(scored_df), error = function(e) NULL)
      if (is.null(point_est)) return(NULL)
      set.seed(123)
      boot <- purrr::map_dfr(seq_len(n_boot), function(i) {
        idx <- sample(seq_len(nrow(scored_df)), replace = TRUE)
        tryCatch(metric_once(scored_df[idx, , drop = FALSE]), error = function(e) NULL)
      })
      ci_tbl <- boot %>%
        dplyr::group_by(.metric) %>%
        dplyr::summarise(lower = stats::quantile(.estimate, 0.025, na.rm = TRUE),
                         upper = stats::quantile(.estimate, 0.975, na.rm = TRUE), .groups = "drop")
      point_est %>%
        dplyr::left_join(ci_tbl, by = ".metric") %>%
        dplyr::mutate(model = model_label, metric = clean_metric_name(.metric),
                      estimate = round(.estimate, 4), lower = round(lower, 4), upper = round(upper, 4)) %>%
        dplyr::select(model, metric, estimate, lower, upper)
    }

    compute_regression_metrics <- function(actual, predicted, model_label, n_boot = 100) {
      actual <- as.numeric(actual); predicted <- as.numeric(predicted)
      ok <- !is.na(actual) & !is.na(predicted)
      actual <- actual[ok]; predicted <- predicted[ok]
      metric_once <- function(a, p) {
        rmse_v <- sqrt(mean((a - p)^2))
        mae_v  <- mean(abs(a - p))
        ss_res <- sum((a - p)^2); ss_tot <- sum((a - mean(a))^2)
        r2_v   <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
        mape_v <- mean(abs((a - p) / a)[is.finite(abs((a - p) / a))]) * 100
        data.frame(metric = c("RMSE", "MAE", "R²", "MAPE (%)"),
                   estimate = c(rmse_v, mae_v, r2_v, mape_v), stringsAsFactors = FALSE)
      }
      point_est <- metric_once(actual, predicted)
      set.seed(123)
      boot <- purrr::map_dfr(seq_len(n_boot), function(i) {
        idx <- sample(seq_along(actual), replace = TRUE)
        tryCatch(metric_once(actual[idx], predicted[idx]), error = function(e) NULL)
      })
      ci_tbl <- boot %>% dplyr::group_by(metric) %>%
        dplyr::summarise(lower = stats::quantile(estimate, 0.025, na.rm = TRUE),
                         upper = stats::quantile(estimate, 0.975, na.rm = TRUE), .groups = "drop")
      point_est %>% dplyr::left_join(ci_tbl, by = "metric") %>%
        dplyr::mutate(model = model_label, estimate = round(estimate, 4),
                      lower = round(lower, 4), upper = round(upper, 4)) %>%
        dplyr::select(model, metric, estimate, lower, upper)
    }

    get_regression_preds <- function(fit_obj, predictor_df) {
      last_regression_pred_error(NULL)
      errs <- character()
      pred_types <- if (inherits(fit_obj, "train")) "raw" else c("raw", "numeric")
      for (tp in pred_types) {
        result <- tryCatch({
          preds <- as.data.frame(predict_source_model_any(fit_obj, predictor_df, tp))
          vals  <- suppressWarnings(as.numeric(preds[[1]]))
          if (!all(is.na(vals))) return(vals)
          errs <- c(errs, sprintf("type='%s': predictions coerced to all-NA (got class %s, cols: %s)",
                                    tp, class(preds[[1]])[1], paste(names(preds), collapse = ", ")))
          NULL
        }, error = function(e) {
          errs <<- c(errs, sprintf("type='%s': %s", tp, conditionMessage(e)))
          NULL
        })
        if (!is.null(result)) return(result)
      }
      final <- tryCatch(align_predictions(as.numeric(predict(fit_obj, newdata = as.data.frame(predictor_df))), predictor_df),
                         error = function(e) {
                           errs <<- c(errs, paste0("fallback predict(): ", conditionMessage(e)))
                           NULL
                         })
      if (!is.null(final) && !all(is.na(final))) return(final)
      if (!is.null(final)) errs <- c(errs, "fallback predict(): returned all-NA")
      last_regression_pred_error(paste(errs, collapse = " | "))
      NULL
    }

    # -------------------------------------------------------------------------
    # Status reactiveVals
    # -------------------------------------------------------------------------

    source_scoring_status  <- reactiveVal("idle")
    compare_scoring_status <- reactiveVal("idle")
    finetune_status        <- reactiveVal("idle")
    last_regression_pred_error <- reactiveVal(NULL)

    source_scoring_error_detail <- reactiveVal(NULL)
    finetune_error_detail       <- reactiveVal(NULL)

    make_alert_ui <- function(s, running_msg, done_msg, error_msg, detail = NULL) {
      if (s == "running") tags$div(class = "alert alert-info",   tags$i(class = "fa fa-spinner fa-spin"), " ", running_msg)
      else if (s == "done")  tags$div(class = "alert alert-success", tags$i(class = "fa fa-check"), " ", done_msg)
      else if (s == "error") tags$div(class = "alert alert-danger",  tags$i(class = "fa fa-times"), " ", error_msg,
                                       if (!is.null(detail) && nzchar(detail)) tags$div(tags$small(tags$em(detail))))
    }

    output$source_scoring_status_ui <- renderUI({
      make_alert_ui(source_scoring_status(),
                    "Running source model on target data, please wait...",
                    "Source model scoring complete.",
                    "An error occurred during source model scoring.",
                    detail = source_scoring_error_detail())
    })
    outputOptions(output, "source_scoring_status_ui", suspendWhenHidden = FALSE)

    output$compare_scoring_status_ui <- renderUI({
      make_alert_ui(compare_scoring_status(),
                    "Scoring retrained models and computing bootstrap CIs, please wait...",
                    "Comparison complete.",
                    "An error occurred during model comparison.")
    })
    outputOptions(output, "compare_scoring_status_ui", suspendWhenHidden = FALSE)

    output$finetune_status_ui <- renderUI({
      make_alert_ui(finetune_status(),
                    "Fine-tuning model, please wait...",
                    "Fine-tuning complete.",
                    "Fine-tuning failed.",
                    detail = finetune_error_detail())
    })
    outputOptions(output, "finetune_status_ui", suspendWhenHidden = FALSE)

    # -------------------------------------------------------------------------
    # Data gate
    # -------------------------------------------------------------------------

    data_ready <- reactive({
      !is.null(rv_current$working_df) && is.data.frame(rv_current$working_df) && nrow(rv_current$working_df) > 0
    })

    output$data_ready_flag <- reactive({ isTRUE(data_ready()) })
    outputOptions(output, "data_ready_flag", suspendWhenHidden = FALSE)

    # Result-ready flags — drive conditional download button visibility
    output$source_ready   <- reactive({ tryCatch(!is.null(source_only_results()),  error = function(e) FALSE) })
    output$subgroup_ready <- reactive({ tryCatch(!is.null(subgroup_performance()), error = function(e) FALSE) })
    output$finetune_ready <- reactive({ tryCatch(!is.null(finetuned_results()),    error = function(e) FALSE) })
    output$compare_ready  <- reactive({ tryCatch(!is.null(compare_results()),      error = function(e) FALSE) })
    output$fi_ready        <- reactive({ tryCatch(!is.null(feature_importance_results()), error = function(e) FALSE) })
    output$shap_ready      <- reactive({ tryCatch(!is.null(shap_results()),               error = function(e) FALSE) })
    # Single-model-selected flag -- drives whether the primary-model metrics table
    # shows on its own, or defers entirely to the multi-model comparison table below it.
    output$source_single_selected <- reactive({ length(unique(input$tl_selected_model_id %||% character())) <= 1 })
    outputOptions(output, "source_ready",   suspendWhenHidden = FALSE)
    outputOptions(output, "subgroup_ready", suspendWhenHidden = FALSE)
    outputOptions(output, "finetune_ready", suspendWhenHidden = FALSE)
    outputOptions(output, "compare_ready",  suspendWhenHidden = FALSE)
    outputOptions(output, "fi_ready",       suspendWhenHidden = FALSE)
    outputOptions(output, "shap_ready",     suspendWhenHidden = FALSE)
    outputOptions(output, "source_single_selected", suspendWhenHidden = FALSE)

    # -------------------------------------------------------------------------
    # Trained model logs
    # -------------------------------------------------------------------------

    trained_models_log <- reactive({
      log_path <- paste0(app_username, "/.log_files")
      shiny::validate(shiny::need(
        isTRUE(Rautoml::check_logs(path = log_path, pattern = "-trained.model.main.log")),
        "No trained model logs found. Please train a model first."
      ))
      df <- Rautoml::collect_logs(path = log_path, pattern = "-trained.model.main.log")
      shiny::validate(shiny::need(!is.null(df) && nrow(df) > 0, "No trained model records found."))
      df
    })

    # -------------------------------------------------------------------------
    # Step 1 — cascade selectors
    # -------------------------------------------------------------------------

    output$tl_dataset_ui <- renderUI({
      df              <- trained_models_log()
      dataset_choices <- Rautoml::extract_value_labels(df, "dataset_id")
      selectInput(ns("tl_dataset_id"), "Dataset trained on", choices = dataset_choices,
                  selected = if (!is.null(rv_current$dataset_id) && rv_current$dataset_id %in% dataset_choices)
                    rv_current$dataset_id else dataset_choices[1], width = "100%")
    })

    filtered_by_dataset <- reactive({
      req(input$tl_dataset_id)
      df <- trained_models_log()
      df <- df[df$dataset_id %in% input$tl_dataset_id, , drop = FALSE]
      shiny::validate(shiny::need(nrow(df) > 0, "No trained models found for selected dataset."))
      df
    })

    output$tl_outcome_ui <- renderUI({
      df <- filtered_by_dataset(); req(rv_current$working_df)
      outcome_col  <- if ("outcome" %in% names(df)) "outcome" else if ("target" %in% names(df)) "target" else NULL
      log_outcomes <- if (!is.null(outcome_col)) unique(as.character(df[[outcome_col]])) else names(rv_current$working_df)
      valid_outcomes <- intersect(log_outcomes, names(rv_current$working_df))
      if (length(valid_outcomes) == 0) valid_outcomes <- names(rv_current$working_df)
      selectInput(ns("tl_outcome"), "Outcome trained on", choices = valid_outcomes, selected = valid_outcomes[1], width = "100%")
    })

    output$tl_session_ui <- renderUI({
      df <- filtered_by_dataset()
      selectInput(ns("tl_session_name"), "Training session",
                  choices = Rautoml::extract_value_labels(df, "session_name"), selected = NULL, width = "100%")
    })

    output$tl_metric_ui <- renderUI({
      df      <- filtered_by_dataset()
      choices <- Rautoml::extract_value_labels(df, "metric")
      selectInput(ns("tl_metric"), "Metric", choices = choices, selected = choices[1], width = "100%")
    })

    filtered_trained_models <- reactive({
      req(input$tl_dataset_id, input$tl_outcome, input$tl_session_name, input$tl_metric)
      df <- trained_models_log()
      df <- df[df$dataset_id %in% input$tl_dataset_id, , drop = FALSE]
      if ("outcome" %in% names(df)) df <- df[df$outcome %in% input$tl_outcome, , drop = FALSE]
      else if ("target" %in% names(df)) df <- df[df$target %in% input$tl_outcome, , drop = FALSE]
      df <- df[as.character(df$session_name) %in% as.character(input$tl_session_name), , drop = FALSE]
      df <- Rautoml::filter_session_metric(df = df, metric_name = input$tl_metric,
                                            session_name_ = input$tl_session_name,
                                            model_name = Rautoml::extract_value_labels(df, "model"))
      shiny::validate(shiny::need(nrow(df) > 0, "No models found for this dataset, outcome, session, and metric."))
      df
    })

    output$tl_model_ui <- renderUI({
      df <- filtered_trained_models()
      model_choices <- if ("model_id" %in% names(df)) {
        stats::setNames(df$model_id, paste0(df$model, " | ", df$model_id))
      } else {
        Rautoml::extract_value_labels(df, "model")
      }
      tagList(
        selectInput(ns("tl_selected_model_id"), "Source model(s)", choices = model_choices, selected = NULL,
                    multiple = TRUE, width = "100%"),
        tags$small(class = "text-muted",
                    "Select multiple to compare their Step 3 scoring side by side (assumes they share the same ",
                    "predictor schema, e.g. models from the same session). Feature importance, SHAP, fine-tuning, ",
                    "Step 4 comparison, and the report use the ", tags$strong("first"), " model selected.")
      )
    })

    # -------------------------------------------------------------------------
    # Selected model helpers
    # -------------------------------------------------------------------------

    # SHAP, feature importance, fine-tuning, Step 4 comparison, and the report all assume a single
    # source model -- when multiple are selected for Step 3 comparison, the first one is "primary"
    # and drives everything downstream of Step 3.
    primary_model_id <- reactive({
      req(input$tl_selected_model_id)
      input$tl_selected_model_id[1]
    })

    selected_model_log_row <- reactive({
      model_id <- primary_model_id()
      df  <- filtered_trained_models()
      row <- if ("model_id" %in% names(df)) df[df$model_id == model_id, , drop = FALSE]
             else df[df$model == model_id, , drop = FALSE]
      shiny::validate(shiny::need(nrow(row) > 0, "Selected model was not found in the trained model logs."))
      row[1, , drop = FALSE]
    })

    selected_model_info <- reactive({
      row      <- selected_model_log_row()
      model_id <- if ("model_id" %in% names(row)) as.character(row$model_id[1]) else as.character(row$model[1])
      list(owner = app_username, model = model_id,
           board_path = file.path(getwd(), app_username, "models"), source = "models", log_row = row)
    })

    selected_board    <- reactive({ pins::board_folder(selected_model_info()$board_path) })
    source_pin_meta   <- reactive({ tryCatch(pins::pin_meta(selected_board(), selected_model_info()$model), error = function(e) NULL) })
    source_model_obj  <- reactive({
      info <- selected_model_info()
      obj  <- tryCatch(vetiver::vetiver_pin_read(selected_board(), info$model), error = function(e) NULL)
      if (is.null(obj)) obj <- pins::pin_read(selected_board(), info$model)
      obj
    })
    source_fit <- reactive({
      req(source_model_obj())
      obj <- source_model_obj()
      if (is.list(obj) && "model" %in% names(obj)) obj <- obj$model
      if (inherits(obj, "bundle")) obj <- bundle::unbundle(obj)
      obj
    })

    source_schema <- reactive({
      req(source_pin_meta(), rv_current$working_df)
      info        <- selected_model_info()
      meta        <- source_pin_meta()
      schema_name <- paste0(info$model, "_schema")
      pins_all    <- tryCatch(pins::pin_list(selected_board()), error = function(e) character())
      if (schema_name %in% pins_all) return(pins::pin_read(selected_board(), schema_name))
      user_meta  <- meta$user %||% list()
      template   <- user_meta$template %||% NULL
      prototype  <- user_meta$prototype %||% NULL
      predictors <- NULL
      if (!is.null(template)) predictors <- names(template)
      else if (!is.null(prototype)) predictors <- names(prototype)
      shiny::validate(shiny::need(!is.null(predictors) && length(predictors) > 0,
                                   "No predictor schema found in the pinned model metadata."))
      outcome_name <- input$tl_outcome %||% rv_ml_ai$outcome %||% rv_ml_ai$target
      if (is.null(outcome_name) || !outcome_name %in% names(rv_current$working_df))
        outcome_name <- setdiff(names(rv_current$working_df), predictors)[1]
      outcome_levels <- NULL
      if (!is.null(outcome_name) && outcome_name %in% names(rv_current$working_df))
        outcome_levels <- levels(as.factor(rv_current$working_df[[outcome_name]]))
      column_types <- sapply(predictors, function(v) {
        vals <- prototype[[v]] %||% template[[v]] %||% NULL
        if (is.numeric(vals)) "numeric" else "factor"
      })
      factor_levels <- lapply(predictors, function(v) {
        vals <- prototype[[v]] %||% template[[v]] %||% NULL
        if (is.numeric(vals)) NULL else unique(as.character(unlist(vals)))
      })
      names(factor_levels) <- predictors
      list(outcome = list(name = outcome_name, levels = outcome_levels), predictors = predictors,
           column_types = column_types, factor_levels = factor_levels,
           owner = info$owner, model = info$model, source = info$source, log_row = info$log_row)
    })

    outcome_var    <- reactive({ req(source_schema()); source_schema()$outcome$name })
    positive_class <- reactive({ "Positive" })
    outcome_type   <- reactive({ input$outcome_type %||% "classification" })

    # -------------------------------------------------------------------------
    # Show only the tabs relevant to the selected outcome type (not just their
    # content) -- classification-only and regression-only tabs are hidden
    # outright from the tab strip in both Step 3 and Step 4.
    # -------------------------------------------------------------------------

    observeEvent(input$outcome_type, {
      is_reg <- identical(input$outcome_type, "regression")

      s3_classification_only <- c("ROC Curve", "Confusion Matrix", "Calibration",
                                   "Prediction by Outcome", "Threshold Optimisation")
      s3_regression_only     <- c("Actual vs Predicted", "Residuals Plot", "Residual Distribution")
      for (tb in s3_classification_only) {
        if (is_reg) shiny::hideTab("source_only_plots_tabs", tb, session = session)
        else        shiny::showTab("source_only_plots_tabs", tb, session = session)
      }
      for (tb in s3_regression_only) {
        if (is_reg) shiny::showTab("source_only_plots_tabs", tb, session = session)
        else        shiny::hideTab("source_only_plots_tabs", tb, session = session)
      }

      s4_classification_only <- c("ROC Curves", "Calibration Comparison")
      s4_regression_only     <- c("Residuals Comparison")
      for (tb in s4_classification_only) {
        if (is_reg) shiny::hideTab("comparison_plots_tabs", tb, session = session)
        else        shiny::showTab("comparison_plots_tabs", tb, session = session)
      }
      for (tb in s4_regression_only) {
        if (is_reg) shiny::showTab("comparison_plots_tabs", tb, session = session)
        else        shiny::hideTab("comparison_plots_tabs", tb, session = session)
      }
    }, ignoreNULL = FALSE)

    source_params <- reactive({
      meta <- source_pin_meta(); info <- selected_model_info(); row <- selected_model_log_row()
      data.frame(
        item  = c("Model type", "Required packages", "Recipe ID", "Dataset ID", "Session name", "Metric", "Model ID", "Model source"),
        value = c(meta$description %||% row$model[1] %||% "Not available",
                  paste(meta$user$required_pkgs %||% "Not available", collapse = ", "),
                  meta$user$recipes %||% "Not available",
                  row$dataset_id[1] %||% "Not available", row$session_name[1] %||% "Not available",
                  row$metric[1] %||% "Not available", info$model, info$source %||% "models"),
        stringsAsFactors = FALSE
      )
    })

    # ---- Step 1 display tables ----

    output$model_metadata_table <- renderTable({
      req(source_schema(), selected_model_info(), selected_model_log_row())
      sch <- source_schema(); meta <- source_pin_meta(); info <- selected_model_info(); row <- selected_model_log_row()
      data.frame(
        field = c("Selected model ID", "Model", "Owner", "Dataset ID", "Session", "Metric", "Outcome", "Predictor count", "Board path", "Pinned metadata"),
        value = c(info$model, row$model[1] %||% "Not available", info$owner,
                  row$dataset_id[1] %||% "Not available", row$session_name[1] %||% "Not available",
                  row$metric[1] %||% "Not available", sch$outcome$name %||% "Not detected",
                  length(sch$predictors), info$board_path, if (is.null(meta)) "Unavailable" else "Available"),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$source_schema_table <- renderTable({
      req(source_schema())
      sch <- source_schema()
      allowed_values <- sapply(sch$predictors, function(v) {
        if (v %in% names(sch$factor_levels) && !is.null(sch$factor_levels[[v]]))
          paste(sch$factor_levels[[v]], collapse = ", ") else "any numeric"
      })
      data.frame(variable = sch$predictors, expected_type = unlist(sch$column_types[sch$predictors]),
                 allowed_values = unname(allowed_values), stringsAsFactors = FALSE)
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$source_hyperparam_table <- renderTable({
      req(source_params()); source_params()
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    # -------------------------------------------------------------------------
    # Step 2 — target data
    # -------------------------------------------------------------------------

    target_data <- reactive({
      req(data_ready(), source_schema())
      df  <- rv_current$working_df; sch <- source_schema()
      missing_cols <- setdiff(c(sch$predictors, sch$outcome$name), names(df))
      shiny::validate(
        shiny::need(length(missing_cols) == 0,
                    paste("The active dataset is missing these required columns:", paste(missing_cols, collapse = ", "))),
        shiny::need(nrow(df) > 10, "Dataset too small for transfer learning.")
      )
      id_cols <- intersect(c("id", "ID", "idcno", "patient_id", "PatientID", "row_id"), names(df))
      id_col  <- if (length(id_cols) > 0) id_cols[1] else NULL
      for (v in sch$predictors) {
        if (sch$column_types[[v]] == "numeric") df[[v]] <- as.numeric(df[[v]])
        if (sch$column_types[[v]] == "factor")  df[[v]] <- factor(as.character(df[[v]]), levels = sch$factor_levels[[v]])
      }
      outcome_name     <- sch$outcome$name; outcome_levels <- sch$outcome$levels
      outcome_vals_raw <- as.character(df[[outcome_name]])
      outcome_vals <- if (length(outcome_levels) == 2) {
        dplyr::case_when(
          outcome_vals_raw %in% c(outcome_levels[1], tolower(outcome_levels[1]),
                                   "0", "No", "NO", "no", "Normal", "normal", "NoDisease", "no_disease", "No Disease") ~ outcome_levels[1],
          outcome_vals_raw %in% c(outcome_levels[2], tolower(outcome_levels[2]),
                                   "1", "Yes", "YES", "yes", "Disease", "disease") ~ outcome_levels[2],
          TRUE ~ outcome_vals_raw)
      } else { outcome_vals_raw }
      if (outcome_type() == "regression") {
        df[[outcome_name]] <- suppressWarnings(as.numeric(outcome_vals_raw))
        shiny::validate(shiny::need(!all(is.na(df[[outcome_name]])),
                                     paste0("Outcome column '", outcome_name, "' cannot be converted to numeric. ",
                                            "Found: ", paste(head(unique(outcome_vals_raw), 5), collapse = ", "))))
      } else {
        outcome_vals_clean <- tolower(trimws(as.character(outcome_vals)))
        mapped_vals <- dplyr::case_when(
          outcome_vals_clean %in% c("1", "yes", "positive", "disease", "case", "event", "dead", "true") ~ "Positive",
          outcome_vals_clean %in% c("0", "no", "negative", "control", "nonevent", "alive", "false")     ~ "Negative",
          TRUE ~ as.character(outcome_vals))
        df[[outcome_name]] <- droplevels(factor(mapped_vals, levels = c("Negative", "Positive")))
        shiny::validate(shiny::need(!all(is.na(df[[outcome_name]])),
                                     paste0("Outcome values do not match source model levels. Expects: ",
                                            paste(outcome_levels, collapse = ", "),
                                            ". Found: ", paste(unique(outcome_vals_raw), collapse = ", "))))
      }
      df$.row_id     <- seq_len(nrow(df))
      df$.display_id <- if (!is.null(id_col)) df[[id_col]] else df$.row_id
      df
    })

    output$active_dataset_info <- renderPrint({
      req(rv_current$working_df)
      cat("Active target dataset:", rv_current$dataset_id %||% "Active platform dataset", "\n")
      cat("Rows:", nrow(rv_current$working_df), "\n")
      cat("Columns:", ncol(rv_current$working_df), "\n")
    })

    output$active_outcome_display <- renderText({ req(source_schema()); source_schema()$outcome$name })

    output$target_data_preview <- renderTable({
      req(target_data()); head(target_data() %>% dplyr::select(-.row_id), 10)
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    # -------------------------------------------------------------------------
    # IMPROVEMENT 1: Schema Compatibility Check
    # -------------------------------------------------------------------------

    schema_compatibility <- reactive({
      req(source_schema(), rv_current$working_df)
      sch <- source_schema(); df <- rv_current$working_df

      purrr::map_dfr(sch$predictors, function(v) {
        expected_type <- sch$column_types[[v]]

        if (!v %in% names(df)) {
          return(data.frame(variable = v, expected_type = expected_type,
                             status = "ERROR", notes = "Column missing from target dataset",
                             stringsAsFactors = FALSE))
        }

        col         <- df[[v]]
        pct_missing <- round(100 * mean(is.na(col)), 1)
        issues      <- character(0)

        if (expected_type == "numeric" && !is.numeric(col) && !is.integer(col))
          issues <- c(issues, "Type mismatch: expected numeric")

        if (expected_type == "factor") {
          target_levels  <- unique(as.character(col[!is.na(col)]))
          source_levels  <- sch$factor_levels[[v]] %||% character(0)
          unseen         <- setdiff(target_levels, source_levels)
          missing_levels <- setdiff(source_levels, target_levels)
          if (length(unseen) > 0)
            issues <- c(issues, paste0("Unseen levels: ", paste(head(unseen, 5), collapse = ", ")))
          if (length(missing_levels) > 0)
            issues <- c(issues, paste0("Missing levels: ", paste(head(missing_levels, 5), collapse = ", ")))
        }

        if (pct_missing > 20) issues <- c(issues, paste0(pct_missing, "% values missing"))

        status <- if (any(grepl("missing from target|Type mismatch", issues))) "ERROR"
                  else if (length(issues) > 0) "WARNING"
                  else "OK"

        data.frame(variable = v, expected_type = expected_type, status = status,
                   pct_missing = pct_missing,
                   notes = if (length(issues) == 0) "Compatible" else paste(issues, collapse = "; "),
                   stringsAsFactors = FALSE)
      })
    })

    output$schema_check_table <- DT::renderDataTable({
      df <- schema_compatibility()
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) %>%
        DT::formatStyle("status",
                         backgroundColor = DT::styleEqual(c("OK", "WARNING", "ERROR"),
                                                          c("#d4edda", "#fff3cd", "#f8d7da")),
                         fontWeight = "bold")
    })

    output$dl_schema_check <- downloadHandler(
      filename = function() paste0("schema_check_", Sys.Date(), ".csv"),
      content  = function(file) { req(schema_compatibility()); utils::write.csv(schema_compatibility(), file, row.names = FALSE) }
    )

    # -------------------------------------------------------------------------
    # IMPROVEMENT 2: Distribution Shift Detection
    # -------------------------------------------------------------------------

    distribution_shift <- reactive({
      req(source_schema(), rv_current$working_df)
      sch <- source_schema(); df <- rv_current$working_df

      purrr::map_dfr(sch$predictors, function(v) {
        if (!v %in% names(df)) return(NULL)
        col           <- df[[v]]
        pct_missing   <- round(100 * mean(is.na(col)), 1)
        expected_type <- sch$column_types[[v]]

        if (expected_type == "factor") {
          source_levels <- sch$factor_levels[[v]] %||% character(0)
          target_levels <- unique(as.character(col[!is.na(col)]))
          n_unseen      <- length(setdiff(target_levels, source_levels))
          n_absent      <- length(setdiff(source_levels, target_levels))

          psi <- if (length(source_levels) > 1) {
            tab_a      <- table(factor(as.character(col), levels = source_levels))
            expected_p <- pmax(rep(1 / length(source_levels), length(source_levels)), 0.0001)
            actual_p   <- pmax(as.numeric(tab_a) / max(1, sum(tab_a)), 0.0001)
            round(sum((actual_p - expected_p) * log(actual_p / expected_p)), 4)
          } else NA_real_

          flag <- dplyr::case_when(is.na(psi) ~ "Unknown", psi < 0.1 ~ "Stable",
                                    psi < 0.2 ~ "Minor shift", TRUE ~ "Major shift")

          data.frame(variable = v, type = "categorical", psi = psi, shift_flag = flag,
                     pct_missing = pct_missing,
                     detail = paste0(length(target_levels), " target levels; ",
                                     n_unseen, " unseen; ", n_absent, " absent from target"),
                     stringsAsFactors = FALSE)
        } else {
          vals    <- as.numeric(col[!is.na(col)])
          if (length(vals) < 2) return(NULL)
          mn      <- round(mean(vals), 3); sdv <- round(stats::sd(vals), 3)
          minv    <- round(min(vals), 3);  maxv <- round(max(vals), 3)
          pct_out <- round(100 * mean(abs(scale(vals)) > 3, na.rm = TRUE), 1)
          flag    <- if (pct_missing > 30) "High missing" else if (pct_out > 10) "Outliers" else "Stable"

          data.frame(variable = v, type = "numeric", psi = NA_real_, shift_flag = flag,
                     pct_missing = pct_missing,
                     detail = paste0("mean=", mn, ", sd=", sdv,
                                     ", range=[", minv, ",", maxv, "], outliers=", pct_out, "%"),
                     stringsAsFactors = FALSE)
        }
      })
    })

    output$shift_summary_table <- DT::renderDataTable({
      df <- distribution_shift()
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) %>%
        DT::formatStyle("shift_flag",
                         backgroundColor = DT::styleEqual(
                           c("Stable", "Minor shift", "Major shift", "High missing", "Outliers", "Unknown"),
                           c("#d4edda", "#fff3cd", "#f8d7da", "#fff3cd", "#fff3cd", "#e2e3e5")))
    })

    make_shift_plot <- function() {
      df     <- distribution_shift()
      cat_df <- df[df$type == "categorical" & !is.na(df$psi), , drop = FALSE]
      if (nrow(cat_df) == 0) return(NULL)
      cat_df$shift_flag <- factor(cat_df$shift_flag, levels = c("Stable", "Minor shift", "Major shift", "Unknown"))
      ggplot2::ggplot(cat_df, ggplot2::aes(x = reorder(variable, psi), y = psi, fill = shift_flag)) +
        ggplot2::geom_col() +
        ggplot2::geom_hline(yintercept = 0.1, linetype = "dashed", colour = "#f39c12", linewidth = 0.8) +
        ggplot2::geom_hline(yintercept = 0.2, linetype = "dashed", colour = "#dd4b39", linewidth = 0.8) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
          values = c(Stable = "#00a65a", "Minor shift" = "#f39c12", "Major shift" = "#dd4b39", Unknown = "#aaaaaa"),
          na.value = "#aaaaaa") +
        ggplot2::labs(title = "Population Stability Index (PSI) — Categorical Predictors",
                      x = NULL, y = "PSI", fill = "Shift Status",
                      caption = "Dashed lines: 0.1 = minor shift, 0.2 = major shift") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                       legend.position = "bottom")
    }

    output$shift_plot <- renderPlot({
      req(distribution_shift())
      p <- make_shift_plot()
      if (is.null(p)) { plot.new(); text(0.5, 0.5, "PSI available for categorical predictors only.", cex = 1.1) }
      else p
    })

    output$dl_shift_table <- downloadHandler(
      filename = function() paste0("distribution_shift_", Sys.Date(), ".csv"),
      content  = function(file) { req(distribution_shift()); utils::write.csv(distribution_shift(), file, row.names = FALSE) }
    )
    output$dl_shift_plot <- downloadHandler(
      filename = function() paste0("psi_plot_", Sys.Date(), ".png"),
      content  = function(file) {
        p <- make_shift_plot()
        if (!is.null(p)) ggplot2::ggsave(file, p, width = 8, height = 5, dpi = 150)
      }
    )

    # -------------------------------------------------------------------------
    # Prediction helper
    # -------------------------------------------------------------------------

    predict_source_model_any <- function(fit_obj, predictor_df, type = "prob") {
      align_predictions_df(predict_source_model_any_raw(fit_obj, predictor_df, type), as.data.frame(predictor_df))
    }

    predict_source_model_any_raw <- function(fit_obj, predictor_df, type = "prob") {
      predictor_df <- as.data.frame(predictor_df)
      if (inherits(fit_obj, "train")) {
        direct_err  <- NULL
        direct_pred <- tryCatch(predict(fit_obj, newdata = predictor_df, type = type),
                                 error = function(e) { direct_err <<- conditionMessage(e); NULL })
        if (!is.null(direct_pred)) return(as.data.frame(direct_pred))
        needed_vars <- fit_obj$finalModel$xNames %||%
          tryCatch(names(fit_obj$finalModel$coefficients), error = function(e) NULL)
        if (!is.null(needed_vars)) {
          # xNames/coefficients list predictors only (caret never includes the response here),
          # so only drop the intercept term -- a real predictor literally named "class" must survive.
          needed_vars <- setdiff(needed_vars, "(Intercept)")
        } else {
          # all.vars(formula(...)) includes the response variable, so the broader strip applies here.
          needed_vars <- tryCatch(all.vars(stats::formula(fit_obj$finalModel)), error = function(e) NULL)
          needed_vars <- setdiff(needed_vars, c("(Intercept)", ".outcome", "Class", "class", "y"))
        }
        manual_df <- data.frame(row_id_temp = seq_len(nrow(predictor_df)))
        for (v in names(predictor_df)) {
          x <- predictor_df[[v]]
          if (is.numeric(x) || is.integer(x)) { manual_df[[v]] <- as.numeric(x) }
          else {
            x <- as.character(x); lvls <- unique(stats::na.omit(x))
            if (length(lvls) > 0) for (lv in lvls) manual_df[[paste0(v, "_", make.names(lv))]] <- ifelse(x == lv, 1, 0)
          }
        }
        manual_df$row_id_temp <- NULL
        if (is.null(needed_vars) || length(needed_vars) == 0) needed_vars <- names(manual_df)
        for (v in setdiff(needed_vars, names(manual_df))) manual_df[[v]] <- 0
        manual_df <- manual_df[, needed_vars, drop = FALSE]
        manual_pred <- tryCatch(as.data.frame(predict(fit_obj, newdata = manual_df, type = type)),
                                 error = function(e) {
                                   stop(sprintf(
                                     "direct predict.train (type='%s') failed: %s | manual dummy-reconstruction also failed: %s | needed_vars: %s | manual_df cols: %s",
                                     type, direct_err %||% "(no error captured)", conditionMessage(e),
                                     paste(needed_vars, collapse = ", "), paste(names(manual_df), collapse = ", ")
                                   ), call. = FALSE)
                                 })
        return(manual_pred)
      }
      if (inherits(fit_obj, "workflow")) return(as.data.frame(predict(fit_obj, new_data = predictor_df, type = type)))
      out <- tryCatch(predict(fit_obj, new_data = predictor_df, type = type),
                      error = function(e1) predict(fit_obj, newdata = predictor_df, type = type))
      as.data.frame(out)
    }

    # -------------------------------------------------------------------------
    # Model loading + scoring (shared by the single primary-model path below
    # and the multi-source-model comparison path)
    # -------------------------------------------------------------------------

    load_pinned_model_fit <- function(model_id) {
      board     <- pins::board_folder(file.path(getwd(), app_username, "models"))
      model_obj <- tryCatch(vetiver::vetiver_pin_read(board, model_id), error = function(e) NULL)
      if (is.null(model_obj)) model_obj <- pins::pin_read(board, model_id)
      fit_obj <- if (is.list(model_obj) && "model" %in% names(model_obj)) model_obj$model else model_obj
      if (inherits(fit_obj, "bundle")) fit_obj <- bundle::unbundle(fit_obj)
      fit_obj
    }

    score_fit_against_target <- function(fit_obj, sch, df, ov, outcome_type_val, threshold) {
      predictor_df <- df %>% dplyr::select(dplyr::all_of(sch$predictors))

      if (outcome_type_val == "regression") {
        pred_vals <- get_regression_preds(fit_obj, predictor_df)
        shiny::validate(shiny::need(!is.null(pred_vals) && !all(is.na(pred_vals)),
                                     paste0(
                                       "Source model did not return numeric predictions. Ensure you selected Regression and the model is a regression model.",
                                       if (!is.null(last_regression_pred_error())) paste0(" Technical detail: ", last_regression_pred_error()) else ""
                                     )))
        scored_df  <- df %>% dplyr::mutate(.predicted_numeric = pred_vals)
        actual     <- as.numeric(scored_df[[ov]])
        metric_tbl <- compute_regression_metrics(actual, scored_df$.predicted_numeric, "Source Model")
        return(list(scored_df = scored_df, prob_col = ".predicted_numeric", metrics = metric_tbl,
                    roc_df = NULL, conf_mat = NULL, calibration = NULL, can_compute_metrics = TRUE))
      }

      pos_class  <- positive_class()
      preds_prob <- tryCatch(
        as.data.frame(predict_source_model_any(fit_obj, predictor_df, "prob")),
        error = function(e) {
          shiny::validate(shiny::need(FALSE, paste0(
            "This source model cannot be scored directly in R. ",
            "This usually happens when the model was trained with PCA or other Python-side preprocessing. ",
            "Please select a different source model. Technical detail: ", conditionMessage(e)
          )))
        }
      )
      if (!any(grepl("^\\.pred_", names(preds_prob)))) names(preds_prob) <- paste0(".pred_", names(preds_prob))
      prob_col <- get_prob_col(preds_prob, pos_class)
      shiny::validate(shiny::need(!is.null(prob_col) && prob_col %in% names(preds_prob),
                                   "The source model did not return a valid probability column."))
      temp_df  <- dplyr::bind_cols(df, preds_prob)
      test_auc <- tryCatch(
        yardstick::roc_auc(temp_df, truth = !!rlang::sym(ov), !!rlang::sym(prob_col), event_level = "second")$.estimate,
        error = function(e) NA_real_)
      if (!is.na(test_auc) && test_auc < 0.5) preds_prob[[prob_col]] <- 1 - preds_prob[[prob_col]]
      scored_df <- dplyr::bind_cols(df, preds_prob) %>%
        dplyr::mutate(predicted_class = make_pred_class(.data[[prob_col]], levels(.data[[ov]]), threshold))
      truth_counts        <- table(scored_df[[ov]], useNA = "no")
      can_compute_metrics <- length(truth_counts) == 2 && all(truth_counts > 0)
      conf_tbl <- yardstick::conf_mat(scored_df, truth = !!rlang::sym(ov), estimate = predicted_class)$table
      if (isTRUE(can_compute_metrics)) {
        metric_tbl <- compute_source_metrics(scored_df, ov, prob_col) %>% dplyr::select(model, metric, estimate, lower, upper)
        roc_df     <- yardstick::roc_curve(scored_df, truth = !!rlang::sym(ov), !!rlang::sym(prob_col), event_level = "second")
        calib_df   <- make_calibration_df(scored_df, ov, prob_col, pos_class)
      } else {
        metric_tbl <- compute_source_metrics(scored_df, ov, prob_col)
        roc_df <- NULL; calib_df <- NULL
      }
      list(scored_df = scored_df, prob_col = prob_col, metrics = metric_tbl, roc_df = roc_df,
           conf_mat = as.data.frame.matrix(conf_tbl), calibration = calib_df, can_compute_metrics = can_compute_metrics)
    }

    # -------------------------------------------------------------------------
    # Step 3 — source-only scoring (primary model — first of the selected models)
    # -------------------------------------------------------------------------

    source_only_results <- eventReactive(input$run_source_only, {
      source_scoring_status("running"); source_scoring_error_detail(NULL)
      result <- tryCatch({
        shiny::withProgress(message = "Running source model...", value = 0, {
          shiny::incProgress(0.15, detail = "Preparing target data")
          req(target_data(), source_fit(), source_schema())
          shiny::incProgress(0.40, detail = "Applying source model")
          out <- score_fit_against_target(source_fit(), source_schema(), target_data(), outcome_var(),
                                           outcome_type(), input$class_threshold)
          shiny::incProgress(1, detail = "Done")
          out
        })
      }, error = function(e) { source_scoring_status("error"); source_scoring_error_detail(conditionMessage(e)); stop(e) })
      source_scoring_status("done"); result
    })

    observeEvent(source_only_results(), {
      rv_ml_ai$transfer_source_only_results <- source_only_results()
      rv_ml_ai$transfer_source_model        <- primary_model_id()
    })

    observeEvent(input$run_source_only, {
      updateTabsetPanel(session, "transfer_learning_steps", selected = "Step 3: Scoring & Analysis")
    })

    output$source_only_metrics <- renderTable({
      req(source_only_results()); source_only_results()$metrics
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    # -------------------------------------------------------------------------
    # Multi-source-model comparison (Step 3) -- when more than one source model
    # is selected, score each one against the same target dataset and put their
    # metrics side by side. Each model runs in its own try/catch so one bad or
    # incompatible model is skipped (with the reason shown) rather than
    # aborting the whole comparison.
    # -------------------------------------------------------------------------

    source_multi_errors <- reactiveVal(character())
    source_multi_roc    <- reactiveVal(NULL)

    output$source_multi_errors_ui <- renderUI({
      errs <- source_multi_errors()
      if (length(errs) == 0) return(NULL)
      tags$div(class = "alert alert-warning",
               tags$i(class = "fa fa-exclamation-triangle"),
               sprintf(" %d of the selected source model(s) could not be scored on this dataset and were skipped:", length(errs)),
               tags$ul(lapply(errs, tags$li)))
    })
    outputOptions(output, "source_multi_errors_ui", suspendWhenHidden = FALSE)

    source_only_results_multi <- eventReactive(input$run_source_only, {
      ids <- unique(input$tl_selected_model_id %||% character())
      if (length(ids) <= 1) { source_multi_errors(character()); return(NULL) }
      req(target_data(), source_schema())

      df <- target_data(); sch <- source_schema(); ov <- outcome_var()
      log_df  <- filtered_trained_models()
      failed  <- character(); all_metrics <- list(); all_roc <- list()

      shiny::withProgress(message = "Scoring selected source models...", value = 0, {
        for (id in ids) {
          row  <- if ("model_id" %in% names(log_df)) log_df[log_df$model_id == id, , drop = FALSE]
                  else log_df[log_df$model == id, , drop = FALSE]
          name <- if (nrow(row) > 0) as.character(row$model[1]) else id
          shiny::incProgress(1 / length(ids), detail = paste0("Model: ", name))

          out <- tryCatch({
            fit_obj <- load_pinned_model_fit(id)
            score_fit_against_target(fit_obj, sch, df, ov, outcome_type(), input$class_threshold)
          }, error = function(e) {
            failed <<- c(failed, paste0(name, ": ", conditionMessage(e)))
            NULL
          })

          if (!is.null(out)) {
            all_metrics[[id]] <- out$metrics %>% dplyr::mutate(model = name)
            if (!is.null(out$roc_df)) {
              all_roc[[id]] <- out$roc_df %>% dplyr::mutate(model = name) %>%
                dplyr::select(model, specificity, sensitivity)
            }
          }
        }
      })

      source_multi_errors(failed)
      source_multi_roc(if (length(all_roc) == 0) NULL else dplyr::bind_rows(all_roc))
      if (length(all_metrics) == 0) return(NULL)
      dplyr::bind_rows(all_metrics)
    })

    output$source_multi_metrics_ui <- renderUI({
      if (length(input$tl_selected_model_id %||% character()) <= 1) return(NULL)
      tagList(
        tags$hr(),
        tags$h5("Comparison Across Selected Source Models"),
        uiOutput(ns("source_multi_errors_ui")),
        DT::dataTableOutput(ns("source_multi_metrics_table"))
      )
    })
    outputOptions(output, "source_multi_metrics_ui", suspendWhenHidden = FALSE)

    output$source_multi_metrics_table <- DT::renderDataTable({
      req(source_only_results_multi())
      df <- source_only_results_multi() %>% dplyr::arrange(metric, model)
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>%
        DT::formatRound(columns = intersect(c("estimate", "lower", "upper"), names(df)), digits = 4)
    })

    output$dl_source_multi_metrics <- downloadHandler(
      filename = function() paste0("source_models_comparison_", Sys.Date(), ".csv"),
      content  = function(file) { req(source_only_results_multi()); utils::write.csv(source_only_results_multi(), file, row.names = FALSE) }
    )

    # ---- Step 3 plot builders ----

    make_source_roc_plot <- function() {
      roc_df <- source_only_results()$roc_df
      if (is.null(roc_df)) return(NULL)
      ggplot2::ggplot(roc_df, ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
        ggplot2::geom_line(linewidth = 1.2, colour = "#00a65a") +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey40") +
        ggplot2::coord_equal() +
        ggplot2::labs(title = "ROC Curve — Source Model", x = "1 - Specificity", y = "Sensitivity") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }

    # ROC overlay across the selected source models (2+), mirrors make_comparison_roc_plot
    # (Step 4 source-vs-retrained) but plots the multi-source scoring data collected in
    # source_only_results_multi()'s loop instead.
    make_source_multi_roc_plot <- function() {
      roc_data <- source_multi_roc()
      if (is.null(roc_data) || nrow(roc_data) == 0) return(NULL)
      ggplot2::ggplot(roc_data, ggplot2::aes(x = 1 - specificity, y = sensitivity, colour = model)) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
        ggplot2::coord_equal() +
        ggplot2::labs(title = "ROC Curves — Selected Source Models",
                      x = "1 - Specificity", y = "Sensitivity", colour = "Model") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5), legend.position = "bottom")
    }

    make_source_conf_mat_plot <- function() {
      res <- source_only_results(); ov <- outcome_var()
      cm_df <- as.data.frame(table(Prediction = res$scored_df$predicted_class, Target = res$scored_df[[ov]]))
      total <- sum(cm_df$Freq, na.rm = TRUE)
      cm_df <- cm_df %>% dplyr::mutate(Percent = ifelse(total > 0, 100 * Freq / total, 0),
                                        Label = paste0(Freq, "\n(", round(Percent, 1), "%)"))
      ggplot2::ggplot(cm_df, ggplot2::aes(x = Target, y = Prediction, fill = Freq)) +
        ggplot2::geom_tile(colour = "white", linewidth = 1.5) +
        ggplot2::geom_text(ggplot2::aes(label = Label), size = 5.5, fontface = "bold", colour = "white") +
        ggplot2::scale_fill_gradient(low = "#AED6F1", high = "#1A5276", name = "Count") +
        ggplot2::labs(title = "Confusion Matrix", x = "Actual", y = "Predicted") +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
                       axis.text  = ggplot2::element_text(size = 12, face = "bold"),
                       plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }

    make_source_calibration_plot <- function() {
      calib <- source_only_results()$calibration
      if (is.null(calib)) return(NULL)
      ggplot2::ggplot(calib, ggplot2::aes(x = mean_pred, y = observed)) +
        ggplot2::geom_point(size = 3, colour = "#00a65a") +
        ggplot2::geom_line(colour = "#00a65a", linewidth = 1) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
        ggplot2::labs(title = "Source Model Calibration", x = "Mean predicted probability", y = "Observed event rate") +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }

    make_source_boxplot <- function() {
      res <- source_only_results(); ov <- outcome_var()
      ggplot2::ggplot(res$scored_df, ggplot2::aes(x = .data[[ov]], y = .data[[res$prob_col]], fill = .data[[ov]])) +
        ggplot2::geom_boxplot(alpha = 0.8) +
        ggplot2::scale_fill_manual(values = c(Negative = "#AED6F1", Positive = "#00a65a")) +
        ggplot2::labs(title = "Predicted Probability by Outcome", x = "Observed Outcome", y = "Predicted Probability") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }

    output$source_only_roc_plot <- renderPlot({
      req(source_only_results())
      p <- make_source_roc_plot()
      if (is.null(p)) { plot.new(); text(0.5, 0.5, "ROC curve not available — both outcome classes required.", cex = 1.1) }
      else p
    })
    output$source_multi_roc_plot <- renderPlot({
      req(source_only_results_multi())
      p <- make_source_multi_roc_plot()
      if (is.null(p)) { plot.new(); text(0.5, 0.5, "ROC curves not available — both outcome classes required.", cex = 1.1) }
      else p
    })
    output$source_only_conf_mat_plot <- renderPlot({ req(source_only_results()); make_source_conf_mat_plot() })
    output$source_only_calibration_plot <- renderPlot({
      req(source_only_results()); p <- make_source_calibration_plot()
      if (is.null(p)) { plot.new(); text(0.5, 0.5, "Calibration not available.", cex = 1.1) } else p
    })
    output$source_only_boxplot <- renderPlot({ req(source_only_results()); make_source_boxplot() })

    # ---- Regression-specific plots ----

    make_actual_vs_pred_plot <- function() {
      res <- source_only_results(); ov <- outcome_var()
      df  <- res$scored_df
      ggplot2::ggplot(df, ggplot2::aes(x = as.numeric(.data[[ov]]), y = .predicted_numeric)) +
        ggplot2::geom_point(alpha = 0.55, colour = "#1A5276", size = 1.8) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "#00a65a", linewidth = 1) +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#e67e22", linewidth = 1) +
        ggplot2::labs(title = "Actual vs Predicted", x = paste0("Actual: ", ov), y = "Predicted") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }

    make_residuals_plot <- function() {
      res <- source_only_results(); ov <- outcome_var()
      df  <- res$scored_df %>% dplyr::mutate(.residual = as.numeric(.data[[ov]]) - .predicted_numeric)
      ggplot2::ggplot(df, ggplot2::aes(x = .predicted_numeric, y = .residual)) +
        ggplot2::geom_point(alpha = 0.55, colour = "#1A5276", size = 1.8) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "#00a65a", linewidth = 1) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE, colour = "#e67e22", linewidth = 1) +
        ggplot2::labs(title = "Residuals vs Predicted", x = "Predicted", y = "Residual (Actual − Predicted)") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }

    make_residual_dist_plot <- function() {
      res <- source_only_results(); ov <- outcome_var()
      df  <- res$scored_df %>% dplyr::mutate(.residual = as.numeric(.data[[ov]]) - .predicted_numeric)
      ggplot2::ggplot(df, ggplot2::aes(x = .residual)) +
        ggplot2::geom_histogram(fill = "#1A5276", colour = "white", bins = 30, alpha = 0.85) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "#00a65a", linewidth = 1) +
        ggplot2::labs(title = "Residual Distribution", x = "Residual (Actual − Predicted)", y = "Count") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }

    output$reg_actual_vs_pred_plot  <- renderPlot({ req(source_only_results()); make_actual_vs_pred_plot() })
    output$reg_residuals_plot       <- renderPlot({ req(source_only_results()); make_residuals_plot() })
    output$reg_residual_dist_plot   <- renderPlot({ req(source_only_results()); make_residual_dist_plot() })

    # ---- Step 3 downloads ----
    output$dl_source_metrics    <- downloadHandler(paste0("source_metrics_",  Sys.Date(), ".csv"),
      function(file) { req(source_only_results()); utils::write.csv(source_only_results()$metrics, file, row.names = FALSE) })
    output$dl_source_roc        <- downloadHandler(paste0("source_roc_",      Sys.Date(), ".png"),
      function(file) { p <- make_source_roc_plot();       if (!is.null(p)) ggplot2::ggsave(file, p, width = 6, height = 5, dpi = 150) })
    output$dl_source_multi_roc  <- downloadHandler(paste0("source_multi_roc_", Sys.Date(), ".png"),
      function(file) { p <- make_source_multi_roc_plot(); if (!is.null(p)) ggplot2::ggsave(file, p, width = 7, height = 6, dpi = 150) })
    output$dl_source_conf_mat   <- downloadHandler(paste0("source_conf_mat_", Sys.Date(), ".png"),
      function(file) { ggplot2::ggsave(file, make_source_conf_mat_plot(), width = 5, height = 4.5, dpi = 150) })
    output$dl_source_calibration <- downloadHandler(paste0("source_calibration_", Sys.Date(), ".png"),
      function(file) { p <- make_source_calibration_plot(); if (!is.null(p)) ggplot2::ggsave(file, p, width = 6, height = 5, dpi = 150) })
    output$dl_source_boxplot    <- downloadHandler(paste0("source_boxplot_",  Sys.Date(), ".png"),
      function(file) { ggplot2::ggsave(file, make_source_boxplot(), width = 6, height = 5, dpi = 150) })
    output$dl_reg_actual_vs_pred  <- downloadHandler(paste0("actual_vs_pred_",    Sys.Date(), ".png"),
      function(file) { ggplot2::ggsave(file, make_actual_vs_pred_plot(),  width = 6, height = 5, dpi = 150) })
    output$dl_reg_residuals       <- downloadHandler(paste0("residuals_plot_",    Sys.Date(), ".png"),
      function(file) { ggplot2::ggsave(file, make_residuals_plot(),       width = 6, height = 5, dpi = 150) })
    output$dl_reg_residual_dist   <- downloadHandler(paste0("residual_dist_",     Sys.Date(), ".png"),
      function(file) { ggplot2::ggsave(file, make_residual_dist_plot(),   width = 6, height = 5, dpi = 150) })

    # -------------------------------------------------------------------------
    # Feature Importance (permutation-based, model-agnostic)
    # Mirrors the "Feature Importance" plot shown in train_model
    # -------------------------------------------------------------------------

    feature_importance_results <- reactive({
      req(source_only_results(), source_schema())
      res <- source_only_results(); sch <- source_schema(); ov <- outcome_var()

      if (outcome_type() == "regression") {
        actual <- as.numeric(res$scored_df[[ov]])
        baseline_rmse <- sqrt(mean((actual - res$scored_df$.predicted_numeric)^2, na.rm = TRUE))
        set.seed(123)
        purrr::map_dfr(sch$predictors, function(v) {
          shuffled_df <- res$scored_df
          shuffled_df[[v]] <- sample(shuffled_df[[v]])
          predictor_df <- shuffled_df %>% dplyr::select(dplyr::all_of(sch$predictors))
          preds <- get_regression_preds(source_fit(), predictor_df)
          if (is.null(preds)) return(data.frame(feature = v, importance = NA_real_, stringsAsFactors = FALSE))
          shuf_rmse <- sqrt(mean((actual - preds)^2, na.rm = TRUE))
          data.frame(feature = v, importance = round(shuf_rmse - baseline_rmse, 4), stringsAsFactors = FALSE)
        }) %>% dplyr::arrange(dplyr::desc(importance))

      } else {
        pos_class <- positive_class()
        baseline_auc <- tryCatch(
          yardstick::roc_auc(res$scored_df, truth = !!rlang::sym(ov), !!rlang::sym(res$prob_col), event_level = "second")$.estimate,
          error = function(e) NA_real_
        )
        shiny::validate(shiny::need(!is.na(baseline_auc), "Feature importance requires both outcome classes to be present."))
        set.seed(123)
        purrr::map_dfr(sch$predictors, function(v) {
          shuffled_df <- res$scored_df
          shuffled_df[[v]] <- sample(shuffled_df[[v]])
          predictor_df <- shuffled_df %>% dplyr::select(dplyr::all_of(sch$predictors))
          preds <- tryCatch(as.data.frame(predict_source_model_any(source_fit(), predictor_df, "prob")), error = function(e) NULL)
          if (is.null(preds)) return(data.frame(feature = v, importance = NA_real_, stringsAsFactors = FALSE))
          if (!any(grepl("^\\.pred_", names(preds)))) names(preds) <- paste0(".pred_", names(preds))
          prob_col <- get_prob_col(preds, pos_class)
          if (is.null(prob_col)) return(data.frame(feature = v, importance = NA_real_, stringsAsFactors = FALSE))
          shuffled_df[[prob_col]] <- preds[[prob_col]]
          shuf_auc <- tryCatch(
            yardstick::roc_auc(shuffled_df, truth = !!rlang::sym(ov), !!rlang::sym(prob_col), event_level = "second")$.estimate,
            error = function(e) NA_real_
          )
          data.frame(feature = v, importance = round(baseline_auc - shuf_auc, 4), stringsAsFactors = FALSE)
        }) %>% dplyr::arrange(dplyr::desc(importance))
      }
    })

    make_feature_importance_plot <- function() {
      df      <- feature_importance_results()
      is_reg  <- outcome_type() == "regression"
      y_label <- if (is_reg) "Importance (Δ RMSE)" else "Importance (Δ AUC)"
      title   <- if (is_reg) "Feature Importance (Permutation — Drop in RMSE)" else "Feature Importance (Permutation — Drop in AUC)"
      ggplot2::ggplot(df, ggplot2::aes(x = reorder(feature, importance), y = importance)) +
        ggplot2::geom_col(fill = "#00a65a") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = title, x = NULL, y = y_label) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }

    output$feature_importance_plot <- renderPlot({
      req(feature_importance_results()); make_feature_importance_plot()
    })

    output$feature_importance_table_ranked <- renderTable({
      req(feature_importance_results())
      col_nm <- if (outcome_type() == "regression") "Importance (Delta RMSE)" else "Importance (Delta AUC)"
      df <- feature_importance_results() %>%
        dplyr::mutate(rank = dplyr::row_number()) %>%
        dplyr::select(rank, feature, importance)
      names(df) <- c("Rank", "Feature", col_nm)
      df
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$dl_feature_importance_plot <- downloadHandler(paste0("feature_importance_plot_", Sys.Date(), ".png"),
      function(file) { req(feature_importance_results()); ggplot2::ggsave(file, make_feature_importance_plot(), width = 7, height = 5, dpi = 150) })

    # -------------------------------------------------------------------------
    # SHAP Values (model-agnostic via fastshap)
    # Mirrors the "SHAP Values" plot shown in train_model
    # -------------------------------------------------------------------------

    shap_status <- reactiveVal("idle")
    output$shap_status_ui <- renderUI({
      make_alert_ui(shap_status(),
                    "Computing SHAP values, please wait — this may take a moment...",
                    "SHAP computation complete.",
                    "SHAP computation failed for this model.")
    })
    outputOptions(output, "shap_status_ui", suspendWhenHidden = FALSE)

    # -------------------------------------------------------------------------
    # Plot-style toggle (Steve's feedback, 2026-08-11): default to the shared
    # Rautoml::compute_shap() pipeline (same one used by the Train/Test Model
    # side), fall back to this app's own custom-styled plots when the source
    # model isn't a plain caret `train` object or the shared call otherwise
    # fails. "Custom" always uses this app's own SHAP computation.
    #
    # compute_shap() is an S3 generic exported by Rautoml that dispatches on
    # a `caretList`-classed list of models (see Rautoml::compute_shap.caretList,
    # R/caret_shap_values.R); there is no public entry point for a single
    # train object, so we wrap source_fit() in a 1-element list and stamp it
    # with class "caretList" purely to reach that dispatch -- the method body
    # only ever does models[[model_name]], so this is safe. It also derives
    # positive_class internally as model$levels[[2]], exactly matching how
    # the Train/Test Model side already calls it -- no override needed here.
    # -------------------------------------------------------------------------

    # Some source models' finalModel was fit against manually dummy-encoded
    # predictor columns (e.g. "gender_male", "slumarea_viwandani") rather than
    # raw factor columns -- predict.train() on the raw columns then fails
    # looking up those exact names. predict_source_model_any_raw() (above)
    # already detects and handles this for every other prediction path in
    # this app via a probe-then-rebuild fallback; Rautoml::compute_shap()
    # calls predict() directly with none of that, so it fails outright for
    # these models ("object 'gender_male' not found"). Reuses the same
    # detection/rebuild logic here to hand Rautoml data it can actually
    # score, rather than patching that fallback into Rautoml itself.
    build_shap_predictor_df <- function(fit_obj, predictor_df) {
      predictor_df <- as.data.frame(predictor_df)
      probe <- utils::head(predictor_df, 1)
      direct_ok <- !is.null(tryCatch(predict(fit_obj, newdata = probe, type = "prob"),
                                      error = function(e) NULL))
      if (direct_ok) return(predictor_df)

      needed_vars <- fit_obj$finalModel$xNames %||%
        tryCatch(names(fit_obj$finalModel$coefficients), error = function(e) NULL)
      if (!is.null(needed_vars)) {
        needed_vars <- setdiff(needed_vars, "(Intercept)")
      } else {
        needed_vars <- tryCatch(all.vars(stats::formula(fit_obj$finalModel)), error = function(e) NULL)
        needed_vars <- setdiff(needed_vars, c("(Intercept)", ".outcome", "Class", "class", "y"))
      }
      manual_df <- data.frame(row_id_temp = seq_len(nrow(predictor_df)))
      for (v in names(predictor_df)) {
        x <- predictor_df[[v]]
        if (is.numeric(x) || is.integer(x)) { manual_df[[v]] <- as.numeric(x) }
        else {
          x <- as.character(x); lvls <- unique(stats::na.omit(x))
          if (length(lvls) > 0) for (lv in lvls) manual_df[[paste0(v, "_", make.names(lv))]] <- ifelse(x == lv, 1, 0)
        }
      }
      manual_df$row_id_temp <- NULL
      if (is.null(needed_vars) || length(needed_vars) == 0) needed_vars <- names(manual_df)
      for (v in setdiff(needed_vars, names(manual_df))) manual_df[[v]] <- 0
      manual_df[, needed_vars, drop = FALSE]
    }

    shap_rautoml_status <- reactiveVal(NULL)  # NULL = ok/untried; character = error message shown to the user

    shap_results_rautoml <- reactive({
      req(source_only_results(), source_schema())  # kept outside tryCatch: req()'s silent-stop must not be caught as an error
      shap_rautoml_status(NULL)
      tryCatch({
        fit <- source_fit()
        if (!inherits(fit, "train")) {
          stop("this source model isn't a plain caret `train` object")
        }
        task <- if (outcome_type() == "regression") "Regression" else "Classification"
        if (identical(task, "Classification")) {
          levs <- tryCatch(fit$levels, error = function(e) NULL)
          if (is.null(levs) || length(levs) < 2) stop("could not determine the model's outcome classes")
          if (length(levs) > 2) stop("multiclass models aren't supported by the shared SHAP pipeline in this view yet")
        }

        res <- source_only_results(); sch <- source_schema(); ov <- outcome_var()
        # Rautoml::compute_shap() samples rows straight from newdata (caret_shap_values.R:61)
        # and calls predict() directly with no realignment -- unlike this app's own
        # predict_source_model_any()/align_predictions(), which exist precisely because
        # caret's predict() silently drops rows with missing predictor values (see the
        # 2026-07-22 crash fix above). A row with an NA predictor here would come back
        # short from predict() inside fastshap::explain(), corrupting everything
        # downstream -- which is exactly the "group_by on NULL" error this was throwing.
        # Filter to complete cases before handing rows to the shared pipeline.
        full_df     <- res$scored_df[, c(sch$predictors, ov), drop = FALSE]
        complete_df <- full_df[stats::complete.cases(full_df), , drop = FALSE]
        if (nrow(complete_df) < 10) {
          stop("fewer than 10 rows with no missing predictor values -- the shared pipeline needs complete cases")
        }
        n_explain <- min(100, nrow(complete_df))
        sample_df <- complete_df[seq_len(n_explain), , drop = FALSE]

        # Reshape predictors into whatever form this model's predict() actually
        # accepts (raw or manually dummy-encoded -- see build_shap_predictor_df above).
        predictor_shaped <- build_shap_predictor_df(fit, sample_df[, sch$predictors, drop = FALSE])
        newdata <- cbind(predictor_shaped, stats::setNames(data.frame(sample_df[[ov]]), ov))

        model_label <- sch$model %||% "source_model"
        models_list <- stats::setNames(list(fit), model_label)
        class(models_list) <- c("caretList", class(models_list))

        result <- Rautoml::compute_shap(
          models = models_list, model_names = model_label,
          newdata = newdata, response = ov, task = task,
          nsim = 15, max_n = n_explain
        )
        # result carries class "Rautomlshap"; Rautoml registers a real, exported
        # plot.Rautomlshap() S3 method (plotfuns.R) that builds the ranked
        # point + error-bar "|SHAP value|" importance plot from the same SHAP
        # computation (via plot_varimp() on result$varimp_df) -- reached through
        # normal S3 dispatch on the base plot() generic, the package's actual
        # public entry point for it (plot_varimp() itself isn't exported).
        sv <- result$sv_viz[[model_label]]
        sv$varimp <- tryCatch(plot(result)$varimp, error = function(e) NULL)
        sv
      }, error = function(e) {
        shap_rautoml_status(conditionMessage(e))
        NULL
      })
    })

    output$shap_rautoml_note <- renderUI({
      if (identical(input$shap_plot_style, "rautoml") && !is.null(shap_rautoml_status())) {
        div(class = "tl-note", style = "color:#a94442;",
            tags$i(class = "fa fa-exclamation-triangle"),
            paste0(" Couldn't use the shared default pipeline for this model (",
                   shap_rautoml_status(), ") — showing this app's built-in SHAP plots instead."))
      }
    })

    shap_results <- eventReactive(input$run_shap, {
      req(source_only_results(), source_schema())
      shap_status("running")

      result <- tryCatch({
        if (!requireNamespace("fastshap", quietly = TRUE)) {
          stop("The 'fastshap' package is not installed. Please install it to enable SHAP values.")
        }
        res <- source_only_results(); sch <- source_schema(); pos_class <- positive_class()
        predictor_df <- res$scored_df %>% dplyr::select(dplyr::all_of(sch$predictors))

        n_explain <- min(100, nrow(predictor_df))
        X_explain <- predictor_df[seq_len(n_explain), , drop = FALSE]

        pred_wrapper <- if (outcome_type() == "regression") {
          function(object, newdata) {
            vals <- get_regression_preds(object, newdata)
            if (is.null(vals)) stop("Regression predictions failed in SHAP wrapper.")
            vals
          }
        } else {
          function(object, newdata) {
            preds <- as.data.frame(predict_source_model_any(object, newdata, "prob"))
            if (!any(grepl("^\\.pred_", names(preds)))) names(preds) <- paste0(".pred_", names(preds))
            prob_col <- get_prob_col(preds, pos_class)
            preds[[prob_col]]
          }
        }

        shiny::withProgress(message = "Computing SHAP values...", value = 0.3, {
          shap_vals <- fastshap::explain(
            object = source_fit(), X = predictor_df, newdata = X_explain,
            pred_wrapper = pred_wrapper, nsim = 15
          )
          shiny::incProgress(1)
        })

        list(shap_vals = as.data.frame(shap_vals), X_explain = X_explain)
      }, error = function(e) { shap_status("error"); stop(e) })

      shap_status("done"); result
    })

    make_shap_summary_plot <- function() {
      shap_df <- shap_results()$shap_vals
      imp <- shap_df %>%
        tidyr::pivot_longer(dplyr::everything(), names_to = "feature", values_to = "shap") %>%
        dplyr::group_by(feature) %>%
        dplyr::summarise(mean_abs_shap = mean(abs(shap), na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(mean_abs_shap)
      ggplot2::ggplot(imp, ggplot2::aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
        ggplot2::geom_col(fill = "#1A5276") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "SHAP-Based Variable Importance (Mean |SHAP value|)", x = NULL, y = "Mean |SHAP value|") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }

    make_shap_beeswarm_plot <- function() {
      shap_df <- shap_results()$shap_vals
      X_df    <- shap_results()$X_explain

      long_shap <- shap_df %>%
        tibble::rowid_to_column("obs") %>%
        tidyr::pivot_longer(-obs, names_to = "feature", values_to = "shap_value")

      long_feat <- X_df %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
        tibble::rowid_to_column("obs") %>%
        tidyr::pivot_longer(-obs, names_to = "feature", values_to = "feature_value") %>%
        dplyr::group_by(feature) %>%
        dplyr::mutate(feat_norm = {
          v <- suppressWarnings(as.numeric(feature_value))
          r <- range(v, na.rm = TRUE)
          if (is.na(diff(r)) || diff(r) == 0) rep(0.5, dplyr::n()) else (v - r[1]) / diff(r)
        }) %>%
        dplyr::ungroup()

      plot_df <- dplyr::left_join(long_shap,
                                   long_feat %>% dplyr::select(obs, feature, feat_norm),
                                   by = c("obs", "feature"))

      feat_order <- plot_df %>%
        dplyr::group_by(feature) %>%
        dplyr::summarise(mean_abs = mean(abs(shap_value), na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(mean_abs) %>%
        dplyr::pull(feature)

      plot_df$feature <- factor(plot_df$feature, levels = feat_order)

      ggplot2::ggplot(plot_df, ggplot2::aes(x = shap_value, y = feature, colour = feat_norm)) +
        ggplot2::geom_jitter(height = 0.25, width = 0, alpha = 0.7, size = 1.8) +
        # Matches shapviz's sv_importance(kind = "beeswarm") default palette (test-model side),
        # which uses scale_color_viridis_c(begin = 0.25, end = 0.85, option = "inferno").
        ggplot2::scale_colour_viridis_c(begin = 0.25, end = 0.85, option = "inferno",
                                         na.value = "grey70", name = "Feature\nvalue\n(normalised)") +
        ggplot2::geom_vline(xintercept = 0, colour = "grey40", linetype = "dashed") +
        ggplot2::labs(title = "SHAP Beeswarm Plot",
                      x = "SHAP value (impact on model output)", y = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                       legend.position = "right")
    }

    make_shap_dependency_plot <- function(feat) {
      shap_df <- shap_results()$shap_vals
      X_df    <- shap_results()$X_explain
      if (is.null(feat) || !feat %in% names(shap_df)) return(NULL)
      plot_df <- data.frame(
        feature_value = suppressWarnings(as.numeric(X_df[[feat]])),
        shap_value    = shap_df[[feat]]
      ) %>% dplyr::filter(!is.na(feature_value))
      if (nrow(plot_df) < 3) return(NULL)
      ggplot2::ggplot(plot_df, ggplot2::aes(x = feature_value, y = shap_value)) +
        ggplot2::geom_point(colour = "#1A5276", alpha = 0.7, size = 2.5) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x, colour = "#00a65a",
                              se = TRUE, linewidth = 1) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        ggplot2::labs(title = paste0("SHAP Dependency Plot: ", feat),
                      x = feat, y = paste0("SHAP value for ", feat)) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }

    make_shap_waterfall_plot <- function(obs_idx) {
      shap_df <- shap_results()$shap_vals
      if (obs_idx < 1 || obs_idx > nrow(shap_df)) return(NULL)
      shap_row   <- as.numeric(shap_df[obs_idx, ])
      feat_names <- names(shap_df)
      ord        <- order(abs(shap_row))
      shap_sorted <- shap_row[ord]; feat_sorted <- feat_names[ord]
      if (length(feat_sorted) > 15) {
        keep <- tail(seq_along(feat_sorted), 15)
        shap_sorted <- shap_sorted[keep]; feat_sorted <- feat_sorted[keep]
      }
      plot_df <- data.frame(
        feature   = factor(feat_sorted, levels = feat_sorted),
        shap      = shap_sorted,
        direction = ifelse(shap_sorted >= 0, "Positive", "Negative")
      )
      ggplot2::ggplot(plot_df, ggplot2::aes(x = feature, y = shap, fill = direction)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        # Matches shapviz::sv_waterfall()'s default fill_colors (test-model side).
        ggplot2::scale_fill_manual(values = c(Positive = "#f7d13d", Negative = "#a52c60"), guide = "none") +
        ggplot2::geom_hline(yintercept = 0, colour = "grey30") +
        ggplot2::labs(title = paste0("SHAP Waterfall Plot — Observation ", obs_idx),
                      x = NULL, y = "SHAP value") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))
    }

    make_shap_force_plot <- function(obs_idx) {
      shap_df <- shap_results()$shap_vals
      if (obs_idx < 1 || obs_idx > nrow(shap_df)) return(NULL)
      shap_row   <- as.numeric(shap_df[obs_idx, ])
      feat_names <- names(shap_df)
      top        <- order(abs(shap_row), decreasing = TRUE)[seq_len(min(10, length(shap_row)))]
      plot_df <- data.frame(
        feature   = feat_names[top],
        shap      = shap_row[top],
        direction = ifelse(shap_row[top] >= 0, "Increases prediction", "Decreases prediction")
      )
      ggplot2::ggplot(plot_df, ggplot2::aes(x = reorder(feature, shap), y = shap, fill = direction)) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::coord_flip() +
        # Matches shapviz::sv_force()'s default fill_colors (test-model side).
        ggplot2::scale_fill_manual(values = c("Increases prediction" = "#f7d13d",
                                               "Decreases prediction" = "#a52c60")) +
        ggplot2::geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.8) +
        ggplot2::labs(title = paste0("SHAP Force Plot — Observation ", obs_idx),
                      subtitle = "Top 10 contributors | Dark = increases prediction, Light = decreases",
                      x = NULL, y = "SHAP value", fill = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5),
                       legend.position = "bottom")
    }

    # Resolves which plot to show for a given SHAP tab, honouring the style
    # toggle: "rautoml" tries the shared Rautoml::compute_shap() plot first
    # (see shap_results_rautoml above) and falls back to this app's own
    # custom-styled plot if that's unavailable for the current source model.
    resolve_shap_plot <- function(kind, obs_idx = NULL) {
      if (identical(input$shap_plot_style, "rautoml")) {
        rp <- shap_results_rautoml()
        if (!is.null(rp)) {
          p <- switch(kind, vi = rp$varimp, bw = rp$sv_bw, wf = rp$sv_wf, f = rp$sv_f)
          if (!is.null(p)) return(p)
        }
      }
      switch(kind,
        vi = make_shap_summary_plot(),
        bw = make_shap_beeswarm_plot(),
        wf = make_shap_waterfall_plot(obs_idx),
        f  = make_shap_force_plot(obs_idx)
      )
    }

    output$shap_summary_plot <- renderPlot({
      req(shap_results())
      tryCatch(resolve_shap_plot("vi"), error = function(e) {
        plot.new(); text(0.5, 0.5, "SHAP plot could not be generated for this model.", cex = 1.1)
      })
    })

    output$shap_beeswarm_plot <- renderPlot({
      req(shap_results())
      tryCatch(resolve_shap_plot("bw"), error = function(e) {
        plot.new(); text(0.5, 0.5, "Beeswarm plot could not be generated.", cex = 1.1)
      })
    })

    output$shap_dep_feature_ui <- renderUI({
      req(shap_results())
      feats <- names(shap_results()$shap_vals)
      selectInput(ns("shap_dep_feature"), "Select feature", choices = feats, width = "100%")
    })

    output$shap_dependency_plot <- renderPlot({
      req(shap_results(), input$shap_dep_feature)
      p <- tryCatch(make_shap_dependency_plot(input$shap_dep_feature), error = function(e) NULL)
      if (is.null(p)) { plot.new(); text(0.5, 0.5, "Dependency plot requires numeric feature values.", cex = 1.1) } else p
    })

    output$shap_obs_slider_ui <- renderUI({
      req(shap_results())
      n <- nrow(shap_results()$shap_vals)
      sliderInput(ns("shap_obs_idx"), "Select observation", min = 1, max = n, value = 1, step = 1, width = "100%")
    })
    # Kept rendering even while the slider is hidden behind the "rautoml" style
    # (conditionalPanel just toggles CSS display) -- otherwise input$shap_obs_idx
    # never gets a value until the user visits "Custom" style at least once.
    outputOptions(output, "shap_obs_slider_ui", suspendWhenHidden = FALSE)

    output$shap_waterfall_plot <- renderPlot({
      req(shap_results())
      if (!identical(input$shap_plot_style, "rautoml")) req(input$shap_obs_idx)
      p <- tryCatch(resolve_shap_plot("wf", input$shap_obs_idx), error = function(e) NULL)
      if (is.null(p)) { plot.new(); text(0.5, 0.5, "Waterfall plot not available.", cex = 1.1) } else p
    })

    output$shap_obs_slider_ui2 <- renderUI({
      req(shap_results())
      n <- nrow(shap_results()$shap_vals)
      sliderInput(ns("shap_obs_idx2"), "Select observation", min = 1, max = n, value = 1, step = 1, width = "100%")
    })
    outputOptions(output, "shap_obs_slider_ui2", suspendWhenHidden = FALSE)

    output$shap_force_plot <- renderPlot({
      req(shap_results())
      if (!identical(input$shap_plot_style, "rautoml")) req(input$shap_obs_idx2)
      p <- tryCatch(resolve_shap_plot("f", input$shap_obs_idx2), error = function(e) NULL)
      if (is.null(p)) { plot.new(); text(0.5, 0.5, "Force plot not available.", cex = 1.1) } else p
    })

    output$dl_shap_plot <- downloadHandler(paste0("shap_summary_plot_", Sys.Date(), ".png"),
      function(file) { req(shap_results()); ggplot2::ggsave(file, resolve_shap_plot("vi"), width = 7, height = 5, dpi = 150) })
    output$dl_shap_beeswarm <- downloadHandler(paste0("shap_beeswarm_", Sys.Date(), ".png"),
      function(file) { req(shap_results()); ggplot2::ggsave(file, resolve_shap_plot("bw"), width = 8, height = 6, dpi = 150) })
    output$dl_shap_dependency <- downloadHandler(paste0("shap_dependency_", Sys.Date(), ".png"),
      function(file) {
        req(shap_results(), input$shap_dep_feature)
        p <- make_shap_dependency_plot(input$shap_dep_feature)
        if (!is.null(p)) ggplot2::ggsave(file, p, width = 7, height = 5, dpi = 150)
      })
    output$dl_shap_waterfall <- downloadHandler(paste0("shap_waterfall_", Sys.Date(), ".png"),
      function(file) {
        req(shap_results())
        if (!identical(input$shap_plot_style, "rautoml")) req(input$shap_obs_idx)
        p <- resolve_shap_plot("wf", input$shap_obs_idx)
        if (!is.null(p)) ggplot2::ggsave(file, p, width = 7, height = 5, dpi = 150)
      })
    output$dl_shap_force <- downloadHandler(paste0("shap_force_", Sys.Date(), ".png"),
      function(file) {
        req(shap_results())
        if (!identical(input$shap_plot_style, "rautoml")) req(input$shap_obs_idx2)
        p <- resolve_shap_plot("f", input$shap_obs_idx2)
        if (!is.null(p)) ggplot2::ggsave(file, p, width = 7, height = 5, dpi = 150)
      })

    # -------------------------------------------------------------------------
    # IMPROVEMENT 3: Threshold Optimisation
    # -------------------------------------------------------------------------

    threshold_sweep <- reactive({
      req(source_only_results())
      res <- source_only_results(); ov <- outcome_var()
      purrr::map_dfr(seq(0.05, 0.95, by = 0.01), function(thr) {
        df <- res$scored_df %>%
          dplyr::mutate(pred_class = make_pred_class(.data[[res$prob_col]], levels(.data[[ov]]), thr))
        tryCatch({
          data.frame(
            threshold   = thr,
            F1          = yardstick::f_meas(df,    truth = !!rlang::sym(ov), estimate = pred_class, event_level = "second")$.estimate,
            Sensitivity = yardstick::sens(df,      truth = !!rlang::sym(ov), estimate = pred_class, event_level = "second")$.estimate,
            Specificity = yardstick::spec(df,      truth = !!rlang::sym(ov), estimate = pred_class, event_level = "second")$.estimate,
            Precision   = yardstick::precision(df, truth = !!rlang::sym(ov), estimate = pred_class, event_level = "second")$.estimate
          )
        }, error = function(e) data.frame(threshold = thr, F1 = NA, Sensitivity = NA, Specificity = NA, Precision = NA))
      })
    })

    make_threshold_plot <- function() {
      df  <- threshold_sweep(); cur <- input$class_threshold
      long_df <- tidyr::pivot_longer(df, cols = -threshold, names_to = "metric", values_to = "value")
      ggplot2::ggplot(long_df, ggplot2::aes(x = threshold, y = value, colour = metric)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_vline(xintercept = cur, linetype = "dashed", colour = "#1A5276", linewidth = 1) +
        ggplot2::annotate("text", x = cur + 0.02, y = 0.05,
                          label = paste0("Current: ", cur), colour = "#1A5276", hjust = 0, size = 3.5) +
        ggplot2::scale_colour_manual(values = c(F1 = "#00a65a", Sensitivity = "#2C7FB8",
                                                 Specificity = "#e67e22", Precision = "#8e44ad")) +
        ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.1)) +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        ggplot2::labs(title = "Metrics vs Classification Threshold",
                      x = "Threshold", y = "Value", colour = "Metric") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                       legend.position = "bottom")
    }

    output$threshold_plot <- renderPlot({
      req(outcome_type() == "classification", threshold_sweep()); make_threshold_plot()
    })
    output$dl_threshold_plot <- downloadHandler(paste0("threshold_optimisation_", Sys.Date(), ".png"),
      function(file) { req(threshold_sweep()); ggplot2::ggsave(file, make_threshold_plot(), width = 8, height = 5, dpi = 150) })

    # -------------------------------------------------------------------------
    # IMPROVEMENT 4: Subgroup Performance
    # -------------------------------------------------------------------------

    output$subgroup_var_ui <- renderUI({
      req(target_data(), source_schema())
      ov      <- outcome_var()
      # Include all columns except the outcome and internal helper cols
      # Predictors can be valid grouping variables (e.g. sex, site, age_group)
      exclude  <- c(ov, ".row_id", ".display_id")
      cols     <- setdiff(names(rv_current$working_df), exclude)
      cat_cols <- cols[sapply(cols, function(v) {
        vals <- rv_current$working_df[[v]]
        n    <- length(unique(stats::na.omit(vals)))
        n >= 2 && n <= 20
      })]
      if (length(cat_cols) == 0)
        return(tags$p("No suitable grouping variables found (need 2–20 unique values)."))
      selectInput(ns("subgroup_var"), "Grouping variable", choices = cat_cols, width = "100%")
    })

    subgroup_performance <- eventReactive(input$run_subgroup, {
      req(source_only_results(), input$subgroup_var)
      res    <- source_only_results(); ov <- outcome_var(); grp <- input$subgroup_var
      scored <- res$scored_df
      grp_vals <- if (grp %in% names(scored)) scored[[grp]]
                  else rv_current$working_df[[grp]][seq_len(nrow(scored))]
      scored[[".group"]] <- as.character(grp_vals)
      groups <- unique(scored[[".group"]]); groups <- groups[!is.na(groups)]

      purrr::map_dfr(groups, function(g) {
        sub <- scored[scored[[".group"]] == g & !is.na(scored[[".group"]]), ]
        if (nrow(sub) < 10) return(NULL)
        if (outcome_type() == "regression") {
          actual    <- as.numeric(sub[[ov]])
          predicted <- as.numeric(sub$.predicted_numeric)
          ok <- !is.na(actual) & !is.na(predicted)
          if (sum(ok) < 5) return(NULL)
          data.frame(group = g, n = nrow(sub),
                     rmse  = round(sqrt(mean((actual[ok] - predicted[ok])^2)), 3),
                     mae   = round(mean(abs(actual[ok] - predicted[ok])), 3),
                     stringsAsFactors = FALSE)
        } else {
          truth_tbl <- table(sub[[ov]], useNA = "no")
          if (length(truth_tbl) < 2 || any(truth_tbl == 0)) return(NULL)
          data.frame(
            group       = g, n = nrow(sub),
            auc         = round(tryCatch(yardstick::roc_auc(sub, truth = !!rlang::sym(ov), !!rlang::sym(res$prob_col), event_level = "second")$.estimate, error = function(e) NA_real_), 3),
            sensitivity = round(tryCatch(yardstick::sens(sub, truth = !!rlang::sym(ov), estimate = predicted_class, event_level = "second")$.estimate, error = function(e) NA_real_), 3),
            specificity = round(tryCatch(yardstick::spec(sub, truth = !!rlang::sym(ov), estimate = predicted_class, event_level = "second")$.estimate, error = function(e) NA_real_), 3),
            stringsAsFactors = FALSE
          )
        }
      })
    })

    make_subgroup_plot <- function() {
      df <- subgroup_performance()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      if (outcome_type() == "regression") {
        long_df <- tidyr::pivot_longer(df, cols = c(rmse, mae), names_to = "metric", values_to = "value")
        ggplot2::ggplot(long_df, ggplot2::aes(x = group, y = value, fill = metric)) +
          ggplot2::geom_col(position = "dodge") +
          ggplot2::scale_fill_manual(values = c(rmse = "#1A5276", mae = "#00a65a")) +
          ggplot2::labs(title = paste0("Subgroup Performance by: ", input$subgroup_var),
                        x = input$subgroup_var, y = "Error", fill = "Metric") +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                         plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                         legend.position = "bottom")
      } else {
        long_df <- tidyr::pivot_longer(df, cols = c(auc, sensitivity, specificity),
                                        names_to = "metric", values_to = "value")
        ggplot2::ggplot(long_df, ggplot2::aes(x = group, y = value, fill = metric)) +
          ggplot2::geom_col(position = "dodge") +
          ggplot2::scale_fill_manual(values = c(auc = "#00a65a", sensitivity = "#2C7FB8", specificity = "#e67e22")) +
          ggplot2::scale_y_continuous(limits = c(0, 1)) +
          ggplot2::labs(title = paste0("Subgroup Performance by: ", input$subgroup_var),
                        x = input$subgroup_var, y = "Value", fill = "Metric") +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                         plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                         legend.position = "bottom")
      }
    }

    output$subgroup_table <- renderTable({
      req(subgroup_performance()); subgroup_performance()
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$subgroup_plot <- renderPlot({
      req(subgroup_performance())
      p <- make_subgroup_plot()
      if (is.null(p)) { plot.new(); text(0.5, 0.5, "No subgroup results available.", cex = 1.1) } else p
    })

    output$dl_subgroup_table <- downloadHandler(paste0("subgroup_performance_", Sys.Date(), ".csv"),
      function(file) { req(subgroup_performance()); utils::write.csv(subgroup_performance(), file, row.names = FALSE) })
    output$dl_subgroup_plot  <- downloadHandler(paste0("subgroup_plot_", Sys.Date(), ".png"),
      function(file) { p <- make_subgroup_plot(); if (!is.null(p)) ggplot2::ggsave(file, p, width = 8, height = 5, dpi = 150) })

    # -------------------------------------------------------------------------
    # Step 4 — comparison
    # -------------------------------------------------------------------------

    observeEvent(input$go_to_initialize, {
      req(rv_current$working_df, input$tl_selected_model_id)
      rv_ml_ai$transfer_learning_source_model     <- primary_model_id()
      rv_ml_ai$transfer_learning_training_dataset <- input$tl_dataset_id
      rv_ml_ai$transfer_learning_training_outcome <- input$tl_outcome
      rv_ml_ai$transfer_learning_training_session <- input$tl_session_name
      rv_ml_ai$transfer_learning_requested        <- TRUE
      shinyalert::shinyalert(title = "Transfer Learning",
                              text  = "You will now be redirected to Machine Learning Initialize. After training and evaluation, return to Transfer Learning Step 4 to compare results.",
                              type  = "info")
      shinyjs::runjs("setTimeout(function() { $('a[data-value=\"setupModels\"]').click(); }, 500);")
    })

    comparison_logs <- reactive({
      req(rv_current$dataset_id); input$refresh_retrained_models
      df <- trained_models_log()
      df <- df[df$dataset_id %in% rv_current$dataset_id, , drop = FALSE]
      shiny::validate(shiny::need(nrow(df) > 0, paste0("No trained models found for active dataset: ", rv_current$dataset_id)))
      df
    })

    output$comparison_metric_picker <- renderUI({
      df <- comparison_logs(); choices <- Rautoml::extract_value_labels(df, "metric")
      selectInput(ns("comparison_metric"), "Metric", choices = choices, selected = choices[1], width = "100%")
    })

    output$comparison_session_filter <- renderUI({
      df <- comparison_logs()
      selectInput(ns("comparison_session"), "Training session",
                  choices = Rautoml::extract_value_labels(df, "session_name"), selected = NULL, width = "100%")
    })

    filtered_comparison_models <- reactive({
      req(input$comparison_metric, input$comparison_session)
      df <- comparison_logs()
      df <- df[as.character(df$session_name) %in% as.character(input$comparison_session), , drop = FALSE]
      df <- Rautoml::filter_session_metric(df = df, metric_name = input$comparison_metric,
                                            session_name_ = input$comparison_session,
                                            model_name = Rautoml::extract_value_labels(df, "model"))
      shiny::validate(shiny::need(nrow(df) > 0, "No models found for the selected session and metric."))
      # Sorted once here so DT's row-selection indices (which reflect the *displayed* order)
      # line up with the rows this same reactive hands back to score_selected_retrained_models().
      df %>% dplyr::arrange(dplyr::desc(estimate))
    })

    output$comparison_model_table <- DT::renderDataTable({
      df        <- filtered_comparison_models()
      show_cols <- intersect(c("date_trained", "model_id", "model", "dataset_id", "outcome",
                                "session_name", "framework", "metric", "lower", "estimate", "upper"), names(df))
      DT::datatable(df[, show_cols, drop = FALSE],
                    selection = list(mode = "multiple", selected = NULL, target = "row"),
                    rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) |>
        DT::formatRound(columns = intersect(c("lower", "estimate", "upper"), show_cols), digits = 4)
    })

    scoring_errors <- reactiveVal(character())

    output$compare_scoring_errors_ui <- renderUI({
      errs <- scoring_errors()
      if (length(errs) == 0) return(NULL)
      tags$div(class = "alert alert-warning",
               tags$i(class = "fa fa-exclamation-triangle"),
               sprintf(" %d of the selected model(s) could not be scored on this dataset and were skipped:", length(errs)),
               tags$ul(lapply(errs, tags$li)))
    })
    outputOptions(output, "compare_scoring_errors_ui", suspendWhenHidden = FALSE)

    score_selected_retrained_models <- eventReactive(input$compare_selected_models, {
      req(input$comparison_model_table_rows_selected, target_data(), source_schema())
      selected_rows   <- input$comparison_model_table_rows_selected
      selected_models <- filtered_comparison_models()[selected_rows, , drop = FALSE]
      shiny::validate(shiny::need(nrow(selected_models) > 0, "Please highlight at least one retrained model."))
      df <- target_data(); ov <- outcome_var(); outcome_levels <- levels(df[[ov]])
      pos_class <- tail(outcome_levels, 1); n_models <- nrow(selected_models); all_results <- list()
      failed <- character()

      # Each model is scored inside its own tryCatch so a single incompatible model or a
      # bad-dataset error (schema mismatch, prediction failure, etc.) is skipped rather than
      # aborting the whole comparison run and leaving the app hung with nothing to show.
      shiny::withProgress(message = "Scoring retrained models...", value = 0, {
        for (i in seq_len(n_models)) {
          model_id   <- as.character(selected_models$model_id[i])
          model_name <- as.character(selected_models$model[i])
          shiny::incProgress(1 / n_models, detail = paste0("Model ", i, "/", n_models, ": ", model_name))

          scored <- tryCatch({
            fit_obj <- load_pinned_model_fit(model_id)
            predictors <- setdiff(names(df), c(ov, ".row_id", ".display_id"))
            pred_df    <- df %>% dplyr::select(dplyr::all_of(predictors))

            if (outcome_type() == "regression") {
              pred_vals <- get_regression_preds(fit_obj, pred_df)
              if (is.null(pred_vals)) stop("model returned no valid numeric predictions on this dataset")
              df %>% dplyr::mutate(.predicted_numeric = pred_vals, model = model_name, model_id = model_id)
            } else {
              preds_prob <- as.data.frame(predict_source_model_any(fit_obj, pred_df, "prob"))
              if (!any(grepl("^\\.pred_", names(preds_prob)))) names(preds_prob) <- paste0(".pred_", names(preds_prob))
              prob_col <- get_prob_col(preds_prob, pos_class)
              if (is.null(prob_col) || !prob_col %in% names(preds_prob))
                stop("model did not return a usable probability column for this dataset")
              temp_df  <- dplyr::bind_cols(df, preds_prob)
              test_auc <- tryCatch(yardstick::roc_auc(temp_df, truth = !!rlang::sym(ov), !!rlang::sym(prob_col), event_level = "second")$.estimate, error = function(e) NA_real_)
              if (!is.na(test_auc) && test_auc < 0.5) preds_prob[[prob_col]] <- 1 - preds_prob[[prob_col]]
              dplyr::bind_cols(df, preds_prob) %>%
                dplyr::mutate(predicted_class = make_pred_class(.data[[prob_col]], outcome_levels, input$class_threshold),
                              model = model_name, model_id = model_id, probability = .data[[prob_col]])
            }
          }, error = function(e) {
            failed <<- c(failed, paste0(model_name, ": ", conditionMessage(e)))
            NULL
          })

          if (!is.null(scored)) all_results[[model_id]] <- scored
        }
      })

      scoring_errors(failed)
      shiny::validate(shiny::need(length(all_results) > 0,
        paste0("None of the selected retrained models could be scored on this dataset.",
               if (length(failed) > 0) paste0(" Errors: ", paste(failed, collapse = " | ")) else "")))
      dplyr::bind_rows(all_results)
    })

    compare_results <- eventReactive(input$compare_selected_models, {
      req(rv_ml_ai$transfer_source_only_results, score_selected_retrained_models())
      compare_scoring_status("running")
      result <- tryCatch({
        source_metrics <- rv_ml_ai$transfer_source_only_results$metrics
        scored_df      <- score_selected_retrained_models(); ov <- outcome_var()
        model_names    <- unique(scored_df$model)
        shiny::withProgress(message = "Computing bootstrap CIs...", value = 0, {
          retrained_metrics <- purrr::map_dfr(seq_along(model_names), function(i) {
            mn  <- model_names[i]
            sub <- scored_df %>% dplyr::filter(model == mn)
            shiny::incProgress(1 / length(model_names), detail = paste0("Model: ", mn))
            if (outcome_type() == "regression") {
              compute_regression_metrics(as.numeric(sub[[ov]]), sub$.predicted_numeric, mn, n_boot = 100)
            } else {
              compute_scored_metrics(scored_df = sub, outcome_var = ov, model_label = mn, n_boot = 100)
            }
          })
        })
        dplyr::bind_rows(source_metrics, retrained_metrics)
      }, error = function(e) { compare_scoring_status("error"); stop(e) })
      compare_scoring_status("done"); result
    })

    # ---- Step 4 plot builders ----

    make_comparison_roc_plot <- function() {
      source_roc <- source_only_results()$roc_df
      if (is.null(source_roc)) return(NULL)
      ov <- outcome_var()
      source_roc <- source_roc %>% dplyr::mutate(model = "Source Model") %>% dplyr::select(model, specificity, sensitivity)
      retrained_roc <- score_selected_retrained_models() %>%
        dplyr::group_by(model) %>%
        dplyr::group_modify(~ yardstick::roc_curve(.x, truth = !!rlang::sym(ov), probability, event_level = "second")) %>%
        dplyr::ungroup() %>% dplyr::select(model, specificity, sensitivity)
      ggplot2::ggplot(dplyr::bind_rows(source_roc, retrained_roc),
                      ggplot2::aes(x = 1 - specificity, y = sensitivity, colour = model)) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
        ggplot2::coord_equal() +
        ggplot2::labs(title = "Source vs Retrained — ROC Curves",
                      x = "1 - Specificity", y = "Sensitivity", colour = "Model") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5), legend.position = "bottom")
    }

    make_comparison_barplot <- function() {
      df <- compare_results()
      p <- ggplot2::ggplot(df, ggplot2::aes(x = metric, y = estimate, fill = model)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                               position = ggplot2::position_dodge(0.9), width = 0.25) +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Metrics Comparison (95% CI)", x = NULL, y = "Estimate", fill = "Model") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5), legend.position = "bottom")
      if (outcome_type() == "classification") p <- p + ggplot2::scale_y_continuous(limits = c(0, 1))
      p
    }

    # ---- IMPROVEMENT 6: Calibration Comparison ----

    make_comparison_calibration_plot <- function() {
      req(source_only_results(), score_selected_retrained_models())
      ov        <- outcome_var(); pos_class <- positive_class()
      src_calib <- source_only_results()$calibration
      if (is.null(src_calib)) return(NULL)
      src_calib$model <- "Source Model"
      scored_df       <- score_selected_retrained_models()
      model_names     <- unique(scored_df$model)
      retrained_calib <- purrr::map_dfr(model_names, function(mn) {
        sub <- scored_df[scored_df$model == mn, ]
        tryCatch(make_calibration_df(sub, ov, "probability", pos_class) %>% dplyr::mutate(model = mn),
                 error = function(e) NULL)
      })
      all_calib <- dplyr::bind_rows(src_calib, retrained_calib)
      ggplot2::ggplot(all_calib, ggplot2::aes(x = mean_pred, y = observed, colour = model)) +
        ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 2.5) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
        ggplot2::scale_colour_brewer(palette = "Set1") +
        ggplot2::scale_x_continuous(limits = c(0, 1)) + ggplot2::scale_y_continuous(limits = c(0, 1)) +
        ggplot2::labs(title = "Calibration Comparison", x = "Mean predicted probability",
                      y = "Observed event rate", colour = "Model") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5), legend.position = "bottom")
    }

    # ---- Residuals Comparison (regression) ----

    make_comparison_residuals_plot <- function() {
      req(source_only_results(), score_selected_retrained_models())
      ov     <- outcome_var()
      src_df <- source_only_results()$scored_df %>%
        dplyr::transmute(model = "Source Model", predicted = .predicted_numeric,
                          residual = as.numeric(.data[[ov]]) - .predicted_numeric)
      retrained_df <- score_selected_retrained_models() %>%
        dplyr::transmute(model = model, predicted = .predicted_numeric,
                          residual = as.numeric(.data[[ov]]) - .predicted_numeric)
      all_df <- dplyr::bind_rows(src_df, retrained_df)
      ggplot2::ggplot(all_df, ggplot2::aes(x = predicted, y = residual, colour = model)) +
        ggplot2::geom_point(alpha = 0.55, size = 1.8) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 1) +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linewidth = 1) +
        ggplot2::scale_colour_brewer(palette = "Set1") +
        ggplot2::labs(title = "Source vs Retrained — Residuals Comparison",
                      x = "Predicted", y = "Residual (Actual − Predicted)", colour = "Model") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5), legend.position = "bottom")
    }

    output$comparison_residuals_plot <- renderPlot({
      req(source_only_results(), score_selected_retrained_models())
      make_comparison_residuals_plot()
    })

    output$dl_comparison_residuals <- downloadHandler(paste0("comparison_residuals_", Sys.Date(), ".png"),
      function(file) { req(compare_results()); ggplot2::ggsave(file, make_comparison_residuals_plot(), width = 7, height = 6, dpi = 150) })

    output$source_vs_retrained_metrics <- renderTable({
      req(compare_results()); compare_results() %>% dplyr::arrange(metric, model)
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$comparison_note <- renderText({
      req(compare_results()); "All metrics computed on the active target dataset with 95% bootstrap CIs."
    })

    output$source_vs_retrained_roc_plot <- renderPlot({
      req(source_only_results(), score_selected_retrained_models())
      p <- make_comparison_roc_plot()
      if (is.null(p)) { plot.new(); text(0.5, 0.5, "Source ROC curve not available.", cex = 1.1) } else p
    })

    output$source_vs_retrained_barplot <- renderPlot({
      req(compare_results()); make_comparison_barplot()
    })

    output$comparison_calibration_plot <- renderPlot({
      req(source_only_results(), score_selected_retrained_models())
      p <- make_comparison_calibration_plot()
      if (is.null(p)) { plot.new(); text(0.5, 0.5, "Calibration comparison not available.", cex = 1.1) } else p
    })

    output$dl_comparison_metrics     <- downloadHandler(paste0("comparison_metrics_",     Sys.Date(), ".csv"),
      function(file) { req(compare_results()); utils::write.csv(compare_results() %>% dplyr::arrange(metric, model), file, row.names = FALSE) })
    output$dl_comparison_roc         <- downloadHandler(paste0("comparison_roc_",         Sys.Date(), ".png"),
      function(file) { p <- make_comparison_roc_plot();         if (!is.null(p)) ggplot2::ggsave(file, p, width = 7, height = 6, dpi = 150) })
    output$dl_comparison_barplot     <- downloadHandler(paste0("comparison_barplot_",     Sys.Date(), ".png"),
      function(file) { req(compare_results()); ggplot2::ggsave(file, make_comparison_barplot(), width = 7, height = 5, dpi = 150) })
    output$dl_comparison_calibration <- downloadHandler(paste0("comparison_calibration_", Sys.Date(), ".png"),
      function(file) { p <- make_comparison_calibration_plot(); if (!is.null(p)) ggplot2::ggsave(file, p, width = 7, height = 6, dpi = 150) })

    # -------------------------------------------------------------------------
    # IMPROVEMENT 5: Fine-tuning / Domain Adaptation
    # -------------------------------------------------------------------------

    finetuned_results <- eventReactive(input$run_finetune, {
      req(source_only_results(), target_data(), source_schema())
      finetune_status("running"); finetune_error_detail(NULL)
      result <- tryCatch({
        shiny::withProgress(message = "Fine-tuning model...", value = 0, {
          shiny::incProgress(0.2, detail = "Preparing data")
          res  <- source_only_results(); df <- target_data(); ov <- outcome_var(); sch <- source_schema()
          mode <- input$finetune_mode %||% "prob_only"

          if (outcome_type() == "regression") {
            ft_df <- df %>%
              dplyr::select(dplyr::all_of(c(ov, sch$predictors))) %>%
              dplyr::mutate(source_pred = res$scored_df$.predicted_numeric)
            ft_df[[ov]] <- as.numeric(ft_df[[ov]])
            ft_df <- ft_df[stats::complete.cases(ft_df), ]
            shiny::validate(shiny::need(nrow(ft_df) >= 30, "Need at least 30 complete observations for fine-tuning."))

            if (mode == "full") {
              valid_preds <- sch$predictors[sch$predictors %in% names(ft_df)]
              valid_preds <- valid_preds[sapply(valid_preds, function(v) {
                col <- ft_df[[v]]
                if (is.factor(col))  return(length(levels(droplevels(col))) >= 2)
                if (is.numeric(col)) return(!is.na(stats::var(col)) && stats::var(col) > 0)
                TRUE
              })]
              use_cols    <- c("source_pred", valid_preds)
              formula_str <- paste(ov, "~", paste(use_cols, collapse = " + "))
            } else {
              formula_str <- paste(ov, "~ source_pred")
            }
            shiny::incProgress(0.5, detail = "Fitting linear regression")
            ft_fit <- stats::lm(stats::as.formula(formula_str), data = ft_df)
            shiny::incProgress(0.75, detail = "Computing bootstrap metrics")
            ft_scored <- ft_df %>%
              dplyr::mutate(.predicted_numeric = stats::predict(ft_fit, newdata = ft_df))
            ft_metrics <- compute_regression_metrics(as.numeric(ft_df[[ov]]), ft_scored$.predicted_numeric,
                                                      "Fine-tuned (Adapted)", n_boot = 100)
            source_metrics <- res$metrics %>% dplyr::mutate(model = "Source Model (Original)")

          } else {
            ft_df <- df %>%
              dplyr::select(dplyr::all_of(c(ov, sch$predictors))) %>%
              dplyr::mutate(source_prob  = res$scored_df[[res$prob_col]],
                            .outcome_bin = ifelse(.data[[ov]] == "Positive", 1L, 0L))
            ft_df <- ft_df[stats::complete.cases(ft_df), ]
            shiny::validate(shiny::need(nrow(ft_df) >= 30, "Need at least 30 complete observations for fine-tuning."))

            if (mode == "full") {
              valid_preds <- sch$predictors[sch$predictors %in% names(ft_df)]
              valid_preds <- valid_preds[sapply(valid_preds, function(v) {
                col <- ft_df[[v]]
                if (is.factor(col))  return(length(levels(droplevels(col))) >= 2)
                if (is.numeric(col)) return(!is.na(stats::var(col)) && stats::var(col) > 0)
                TRUE
              })]
              use_cols    <- c("source_prob", valid_preds)
              formula_str <- paste(".outcome_bin ~", paste(use_cols, collapse = " + "))
            } else {
              formula_str <- ".outcome_bin ~ source_prob"
            }
            shiny::incProgress(0.5, detail = "Fitting logistic regression")
            ft_fit <- stats::glm(stats::as.formula(formula_str), data = ft_df, family = stats::binomial())
            shiny::incProgress(0.75, detail = "Computing bootstrap metrics")
            outcome_levels <- levels(df[[ov]])
            ft_scored <- ft_df %>%
              dplyr::mutate(probability     = stats::predict(ft_fit, newdata = ft_df, type = "response"),
                            predicted_class = make_pred_class(probability, outcome_levels, input$class_threshold))
            ft_scored[[ov]] <- ft_df[[ov]]
            ft_metrics <- compute_scored_metrics(scored_df = ft_scored, outcome_var = ov,
                                                  model_label = "Fine-tuned (Adapted)", n_boot = 100)
            source_metrics <- res$metrics %>% dplyr::mutate(model = "Source Model (Original)")
          }

          shiny::incProgress(1, detail = "Done")
          list(ft_scored = ft_scored, metrics = dplyr::bind_rows(source_metrics, ft_metrics), mode = mode)
        })
      }, error = function(e) { finetune_status("error"); finetune_error_detail(conditionMessage(e)); stop(e) })
      finetune_status("done"); result
    })

    make_finetune_barplot <- function() {
      df <- finetuned_results()$metrics
      ggplot2::ggplot(df, ggplot2::aes(x = metric, y = estimate, fill = model)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                               position = ggplot2::position_dodge(0.9), width = 0.25) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = c("Source Model (Original)" = "#2C7FB8",
                                               "Fine-tuned (Adapted)"    = "#00a65a")) +
        { if (outcome_type() == "classification") ggplot2::scale_y_continuous(limits = c(0, 1)) else ggplot2::scale_y_continuous() } +
        ggplot2::labs(title = "Source vs Fine-tuned Model (95% CI)", x = NULL, y = "Estimate", fill = "Model") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5), legend.position = "bottom")
    }

    output$finetune_metrics_table <- renderTable({
      req(finetuned_results()); finetuned_results()$metrics
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$finetune_barplot <- renderPlot({ req(finetuned_results()); make_finetune_barplot() })

    output$dl_finetune_metrics <- downloadHandler(paste0("finetune_metrics_", Sys.Date(), ".csv"),
      function(file) { req(finetuned_results()); utils::write.csv(finetuned_results()$metrics, file, row.names = FALSE) })
    output$dl_finetune_barplot <- downloadHandler(paste0("finetune_barplot_", Sys.Date(), ".png"),
      function(file) { req(finetuned_results()); ggplot2::ggsave(file, make_finetune_barplot(), width = 8, height = 5, dpi = 150) })

    # -------------------------------------------------------------------------
    # IMPROVEMENT 7: Export Report
    # -------------------------------------------------------------------------

    output$dl_transfer_report <- downloadHandler(
      filename = function() paste0("transfer_learning_report_", Sys.Date(), ".html"),
      content  = function(file) {
        req(source_schema(), rv_current$working_df)
        shiny::withProgress(message = "Generating report...", value = 0, {
          shiny::incProgress(0.1, detail = "Collecting results")
          secs <- input$report_sections %||% character(0)
          report_data <- list(
            report_date      = as.character(Sys.Date()),
            source_model_id  = tryCatch(selected_model_info()$model, error = function(e) "Unknown"),
            dataset_id       = rv_current$dataset_id %||% "Unknown",
            outcome_var      = tryCatch(outcome_var(), error = function(e) "Unknown"),
            schema_check     = if ("schema"   %in% secs) tryCatch(schema_compatibility(),             error = function(e) NULL) else NULL,
            shift_table      = if ("shift"    %in% secs) tryCatch(distribution_shift(),               error = function(e) NULL) else NULL,
            source_metrics   = if ("source"   %in% secs) tryCatch(source_only_results()$metrics,      error = function(e) NULL) else NULL,
            compare_metrics  = if ("compare"  %in% secs) tryCatch(compare_results(),                  error = function(e) NULL) else NULL,
            finetune_metrics = if ("finetune" %in% secs) tryCatch(finetuned_results()$metrics,        error = function(e) NULL) else NULL,
            subgroup_table   = if ("subgroup" %in% secs) tryCatch(subgroup_performance(),             error = function(e) NULL) else NULL
          )

          shiny::incProgress(0.4, detail = "Writing template")
          rmd_content <- '---
title: "Transfer Learning Report"
date: "`r params$data$report_date`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float: true
params:
  data: NULL
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
d <- params$data
```

## Summary

| Field | Value |
|---|---|
| Source model | `r d$source_model_id` |
| Target dataset | `r d$dataset_id` |
| Outcome variable | `r d$outcome_var` |
| Report date | `r d$report_date` |

```{r schema, results="asis", eval=!is.null(d$schema_check)}
cat("## Schema Compatibility\n\n")
print(knitr::kable(d$schema_check, format = "html"))
```

```{r shift, results="asis", eval=!is.null(d$shift_table)}
cat("## Distribution Shift\n\n")
print(knitr::kable(d$shift_table, format = "html", digits = 4))
```

```{r source_metrics, results="asis", eval=!is.null(d$source_metrics)}
cat("## Source Model Performance\n\n")
print(knitr::kable(d$source_metrics, format = "html", digits = 4))
```

```{r compare, results="asis", eval=!is.null(d$compare_metrics)}
cat("## Model Comparison\n\n")
print(knitr::kable(d$compare_metrics, format = "html", digits = 4))
```

```{r finetune, results="asis", eval=!is.null(d$finetune_metrics)}
cat("## Fine-tuning Results\n\n")
print(knitr::kable(d$finetune_metrics, format = "html", digits = 4))
```

```{r subgroup, results="asis", eval=!is.null(d$subgroup_table)}
cat("## Subgroup Performance\n\n")
print(knitr::kable(d$subgroup_table, format = "html", digits = 3))
```
'
          tmp_rmd <- tempfile(fileext = ".Rmd")
          writeLines(rmd_content, tmp_rmd)

          shiny::incProgress(0.7, detail = "Rendering HTML")
          rmarkdown::render(input = tmp_rmd, output_file = file,
                            params = list(data = report_data),
                            envir  = new.env(parent = globalenv()), quiet = TRUE)
          shiny::incProgress(1, detail = "Done")
        })
      }
    )

  })
}
