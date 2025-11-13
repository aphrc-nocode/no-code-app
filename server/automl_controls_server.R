automl_controls_server <- function(id, rv_current, rv_ml_ai, api_base) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- LOG HELPERS --------------------------------------------------------------

    # --- Standardization of metric column names --------------------
    # Converts session_name/seed -> stable integer for the API
    # Converts session_name/seed -> stable integer for the API (without overflow)
    .to_session_id_int <- function(session_name = NULL, seed_value = NULL, seed = NULL) {
      # 1) if the user has entered an integer (or numeric string), it is respected
      if (!is.null(session_name)) {
        s_trim <- trimws(as.character(session_name))
        if (grepl("^-?\\d+$", s_trim)) {
          val <- suppressWarnings(as.numeric(s_trim))
          if (is.finite(val)) return(as.integer(val %% 2147483647))
        }
      }
      # 2) otherwise: light hash, calculated twice to avoid overflow
      s <- as.character(session_name %||% seed_value %||% seed %||% "123")
      bytes <- utf8ToInt(s)
      if (!length(bytes)) return(123L)

      h <- 0.0                          # <- duplicate (no overflow)
      for (b in bytes) {
        h <- (h * 131 + b) %% 2147483647
      }
      h_int <- as.integer(h)
      if (is.na(h_int) || h_int == 0L) 123L else h_int
    }


    .std_metric_colnames <- function(df) {
      if (is.null(df) || !is.data.frame(df)) return(df)
      if (!length(names(df))) return(df)
      nm <- names(df)

      # alias table -> canonical name
      norm_map <- c(
        "auc"         = "AUC",
        "roc_auc"     = "AUC",
        "rocauc"      = "AUC",
        "accuracy"    = "Accuracy",
        "acc"         = "Accuracy",
        "f1"          = "F1",
        "f1_score"    = "F1",
        "recall"      = "Recall",
        "tpr"         = "Recall",
        "sensitivity" = "Recall",
        "prec"        = "Precision",
        "prec."       = "Precision",
        "precision"   = "Precision",
        "specificity" = "Specificity",
        "kappa"       = "Kappa",
        "mcc"         = "MCC",
        "r2"          = "R2",
        "rmse"        = "RMSE",
        "mae"         = "MAE",
        "mape"        = "MAPE",
        "rmsle"       = "RMSLE"
      )

      # normalization key: tolower + remove spaces/periods/underscores
      keyize <- function(x) gsub("[\\s\\._]+", "", tolower(x))

      keys <- keyize(nm)
      for (i in seq_along(nm)) {
        k <- keys[i]
        if (k %in% names(norm_map)) nm[i] <- norm_map[[k]]
      }
      names(df) <- nm
      df
    }

    # Select the first available metric from a list of options
    .first_present_numeric <- function(df, candidates) {
      for (m in candidates) {
        if (m %in% names(df)) {
          v <- suppressWarnings(as.numeric(df[[m]]))
          if (any(is.finite(v))) return(m)
        }
      }
       # fallback: first numeric column
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      if (length(num_cols)) return(num_cols[1])
      NA_character_
    }

        # --- Standardization of data frames for metrics ----------------------------
    .clean_colnames <- function(df) {
      if (is.null(df) || !is.data.frame(df)) return(df)
      nm <- names(df)
      nm <- trimws(nm)
      nm <- gsub("\\u00A0", " ", nm, useBytes = TRUE)     # NBSP -> space
      nm <- gsub("\\s+", "", nm)                          # drop spaces
      names(df) <- nm
      df
    }

    .force_numeric_cols <- function(df) {
      if (is.null(df) || !is.data.frame(df)) return(df)
      is_numlike <- function(x) {
        # character vector that looks like numeric (NA ok)
        if (!is.character(x)) return(FALSE)
        ok <- grepl("^[-+]?\\d*(?:\\.\\d+)?(?:[eE][-+]?\\d+)?$", trimws(x)) | is.na(x)
        all(ok)
      }
      for (n in names(df)) {
        x <- df[[n]]
        if (is_numlike(x)) {
          df[[n]] <- suppressWarnings(as.numeric(trimws(x)))
        }
      }
      df
    }

    .normalize_metrics_df <- function(df) {
      df <- .clean_colnames(df)
      df <- .force_numeric_cols(df)
      df
    }

    .dir_models_logs <- function() {
      d <- file.path(getwd(), "logs", "models")
      if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
      d
    }
    .models_index_csv <- function() file.path(.dir_models_logs(), "index.csv")

    .safe_chr1 <- function(x, default = NA_character_) {
      if (is.null(x) || length(x) == 0) return(default)
      s <- as.character(x[1]); if (!nzchar(s)) default else s
    }

    # --- SAFE ACCESSORS (avoid all [[ vectors) -------------------------------
    .get1 <- function(x, key) {
      if (is.null(x) || is.null(key) || length(key) == 0) return(NULL)
      if (length(key) > 1) key <- key[1]
      if (is.character(key)) {
        if (!length(names(x)) || !(key %in% names(x))) return(NULL)
        x[[key]]
      } else {
        key <- as.integer(key[1])
        if (is.na(key) || key < 1 || key > length(x)) return(NULL)
        x[[key]]
      }
    }
    .col1 <- function(df, name) {
      if (is.null(df) || !is.data.frame(df) || is.null(name) || length(name) == 0) return(NULL)
      nm <- as.character(name[1])
      if (!(nm %in% names(df))) return(NULL)
      df[[nm]]
    }
    .cols <- function(df, names_vec) {
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      nn <- intersect(as.character(names_vec), names(df))
      if (!length(nn)) return(NULL)
      df[, nn, drop = FALSE]
    }

      # --- MEASURING TOOLS ------------------------------------------------------------
    .pick_primary_metric <- function(lb, task = "classification") {
      if (is.null(lb) || !NROW(lb)) return(list(name = NA_character_, value = NA_real_))

      # standardize column names
      lb <- .std_metric_colnames(lb)

      cand_cls <- c("AUC","Accuracy","F1","Recall","Precision","Specificity","Kappa","MCC")
      cand_reg <- c("R2","RMSE","MAE","MAPE","RMSLE")
      cols <- if (tolower(task) %in% c("regression")) cand_reg else cand_cls

      m <- .first_present_numeric(lb, cols)
      if (is.na(m)) return(list(name = NA_character_, value = NA_real_))

      v <- suppressWarnings(as.numeric(lb[[m]][1]))
      list(name = m, value = v)
    }


    .metric_candidates <- function(df, task = "classification") {
      if (is.null(df) || !NROW(df)) return(character(0))
      df <- .std_metric_colnames(df)

      cand_cls <- c("AUC","Accuracy","F1","Recall","Precision","Specificity","Kappa","MCC")
      cand_reg <- c("R2","RMSE","MAE","MAPE","RMSLE")

      # only display columns that are present and numeric
      present_num <- names(df)[vapply(df, function(x) {
        any(is.finite(suppressWarnings(as.numeric(x))))
      }, logical(1))]

      if (tolower(task) %in% c("regression")) {
        inter <- intersect(cand_reg, present_num)
        if (length(inter)) return(inter)
        return(present_num)
      } else {
        inter <- intersect(cand_cls, present_num)
        if (length(inter)) return(inter)
        return(present_num)
      }
    }

      write_model_log <- function(date_trained, dataset_id, outcome,
                                  session_name = NA_character_, session_id = NA_integer_,
                                  framework = "PyCaret",
                                  model_code, model_label = NA_character_,
                                  metric_name, estimate_value, path_model = NA_character_) {

        idx <- .models_index_csv()
        row <- data.frame(
          date_trained = .safe_chr1(format(as.POSIXct(date_trained), "%Y-%m-%d %H:%M:%S")),
          dataset_id   = .safe_chr1(dataset_id),
          outcome      = .safe_chr1(outcome),
          session_name = .safe_chr1(session_name),
          session_id   = suppressWarnings(as.integer(session_id)),
          framework    = .safe_chr1(framework, "PyCaret"),
          model        = .safe_chr1(tolower(gsub("\\s+", "", model_code))),  # <-- still the CODE
          model_label  = .safe_chr1(model_label),                            # <-- separate label
          metric       = .safe_chr1(metric_name),
          estimate     = suppressWarnings(as.numeric(estimate_value)),
          path_model   = .safe_chr1(path_model),
          stringsAsFactors = FALSE
        )

        if (!file.exists(idx)) {
          utils::write.table(row, idx, sep = ",", row.names = FALSE, col.names = TRUE,  append = FALSE, qmethod = "double", na = "")
        } else {
          utils::write.table(row, idx, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE,  qmethod = "double", na = "")
        }
      }


    `%||%` <- function(a, b) if (is.null(a)) b else a

    # ---------- Helpers parsing ---------------------------------------------------
    .parse_records_df <- function(x) {
      if (is.null(x)) return(NULL)
      out <- tryCatch(jsonlite::fromJSON(jsonlite::toJSON(x), simplifyDataFrame = TRUE),
                      error = function(e) NULL)
      if (is.null(out)) {
        out <- tryCatch(
          do.call(rbind, lapply(x, function(r) as.data.frame(r, stringsAsFactors = FALSE))),
          error = function(e) NULL
        )
      }
      if (is.null(out)) out <- as.data.frame(x, stringsAsFactors = FALSE)
      out
    }
    .rename_if <- function(df, old, new) {
      if (!is.null(df) && old %in% names(df)) names(df)[names(df) == old] <- new
      df
    }
    .norm_models_map <- function(m) {
      if (is.null(m)) return(NULL)
      if (is.list(m)) m <- unlist(m, use.names = TRUE)
      m <- as.character(m)
      if (is.null(names(m))) return(NULL)
      m
    }
    .scalar_chr_or_null <- function(x) {
      if (is.null(x) || length(x) < 1) return(NULL)
      s <- as.character(x[1]); if (!nzchar(s) || is.na(s)) return(NULL); s
    }
    .get_topn_codes <- function(lb, models_map, n = 5) {
      if (is.null(lb) || !NROW(lb)) return(character(0))
      if (!("Model" %in% names(lb))) {
        if ("model_name" %in% names(lb)) lb$Model <- lb$model_name else lb$Model <- as.character(seq_len(NROW(lb)))
      }
      n <- min(n, NROW(lb))
      labels <- head(lb$Model, n)
      mm <- .norm_models_map(models_map)
      if (!is.null(mm) && length(mm)) {
        keep <- labels[labels %in% names(mm)]
        if (length(keep)) return(unname(as.character(mm[keep])))
      }
      if ("id" %in% names(lb)) return(head(as.character(lb$id), n))
      character(0)
    }
    .all_idle <- function(busy) {
      is.list(busy) && isTRUE(!busy$train) && isTRUE(!busy$test) && isTRUE(!busy$eval)
    }

    # === PIPELINE ================================================================
    observeEvent(input$launch_automl, {
      showNotification("Launch AutoML clicked", type = "message")

      # --- Basic checks
      req(isTruthy(rv_current$working_df))
      req(isTruthy(rv_ml_ai$outcome))

      # --- Reset global state + FLAGS anti-double appel
      rv_ml_ai$run_id            <- (rv_ml_ai$run_id %||% 0) + 1
      local_run_id               <- rv_ml_ai$run_id
      rv_ml_ai$status            <- "Running"
      rv_ml_ai$leaderboard       <- NULL
      rv_ml_ai$leaderboard_full  <- NULL
      rv_ml_ai$models            <- NULL
      rv_ml_ai$test_leaderboard  <- NULL
      rv_ml_ai$eval_metrics      <- NULL
      rv_ml_ai$eval_plots        <- NULL
      rv_ml_ai$compute_source    <- list(test="controls", eval="controls")
      rv_ml_ai$busy              <- list(train=TRUE, test=FALSE, eval=FALSE)

      # Go to the Train tab if present
      try(updateTabItems(session, "tabs", "trainModel"), silent = TRUE)

      # --- Settings
      df            <- rv_current$working_df
      outcome       <- rv_ml_ai$outcome

      # Current dataset for this run
      dataset_id <- rv_current$dataset_id
      req(!is.null(dataset_id) && nzchar(dataset_id))

      # Memorize the dataset associated with the last PyCaret run
      rv_ml_ai$trained_dataset_id <- dataset_id
      #seed          <- rv_ml_ai$seed_value %||% rv_ml_ai$seed %||% 123
      seed          <- suppressWarnings(as.integer(rv_ml_ai$seed_value %||% rv_ml_ai$seed))
      #session_name <- rv_ml_ai$session_name %||% paste0("session_", seed)
      session_name  <- rv_ml_ai$session_name %||% paste0("session_", (rv_ml_ai$seed_value %||% rv_ml_ai$seed %||% 123))
      session_id_int <- .to_session_id_int(session_name, seed_value = rv_ml_ai$seed_value, seed = rv_ml_ai$seed)
      train_ratio   <- rv_ml_ai$train_size %||% rv_ml_ai$train_ratio %||% 0.8
      task_raw      <- tolower(as.character(rv_ml_ai$task %||% rv_ml_ai$analysis_type %||% "classification"))
      analysis_type <- if (task_raw %in% c("classification","regression","supervised")) "supervised" else "unsupervised"

      # Temporary CSV (reused later)
      tmpfile <- tempfile(fileext = ".csv")
      on.exit(unlink(tmpfile), add = TRUE)
      utils::write.csv(df, tmpfile, row.names = FALSE)

      withProgress(message = "Running AutoML pipeline…", value = 0, {

        # ------------------------------ 1) /automl (TRAIN) ------------------------
        setProgress(0.05, detail = "Training (PyCaret /automl)")
        res <- tryCatch(
          httr::POST(
            url   = paste0(api_base, "/automl"),
            body  = list(
              file          = httr::upload_file(tmpfile),
              target        = .scalar_chr_or_null(outcome),
              #session_id    = .scalar_chr_or_null(seed),
              session_id    = .scalar_chr_or_null(session_id_int),
              train_size    = .scalar_chr_or_null(train_ratio),
              task          = .scalar_chr_or_null(task_raw),
              analysis_type = .scalar_chr_or_null(analysis_type)
            ),
            encode = "multipart",
            httr::timeout(600),
            verbose()
          ),
          error = function(e) e
        )
        if (inherits(res, "error")) {
          rv_ml_ai$status <- "Failed"; rv_ml_ai$busy$train <- FALSE
          showNotification(paste("AutoML call failed:", res$message), type = "error", duration = 8)
          return(invisible(NULL))
        }
        if (httr::http_error(res)) {
          rv_ml_ai$status <- paste0("Failed: HTTP ", httr::status_code(res)); rv_ml_ai$busy$train <- FALSE
          txt <- httr::content(res, as = "text", encoding = "UTF-8")
          showNotification(paste("HTTP error:", httr::status_code(res), substr(txt, 1, 200)), type = "error", duration = 10)
          return(invisible(NULL))
        }
        out <- tryCatch(httr::content(res, as = "parsed", type = "application/json"),
                        error = function(e) NULL)
        if (is.null(out)) {
          rv_ml_ai$status <- "Failed"; rv_ml_ai$busy$train <- FALSE
          showNotification("Invalid JSON payload from /automl.", type = "error", duration = 8)
          return(invisible(NULL))
        }
        lb <- .parse_records_df(out$leaderboard)
        lb <- .normalize_metrics_df(lb)
        lb <- .std_metric_colnames(lb)

        if (is.null(lb) || !NROW(lb)) {
          rv_ml_ai$status <- "Failed"; rv_ml_ai$busy$train <- FALSE
          showNotification("AutoML returned an empty leaderboard.", type = "warning", duration = 8)
          return(invisible(NULL))
        }
        lb <- .rename_if(lb, "model_name", "Model")
        lb <- .rename_if(lb, "model_id",  "id")
        rv_ml_ai$leaderboard       <- lb
        rv_ml_ai$leaderboard_full  <- lb

        # mapping “readable label -> pycaret code”
        if (all(c("id","Model") %in% colnames(lb))) {
          rv_ml_ai$models <- stats::setNames(lb$id, lb$Model)
        } else if (!is.null(out$models)) {
          m <- out$models; if (is.list(m)) m <- unlist(m, use.names = TRUE)
          m <- as.character(m)
          rv_ml_ai$models <- stats::setNames(m, names(m))
        } else if ("Model" %in% colnames(lb)) {
          ids <- tolower(gsub("[^a-z0-9]+", "_", lb$Model))
          rv_ml_ai$models <- stats::setNames(ids, lb$Model)
        } else {
          rv_ml_ai$models <- NULL
        }
        rv_ml_ai$busy$train <- FALSE

        # -------------------------- 2) /test_leaderboard (TEST) -------------------
        setProgress(0.45, detail = "Evaluating on TEST (/test_leaderboard)")
        rv_ml_ai$busy$test <- TRUE; rv_ml_ai$test_source <- "controls"

        lb <- rv_ml_ai$leaderboard
        all_ids <- unname(rv_ml_ai$models[ match(lb$Model, names(rv_ml_ai$models)) ])
        all_ids <- as.character(all_ids)
        all_ids <- all_ids[!is.na(all_ids) & nzchar(all_ids)]

        res_test <- tryCatch(
          httr::POST(
            url = paste0(api_base, "/test_leaderboard"),
            body = list(
              file       = httr::upload_file(tmpfile, type = "text/csv"),
              target     = .scalar_chr_or_null(outcome),
              #session_id = .scalar_chr_or_null(seed),
              session_id = .scalar_chr_or_null(session_id_int),
              train_size = .scalar_chr_or_null(train_ratio),
              model_ids  = jsonlite::toJSON(all_ids, auto_unbox = FALSE)
            ),
            encode = "multipart",
            httr::add_headers(`accept` = "application/json"),
            httr::timeout(600)
          ),
          error = function(e) e
        )

        if (inherits(res_test, "error")) {
          rv_ml_ai$status <- "Failed"; rv_ml_ai$busy$test <- FALSE
          showNotification(paste("Test leaderboard call failed:", res_test$message), type = "error", duration = 8)
          return(invisible(NULL))
        }
        if (!inherits(res_test, "response") || httr::http_error(res_test)) {
          rv_ml_ai$status <- paste0("Failed: HTTP ", httr::status_code(res_test)); rv_ml_ai$busy$test <- FALSE
          txt <- try(httr::content(res_test, as = "text", encoding = "UTF-8"), silent = TRUE)
          showNotification(paste("HTTP error (test):", httr::status_code(res_test), substr(as.character(txt), 1, 200)),
                           type = "error", duration = 10)
          return(invisible(NULL))
        }

        out_test <- tryCatch(httr::content(res_test, as = "parsed", type = "application/json"),
                             error = function(e) NULL)
        tbl <- out_test$test_leaderboard %||% out_test$test_performance
        df_test <- if (!is.null(tbl) && length(tbl) > 0) .parse_records_df(tbl) else NULL
        df_test <- .normalize_metrics_df(df_test)
        df_test <- .std_metric_colnames(df_test)


        df_test <- .rename_if(df_test, "model_name", "Model")
        df_test <- .rename_if(df_test, "model_id",  "id")

        expected_models <- lb$Model
        if (!is.null(df_test) && NROW(df_test)) {
          by_cols <- intersect(c("Model","id"), intersect(names(df_test), names(lb)))
          if (!length(by_cols)) {
            df_aligned <- data.frame(Model = expected_models, stringsAsFactors = FALSE)
          } else {
            df_aligned <- merge(
              x = lb[, by_cols, drop = FALSE],
              y = df_test,
              by = by_cols,
              all.x = TRUE,
              sort = FALSE
            )
            if (!("Model" %in% names(df_aligned)) && "Model" %in% names(lb)) {
              df_aligned$Model <- lb$Model
            }
          }
        } else {
          df_aligned <- data.frame(Model = expected_models, stringsAsFactors = FALSE)
        }

        rv_ml_ai$test_leaderboard_full <- df_aligned
        rv_ml_ai$test_leaderboard      <- df_aligned
        # assure la normalisation même après merge
        rv_ml_ai$test_leaderboard_full <- .normalize_metrics_df(rv_ml_ai$test_leaderboard_full)
        rv_ml_ai$test_leaderboard      <- .normalize_metrics_df(rv_ml_ai$test_leaderboard)

        rv_ml_ai$busy$test <- FALSE
        rv_ml_ai$test_leaderboard_computed_at <- Sys.time()

        # === Suggest metrics for the Validate & Deploy tab ===============
      # === after filling in rv_ml_ai$leaderboard / test_leaderboard_full ===
      task_guess <- tolower(rv_ml_ai$task %||% rv_ml_ai$analysis_type %||% "classification")
      df_metrics_src <- rv_ml_ai$test_leaderboard_full %||% rv_ml_ai$leaderboard
      df_metrics_src <- .normalize_metrics_df(df_metrics_src)
      df_metrics_src <- .std_metric_colnames(df_metrics_src)

      metric_choices <- .metric_candidates(df_metrics_src, task_guess)
      default_metric <- if ("Accuracy" %in% metric_choices) "Accuracy" else {
        .pick_primary_metric(df_metrics_src, task_guess)$name
      }
      rv_ml_ai$deploy_metric <- default_metric

      try(updateSelectInput(session, "deploy_metric",
                            choices = metric_choices,
                            selected = rv_ml_ai$deploy_metric),
          silent = TRUE)


        # ------------------------------ 3) /evaluate_model (PLOTS) ----------------
        setProgress(0.75, detail = "Generating plots (/evaluate_model)")
        rv_ml_ai$busy$eval <- TRUE

        # ROBUST selection of a model_id
        best_model_code  <- NA_character_
        best_model_label <- .safe_chr1(rv_ml_ai$leaderboard$Model) %||% .safe_chr1(names(rv_ml_ai$models))
        mm <- .norm_models_map(rv_ml_ai$models)

        if (!is.null(rv_ml_ai$leaderboard) && NROW(rv_ml_ai$leaderboard) > 0 && ("id" %in% names(rv_ml_ai$leaderboard))) {
          cand <- as.character(rv_ml_ai$leaderboard$id[1])
          if (!is.na(cand) && nzchar(cand)) best_model_code <- cand
        }
        if ((is.na(best_model_code) || !nzchar(best_model_code)) && !is.null(mm) && length(mm) > 0) {
          cand <- unname(as.character(mm[1]))
          if (!is.na(cand) && nzchar(cand)) best_model_code <- cand
        }
        if ((is.na(best_model_code) || !nzchar(best_model_code)) && !is.null(best_model_label)) {
          cand <- tolower(gsub("[^a-z0-9]+","_", best_model_label))
          if (!is.na(cand) && nzchar(cand)) best_model_code <- cand
        }

        # dataset/session to correctly name the model on the API side
        slugify <- function(s) {
          s <- tolower(trimws(s %||% ""))
          s <- gsub("[^a-z0-9\\-]+", "_", s)
          s <- gsub("_+", "_", s)
          s <- gsub("^_|_$", "", s)
          s
        }
        dataset_id <- rv_ml_ai$trained_dataset_id %||% rv_current$dataset_id
        req(is.character(dataset_id) && nzchar(dataset_id))

        #req(!is.null(dataset_id) && nzchar(dataset_id))

        session_name <- rv_ml_ai$session_name %||% paste0("session_", (rv_ml_ai$seed_value %||% rv_ml_ai$seed %||% 123))

        plots_ok <- FALSE
        if (isTRUE(nzchar(best_model_code))) {
          body_eval <- list(
            file       = httr::upload_file(tmpfile, type = "text/csv"),
            target     = .scalar_chr_or_null(outcome),
            model_id   = .scalar_chr_or_null(best_model_code),
            #session_id = .scalar_chr_or_null(seed)
            session_id = .scalar_chr_or_null(session_id_int),
            session_name = .scalar_chr_or_null(session_name),
            dataset_id   = .scalar_chr_or_null(dataset_id) 
          )
          nm <- .scalar_chr_or_null(best_model_label)
          if (!is.null(nm)) body_eval$model_name <- nm

          res_eval <- tryCatch(
            httr::POST(
              url = paste0(api_base, "/evaluate_model"),
              body = body_eval,
              encode = "multipart",
              httr::add_headers(`accept` = "application/json"),
              httr::timeout(600),
              verbose()
            ),
            error = function(e) e
          )
          if (inherits(res_eval, "response") && !httr::http_error(res_eval)) {
            out_eval <- tryCatch(httr::content(res_eval, as = "parsed", type = "application/json"),
                                 error = function(e) NULL)
            if (!is.null(out_eval)) {
              rv_ml_ai$eval_metrics <- out_eval$metrics %||% NULL
              rv_ml_ai$eval_plots   <- out_eval$plots   %||% NULL
               # -- expose aussi un cache "prefetch" pour l'onglet Train model --
              rv_ml_ai$prefetch <- list(
                best_id = best_model_code,
                metrics = out_eval$metrics,
                plots   = out_eval$plots,
                extras  = out_eval$extras
              )
              plots_ok <- TRUE
            }
          } else {
            msg <- if (inherits(res_eval, "response")) paste("HTTP", httr::status_code(res_eval)) else conditionMessage(res_eval)
            showNotification(paste("Evaluate error:", msg), type = "warning", duration = 8)
          }
        } else {
          showNotification("No usable best model to evaluate.", type = "warning", duration = 6)
        }
        rv_ml_ai$busy$eval <- FALSE

        # ------------------------------ 4) Final status ----------------------------
        setProgress(1.0, detail = if (plots_ok) "Done" else "Done (no plots)")
        if (.all_idle(rv_ml_ai$busy)) {
          rv_ml_ai$status <- if (plots_ok) "Finished" else "Finished_NoPlots"
          showNotification(
            if (plots_ok) "AutoML complete: Train + Test + Plots ready"
            else          "AutoML completed: Train + Test ready (plots unavailable)",
            type = if (plots_ok) "message" else "warning", duration = 5
          )
        } else {
          rv_ml_ai$status <- "Running"
        }

        # ===== PREFETCH: calculate ROC/CM/FI/SHAP of the best model immediately =====
        best_code <- NULL
        lb <- rv_ml_ai$leaderboard
        if (!is.null(lb) && NROW(lb) > 0) {
          if ("id" %in% names(lb)) {
            best_code <- tolower(trimws(lb$id[1]))
          } else if ("Model" %in% names(lb)) {
            best_code <- tolower(trimws(lb$Model[1]))
          }
        }
        if (!is.null(best_code) && nzchar(best_code)) {
          # 1) Current CSV
          tmpfile <- file.path(tempdir(), "prefetch_eval.csv")
          write.csv(isolate(rv_current$working_df %||% rv_current$dataset), tmpfile, row.names = FALSE)

          # 2) canonical dataset_id (required on the API side)
          ds_id <- isolate(rv_ml_ai$dataset_id %||% rv_current$dataset_id)
          req(is.character(ds_id) && nzchar(ds_id))

          # 3) body for /evaluate_model
          body_eval <- list(
            file         = httr::upload_file(tmpfile),
            target       = isolate(rv_ml_ai$outcome),
            model_id     = best_code,
            session_id   = isolate(rv_ml_ai$seed_value),
            session_name = isolate(rv_ml_ai$session_id),
            train_size   = isolate(rv_ml_ai$train_size %||% 0.8),
            dataset_id   = ds_id
          )

          # 4) synchronous call (calculated immediately)
          res <- try(httr::POST(paste0(api_base, "/evaluate_model"),
                                body = body_eval, encode = "multipart",
                                httr::timeout(600)), silent = TRUE)
          if (!inherits(res, "try-error") && !httr::http_error(res)) {
            payload <- try(httr::content(res, as = "parsed"), silent = TRUE)
            if (is.list(payload)) {
                rv_ml_ai$prefetch <- list(
                  best_id = best_code,
                  metrics = payload$metrics,
                  plots   = payload$plots,
                  extras  = payload$extras
                )
            }
          }
        }
        # ---- LOG ---------------------------------------------------------------
        task_guess <- tolower(rv_ml_ai$task %||% rv_ml_ai$analysis_type %||% "classification")
        best_label <- .safe_chr1(rv_ml_ai$leaderboard$Model) %||% .safe_chr1(names(rv_ml_ai$models))
        best_code  <- if (!is.null(rv_ml_ai$models) && !is.null(best_label)) .get1(rv_ml_ai$models, best_label) else NA_character_
        pm <- .pick_primary_metric(rv_ml_ai$leaderboard, task_guess)

        best_path <- {
          base <- file.path("models", paste0(tolower(best_code %||% gsub("[^a-z0-9]+","_", best_label)),
                                            "__", slugify(dataset_id), ".pkl"))
          if (file.exists(base)) base else NA_character_
        }


        #dataset_id   <- rv_current$dataset_id %||% (rv_current$raw_filename %||% "dataset.csv")
        dataset_id <- rv_ml_ai$trained_dataset_id %||% rv_current$dataset_id
        req(is.character(dataset_id) && nzchar(dataset_id))
        session_name <- rv_ml_ai$session_name %||% paste0("session_", (rv_ml_ai$seed_value %||% rv_ml_ai$seed %||% 123))

        write_model_log(
          date_trained = Sys.time(),
          dataset_id   = dataset_id,
          outcome      = rv_ml_ai$outcome,
          session_name = session_name,
          session_id   = session_id_int,
          framework    = "PyCaret",
          model_code   = best_code %||% tolower(gsub("[^a-z0-9]+","_", best_label)),
          model_label  = best_label,
          metric       = pm$name,
          estimate     = pm$value,
          path_model   = best_path
        )

      }) # withProgress
    })

    # ====== VALIDATE & DEPLOY: metric-driven table ======================
    .deploy_metrics_df <- reactive({
      rv_ml_ai$test_leaderboard_full %||% rv_ml_ai$leaderboard
    })

    observeEvent(input$deploy_metric, {
      if (!is.null(input$deploy_metric) && nzchar(input$deploy_metric)) {
        rv_ml_ai$deploy_metric <- input$deploy_metric
      }
    }, ignoreInit = TRUE)

    deploy_table <- reactive({
      df <- .deploy_metrics_df()
      req(!is.null(df), NROW(df) > 0)

       df <- .std_metric_colnames(df)

      model_col <- if ("Model" %in% names(df)) "Model" else if ("model_name" %in% names(df)) "model_name" else NA_character_
      id_col    <- if ("id"    %in% names(df)) "id"    else if ("model_id"   %in% names(df)) "model_id"   else NA_character_

      models <- if (!is.na(model_col)) as.character(df[[model_col]]) else {
        if (!is.na(id_col)) as.character(df[[id_col]]) else as.character(seq_len(NROW(df)))
      }

      m <- rv_ml_ai$deploy_metric %||% input$deploy_metric
      if (is.null(m) || !(m %in% names(df))) {
        task_guess <- tolower(rv_ml_ai$task %||% rv_ml_ai$analysis_type %||% "classification")
        m <- .pick_primary_metric(df, task_guess)$name
      }

      est <- if (!is.null(m) && (m %in% names(df))) suppressWarnings(as.numeric(df[[m]])) else rep(NA_real_, NROW(df))

      dataset_id <- rv_current$dataset_id
      req(!is.null(dataset_id) && nzchar(dataset_id))
      session_name <- rv_ml_ai$session_name %||% paste0("session_", (rv_ml_ai$seed_value %||% rv_ml_ai$seed %||% 123))

      out <- data.frame(
        date_trained = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        dataset_id   = dataset_id,
        outcome      = rv_ml_ai$outcome %||% NA_character_,
        session_name = session_name,
        framework    = "PyCaret",
        model        = models,
        metric       = m,
        estimate     = est,
        stringsAsFactors = FALSE
      )
      if (all(!is.na(out$estimate))) out <- out[order(-out$estimate), , drop = FALSE]
      out
    })

    output$deploy_table <- DT::renderDT({
      req(deploy_table())
      DT::datatable(
        deploy_table(),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

  })
}
