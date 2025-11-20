train_model_server <- function(id, rv_ml_ai, rv_current, api_base) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---------- Helpers ----------
    # ---- Helpers prefetch ----
    .use_prefetch <- function(rv_ml_ai, code = NULL) {
      pf <- rv_ml_ai$prefetch
      if (is.null(pf)) return(NULL)
      if (!is.null(code) && nzchar(code)) {
        if (!identical(tolower(code), tolower(pf$best_id %||% ""))) return(NULL)
      }
      pf
    }
    .render_from_prefetch <- function(ns, rv_ml_ai, output) {
      pf <- rv_ml_ai$prefetch
      if (is.null(pf)) return(invisible(NULL))

      # 1) metrics (test table)
      mdf <- tryCatch({
        if (is.data.frame(pf$metrics)) pf$metrics else {
          o <- jsonlite::fromJSON(jsonlite::toJSON(pf$metrics), simplifyDataFrame = TRUE)
          as.data.frame(o, stringsAsFactors = FALSE)
        }
      }, error = function(e) NULL)
      if (!is.null(mdf)) {
        output$metrics_table <- renderUI({ DT::dataTableOutput(ns("metrics_dt")) })
        output$metrics_dt <- DT::renderDataTable({
          DT::datatable(mdf, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
        })
      } else {
        output$metrics_table <- renderUI(tags$em("No metrics available"))
      }

      # 2) plots (b64)
      p <- pf$plots %||% list()
      roc_b64  <- p$roc  %||% p$auc
      cm_b64   <- p$confusion %||% p$confusion_matrix
      fi_b64   <- p$importance %||% p$feature %||% p$feature_importance
      shap_b64 <- p$shap_summary %||% p$shap

      output$roc_ui  <- renderUI(render_b64_img(roc_b64))
      output$cm_ui   <- renderUI(render_b64_img(cm_b64))
      output$fi_ui   <- renderUI(render_b64_img(fi_b64))
      output$shap_ui <- renderUI(render_b64_img(shap_b64))

      # 3) sélectionner le bon onglet
      if (has_b64(roc_b64)) {
        updateTabsetPanel(getDefaultReactiveDomain(), "plots_tabs", selected = "ROC Curve")
      } else if (has_b64(cm_b64)) {
        updateTabsetPanel(getDefaultReactiveDomain(), "plots_tabs", selected = "Confusion Matrix")
      } else if (has_b64(fi_b64)) {
        updateTabsetPanel(getDefaultReactiveDomain(), "plots_tabs", selected = "Feature Importance")
      } else {
        updateTabsetPanel(getDefaultReactiveDomain(), "plots_tabs", selected = "SHAP Values")
      }
    }

    `%||%` <- function(a, b) { if (!is.null(a)) { if (is.character(a)) { if (length(a) > 0 && nzchar(a[1])) return(a) } else { return(a) } } ; b }
    has_b64 <- function(x) is.character(x) && length(x) == 1 && nzchar(x)
    render_b64_img <- function(x, error_msg = NULL) {
      is_b64 <- is.character(x) && length(x) == 1 && nzchar(x)
      if (!is_b64) {
        if (!is.null(error_msg) && nzchar(error_msg)) {
          return(tags$pre(
            style="white-space:pre-wrap;color:#9a3f00;background:#fff6e6;border:1px solid #f3d5a8;padding:8px;border-radius:6px;",
            paste0("Plot not available:\n", error_msg)
          ))
        }
        return(tags$div(tags$em("Not available for this model.")))
      }
      tags$img(src=paste0("data:image/png;base64,", x), 
      style="max-width:100%;border:1px solid #eee;border-radius:6px;",
      alt = "SHAP summary")
    }

    # Déterminer si on veut VRAIMENT afficher un plot SHAP
    .shap_supported_model <- function(debug_list) {
      cls <- tryCatch(debug_list$feature_importance$base_model_cls, error = function(e) NULL)
      cls_low <- tolower(cls %||% "")

      if (!nzchar(cls_low)) return(TRUE)  # par défaut on tente

      # Modèles pour lesquels SHAP n'est pas pertinent / pose problème
      if (grepl("naive", cls_low)  ||
          grepl("bayes", cls_low)  ||
          grepl("discriminantanalysis", cls_low) ||
          grepl("\\blda\\b", cls_low) ||
          grepl("\\bqda\\b", cls_low) ||
          grepl("kneighbors", cls_low) ||
          grepl("neighbors", cls_low) ||
          grepl("\\bknn\\b", cls_low)) {
        return(FALSE)
      }
      TRUE
    }



    parse_records_df <- function(x){
      if (is.null(x)) return(NULL)
      o <- tryCatch(jsonlite::fromJSON(jsonlite::toJSON(x), simplifyDataFrame=TRUE), error=function(e) NULL)
      if (is.null(o)) o <- tryCatch(do.call(rbind, lapply(x, function(r) as.data.frame(r, stringsAsFactors = FALSE))), error=function(e) NULL)
      if (is.null(o)) o <- as.data.frame(x, stringsAsFactors = FALSE)
      o
    }
    .scalar_chr_or_null <- function(x) {
      if (is.null(x) || length(x) < 1) return(NULL)
      s <- as.character(x[1]); if (!nzchar(s) || is.na(s)) return(NULL); s
    }
    .rename_if <- function(df, old, new) { if (!is.null(df) && old %in% names(df)) names(df)[names(df)==old] <- new; df }
    # ---- Helpers prefetch ----
    .use_prefetch <- function(rv_ml_ai, code = NULL) {
      pf <- rv_ml_ai$prefetch
      if (is.null(pf)) return(NULL)
      if (!is.null(code) && nzchar(code)) {
        if (!identical(tolower(code), tolower(pf$best_id %||% ""))) return(NULL)
      }
      pf
    }
    .render_from_prefetch <- function(ns, rv_ml_ai, output, render_metrics_msg = TRUE) {
      pf <- rv_ml_ai$prefetch
      if (is.null(pf)) return(invisible(NULL))

      # 1) metrics table (test)
      mdf <- tryCatch({
        if (is.data.frame(pf$metrics)) pf$metrics else {
          o <- jsonlite::fromJSON(jsonlite::toJSON(pf$metrics), simplifyDataFrame = TRUE)
          as.data.frame(o, stringsAsFactors = FALSE)
        }
      }, error = function(e) NULL)

      if (!is.null(mdf)) {
        output$metrics_table <- renderUI({ DT::dataTableOutput(ns("metrics_dt")) })
        output$metrics_dt <- DT::renderDataTable({
          DT::datatable(mdf, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
        })
      } else if (isTRUE(render_metrics_msg)) {
        output$metrics_table <- renderUI(tags$em("No metrics available."))
      }
      # 2) plots (b64)
      p <- pf$plots %||% list()
      roc_b64  <- p$roc  %||% p$auc
      cm_b64   <- p$confusion %||% p$confusion_matrix
      fi_b64   <- p$importance %||% p$feature %||% p$feature_importance
      shap_b64 <- p$shap_summary %||% p$shap

      output$roc_ui  <- renderUI(render_b64_img(roc_b64))
      output$cm_ui   <- renderUI(render_b64_img(cm_b64))
      output$fi_ui   <- renderUI(render_b64_img(fi_b64))
      output$shap_ui <- renderUI(render_b64_img(shap_b64))

      if (has_b64(roc_b64)) {
        updateTabsetPanel(getDefaultReactiveDomain(), "plots_tabs", selected = "ROC Curve")
      } else if (has_b64(cm_b64)) {
        updateTabsetPanel(getDefaultReactiveDomain(), "plots_tabs", selected = "Confusion Matrix")
      } else if (has_b64(fi_b64)) {
        updateTabsetPanel(getDefaultReactiveDomain(), "plots_tabs", selected = "Feature Importance")
      } else {
        updateTabsetPanel(getDefaultReactiveDomain(), "plots_tabs", selected = "SHAP Values")
      }
    }
    # ---- READY if there is a leaderboard and the run is complete ----
    train_ready <- reactive({
      isTRUE(rv_ml_ai$status %in% c("Finished", "Finished_NoPlots")) &&
      !is.null(rv_ml_ai$leaderboard) &&
      NROW(rv_ml_ai$leaderboard) > 0
    })
    output$train_ready <- reactive(train_ready())
    outputOptions(output, "train_ready", suspendWhenHidden = FALSE)

    # ---------- State ----------
    output$job_status <- renderText({ if (is.null(rv_ml_ai$status)) "Idle" else rv_ml_ai$status })
    # When AutoML has finished and we have a prefetch, we display everything immediately.
    observeEvent(rv_ml_ai$status, ignoreInit = TRUE, {
      if (isTRUE(rv_ml_ai$status %in% c("Finished","Finished_NoPlots")) && !is.null(rv_ml_ai$prefetch)) {
        best <- rv_ml_ai$prefetch$best_id %||% NULL
        if (!is.null(best) && nzchar(best)) {
          updateSelectInput(session, "model_id", selected = best)
        }
        .render_from_prefetch(ns, rv_ml_ai, output)
      }
    })
    # As soon as AutoML is complete and a prefetch exists, we fill it immediately.
    observeEvent(rv_ml_ai$status, ignoreInit = TRUE, {
      if (isTRUE(rv_ml_ai$status %in% c("Finished", "Finished_NoPlots")) && !is.null(rv_ml_ai$prefetch)) {
        # 1) Select the pre-calculated model by default
        best <- rv_ml_ai$prefetch$best_id %||% NULL
        if (!is.null(best) && nzchar(best)) {
          updateSelectInput(session, "model_id", selected = best)
        }
        #2) Paint the metrics + markers immediately.
        .render_from_prefetch(ns, rv_ml_ai, output)
      }
    })
    # ---------- TRAIN Leaderboard (Top-N view) ----------
    observeEvent(list(rv_ml_ai$leaderboard, rv_ml_ai$leaderboard_full, input$top_n, rv_ml_ai$status), ignoreInit = FALSE, {
      lb_full <- rv_ml_ai$leaderboard_full %||% rv_ml_ai$leaderboard
      if (is.null(lb_full) || !NROW(lb_full)) {
        output$leaderboard_train_table <- DT::renderDataTable(NULL)
        output$model_selector <- renderUI(NULL)
        return()
      }

      df_all <- as.data.frame(lb_full, stringsAsFactors = FALSE)
      # harmonized columns
      df_all <- .rename_if(df_all, "model_name", "Model")
      df_all <- .rename_if(df_all, "model_id",  "id")

      n_show <- input$top_n %||% nrow(df_all)
      n_show <- suppressWarnings(as.integer(n_show))
      if (is.na(n_show)) n_show <- nrow(df_all)
      n_show <- max(1, min(nrow(df_all), n_show))
      df_view <- utils::head(df_all, n_show)

      output$leaderboard_train_table <- DT::renderDataTable({
        req(train_ready())
        DT::datatable(
          df_view,
          options = list(pageLength = min(20, nrow(df_view)), scrollX = TRUE),
          rownames = FALSE
        )
      })
      output$prereq_status <- renderUI({
        if (identical(rv_ml_ai$status, "Running")) {
          tags$div(class="alert alert-info",
                   "AutoML in progress: leaderboard (Train & Test) + graphs (ROC, FI, SHAP), etc.")
        } else if (isTRUE(rv_ml_ai$status %in% c("Finished","Finished_NoPlots"))) {
          tags$div(class="alert alert-success", "Done")
        } else {
          tags$div(class="alert alert-secondary", "Ready to launch AutoML")
        }
      })
              # --- after the AutoML run / train ---
        if (!is.null(rv_ml_ai$leaderboard) && NROW(rv_ml_ai$leaderboard) > 0) {
          R.utils::mkdirs(file.path(getwd(), "logs", "models"))
          save_best_model_log(
            leaderboard = rv_ml_ai$leaderboard,       # or rv_ml_ai$leaderboard_full if available
            session_name = rv_ml_ai$session_name,
            session_id   = rv_ml_ai$session_id,
            dataset_id   = rv_ml_ai$dataset_id,
            outcome      = rv_ml_ai$target,
            framework    = "PyCaret"
          )
        }
      # Selector based on the Top-N view
      output$model_selector <- renderUI({
        # Take the visible labels
        labels <- if ("Model" %in% names(df_view)) df_view$Model else {
          if ("id" %in% names(df_view)) df_view$id else seq_len(nrow(df_view))
        }
        # Try to obtain the codes via rv_ml_ai$models (names=labels, values=codes)
        codes_from_map <- if (!is.null(rv_ml_ai$models)) {
          unname(rv_ml_ai$models[ match(labels, names(rv_ml_ai$models)) ])
        } else NULL
        # fallback: ‘id’ column if present
        if (is.null(codes_from_map) || all(is.na(codes_from_map))) {
          codes_from_map <- if ("id" %in% names(df_view)) df_view$id else labels
        }
        codes_from_map <- as.character(codes_from_map)
        choices_labels <- as.character(labels)
        default_sel <- if (length(codes_from_map) > 0) codes_from_map[1] else NULL
        selectInput(
          ns("model_id"),
          "Model:",
          choices = stats::setNames(codes_from_map, choices_labels),
          selected = default_sel
        )
      })
    })
    # ---------- TEST Leaderboard (Top-N view aligned) ----------
    # We do NOT call the API here: we display what is in memory (calculated on the controls side)
    observeEvent(list(rv_ml_ai$test_leaderboard_full, input$top_n, rv_ml_ai$status), ignoreInit = FALSE, {
      if (is.null(rv_ml_ai$test_leaderboard_full) || !NROW(rv_ml_ai$test_leaderboard_full)) {
        output$test_leaderboard_table <- DT::renderDataTable({
          DT::datatable(
            data.frame(info = "No Test leaderboard available yet."),
            options = list(dom = "t"), rownames = FALSE
          )
        })
        return()
      }
      df_all <- as.data.frame(rv_ml_ai$test_leaderboard_full, stringsAsFactors = FALSE)
      # Harmonize columns
      df_all <- .rename_if(df_all, "model_name", "Model")
      # Truncate to Top N (same as TRAIN)
      n <- input$top_n %||% nrow(df_all)
      n <- suppressWarnings(as.integer(n))
      if (is.na(n)) n <- nrow(df_all)
      n <- max(1, min(nrow(df_all), n))
      df_view <- utils::head(df_all, n)
      output$test_leaderboard_table <- DT::renderDataTable({
        DT::datatable(
          df_view,
          options = list(pageLength = min(10, n), scrollX = TRUE, dom = "tip"),
          rownames = FALSE
        ) |>
          DT::formatRound(
            intersect(c("Accuracy","AUC","Recall","Prec.","F1","Kappa","MCC","TT (Sec)"), names(df_view)),
            4
          )
      })

    })
    # ---------- Detailed evaluation (plots & metrics) ----------
    observeEvent(input$model_id, ignoreInit = TRUE, {
       # --- if the prefetch matches this pattern, DO NOT CALL the API ---
      model_code  <- input$model_id
      pf <- .use_prefetch(rv_ml_ai, code = model_code)
      if (!is.null(pf)) {
        .render_from_prefetch(ns, rv_ml_ai, output)
        return(invisible(NULL))
      }
      output$metrics_table <- renderUI({ tags$em("Evaluating model on test set...") })
      tmpfile <- tempfile(fileext = ".csv")
      utils::write.csv(rv_current$working_df, tmpfile, row.names = FALSE)
      model_code  <- input$model_id
      # Try to find a readable label for model_name
      model_label <- names(rv_ml_ai$models)[match(model_code, rv_ml_ai$models)]
      model_label <- .scalar_chr_or_null(model_label)
      # --- si le prefetch correspond, on n'appelle PAS l'API ---
      pf <- .use_prefetch(rv_ml_ai, code = model_code)
      if (!is.null(pf)) {
        .render_from_prefetch(ns, rv_ml_ai, output)
        return(invisible(NULL))
      }
      shiny::withProgress(message = "Evaluating…", value = 0.3, {
        # --- canonical dataset_id (no fallback) ---
        dataset_id <- rv_ml_ai$trained_dataset_id %||% rv_current$dataset_id
        req(is.character(dataset_id) && nzchar(dataset_id))

        body_eval <- list(
          file       = httr::upload_file(tmpfile),
          target     = rv_ml_ai$outcome,
          model_id   = model_code,
          session_id = if (!is.null(rv_ml_ai$seed_value)) rv_ml_ai$seed_value else rv_ml_ai$seed,
          session_name = rv_ml_ai$session_id,
          dataset_id = dataset_id
        )
        if (!is.null(model_label)) body_eval$model_name <- model_label

        res <- tryCatch(
          httr::POST(
            url   = paste0(api_base(), "/evaluate_model"),
            body  = body_eval,
            encode = "multipart",
            httr::timeout(600),
            verbose()
          ),
          error = function(e) e
        )
        if (!inherits(res, "response") || httr::http_error(res)) {
          txt <- if (inherits(res, "response")) httr::content(res, as = "text", encoding = "UTF-8") else conditionMessage(res)
          output$metrics_table <- renderUI(
            tags$pre(style="color:red;white-space:pre-wrap;",
                     paste("Evaluate error:", txt))
          )
          output$roc_ui  <- renderUI(tags$div(tags$em("No plot.")))
          output$cm_ui   <- renderUI(tags$div(tags$em("No plot.")))
          output$fi_ui   <- renderUI(tags$div(tags$em("No plot.")))
          output$shap_ui <- renderUI(render_b64_img(NULL, error_msg = "SHAP not available for this model or backend"))
          unlink(tmpfile)
          return(invisible(NULL))
        }
        out <- httr::content(res, as = "parsed", type = "application/json")
        # after: out <- httr::content(res, as = “parsed”, type = “application/json”)
        dbg <- tryCatch(out$extras$debug, error=function(e) NULL)
        if (!is.null(dbg)) {
          cat("[EVALUATE DEBUG] ", jsonlite::toJSON(dbg, auto_unbox = TRUE), "\n")
        }
        unlink(tmpfile)
        # ---- METRICS
        metrics_df <- parse_records_df(out$metrics)
        rv_ml_ai$eval_metrics <- metrics_df
        # ---- Feature importance data (for CSV download) ----
        fi_tbl <- tryCatch(
          parse_records_df(out$extras$feature_importance),
          error = function(e) NULL
          )
        rv_ml_ai$feature_importance <- fi_tbl

        output$metrics_table <- renderUI({ DT::dataTableOutput(ns("metrics_dt")) })
        output$metrics_dt <- DT::renderDataTable({
          DT::datatable(rv_ml_ai$eval_metrics, options = list(dom = "t", scrollX = TRUE), rownames = FALSE)
        })
        # ---- PLOTS (b64)
        p <- out$plots %||% out$plots_data
        roc_b64  <- if (!is.null(p)) (p$roc %||% p$auc) else NULL
        cm_b64   <- if (!is.null(p)) (p$confusion %||% p$confusion_matrix) else NULL
        fi_b64   <- if (!is.null(p)) (p$importance %||% p$feature_importance %||% p$feature_importance_plot) else NULL
        shap_b64 <- if (!is.null(p)) (p$shap_summary %||% p$shap %||% p$shap_values) else NULL
        output$roc_ui  <- renderUI(render_b64_img(roc_b64))
        output$cm_ui   <- renderUI(render_b64_img(cm_b64))
        output$fi_ui   <- renderUI(render_b64_img(fi_b64))
        output$shap_ui <- renderUI(render_b64_img(shap_b64))

        # Auto-select the first available tab
        if (has_b64(roc_b64)) {
          updateTabsetPanel(session, "plots_tabs", selected = "ROC Curve")
        } else if (has_b64(cm_b64)) {
          updateTabsetPanel(session, "plots_tabs", selected = "Confusion Matrix")
        } else if (has_b64(fi_b64)) {
          updateTabsetPanel(session, "plots_tabs", selected = "Feature Importance")
        } else {
          updateTabsetPanel(session, "plots_tabs", selected = "SHAP Values")
        }
      })
    })

    output$download_fi <- downloadHandler(
      filename = function() {
        paste0("feature_importance_", Sys.Date(), ".csv")
      },
      content = function(file) {

        # 1) Récupérer la table de feature importance
        fi <- rv_ml_ai$feature_importance
        if (is.null(fi)) {
          fi <- rv_current$train_model$feature_importance
        }

        # 2) Si c'est NULL → CSV vide "propre"
        if (is.null(fi)) {
          fi <- data.frame(
            feature    = character(),
            importance = numeric(),
            stringsAsFactors = FALSE
          )
        }

        # 3) Si c'est une liste de records, la transformer en data.frame
        if (is.list(fi) && !is.data.frame(fi)) {
          fi <- parse_records_df(fi)
        }

        fi <- as.data.frame(fi, stringsAsFactors = FALSE)

        # 4) Aplatir les colonnes de type list (source de l'erreur write.table)
        for (nm in names(fi)) {
          if (is.list(fi[[nm]])) {
            fi[[nm]] <- vapply(
              fi[[nm]],
              function(x) {
                if (length(x) == 1) {
                  # élément simple → on le convertit en chaîne
                  as.character(x[[1]])
                } else {
                  # élément complexe → on sérialise en JSON
                  jsonlite::toJSON(x, auto_unbox = TRUE)
                }
              },
              character(1)
            )
          }
        }

        # 5) Écriture du CSV
        utils::write.csv(fi, file, row.names = FALSE)
      }
    )




  })
}
