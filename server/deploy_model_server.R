deployment_server <- function(id, rv_ml_ai, rv_current, api_base) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # --- helpers for normalizing metric names ---
    `%||%` <- function(a, b) if (is.null(a)) b else a

    .slugify <- function(s) {
      s <- tolower(trimws(s %||% ""))
      s <- gsub("[^a-z0-9\\-]+", "_", s)
      s <- gsub("_+", "_", s)
      s <- gsub("^_|_$", "", s)
      s
    }

    .parse_time <- function(x) {
      out <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
      if (all(is.na(out))) out <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
      if (all(is.na(out))) out <- suppressWarnings(as.POSIXct(gsub("Z$","", x), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
      out
    }


    normalize_metric <- function(x) {
      x <- tolower(trimws(x %||% ""))
      aliases <- list(
        accuracy = c("accuracy","acc"),
        auc      = c("auc","roc_auc","roc auc"),
        f1       = c("f1","f1 score","f1_score"),
        recall   = c("recall","tpr","sensitivity"),
        precision= c("precision","ppv"),
        kappa    = c("kappa","cohen kappa","cohen_kappa"),
        mcc      = c("mcc","matthews"),
        rmse     = c("rmse"),
        r2       = c("r2","r^2")
      )
      for (nm in names(aliases)) if (x %in% aliases[[nm]]) return(nm)
      x
    }
    title_metric <- function(canon) {
      lut <- c(accuracy="Accuracy", auc="AUC", f1="F1", recall="Recall",
              precision="Precision", kappa="Kappa", mcc="MCC", rmse="RMSE", r2="R2")
      if (!is.na(lut[canon])) lut[canon] else tools::toTitleCase(canon)
    }

    refresh_key <- reactiveVal(0)

    # ------------------------------------------------------------
    # 0) DATASET CONTEXT (optional)
    # ------------------------------------------------------------
    current_dataset <- reactiveVal(NULL)
    observe({
      ds <- rv_ml_ai$dataset_id %||% rv_current$dataset_id
      if (!is.null(ds) && nzchar(ds)) current_dataset(ds)
    })
    output$dataset_info <- renderUI({
      ds <- current_dataset()
      if (is.null(ds) || !nzchar(ds)) {
        div(
          tags$strong("No dataset selected yet."),
          p("Pick a dataset to discover existing trained models and deploy one.")
        )
      } else {
        div(
          tags$strong("Selected dataset: "), code(ds), br(),
          tags$em("If trained PyCaret models exist for this dataset, they’ll appear below.")
        )
      }
    })

    output$dataset_selector <- renderUI({
      ds <- current_dataset()
      if (!is.null(ds) && nzchar(ds)) return(NULL)
      choices <- rv_current$known_datasets
      if (is.null(choices) || length(choices) == 0) return(div(tags$em("No datasets indexed yet.")))
      selectInput(ns("pick_dataset"), "Choose dataset", choices = choices, selected = NULL)
    })
    observeEvent(input$pick_dataset, {
      req(input$pick_dataset); current_dataset(input$pick_dataset)
    }, ignoreInit = TRUE)

    # ------------------------------------------------------------
    # 1) READY FLAG (for conditionalPanel in the UI)
    # ------------------------------------------------------------
    ready_flag <- reactive({
      # (A) a completed PyCaret run
      cond_train <- isTRUE(rv_ml_ai$status %in% c("Finished","Finished_NoPlots")) &&
                    !is.null(rv_ml_ai$leaderboard) && NROW(rv_ml_ai$leaderboard) > 0
      # (B) a selected dataset + available history
      ds   <- current_dataset()
      imap <- index_map()
      cond_hist <- !is.null(ds) && nzchar(ds) && !is.null(imap) && any(imap$dataset_id == ds & (imap$framework %in% c("PyCaret","pycaret","Pycaret")))
      isTRUE(cond_train || cond_hist)
    })
    output$ready_flag <- reactive({ ready_flag() })
    outputOptions(output, "ready_flag", suspendWhenHidden = FALSE)

    output$prereq_status <- renderUI({
      if (isTRUE(ready_flag())) return(HTML(""))
      HTML("<div class='alert alert-info' role='alert' style='margin-top:8px;'>
      Deployment unavailable: Run <b>PyCaret</b> (Launch AutoML → Train) 
      or choose a <b>dataset</b> that already has trained models. </div>")
    })


    # ------------------------------------------------------------
    # 2) DATA SOURCES (API)
    #    /saved_models: list of saved models (with meta)
    #    /deployments: list of models already deployed (status)
    # ------------------------------------------------------------
    .get_saved_models <- function() {
      url <- paste0(api_base(), "/saved_models")
      res <- try(httr::GET(url), silent = TRUE)
      if (inherits(res, "try-error") || httr::http_error(res)) return(NULL)
      txt <- httr::content(res, as = "text", encoding = "UTF-8")
      # IMPORTANT: no simplify to preserve list-columns
      j <- try(jsonlite::fromJSON(txt, simplifyVector = FALSE), silent = TRUE)
      if (inherits(j, "try-error") || is.null(j$items)) return(NULL)
      j$items  # list of items (each is a named list)
    }
    .get_deployments <- function() {
      url <- paste0(api_base(), "/deployments")
      res <- try(httr::GET(url), silent = TRUE)
      if (inherits(res, "try-error") || httr::http_error(res)) return(data.frame())
      txt <- httr::content(res, as = "text", encoding = "UTF-8")
      j <- try(jsonlite::fromJSON(txt), silent = TRUE)
      if (inherits(j, "try-error") || is.null(j$items)) return(data.frame())
      as.data.frame(j$items, stringsAsFactors = FALSE)
    }

    # Index of historical models: logs/models/index.csv
    .index_path <- function() file.path(getwd(), "logs", "models", "index.csv")

    index_map <- reactive({
      p <- .index_path()
      if (!file.exists(p)) return(NULL)
      df <- tryCatch(read.csv(p, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
      if (is.null(df) || !"dataset_id" %in% names(df) || !"model" %in% names(df)) return(NULL)

      # standard model key (always lowercase code, no spaces)
      df$model <- tolower(gsub("\\s+", "", trimws(df$model)))

      # prioritize lines with a “displayable session name”
      # (session_name not NA/not empty OR session_id not NA/not empty)
      has_name <- (!is.na(df$session_name) & nzchar(as.character(df$session_name))) |
                  (!is.na(df$session_id)   & nzchar(as.character(df$session_id)))

      # try to sort by date as well, if available
      suppressWarnings({
        dt <- try(as.POSIXct(df$date_trained), silent = TRUE)
        if (inherits(dt, "try-error")) dt <- rep(Sys.time(), nrow(df))
      })

      ord <- order(-as.integer(has_name), -as.numeric(dt))
      df <- df[ord, , drop = FALSE]

      # keep 1 line per model (the “best” thanks to the order)
      df <- df[!duplicated(df$model), , drop = FALSE]
      df
    })
    # Raw data (without filters)
    raw_models <- reactive({
      refresh_key()
      items <- .get_saved_models()
      if (is.null(items) || length(items) == 0) return(data.frame())

      # Build a data.frame + keep list-columns for metrics_map/available_metrics
      df <- data.frame(
        model_id   = vapply(items, function(x) x$model_id   %||% "", character(1)),
        model_name = vapply(items, function(x) x$model_name %||% "", character(1)),
        target     = vapply(items, function(x) x$target     %||% "", character(1)),
        session_id = vapply(items, function(x) x$session_id %||% "", character(1)),
        session_name = vapply(items, function(x) x$session_name %||% "", character(1)),
        metric_name  = vapply(items, function(x) x$metric_name  %||% "", character(1)),
        metric_value = vapply(items, function(x) x$metric_value %||% NA_real_, numeric(1)),
        created_at = vapply(items, function(x) x$created_at %||% "", character(1)),
        framework  = "PyCaret",
        stringsAsFactors = FALSE
      )

      # fields returned by /saved_models (if present)
      df$dataset_name <- vapply(items, function(x) x$dataset_name %||% "", character(1))
      df$dataset_slug <- vapply(items, function(x) x$dataset_slug %||% "", character(1))

      # ---- COALESCE FOR DISPLAY: display the session name if it exists ----
      df$session_id <- ifelse(nzchar(df$session_name), df$session_name, df$session_id)


      # extract the template code (prefix before __)
      df$model_code <- sub("^([a-z]+).*", "\\1", tolower(df$model_id %||% ""))

      # attach list-cols
      df$metrics_map <- lapply(items, function(x) x$metrics_map %||% list())
      df$available_metrics <- lapply(items, function(x) x$available_metrics %||% list())
      # === Filtering by dataset: priority index.csv, then filter by slug =====
      imap <- index_map()
      ds   <- current_dataset()
      want <- .slugify(ds)

      if (!is.null(ds) && nzchar(ds)) {
        if (!is.null(imap)) {
          # index.csv -> we compare the CODES (ridge, lr, rf, …)
          keep_codes <- unique(imap$model[imap$dataset_id == ds & (imap$framework %in% c("PyCaret","pycaret","Pycaret"))])
          keep_codes <- tolower(trimws(keep_codes))
          if (length(keep_codes)) {
            df <- df[df$model_code %in% keep_codes, , drop = FALSE]
          } else {
            df <- df[0, , drop = FALSE]
          }
        }

        # --- In ALL cases: second filter by dataset slug (avoids old files...) ---
        if (nrow(df)) {
          # normalize meta if absent
          if (!("dataset_slug" %in% names(df))) df$dataset_slug <- NA_character_
          df$dataset_slug <- tolower(df$dataset_slug)

          # suffixe <code>__<suffix>
          df$model_suffix <- tolower(sub("^[a-z]+__?", "", df$model_id))

          has_meta <- nzchar(df$dataset_slug)
          keep2 <- logical(nrow(df))
          # a) meta explicit
          keep2[has_meta] <- df$dataset_slug[has_meta] == want
          # b) fallback: suffix of the model_id
          keep2[!has_meta] <- df$model_suffix[!has_meta] == want

          df <- df[keep2, , drop = FALSE]
        }
      }

      # --- Attach a session label “display_session” from the index --------
      if (!is.null(imap) && !is.null(ds) && nzchar(ds) && nrow(df)) {
        sub <- imap[imap$dataset_id == ds & (imap$framework %in% c("PyCaret","pycaret","Pycaret")),
                    c("model","session_name","session_id"), drop = FALSE]

        if (nrow(sub)) {
          # normalize the model key (code)
          sub$model <- tolower(gsub("\\s+", "", trimws(sub$model)))

          # function that selects the best display value for 1 model
          choose_display <- function(sn, sid) {
            sn  <- as.character(sn);  sn[!nzchar(sn)]  <- NA_character_
            sid <- as.character(sid); sid[!nzchar(sid)] <- NA_character_

            # 1) session_name “human” (different from “session_###”)
            cand1 <- sn[!is.na(sn) & !grepl("^session_\\d+$", sn)]
            if (length(cand1) && any(nzchar(cand1))) return(cand1[1])

            # 2) Non-numeric session_id (e.g., “test”)
            cand2 <- sid[!is.na(sid) & !grepl("^\\d+$", sid)]
            if (length(cand2) && any(nzchar(cand2))) return(cand2[1])

            # 3) otherwise any session_name
            cand3 <- sn[!is.na(sn)]
            if (length(cand3) && any(nzchar(cand3))) return(cand3[1])

            # 4) otherwise session_id (same numeric value)
            cand4 <- sid[!is.na(sid)]
            if (length(cand4) && any(nzchar(cand4))) return(cand4[1])

            NA_character_
          }

          # aggregation by model -> a display_session value
          agg <- by(sub, sub$model, function(g) {
            data.frame(
              model = unique(as.character(g$model))[1],
              display_session = choose_display(g$session_name, g$session_id),
              stringsAsFactors = FALSE
            )
          })
          map <- do.call(rbind, as.list(agg))
          if (!is.null(map) && nrow(map)) {
            df$model_id <- tolower(gsub("\\s+", "", trimws(df$model_id)))
            df$display_session <- map$display_session[ match(df$model_id, map$model) ]
          } else {
            df$display_session <- NA_character_
          }
        } else {
          df$display_session <- NA_character_
        }
      } else if (nrow(df)) {
        df$display_session <- NA_character_
      }
      
      df
    })

    # ------------------------------------------------------------
    # #3) FILTERS (UI): sessions & metrics
    # ------------------------------------------------------------
    output$session_filter <- renderUI({
    df <- filtered_models()
    ch <- unique(na.omit(as.character(df$session_id)))
    ch <- ch[ch != ""]
    selectInput(ns("f_session"), "Session", choices = c("All", ch), selected = "All")
      })
    output$metric_picker <- renderUI({
      df <- raw_models()
      allm <- unique(unlist(df$available_metrics, use.names = FALSE))
      allm <- allm[!is.na(allm) & nzchar(allm)]
      if (length(allm) == 0) {
        allm <- c("Accuracy","AUC","F1","Recall","Precision","Kappa","MCC","RMSE","R2")
      }
      default_metric <- if ("Accuracy" %in% allm) "Accuracy" else allm[1]
      selectInput(ns("f_metric"), "Metric", choices = allm, selected = default_metric)
    })

    # Applying filters + status enrichment
    filtered_models <- reactive({
      refresh_key()
      df <- raw_models()
      if (!nrow(df)) return(df)

      # --- Session filter
      if (!is.null(input$f_session) && input$f_session != "All") {
        df <- subset(df, session_id == input$f_session)
      }

      # --- selected metric -> keep NUMERIC here
      chosen <- input$f_metric
      if (!is.null(chosen) && nzchar(chosen)) {
        canon <- normalize_metric(chosen)        # Canonization function
        df$metric_name <- title_metric(canon)    # Wording for display

        df$metric_value <- vapply(seq_len(nrow(df)), function(i) {
          mm <- df$metrics_map[[i]]
          if (is.null(mm) || length(mm) == 0) return(NA_real_)
          # search for the canonical key first, then for variants
          val <- mm[[canon]]
          if (is.null(val)) {
            val <- mm[[chosen]] %||% mm[[tolower(chosen)]] %||% mm[[toupper(chosen)]]
          }
          suppressWarnings(as.numeric(val))
        }, numeric(1))
      } else {
        # if no metric is selected, ensure a numeric value (avoid list/character)
        df$metric_value <- rep(NA_real_, nrow(df))
      }

      # --- deployment status (merge correct, not all_x)
      depl <- .get_deployments()
      if (nrow(depl) > 0 && "model_id" %in% names(depl)) {
        keep <- intersect(c("model_id","status","api","url"), names(depl))
        depl2 <- unique(depl[, keep, drop = FALSE])
        df <- merge(df, depl2, by = "model_id", all.x = TRUE, sort = FALSE)
        if (!"status" %in% names(df)) df$status <- NA_character_
        df$status[is.na(df$status)] <- "Not deployed"
      } else {
        df$status <- "Not deployed"
      }

      # --- DEDUP: only one line per model_code (best metric_value, otherwise most recent) ---
      if (nrow(df)) {
        # restore raw digital data for comparison
        score_num <- suppressWarnings(as.numeric(df$metric_value))
        dt_parsed <- .parse_time(df$created_at)

        df$model_code <- sub("^([a-z]+).*", "\\1", tolower(df$model_id))

        pick_best <- function(d) {
          sc <- suppressWarnings(as.numeric(d$metric_value))
          if (any(is.finite(sc))) {
            return(d[which.max(replace(sc, !is.finite(sc), -Inf))[1], , drop = FALSE])
          }
          dt <- .parse_time(d$created_at)
          if (any(!is.na(dt))) {
            return(d[order(dt, decreasing = TRUE, na.last = NA)[1], , drop = FALSE])
          }
          d[1, , drop = FALSE]
        }
        df <- do.call(rbind, lapply(split(df, df$model_code), pick_best))
        rownames(df) <- NULL
      }

      # --- display formatting (DO ONCE ONLY)
      # 3 decimal places -> new text column (avoids breaking the number)
      df$metric_value <- ifelse(
        is.na(df$metric_value),
        NA_character_,
        sprintf("%.3f", as.numeric(df$metric_value))
      )

      # legible date
      suppressWarnings({
        dt <- try(as.POSIXct(df$created_at, tz = "UTC"), silent = TRUE)
        if (!inherits(dt, "try-error")) {
          df$created_at <- format(dt, "%Y-%m-%d %H:%M")
        }
      })

      df
    })

    # ------------------------------------------------------------
    # 4) MAIN TABLE + Deploy Button
    # ---------------- --------------------------------------------
    # --- Session Display: prefer session_name (text) to session_id (integer)

    output$validate_table <- DT::renderDataTable({
      df <- filtered_models()
      # ---- Force the display of “session_id” from the index (current dataset) ----
      imap <- index_map()
      ds   <- current_dataset()

      if (!is.null(imap) && !is.null(ds) && nzchar(ds) && nrow(df)) {
        sub <- imap[imap$dataset_id == ds & (imap$framework %in% c("PyCaret","pycaret","Pycaret")),
                    c("model","session_name","session_id"), drop = FALSE]

        if (nrow(sub)) {
          # key = model code (lowercase, no spaces)
          norm_code <- function(x) tolower(gsub("\\s+", "", trimws(x)))
          sub$model <- norm_code(sub$model)

          choose_display <- function(sn, sid) {
            sn  <- as.character(sn);  sn[!nzchar(sn)]  <- NA_character_
            sid <- as.character(sid); sid[!nzchar(sid)] <- NA_character_
            cand <- c(
              sn[!is.na(sn)  & !grepl("^session_\\d+$", sn)],
              sid[!is.na(sid) & !grepl("^\\d+$", sid)],
              sn[!is.na(sn)],
              sid[!is.na(sid)]
            )
            if (length(cand)) cand[1] else NA_character_
          }

          # aggregation model -> display_session
          map <- tapply(
            X    = seq_len(nrow(sub)),
            INDEX= sub$model,
            FUN  = function(ix) choose_display(sub$session_name[ix], sub$session_id[ix])
          )
          map  <- as.character(map)
          keys <- names(map)

          # Ici on calcule des CODES pour la jointure,
          # mais on ne modifie PAS df$model_id
          tmp_codes <- norm_code(df$model_id)
          disp <- map[ match(tmp_codes, keys) ]

          # fallback: garder l'ancienne colonne session_id si pas de match
          orig <- as.character(df$session_id)
          disp[is.na(disp) | !nzchar(disp)] <- orig[is.na(disp) | !nzchar(disp)]

          df$session_id <- disp
        }
      }


      df$swagger <- vapply(seq_len(nrow(df)), function(i) {
        #swagger_url <- paste0(api_base, "/docs")
        swagger_url <- paste0(
            api_base(),
            "/docs#/default/predict_deployed_model_predict_deployed_model_post"
          )

        sprintf("<a href='%s' target='_blank'>Swagger</a>",
                htmltools::htmlEscape(swagger_url))
      }, character(1))


      show_cols <- c("model_id","model_name","target","session_id","metric_name",
                    "metric_value","framework","created_at","swagger","status")
      show_cols <- show_cols[show_cols %in% names(df)]
      df <- df[, show_cols, drop = FALSE]

      df$action <- vapply(seq_len(nrow(df)), function(i) {
        mid <- df$model_id[i]
        st  <- tolower(df$status[i] %||% "")
        if (st == "deployed") {
          sprintf("<button class='btn btn-sm btn-danger action-stop' data-model='%s'>Stop</button>", mid)
        } else {
          sprintf("<button class='btn btn-sm btn-primary action-deploy' data-model='%s'>Deploy</button>", mid)
        }
      }, character(1))

      DT::datatable(df, rownames = FALSE, escape = FALSE, options = list(pageLength = 10, dom = "tip"))
    })



  # ---- Binder JS : un seul observe suffit ----
  observe({
    session$sendCustomMessage("bindDeployBtn", list(ns = ns("")))
  })

  # ---- Deploy ----
  observeEvent(input$deploy_model_id, {
    req(input$deploy_model_id)

    res <- try(
      httr::POST(
        paste0(api_base(), "/deploy_model"),
        body   = list(model_id = input$deploy_model_id),
        encode = "multipart"
      ),
      silent = TRUE
    )

    if (inherits(res, "try-error") || httr::http_error(res)) {
      showNotification("Deployment failed.", type = "error")
      # we still force a redraw to reactivate the button on the JS side
      refresh_key(isolate(refresh_key()) + 1)
      return()
    }

    showNotification("Model deployed successfully.", type = "message")

    # Open Swagger if the API is returned by FastAPI
    # Open Swagger (adapté au mode Docker)
    parsed <- tryCatch(httr::content(res, as = "parsed"), error = function(e) NULL)
    api_url <- NULL
    swagger_url <- paste0(
      api_base(),
      "/docs#/default/predict_deployed_model_predict_deployed_model_post"
    )
    session$sendCustomMessage("openSwagger", list(url = api_url))

  })

  # ---- Stop (undeploy) ----
  observeEvent(input$stop_model_id, {
    req(input$stop_model_id)

    res <- try(
      httr::POST(
        paste0(api_base(), "/undeploy_model"),
        body = list(model_id = input$stop_model_id),
        encode = "multipart"
      ),
      silent = TRUE
    )

    if (inherits(res, "try-error") || httr::http_error(res)) {
      showNotification("Stop failed.", type = "error")
      refresh_key(isolate(refresh_key()) + 1)
      return()
    }

    showNotification("Model stopped.", type = "message")

    # short delay so that /deployments no longer lists the template
    later::later(function() {
      refresh_key(isolate(refresh_key()) + 1)
    }, delay = 0.6)
  })


    # ------------------------------------------------------------
    # 5) LOGS / HISTO
    # ------------------------------------------------------------
    output$logs_filters <- renderUI({ tagList() })
    output$logs_table   <- DT::renderDataTable({ DT::datatable(data.frame(), rownames = FALSE) })
  })
}

