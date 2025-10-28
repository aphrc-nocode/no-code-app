deployment_server <- function(id, rv_ml_ai, rv_current, api_base="http://127.0.0.1:8000") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (is.null(a)) b else a

    safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)
    make_key <- function(code, id, sess) {
      a <- as.character(code %||% id %||% "")
      b <- as.character(sess %||% "")
      paste0(a, "__", b)  # ex: "lr__test"
    }


    rv_deploy <- reactiveValues(
      deployed = data.frame(
        model_id = character(0),
        model    = character(0),
        session_id = character(0),
        url     = character(0),
        api     = character(0),
        status  = character(0),
        stringsAsFactors = FALSE
      )
    )

    # --- Periodic retrieval of the backend status (/deployments) ---
    fetch_deployments <- function() {
      res <- tryCatch(httr::GET(paste0(api_base, "/deployments"), httr::timeout(10)), error = function(e) NULL)
      if (is.null(res) || httr::http_error(res)) return(data.frame())
      out <- tryCatch(httr::content(res, as = "parsed", type = "application/json"), error = function(e) list(items = list()))
      items <- out$items %||% list()
      if (!length(items)) return(data.frame())
      as.data.frame(do.call(rbind, lapply(items, as.data.frame)), stringsAsFactors = FALSE)
    }

    # refreshes every 2 seconds
    deployed_df <- reactivePoll(2000, session,
      checkFunc = function() { Sys.time() },
      valueFunc = function() {
        df <- fetch_deployments()
        need <- c("model_id","model","url","api","status")

        # Add the missing columns while respecting the number of rows (0 or >0)
        for (nm in need) {
          if (!nm %in% names(df)) {
            df[[nm]] <- if (NROW(df)) NA_character_ else character(0)
          }
        }

        # Ensure order + character types
        df <- df[, need, drop = FALSE]
        for (nm in need) df[[nm]] <- as.character(df[[nm]])
        df
      }
    )



        # Final URLs (“single active template” version)
    predict_url <- function() paste0(api_base, "/predict_deployed_model")
    docs_url    <- function() paste0(api_base, "/docs")   # or “/__docs__” depending on your configuration

    # Retrieves a “selection” view from the filtered table at the top
    selected_models_view <- reactive({
      # we use the table that has already been built (deploy_table())
      df <- deploy_table()
      if (is.null(df) || !NROW(df)) return(df)

      # if the user has performed a search/filter, we take everything that remains visible
      df
    })

    # --- Combined view: visible candidates + backend status + Start/Stop button ---
# --- Combined view: visible candidates + backend status + Start/Stop button ---
    deploy_view <- reactive({
      cand <- deploy_table()
      remote <- deployed_df()
      dep <- if (NROW(remote)) remote else (rv_deploy$deployed %||% data.frame())

      if (is.null(cand) || !NROW(cand)) {
        if (!NROW(dep)) return(data.frame())
        out <- dep
        if (!"session_id" %in% names(out)) out$session_id <- NA_character_
      } else {
        # BEFORE the merge, keep the model_id from deploy_table() (already = model_key)
        key_cand <- cand$model_id %||% cand$model_code %||% cand$model
        cand$model_id <- as.character(key_cand)


        need_dep <- c("model_id","url","api","status")
        for (nm in need_dep) if (!nm %in% names(dep)) dep[[nm]] <- NA_character_
        # ensure that the model_id column is of type character
        cand$model_id <- as.character(cand$model_id)

        out <- merge(
          cand[, c("model_id","model","session_id"), drop = FALSE],
          dep[,  need_dep, drop = FALSE],
          by = "model_id", all.x = TRUE, sort = FALSE
        )
      }

      # Default status
      out$status[is.na(out$status) | out$status == ""] <- "Not deployed"
      # Valid IDs for inputId (no spaces/accents)
      safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

      # Start/Stop buttons per line (no vectorized ifelse)
      out$action <- vapply(seq_len(NROW(out)), function(i) {
        mid_safe <- safe_id(out$model_id[i])
        if (identical(out$status[i], "Deployed")) {
          as.character(actionButton(ns(paste0("stop_", mid_safe)), "Stop", class = "btn btn-warning btn-sm"))
        } else {
          as.character(actionButton(ns(paste0("start_", mid_safe)), "Start", class = "btn btn-success btn-sm"))
        }
      }, character(1))


      out
    })



    # small factory with a ‘deployed’ line
    make_deployed_row <- function(model_id, model, session_id, status, url = NA_character_, api = NA_character_) {
      data.frame(
        model_id   = as.character(model_id),
        model      = as.character(model),
        session_id = as.character(session_id),
        url        = as.character(url),
        api        = as.character(api),
        status     = as.character(status),
        stringsAsFactors = FALSE
      )
    }



      observeEvent(input$deploy_selected, {
        df <- selected_models_view()
        if (is.null(df) || !NROW(df)) {
          showNotification("No visible templates to deploy (empty filter?)", type = "warning")
          return()
        }
        idx <- input$logs_table_rows_selected
        if (is.null(idx) || !length(idx)) idx <- 1L
        row <- df[idx[1], , drop = FALSE]

        model_code <- row$model_code %||% row$model_id %||% row$model
        model_name <- row$model       %||% row$model_code
        session_id <- row$session_id  %||% ""
        model_key  <- make_key(model_code, row$model_id, session_id)  # <-- uniq key

        if (is.null(model_code) || !nzchar(as.character(model_code))) {
          showNotification("Unable to determine the model ID.", type="error")
          return()
        }

        body <- list(
          model_id   = as.character(model_key),  # <-- send the unique key to the backend
          model_name = as.character(model_name),
          session_id = as.character(session_id)
        )

        res <- tryCatch(
          httr::POST(paste0(api_base, "/deploy_model"), body = body, encode = "form", httr::timeout(120)),
          error = function(e) e
        )
        if (inherits(res, "response") && httr::status_code(res) == 422) {
          res <- tryCatch(
            httr::POST(paste0(api_base, "/deploy_model"), body = body, encode = "multipart", httr::timeout(120)),
            error = function(e) e
          )
        }
        if (!inherits(res, "response") || httr::http_error(res)) {
          msg <- if (inherits(res, "response")) paste(httr::status_code(res), httr::content(res, as="text", encoding="UTF-8")) else conditionMessage(res)
          showNotification(paste("Deploy error:", msg), type = "error", duration = 10)
          return()
        }

        parsed <- tryCatch(httr::content(res, as = "parsed", type = "application/json"), error = function(e) list())
        new_row <- make_deployed_row(
          model_id   = model_key,
          model      = model_name,
          session_id = session_id,
          status     = parsed$status %||% "Deployed",
          url        = parsed$url    %||% NA_character_,
          api        = parsed$api    %||% NA_character_
        )

        if (!NROW(rv_deploy$deployed)) {
          rv_deploy$deployed <- new_row
        } else {
          ix <- match(model_key, rv_deploy$deployed$model_id)
          if (is.na(ix)) rv_deploy$deployed <- rbind(rv_deploy$deployed, new_row)
          else rv_deploy$deployed[ix, names(new_row)] <- new_row[1, names(new_row)]
        }
        showNotification(paste("Model", model_key, "deployed."), type = "message")
      })





    observeEvent(input$stop_all, {
      # Attempt to deploy the model; if absent, fallback to no-op but clear the local state
      res <- tryCatch(
        httr::POST(paste0(api_base, "/undeploy_model"), encode="json", httr::timeout(60)),
        error = function(e) e
      )

      # ignore errors if the endpoint does not exist
      rv_deploy$deployed <- rv_deploy$deployed[0, ]
      showNotification("Deployment stopped.", type = "warning")
    })


    # --- Top table: Deployed models (auto-populated + buttons) ---
    output$deployed_table <- DT::renderDataTable({
      df <- deploy_view()
      if (!NROW(df)) {
        return(DT::datatable(
          data.frame(info = "No data available in table"),
          options = list(pageLength = 10, scrollX = TRUE, dom = "tip", stateSave = TRUE),
          rownames = FALSE
        ))
      }

      df$url <- ifelse(!is.na(df$url) & df$url!="", sprintf('<a href="%s" target="_blank">predict</a>', df$url), "")
      df$api <- ifelse(!is.na(df$api) & df$api!="", sprintf('<a href="%s" target="_blank">docs</a>', df$api), "")

      DT::datatable(
        df[, c("model_id","model","session_id","url","api","status","action")],
        rownames = FALSE, escape = FALSE,
        options = list(
          pageLength = 10, scrollX = TRUE, dom = "tip",
            # *** key for actionButtons to work ***
          preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
          drawCallback    = DT::JS('function() { Shiny.bindAll(this.api().table().node()); }')
        )
      )
    })



    # --- Dynamic Start/Stop actions per line ---
# --- Dynamic Start/Stop actions per line (with real actionButtons) ---
# --- Dynamic Start/Stop actions per line (with real actionButtons) ---
# --- Dynamic Start/Stop actions per line (bind-once) ---
observe({
  df <- deploy_view()
  if (!NROW(df)) return()
  
  # remembers which IDs already have handlers
  if (is.null(session$userData$bound_ids)) session$userData$bound_ids <- character(0)
  bound <- session$userData$bound_ids
  
  safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)
  ids <- unique(as.character(df$model_id))
  to_bind <- setdiff(ids, bound)
  if (!length(to_bind)) return()

  for (mid in to_bind) local({
    mid_local <- mid
    mid_safe  <- safe_id(mid_local)
    row_fn <- function() df[df$model_id == mid_local, , drop = FALSE][1, ]

    # STOP (bind once only)
    observeEvent(input[[paste0("stop_", mid_safe)]], {
      row <- row_fn()
      res <- tryCatch(
        httr::POST(paste0(api_base, "/undeploy_model"),
                   body = list(model_id = mid_local), encode = "form", httr::timeout(60)),
        error = function(e) e
      )
      if (!inherits(res, "response") || httr::http_error(res)) {
        showNotification("Stop error", type = "error"); return()
      }
      if (NROW(rv_deploy$deployed)) {
        keep <- rv_deploy$deployed$model_id != mid_local
        rv_deploy$deployed <- rv_deploy$deployed[keep, , drop = FALSE]
      }
      showNotification(paste("Model", mid_local, "stopped."), type = "warning", duration = 3, id = paste0("notif_", mid_safe))
    }, ignoreInit = TRUE, once = FALSE)

    # START (bind once only)
    observeEvent(input[[paste0("start_", mid_safe)]], {
      row <- row_fn()
      body <- list(
        model_id   = mid_local,
        model_name = row$model %||% mid_local,
        session_id = row$session_id %||% ""
      )
      res <- tryCatch(
        httr::POST(paste0(api_base, "/deploy_model"),
                   body = body, encode = "form", httr::timeout(120)),
        error = function(e) e
      )
      if (inherits(res, "response") && httr::status_code(res) == 422) {
        res <- tryCatch(
          httr::POST(paste0(api_base, "/deploy_model"),
                     body = body, encode = "multipart", httr::timeout(120)),
          error = function(e) e
        )
      }
      if (!inherits(res, "response") || httr::http_error(res)) {
        showNotification("Start error", type = "error"); return()
      }
      parsed <- tryCatch(httr::content(res, as = "parsed", type = "application/json"), error = function(e) list())
      url  <- parsed$url  %||% NA_character_
      api  <- parsed$api  %||% NA_character_
      stat <- parsed$status %||% "Deployed"

      new_row <- data.frame(
        model_id   = mid_local,
        model      = row$model %||% mid_local,
        session_id = row$session_id %||% "",
        url        = url,
        api        = api,
        status     = stat,
        stringsAsFactors = FALSE
      )
      if (!NROW(rv_deploy$deployed)) {
        rv_deploy$deployed <- new_row
      } else {
        ix <- match(mid_local, rv_deploy$deployed$model_id)
        if (is.na(ix)) rv_deploy$deployed <- rbind(rv_deploy$deployed, new_row)
        else rv_deploy$deployed[ix, names(new_row)] <- new_row[1, names(new_row)]
      }
      showNotification(paste("Model", mid_local, "deployed."), type = "message", duration = 3, id = paste0("notif_", mid_safe))
    }, ignoreInit = TRUE, once = FALSE)
  })

  # mark these IDs as already bound
  session$userData$bound_ids <- union(bound, to_bind)
})




    logs_path <- file.path(getwd(), "logs", "models", "index.csv")
    dir.create(dirname(logs_path), recursive = TRUE, showWarnings = FALSE)

    read_logs <- function(filePath, ...) {
      if (!file.exists(filePath)) return(data.frame())
      df <- tryCatch(utils::read.csv(filePath, check.names = FALSE, stringsAsFactors = FALSE),
                    error = function(e) data.frame())
      if (!NROW(df)) return(df)

      # --- column coalescing utility
# --- rowwise utility: fills ‘target’ row by row starting from the first non-empty candidate column
      rowwise_coalesce <- function(d, target, candidates) {
        if (!target %in% names(d)) d[[target]] <- NA_character_
        for (c in candidates) {
          if (c %in% names(d)) {
            empty <- is.na(d[[target]]) | trimws(d[[target]]) == "" | d[[target]] == "NA"
            src   <- d[[c]]
            take  <- empty & !is.na(src) & trimws(as.character(src)) != "" & src != "NA"
            if (any(take)) d[[target]][take] <- as.character(src[take])
          }
        }
        d
      }
      #--- reconcile outcome/session (line by line)
      df <- rowwise_coalesce(df, "outcome",    c("outcome","Outcome","target","Target"))
      df <- rowwise_coalesce(df, "session_id", c("session_id","session_name","session","seed"))

      # Force clean types + trim
      df$outcome    <- trimws(as.character(df$outcome));    df$outcome[is.na(df$outcome)] <- ""
      df$session_id <- trimws(as.character(df$session_id)); df$session_id[is.na(df$session_id)] <- ""

      # session_name is not retained
      if ("session_name" %in% names(df)) df$session_name <- NULL

      # --- date
      if ("date_trained" %in% names(df)) {
        dt <- suppressWarnings(as.POSIXct(df$date_trained, tz = "UTC"))
        if (all(is.na(dt))) dt <- suppressWarnings(as.POSIXct(df$date_trained, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
        df$date_trained <- dt
        df$date_trained_display <- if (inherits(dt, "POSIXct")) format(dt, "%Y-%m-%d %H:%M:%S") else df$date_trained
      }

      # --- reconcile outcome / session
      #df <- rowwise_coalesce(df, « outcome »,    c(« Outcome », “target”, « Target »))

      # Fill in session_id from alias then force to character
      df <- rowwise_coalesce(df, "session_id", c("session_id", "session_name", "session", "seed"))
      df$session_id <- as.character(df$session_id)
      df$session_id[is.na(df$session_id)] <- ""

      # session_name is not retained
      if ("session_name" %in% names(df)) df$session_name <- NULL

      if (!"framework" %in% names(df)) df$framework <- "PyCaret"

      # --- digitize known metrics
      to_num <- function(v){
        v <- as.character(v); v <- trimws(gsub("\u00A0"," ",v, useBytes=TRUE))
        v <- gsub(",", ".", v); v <- gsub("%","", v)
        suppressWarnings(as.numeric(v))
      }
      known <- c("Accuracy","AUC","ROC AUC","ROC_AUC","F1","F1 Score","Recall","Sensitivity",
                "Precision","Prec.","Specificity","Kappa","MCC","R2","RMSE","MAE","MAPE","RMSLE")
      for (nm in intersect(known, names(df))) {
        if (!is.numeric(df[[nm]])) {
          num <- to_num(df[[nm]])
          if (any(!is.na(num))) df[[nm]] <- num
        }
      }

      df[order(df$date_trained, decreasing = TRUE), , drop = FALSE]
    }


    logs <- reactiveFileReader(1500, session, logs_path, read_logs)

    # — candidate metrics: all numeric metrics except meta
    metric_candidates <- function(df) {
      if (!NROW(df)) return(character(0))
      drop <- c("date_trained","date_trained_display","dataset_id","outcome","session_name","session_id",
                "framework","model","model_id","model_code","model_readable","metric","estimate",
                "path_model","id","ID","Rank","Fold","TT","TT (Sec)","n_test","Time")
      num <- names(df)[vapply(df, is.numeric, logical(1))]
      num <- setdiff(num, drop)
      order_known <- c("Accuracy","AUC","ROC AUC","ROC_AUC","F1","F1 Score","Recall","Sensitivity",
                       "Precision","Prec.","Specificity","Kappa","MCC","R2","RMSE","MAE","MAPE","RMSLE")
      c(intersect(order_known, num), setdiff(num, order_known))
    }

    # — metric direction (TRUE = larger is better)
    metric_maximize <- function(m) {
      if (is.null(m)) return(TRUE)
      to_min <- c("RMSE","MAE","MAPE","RMSLE")
      !m %in% to_min
    }

    # ---- Filter UI ----
  output$logs_filters <- renderUI({
    df <- logs()
    # prefer session_id
    #ses_choices <- if (“session_id” %in% names(df)) df$session_id else df$session_name
    #sessions <- sort(unique(ses_choices))
    sessions <- sort(unique(df$session_id))

    # available digital metrics
    metric_candidates <- function(d) {
      drop <- c("date_trained","date_trained_display","dataset_id","outcome","session_name","session_id",
                "framework","model","model_id","model_code","metric","estimate","path_model","id","ID")
      num <- names(d)[vapply(d, is.numeric, logical(1))]
      setdiff(num, drop)
    }
    metrics <- metric_candidates(df)

    fluidRow(
      column(5,
        selectizeInput(ns("filter_sessions"), "Sessions", choices = sessions, multiple = TRUE,
                      options = list(placeholder = "Sélectionner une ou plusieurs sessions"))
      ),
      column(4,
        selectInput(ns("filter_metric"), "Métrique", choices = metrics, selected = metrics[1])
      ),
      column(3,
        uiOutput(ns("metric_value_ui"))
      )
    )
  })


    # dynamic threshold field (min or max)
    output$metric_value_ui <- renderUI({
      req(input$filter_metric)
      df <- logs()
      m  <- input$filter_metric
      vals <- df[[m]]
      rng <- range(vals, na.rm = TRUE)
      lab <- if (metric_maximize(m)) "Seuil minimum" else "Seuil maximum"
      numericInput(ns("metric_threshold"), lab, value = if (metric_maximize(m)) rng[1] else rng[2], step = 0.001)
    })

    # ---- filtered table ----
    deploy_table <- reactive({
      df <- logs()
      req(NROW(df) > 0)

      # filter sessions (we use session_id priority)
      if (!is.null(input$filter_sessions) && length(input$filter_sessions) > 0) {
        sel <- input$filter_sessions
        if ("session_id" %in% names(df)) {
          df <- df[df$session_id %in% sel, , drop = FALSE]
        } else {
          df <- df[df$session_name %in% sel, , drop = FALSE]
        }
      }

      # --- Fallback outcome if empty: use the current target of the session
      # (secure because we have just filtered by session)
      if (NROW(df) > 0) {
        empty_out <- is.na(df$outcome) | df$outcome == "" | df$outcome == "NA"
        fallback  <- rv_ml_ai$target %||% rv_current$target %||% rv_ml_ai$outcome
        if (!is.null(fallback) && nzchar(fallback) && any(empty_out)) {
          df$outcome[empty_out] <- as.character(fallback)
        }
      }


      # build the view with a single metric
      metric_selected <- input$filter_metric
      if (!is.null(metric_selected) && metric_selected %in% names(df)) {
        val <- df[[metric_selected]]
      } else {
        metric_selected <- NA_character_
        val <- NA_real_
      }

      # 1) Filter by sessions (session_id only)
      if (!is.null(input$filter_sessions) && length(input$filter_sessions) > 0) {
        sel <- input$filter_sessions
        df  <- df[df$session_id %in% sel, , drop = FALSE]
      }

      # 2) Remove obviously invalid lines
      #    - missing/empty model
      #    - missing/empty session_id
      bad_model <- is.null(df$model) | is.na(df$model) | df$model == "" | df$model == "NA"
      bad_sess  <- is.null(df$session_id) | is.na(df$session_id) | df$session_id == "" | df$session_id == "NA"
      df <- df[!(bad_model | bad_sess), , drop = FALSE]

      # 3) Calculation of the chosen metric (as you had it)
      metric_selected <- input$filter_metric
      if (!is.null(metric_selected) && metric_selected %in% names(df)) {
        val <- suppressWarnings(as.numeric(df[[metric_selected]]))
      } else {
        metric_selected <- NA_character_
        val <- rep(NA_real_, nrow(df))
      }

      # 4) Remove lines where the metric is missing (avoids ‘lr’ without value)
      df   <- df[!is.na(val), , drop = FALSE]
      val  <- val[!is.na(val)]

      # colonnes méta
      #keep_meta <- c(« date_trained_display »,« model_id »,« model »,« dataset_id »,« outcome »,“session_id”,« framework »)
      #keep_meta <- intersect(keep_meta, names(df))

      # meta columns (add model_code if present)
      keep_meta <- c("date_trained_display","model_id","model_code","model",
                    "dataset_id","outcome","session_id","framework")
      keep_meta <- intersect(keep_meta, names(df))


      # if nothing to display → return an empty data.frame with the correct columns
      if (!NROW(df) || !length(keep_meta)) {
        empty <- data.frame(
          date_trained = character(0),
          model        = character(0),
          dataset_id   = character(0),
          outcome      = character(0),
          session_id   = character(0),
          framework    = character(0),
          metric       = character(0),
          value        = numeric(0),
          stringsAsFactors = FALSE
        )
        return(empty)
      }

      out <- df[, keep_meta, drop = FALSE]

      # rename BEFORE adding metric/value
      if ("date_trained_display" %in% names(out)) {
        names(out)[names(out) == "date_trained_display"] <- "date_trained"
      }

      # add metric/value respecting nrow(out)
      out$metric <- rep_len(input$filter_metric %||% NA_character_, nrow(out))
      out$value  <- rep_len(suppressWarnings(as.numeric(val)), nrow(out))

      # light deduplication
      dedup_keys <- intersect(c("session_id","model_id","model","dataset_id","outcome","date_trained"), names(out))
      if (length(dedup_keys) > 0) {
        out <- out[!duplicated(out[, dedup_keys, drop = FALSE]), , drop = FALSE]
      }

      # sorting
      if (NROW(out) && !all(is.na(out$value))) {
        minimize <- (out$metric[1] %in% c("RMSE","MAE","MAPE","RMSLE"))
        ord <- order(out$value, decreasing = !minimize, na.last = TRUE)
        out <- out[ord, , drop = FALSE]
      }

      # unique key (model + session)
      out$model_key <- make_key(out$model_code, out$model_id, out$session_id)
      # and, for internal consistency, which will be used everywhere instead of model_id
      out$model_id <- out$model_key

      rownames(out) <- NULL
      out

    })


    output$logs_table <- DT::renderDataTable({
      df <- deploy_table()
      if (!NROW(df)) return(NULL)
      dt <- DT::datatable(df,
            rownames = FALSE,
            selection = "single",            # <-- add this
            options = list(pageLength = 10, scrollX = TRUE))
      num_cols <- which(vapply(df, is.numeric, logical(1)))
      if (length(num_cols)) for (j in num_cols) dt <- DT::formatRound(dt, j, 4)
      dt
    })


    # small flag to disable the suite if there are no logs (used later)
    output$ready_flag <- reactive(NROW(logs()) > 0)
    outputOptions(output, "ready_flag", suspendWhenHidden = FALSE)
  })
}


