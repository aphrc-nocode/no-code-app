predict_pycaret_server <- function(id, api_base, rv_current, rv_ml_ai) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------- Helpers--------
    .idx_csv <- reactive({
      normalizePath(file.path("logs", "models", "index.csv"), mustWork = FALSE)
    })
    .safe_read_csv <- function(path) {
      tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    }
    `%||%` <- function(a,b) if (!is.null(a) && length(a)>0) a else b

    # -------- Detection of the “deployed” model
    # ----- Find the PyCaret model deployed for the selected dataset -----
    deployed_model_info <- reactive({
      # -- helpers
      `%||%` <- function(a,b) if (!is.null(a)) a else b
      slugify <- function(s) {
        s <- tolower(trimws(s %||% ""))
        s <- gsub("[^a-z0-9\\-]+", "_", s)
        s <- gsub("_+", "_", s)
        s <- gsub("^_|_$", "", s)
        s
      }

      # 0) current dataset
      ds <- rv_ml_ai$dataset_id %||% rv_current$dataset_id

      if (is.null(ds) || !nzchar(ds)) return(NULL)
      ds_slug <- slugify(ds)

      idx <- file.path("logs","models","index.csv")

      # ---------- A) Path ‘index.csv’ (priority if usable) ----------
      if (file.exists(idx)) {
        df <- tryCatch(read.csv(idx, stringsAsFactors = FALSE), error = function(e) NULL)

        if (!is.null(df) && nrow(df) > 0) {
          nm <- tolower(names(df))
          col_of <- function(x) { w <- which(nm == tolower(x)); if (length(w)) names(df)[w[1]] else NULL }
          getcol <- function(x) if (!is.null(col_of(x))) df[[ col_of(x) ]] else NULL

          framework   <- getcol("framework")
          dataset_id  <- getcol("dataset_id")
          model_id    <- getcol("model_id")          # ex: "ridge__diabetes"
          model_code  <- getcol("model_code")        # ex: "ridge"
          path_model  <- getcol("path_model")        # absolute/relative path to .pkl
          model       <- getcol("model")             # sometimes code in this column

          # 1) Filter dataset + PyCaret
          keep <- rep(TRUE, nrow(df))
          if (!is.null(framework))  keep <- keep & (tolower(framework) == "pycaret")
          if (!is.null(dataset_id)) keep <- keep & (as.character(dataset_id) == as.character(ds))
          if (!any(keep, na.rm = TRUE)) {
            # nothing via index → switch to fallback B)
          } else {
            d <- df[keep, , drop = FALSE]
            nm2 <- tolower(names(d))
            col_of2 <- function(x) { w <- which(nm2 == tolower(x)); if (length(w)) names(d)[w[1]] else NULL }

            # 2) .pkl candidates per line (multiple strategies)
            build_model_name <- function(i) {
              # a) an explicit .pkl path?
              pm <- if (!is.null(path_model)) path_model[i] else NA
              if (!is.na(pm) && nzchar(pm) && grepl("\\.pkl$", pm, ignore.case = TRUE))
                return(basename(pm))

              # b) a complete model_id/basename (e.g., ridge__diabetes)?
              mid <- if (!is.null(model_id)) model_id[i] else NA
              if (!is.na(mid) && nzchar(mid))
                return(if (grepl("\\.pkl$", mid, ignore.case = TRUE)) basename(mid) else paste0(mid, ".pkl"))

              # c) otherwise, rebuild from the code + dataset_slug
              mc <- if (!is.null(model_code)) model_code[i] else if (!is.null(model)) model[i] else NA
              if (!is.na(mc) && nzchar(mc))
                return(paste0(tolower(mc), "__", ds_slug, ".pkl"))

              NA_character_
            }

            model_names <- vapply(seq_len(nrow(d)), build_model_name, character(1))
            # remove NA and obvious duplicates
            model_names <- unique(model_names[is.finite(nchar(model_names)) & nzchar(model_names)])
            exists_vec  <- file.exists(file.path("models", model_names))

            if (any(exists_vec, na.rm = TRUE)) {
              # 3) We choose the “best” based on metrics, if available.
              d2 <- d[match(model_names[exists_vec], vapply(seq_len(nrow(d)), build_model_name, character(1))), , drop = FALSE]
              model_names2 <- model_names[exists_vec]

              to_num <- function(x) suppressWarnings(as.numeric(x))
              col_of_acc <- col_of2("Accuracy")
              col_of_auc <- col_of2("AUC")
              col_of_mv  <- col_of2("metric_value")

              acc <- if (!is.null(col_of_acc)) to_num(d2[[ col_of_acc ]]) else rep(NA_real_, nrow(d2))
              auc <- if (!is.null(col_of_auc)) to_num(d2[[ col_of_auc ]]) else rep(NA_real_, nrow(d2))
              mv  <- if (!is.null(col_of_mv))  to_num(d2[[ col_of_mv  ]]) else rep(NA_real_, nrow(d2))

              choose_idx <- function() {
                if (any(!is.na(acc))) return(which.max(replace(acc, is.na(acc), -Inf)))
                if (any(!is.na(auc))) return(which.max(replace(auc, is.na(auc), -Inf)))
                if (any(!is.na(mv)))  return(which.max(replace(mv,  is.na(mv),  -Inf)))
                # otherwise: most recent according to date if available, otherwise mtime of the .pkl file
                dt_col <- if (!is.null(col_of2("date_trained"))) d2[[ col_of2("date_trained") ]] else NULL
                parse_dt <- function(x) {
                  out <- suppressWarnings(as.POSIXct(x, tz="UTC"))
                  if (all(is.na(out))) out <- suppressWarnings(as.POSIXct(x, format="%d/%m/%Y %H:%M", tz="UTC"))
                  if (all(is.na(out))) out <- suppressWarnings(as.POSIXct(gsub("Z$","",x), format="%Y-%m-%dT%H:%M:%S", tz="UTC"))
                  out
                }
                if (!is.null(dt_col)) {
                  dt <- parse_dt(dt_col)
                  if (!all(is.na(dt))) return(order(dt, decreasing = TRUE, na.last = NA)[1])
                }
                mt <- file.info(file.path("models", model_names2))$mtime
                order(mt, decreasing = TRUE, na.last = NA)[1]
              }

              i <- choose_idx()
              return(list(
                model_name = model_names2[i],
                dataset_id = as.character(ds)
              ))
            }
            # otherwise: no .pkl exists for these lines -> we will move on to fallback B)
          }
        }
      }
      # ---------- B) Fallback without index: search directly in models/ ----------
      cand <- list.files("models", pattern = paste0("^.*__", ds_slug, "\\.pkl$"), full.names = TRUE)
      if (!length(cand)) return(NULL)

      mt <- file.info(cand)$mtime
      best <- cand[order(mt, decreasing = TRUE)][1]

      list(
        model_name = basename(best),
        dataset_id = as.character(ds)
      )
    })
    has_deployed_pycaret <- reactive({
      inf <- deployed_model_info()
      !is.null(inf) && nzchar(inf$model_name %||% "")
    })

    output$predict_pycaret_content_ui <- renderUI({
      inf <- deployed_model_info()
      if (is.null(inf)) return(NULL)   # <- empty tab if no template for this dataset

      tagList(
        div(class = "card p-3 shadow-sm",
          h3("Predict / Classify"),
          div(class = "mb-2 text-success",
              paste("Deployed for dataset:", inf$dataset_id)
          ),
          br(),
          div(class = "mb-2 text-success",
              paste("Model:", inf$model_name)
          ),
          br(),
          fileInput(ns("test_data_file"), "Upload Test Dataset (.csv)", accept = ".csv"),
          br(),
          actionButton(ns("run_prediction"), "Run Prediction", class = "btn btn-primary mt-2"),
          br(),
          uiOutput(ns("prediction_result_ui")),
          br()
        )
      )
    })
    # -------- Action: prediction (sends the correct model_name)
    observeEvent(input$run_prediction, {
      req(isTRUE(has_deployed_pycaret()))
      req(input$test_data_file)

      test_path  <- input$test_data_file$datapath
      model_name <- deployed_model_info()$model_name

      tryCatch({
        res <- httr::POST(
          url = paste0(api_base, "/predict_deployed_model"),
          body = list(
            model_name = model_name,
            file       = httr::upload_file(test_path)
          ),
          encode = "multipart"
        )
        if (res$status_code == 200) {
          tmp <- tempfile(fileext = ".csv")
          writeBin(httr::content(res, as = "raw"), tmp)
          preds_df <- tryCatch(read.csv(tmp, stringsAsFactors = FALSE), error = function(e) NULL)
          if (is.null(preds_df)) stop("Cannot read predictions CSV")
          preview_df <- utils::head(preds_df, 10)
          output$prediction_result_ui <- renderUI({
            tagList(
              div(class = "card p-3 mt-3 shadow-sm",
                h4("Preview of Predictions"),
                DT::dataTableOutput(ns("prediction_preview")),
                br(),
                downloadButton(ns("download_predictions"), "Download Predictions")
              )
            )
          })
          output$prediction_preview <- DT::renderDataTable({
            DT::datatable(preview_df, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
          })
          output$download_predictions <- downloadHandler(
            filename = function() paste0("predictions_", tools::file_path_sans_ext(model_name), ".csv"),
            content  = function(file) file.copy(tmp, file)
          )
          showNotification("Prediction completed successfully!", type = "message", duration = 3)
        } else {
          showNotification(paste("Prediction error:", res$status_code), type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })
}
