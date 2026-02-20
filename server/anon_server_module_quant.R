anon_quant_server_logic <- function(input, output, session, rv_current = NULL) {
  
  # ======================================================================
  # Quant Anon — Module-safe server with coordinate-safe risk + QGIS export
  # FIXED: stable method codes + robust translated choices (no missing-choices error)
  # ======================================================================
  
  if (requireNamespace("rlang", quietly = TRUE)) {
    options(error = function() {
      message("\n--- ERROR TRACE (rlang::last_trace) ---\n")
      try(print(rlang::last_trace()), silent = TRUE)
      message("\n--- BASE TRACEBACK() ---\n")
      traceback(2)
    })
  } else {
    options(error = function() {
      message("\n--- BASE TRACEBACK() ---\n")
      traceback(2)
    })
  }
  
  ns <- session$ns
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # ---------- Platform label helpers -----------------------

  .lbl <- function(key) {
    val <- tryCatch(as.character(get_rv_labels(key)), error = function(e) "")
    if (nzchar(val)) val else key
  }
  
  .notify <- function(key, type = "message", duration = 5) {
    shiny::showNotification(.lbl(key), type = type, duration = duration)
  }
  .notify_msg  <- function(key) .notify(key, type = "message", duration = 5)
  .notify_warn <- function(key) .notify(key, type = "warning", duration = 6)
  .notify_err  <- function(key, duration = NULL) .notify(key, type = "error", duration = duration)
  
  # --- Safe language getter (string, not reactive) -------------------------
  .get_lang <- function(default = "English") {
    rv <- get0("rv_lang", inherits = TRUE, ifnotfound = NULL)
    if (!is.null(rv)) {
      for (nm in c("selected_language", "language", "lang")) {
        if (!is.null(rv[[nm]]) && nzchar(as.character(rv[[nm]]))) return(as.character(rv[[nm]]))
      }
    }
    for (nm in c("change_language", "language", "lang")) {
      if (!is.null(input[[nm]]) && nzchar(as.character(input[[nm]]))) return(as.character(input[[nm]]))
    }
    default
  }
  
  # --- Access platform "choices" df if present -----------------------------
  .get_choices_df <- function() {
    rv <- get0("rv_lang", inherits = TRUE, ifnotfound = NULL)
    if (!is.null(rv)) {
      df <- tryCatch(rv$labelling_file_df$choices, error = function(e) NULL)
      if (!is.null(df) && is.data.frame(df)) return(df)
    }
    f <- get0("get_choices_df", inherits = TRUE, ifnotfound = NULL)
    if (is.function(f)) {
      df <- tryCatch(f(), error = function(e) NULL)
      if (!is.null(df) && is.data.frame(df)) return(df)
    }
    NULL
  }
  
  # Build translated choice vectors from Excel "choices" sheet.
  # Returned: named vector where names=display labels, values=stable codes in `label`.
  # Build translated choice vectors from Excel "choices" sheet.
  # Returned: named vector where names=display labels, values=stable codes in `label`.
  # Build translated choice vectors from Excel "choices" sheet.
  # Returned: named vector where names=display labels, values=stable codes in `label`.
  .choice_vec <- function(variable, lang = NULL) {
    `%||%` <- function(a, b) if (is.null(a)) b else a
    lang <- lang %||% .get_lang("English")
    
    # Normalize language key to match lowercased column names
    lang_key <- trimws(tolower(as.character(lang)))
    
    # Allow common variants
lang_map = get_rv_labels("input_language")
    
    # Helper: coerce a "choices source" into names=labels, values=codes
    .as_choices <- function(x, known_codes = character()) {
      if (is.null(x)) return(NULL)
      
      # data.frame input
      if (is.data.frame(x)) {
        df <- x
        names(df) <- trimws(tolower(names(df)))
        if ("var"  %in% names(df) && !"variable" %in% names(df)) df$variable <- df$var
        if ("code" %in% names(df) && !"label"    %in% names(df)) df$label    <- df$code
        if (!all(c("variable", "label") %in% names(df))) return(NULL)
        return(df)
      }
      
      # named atomic vector input
      if (is.atomic(x) && !is.null(names(x)) && length(x) > 0) {
        n <- trimws(as.character(names(x)))     # could be codes or labels
        v <- trimws(as.character(unname(x)))    # could be labels or codes
        
        n_has_codes <- any(n %in% known_codes)
        v_has_codes <- any(v %in% known_codes)
        
        # Case A: names=codes, values=labels  -> flip to names=labels, values=codes
        if (n_has_codes && !v_has_codes) {
          return(stats::setNames(n, v))
        }
        
        # Case B: names=labels, values=codes  -> already correct
        if (v_has_codes && !n_has_codes) {
          return(stats::setNames(v, n))
        }
        
        # Ambiguous: assume already correct (names=labels, values=codes)
        return(stats::setNames(v, n))
      }
      
      NULL
    }
    
    # 1) Try platform helper first
    df <- NULL
    known_codes <- character()
    
    # If we can read known codes from the Excel choices df, do it early (helps auto-detect flips)
    df0 <- .get_choices_df()
    if (!is.null(df0) && is.data.frame(df0)) {
      tmp <- df0
      names(tmp) <- trimws(tolower(names(tmp)))
      if ("var"  %in% names(tmp) && !"variable" %in% names(tmp)) tmp$variable <- tmp$var
      if ("code" %in% names(tmp) && !"label"    %in% names(tmp)) tmp$label    <- tmp$code
      if (all(c("variable", "label") %in% names(tmp))) {
        tmp$variable <- trimws(as.character(tmp$variable))
        tmp$label    <- trimws(as.character(tmp$label))
        sub0 <- tmp[tmp$variable == trimws(variable), , drop = FALSE]
        if (nrow(sub0)) known_codes <- unique(sub0$label[nzchar(sub0$label)])
      }
    }
    
    f <- get0("get_named_choices", inherits = TRUE, ifnotfound = NULL)
    if (is.function(f)) {
      out <- tryCatch(
        f(input_choices_file, .get_lang("English"), variable),
        error = function(e) NULL
      )
      
      # If it's already a named vector, normalize shape robustly and return immediately
      if (is.atomic(out) && !is.null(names(out)) && length(out) > 0) {
        cv <- .as_choices(out, known_codes = known_codes)
        if (!is.null(cv) && length(cv) > 0) return(cv)
      }
      
      # If platform returns a df, keep it for the Excel-style path below
      if (is.data.frame(out) && nrow(out) > 0) df <- out
    }
    
    # 2) Otherwise fall back to platform-loaded choices df
    if (is.null(df)) df <- .get_choices_df()
    
    if (is.null(df) || !is.data.frame(df)) return(NULL)
    
    # Normalize column names
    names(df) <- trimws(tolower(names(df)))
    if ("var"  %in% names(df) && !"variable" %in% names(df)) df$variable <- df$var
    if ("code" %in% names(df) && !"label"    %in% names(df)) df$label    <- df$code
    if (!all(c("variable", "label") %in% names(df))) return(NULL)
    
    df$variable <- trimws(as.character(df$variable))
    df$label    <- trimws(as.character(df$label))
    
    sub <- df[df$variable == trimws(variable), , drop = FALSE]
    if (!nrow(sub)) return(NULL)
    
    # Pick language column
    lang_col <- NULL
    if (!is.null(lang_key) && lang_key %in% names(sub)) lang_col <- lang_key
    if (is.null(lang_col) && "english" %in% names(sub)) lang_col <- "english"
    if (is.null(lang_col)) {
      candidate_cols <- setdiff(names(sub), c("variable", "label"))
      if (length(candidate_cols)) lang_col <- candidate_cols[1]
    }
    if (is.null(lang_col)) return(NULL)
    
    labs <- as.character(sub[[lang_col]])
    
    # Fallback to English if blank
    if ("english" %in% names(sub)) {
      labs_en <- as.character(sub[["english"]])
      miss <- is.na(labs) | !nzchar(trimws(labs))
      labs[miss] <- labs_en[miss]
    }
    
    vals <- as.character(sub$label)
    
    ok <- !is.na(vals) & nzchar(trimws(vals)) & !is.na(labs) & nzchar(trimws(labs))
    vals <- trimws(vals[ok])
    labs <- trimws(labs[ok])
    if (!length(vals)) return(NULL)
    
    # Final shape: names = display labels, values = stable code
    stats::setNames(vals, labs)
  }
  
  # --- Normalize method input to stable internal codes --------------------
  .norm_method <- function(x) {
    if (is.null(x) || !nzchar(as.character(x))) return("masking")
    x0 <- trimws(as.character(x))
    
    known_codes <- c(
      "masking","suppression","bucketing","pseudonymization","tokenization",
      "kanonymity","generalization","anonymizecoordinates","ldiversity","tcloseness"
    )
    if (tolower(x0) %in% known_codes) return(tolower(x0))
    
    map = get_named_choices(input_choices_file, input$change_language, "quant_anon_method")
    
    x1 <- tolower(gsub("[^a-z]", "", x0))
    if (grepl("mask", x1)) return("masking")
    if (grepl("supp", x1)) return("suppression")
    if (grepl("bucket", x1)) return("bucketing")
    if (grepl("pseudo", x1)) return("pseudonymization")
    if (grepl("token", x1)) return("tokenization")
    if (grepl("kanon", x1)) return("kanonymity")
    if (grepl("general", x1)) return("generalization")
    if (grepl("coord", x1) || grepl("geo", x1)) return("anonymizecoordinates")
    if (grepl("ldiver", x1)) return("ldiversity")
    if (grepl("tclose", x1)) return("tcloseness")
    "masking"
  }
  
  # Reactive invalidation on language change
  .lang_tick <- shiny::reactive({
    rv <- get0("rv_lang", inherits = TRUE, ifnotfound = NULL)
    if (!is.null(rv)) {
      tryCatch({ rv$selected_language; rv$labelling_file_df }, error = function(e) NULL)
    } else {
      .get_lang("English")
    }
  })
  
  # ---------- Translatable UI outputs --------------------------------------
  output$quant_anon_tab_dashboard    <- shiny::renderUI(shiny::span(.lbl("quant_anon_tab_dashboard")))
  output$quant_anon_tab_codes        <- shiny::renderUI(shiny::span(.lbl("quant_anon_tab_codes")))
  output$quant_anon_tab_descriptions <- shiny::renderUI(shiny::span(.lbl("quant_anon_tab_descriptions")))
  
  output$quant_anon_step0_title_ui              <- shiny::renderUI(shiny::tags$h4(.lbl("quant_anon_step0_title")))
  output$quant_anon_step0_use_platform_title_ui <- shiny::renderUI(shiny::tags$strong(.lbl("quant_anon_step0_use_platform_title")))
  output$quant_anon_step1_title_ui              <- shiny::renderUI(shiny::tags$h4(.lbl("quant_anon_step1_title")))
  output$quant_anon_step2_title_ui              <- shiny::renderUI(shiny::tags$h4(.lbl("quant_anon_step2_title")))
  output$quant_anon_step3_title_ui              <- shiny::renderUI(shiny::tags$h4(.lbl("quant_anon_step3_title")))
  
  output$quant_anon_data_preview_title_ui    <- shiny::renderUI(shiny::tags$h3(.lbl("quant_anon_tab_data_preview")))
  output$quant_anon_risk_assessment_title_ui <- shiny::renderUI(shiny::tags$h3(.lbl("quant_anon_tab_risk")))
  output$quant_anon_risk_before_title_ui     <- shiny::renderUI(shiny::tags$h4(.lbl("quant_anon_risk_before")))
  output$quant_anon_risk_after_title_ui      <- shiny::renderUI(shiny::tags$h4(.lbl("quant_anon_risk_after")))
  
  # Provide translated download buttons as UI outputs (use in UI to avoid hardcoding)
  output$download_btn_csv_ui <- shiny::renderUI(
    shiny::downloadButton(ns("download"), .lbl("quant_anon_download_csv"), class = "btn-block mb-1")
  )
  output$download_btn_excel_ui <- shiny::renderUI(
    shiny::downloadButton(ns("download_excel"), .lbl("quant_anon_download_excel"), class = "btn-block mb-1")
  )
  output$download_btn_dta_ui <- shiny::renderUI(
    shiny::downloadButton(ns("download_dta"), .lbl("quant_anon_download_stata"), class = "btn-block mb-1")
  )
  output$download_btn_report_ui <- shiny::renderUI(
    shiny::downloadButton(ns("download_report"), .lbl("quant_anon_download_risk_pdf"), class = "btn-block mb-1")
  )
  
  # Optional: expose copy strings (if UI reads them)
  output$quant_anon_js_copy_txt   <- shiny::renderText(.lbl("quant_anon_copy"))
  output$quant_anon_js_copied_txt <- shiny::renderText(.lbl("quant_anon_copied"))
  
  # Update labels + choices on language change
  shiny::observeEvent(.lang_tick(), {
    
    shiny::updateActionButton(session, "use_platform_data", label = .lbl("quant_anon_step0_load_selected"))
    shiny::updateActionButton(session, "apply",            label = .lbl("quant_anon_apply"))
    shiny::updateActionButton(session, "undo",             label = .lbl("quant_anon_undo"))
    shiny::updateActionButton(session, "reset",            label = .lbl("quant_anon_reset"))
    shiny::updateActionButton(session, "advisor_run",      label = .lbl("quant_anon_show_suggestions"))
    shiny::updateActionButton(session, "view_report", label = .lbl("quant_anon_view_report"))
    
    
    
    # --- FIX: robust method choices (no error toast, safe fallback) -------
    ch_method <- .choice_vec("quant_anon_method", lang = .get_lang("English"))
   
    
    if (is.null(ch_method) || length(ch_method) == 0) {
      # fallback uses ui_labels keys (no English hardcoding)
      ch_method <- stats::setNames(
        c("masking","suppression","bucketing","pseudonymization","tokenization",
          "kanonymity","generalization","anonymizecoordinates"),
        c(.lbl("quant_anon_method_masking"),
          .lbl("quant_anon_method_suppression"),
          .lbl("quant_anon_method_bucketing"),
          .lbl("quant_anon_method_pseudonymization"),
          .lbl("quant_anon_method_tokenization"),
          .lbl("quant_anon_method_kanonymity"),
          .lbl("quant_anon_method_generalization"),
          .lbl("quant_anon_method_anonymizecoordinates"))
      )
      
      # warn only once, and do not append raw debug "(quant_anon_method)"
      if (!isTRUE(isolate(session$userData$warned_missing_quant_method))) {
        session$userData$warned_missing_quant_method <- TRUE
        shiny::showNotification(.lbl("quant_anon_notif_missing_choices_method"), type = "warning", duration = 6)
      }
    }
    
    sel <- .norm_method(isolate(input$method))
    if (is.null(sel) || !nzchar(sel) || !(sel %in% unname(ch_method))) {
      sel <- unname(ch_method)[1]
    }
    
    shiny::updateSelectInput(
      session, "method",
      label    = .lbl("quant_anon_method_label"),
      choices  = ch_method,
      selected = sel
    )
    
    shiny::updateSelectInput(session, "advisor_var", label = .lbl("quant_anon_choose_numeric_var"))
  }, ignoreInit = FALSE)
  
  # CRITICAL FIX: avoid htmlwidgets::validate masking shiny::validate
  vld  <- shiny::validate
  need <- shiny::need
  
  get_after_col <- function(q, df_after) {
    anon_col <- paste0(q, "_anon")
    if (anon_col %in% names(df_after)) anon_col else q
  }
  
  shinyjs::inlineCSS("
    .gauge-value { display: none !important; }
    #preview-table { padding-right: 0 !important; }
  ")
  
  # ---------- Helpers -----------------------------------------------------
  parse_ranges_text <- function(text) {
    entries <- unlist(strsplit(text, ";", fixed = TRUE))
    parsed <- lapply(entries, function(e) {
      e <- trimws(e)
      if (e == "") return(NULL)
      
      parts <- strsplit(e, ":", fixed = TRUE)[[1]]
      if (length(parts) != 2) stop(sprintf(.lbl("quant_anon_err_range_format"), e))
      
      range_part <- trimws(parts[1])
      label <- trimws(parts[2])
      if (label == "") stop(sprintf(.lbl("quant_anon_err_range_label_missing"), e))
      
      bounds <- strsplit(range_part, "-", fixed = TRUE)[[1]]
      if (length(bounds) == 1) {
        num <- as.numeric(bounds[1])
        if (is.na(num)) stop(sprintf(.lbl("quant_anon_err_range_non_numeric_bound"), range_part))
        lower <- num
        upper <- num
      } else if (length(bounds) == 2) {
        lower_raw <- trimws(bounds[1])
        upper_raw <- trimws(bounds[2])
        lower <- if (lower_raw == "" || lower_raw == "-Inf") -Inf else as.numeric(lower_raw)
        upper <- if (upper_raw == "" || upper_raw == "Inf")  Inf else as.numeric(upper_raw)
        if (is.na(lower) || is.na(upper)) stop(sprintf(.lbl("quant_anon_err_range_non_numeric_bound"), range_part))
      } else {
        stop(sprintf(.lbl("quant_anon_err_range_malformed"), range_part))
      }
      
      if (lower > upper) stop(sprintf(.lbl("quant_anon_err_range_lower_gt_upper"), label))
      list(lower = lower, upper = upper, label = label)
    })
    
    parsed <- Filter(Negate(is.null), parsed)
    if (length(parsed) == 0) stop(.lbl("quant_anon_err_range_none_valid"))
    
    df <- do.call(rbind, lapply(parsed, function(p) {
      data.frame(lower = p$lower, upper = p$upper, label = p$label, stringsAsFactors = FALSE)
    }))
    
    df <- df[order(df$lower, df$upper), ]
    if (nrow(df) > 1) {
      for (i in seq_len(nrow(df) - 1)) {
        if (df$upper[i] >= df$lower[i + 1]) {
          stop(sprintf(.lbl("quant_anon_err_range_overlap"), df$label[i], df$label[i + 1]))
        }
      }
    }
    df
  }
  
  is_finite_bbox <- function(bbox) {
    if (is.null(bbox)) return(FALSE)
    all(is.finite(unname(as.numeric(bbox))))
  }
  
  nz_pos <- function(x, default = 100) {
    x2 <- suppressWarnings(as.numeric(x))
    if (length(x2) == 0 || is.na(x2) || !is.finite(x2) || x2 <= 0) default else x2
  }
  
  is_geo_name <- function(nm) {
    tolower(nm) %in% c("lat", "latitude", "lon", "long", "longitude", "geom", "geometry",
                       "wkt", "cell_wkt", "polygon")
  }
  
  risk_qids <- function(qids) {
    qids[!sapply(qids, is_geo_name)]
  }
  
  drop_geo_cols <- function(dat) {
    drop <- intersect(names(dat), names(dat)[sapply(names(dat), is_geo_name)])
    drop <- setdiff(drop, c("cell_wkt", "wkt", "polygon"))
    dat[, setdiff(names(dat), drop), drop = FALSE]
  }
  
  # ----- CRS safety layer -------------------------------------------------
  is_valid_utm_epsg <- function(epsg) {
    if (is.na(epsg)) return(FALSE)
    epsg %in% c(3857, 4326) ||
      (epsg >= 32601 && epsg <= 32660) ||
      (epsg >= 32701 && epsg <= 32760)
  }
  
  guess_utm_epsg <- function(lon, lat) {
    lon <- lon[is.finite(lon)]
    lat <- lat[is.finite(lat)]
    if (!length(lon) || !length(lat)) return(3857)
    zone <- floor((mean(lon) + 180) / 6) + 1
    epsg <- if (mean(lat) >= 0)  32600 + zone else 32700 + zone
    if (!is_valid_utm_epsg(epsg)) 3857 else epsg
  }
  
  safe_transform <- function(obj, target_epsg) {
    if (inherits(obj, "sf") && is.na(sf::st_crs(obj))) {
      suppressWarnings(sf::st_crs(obj) <- 4326)
    }
    if (!is_valid_utm_epsg(target_epsg)) target_epsg <- 3857
    
    out <- try(suppressWarnings(sf::st_transform(obj, target_epsg)), silent = TRUE)
    if (inherits(out, "try-error")) {
      out <- try(suppressWarnings(sf::st_transform(obj, 4326)), silent = TRUE)
      if (inherits(out, "try-error")) {
        out <- try(suppressWarnings(sf::st_transform(obj, 3857)), silent = TRUE)
        if (inherits(out, "try-error")) return(obj)
      }
    }
    out
  }
  
  build_grid_agg <- function(pts_utm, cell_m) {
    if (!inherits(pts_utm, "sf")) stop(.lbl("quant_anon_err_pts_not_sf"))
    if (!inherits(sf::st_geometry(pts_utm), "sfc_POINT")) stop(.lbl("quant_anon_err_pts_not_points"))
    if (is.na(sf::st_crs(pts_utm))) sf::st_crs(pts_utm) <- 3857
    
    cell_m <- nz_pos(cell_m, default = 100)
    
    bbox <- suppressWarnings(sf::st_bbox(pts_utm))
    if (!is_finite_bbox(bbox)) {
      return(sf::st_sf(geometry = sf::st_sfc(), n = integer(0), crs = sf::st_crs(pts_utm)))
    }
    
    grd <- try(sf::st_make_grid(pts_utm, cellsize = c(cell_m, cell_m), square = TRUE), silent = TRUE)
    if (inherits(grd, "try-error") || length(grd) == 0) {
      return(sf::st_sf(geometry = sf::st_sfc(), n = integer(0), crs = sf::st_crs(pts_utm)))
    }
    
    grd_sf <- sf::st_sf(geometry = grd)
    sf::st_crs(grd_sf) <- sf::st_crs(pts_utm)
    
    ints <- try(sf::st_intersects(grd_sf, pts_utm), silent = TRUE)
    if (inherits(ints, "try-error")) {
      return(sf::st_sf(geometry = sf::st_sfc(), n = integer(0), crs = sf::st_crs(pts_utm)))
    }
    
    grd_sf$n <- lengths(ints)
    grd_sf <- grd_sf[grd_sf$n > 0, , drop = FALSE]
    grd_sf <- sf::st_make_valid(grd_sf)
    grd_sf <- sf::st_zm(grd_sf, drop = TRUE, what = "ZM")
    if (nrow(grd_sf)) grd_sf <- grd_sf[!sf::st_is_empty(grd_sf), , drop = FALSE]
    
    if (!nrow(grd_sf)) {
      return(sf::st_sf(geometry = sf::st_sfc(), n = integer(0), crs = sf::st_crs(pts_utm)))
    }
    grd_sf
  }
  
  if (requireNamespace("sf", quietly = TRUE)) {
    sf::sf_use_s2(FALSE)
  }
  
  # ---------- Reactives ---------------------------------------------------
  raw_data             <- shiny::reactiveVal()
  data                 <- shiny::reactiveVal()
  anon_data            <- shiny::reactiveVal()
  anonymized_cols      <- shiny::reactiveVal(character())
  initial_qids         <- shiny::reactiveVal(character())
  import_snip          <- shiny::reactiveVal(character())
  import_stata_snip    <- shiny::reactiveVal(character())
  import_py_snip       <- shiny::reactiveVal(character())
  r_steps              <- shiny::reactiveVal(character())
  stata_steps          <- shiny::reactiveVal(character())
  python_steps         <- shiny::reactiveVal(character())
  previous_data_stack  <- shiny::reactiveVal(list())
  previous_r_stack     <- shiny::reactiveVal(list())
  previous_stata_stack <- shiny::reactiveVal(list())
  previous_py_stack    <- shiny::reactiveVal(list())
  log_steps            <- shiny::reactiveVal(character())
  k_bins               <- shiny::reactiveVal(list())
  gen_assigned                <- shiny::reactiveVal(list())
  previous_gen_assigned_stack <- shiny::reactiveVal(list())
  last_method               <- shiny::reactiveVal(NULL)
  report_temp               <- shiny::reactiveVal(NULL)
  
  r_obj_name  <- shiny::reactiveVal(NULL)
  py_obj_name <- shiny::reactiveVal(NULL)
  
  geo_preview_layer   <- shiny::reactiveVal(NULL)
  geo_after_layer     <- shiny::reactiveVal(NULL)
  
  risk_before_metrics <- shiny::reactiveVal(NULL)
  risk_after_metrics  <- shiny::reactiveVal(NULL)
  
  # FIX: always treat method as stable internal code
  safe_method <- shiny::reactive(.norm_method(input$method))
  safe_selected_qids <- shiny::reactive(input$selected_qids %||% character())
  
  # ======================================================================
  # Load data from MAIN PLATFORM (rv_current) instead of upload
  # ======================================================================
  get_platform_df <- function() {
    if (is.null(rv_current)) return(NULL)
    df <- rv_current$working_df %||% rv_current$data %||% rv_current$df %||% NULL
    if (is.null(df)) return(NULL)
    tryCatch(as.data.frame(df, stringsAsFactors = FALSE), error = function(e) NULL)
  }
  
  init_state_from_df <- function(df, source_label = "platform_dataset") {
    r_obj_name(make.names(source_label))
    py_obj_name(gsub("[^A-Za-z0-9_]", "_", source_label))
    
    raw_data(df)
    data(df)
    anon_data(NULL)
    
    anonymized_cols(character())
    initial_qids(character())
    
    previous_data_stack(list())
    previous_r_stack(list())
    previous_stata_stack(list())
    previous_py_stack(list())
    
    log_steps(character())
    k_bins(list())
    
    gen_assigned(list())
    previous_gen_assigned_stack(list())
    
    geo_preview_layer(NULL)
    geo_after_layer(NULL)
    
    risk_before_metrics(NULL)
    risk_after_metrics(NULL)
    
    import_snip(c(
      .lbl("quant_anon_code_using_platform_ds"),
      "library(dplyr)",
      .lbl("quant_anon_code_no_import_needed")
    ))
    r_steps(import_snip())
    
    import_stata_snip(c(
      .lbl("quant_anon_code_using_platform_ds_stata"),
      .lbl("quant_anon_code_no_import_needed_stata")
    ))
    stata_steps(import_stata_snip())
    
    import_py_snip(c(
      .lbl("quant_anon_code_using_platform_ds_py"),
      .lbl("quant_anon_code_no_import_needed_py")
    ))
    python_steps(import_py_snip())
    
    shinyAce::updateAceEditor(session, "r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
  }
  
  shiny::observeEvent(input$use_platform_data, {
    df <- get_platform_df()
    if (is.null(df) || !nrow(df)) {
      .notify_err("quant_anon_notif_no_platform_ds")
      return()
    }
    init_state_from_df(df, source_label = "platform_dataset")
    .notify_msg("quant_anon_notif_loaded_platform_ds")
  }, ignoreInit = TRUE)
  
  # ======================================================================
  # Upload behavior (optional)
  # ======================================================================
  shiny::observeEvent(input$file, {
    shiny::req(input$file)
    
    fname <- input$file$name
    ext   <- tools::file_ext(fname)
    dname_raw <- tools::file_path_sans_ext(fname)
    
    r_name  <- make.names(dname_raw)
    py_name <- gsub("[^A-Za-z0-9_]", "_", dname_raw)
    r_obj_name(r_name)
    py_obj_name(py_name)
    
    df <- switch(
      tolower(ext),
      csv  = data.table::fread(input$file$datapath, data.table = FALSE, showProgress = FALSE),
      xlsx = readxl::read_excel(input$file$datapath),
      dta  = haven::read_dta(input$file$datapath),
      { .notify_err("quant_anon_notif_unsupported_file"); return() }
    )
    
    raw_data(df)
    data(df)
    anon_data(NULL)
    
    anonymized_cols(character())
    initial_qids(character())
    
    previous_data_stack(list())
    previous_r_stack(list())
    previous_stata_stack(list())
    previous_py_stack(list())
    
    log_steps(character())
    k_bins(list())
    
    gen_assigned(list())
    previous_gen_assigned_stack(list())
    
    geo_preview_layer(NULL)
    geo_after_layer(NULL)
    
    risk_before_metrics(NULL)
    risk_after_metrics(NULL)
    
    r_pkg_base <- c("library(dplyr)", "library(sdcMicro)")
    r_pkg_read <- if (tolower(ext) == "xlsx") "library(readxl)" else if (tolower(ext) == "dta") "library(haven)" else NULL
    
    r_read <- if (tolower(ext) == "csv") {
      sprintf("%s <- read.csv(%s, stringsAsFactors = FALSE)", r_name, shQuote(fname))
    } else if (tolower(ext) == "xlsx") {
      sprintf("%s <- read_excel(%s)", r_name, shQuote(fname))
    } else {
      sprintf("%s <- read_dta(%s)", r_name, shQuote(fname))
    }
    
    import_snip(c(.lbl("quant_anon_code_load_data"), r_pkg_base, r_pkg_read, r_read))
    r_steps(import_snip())
    
    stata_snip <- switch(
      tolower(ext),
      csv  = sprintf("import delimited %s, clear", shQuote(fname)),
      xlsx = sprintf("import excel %s, clear",     shQuote(fname)),
      dta  = sprintf("use %s, clear",              shQuote(fname))
    )
    import_stata_snip(c(.lbl("quant_anon_code_load_data_stata"), stata_snip))
    stata_steps(import_stata_snip())
    
    python_snip <- switch(
      tolower(ext),
      csv  = c(.lbl("quant_anon_code_load_data_py"), "import pandas as pd", sprintf("%s = pd.read_csv(%s)", py_name, shQuote(fname))),
      xlsx = c(.lbl("quant_anon_code_load_data_py"), "import pandas as pd", sprintf("%s = pd.read_excel(%s)", py_name, shQuote(fname))),
      dta  = c(.lbl("quant_anon_code_load_data_py"), "import pandas as pd", "import pyreadstat",
               sprintf("%s, meta = pyreadstat.read_dta(%s)", py_name, shQuote(fname)))
    )
    
    import_py_snip(python_snip)
    python_steps(import_py_snip())
    
    shinyAce::updateAceEditor(session, "r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
  })
  
  # ---------- Bin-Size Advisor -------------------------------------------
  shiny::observe({
    shiny::req(data())
    nums <- names(data())[sapply(data(), is.numeric)]
    shiny::updateSelectInput(
      session, "advisor_var",
      choices  = nums,
      selected = if (length(nums) > 0) nums[1] else NULL
    )
  })
  
  advisor_x <- shiny::reactive({
    shiny::req(data(), input$advisor_var)
    stats::na.omit(data()[[input$advisor_var]])
  })
  
  output$advisor_dist <- shiny::renderPlot({
    shiny::req(advisor_x())
    x <- advisor_x()
    hist(
      x,
      main   = paste(.lbl("quant_anon_histogram_of"), input$advisor_var),
      xlab   = input$advisor_var,
      border = "white"
    )
  })
  
  output$advisor_summary <- shiny::renderPrint({
    shiny::req(advisor_x())
    x <- advisor_x()
    stats_out <- c(
      Mean   = mean(x),
      Median = median(x),
      IQR    = IQR(x),
      SD     = sd(x),
      Min    = min(x),
      Max    = max(x)
    )
    print(stats_out)
  })
  
  bin_advice <- shiny::eventReactive(input$advisor_run, {
    shiny::req(advisor_x())
    x <- advisor_x()
    n <- length(x)
    rng <- range(x)
    range_x <- diff(rng)
    iqr_x <- IQR(x)
    sd_x <- sd(x)
    
    k_sturges <- ceiling(log2(n) + 1)
    w_sturges <- range_x / k_sturges
    
    w_fd  <- if (iqr_x > 0) 2 * iqr_x / (n^(1 / 3)) else NA
    k_fd  <- if (!is.na(w_fd) && w_fd > 0) ceiling(range_x / w_fd) else NA
    
    w_scott <- if (sd_x > 0) 3.5 * sd_x / (n^(1 / 3)) else NA
    k_scott <- if (!is.na(w_scott) && w_scott > 0) ceiling(range_x / w_scott) else NA
    
    k_sqrt <- ceiling(sqrt(n))
    w_sqrt <- range_x / k_sqrt
    
    data.frame(
      Method    = c(.lbl("quant_anon_bins_method_sturges"),
                    .lbl("quant_anon_bins_method_fd"),
                    .lbl("quant_anon_bins_method_scott"),
                    .lbl("quant_anon_bins_method_sqrt")),
      Bin_Width = c(w_sturges, w_fd, w_scott, w_sqrt),
      Num_Bins  = c(k_sturges, k_fd, k_scott, k_sqrt),
      stringsAsFactors = FALSE
    )
  })
  
  output$advisor_table <- shiny::renderTable({
    df <- bin_advice()
    df$Bin_Width <- round(df$Bin_Width, 2)
    df
  }, striped = TRUE, hover = TRUE, spacing = "l")
  
  output$advisor_plot <- shiny::renderPlot({
    df <- bin_advice()
    shiny::req(nrow(df) > 0)
    x <- advisor_x()
    
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    rows <- ceiling(nrow(df) / 2)
    cols <- 2
    par(mfrow = c(rows, cols), mar = c(4, 4, 2, 1))
    
    for (i in seq_len(nrow(df))) {
      bw <- df$Bin_Width[i]
      if (!is.finite(bw) || bw <= 0) next
      brks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE) + bw, by = bw)
      hist(x, breaks = brks, main = df$Method[i], xlab = input$advisor_var, border = "white")
    }
  })
  
  # ---------- Risk metric calculators (cached) ----------------------------
  calc_risk_metrics <- function(df, cols) {
    if (!nrow(df) || !length(cols)) return(NULL)
    tbl <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(cols))) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(r = 1 / n)
    list(
      avg = mean(tbl$r),
      max = max(tbl$r),
      pct_unique = mean(tbl$n == 1)
    )
  }
  
  shiny::observeEvent(list(data(), initial_qids()), {
    shiny::req(data(), initial_qids())
    q <- risk_qids(initial_qids())
    if (!length(q)) { risk_before_metrics(NULL); return() }
    risk_before_metrics(calc_risk_metrics(data(), q))
  }, ignoreInit = TRUE)
  
  shiny::observeEvent(list(anon_data(), initial_qids()), {
    shiny::req(anon_data(), initial_qids())
    q <- risk_qids(initial_qids())
    if (!length(q)) { risk_after_metrics(NULL); return() }
    df_a <- anon_data()
    after_cols <- sapply(q, get_after_col, df_after = df_a, USE.NAMES = FALSE)
    risk_after_metrics(calc_risk_metrics(df_a, after_cols))
  }, ignoreInit = TRUE)
  
  # ---------- Risk AFTER (coords excluded) --------------------------------
  output$risk_after <- shiny::renderUI({
    shiny::req(anon_data(), initial_qids())
    q <- risk_qids(initial_qids())
    vld(need(length(q) > 0, .lbl("quant_anon_need_noncoord_qids")))
    
    m <- risk_after_metrics()
    shiny::req(m)
    
    shiny::tags$p(sprintf(
      .lbl("quant_anon_risk_after_text"),
      m$avg, m$max, m$pct_unique * 100
    ))
  })
  
  output$gauge_after <- flexdashboard::renderGauge({
    shiny::req(anon_data(), initial_qids())
    q <- risk_qids(initial_qids())
    vld(need(length(q) > 0, .lbl("quant_anon_need_noncoord_qids")))
    
    m <- risk_after_metrics()
    shiny::req(m)
    
    pct <- round(m$avg * 100, 2)
    flexdashboard::gauge(
      pct, min = 0, max = 100, symbol = "%",
      sectors = flexdashboard::gaugeSectors(success = c(0, 20), warning = c(20, 50), danger = c(50, 100)),
      label = sprintf("%.2f%%", pct), abbreviate = FALSE
    )
  })
  
  output$gauge_after_value <- shiny::renderText({
    shiny::req(anon_data(), initial_qids())
    q <- risk_qids(initial_qids())
    vld(need(length(q) > 0, .lbl("quant_anon_need_noncoord_qids")))
    
    m <- risk_after_metrics()
    shiny::req(m)
    
    sprintf("%.2f%%", round(m$avg * 100, 2))
  })
  
  # ---------- Descriptions (TRANSLATABLE) ---------------------------------
  app_dir <- shiny::getShinyOption("appDir")
  if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
  
  manual2_rmd <- normalizePath(file.path(app_dir, "static_files", "anon", "docs", "descriptions1.Rmd"), mustWork = FALSE)
  labels_xlsx <- normalizePath(file.path(app_dir, "labelling_file_with_manual_full.xlsx"), mustWork = FALSE)
  
  output$descriptions_panel <- shiny::renderUI({
    lang <- .get_lang("English")
    shiny::req(file.exists(manual2_rmd))
    shiny::req(file.exists(labels_xlsx))
    
    out_dir  <- tempdir()
    out_file <- file.path(out_dir, paste0("descriptions1_", lang, ".html"))
    
    ok <- TRUE
    tryCatch({
      rmarkdown::render(
        input         = manual2_rmd,
        output_file   = basename(out_file),
        output_dir    = out_dir,
        output_format = "html_document",
        params        = list(language = lang, labels_path = labels_xlsx),
        envir         = new.env(parent = globalenv()),
        quiet         = TRUE
      )
    }, error = function(e) {
      ok <<- FALSE
      shiny::showNotification(sprintf("%s: %s", .lbl("quant_anon_desc_render_error"), e$message), type = "error")
    })
    
    if (!ok) return(NULL)
    
    res_name_tmp <- paste0("anon_docs_tmp_", gsub("[^A-Za-z0-9]", "_", lang), "_", session$token)
    shiny::addResourcePath(res_name_tmp, out_dir)
    
    shiny::tags$iframe(
      src   = paste0(res_name_tmp, "/", basename(out_file), "?v=", as.integer(Sys.time())),
      style = "width:100%; height:calc(100vh - 150px); border:none;"
    )
  })
  
  # ---------- Suppress & Remove Direct Identifiers ------------------------
  output$identifier_selector <- shiny::renderUI({
    shiny::req(data())
    shiny::tagList(
      shiny::checkboxGroupInput(ns("direct_ids"), .lbl("quant_anon_select_direct_ids_remove"), choices = names(data())),
      shiny::actionButton(ns("remove_ids"), .lbl("quant_anon_btn_remove_ids"), class = "btn btn-danger btn-block")
    )
  })
  
  shiny::observeEvent(input$remove_ids, {
    shiny::req(data(), input$direct_ids)
    
    previous_data_stack(c(previous_data_stack(), list(if (is.null(anon_data())) data() else anon_data())))
    previous_r_stack(c(previous_r_stack(), list(r_steps())))
    previous_stata_stack(c(previous_stata_stack(), list(stata_steps())))
    previous_py_stack(c(previous_py_stack(), list(python_steps())))
    
    df2 <- data()[, !names(data()) %in% input$direct_ids, drop = FALSE]
    data(df2)
    anon_data(NULL)
    anonymized_cols(character())
    initial_qids(character())
    
    risk_before_metrics(NULL)
    risk_after_metrics(NULL)
    
    cols   <- input$direct_ids
    r_name <- r_obj_name() %||% "platform_dataset"
    py_name <- py_obj_name() %||% "platform_dataset"
    
    code_r <- paste0(
      "# ", .lbl("quant_anon_code_suppression"), "\nlibrary(dplyr)\n",
      r_name, " <- ", r_name, " %>% dplyr::select(-", paste(cols, collapse = ", -"), ")"
    )
    code_s <- paste0("# ", .lbl("quant_anon_code_suppression"), "\n", "drop ", paste(cols, collapse = " "))
    code_py <- paste0(
      "# ", .lbl("quant_anon_code_suppression"), "\nimport pandas as pd\n",
      py_name, ".drop(columns=[", paste(shQuote(cols), collapse = ", "), "], inplace=True)"
    )
    
    r_steps(c(r_steps(), code_r))
    stata_steps(c(stata_steps(), code_s))
    python_steps(c(python_steps(), code_py))
    log_steps(c(log_steps(), paste(Sys.time(), "-", .lbl("quant_anon_log_ids_suppressed"))))
    
    shinyAce::updateAceEditor(session, "r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
    
    .notify_msg("quant_anon_notif_ids_removed")
  })
  
  # ---------- QID picker & sync -------------------------------------------
  output$bucket_ui <- shiny::renderUI({
    shiny::req(data())
    avail <- setdiff(names(data()), safe_selected_qids())
    
    sortable::bucket_list(
      header      = .lbl("quant_anon_bucket_header"),
      group_name  = ns("qid_group"),
      orientation = "horizontal",
      sortable::add_rank_list(.lbl("quant_anon_bucket_available"), ns("available_vars"), labels = avail),
      sortable::add_rank_list(.lbl("quant_anon_bucket_selected"),  ns("selected_qids"), labels = initial_qids())
    )
  })
  
  shiny::observeEvent(input$selected_qids, {
    initial_qids(input$selected_qids %||% character())
  }, ignoreNULL = FALSE)
  
  shiny::observe({
    shiny::req(initial_qids())
    if (!isTRUE(safe_method() == "generalization")) return()
    
    choices <- initial_qids()
    if (length(choices) == 0) return()
    
    current <- isolate(input$gen_var)
    if (is.null(current) || !(current %in% choices)) {
      shiny::updateSelectInput(session, "gen_var", choices = choices, selected = choices[1])
    } else {
      shiny::updateSelectInput(session, "gen_var", choices = choices, selected = current)
    }
  })
  
  # ---------- Technique parameters UI (server side) -----------------------
  output$k_num_picker <- shiny::renderUI({
    shiny::req(data(), initial_qids())
    nq <- initial_qids()[sapply(data()[, initial_qids(), drop = FALSE], is.numeric)]
    if (length(nq) == 0) return(shiny::helpText(.lbl("quant_anon_no_numeric_qids")))
    shiny::selectInput(ns("k_bucket_var"), .lbl("quant_anon_numeric_qid_to_bucket"), choices = nq, selected = nq[1])
  })
  
  output$extra_input <- shiny::renderUI({
    shiny::req(data())
    cols <- names(data())
    
    # geo_mode choices
    geo_choices <- .choice_vec("quant_anon_geo_mode", lang = .get_lang("English"))
    if (is.null(geo_choices) || !length(geo_choices)) {
      geo_choices <- stats::setNames(
        c("aggregate", "truncate"),
        c(.lbl("quant_anon_geo_mode_aggregate_fallback"),
          .lbl("quant_anon_geo_mode_truncate_fallback"))
      )
    }
    
    switch(
      safe_method(),
      
      "masking"          = shiny::checkboxGroupInput(ns("mask_cols"), .lbl("quant_anon_cols_to_mask"), choices = cols),
      "suppression"      = shiny::checkboxGroupInput(ns("supp_cols"), .lbl("quant_anon_cols_to_suppress"), choices = cols),
      "pseudonymization" = shiny::checkboxGroupInput(ns("ps_cols"),   .lbl("quant_anon_cols_to_pseudonymize"), choices = cols),
      "tokenization"     = shiny::checkboxGroupInput(ns("tok_cols"),  .lbl("quant_anon_cols_to_tokenize"), choices = cols),
      
      "bucketing" = shiny::tagList(
        shiny::selectInput(ns("bucket_col"), .lbl("quant_anon_col_to_bucket"), choices = cols),
        shiny::numericInput(ns("bin_interval"), .lbl("quant_anon_bin_size"), value = 4, min = 1)
      ),
      
      "kanonymity" = shiny::tagList(
        shiny::uiOutput(ns("k_num_picker")),
        shiny::numericInput(ns("k_bin_size"), .lbl("quant_anon_bin_size"), value = 5, min = 1),
        shiny::numericInput(ns("k_value"), .lbl("quant_anon_k_threshold"), value = 2, min = 2)
      ),
      
      "ldiversity" = shiny::tagList(
        shiny::selectInput(ns("sensitive_attr"), .lbl("quant_anon_sensitive_attribute"), choices = cols),
        shiny::numericInput(ns("l_value"), .lbl("quant_anon_l_threshold"), value = 2, min = 2)
      ),
      
      "tcloseness" = shiny::tagList(
        shiny::selectInput(ns("sensitive_attr_tc"), .lbl("quant_anon_sensitive_attribute"), choices = cols),
        shiny::numericInput(ns("t_threshold"), .lbl("quant_anon_t_threshold"), value = 0.1, min = 0, step = 0.01)
      ),
      
      "generalization" = shiny::tagList(
        shiny::selectInput(ns("gen_var"), .lbl("quant_anon_variable_to_generalize"), choices = initial_qids()),
        shiny::uiOutput(ns("gen_groups_ui"))
      ),
      
      "anonymizecoordinates" = shiny::tagList(
        shiny::selectInput(ns("geo_lat_col"), .lbl("quant_anon_latitude_column"), choices = cols),
        shiny::selectInput(ns("geo_lon_col"), .lbl("quant_anon_longitude_column"), choices = cols),
        shiny::radioButtons(
          ns("geo_mode"), .lbl("quant_anon_geo_mode_label"),
          choices  = geo_choices,
          selected = "aggregate", inline = TRUE
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'truncate'", ns("geo_mode")),
          shiny::numericInput(ns("geo_decimals"), .lbl("quant_anon_keep_decimals"), value = 3, min = 0, step = 1),
          shiny::helpText(.lbl("quant_anon_truncate_help"))
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'aggregate'", ns("geo_mode")),
          shiny::numericInput(ns("geo_grid_m"), .lbl("quant_anon_grid_cell_size_m"), value = 500, min = 10, step = 10),
          shiny::helpText(.lbl("quant_anon_aggregate_help"))
        ),
        shiny::helpText(.lbl("quant_anon_geo_preview_help"))
      ),
      
      NULL
    )
  })
  
  # ---------- Apply anonymization (incl. GEO) -----------------------------
  shiny::observeEvent(input$apply, {
    shiny::req(data(), input$method)
    
    method_code <- safe_method()
    if (method_code != "anonymizecoordinates") shiny::req(initial_qids())
    
    last_method(method_code)
    
    previous_data_stack(c(previous_data_stack(), list(if (is.null(anon_data())) data() else anon_data())))
    previous_r_stack(c(previous_r_stack(), list(r_steps())))
    previous_stata_stack(c(previous_stata_stack(), list(stata_steps())))
    previous_py_stack(c(previous_py_stack(), list(python_steps())))
    
    df        <- if (is.null(anon_data())) data() else anon_data()
    code_r    <- code_s <- code_py <- NULL
    step_cols <- NULL
    r_name    <- r_obj_name() %||% "platform_dataset"
    py_name   <- py_obj_name() %||% "platform_dataset"
    
    if (method_code == "masking") {
      step_cols <- input$mask_cols
      if (length(step_cols) == 0) { .notify_err("quant_anon_notif_select_mask_cols"); return() }
      
      df[step_cols] <- lapply(df[step_cols], function(x)
        ifelse(is.na(x), NA_character_, strrep("*", 10))
      )
      
      code_r <- paste0(
        "# ", .lbl("quant_anon_code_masking"), "\nlibrary(dplyr)\n",
        r_name, " <- ", r_name, " %>% dplyr::mutate(across(all_of(c(",
        paste(shQuote(step_cols), collapse = ", "),
        ")), ~ ifelse(is.na(.), NA_character_, strrep(\"*\", 10))))"
      )
      
      code_s <- paste0(
        "* ", .lbl("quant_anon_code_masking"), "\n",
        "foreach v of varlist ", paste(step_cols, collapse = " "), " {\n",
        "  capture confirm numeric variable `v'\n",
        "  if !_rc { tostring `v', replace force }\n",
        "  replace `v' = \"**********\" if !missing(`v')\n",
        "}"
      )
      
      code_py <- paste0(
        "# ", .lbl("quant_anon_code_masking"), "\n",
        "import pandas as pd\n",
        "for col in [", paste(shQuote(step_cols), collapse = ", "), "]:\n",
        "    ", py_name, "[col] = ", py_name, "[col].apply(lambda x: None if pd.isna(x) else '*'*10)"
      )
      
    } else if (method_code == "suppression") {
      step_cols <- input$supp_cols
      if (length(step_cols) == 0) { .notify_err("quant_anon_notif_select_suppress_cols"); return() }
      
      df <- df[, !names(df) %in% step_cols, drop = FALSE]
      
      code_r <- paste0(
        "# ", .lbl("quant_anon_code_suppression"), "\nlibrary(dplyr)\n",
        r_name, " <- ", r_name, " %>% dplyr::select(-", paste(step_cols, collapse = ", -"), ")"
      )
      code_s  <- paste0("* ", .lbl("quant_anon_code_suppression"), "\n", "drop ", paste(step_cols, collapse = " "))
      code_py <- paste0(
        "# ", .lbl("quant_anon_code_suppression"), "\nimport pandas as pd\n",
        py_name, ".drop(columns=[", paste(shQuote(step_cols), collapse = ", "), "], inplace=True)"
      )
      
    } else if (method_code == "bucketing") {
      step_cols <- input$bucket_col; b <- input$bin_interval
      if (is.null(step_cols) || is.null(b)) { .notify_err("quant_anon_notif_choose_col_bin"); return() }
      
      vals <- df[[step_cols]]
      if (!is.numeric(vals)) { .notify_err("quant_anon_notif_bucket_requires_numeric"); return() }
      
      minv <- suppressWarnings(min(vals, na.rm = TRUE)); maxv <- suppressWarnings(max(vals, na.rm = TRUE))
      if (!is.finite(minv) || !is.finite(maxv)) { .notify_err("quant_anon_notif_no_finite"); return() }
      
      start <- floor(minv / b) * b; end <- ceiling((maxv + 1) / b) * b
      brks <- seq(start, end, by = b)
      lbls <- paste0(head(brks, -1), "-", brks[-1] - 1)
      
      df[[step_cols]] <- cut(vals, breaks = brks, labels = lbls, include.lowest = TRUE, right = FALSE)
      
      code_r <- paste0(
        "# ", .lbl("quant_anon_code_bucketing"), "\nlibrary(dplyr)\n",
        r_name, " <- ", r_name, " %>% dplyr::mutate(", step_cols, " = cut(", step_cols, ",\n",
        "  breaks = seq(", start, ", ", end, ", by=", b, "),\n",
        "  labels = c(", paste(shQuote(lbls), collapse = ", "), "), right=FALSE, include_lowest=TRUE))"
      )
      
      code_s <- paste0(
        "* ", .lbl("quant_anon_code_bucketing"), "\n",
        "gen long __b = floor(", step_cols, "/", b, ")*", b, "\n",
        "tostring __b, gen(__bstr)\n",
        "gen long __e = __b + ", b, " - 1\n",
        "tostring __e, gen(__estr)\n",
        "replace ", step_cols, " = __bstr + \"-\" + __estr if !missing(", step_cols, ")\n",
        "drop __b __e __bstr __estr"
      )
      
      code_py <- paste0(
        "# ", .lbl("quant_anon_code_bucketing"), "\nimport pandas as pd\nimport numpy as np\n",
        "b = ", b, "\n",
        "minv = int(np.floor(", py_name, "['", step_cols, "'].min()/b)*b)\n",
        "maxv = int(np.ceil((", py_name, "['", step_cols, "'].max()+1)/b)*b)\n",
        "bins = list(range(minv, maxv+1, b))\n",
        "labels = [f\"{bins[i]}-{bins[i+1]-1}\" for i in range(len(bins)-1)]\n",
        py_name, "['", step_cols, "'] = pd.cut(", py_name, "['", step_cols, "'], bins=bins, labels=labels, right=False, include_lowest=True)"
      )
      
    } else if (method_code == "pseudonymization") {
      step_cols <- input$ps_cols
      if (length(step_cols) == 0) { .notify_err("quant_anon_notif_select_pseudonymize_cols"); return() }
      
      salt_vec <- uuid::UUIDgenerate(n = nrow(df))
      hash_vec <- function(values, salts) {
        mapply(function(v, s) digest::digest(paste0(as.character(v), s), algo = "sha256"),
               values, salts, USE.NAMES = FALSE)
      }
      for (col in step_cols) {
        x <- df[[col]]
        df[[col]] <- ifelse(is.na(x), NA_character_, hash_vec(x, salt_vec))
      }
      
      code_r <- paste0("# ", .lbl("quant_anon_code_pseudonymization_applied"))
      code_s <- paste0("* ", .lbl("quant_anon_code_no_stata_analogue"))
      code_py <- paste0("# ", .lbl("quant_anon_code_pseudonymization_applied"))
      
    } else if (method_code == "tokenization") {
      step_cols <- input$tok_cols
      if (length(step_cols) == 0) { .notify_err("quant_anon_notif_select_tokenize_cols"); return() }
      
      df <- dplyr::mutate(
        df,
        dplyr::across(
          dplyr::all_of(step_cols),
          ~ ifelse(is.na(.x), NA_character_, stringi::stri_rand_strings(length(.x), 10, pattern = "[A-Za-z0-9]"))
        )
      )
      
      code_r <- paste0("# ", .lbl("quant_anon_code_tokenization_applied"))
      code_s <- paste0("* ", .lbl("quant_anon_code_no_stata_analogue"))
      code_py <- paste0("# ", .lbl("quant_anon_code_tokenization_applied"))
      
    } else if (method_code == "kanonymity") {
      # (unchanged from your original; method_code already equals "kanonymity")
      qids_all <- initial_qids()
      k_val    <- as.integer(input$k_value)
      var_now  <- input$k_bucket_var
      bin_size <- as.integer(input$k_bin_size)
      
      vld(
        need(length(qids_all) > 0, .lbl("quant_anon_need_select_qids_step2")),
        need(!is.na(k_val) && k_val >= 2, .lbl("quant_anon_need_k_ge_2"))
      )
      
      bins <- k_bins()
      if (!is.null(var_now) && !is.na(bin_size)) { bins[[var_now]] <- bin_size; k_bins(bins) }
      
      bucket_one <- function(x, b) {
        if (!is.numeric(x)) return(x)
        rng <- range(x, na.rm = TRUE)
        if (!all(is.finite(rng))) return(x)
        start <- floor(rng[1]/b)*b
        end   <- ceiling((rng[2]+1)/b)*b
        cut(x, breaks = seq(start, end, by = b), right = FALSE, include.lowest = TRUE,
            labels = sprintf("%d-%d", head(seq(start, end, by=b), -1), seq(start, end, by=b)[-1]-1))
      }
      
      if (length(bins)) for (nm in names(bins)) if (nm %in% names(df)) df[[nm]] <- bucket_one(df[[nm]], bins[[nm]])
      k_qids_use <- intersect(names(bins), qids_all)
      vld(need(length(k_qids_use) > 0, .lbl("quant_anon_need_bucket_one_numeric_qid")))
      
      grouped_sizes <- df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(k_qids_use))) %>%
        dplyr::tally(name=".__size__")
      
      df_tagged <- df %>%
        dplyr::left_join(grouped_sizes, by = k_qids_use) %>%
        dplyr::mutate(.__ok__ = .__size__ >= k_val)
      
      df_ok  <- df_tagged %>% dplyr::filter(.__ok__)  %>% dplyr::select(-.__ok__, -.__size__)
      df_bad <- df_tagged %>% dplyr::filter(!.__ok__) %>% dplyr::select(-.__ok__, -.__size__)
      df <- dplyr::bind_rows(df_ok, df_bad)
      
      anon_data(df)
      anonymized_cols(union(anonymized_cols(), k_qids_use))
      
      code_r  <- paste0("# ", .lbl("quant_anon_code_kanonymity_applied"))
      code_s  <- paste0("* ", .lbl("quant_anon_code_no_stata_analogue"))
      code_py <- paste0("# ", .lbl("quant_anon_code_no_python_analogue"))
      
    } else if (method_code == "tcloseness") {
      step_cols <- risk_qids(initial_qids())
      vld(need(length(step_cols) > 0, .lbl("quant_anon_need_noncoord_qids")))
      
      df <- apply_t_closeness(df, qids = step_cols, sensitive = input$sensitive_attr_tc, t = input$t_threshold)
      
      code_r  <- paste0("# ", .lbl("quant_anon_code_tcloseness_applied"))
      code_s  <- paste0("* ", .lbl("quant_anon_code_no_stata_analogue"))
      code_py <- paste0("# ", .lbl("quant_anon_code_tcloseness_applied"))
      
    } else if (method_code == "anonymizecoordinates") {
      # (unchanged from your original; only switched to method_code)
      latc <- input$geo_lat_col; lonc <- input$geo_lon_col
      
      vld(
        need(!is.null(latc) && !is.null(lonc), .lbl("quant_anon_need_pick_lat_lon")),
        need(latc %in% names(df) && lonc %in% names(df), .lbl("quant_anon_need_valid_coord_cols")),
        need(is.numeric(df[[latc]]), .lbl("quant_anon_need_lat_numeric")),
        need(is.numeric(df[[lonc]]), .lbl("quant_anon_need_lon_numeric"))
      )
      
      pts_df <- df[, c(lonc, latc), drop = FALSE]
      names(pts_df) <- c("lon","lat")
      comp_mask_full <- stats::complete.cases(pts_df)
      pts_df <- pts_df[comp_mask_full, , drop = FALSE]
      
      if (!nrow(pts_df)) {
        .notify_err("quant_anon_notif_no_valid_coord_rows"); return()
      }
      
      pts_wgs <- sf::st_as_sf(pts_df, coords = c("lon","lat"), crs = 4326)
      xy      <- sf::st_coordinates(pts_wgs)
      epsg    <- guess_utm_epsg(xy[,1], xy[,2])
      pts_utm <- safe_transform(pts_wgs, epsg)
      
      mode <- input$geo_mode %||% "aggregate"
      cell_wkt_full <- rep(NA_character_, nrow(df))
      
      if (mode == "truncate") {
        d  <- max(0, suppressWarnings(as.integer(input$geo_decimals))); if (is.na(d)) d <- 3
        trunc_dec <- function(x, d) { f <- 10^d; sign(x) * floor(abs(x) * f) / f }
        
        xy_wgs <- sf::st_coordinates(safe_transform(pts_utm, 4326))
        lon_a  <- trunc_dec(xy_wgs[,1], d)
        lat_a  <- trunc_dec(xy_wgs[,2], d)
        
        lat_mean <- mean(lat_a, na.rm = TRUE)
        m_per_deg_lat <- 111320
        m_per_deg_lon <- 111320 * cos(lat_mean * pi/180)
        cell_m <- nz_pos(min(m_per_deg_lat, m_per_deg_lon) / (10^d), 100)
        
        agg    <- build_grid_agg(pts_utm, cell_m)
        agg_wgs <- safe_transform(agg, 4326)
        geo_after_layer(agg_wgs)
        
        if (nrow(agg)) {
          join_ix <- sf::st_intersects(pts_utm, agg)
          agg_wkt <- sf::st_as_text(sf::st_geometry(agg_wgs))
          
          cell_wkt_new <- rep(NA_character_, nrow(pts_utm))
          for (i in seq_len(nrow(pts_utm))) {
            cell_idx <- if (length(join_ix[[i]]) >= 1) join_ix[[i]][1] else NA_integer_
            if (is.finite(cell_idx)) cell_wkt_new[i] <- agg_wkt[cell_idx]
          }
          
          df_rows_idx <- which(comp_mask_full)
          df[[lonc]][df_rows_idx] <- lon_a
          df[[latc]][df_rows_idx] <- lat_a
          cell_wkt_full[df_rows_idx] <- cell_wkt_new
        } else {
          .notify_warn("quant_anon_notif_no_grid_cells_decimals")
        }
        
        anonymized_cols(union(anonymized_cols(), c(latc, lonc, "cell_wkt")))
        .notify_msg("quant_anon_notif_coord_truncate_applied")
        
      } else if (mode == "aggregate") {
        gsize <- nz_pos(input$geo_grid_m, 500)
        
        agg <- build_grid_agg(pts_utm, gsize)
        agg_wgs <- safe_transform(agg, 4326)
        geo_after_layer(agg_wgs)
        
        if (nrow(agg)) {
          join_ix <- sf::st_intersects(pts_utm, agg)
          
          cents <- sf::st_centroid(agg)
          cents_wgs <- safe_transform(cents, 4326)
          cxy <- sf::st_coordinates(cents_wgs)
          
          agg_wkt <- sf::st_as_text(sf::st_geometry(agg_wgs))
          
          lon_new <- rep(NA_real_, nrow(pts_utm))
          lat_new <- rep(NA_real_, nrow(pts_utm))
          cell_wkt_new <- rep(NA_character_, nrow(pts_utm))
          
          for (i in seq_len(nrow(pts_utm))) {
            cell_idx <- if (length(join_ix[[i]]) >= 1) join_ix[[i]][1] else NA_integer_
            if (is.finite(cell_idx)) {
              lon_new[i] <- cxy[cell_idx, 1]
              lat_new[i] <- cxy[cell_idx, 2]
              cell_wkt_new[i] <- agg_wkt[cell_idx]
            }
          }
          
          df_rows_idx <- which(comp_mask_full)
          df[[lonc]][df_rows_idx] <- lon_new
          df[[latc]][df_rows_idx] <- lat_new
          cell_wkt_full[df_rows_idx] <- cell_wkt_new
          
        } else {
          .notify_warn("quant_anon_notif_no_grid_cells_grid")
        }
        
        anonymized_cols(union(anonymized_cols(), c(latc, lonc, "cell_wkt")))
        .notify_msg("quant_anon_notif_coord_aggregate_applied")
        
      } else {
        .notify_err("quant_anon_notif_unsupported_mode"); return()
      }
      
      df$cell_wkt <- cell_wkt_full
      df <- df[, setdiff(names(df), c(lonc, latc)), drop = FALSE]
      
      anon_data(df)
      log_steps(c(log_steps(), paste(Sys.time(), "-", .lbl("quant_anon_log_applied_coord"), mode)))
      geo_preview_layer(NULL)
      
      code_r  <- paste0("# ", .lbl("quant_anon_code_coord_applied"))
      code_s  <- paste0("* ", .lbl("quant_anon_code_no_stata_analogue"))
      code_py <- paste0("# ", .lbl("quant_anon_code_coord_applied"))
    }
    
    if (!is.null(step_cols) && method_code %in% c(
      "masking","suppression","bucketing","pseudonymization","tokenization","tcloseness"
    )) {
      anon_data(df)
      anonymized_cols(union(anonymized_cols(), step_cols))
    }
    
    if (!is.null(code_r))  r_steps(   c(r_steps(),    code_r))
    if (!is.null(code_s))  stata_steps(c(stata_steps(), code_s))
    if (!is.null(code_py)) python_steps(c(python_steps(), code_py))
    
    if (method_code != "anonymizecoordinates") {
      log_steps(c(log_steps(), paste(Sys.time(), "-", .lbl("quant_anon_log_applied_method"), method_code)))
    }
    
    shinyAce::updateAceEditor(session, "r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
  })
  
  # ---------- K-Report ----------------------------------------------------
  output$k_report <- shiny::renderUI({
    shiny::req(last_method())
    if (last_method() == "kanonymity") {
      bins <- k_bins(); k_val <- input$k_value
      shiny::tags$div(
        shiny::tags$h4(.lbl("quant_anon_k_report_title")),
        shiny::tags$p(paste0(.lbl("quant_anon_k_report_threshold_prefix"), " ", k_val)),
        if (length(bins) > 0)
          shiny::tags$p(paste0(.lbl("quant_anon_k_report_bucket_sizes_prefix"), " ",
                               paste(paste0(names(bins)," = ",bins), collapse=", ")))
      )
    }
  })
  
  # ---------- Generalization UI & Logic -----------------------------------
  output$gen_groups_ui <- shiny::renderUI({
    shiny::req(input$gen_var)
    working <- if (!is.null(anon_data()) && input$gen_var %in% names(anon_data())) anon_data() else data()
    is_num <- is.numeric(working[[input$gen_var]])
    default_mode <- if (is_num) "numeric" else "categorical"
    
    choices <- stats::setNames(
      c("categorical"),
      c(.lbl("quant_anon_generalization_mode_categorical"))
    )
    if (is_num) {
      choices <- c(
        choices,
        stats::setNames(
          c("numeric"),
          c(.lbl("quant_anon_generalization_mode_numeric"))
        )
      )
    }
    
    shiny::tagList(
      shiny::radioButtons(
        ns("gen_mode"),
        .lbl("quant_anon_generalization_mode"),
        choices  = choices,
        selected = default_mode,
        inline   = TRUE
      ),
      
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'categorical'", ns("gen_mode")),
        {
          cats <- unique(as.character(working[[input$gen_var]]))
          shiny::tagList(
            sortable::bucket_list(
              header     = .lbl("quant_anon_gen_drag_values"),
              group_name = ns("gen_group"),
              orientation= "horizontal",
              sortable::add_rank_list(.lbl("quant_anon_gen_available_categories"), ns("gen_available"), labels = cats),
              sortable::add_rank_list(.lbl("quant_anon_gen_selected_categories"),  ns("gen_selected"),  labels = NULL)
            ),
            shiny::textInput(ns("gen_new_label"), .lbl("quant_anon_gen_new_label")),
            shiny::actionButton(ns("apply_generalization"), .lbl("quant_anon_apply_generalization"), class = "btn btn-primary btn-block")
          )
        }
      ),
      
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'numeric'", ns("gen_mode")),
        shiny::tagList(
          shiny::helpText(.lbl("quant_anon_gen_numeric_help")),
          shiny::textInput(ns("gen_ranges_text"), .lbl("quant_anon_gen_ranges_label"),
                           value = "0-10:Group 1;11-40:Group 2;41-100:Group 3"),
          shiny::helpText(.lbl("quant_anon_gen_ranges_rules")),
          shiny::actionButton(ns("apply_generalization"), .lbl("quant_anon_apply_generalization"), class = "btn btn-primary btn-block")
        )
      )
    )
  })
  
  shiny::observeEvent(input$apply_generalization, {
    shiny::req(data(), input$gen_var, input$gen_mode)
    last_method("generalization")
    
    previous_data_stack(c(previous_data_stack(), list(if (is.null(anon_data())) data() else anon_data())))
    previous_r_stack(c(previous_r_stack(), list(r_steps())))
    previous_stata_stack(c(previous_stata_stack(), list(stata_steps())))
    previous_py_stack(c(previous_py_stack(), list(python_steps())))
    previous_gen_assigned_stack(c(previous_gen_assigned_stack(), list(gen_assigned())))
    
    df  <- if (is.null(anon_data())) data() else anon_data()
    var <- input$gen_var
    code_r <- code_s <- code_py <- NULL
    
    if (input$gen_mode == "categorical") {
      sel <- input$gen_selected; lab <- input$gen_new_label
      if (is.null(sel) || length(sel) == 0) { .notify_err("quant_anon_notif_no_categories_selected"); return() }
      if (is.null(lab) || trimws(lab) == "") { .notify_err("quant_anon_notif_provide_new_label"); return() }
      
      ga <- gen_assigned(); ga[[var]] <- unique(c(ga[[var]] %||% character(), sel)); gen_assigned(ga)
      df[[var]] <- as.character(df[[var]]); df[[var]][ df[[var]] %in% sel ] <- lab
      
      anon_data(df)
      anonymized_cols(union(anonymized_cols(), var))
      
      code_r <- paste0("# ", .lbl("quant_anon_code_generalization_applied"))
      code_s <- paste0("* ", .lbl("quant_anon_code_no_stata_analogue"))
      code_py <- paste0("# ", .lbl("quant_anon_code_generalization_applied"))
      
    } else if (input$gen_mode == "numeric") {
      shiny::req(input$gen_ranges_text); range_txt <- input$gen_ranges_text
      ranges_df <- tryCatch(parse_ranges_text(range_txt), error = function(e) {
        shiny::showNotification(sprintf("%s: %s", .lbl("quant_anon_range_parse_error"), e$message), type = "error")
        NULL
      })
      if (is.null(ranges_df)) return()
      
      if (!is.numeric(df[[var]])) {
        shiny::showNotification(sprintf(.lbl("quant_anon_err_var_must_be_numeric"), var), type = "error")
        return()
      }
      
      x <- df[[var]]
      generalized <- rep(NA_character_, length(x))
      
      for (i in seq_len(nrow(ranges_df))) {
        lower <- ranges_df$lower[i]; upper <- ranges_df$upper[i]; label <- as.character(ranges_df$label[i])
        in_range <- (x >= lower) & (x <= upper)
        if (is.infinite(lower) && lower < 0) in_range <- x <= upper
        if (is.infinite(upper) && upper > 0) in_range <- x >= lower
        generalized[in_range] <- label
      }
      
      df[[var]] <- generalized
      anon_data(df)
      anonymized_cols(union(anonymized_cols(), var))
      
      code_r <- paste0("# ", .lbl("quant_anon_code_generalization_applied"))
      code_s <- paste0("* ", .lbl("quant_anon_code_no_stata_analogue"))
      code_py <- paste0("# ", .lbl("quant_anon_code_generalization_applied"))
    }
    
    if (!is.null(code_r)) r_steps(c(r_steps(), code_r))
    if (!is.null(code_s)) stata_steps(c(stata_steps(), code_s))
    if (!is.null(code_py)) python_steps(c(python_steps(), code_py))
    
    log_steps(c(log_steps(), paste(Sys.time(), "-", .lbl("quant_anon_log_generalization_applied"), input$gen_mode, var)))
    
    shinyAce::updateAceEditor(session, "r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
    
    .notify_msg("quant_anon_notif_generalization_applied")
  })
  
  # ---------- Copy buttons ------------------------------------------------
  shiny::observeEvent(input$copy_r, {
    shinyjs::runjs(sprintf("copyAce('%s','%s')", ns("r_code_ace"), ns("copy_r")))
  })
  shiny::observeEvent(input$copy_stata, {
    shinyjs::runjs(sprintf("copyAce('%s','%s')", ns("stata_code_ace"), ns("copy_stata")))
  })
  shiny::observeEvent(input$copy_py, {
    shinyjs::runjs(sprintf("copyAce('%s','%s')", ns("python_code_ace"), ns("copy_py")))
  })
  
  # ---------- Undo & Reset ------------------------------------------------
  shiny::observeEvent(input$undo, {
    prev <- previous_data_stack(); pr <- previous_r_stack()
    ps   <- previous_stata_stack(); pp <- previous_py_stack()
    if (length(prev) > 0) {
      idx <- length(prev)
      anon_data(prev[[idx]])
      r_steps(pr[[idx]]); stata_steps(ps[[idx]]); python_steps(pp[[idx]])
      previous_data_stack(prev[-idx]); previous_r_stack(pr[-idx])
      previous_stata_stack(ps[-idx]); previous_py_stack(pp[-idx])
      log_steps(c(log_steps(), paste(Sys.time(), "-", .lbl("quant_anon_log_undo"))))
      
      pgs <- previous_gen_assigned_stack()
      if (length(pgs) > 0) {
        gen_assigned(pgs[[length(pgs)]])
        previous_gen_assigned_stack(pgs[-length(pgs)])
      }
      
      geo_preview_layer(NULL); geo_after_layer(NULL)
      
      .notify_msg("quant_anon_notif_undo_success")
      shinyAce::updateAceEditor(session,"r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
      shinyAce::updateAceEditor(session,"stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
      shinyAce::updateAceEditor(session,"python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
    }
  })
  
  shiny::observeEvent(input$reset, {
    shiny::req(raw_data())
    data(raw_data()); anon_data(NULL)
    anonymized_cols(character()); initial_qids(character())
    r_steps(import_snip()); stata_steps(import_stata_snip()); python_steps(import_py_snip())
    previous_data_stack(list()); previous_r_stack(list())
    previous_stata_stack(list()); previous_py_stack(list())
    log_steps(c(log_steps(), paste(Sys.time(), "-", .lbl("quant_anon_log_reset"))))
    k_bins(list()); gen_assigned(list()); previous_gen_assigned_stack(list())
    
    geo_preview_layer(NULL); geo_after_layer(NULL)
    
    risk_before_metrics(NULL)
    risk_after_metrics(NULL)
    
    .notify_warn("quant_anon_notif_reset_complete")
    shinyAce::updateAceEditor(session,"r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
    shinyAce::updateAceEditor(session,"stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
    shinyAce::updateAceEditor(session,"python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
  })
  
  # ---------- Preview merged table ----------------------------------------
  output$preview_merged <- shiny::renderTable({
    shiny::req(data())
    orig <- data()
    cur  <- if (is.null(anon_data())) orig else anon_data()
    
    pr <- list()
    for (col in anonymized_cols()) {
      if (col %in% names(orig) && col %in% names(cur)) {
        pr[[col]] <- orig[[col]]
        pr[[paste0(col, "_anon")]] <- cur[[col]]
      }
    }
    for (col in setdiff(names(cur), anonymized_cols())) pr[[col]] <- cur[[col]]
    
    head(as.data.frame(pr, stringsAsFactors = FALSE), 10)
  }, rownames = FALSE)
  
  # ---------- Map (module-safe leafletProxy) ------------------------------
  output$geo_map <- leaflet::renderLeaflet({
    shiny::req(safe_method() == "anonymizecoordinates", data())
    
    m <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 1)) |>
      leaflet::addTiles() |>
      leaflet::addScaleBar(position = "bottomleft")
    
    latc <- input$geo_lat_col
    lonc <- input$geo_lon_col
    
    if (!is.null(latc) && !is.null(lonc) &&
        latc %in% names(data()) && lonc %in% names(data()) &&
        is.numeric(data()[[latc]]) && is.numeric(data()[[lonc]])) {
      
      df0 <- data()[, c(lonc, latc), drop = FALSE]
      names(df0) <- c("lon","lat")
      df0 <- df0[stats::complete.cases(df0), , drop = FALSE]
      
      if (nrow(df0)) {
        m <- m |>
          leaflet::addCircleMarkers(
            lng = df0$lon, lat = df0$lat,
            radius = 3, stroke = FALSE, fillOpacity = 0.6,
            clusterOptions = leaflet::markerClusterOptions(),
            group = .lbl("quant_anon_map_group_before")
          )
        
        if (all(is.finite(range(df0$lon))) && all(is.finite(range(df0$lat)))) {
          m <- m |>
            leaflet::fitBounds(
              lng1 = min(df0$lon), lat1 = min(df0$lat),
              lng2 = max(df0$lon), lat2 = max(df0$lat)
            )
        }
      }
    }
    
    m |>
      leaflet::addLayersControl(
        overlayGroups = c(.lbl("quant_anon_map_group_before"), .lbl("quant_anon_map_group_after")),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  })
  
  shiny::observe({
    shiny::req(safe_method() == "anonymizecoordinates")
    pol <- geo_after_layer()
    
    prx <- leaflet::leafletProxy("geo_map", session = session)
    
    if (is.null(pol) || !inherits(pol, "sf") || nrow(pol) == 0) {
      prx |> leaflet::clearGroup(.lbl("quant_anon_map_group_after"))
      return()
    }
    
    pol <- sf::st_make_valid(pol)
    pol <- sf::st_zm(pol, drop = TRUE, what = "ZM")
    pol <- pol[!sf::st_is_empty(pol), , drop = FALSE]
    if (!nrow(pol)) { prx |> leaflet::clearGroup(.lbl("quant_anon_map_group_after")); return() }
    
    prx |>
      leaflet::clearGroup(.lbl("quant_anon_map_group_after")) |>
      leaflet::addPolygons(
        data = pol,
        weight = 1, color = "#555555", fillOpacity = 0.5, fillColor = "#2A9D8F",
        group = .lbl("quant_anon_map_group_after"),
        popup = ~paste0(.lbl("quant_anon_map_popup_n_prefix"), " ", n)
      )
  })
  
  # ---------- Risk BEFORE (coords excluded) --------------------------------
  output$risk_before <- shiny::renderUI({
    shiny::req(data(), initial_qids())
    q <- risk_qids(initial_qids())
    vld(need(length(q) > 0, .lbl("quant_anon_need_noncoord_qids")))
    
    m <- risk_before_metrics()
    shiny::req(m)
    
    shiny::tags$p(sprintf(
      .lbl("quant_anon_risk_before_text"),
      m$avg, m$max, m$pct_unique * 100
    ))
  })
  
  output$gauge_before <- flexdashboard::renderGauge({
    shiny::req(data(), initial_qids())
    q <- risk_qids(initial_qids())
    vld(need(length(q) > 0, .lbl("quant_anon_need_noncoord_qids")))
    
    m <- risk_before_metrics()
    shiny::req(m)
    
    pct <- round(m$avg * 100, 2)
    flexdashboard::gauge(
      pct, min=0, max=100, symbol="%",
      sectors = flexdashboard::gaugeSectors(success=c(0,20),warning=c(20,50),danger=c(50,100)),
      label=sprintf("%.2f%%", pct), abbreviate=FALSE
    )
  })
  
  # ---------- Row counts / log --------------------------------------------
  output$n_obs_text <- shiny::renderText({
    if (is.null(data())) return("")
    after_txt <- if (!is.null(anon_data())) paste0(" | ", .lbl("quant_anon_rows_after_prefix"), " ", nrow(anon_data())) else ""
    paste0(.lbl("quant_anon_rows_prefix"), " ", nrow(data()), after_txt)
  })
  
  output$step_log <- shiny::renderText({ paste(log_steps(), collapse = "\n") })
  
  # ---------- Downloads ----------------------------------------------------
  output$download <- shiny::downloadHandler(
    filename = function() paste0("anonymized_", Sys.Date(), ".csv"),
    content  = function(file) {
      dat <- anon_data()
      if (is.null(dat)) stop(.lbl("quant_anon_err_no_anon_data_download"))
      utils::write.csv(drop_geo_cols(dat), file, row.names = FALSE)
    }
  )
  
  output$download_excel <- shiny::downloadHandler(
    filename = function() paste0("anonymized_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      dat <- anon_data()
      if (is.null(dat)) stop(.lbl("quant_anon_err_no_anon_data_download"))
      openxlsx::write.xlsx(drop_geo_cols(dat), file)
    }
  )
  
  output$download_dta <- shiny::downloadHandler(
    filename = function() paste0("anonymized_", Sys.Date(), ".dta"),
    content  = function(file) {
      dat <- anon_data()
      if (is.null(dat)) stop(.lbl("quant_anon_err_no_anon_data_download"))
      haven::write_dta(drop_geo_cols(dat), file)
    }
  )
  
  # ---------- Report preview/download -------------------------------------
  render_html_report <- function() {
    shiny::req(data(), anon_data(), initial_qids())
    
    app_dir2 <- shiny::getShinyOption("appDir") %||% getwd()
    tmpl <- normalizePath(file.path(app_dir2, "static_files", "report_template.Rmd"), mustWork = FALSE)
    
    if (!file.exists(tmpl)) {
      shiny::showNotification(sprintf("%s: %s", .lbl("quant_anon_missing_report_template"), tmpl), type = "error")
      return(NULL)
    }
    
    q_main <- risk_qids(initial_qids())
    vld(need(length(q_main) > 0, .lbl("quant_anon_need_noncoord_qids")))
    
    before_tbl <- data() %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(q_main))) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(r = round(1 / n, 4))
    
    bm <- list(
      avg = round(mean(before_tbl$r), 4),
      max = round(max(before_tbl$r), 4),
      pct_unique = paste0(round(mean(before_tbl$n == 1)*100,4), "%")
    )
    
    df_after <- anon_data()
    after_cols_main <- sapply(q_main, get_after_col, df_after = df_after, USE.NAMES = FALSE)
    
    after_tbl <- df_after %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(after_cols_main))) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(r = round(1 / n, 4))
    
    am <- list(
      avg = round(mean(after_tbl$r), 4),
      max = round(max(after_tbl$r), 4),
      pct_unique = paste0(round(mean(after_tbl$n == 1)*100,4), "%")
    )
    
    all_subsets2 <- lapply(seq_along(q_main), function(k) combn(q_main, k, simplify = FALSE))
    subsets <- unlist(all_subsets2, recursive = FALSE)
    
    perm_tbl_before <- do.call(rbind, lapply(subsets, function(sub) {
      df_sub <- data() %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(sub))) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(r = round(1 / n, 4))
      data.frame(
        QIDs = paste(sub, collapse = ", "),
        Average_Risk = round(mean(df_sub$r), 4),
        Maximum_Risk = round(max(df_sub$r), 4),
        Percent_Unique = paste0(round(mean(df_sub$n == 1)*100,4), "%"),
        stringsAsFactors = FALSE
      )
    }))
    
    perm_tbl_after <- do.call(rbind, lapply(subsets, function(sub) {
      after_sub <- sapply(sub, get_after_col, df_after = df_after, USE.NAMES = FALSE)
      df_sub <- df_after %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(after_sub))) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(r = round(1 / n, 4))
      data.frame(
        QIDs = paste(sub, collapse = ", "),
        Average_Risk = round(mean(df_sub$r), 4),
        Maximum_Risk = round(max(df_sub$r), 4),
        Percent_Unique = paste0(round(mean(df_sub$n == 1)*100,4), "%"),
        stringsAsFactors = FALSE
      )
    }))
    
    tmp_html <- tempfile(fileext = ".html")
    rmarkdown::render(
      input         = tmpl,
      output_format = "html_document",
      output_file   = basename(tmp_html),
      output_dir    = dirname(tmp_html),
      params        = list(
        before_metrics    = bm,
        after_metrics     = am,
        perm_table_before = perm_tbl_before,
        perm_table_after  = perm_tbl_after
      ),
      envir = new.env(parent = globalenv()),
      quiet = TRUE
    )
    
    tmp_html
  }
  
  shiny::observeEvent(input$view_report, {
    shiny::req(data(), anon_data(), initial_qids())
    
    shiny::withProgress(message = .lbl("quant_anon_building_report"), value = 0.1, {
      path <- render_html_report()
      if (is.null(path)) return(NULL)
      res_name <- paste0("reports_tmp_", gsub("[^A-Za-z0-9]", "_", session$token))
      shiny::addResourcePath(res_name, dirname(path))
      report_temp(path)
      session$userData$report_res_name <- res_name
    })
    
    if (is.null(report_temp())) return(NULL)
    
    res_name <- session$userData$report_res_name %||% "reports_tmp"
    
    shiny::showModal(shiny::modalDialog(
      title = .lbl("quant_anon_risk_report_preview_title"),
      size  = "l", easyClose = TRUE,
      shiny::tags$iframe(
        src = paste0(res_name, "/", basename(report_temp())),
        style = "width:100%; height:600px; border:none;"
      ),
      footer = shiny::modalButton(.lbl("quant_anon_close"))
    ))
  })
  
  output$download_report <- shiny::downloadHandler(
    filename = function() paste0("risk_report_", Sys.Date(), ".pdf"),
    content  = function(file) {
      shiny::req(report_temp())
      if (requireNamespace("pagedown", quietly = TRUE)) {
        pagedown::chrome_print(input = report_temp(), output = file)
      } else {
        file.copy(report_temp(), file, overwrite = TRUE)
      }
    }
  )
}



mod_quant_anon_server <- function(id, rv_current = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    if (exists("anon_quant_server_logic", mode = "function", inherits = TRUE)) {
      get("anon_quant_server_logic", mode = "function", inherits = TRUE)(
        input, output, session,
        rv_current = rv_current
      )
    } else {
      shiny::showNotification(
        "anon_quant_server_logic() not found. Check server/anon/server_module_quant.R",
        type = "error"
      )
    }
  })
}
