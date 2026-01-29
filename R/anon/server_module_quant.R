anon_quant_server_logic <- function(input, output, session) {
  
  # ======================================================================
  # Quant Anon — Module-safe server with coordinate-safe risk + QGIS export
  # ======================================================================
  
  options(shiny.maxRequestSize = 1024^3)
  
  # ---- DEBUG: show full stack traces inside Shiny ----
  options(shiny.fullstacktrace = TRUE)
  
  # If you use rlang anywhere, this is very helpful
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
  
  ns <- session$ns  # IMPORTANT: used for dynamic UI ids created in renderUI()
  
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # ✅ CRITICAL FIX: avoid htmlwidgets::validate masking shiny::validate
  # Always call Shiny's validate/need explicitly.
  vld  <- shiny::validate
  need <- shiny::need
  
  get_after_col <- function(q, df_after) {
    anon_col <- paste0(q, "_anon")
    if (anon_col %in% names(df_after)) anon_col else q
  }
  
  # ---------- Module-safe DOM ids (for shinyjs hide/show) -----------------
  landing_dom   <- ns("landing")
  dashboard_dom <- ns("dashboard")
  
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
      if (length(parts) != 2) stop(sprintf("Invalid entry '%s': must be of form lower-upper:label", e))
      
      range_part <- trimws(parts[1])
      label <- trimws(parts[2])
      if (label == "") stop(sprintf("Label missing in entry '%s'", e))
      
      bounds <- strsplit(range_part, "-", fixed = TRUE)[[1]]
      if (length(bounds) == 1) {
        num <- as.numeric(bounds[1])
        if (is.na(num)) stop(sprintf("Non-numeric bound in '%s'", range_part))
        lower <- num
        upper <- num
      } else if (length(bounds) == 2) {
        lower_raw <- trimws(bounds[1])
        upper_raw <- trimws(bounds[2])
        lower <- if (lower_raw == "" || lower_raw == "-Inf") -Inf else as.numeric(lower_raw)
        upper <- if (upper_raw == "" || upper_raw == "Inf")  Inf else as.numeric(upper_raw)
        if (is.na(lower) || is.na(upper)) stop(sprintf("Non-numeric bound in '%s'", range_part))
      } else {
        stop(sprintf("Range part '%s' is malformed", range_part))
      }
      
      if (lower > upper) stop(sprintf("Range '%s' has lower > upper", label))
      list(lower = lower, upper = upper, label = label)
    })
    
    parsed <- Filter(Negate(is.null), parsed)
    if (length(parsed) == 0) stop("No valid ranges found.")
    
    df <- do.call(rbind, lapply(parsed, function(p) {
      data.frame(lower = p$lower, upper = p$upper, label = p$label, stringsAsFactors = FALSE)
    }))
    
    df <- df[order(df$lower, df$upper), ]
    if (nrow(df) > 1) {
      for (i in seq_len(nrow(df) - 1)) {
        if (df$upper[i] >= df$lower[i + 1]) {
          stop(sprintf(
            "Ranges '%s' and '%s' overlap or touch; make upper of one < lower of next.",
            df$label[i], df$label[i + 1]
          ))
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
    if (!inherits(pts_utm, "sf")) stop("pts_utm must be sf")
    if (!inherits(sf::st_geometry(pts_utm), "sfc_POINT")) stop("pts_utm must be POINT geometries")
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
  raw_data             <- reactiveVal()
  data                 <- reactiveVal()
  anon_data            <- reactiveVal()
  anonymized_cols      <- reactiveVal(character())
  initial_qids         <- reactiveVal(character())
  import_snip          <- reactiveVal(character())
  import_stata_snip    <- reactiveVal(character())
  import_py_snip       <- reactiveVal(character())
  r_steps              <- reactiveVal(character())
  stata_steps          <- reactiveVal(character())
  python_steps         <- reactiveVal(character())
  previous_data_stack  <- reactiveVal(list())
  previous_r_stack     <- reactiveVal(list())
  previous_stata_stack <- reactiveVal(list())
  previous_py_stack    <- reactiveVal(list())
  log_steps            <- reactiveVal(character())
  k_bins               <- reactiveVal(list())
  gen_assigned                <- reactiveVal(list())
  previous_gen_assigned_stack <- reactiveVal(list())
  last_method               <- reactiveVal(NULL)
  report_temp               <- reactiveVal(NULL)
  
  r_obj_name  <- reactiveVal(NULL)
  py_obj_name <- reactiveVal(NULL)
  
  geo_preview_layer   <- reactiveVal(NULL)
  geo_after_layer     <- reactiveVal(NULL)
  geo_lat_col         <- reactiveVal(NULL)
  geo_lon_col         <- reactiveVal(NULL)
  
  # ✅ Perf: cache risk metrics so they don't recompute many times
  risk_before_metrics <- reactiveVal(NULL)
  risk_after_metrics  <- reactiveVal(NULL)
  
  # ======================================================================
  # ✅ Continue button must always be observed
  # ======================================================================
  observeEvent(input$continue, {
    shinyjs::hide(id = landing_dom)
    shinyjs::show(id = dashboard_dom)
  }, ignoreInit = TRUE)
  
  observeEvent(input$modal_continue, {
    removeModal()
    shinyjs::hide(id = landing_dom)
    shinyjs::show(id = dashboard_dom)
  }, ignoreInit = TRUE)
  
  # ======================================================================
  # ✅ Guard against missing input$method/input$selected_qids
  # ======================================================================
  safe_method <- reactive(input$method %||% "Masking")
  safe_selected_qids <- reactive(input$selected_qids %||% character())
  
  # ---------- Bin-Size Advisor -------------------------------------------
  observe({
    req(data())
    nums <- names(data())[sapply(data(), is.numeric)]
    updateSelectInput(
      session, "advisor_var",
      choices  = nums,
      selected = if (length(nums) > 0) nums[1] else NULL
    )
  })
  
  advisor_x <- reactive({
    req(data(), input$advisor_var)
    na.omit(data()[[input$advisor_var]])
  })
  
  output$advisor_dist <- renderPlot({
    req(advisor_x())
    x <- advisor_x()
    hist(
      x,
      main   = paste("Histogram of", input$advisor_var),
      xlab   = input$advisor_var,
      border = "white"
    )
  })
  
  output$advisor_summary <- renderPrint({
    req(advisor_x())
    x <- advisor_x()
    stats <- c(
      Mean   = mean(x),
      Median = median(x),
      IQR    = IQR(x),
      SD     = sd(x),
      Min    = min(x),
      Max    = max(x)
    )
    print(stats)
  })
  
  bin_advice <- eventReactive(input$advisor_run, {
    req(advisor_x())
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
      Method    = c("Sturges", "Freedman–Diaconis", "Scott", "Square root"),
      Bin_Width = c(w_sturges, w_fd, w_scott, w_sqrt),
      Num_Bins  = c(k_sturges, k_fd, k_scott, k_sqrt),
      stringsAsFactors = FALSE
    )
  })
  
  output$advisor_table <- renderTable({
    df <- bin_advice()
    df$Bin_Width <- round(df$Bin_Width, 2)
    df
  }, striped = TRUE, hover = TRUE, spacing = "l")
  
  output$advisor_plot <- renderPlot({
    df <- bin_advice()
    req(nrow(df) > 0)
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
      dplyr::group_by(dplyr::across(all_of(cols))) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(r = 1 / n)
    list(
      avg = mean(tbl$r),
      max = max(tbl$r),
      pct_unique = mean(tbl$n == 1)
    )
  }
  
  observeEvent(list(data(), initial_qids()), {
    req(data(), initial_qids())
    q <- risk_qids(initial_qids())
    if (!length(q)) { risk_before_metrics(NULL); return() }
    risk_before_metrics(calc_risk_metrics(data(), q))
  }, ignoreInit = TRUE)
  
  observeEvent(list(anon_data(), initial_qids()), {
    req(anon_data(), initial_qids())
    q <- risk_qids(initial_qids())
    if (!length(q)) { risk_after_metrics(NULL); return() }
    df_a <- anon_data()
    after_cols <- sapply(q, get_after_col, df_after = df_a, USE.NAMES = FALSE)
    risk_after_metrics(calc_risk_metrics(df_a, after_cols))
  }, ignoreInit = TRUE)
  
  # ---------- Risk AFTER (coords excluded) --------------------------------
  output$risk_after <- renderUI({
    req(anon_data(), initial_qids())
    q <- risk_qids(initial_qids())
    vld(need(length(q) > 0, "Select non-coordinate QIDs."))
    
    m <- risk_after_metrics()
    req(m)
    
    tags$p(sprintf(
      "After: Average Risk: %.4f; Maximum Risk: %.4f; Percentage Unique: %.4f%%",
      m$avg, m$max, m$pct_unique * 100
    ))
  })
  
  output$gauge_after <- flexdashboard::renderGauge({
    req(anon_data(), initial_qids())
    q <- risk_qids(initial_qids())
    vld(need(length(q) > 0, "Select non-coordinate QIDs."))
    
    m <- risk_after_metrics()
    req(m)
    
    pct <- round(m$avg * 100, 2)
    flexdashboard::gauge(
      pct, min = 0, max = 100, symbol = "%",
      sectors = flexdashboard::gaugeSectors(success = c(0, 20), warning = c(20, 50), danger = c(50, 100)),
      label = sprintf("%.2f%%", pct), abbreviate = FALSE
    )
  })
  
  output$gauge_after_value <- renderText({
    req(anon_data(), initial_qids())
    q <- risk_qids(initial_qids())
    vld(need(length(q) > 0, "Select non-coordinate QIDs."))
    
    m <- risk_after_metrics()
    req(m)
    
    sprintf("%.2f%%", round(m$avg * 100, 2))
  })
  
  # ---------- Manuals ------------------------------------------------------
  manual1_rmd  <- normalizePath(file.path("..", "Anon", "docs", "descriptions.Rmd"),  mustWork = FALSE)
  manual1_html <- normalizePath(file.path("..", "Anon", "www", "descriptions.html"), mustWork = FALSE)
  manual2_rmd  <- normalizePath(file.path("..", "Anon", "docs", "descriptions1.Rmd"),  mustWork = FALSE)
  manual2_html <- normalizePath(file.path("..", "Anon", "www", "descriptions1.html"), mustWork = FALSE)
  
  observeEvent(input$view_manual, {
    res_name_html <- paste0("manuals_", gsub("[^A-Za-z0-9]", "_", landing_dom))
    res_name_tmp  <- paste0("manuals_tmp_", gsub("[^A-Za-z0-9]", "_", landing_dom))
    
    if (file.exists(manual1_html)) {
      addResourcePath(res_name_html, normalizePath(file.path("..", "Anon", "www"), mustWork = TRUE))
      src <- paste0(res_name_html, "/", basename(manual1_html))
      
    } else if (file.exists(manual1_rmd)) {
      out_dir  <- tempdir()
      out_file <- file.path(out_dir, "descriptions.html")
      
      try({
        rmarkdown::render(
          input         = manual1_rmd,
          output_file   = basename(out_file),
          output_dir    = out_dir,
          output_format = "html_document",
          quiet         = TRUE
        )
      }, silent = TRUE)
      
      addResourcePath(res_name_tmp, out_dir)
      src <- paste0(res_name_tmp, "/", basename(out_file))
      
    } else {
      showNotification("Manual not found (neither pre-knit HTML nor Rmd).", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "User Manual", size = "l", easyClose = TRUE,
      tags$iframe(src = src, style = "width:100%; height:60vh; border:none;"),
      footer = tagList(
        modalButton("Close"),
        actionButton(ns("modal_continue"), "Continue", class = "btn btn-primary")
      )
    ))
  })
  
  output$descriptions_panel <- renderUI({
    res_name_html <- paste0("manuals2_", gsub("[^A-Za-z0-9]", "_", landing_dom))
    res_name_tmp  <- paste0("manuals2_tmp_", gsub("[^A-Za-z0-9]", "_", landing_dom))
    
    if (file.exists(manual2_html)) {
      addResourcePath(res_name_html, normalizePath(file.path("..", "Anon", "www"), mustWork = TRUE))
      tags$iframe(
        src   = paste0(res_name_html, "/", basename(manual2_html)),
        style = "width:100%; height:calc(100vh - 150px); border:none;"
      )
      
    } else if (file.exists(manual2_rmd)) {
      out_dir  <- tempdir()
      out_file <- file.path(out_dir, "descriptions1.html")
      ok <- TRUE
      
      tryCatch({
        rmarkdown::render(
          manual2_rmd,
          output_file   = basename(out_file),
          output_dir    = out_dir,
          output_format = "html_document",
          quiet         = TRUE
        )
      }, error = function(e) {
        ok <<- FALSE
        showNotification(paste("Manual render error:", e$message), type = "error")
      })
      
      if (!ok) return(NULL)
      addResourcePath(res_name_tmp, out_dir)
      
      tags$iframe(
        src   = paste0(res_name_tmp, "/", basename(out_file)),
        style = "width:100%; height:calc(100vh - 150px); border:none;"
      )
      
    } else {
      tags$div(style = "padding:10px;", "Manual not available.")
    }
  })
  
  # ---------- File upload & snippet init ---------------------------------
  observeEvent(input$file, {
    req(input$file)
    
    fname <- input$file$name
    ext   <- tools::file_ext(fname)
    dname_raw <- tools::file_path_sans_ext(fname)
    
    r_name  <- make.names(dname_raw)
    py_name <- gsub("[^A-Za-z0-9_]", "_", dname_raw)
    r_obj_name(r_name)
    py_obj_name(py_name)
    
    # ✅ Faster load for CSV; Excel can be slow for huge files
    df <- switch(
      tolower(ext),
      csv  = data.table::fread(input$file$datapath, data.table = FALSE, showProgress = FALSE),
      xlsx = readxl::read_excel(input$file$datapath),
      dta  = haven::read_dta(input$file$datapath),
      { showNotification("Unsupported file type.", type = "error"); return() }
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
    geo_lat_col(NULL)
    geo_lon_col(NULL)
    
    # reset cached risks
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
    
    import_snip(c("# Load data", r_pkg_base, r_pkg_read, r_read))
    r_steps(import_snip())
    
    stata_snip <- switch(
      tolower(ext),
      csv  = sprintf("import delimited %s, clear", shQuote(fname)),
      xlsx = sprintf("import excel %s, clear",     shQuote(fname)),
      dta  = sprintf("use %s, clear",              shQuote(fname))
    )
    
    import_stata_snip(c("// Load data", stata_snip))
    stata_steps(import_stata_snip())
    
    python_snip <- switch(
      tolower(ext),
      csv  = c("# Load data", "import pandas as pd", sprintf("%s = pd.read_csv(%s)", py_name, shQuote(fname))),
      xlsx = c("# Load data", "import pandas as pd", sprintf("%s = pd.read_excel(%s)", py_name, shQuote(fname))),
      dta  = c("# Load data", "import pandas as pd", "import pyreadstat",
               sprintf("%s, meta = pyreadstat.read_dta(%s)", py_name, shQuote(fname)))
    )
    
    import_py_snip(python_snip)
    python_steps(import_py_snip())
    
    shinyAce::updateAceEditor(session, "r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
  })
  
  # ---------- Suppress & Remove Direct Identifiers ------------------------
  output$identifier_selector <- renderUI({
    req(data())
    tagList(
      checkboxGroupInput(ns("direct_ids"), "Select Direct Identifiers to Remove:", choices = names(data())),
      actionButton(ns("remove_ids"), "Suppress & Remove Identifiers", class = "btn btn-danger btn-block")
    )
  })
  
  observeEvent(input$remove_ids, {
    req(data(), input$direct_ids)
    
    previous_data_stack(c(previous_data_stack(), list(if (is.null(anon_data())) data() else anon_data())))
    previous_r_stack(c(previous_r_stack(), list(r_steps())))
    previous_stata_stack(c(previous_stata_stack(), list(stata_steps())))
    previous_py_stack(c(previous_py_stack(), list(python_steps())))
    
    df2 <- data()[, !names(data()) %in% input$direct_ids, drop = FALSE]
    data(df2)
    anon_data(NULL)
    anonymized_cols(character())
    initial_qids(character())
    
    # reset cached risks
    risk_before_metrics(NULL)
    risk_after_metrics(NULL)
    
    cols   <- input$direct_ids
    r_name <- r_obj_name()
    py_name <- py_obj_name()
    
    code_r <- paste0(
      "# Suppression\nlibrary(dplyr)\n",
      r_name, " <- ", r_name, " %>% dplyr::select(-", paste(cols, collapse = ", -"), ")"
    )
    code_s <- paste0("// Suppression\n", "drop ", paste(cols, collapse = " "))
    code_py <- paste0(
      "# Suppression\nimport pandas as pd\n",
      py_name, ".drop(columns=[", paste(shQuote(cols), collapse = ", "), "], inplace=True)"
    )
    
    r_steps(c(r_steps(), code_r))
    stata_steps(c(stata_steps(), code_s))
    python_steps(c(python_steps(), code_py))
    log_steps(c(log_steps(), paste(Sys.time(), "- Suppressed IDs")))
    
    shinyAce::updateAceEditor(session, "r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
    
    showNotification("Identifiers suppressed & removed.", type = "message")
  })
  
  # ---------- QID picker & sync -------------------------------------------
  output$bucket_ui <- renderUI({
    req(data())
    avail <- setdiff(names(data()), safe_selected_qids())
    
    sortable::bucket_list(
      header      = "Drag variables into QID bucket",
      group_name  = ns("qid_group"),
      orientation = "horizontal",
      sortable::add_rank_list("Available Variables", ns("available_vars"), labels = avail),
      sortable::add_rank_list("Selected QIDs",       ns("selected_qids"), labels = initial_qids())
    )
  })
  
  observeEvent(input$selected_qids, {
    initial_qids(input$selected_qids %||% character())
  }, ignoreNULL = FALSE)
  
  observe({
    req(initial_qids())
    if (!isTRUE(safe_method() == "Generalization")) return()
    
    choices <- initial_qids()
    if (length(choices) == 0) return()
    
    current <- isolate(input$gen_var)
    if (is.null(current) || !(current %in% choices)) {
      updateSelectInput(session, "gen_var", choices = choices, selected = choices[1])
    } else {
      updateSelectInput(session, "gen_var", choices = choices, selected = current)
    }
  })
  
  # ---------- Technique parameters UI (server side) -----------------------
  output$k_num_picker <- renderUI({
    req(data(), initial_qids())
    nq <- initial_qids()[sapply(data()[, initial_qids(), drop = FALSE], is.numeric)]
    if (length(nq) == 0) return(helpText("No numeric QIDs to bucket."))
    selectInput(ns("k_bucket_var"), "Numeric QID to bucket:", choices = nq, selected = nq[1])
  })
  
  output$extra_input <- renderUI({
    req(data())
    cols <- names(data())
    
    switch(
      safe_method(),
      
      "Masking"          = checkboxGroupInput(ns("mask_cols"), "Columns to mask:", choices = cols),
      "Suppression"      = checkboxGroupInput(ns("supp_cols"), "Columns to suppress:", choices = cols),
      "Pseudonymization" = checkboxGroupInput(ns("ps_cols"),   "Columns to pseudonymize:", choices = cols),
      "Tokenization"     = checkboxGroupInput(ns("tok_cols"),  "Columns to tokenize:", choices = cols),
      
      "Bucketing" = tagList(
        selectInput(ns("bucket_col"), "Column to bucket:", choices = cols),
        numericInput(ns("bin_interval"), "Bin size:", value = 4, min = 1)
      ),
      
      "K-Anonymity" = tagList(
        uiOutput(ns("k_num_picker")),
        numericInput(ns("k_bin_size"), "Bin size:", value = 5, min = 1),
        numericInput(ns("k_value"), "k threshold:", value = 2, min = 2)
      ),
      
      "L-Diversity" = tagList(
        selectInput(ns("sensitive_attr"), "Sensitive attribute:", choices = cols),
        numericInput(ns("l_value"), "l threshold:", value = 2, min = 2)
      ),
      
      "T-Closeness" = tagList(
        selectInput(ns("sensitive_attr_tc"), "Sensitive attribute:", choices = cols),
        numericInput(ns("t_threshold"), "t threshold:", value = 0.1, min = 0, step = 0.01)
      ),
      
      "Generalization" = tagList(
        selectInput(ns("gen_var"), "Variable to generalize:", choices = initial_qids()),
        uiOutput(ns("gen_groups_ui"))
      ),
      
      "Anonymize Coordinates" = tagList(
        selectInput(ns("geo_lat_col"), "Latitude column:", choices = cols),
        selectInput(ns("geo_lon_col"), "Longitude column:", choices = cols),
        radioButtons(
          ns("geo_mode"), "Mode:",
          choices = c("Truncate decimals" = "truncate", "Aggregate to polygons" = "aggregate"),
          selected = "aggregate", inline = TRUE
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'truncate'", ns("geo_mode")),
          numericInput(ns("geo_decimals"), "Keep this many decimals:", value = 3, min = 0, step = 1),
          helpText("Truncation (not rounding): reduces precision to the specified number of decimals.")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'aggregate'", ns("geo_mode")),
          numericInput(ns("geo_grid_m"), "Grid cell size (meters):", value = 500, min = 10, step = 10),
          helpText("Points are snapped to the grid cell centroid; polygons are drawn on the map.")
        ),
        helpText("Preview: original points + faint preview (for truncate). Apply: dataset updated; polygons drawn.")
      ),
      
      NULL
    )
  })
  
  # ---------- Apply anonymization (incl. GEO) --------------------------------
  observeEvent(input$apply, {
    req(data(), input$method)
    if (input$method != "Anonymize Coordinates") req(initial_qids())
    
    last_method(input$method)
    
    previous_data_stack(c(previous_data_stack(), list(if (is.null(anon_data())) data() else anon_data())))
    previous_r_stack(c(previous_r_stack(), list(r_steps())))
    previous_stata_stack(c(previous_stata_stack(), list(stata_steps())))
    previous_py_stack(c(previous_py_stack(), list(python_steps())))
    
    df        <- if (is.null(anon_data())) data() else anon_data()
    code_r    <- code_s <- code_py <- NULL
    step_cols <- NULL
    r_name    <- r_obj_name(); py_name <- py_obj_name()
    
    if (input$method == "Masking") {
      step_cols <- input$mask_cols
      if (length(step_cols) == 0) { showNotification("Select columns to mask.", type="error"); return() }
      
      df[step_cols] <- lapply(df[step_cols], function(x)
        ifelse(is.na(x), NA_character_, strrep("*", 10))
      )
      
      code_r <- paste0(
        "# Masking (strings & numerics)\nlibrary(dplyr)\n",
        r_name, " <- ", r_name, " %>% dplyr::mutate(across(all_of(c(",
        paste(shQuote(step_cols), collapse = ", "),
        ")), ~ ifelse(is.na(.), NA_character_, strrep(\"*\", 10))))"
      )
      
      code_s <- paste0(
        "// Masking (strings & numerics)\n",
        "foreach v of varlist ", paste(step_cols, collapse = " "), " {\n",
        "  capture confirm numeric variable `v'\n",
        "  if !_rc { tostring `v', replace force }\n",
        "  replace `v' = \"**********\" if !missing(`v')\n",
        "}"
      )
      
      code_py <- paste0(
        "# Masking (strings & numerics)\n",
        "import pandas as pd\n",
        "for col in [", paste(shQuote(step_cols), collapse = ", "), "]:\n",
        "    ", py_name, "[col] = ", py_name, "[col].apply(lambda x: None if pd.isna(x) else '*'*10)"
      )
      
    } else if (input$method == "Suppression") {
      step_cols <- input$supp_cols
      if (length(step_cols) == 0) { showNotification("Select columns to suppress.", type="error"); return() }
      
      df <- df[, !names(df) %in% step_cols, drop = FALSE]
      
      code_r <- paste0(
        "# Suppression\nlibrary(dplyr)\n",
        r_name, " <- ", r_name, " %>% dplyr::select(-", paste(step_cols, collapse = ", -"), ")"
      )
      code_s  <- paste0("// Suppression\n", "drop ", paste(step_cols, collapse = " "))
      code_py <- paste0(
        "# Suppression\nimport pandas as pd\n",
        py_name, ".drop(columns=[", paste(shQuote(step_cols), collapse = ", "), "], inplace=True)"
      )
      
    } else if (input$method == "Bucketing") {
      step_cols <- input$bucket_col; b <- input$bin_interval
      if (is.null(step_cols) || is.null(b)) { showNotification("Choose a column and bin size.", type="error"); return() }
      
      vals <- df[[step_cols]]
      if (!is.numeric(vals)) { showNotification("Bucketing requires numeric column.", type = "error"); return() }
      
      minv <- suppressWarnings(min(vals, na.rm = TRUE)); maxv <- suppressWarnings(max(vals, na.rm = TRUE))
      if (!is.finite(minv) || !is.finite(maxv)) { showNotification("No finite values to bucket.", type="error"); return() }
      
      start <- floor(minv / b) * b; end <- ceiling((maxv + 1) / b) * b
      brks <- seq(start, end, by = b)
      lbls <- paste0(head(brks, -1), "-", brks[-1] - 1)
      
      df[[step_cols]] <- cut(vals, breaks = brks, labels = lbls, include.lowest = TRUE, right = FALSE)
      
      code_r <- paste0(
        "# Bucketing\nlibrary(dplyr)\n",
        r_name, " <- ", r_name, " %>% dplyr::mutate(", step_cols, " = cut(", step_cols, ",\n",
        "  breaks = seq(", start, ", ", end, ", by=", b, "),\n",
        "  labels = c(", paste(shQuote(lbls), collapse = ", "), "), right=FALSE, include_lowest=TRUE))"
      )
      
      code_s <- paste0(
        "// Bucketing\n",
        "gen long __b = floor(", step_cols, "/", b, ")*", b, "\n",
        "tostring __b, gen(__bstr)\n",
        "gen long __e = __b + ", b, " - 1\n",
        "tostring __e, gen(__estr)\n",
        "replace ", step_cols, " = __bstr + \"-\" + __estr if !missing(", step_cols, ")\n",
        "drop __b __e __bstr __estr"
      )
      
      code_py <- paste0(
        "# Bucketing\nimport pandas as pd\nimport numpy as np\n",
        "b = ", b, "\n",
        "minv = int(np.floor(", py_name, "['", step_cols, "'].min()/b)*b)\n",
        "maxv = int(np.ceil((", py_name, "['", step_cols, "'].max()+1)/b)*b)\n",
        "bins = list(range(minv, maxv+1, b))\n",
        "labels = [f\"{bins[i]}-{bins[i+1]-1}\" for i in range(len(bins)-1)]\n",
        py_name, "['", step_cols, "'] = pd.cut(", py_name, "['", step_cols, "'], bins=bins, labels=labels, right=False, include_lowest=True)"
      )
      
    } else if (input$method == "Pseudonymization") {
      step_cols <- input$ps_cols
      if (length(step_cols) == 0) { showNotification("Select columns to pseudonymize.", type="error"); return() }
      
      salt_vec <- uuid::UUIDgenerate(n = nrow(df))
      hash_vec <- function(values, salts) {
        mapply(function(v, s) digest::digest(paste0(as.character(v), s), algo = "sha256"),
               values, salts, USE.NAMES = FALSE)
      }
      
      for (col in step_cols) {
        x <- df[[col]]
        df[[col]] <- ifelse(is.na(x), NA_character_, hash_vec(x, salt_vec))
      }
      
      code_r <- "# Pseudonymization applied in-app"
      code_s <- "// Pseudonymization\n// no direct Stata analogue"
      code_py <- "# Pseudonymization applied in-app"
      
    } else if (input$method == "Tokenization") {
      step_cols <- input$tok_cols
      if (length(step_cols) == 0) { showNotification("Select columns to tokenize.", type="error"); return() }
      
      df <- dplyr::mutate(
        df,
        dplyr::across(
          dplyr::all_of(step_cols),
          ~ ifelse(is.na(.x), NA_character_, stringi::stri_rand_strings(length(.x), 10, pattern = "[A-Za-z0-9]"))
        )
      )
      
      code_r <- "# Tokenization applied in-app"
      code_s <- "// Tokenization\n// no direct Stata analogue"
      code_py <- "# Tokenization applied in-app"
      
    } else if (input$method == "K-Anonymity") {
      qids_all <- initial_qids()
      k_val    <- as.integer(input$k_value)
      var_now  <- input$k_bucket_var
      bin_size <- as.integer(input$k_bin_size)
      
      vld(
        need(length(qids_all) > 0, "Select QIDs in step 2 first."),
        need(!is.na(k_val) && k_val >= 2, "k must be ≥ 2")
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
      vld(need(length(k_qids_use) > 0, "Bucket at least one numeric QID before applying k-anonymity."))
      
      grouped_sizes <- df %>% dplyr::group_by(dplyr::across(all_of(k_qids_use))) %>% dplyr::tally(name=".__size__")
      df_tagged <- df %>% dplyr::left_join(grouped_sizes, by = k_qids_use) %>%
        dplyr::mutate(.__ok__ = .__size__ >= k_val)
      
      df_ok  <- df_tagged %>% dplyr::filter(.__ok__)  %>% dplyr::select(-.__ok__, -.__size__)
      df_bad <- df_tagged %>% dplyr::filter(!.__ok__) %>% dplyr::select(-.__ok__, -.__size__)
      df <- dplyr::bind_rows(df_ok, df_bad)
      
      anon_data(df)
      anonymized_cols(union(anonymized_cols(), k_qids_use))
      
      code_r  <- "# K-Anonymity applied in-app"
      code_s  <- "* no Stata analogue"
      code_py <- "# no Python analogue"
      
    } else if (input$method == "T-Closeness") {
      step_cols <- risk_qids(initial_qids())
      vld(need(length(step_cols) > 0, "Select non-coordinate QIDs."))
      
      # apply_t_closeness() must be sourced from anonymization_functions.R
      df <- apply_t_closeness(df, qids = step_cols, sensitive = input$sensitive_attr_tc, t = input$t_threshold)
      
      code_r  <- "# T-Closeness applied in-app"
      code_s  <- "// T-Closeness\n// no direct Stata analogue"
      code_py <- "# T-Closeness applied in-app"
      
    } else if (input$method == "Anonymize Coordinates") {
      latc <- input$geo_lat_col; lonc <- input$geo_lon_col
      
      vld(
        need(!is.null(latc) && !is.null(lonc), "Pick latitude & longitude columns first."),
        need(latc %in% names(df) && lonc %in% names(df), "Invalid coordinate columns."),
        need(is.numeric(df[[latc]]), "Latitude must be numeric."),
        need(is.numeric(df[[lonc]]), "Longitude must be numeric.")
      )
      
      pts_df <- df[, c(lonc, latc), drop = FALSE]
      names(pts_df) <- c("lon","lat")
      comp_mask_full <- stats::complete.cases(pts_df)
      pts_df <- pts_df[comp_mask_full, , drop = FALSE]
      
      if (!nrow(pts_df)) {
        showNotification("No valid coordinate rows to anonymize.", type="error"); return()
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
          showNotification("No grid cells created (empty). Check decimals or data extent.", type="warning")
        }
        
        anonymized_cols(union(anonymized_cols(), c(latc, lonc, "cell_wkt")))
        showNotification("Coordinate anonymization (truncate) applied.", type = "message")
        
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
          showNotification("No grid cells created (empty). Check grid size or data extent.", type="warning")
        }
        
        anonymized_cols(union(anonymized_cols(), c(latc, lonc, "cell_wkt")))
        showNotification("Coordinate anonymization (aggregate to polygons) applied.", type = "message")
        
      } else {
        showNotification("Unsupported mode selected.", type = "error"); return()
      }
      
      df$cell_wkt <- cell_wkt_full
      df <- df[, setdiff(names(df), c(lonc, latc)), drop = FALSE]
      
      anon_data(df)
      log_steps(c(log_steps(), paste(Sys.time(), "- Applied Anonymize Coordinates (mode:", mode, ")")))
      geo_preview_layer(NULL)
      
      code_r  <- "# Anonymize Coordinates applied in-app (cell_wkt added; lat/lon dropped)"
      code_s  <- "* no direct Stata analogue"
      code_py <- "# Anonymize Coordinates applied (cell_wkt added; lat/lon dropped)"
    }
    
    if (!is.null(step_cols) && input$method %in% c(
      "Masking","Suppression","Bucketing","Pseudonymization","Tokenization","T-Closeness"
    )) {
      anon_data(df)
      anonymized_cols(union(anonymized_cols(), step_cols))
    }
    
    if (!is.null(code_r))  r_steps(   c(r_steps(),    code_r))
    if (!is.null(code_s))  stata_steps(c(stata_steps(), code_s))
    if (!is.null(code_py)) python_steps(c(python_steps(), code_py))
    
    if (input$method != "Anonymize Coordinates") {
      log_steps(c(log_steps(), paste(Sys.time(), "- Applied", input$method)))
    }
    
    shinyAce::updateAceEditor(session, "r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
  })
  
  # ---------- K-Report --------------------------------------------------------
  output$k_report <- renderUI({
    req(last_method())
    if (last_method() == "K-Anonymity") {
      bins <- k_bins(); k_val <- input$k_value
      tags$div(
        tags$h4("K-Anonymity Report"),
        tags$p(paste0("Threshold k = ", k_val)),
        if (length(bins) > 0)
          tags$p(paste0("Bucket sizes: ",
                        paste(paste0(names(bins)," = ",bins), collapse=", ")))
      )
    }
  })
  
  # ---------- Generalization UI & Logic --------------------------------------
  output$gen_groups_ui <- renderUI({
    req(input$gen_var)
    working <- if (!is.null(anon_data()) && input$gen_var %in% names(anon_data())) anon_data() else data()
    is_num <- is.numeric(working[[input$gen_var]])
    default_mode <- if (is_num) "numeric" else "categorical"
    
    tagList(
      radioButtons(
        ns("gen_mode"), "Generalization mode:",
        choices = c("Categorical (drag & drop)" = "categorical",
                    "Custom numeric ranges"     = if (is_num) "numeric" else NULL),
        selected = default_mode, inline = TRUE
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'categorical'", ns("gen_mode")),
        {
          cats <- unique(as.character(working[[input$gen_var]]))
          tagList(
            sortable::bucket_list(
              header     = "Drag values to collapse",
              group_name = ns("gen_group"),
              orientation= "horizontal",
              sortable::add_rank_list("Available categories", ns("gen_available"), labels = cats),
              sortable::add_rank_list("Selected categories",  ns("gen_selected"),  labels = NULL)
            ),
            textInput(ns("gen_new_label"), "New label for selected categories:"),
            actionButton(ns("apply_generalization"), "Apply Generalization", class = "btn btn-primary btn-block")
          )
        }
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'numeric'", ns("gen_mode")),
        tagList(
          helpText("Define numeric buckets: e.g., '0-10:Group 1;11-40:Group 2;41-100:Group 3'"),
          textInput(ns("gen_ranges_text"), "Ranges (lower-upper:label; semicolon separated):",
                    value = "0-10:Group 1;11-40:Group 2;41-100:Group 3"),
          helpText("Use '-Inf' or 'Inf' for open bounds. Ranges must not overlap or touch."),
          actionButton(ns("apply_generalization"), "Apply Generalization", class = "btn btn-primary btn-block")
        )
      )
    )
  })
  
  observeEvent(input$apply_generalization, {
    req(data(), input$gen_var, input$gen_mode)
    last_method("Generalization")
    
    previous_data_stack(c(previous_data_stack(), list(if (is.null(anon_data())) data() else anon_data())))
    previous_r_stack(c(previous_r_stack(), list(r_steps())))
    previous_stata_stack(c(previous_stata_stack(), list(stata_steps())))
    previous_py_stack(c(previous_py_stack(), list(python_steps())))
    previous_gen_assigned_stack(c(previous_gen_assigned_stack(), list(gen_assigned())))
    
    df  <- if (is.null(anon_data())) data() else anon_data()
    var <- input$gen_var
    code_r <- code_s <- code_py <- NULL
    r_name <- r_obj_name(); py_name <- py_obj_name()
    
    if (input$gen_mode == "categorical") {
      sel <- input$gen_selected; lab <- input$gen_new_label
      if (is.null(sel) || length(sel) == 0) { showNotification("No categories selected to generalize.", type = "error"); return() }
      if (is.null(lab) || trimws(lab) == "") { showNotification("Provide a new label.", type = "error"); return() }
      
      ga <- gen_assigned(); ga[[var]] <- unique(c(ga[[var]] %||% character(), sel)); gen_assigned(ga)
      df[[var]] <- as.character(df[[var]]); df[[var]][ df[[var]] %in% sel ] <- lab
      
      anon_data(df)
      anonymized_cols(union(anonymized_cols(), var))
      
      code_r <- "# Generalization (categorical) applied in-app"
      code_s <- "// Generalization\n// no direct Stata analogue"
      code_py <- "# Generalization applied in-app"
      
    } else if (input$gen_mode == "numeric") {
      req(input$gen_ranges_text); range_txt <- input$gen_ranges_text
      ranges_df <- tryCatch({ parse_ranges_text(range_txt) }, error = function(e) {
        showNotification(paste("Range parse error:", e$message), type = "error"); NULL
      })
      if (is.null(ranges_df)) return()
      if (!is.numeric(df[[var]])) { showNotification(sprintf("Variable '%s' must be numeric.", var), type = "error"); return() }
      
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
      
      code_r <- "# Generalization (numeric ranges) applied in-app"
      code_s <- "// Generalization\n// no direct Stata analogue"
      code_py <- "# Generalization applied in-app"
    }
    
    if (!is.null(code_r)) r_steps(c(r_steps(), code_r))
    if (!is.null(code_s)) stata_steps(c(stata_steps(), code_s))
    if (!is.null(code_py)) python_steps(c(python_steps(), code_py))
    log_steps(c(log_steps(), paste(Sys.time(), "- Applied generalization (mode:", input$gen_mode, ") to", var)))
    
    shinyAce::updateAceEditor(session, "r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
    shinyAce::updateAceEditor(session, "python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
    
    showNotification("Generalization applied.", type = "message")
  })
  
  # ---------- Copy buttons ----------------------------------------------------
  observeEvent(input$copy_r, {
    shinyjs::runjs(sprintf("copyAce('%s','%s')", ns("r_code_ace"), ns("copy_r")))
  })
  observeEvent(input$copy_stata, {
    shinyjs::runjs(sprintf("copyAce('%s','%s')", ns("stata_code_ace"), ns("copy_stata")))
  })
  observeEvent(input$copy_py, {
    shinyjs::runjs(sprintf("copyAce('%s','%s')", ns("python_code_ace"), ns("copy_py")))
  })
  
  # ---------- Undo & Reset ----------------------------------------------------
  observeEvent(input$undo, {
    prev <- previous_data_stack(); pr <- previous_r_stack()
    ps   <- previous_stata_stack(); pp <- previous_py_stack()
    if (length(prev) > 0) {
      idx <- length(prev)
      anon_data(prev[[idx]])
      r_steps(pr[[idx]]); stata_steps(ps[[idx]]); python_steps(pp[[idx]])
      previous_data_stack(prev[-idx]); previous_r_stack(pr[-idx])
      previous_stata_stack(ps[-idx]); previous_py_stack(pp[-idx])
      log_steps(c(log_steps(), paste(Sys.time(), "- Undid last step")))
      
      pgs <- previous_gen_assigned_stack()
      if (length(pgs) > 0) {
        gen_assigned(pgs[[length(pgs)]])
        previous_gen_assigned_stack(pgs[-length(pgs)])
      }
      
      geo_preview_layer(NULL); geo_after_layer(NULL)
      
      showNotification("Undo successful.", type = "message")
      shinyAce::updateAceEditor(session,"r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
      shinyAce::updateAceEditor(session,"stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
      shinyAce::updateAceEditor(session,"python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
    }
  })
  
  observeEvent(input$reset, {
    req(raw_data())
    data(raw_data()); anon_data(NULL)
    anonymized_cols(character()); initial_qids(character())
    r_steps(import_snip()); stata_steps(import_stata_snip()); python_steps(import_py_snip())
    previous_data_stack(list()); previous_r_stack(list())
    previous_stata_stack(list()); previous_py_stack(list())
    log_steps(c(log_steps(), paste(Sys.time(),"- Reset complete")))
    k_bins(list()); gen_assigned(list()); previous_gen_assigned_stack(list())
    
    geo_preview_layer(NULL); geo_after_layer(NULL)
    geo_lat_col(NULL); geo_lon_col(NULL)
    
    # reset cached risks
    risk_before_metrics(NULL)
    risk_after_metrics(NULL)
    
    showNotification("Reset complete.", type = "warning")
    shinyAce::updateAceEditor(session,"r_code_ace",      value = paste(r_steps(),      collapse = "\n\n"))
    shinyAce::updateAceEditor(session,"stata_code_ace",  value = paste(stata_steps(),  collapse = "\n\n"))
    shinyAce::updateAceEditor(session,"python_code_ace", value = paste(python_steps(), collapse = "\n\n"))
  })
  
  # ---------- Preview merged table -------------------------------------------
  output$preview_merged <- renderTable({
    req(data())
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
  
  # ---------- Map (safe basemap + module-safe leafletProxy) -------------------
  output$geo_map <- leaflet::renderLeaflet({
    req(input$method == "Anonymize Coordinates", data())
    
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
            group = "Before (points)"
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
        overlayGroups = c("Before (points)", "After (areas)"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    req(input$method == "Anonymize Coordinates")
    pol <- geo_after_layer()
    
    prx <- leaflet::leafletProxy(session$ns("geo_map"), session = session)
    
    if (is.null(pol) || !inherits(pol, "sf") || nrow(pol) == 0) {
      prx |> leaflet::clearGroup("After (areas)")
      return()
    }
    
    pol <- sf::st_make_valid(pol)
    pol <- sf::st_zm(pol, drop = TRUE, what = "ZM")
    pol <- pol[!sf::st_is_empty(pol), , drop = FALSE]
    if (!nrow(pol)) { prx |> leaflet::clearGroup("After (areas)"); return() }
    
    prx |>
      leaflet::clearGroup("After (areas)") |>
      leaflet::addPolygons(
        data = pol,
        weight = 1, color = "#555555", fillOpacity = 0.5, fillColor = "#2A9D8F",
        group = "After (areas)",
        popup = ~paste0("n = ", n)
      )
  })
  
  # ---------- Risk BEFORE (coords excluded) ----------------------------------
  output$risk_before <- renderUI({
    req(data(), initial_qids())
    q <- risk_qids(initial_qids())
    vld(need(length(q) > 0, "Select non-coordinate QIDs."))
    
    m <- risk_before_metrics()
    req(m)
    
    tags$p(sprintf(
      "Before: Average Risk: %.4f; Maximum Risk: %.4f; Percentage Unique: %.4f%%",
      m$avg, m$max, m$pct_unique * 100
    ))
  })
  
  output$gauge_before <- flexdashboard::renderGauge({
    req(data(), initial_qids())
    q <- risk_qids(initial_qids())
    vld(need(length(q) > 0, "Select non-coordinate QIDs."))
    
    m <- risk_before_metrics()
    req(m)
    
    pct <- round(m$avg * 100, 2)
    flexdashboard::gauge(
      pct, min=0, max=100, symbol="%",
      sectors = flexdashboard::gaugeSectors(success=c(0,20),warning=c(20,50),danger=c(50,100)),
      label=sprintf("%.2f%%", pct), abbreviate=FALSE
    )
  })
  
  # ---------- Row counts / log -----------------------------------------------
  output$n_obs_text <- renderText({
    if (is.null(data())) return("")
    after_txt <- if (!is.null(anon_data())) paste0(" | After: ", nrow(anon_data())) else ""
    paste0("Rows: ", nrow(data()), after_txt)
  })
  
  output$step_log <- renderText({ paste(log_steps(), collapse = "\n") })
  
  # ---------- Downloads -------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() paste0("anonymized_", Sys.Date(), ".csv"),
    content  = function(file) {
      dat <- anon_data()
      if (is.null(dat)) stop("No anonymized data available to download.")
      utils::write.csv(drop_geo_cols(dat), file, row.names = FALSE)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() paste0("anonymized_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      dat <- anon_data()
      if (is.null(dat)) stop("No anonymized data available to download.")
      openxlsx::write.xlsx(drop_geo_cols(dat), file)
    }
  )
  
  output$download_dta <- downloadHandler(
    filename = function() paste0("anonymized_", Sys.Date(), ".dta"),
    content  = function(file) {
      dat <- anon_data()
      if (is.null(dat)) stop("No anonymized data available to download.")
      haven::write_dta(drop_geo_cols(dat), file)
    }
  )
  
  # ---------- Report preview/download ----------------------------------------
  render_html_report <- function() {
    req(data(), anon_data(), initial_qids())
    
    tmpl <- normalizePath(file.path("..","Anon","docs","report_template.Rmd"), mustWork = FALSE)
    if (!file.exists(tmpl)) {
      showNotification("Missing report template: ../Anon/docs/report_template.Rmd", type = "error")
      return(NULL)
    }
    
    q_main <- risk_qids(initial_qids())
    vld(need(length(q_main) > 0, "Select non-coordinate QIDs."))
    
    before_tbl <- data() %>%
      dplyr::group_by(dplyr::across(all_of(q_main))) %>%
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
      dplyr::group_by(dplyr::across(all_of(after_cols_main))) %>%
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
        dplyr::group_by(dplyr::across(all_of(sub))) %>%
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
        dplyr::group_by(dplyr::across(all_of(after_sub))) %>%
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
  
  observeEvent(input$view_report, {
    req(data(), anon_data(), initial_qids())
    
    withProgress(message = "Building report...", value = 0.1, {
      path <- render_html_report()
      if (is.null(path)) return(NULL)
      res_name <- paste0("reports_tmp_", gsub("[^A-Za-z0-9]", "_", landing_dom))
      addResourcePath(res_name, dirname(path))
      report_temp(path)
    })
    
    if (is.null(report_temp())) return(NULL)
    
    res_name <- paste0("reports_tmp_", gsub("[^A-Za-z0-9]", "_", landing_dom))
    showModal(modalDialog(
      title = "Risk Report Preview",
      size  = "l", easyClose = TRUE,
      tags$iframe(
        src = paste0(res_name, "/", basename(report_temp())),
        style = "width:100%; height:600px; border:none;"
      ),
      footer = modalButton("Close")
    ))
  })
  
  output$download_report <- downloadHandler(
    filename = function() paste0("risk_report_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(report_temp())
      if (requireNamespace("pagedown", quietly = TRUE)) {
        pagedown::chrome_print(input = report_temp(), output = file)
      } else {
        file.copy(report_temp(), file, overwrite = TRUE)
      }
    }
  )
}
