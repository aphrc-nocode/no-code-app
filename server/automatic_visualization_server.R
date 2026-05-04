automatic_visualization_server <- function(
    input,
    output,
    session,
    plots_auto_rv,
    rv_current,
    get_rv_labels = NULL,
    get_rv_choices = NULL
) {
  
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  
  rv_label <- function(key) {
    if (is.function(get_rv_labels)) {
      val <- get_rv_labels(key)
      if (!is.null(val) && length(val) > 0 && nzchar(as.character(val[1]))) {
        return(as.character(val[1]))
      }
    }
    key
  }
  
  rv_choices <- function(key) {
    defaults <- list(
      bivariate_palette = c(
        Dark2 = "Dark2", Set1 = "Set1", Set2 = "Set2", Set3 = "Set3",
        Paired = "Paired", Pastel1 = "Pastel1", Pastel2 = "Pastel2",
        Accent = "Accent"
      ),
      corrplot_palette = c(
        RdYlBu = "RdYlBu", RdBu = "RdBu", BrBG = "BrBG",
        PiYG = "PiYG", PRGn = "PRGn", Spectral = "Spectral"
      )
    )
    
    if (is.function(get_rv_choices)) {
      vals <- get_rv_choices(key)
      if (!is.null(vals) && length(vals) > 0) return(vals)
    }
    
    defaults[[key]] %||% character(0)
  }
  
  is_blank <- function(x) {
    is.null(x) || length(x) == 0 || identical(x, "") ||
      (length(x) == 1 && isTRUE(is.na(x)))
  }
  
  safe_scalar <- function(x, default = NULL) {
    if (is.null(x) || length(x) == 0) return(default)
    x1 <- x[1]
    if (length(x1) == 0 || isTRUE(is.na(x1)) || identical(x1, "")) {
      return(default)
    }
    x1
  }
  
  is_numeric_col <- function(x) is.numeric(x) || is.integer(x)
  
  is_categorical_col <- function(x) {
    is.factor(x) || is.character(x) || is.logical(x)
  }
  
  coerce_categorical <- function(x) {
    if (is.factor(x)) return(x)
    if (is.logical(x)) return(factor(x, levels = c(TRUE, FALSE)))
    factor(as.character(x))
  }
  
  has_variation <- function(x) {
    length(unique(stats::na.omit(x))) > 1
  }
  
  get_df <- shiny::reactive({
    shiny::req(rv_current$working_df)
    as.data.frame(rv_current$working_df, stringsAsFactors = FALSE)
  })
  
  safe_palette_values <- function(palette_name, n) {
    palette_name <- safe_scalar(palette_name, "Dark2")
    n <- max(3L, min(12L, as.integer(n)))
    
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      info <- RColorBrewer::brewer.pal.info
      if (palette_name %in% rownames(info)) {
        max_n <- info[palette_name, "maxcolors"]
        return(RColorBrewer::brewer.pal(min(max_n, max(3L, n)), palette_name))
      }
    }
    
    grDevices::hcl.colors(n, "Set 2")
  }
  
  safe_corr_gradient <- function(palette_name, n = 200) {
    palette_name <- safe_scalar(palette_name, "RdYlBu")
    n <- max(3L, as.integer(n))
    
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      info <- RColorBrewer::brewer.pal.info
      if (palette_name %in% rownames(info)) {
        max_n <- info[palette_name, "maxcolors"]
        base_pal <- RColorBrewer::brewer.pal(min(max_n, 11L), palette_name)
        return(grDevices::colorRampPalette(base_pal)(n))
      }
    }
    
    grDevices::colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(n)
  }
  
  sanitize_corr_features <- function(df, features) {
    features <- intersect(features %||% character(0), names(df))
    features <- features[vapply(df[, features, drop = FALSE], is_numeric_col, logical(1))]
    features[vapply(df[, features, drop = FALSE], has_variation, logical(1))]
  }
  
  sanitize_bivariate_features <- function(df, outcome, features) {
    features <- intersect(features %||% character(0), names(df))
    features <- setdiff(features, outcome)
    features[vapply(df[, features, drop = FALSE], has_variation, logical(1))]
  }
  
  build_corrplot_gg <- function(df, features, palette_name = "RdYlBu") {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    
    features <- sanitize_corr_features(df, features)
    if (length(features) < 2) return(NULL)
    
    cor_mat <- suppressWarnings(
      stats::cor(df[, features, drop = FALSE], use = "pairwise.complete.obs")
    )
    
    if (is.null(cor_mat) || all(is.na(cor_mat))) return(NULL)
    
    corr_df <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
    names(corr_df) <- c("Var1", "Var2", "Correlation")
    
    ggplot2::ggplot(corr_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.4) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Correlation)), size = 3.5) +
      ggplot2::scale_fill_gradientn(
        colours = safe_corr_gradient(palette_name, 200),
        limits = c(-1, 1),
        name = rv_label("correlation")
      ) +
      ggplot2::coord_equal() +
      ggplot2::labs(title = rv_label("correlation_analysis"), x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid = ggplot2::element_blank()
      )
  }
  
  build_bivariate_plot <- function(df, outcome, features, palette_name = "Dark2", plot_title = NULL) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    if (is_blank(outcome) || !outcome %in% names(df)) return(NULL)
    
    features <- sanitize_bivariate_features(df, outcome, features)
    if (length(features) == 0) return(NULL)
    
    outcome_vec <- df[[outcome]]
    numeric_features <- features[vapply(df[, features, drop = FALSE], is_numeric_col, logical(1))]
    categorical_features <- features[vapply(df[, features, drop = FALSE], is_categorical_col, logical(1))]
    
    title_val <- safe_scalar(plot_title, "")
    if (!nzchar(title_val)) title_val <- paste("Bivariate View of", outcome)
    
    if (is_numeric_col(outcome_vec) && length(numeric_features) > 0) {
      long_df <- do.call(rbind, lapply(numeric_features, function(feat) {
        data.frame(feature_name = feat, x = df[[feat]], y = outcome_vec)
      }))
      
      return(
        ggplot2::ggplot(long_df, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_point(alpha = 0.55, size = 1.8, color = "#111827", na.rm = TRUE) +
          ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#2563eb", linewidth = 0.9, na.rm = TRUE) +
          ggplot2::facet_wrap(~feature_name, scales = "free_x") +
          ggplot2::labs(title = title_val, x = rv_label("feature_value"), y = outcome) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
      )
    }
    
    if (is_numeric_col(outcome_vec) && length(categorical_features) > 0) {
      long_df <- do.call(rbind, lapply(categorical_features, function(feat) {
        data.frame(feature_name = feat, group = coerce_categorical(df[[feat]]), y = outcome_vec)
      }))
      
      return(
        ggplot2::ggplot(long_df, ggplot2::aes(x = group, y = y, fill = group)) +
          ggplot2::geom_boxplot(na.rm = TRUE, outlier.alpha = 0.35) +
          ggplot2::facet_wrap(~feature_name, scales = "free_x") +
          ggplot2::scale_fill_manual(values = safe_palette_values(palette_name, length(unique(long_df$group)))) +
          ggplot2::labs(title = title_val, x = rv_label("category"), y = outcome, fill = rv_label("category")) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
          )
      )
    }
    
    if (is_categorical_col(outcome_vec) && length(numeric_features) > 0) {
      outcome_fac <- coerce_categorical(outcome_vec)
      
      long_df <- do.call(rbind, lapply(numeric_features, function(feat) {
        data.frame(feature_name = feat, outcome = outcome_fac, value = df[[feat]])
      }))
      
      return(
        ggplot2::ggplot(long_df, ggplot2::aes(x = outcome, y = value, fill = outcome)) +
          ggplot2::geom_boxplot(na.rm = TRUE, outlier.alpha = 0.35) +
          ggplot2::facet_wrap(~feature_name, scales = "free_y") +
          ggplot2::scale_fill_manual(values = safe_palette_values(palette_name, length(unique(long_df$outcome)))) +
          ggplot2::labs(title = title_val, x = outcome, y = rv_label("feature_value"), fill = outcome) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
          )
      )
    }
    
    if (is_categorical_col(outcome_vec) && length(categorical_features) > 0) {
      outcome_fac <- coerce_categorical(outcome_vec)
      
      pieces <- lapply(categorical_features, function(feat) {
        tmp <- data.frame(
          feature_name = feat,
          feature_value = coerce_categorical(df[[feat]]),
          outcome = outcome_fac
        )
        
        out <- as.data.frame(with(tmp, table(feature_value, outcome)), stringsAsFactors = FALSE)
        if (ncol(out) != 3) return(NULL)
        
        names(out) <- c("feature_value", "outcome", "n")
        out$feature_name <- feat
        out
      })
      
      pieces <- Filter(Negate(is.null), pieces)
      if (length(pieces) == 0) return(NULL)
      
      count_df <- do.call(rbind, pieces)
      
      return(
        ggplot2::ggplot(count_df, ggplot2::aes(x = feature_value, y = n, fill = outcome)) +
          ggplot2::geom_col(position = "dodge", na.rm = TRUE) +
          ggplot2::facet_wrap(~feature_name, scales = "free_x") +
          ggplot2::scale_fill_manual(values = safe_palette_values(palette_name, length(unique(count_df$outcome)))) +
          ggplot2::labs(title = title_val, x = rv_label("feature_value"), y = rv_label("count"), fill = outcome) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
          )
      )
    }
    
    NULL
  }
  
  build_missing_profile_plot <- function(df) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    
    summary_df <- data.frame(
      metric = c(
        rv_label("discrete_columns"),
        rv_label("continuous_columns"),
        rv_label("all_missing_columns"),
        rv_label("complete_rows"),
        rv_label("missing_observations")
      ),
      value = c(
        sum(vapply(df, function(x) !is_numeric_col(x), logical(1))),
        sum(vapply(df, is_numeric_col, logical(1))),
        sum(vapply(df, function(x) all(is.na(x)), logical(1))),
        sum(stats::complete.cases(df)),
        sum(!stats::complete.cases(df))
      )
    )
    
    summary_df <- summary_df[order(summary_df$value), , drop = FALSE]
    summary_df$metric <- factor(summary_df$metric, levels = summary_df$metric)
    
    ggplot2::ggplot(summary_df, ggplot2::aes(x = metric, y = value, fill = metric)) +
      ggplot2::geom_col(show.legend = FALSE, width = 0.75) +
      ggplot2::geom_text(ggplot2::aes(label = value), hjust = -0.12, size = 4.2, fontface = "bold") +
      ggplot2::coord_flip(clip = "off") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
      ggplot2::labs(title = rv_label("missing_data_profile"), x = NULL, y = rv_label("count")) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        axis.text.y = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )
  }
  
  build_hist_plot <- function(df) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    
    num_cols <- utils::head(names(df)[vapply(df, is_numeric_col, logical(1))], 12)
    if (length(num_cols) == 0) return(NULL)
    
    long_df <- do.call(rbind, lapply(num_cols, function(feat) {
      data.frame(feature = feat, value = df[[feat]])
    }))
    
    ggplot2::ggplot(long_df, ggplot2::aes(x = value)) +
      ggplot2::geom_histogram(bins = 30, fill = "#4b5563", color = "white", na.rm = TRUE) +
      ggplot2::facet_wrap(~feature, scales = "free", ncol = 3) +
      ggplot2::labs(title = rv_label("univariate_distribution"), x = NULL, y = rv_label("frequency")) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }
  
  build_density_plot <- function(df) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    
    num_cols <- utils::head(names(df)[vapply(df, is_numeric_col, logical(1))], 12)
    if (length(num_cols) == 0) return(NULL)
    
    density_list <- lapply(num_cols, function(feat) {
      vals <- stats::na.omit(df[[feat]])
      vals <- vals[is.finite(vals)]
      
      if (length(vals) < 2 || length(unique(vals)) < 2) return(NULL)
      
      data.frame(feature = feat, value = vals)
    })
    
    density_list <- Filter(Negate(is.null), density_list)
    if (length(density_list) == 0) return(NULL)
    
    long_df <- do.call(rbind, density_list)
    
    ggplot2::ggplot(long_df, ggplot2::aes(x = value)) +
      ggplot2::geom_density(color = "#111827", linewidth = 0.8, na.rm = TRUE) +
      ggplot2::facet_wrap(~feature, scales = "free", ncol = 4) +
      ggplot2::labs(title = rv_label("density_estimates"), x = rv_label("value"), y = rv_label("density")) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }
  
  build_qq_plot <- function(df) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    
    num_cols <- utils::head(names(df)[vapply(df, is_numeric_col, logical(1))], 9)
    if (length(num_cols) == 0) return(NULL)
    
    qq_list <- lapply(num_cols, function(feat) {
      vals <- stats::na.omit(df[[feat]])
      if (length(vals) == 0) return(NULL)
      
      data.frame(feature = feat, value = vals)
    })
    
    qq_list <- Filter(Negate(is.null), qq_list)
    if (length(qq_list) == 0) return(NULL)
    
    long_df <- do.call(rbind, qq_list)
    
    ggplot2::ggplot(long_df, ggplot2::aes(sample = value)) +
      ggplot2::stat_qq(size = 1, alpha = 0.65) +
      ggplot2::stat_qq_line(color = "#111827") +
      ggplot2::facet_wrap(~feature, scales = "free", ncol = 3) +
      ggplot2::labs(
        title = rv_label("qq_plot"),
        x = rv_label("theoretical_quantiles"),
        y = rv_label("sample_quantiles")
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }
  
  build_pca_plot <- function(df) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    
    num_df <- df[, names(df)[vapply(df, is_numeric_col, logical(1))], drop = FALSE]
    if (ncol(num_df) < 2) return(NULL)
    
    num_df <- num_df[stats::complete.cases(num_df), , drop = FALSE]
    if (nrow(num_df) < 3) return(NULL)
    
    pca <- tryCatch(stats::prcomp(num_df, scale. = TRUE), error = function(e) NULL)
    if (is.null(pca)) return(NULL)
    
    var_exp <- (pca$sdev^2) / sum(pca$sdev^2)
    keep <- seq_len(min(6L, length(var_exp)))
    
    pca_df <- data.frame(
      component = paste0("PC", keep),
      variance = 100 * var_exp[keep]
    )
    
    pca_df <- pca_df[order(pca_df$variance), , drop = FALSE]
    pca_df$component <- factor(pca_df$component, levels = pca_df$component)
    
    ggplot2::ggplot(pca_df, ggplot2::aes(x = component, y = variance)) +
      ggplot2::geom_col(fill = "#4b5563", width = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", variance)), hjust = -0.12, size = 5) +
      ggplot2::coord_flip(clip = "off") +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
      ggplot2::labs(
        title = rv_label("principal_component_analysis"),
        x = rv_label("principal_component"),
        y = rv_label("percent_variance_explained")
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        axis.text.y = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )
  }
  
  build_pca_loadings_plot <- function(df) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    
    num_df <- df[, names(df)[vapply(df, is_numeric_col, logical(1))], drop = FALSE]
    if (ncol(num_df) < 2) return(NULL)
    
    num_df <- num_df[stats::complete.cases(num_df), , drop = FALSE]
    if (nrow(num_df) < 3) return(NULL)
    
    pca <- tryCatch(stats::prcomp(num_df, scale. = TRUE), error = function(e) NULL)
    if (is.null(pca) || is.null(pca$rotation)) return(NULL)
    
    keep <- seq_len(min(4L, ncol(pca$rotation)))
    
    load_df <- data.frame(
      feature = rep(rownames(pca$rotation), times = length(keep)),
      component = rep(colnames(pca$rotation)[keep], each = nrow(pca$rotation)),
      loading = as.vector(pca$rotation[, keep, drop = FALSE])
    )
    
    ggplot2::ggplot(load_df, ggplot2::aes(x = loading, y = stats::reorder(feature, loading))) +
      ggplot2::geom_col(fill = "#4b5563", width = 0.8) +
      ggplot2::facet_wrap(~component, scales = "free_x") +
      ggplot2::labs(
        title = rv_label("principal_component_loadings"),
        x = rv_label("relative_importance"),
        y = rv_label("feature")
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }
  
  bivariate_state <- shiny::reactive({
    df <- get_df()
    outcome <- safe_scalar(input$cboBivariateOutcome, "")
    
    list(
      df = df,
      outcome = outcome,
      biv_features = sanitize_bivariate_features(df, outcome, input$cboBivariateFeatures %||% character(0)),
      biv_palette = safe_scalar(input$cboColorBrewerBivariate, "Dark2"),
      biv_title = safe_scalar(input$txtPlotBivariateTitle, "")
    )
  })
  
  corr_state <- shiny::reactive({
    df <- get_df()
    
    list(
      df = df,
      corr_features = sanitize_corr_features(df, input$cboCorrFeatures %||% character(0)),
      corr_palette = safe_scalar(input$cboColorBrewerCorrplot, "RdYlBu")
    )
  })
  
  plots_state <- shiny::reactiveValues(
    corr_plot = NULL,
    bivariate_plot = NULL,
    missing_plot = NULL,
    hist_plot = NULL,
    density_plot = NULL,
    qq_plot = NULL,
    pca_plot = NULL,
    pca_loadings_plot = NULL
  )
  
  output$bivariate_header_label <- shiny::renderUI({
    shiny::tags$span(rv_label("automatic_bivariate_header_label"))
  })
  
  output$corrplot_header_label <- shiny::renderUI({
    shiny::tags$span(rv_label("automatic_corrplot_header_label"))
  })
  
  output$user_select_color_parlet_bivariate <- shiny::renderUI({
    shiny::selectInput(
      "cboColorBrewerBivariate",
      paste0(rv_label("palette"), ":"),
      choices = rv_choices("bivariate_palette"),
      selected = "Dark2"
    )
  })
  
  output$user_select_color_parlet_corrplot <- shiny::renderUI({
    shiny::selectInput(
      "cboColorBrewerCorrplot",
      paste0(rv_label("palette"), ":"),
      choices = rv_choices("corrplot_palette"),
      selected = "RdYlBu"
    )
  })
  
  output$bivariate_plot_title <- shiny::renderUI({
    shiny::textInput("txtPlotBivariateTitle", rv_label("plot_title"), value = "")
  })
  
  output$user_select_corr_features <- shiny::renderUI({
    df <- get_df()
    num_cols <- names(df)[vapply(df, is_numeric_col, logical(1))]
    
    shiny::selectInput(
      "cboCorrFeatures",
      paste0(rv_label("correlation_numeric_variables"), ":"),
      choices = num_cols,
      selected = utils::head(num_cols, 6),
      multiple = TRUE
    )
  })
  
  output$user_select_bivariate_outcome <- shiny::renderUI({
    cols <- names(get_df())
    
    shiny::selectInput(
      "cboBivariateOutcome",
      paste0(rv_label("outcome"), ":"),
      choices = cols,
      selected = cols[1]
    )
  })
  
  output$user_select_Bivariate_features <- shiny::renderUI({
    df <- get_df()
    outcome <- safe_scalar(input$cboBivariateOutcome, "")
    cols <- setdiff(names(df), outcome)
    
    shiny::selectInput(
      "cboBivariateFeatures",
      paste0(rv_label("features"), ":"),
      choices = cols,
      selected = utils::head(cols, 6),
      multiple = TRUE
    )
  })
  
  observeEvent(rv_current$working_df, {
    df <- get_df()
    cols <- names(df)
    num_cols <- names(df)[vapply(df, is_numeric_col, logical(1))]
    
    shiny::updateSelectInput(session, "cboBivariateOutcome", choices = cols, selected = cols[1] %||% "")
    shiny::updateSelectInput(session, "cboCorrFeatures", choices = num_cols, selected = utils::head(num_cols, 6))
    shiny::updateSelectInput(
      session,
      "cboBivariateFeatures",
      choices = setdiff(cols, cols[1] %||% ""),
      selected = utils::head(setdiff(cols, cols[1] %||% ""), 6)
    )
    
    plots_state$missing_plot <- build_missing_profile_plot(df)
    plots_state$hist_plot <- build_hist_plot(df)
    plots_state$density_plot <- build_density_plot(df)
    plots_state$qq_plot <- build_qq_plot(df)
    plots_state$pca_plot <- build_pca_plot(df)
    plots_state$pca_loadings_plot <- build_pca_loadings_plot(df)
  }, ignoreInit = FALSE)
  
  observeEvent(input$cboBivariateOutcome, {
    df <- get_df()
    outcome <- safe_scalar(input$cboBivariateOutcome, "")
    cols <- setdiff(names(df), outcome)
    selected <- intersect(input$cboBivariateFeatures %||% character(0), cols)
    
    shiny::updateSelectInput(
      session,
      "cboBivariateFeatures",
      choices = cols,
      selected = selected
    )
  }, ignoreInit = TRUE)
  
  bivariate_request <- shiny::debounce(shiny::reactive({
    state <- bivariate_state()
    
    list(
      outcome = state$outcome,
      biv_features = sort(state$biv_features),
      biv_palette = state$biv_palette,
      biv_title = state$biv_title,
      nrow = nrow(state$df),
      ncol = ncol(state$df)
    )
  }), 120)
  
  corr_request <- shiny::debounce(shiny::reactive({
    state <- corr_state()
    
    list(
      corr_features = sort(state$corr_features),
      corr_palette = state$corr_palette,
      nrow = nrow(state$df),
      ncol = ncol(state$df)
    )
  }), 120)
  
  observeEvent(bivariate_request(), {
    state <- bivariate_state()
    
    plots_state$bivariate_plot <- build_bivariate_plot(
      df = state$df,
      outcome = state$outcome,
      features = state$biv_features,
      palette_name = state$biv_palette,
      plot_title = state$biv_title
    )
    
    plots_auto_rv$plot_bivariate_auto <- plots_state$bivariate_plot
  }, ignoreInit = FALSE)
  
  observeEvent(corr_request(), {
    state <- corr_state()
    
    plots_state$corr_plot <- build_corrplot_gg(
      df = state$df,
      features = state$corr_features,
      palette_name = state$corr_palette
    )
    
    plots_auto_rv$plot_corr <- plots_state$corr_plot
  }, ignoreInit = FALSE)
  
  output$ReportBasicStatsTable <- shiny::renderTable({
    df <- get_df()
    
    data.frame(
      Item = c(
        rv_label("dataset_rows"),
        rv_label("dataset_columns"),
        rv_label("discrete_columns"),
        rv_label("continuous_columns"),
        rv_label("all_missing_columns"),
        rv_label("missing_observations"),
        rv_label("complete_rows"),
        rv_label("total_observations")
      ),
      Value = c(
        nrow(df),
        ncol(df),
        sum(vapply(df, function(x) !is_numeric_col(x), logical(1))),
        sum(vapply(df, is_numeric_col, logical(1))),
        sum(vapply(df, function(x) all(is.na(x)), logical(1))),
        sum(!stats::complete.cases(df)),
        sum(stats::complete.cases(df)),
        nrow(df) * ncol(df)
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$ReportMissingPlot <- shiny::renderPlot({
    shiny::req(plots_state$missing_plot)
    print(plots_state$missing_plot)
  }, res = 110, height = 420)
  
  output$ReportHistPlot <- shiny::renderPlot({
    shiny::req(plots_state$hist_plot)
    print(plots_state$hist_plot)
  }, res = 110, height = 700)
  
  output$ReportDensityPlot <- shiny::renderPlot({
    shiny::req(plots_state$density_plot)
    print(plots_state$density_plot)
  }, res = 110, height = 700)
  
  output$ReportQQPlot <- shiny::renderPlot({
    shiny::req(plots_state$qq_plot)
    print(plots_state$qq_plot)
  }, res = 110, height = 700)
  
  output$CorrPlotOutputDedicated <- shiny::renderPlot({
    shiny::validate(
      shiny::need(
        !is.null(plots_state$corr_plot),
        rv_label("select_at_least_two_valid_numeric_variables")
      )
    )
    print(plots_state$corr_plot)
  }, res = 120, height = 650)
  
  output$BivariatePlotOutputReport <- shiny::renderPlot({
    shiny::req(plots_state$bivariate_plot)
    print(plots_state$bivariate_plot)
  }, res = 120, height = 700)
  
  output$ReportPCAPlot <- shiny::renderPlot({
    shiny::req(plots_state$pca_plot)
    print(plots_state$pca_plot)
  }, res = 110, height = 480)
  
  output$ReportPCALoadingsPlot <- shiny::renderPlot({
    shiny::req(plots_state$pca_loadings_plot)
    print(plots_state$pca_loadings_plot)
  }, res = 110, height = 700)
  
  output$auto_visualization_report_view <- shiny::renderUI({
    shiny::tagList(
      shiny::div(
        class = "auto-viz-report-section",
        shiny::tags$h3(rv_label("automatic_visualization_report"))
      ),
      shiny::div(
        class = "auto-viz-report-section",
        shiny::tags$h3(rv_label("basic_statistics")),
        shiny::div(class = "auto-viz-table-wrap", shiny::tableOutput("ReportBasicStatsTable"))
      ),
      shiny::div(
        class = "auto-viz-report-section",
        shiny::tags$h3(rv_label("missing_data_profile")),
        shiny::plotOutput("ReportMissingPlot", height = "420px")
      ),
      shiny::div(
        class = "auto-viz-report-section",
        shiny::tags$h3(rv_label("univariate_distribution")),
        shiny::plotOutput("ReportHistPlot", height = "700px")
      ),
      shiny::div(
        class = "auto-viz-report-section",
        shiny::tags$h3(rv_label("density_estimates")),
        shiny::plotOutput("ReportDensityPlot", height = "700px")
      ),
      shiny::div(
        class = "auto-viz-report-section",
        shiny::tags$h3(rv_label("qq_plot")),
        shiny::plotOutput("ReportQQPlot", height = "700px")
      ),
      shiny::div(
        class = "auto-viz-report-section",
        shiny::tags$h3(rv_label("correlation_analysis")),
        shiny::tags$p(
          style = "margin-bottom:0; color:#4b5563;",
          rv_label("correlation_plot_note")
        )
      ),
      shiny::div(
        class = "auto-viz-report-section",
        shiny::tags$h3(rv_label("bivariate_visualization")),
        shiny::plotOutput("BivariatePlotOutputReport", height = "700px")
      ),
      shiny::div(
        class = "auto-viz-report-section",
        shiny::tags$h3(rv_label("principal_component_analysis")),
        shiny::plotOutput("ReportPCAPlot", height = "480px")
      ),
      shiny::div(
        class = "auto-viz-report-section",
        shiny::tags$h3(rv_label("principal_component_loadings")),
        shiny::plotOutput("ReportPCALoadingsPlot", height = "700px")
      )
    )
  })
  
  auto_report_meta <- shiny::reactive({
    b_state <- bivariate_state()
    c_state <- corr_state()
    
    list(
      rows = nrow(b_state$df),
      cols = ncol(b_state$df),
      outcome = if (nzchar(b_state$outcome)) b_state$outcome else rv_label("not_selected"),
      corr_features = if (length(c_state$corr_features) > 0) {
        paste(c_state$corr_features, collapse = ", ")
      } else {
        rv_label("none")
      },
      biv_features = if (length(b_state$biv_features) > 0) {
        paste(b_state$biv_features, collapse = ", ")
      } else {
        rv_label("none")
      },
      generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })
  
  output$user_download_autoreport <- shiny::renderUI({
    shiny::div(
      style = "display:flex; gap:10px; justify-content:flex-end; flex-wrap:wrap;",
      shiny::downloadButton("btnDownloadReportAutoPdf", rv_label("download_pdf_report"))
    )
  })
  
  output$btnDownloadReportAutoPdf <- shiny::downloadHandler(
    filename = function() {
      paste0("automatic_visualization_report_", format(Sys.Date(), "%Y-%m-%d"), ".pdf")
    },
    content = function(file) {
      meta <- auto_report_meta()
      
      grDevices::pdf(file = file, width = 14, height = 9, onefile = TRUE)
      on.exit(grDevices::dev.off(), add = TRUE)
      
      graphics::plot.new()
      graphics::text(
        x = 0.5,
        y = 0.85,
        labels = rv_label("automatic_visualization_report"),
        cex = 2,
        font = 2
      )
      
      graphics::text(
        x = 0.5,
        y = 0.68,
        labels = paste0(
          rv_label("dataset_rows"), ": ", meta$rows, "\n",
          rv_label("dataset_columns"), ": ", meta$cols, "\n",
          rv_label("outcome"), ": ", meta$outcome, "\n",
          rv_label("bivariate_features"), ": ", meta$biv_features, "\n",
          rv_label("correlation_features"), ": ", meta$corr_features, "\n",
          rv_label("generated"), ": ", meta$generated
        ),
        cex = 1.1
      )
      
      plot_list <- Filter(
        Negate(is.null),
        list(
          plots_state$missing_plot,
          plots_state$hist_plot,
          plots_state$density_plot,
          plots_state$qq_plot,
          plots_state$corr_plot,
          plots_state$bivariate_plot,
          plots_state$pca_plot,
          plots_state$pca_loadings_plot
        )
      )
      
      for (p in plot_list) print(p)
    },
    contentType = "application/pdf"
  )
}