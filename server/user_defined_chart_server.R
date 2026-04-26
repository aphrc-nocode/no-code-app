user_defined_chart_server <- function(input, output, session, rv_current, plots_custom_rv, get_rv_labels = NULL, get_rv_choices = NULL) {
  
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  default_rv_choices <- function(key) {
    defaults <- list(
      user_output_type = c(Chart = "Chart", Table = "Table"),
      yes_no_boolean = c(Yes = "TRUE", No = "FALSE"),
      user_visual_orientation = c(Vertical = "vertical", Horizontal = "horizontal"),
      user_report_numeric = c(Mean = "mean", Median = "median"),
      user_numeric_summary = c(`Standard deviation` = "sd", `Min-max` = "min-max"),
      user_y_variable_summary_type = c(Total = "Total", Mean = "Mean", Median = "Median", Count = "Count"),
      user_select_line_type = c(Solid = "solid", Dashed = "dashed", Dotted = "dotted", Dotdash = "dotdash", Longdash = "longdash", Twodash = "twodash"),
      user_select_shape = c(Circle = "16", Triangle = "17", Square = "15", Diamond = "18", Plus = "3", Cross = "4"),
      user_add_smooth = c(None = "none", Loess = "loess", Linear = "lm"),
      user_select_line_join = c(Round = "round", Mitre = "mitre", Bevel = "bevel"),
      user_select_color_parlet = c(Dark2 = "Dark2", Set1 = "Set1", Set2 = "Set2", Set3 = "Set3", Paired = "Paired", Pastel1 = "Pastel1", Pastel2 = "Pastel2", Accent = "Accent"),
      bivariate_palette = c(Dark2 = "Dark2", Set1 = "Set1", Set2 = "Set2", Set3 = "Set3", Paired = "Paired", Pastel1 = "Pastel1", Pastel2 = "Pastel2", Accent = "Accent"),
      corrplot_palette = c(RdYlBu = "RdYlBu", RdBu = "RdBu", BrBG = "BrBG", PiYG = "PiYG", PRGn = "PRGn", Spectral = "Spectral"),
      r_colors = c(Blue = "#1591a3", Red = "red", Green = "green", Black = "black", Orange = "orange", Purple = "purple", Grey = "grey", Brown = "brown", Pink = "pink"),
      user_ggthemes = c(Grey = "theme_grey", Minimal = "theme_minimal", Classic = "theme_classic", Light = "theme_light", BW = "theme_bw")
    )
    defaults[[key]] %||% character(0)
  }

  rv_choices <- function(key) {
    if (exists("get_rv_choices", mode = "function", inherits = TRUE)) {
      vals <- get_rv_choices(key)
      if (!is.null(vals) && length(vals) > 0) return(vals)
    }
    if (exists("get_named_choices", mode = "function", inherits = TRUE) &&
        exists("input_choices_file", inherits = TRUE) &&
        !is.null(input$change_language)) {
      vals <- get_named_choices(input_choices_file, input$change_language, key)
      if (!is.null(vals) && length(vals) > 0) return(vals)
    }
    default_rv_choices(key)
  }

  rv_choice_values <- function(key) {
    vals <- rv_choices(key)
    unname(vals)
  }

  filter_rv_choices <- function(key, allowed) {
    vals <- rv_choices(key)
    if (length(vals) == 0) return(character(0))
    vals[unname(vals) %in% allowed]
  }

  select_placeholder <- function(label = rv_label("select_variable")) {
    out <- list("")
    names(out) <- label
    out
  }
  
  is_blank <- function(x) {
    is.null(x) || length(x) == 0 || identical(x, "") || (length(x) == 1 && is.na(x))
  }
  
  non_blank <- function(x) !is_blank(x)
  
  safe_scalar <- function(x, default = NULL) {
    if (is.null(x) || length(x) == 0) return(default)
    x1 <- x[1]
    if (is.na(x1) || identical(x1, "")) return(default)
    x1
  }
  
  safe_numeric <- function(x, default = NULL) {
    x1 <- safe_scalar(x, default)
    out <- suppressWarnings(as.numeric(x1))
    if (length(out) == 0 || is.na(out)) default else out
  }
  
  safe_bool <- function(x, default = FALSE) {
    if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) return(default)
    
    if (is.logical(x)) {
      if (length(x) == 0 || is.na(x[1])) return(default)
      return(isTRUE(x[1]))
    }
    
    x_chr <- tolower(trimws(as.character(x[1])))
    
    if (x_chr %in% c("true", "t", "1", "yes", "y", "vertical", "stack", "stacked", "show")) return(TRUE)
    if (x_chr %in% c("false", "f", "0", "no", "n", "horizontal", "dodge", "grouped", "hide")) return(FALSE)
    
    default
  }
  
  safe_theme_fun <- function(theme_name) {
    theme_name <- safe_scalar(theme_name, "theme_grey")
    if (!exists(theme_name, mode = "function")) theme_name <- "theme_grey"
    match.fun(theme_name)
  }
  
  keep_valid_single <- function(selected, choices) {
    if (is.null(selected) || length(selected) == 0 || identical(selected, "") || !selected %in% choices) "" else selected
  }
  
  non_numric_df <- function(df) {
    df[, sapply(df, function(x) {
      is.character(x) || is.factor(x) || is.logical(x) ||
        inherits(x, "Date") || inherits(x, c("POSIXct", "POSIXt"))
    }), drop = FALSE]
  }
  
  numeric_df <- function(df) {
    df[, sapply(df, function(x) is.numeric(x) || is.integer(x)), drop = FALSE]
  }
  
  # Only true categorical variables for wrap dropdown:
  # character, factor, logical
  non_numric_non_date_df <- function(df) {
    df[, sapply(df, function(x) {
      is.character(x) || is.factor(x) || is.logical(x)
    }), drop = FALSE]
  }
  
  get_var_type <- function(df, var) {
    if (is_blank(var) || !var %in% names(df)) return("none")
    x <- df[[var]]
    
    if (is.numeric(x) || is.integer(x)) return("numeric")
    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return("date")
    if (is.factor(x) || is.character(x) || is.logical(x)) return("categorical")
    
    "other"
  }
  
  is_wrap_candidate <- function(x, nm = "") {
    if (!(is.character(x) || is.factor(x) || is.logical(x))) return(FALSE)
    
    nm_clean <- tolower(gsub("[^a-z0-9]", "", nm))
    if (grepl("(^id$|id$|^id|phone|mobile|email|mail|address|street|name|fullname|firstname|lastname|uuid|guid|passport|ssn|mrn)", nm_clean)) {
      return(FALSE)
    }
    
    vals <- x[!is.na(x)]
    vals <- vals[trimws(as.character(vals)) != ""]
    if (length(vals) == 0) return(FALSE)
    
    nunique <- length(unique(as.character(vals)))
    n <- length(vals)
    if (nunique <= 1) return(FALSE)
    if (nunique > max(12, min(30, ceiling(n * 0.20)))) return(FALSE)
    if ((nunique / max(1, n)) > 0.50) return(FALSE)
    
    if (is.character(vals)) {
      char_len <- stats::median(nchar(as.character(vals)), na.rm = TRUE)
      if (!is.na(char_len) && char_len > 30) return(FALSE)
    }
    
    TRUE
  }
  
  wrap_candidate_names <- function(df) {
    nms <- names(df)
    keep <- vapply(seq_along(df), function(i) is_wrap_candidate(df[[i]], nms[i]), logical(1))
    nms[keep]
  }
  
  get_var_type <- function(df, var) {
    if (is_blank(var) || !var %in% names(df)) return("none")
    x <- df[[var]]
    
    if (is.numeric(x) || is.integer(x)) return("numeric")
    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return("date")
    if (is.factor(x) || is.character(x) || is.logical(x)) return("categorical")
    
    "other"
  }
  
  normalize_facet_values <- function(x) {
    out <- if (is.factor(x)) {
      as.character(x)
    } else if (is.logical(x)) {
      ifelse(is.na(x), NA_character_, ifelse(x, "TRUE", "FALSE"))
    } else {
      as.character(x)
    }
    
    out[is.na(out) | trimws(out) == ""] <- "(Missing)"
    out
  }
  
  prepare_facet_data <- function(df, facet_var) {
    if (is_blank(facet_var) || !facet_var %in% names(df)) {
      return(list(df = df, facet_var = NULL, n_panels = 0))
    }
    
    facet_source <- df[[facet_var]]
    if (!(is.character(facet_source) || is.factor(facet_source) || is.logical(facet_source))) {
      return(list(df = df, facet_var = NULL, n_panels = 0))
    }
    
    facet_col <- "facet_panel_var_internal"
    facet_vals <- normalize_facet_values(facet_source)
    df[[facet_col]] <- factor(facet_vals, levels = unique(facet_vals))
    
    list(
      df = df,
      facet_var = facet_col,
      n_panels = length(levels(df[[facet_col]]))
    )
  }
  
  add_facet_to_plot <- function(p, facet_var = NULL, facet_title_size = 20, n_panels = 0) {
    if (is.null(p) || is_blank(facet_var)) return(p)
    if (!inherits(p, "ggplot")) return(p)
    
    ncol_val <- if (!is.null(n_panels) && n_panels == 2) 2 else NULL
    
    p +
      ggplot2::facet_wrap(
        stats::as.formula(paste0("~ `", facet_var, "`")),
        ncol = ncol_val
      ) +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = facet_title_size)
      )
  }
  
  summary_fun_for_choice <- function(choice) {
    choice <- safe_scalar(choice, "Total")
    if (identical(choice, "Average")) {
      function(z) mean(z, na.rm = TRUE)
    } else {
      function(z) sum(z, na.rm = TRUE)
    }
  }
  
  coalesce_label <- function(label_value, fallback_value) {
    label_value <- safe_scalar(label_value, "")
    if (identical(label_value, "")) fallback_value else label_value
  }
  
  build_faceted_plot_native <- function(df, chart_type, xvar, yvar, color_var, facet_var,
                                        txt_xlab, txt_ylab, txt_title, txt_legend,
                                        vertical_flag, stacked_flag, add_shapes_flag,
                                        display_se_flag, add_line_type_flag, add_points_flag,
                                        line_type_val, line_join_val, add_smooth_val,
                                        summary_type_val, shape_val, title_pos_val,
                                        title_size_val, axis_title_size_val,
                                        axis_text_size_val, facet_title_size_val,
                                        data_label_size_val, axis_text_angle_val,
                                        bar_width_val, line_size_val, conf_int_val,
                                        brewer_val, single_color_val, theme_fun) {
    if (is_blank(facet_var) || !facet_var %in% names(df)) return(NULL)
    if (!chart_type %in% c("Bar", "Boxplot", "Violin", "Line", "Scatterplot")) return(NULL)
    
    needed <- unique(c(xvar, if (non_blank(yvar)) yvar else NULL,
                       if (non_blank(color_var)) color_var else NULL, facet_var))
    plot_df <- df[, needed, drop = FALSE]
    
    if (non_blank(yvar) && yvar %in% names(plot_df)) {
      plot_df <- plot_df[!is.na(plot_df[[yvar]]), , drop = FALSE]
    }
    if (xvar %in% names(plot_df)) {
      plot_df <- plot_df[!is.na(plot_df[[xvar]]), , drop = FALSE]
    }
    plot_df <- plot_df[!is.na(plot_df[[facet_var]]), , drop = FALSE]
    if (nrow(plot_df) == 0) return(NULL)
    
    n_panels <- length(unique(as.character(plot_df[[facet_var]])))
    ncol_val <- if (n_panels == 2) 2 else NULL
    summary_fun <- summary_fun_for_choice(summary_type_val)
    x_label <- coalesce_label(txt_xlab, xvar)
    y_label <- coalesce_label(txt_ylab, if (non_blank(yvar)) yvar else "Count")
    
    base_theme <- theme_fun() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = title_pos_val, size = title_size_val),
        axis.title.x = ggplot2::element_text(size = axis_title_size_val),
        axis.title.y = ggplot2::element_text(size = axis_title_size_val),
        axis.text.x = ggplot2::element_text(size = axis_text_size_val, angle = axis_text_angle_val, hjust = if (axis_text_angle_val == 0) 0.5 else 1),
        axis.text.y = ggplot2::element_text(size = axis_text_size_val),
        strip.text = ggplot2::element_text(size = facet_title_size_val)
      )
    
    p <- NULL
    
    if (identical(chart_type, "Bar")) {
      if (!non_blank(yvar)) {
        if (non_blank(color_var)) {
          p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(x = xvar, fill = color_var)) +
            ggplot2::geom_bar(width = bar_width_val, position = if (stacked_flag) "stack" else ggplot2::position_dodge2(width = bar_width_val, preserve = "single"))
        } else {
          p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(x = xvar)) +
            ggplot2::geom_bar(width = bar_width_val, fill = single_color_val)
        }
        y_label <- coalesce_label(txt_ylab, "Count")
      } else {
        grouping <- c(xvar, facet_var, if (non_blank(color_var)) color_var else NULL)
        sum_df <- stats::aggregate(plot_df[[yvar]], plot_df[grouping], sum, na.rm = TRUE)
        names(sum_df)[names(sum_df) == "x"] <- yvar
        if (non_blank(color_var)) {
          p <- ggplot2::ggplot(sum_df, ggplot2::aes_string(x = xvar, y = yvar, fill = color_var)) +
            ggplot2::geom_col(width = bar_width_val, position = if (stacked_flag) "stack" else ggplot2::position_dodge2(width = bar_width_val, preserve = "single"))
        } else {
          p <- ggplot2::ggplot(sum_df, ggplot2::aes_string(x = xvar, y = yvar)) +
            ggplot2::geom_col(width = bar_width_val, fill = single_color_val)
        }
      }
      p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~ `", facet_var, "`")), ncol = ncol_val)
      if (!vertical_flag) p <- p + ggplot2::coord_flip()
    }
    
    if (identical(chart_type, "Scatterplot") && non_blank(yvar)) {
      if (non_blank(color_var)) {
        p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(x = xvar, y = yvar, color = color_var))
      } else {
        p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(x = xvar, y = yvar))
      }
      p <- p + ggplot2::geom_point(size = line_size_val + 1, shape = if (add_shapes_flag) shape_val else 16,
                                   color = if (non_blank(color_var)) NULL else single_color_val)
      if (!identical(add_smooth_val, "none")) {
        p <- p + ggplot2::geom_smooth(method = add_smooth_val, se = display_se_flag, level = conf_int_val,
                                      color = if (non_blank(color_var)) NULL else single_color_val)
      }
      p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~ `", facet_var, "`")), ncol = ncol_val)
    }
    
    if (identical(chart_type, "Boxplot")) {
      if (non_blank(yvar)) {
        if (non_blank(color_var)) {
          p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(x = xvar, y = yvar, fill = color_var)) + ggplot2::geom_boxplot()
        } else {
          p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(x = xvar, y = yvar)) + ggplot2::geom_boxplot(fill = single_color_val)
        }
        if (!vertical_flag) p <- p + ggplot2::coord_flip()
      } else {
        if (non_blank(color_var)) {
          p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(y = xvar, fill = color_var)) + ggplot2::geom_boxplot()
        } else {
          p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(y = xvar)) + ggplot2::geom_boxplot(fill = single_color_val)
        }
        x_label <- coalesce_label(txt_xlab, "")
        y_label <- coalesce_label(txt_ylab, xvar)
      }
      p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~ `", facet_var, "`")), ncol = ncol_val)
    }
    
    if (identical(chart_type, "Violin") && non_blank(yvar)) {
      if (non_blank(color_var)) {
        p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(x = xvar, y = yvar, fill = color_var)) + ggplot2::geom_violin(trim = FALSE)
      } else {
        p <- ggplot2::ggplot(plot_df, ggplot2::aes_string(x = xvar, y = yvar)) + ggplot2::geom_violin(trim = FALSE, fill = single_color_val)
      }
      p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~ `", facet_var, "`")), ncol = ncol_val)
    }
    
    if (identical(chart_type, "Line") && non_blank(yvar)) {
      grouping <- c(xvar, facet_var, if (non_blank(color_var)) color_var else NULL)
      line_df <- stats::aggregate(plot_df[[yvar]], plot_df[grouping], summary_fun)
      names(line_df)[names(line_df) == "x"] <- yvar
      if (non_blank(color_var)) {
        if (isTRUE(add_line_type_flag)) {
          mapping <- ggplot2::aes_string(x = xvar, y = yvar, color = color_var, group = color_var, linetype = color_var)
        } else {
          mapping <- ggplot2::aes_string(x = xvar, y = yvar, color = color_var, group = color_var)
        }
        p <- ggplot2::ggplot(line_df, mapping) + ggplot2::geom_line(size = line_size_val, linejoin = line_join_val)
      } else {
        p <- ggplot2::ggplot(line_df, ggplot2::aes_string(x = xvar, y = yvar, group = 1)) +
          ggplot2::geom_line(size = line_size_val, linejoin = line_join_val, color = single_color_val, linetype = line_type_val)
      }
      if (add_points_flag) {
        p <- p + ggplot2::geom_point(size = line_size_val + 1,
                                     color = if (non_blank(color_var)) NULL else single_color_val)
      }
      p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~ `", facet_var, "`")), ncol = ncol_val)
    }
    
    if (is.null(p)) return(NULL)
    
    p <- p + ggplot2::labs(
      x = x_label,
      y = y_label,
      title = txt_title,
      fill = if (non_blank(txt_legend)) txt_legend else ggplot2::waiver(),
      color = if (non_blank(txt_legend)) txt_legend else ggplot2::waiver(),
      linetype = if (non_blank(txt_legend)) txt_legend else ggplot2::waiver()
    ) + base_theme
    
    if (non_blank(color_var)) {
      p <- p + ggplot2::scale_fill_brewer(palette = brewer_val) + ggplot2::scale_color_brewer(palette = brewer_val)
    }
    
    p
  }
  
  has_colourpicker <- requireNamespace("colourpicker", quietly = TRUE)
  
  rv_custom <- reactiveValues(
    chart_type = "Bar",
    plot_drawn = FALSE,
    plot_status = rv_label("choose_chart_variables_to_begin"),
    graphmore_open = FALSE
  )
  
  safe_df <- reactive({
    req(rv_current$working_df)
    rv_current$working_df
  })
  
  is_chart_mode <- reactive({
    identical(input$cboOutput %||% "Chart", "Chart")
  })
  
  current_chart_type <- reactive({
    input$btnChartType %||% rv_custom$chart_type %||% "Bar"
  })
  
  has_plot_available <- reactive({
    isTRUE(is_chart_mode()) &&
      isTRUE(rv_custom$plot_drawn) &&
      !is.null(plots_custom_rv$plot_rv)
  })
  
  pie_allowed <- reactive({
    req(safe_df())
    
    xvar <- input$cboXVar %||% ""
    yvar <- input$cboYVar %||% ""
    
    if (!non_blank(xvar) || !xvar %in% names(safe_df())) return(FALSE)
    if (non_blank(yvar)) return(FALSE)
    
    get_var_type(safe_df(), xvar) %in% c("categorical", "date")
  })
  
  boxplot_requires_y <- reactive({
    req(safe_df())
    
    xvar <- input$cboXVar %||% ""
    if (!non_blank(xvar) || !xvar %in% names(safe_df())) return(FALSE)
    
    get_var_type(safe_df(), xvar) %in% c("categorical", "date")
  })
  
  safe_bind_output <- function(name, obj_name) {
    if (exists(obj_name, inherits = TRUE)) {
      output[[name]] <- get(obj_name, inherits = TRUE)
    }
  }
  
  safe_update_select <- function(input_id, choices = NULL, selected = NULL) {
    try(updateSelectInput(session, input_id, choices = choices, selected = selected), silent = TRUE)
  }
  
  bind_chart_outputs <- function() {
    safe_bind_output("user_plot_options", "user_plot_options")
    safe_bind_output("user_select_variable_on_x_axis", "user_select_variable_on_x_axis")
    safe_bind_output("user_select_variable_on_y_axis", "user_select_variable_on_y_axis")
    safe_bind_output("user_plot_title", "user_plot_title")
    safe_bind_output("user_x_axis_label", "user_x_axis_label")
    safe_bind_output("user_y_axis_label", "user_y_axis_label")
    safe_bind_output("user_download", "user_download")
    
    safe_bind_output("user_more_plot_options", "user_more_plot_options")
    safe_bind_output("user_transform_to_doughnut", "user_transform_to_doughnut")
    safe_bind_output("user_select_color_variable", "user_select_color_variable")
    safe_bind_output("user_visual_orientation", "user_visual_orientation")
    safe_bind_output("user_bar_width", "user_bar_width")
    safe_bind_output("user_bin_width", "user_bin_width")
    safe_bind_output("user_line_size", "user_line_size")
    safe_bind_output("user_select_line_type", "user_select_line_type")
    safe_bind_output("user_add_shapes", "user_add_shapes")
    safe_bind_output("user_select_shape", "user_select_shape")
    safe_bind_output("user_add_smooth", "user_add_smooth")
    safe_bind_output("user_display_confidence_interval", "user_display_confidence_interval")
    safe_bind_output("user_level_of_confidence_interval", "user_level_of_confidence_interval")
    safe_bind_output("user_select_line_join", "user_select_line_join")
    safe_bind_output("user_add_line_type", "user_add_line_type")
    safe_bind_output("user_add_points", "user_add_points")
    safe_bind_output("user_y_variable_summary_type", "user_y_variable_summary_type")
    safe_bind_output("user_title_position", "user_title_position")
    safe_bind_output("user_size_of_plot_title", "user_size_of_plot_title")
    safe_bind_output("user_axis_title_size", "user_axis_title_size")
    safe_bind_output("user_facet_title_size", "user_facet_title_size")
    safe_bind_output("user_axis_text_size", "user_axis_text_size")
    safe_bind_output("user_data_label_size", "user_data_label_size")
    safe_bind_output("user_x_axis_text_angle", "user_x_axis_text_angle")
    safe_bind_output("user_legend_title", "user_legend_title")
    safe_bind_output("user_stacked", "user_stacked")
    safe_bind_output("user_add_density", "user_add_density")
    safe_bind_output("user_remove_histogram", "user_remove_histogram")
    
    output$user_create <- NULL
  }
  
  close_graph_more <- function() {
    rv_custom$graphmore_open <- FALSE
    try(updateSwitchInput(session, "graphmore", value = FALSE), silent = TRUE)
    shinyjs::runjs("$('#graphmoreoption').removeClass('open-panel');")
  }
  
  observeEvent(c(input$manage_data_apply, input$change_language, rv_current$working_df), {
    if (!is.null(rv_current$working_df)) {
      bind_chart_outputs()
      
      if ("cboOutput" %in% names(input)) {
        updateRadioButtons(session, "cboOutput", selected = "Chart")
      }
      
      close_graph_more()
    }
  }, ignoreInit = FALSE)
  
  observe({
    if (!is.null(rv_current$working_df)) {
      output$user_ggthemes <- renderUI({
        choices <- rv_choices("user_ggthemes")
        if (length(choices) == 0) choices <- default_rv_choices("user_ggthemes")
        selected <- if ("theme_grey" %in% unname(choices)) "theme_grey" else unname(choices)[1]
        selectInput(
          "ggplot_theme",
          rv_label("user_ggthemes"),
          choices = choices,
          selected = selected
        )
      })
    } else {
      output$user_ggthemes <- NULL
    }
  })
  
  output$user_select_facet_variable <- renderUI({
    req(!is.null(rv_current$working_df))
    
    facet_choices <- facet_choices_for_chart()
    
    selectInput(
      inputId = "cboFacetVar",
      label = paste0(rv_label("wrap_by_variable"), ":"),
      choices = c(select_placeholder(rv_label("select_variable")), facet_choices),
      selected = keep_valid_single(input$cboFacetVar, facet_choices)
    )
  })
  
  observe({
    if (!is.null(rv_current$working_df)) {
      shinyjs::showElement("DivvisualizationMenu")
      shinyjs::showElement("Divcustomvisiz")
    } else {
      shinyjs::hide("DivvisualizationMenu")
      shinyjs::showElement("Divcustomvisiz")
    }
  })
  
  
  output$user_graph_more_out <- renderUI({
    if (isTRUE(has_plot_available()) && isTRUE(is_chart_mode())) {
      shinyWidgets::switchInput(
        inputId = "graphmore",
        label = NULL,
        value = isTRUE(rv_custom$graphmore_open),
        onLabel = rv_label("hide_details"),
        offLabel = rv_label("show_more_details"),
        onStatus = "success",
        offStatus = "default",
        size = "normal",
        labelWidth = "90px",
        handleWidth = "35px"
      )
    } else {
      NULL
    }
  })
  
  output$custom_plot_status <- renderUI({
    div(style = "margin: 6px 0 10px 0; color:#5f6b76;", rv_custom$plot_status)
  })
  
  observeEvent(input$graphmore, {
    rv_custom$graphmore_open <- isTRUE(input$graphmore)
  }, ignoreInit = TRUE)
  
  available_chart_types <- reactive({
    req(safe_df())
    
    df <- safe_df()
    xvar <- input$cboXVar %||% ""
    yvar <- input$cboYVar %||% ""
    
    if (is_blank(xvar) || !xvar %in% names(df)) {
      return(c("Bar", "Histogram", "Scatterplot", "Boxplot", "Line", "Violin"))
    }
    
    x_type <- get_var_type(df, xvar)
    y_type <- get_var_type(df, yvar)
    has_y <- non_blank(yvar) && yvar %in% names(df)
    
    choices <- c("Bar")
    
    if (x_type == "numeric") {
      choices <- c(choices, "Histogram", "Boxplot")
      if (has_y && y_type == "numeric") {
        choices <- c(choices, "Scatterplot", "Line")
      }
    }
    
    if (x_type %in% c("categorical", "date")) {
      if (!has_y) {
        choices <- c(choices, "Pie")
      }
      if (has_y && y_type == "numeric") {
        choices <- c(choices, "Boxplot", "Violin", "Line")
      }
    }
    
    unique(choices)
  })
  
  x_choices_for_chart <- reactive({
    req(safe_df())
    
    df <- safe_df()
    ctype <- current_chart_type()
    
    if (identical(ctype, "Pie")) {
      names(non_numric_df(df))
    } else if (identical(ctype, "Histogram")) {
      names(numeric_df(df))
    } else {
      names(df)
    }
  })
  
  y_choices_for_chart <- reactive({
    req(safe_df())
    
    df <- safe_df()
    ctype <- current_chart_type()
    xvar <- input$cboXVar %||% ""
    x_type <- get_var_type(df, xvar)
    
    out <- switch(
      ctype,
      "Histogram" = character(0),
      "Pie" = character(0),
      "Scatterplot" = names(numeric_df(df)),
      "Line" = names(numeric_df(df)),
      "Violin" = names(numeric_df(df)),
      "Boxplot" = if (x_type %in% c("categorical", "date")) names(numeric_df(df)) else character(0),
      "Bar" = names(df),
      names(numeric_df(df))
    )
    
    setdiff(out, xvar)
  })
  
  selected_facet_var <- reactive({
    safe_scalar(input$cboFacetVar, "")
  })
  
  color_choices_for_chart <- reactive({
    req(safe_df())
    setdiff(
      names(non_numric_non_date_df(safe_df())),
      c(input$cboXVar %||% "", input$cboYVar %||% "", input$cboFacetVar %||% "")
    )
  })
  
  facet_choices_for_chart <- reactive({
    req(safe_df())
    setdiff(
      names(non_numric_non_date_df(safe_df())),
      c(input$cboXVar %||% "", input$cboYVar %||% "", input$cboColorVar %||% "")
    )
  })
  
  update_facet_inputs <- function(facet_choices) {
    facet_choice_set <- c(select_placeholder(rv_label("select_variable")), facet_choices)
    
    safe_update_select(
      "cboFacetVar",
      choices = facet_choice_set,
      selected = keep_valid_single(input$cboFacetVar, facet_choices)
    )
  }
  
  observe({
    shinyjs::runjs("
      var cssId = 'custom-visualization-smooth-panels';
      if (!document.getElementById(cssId)) {
        var style = document.createElement('style');
        style.id = cssId;
        style.innerHTML = `
          #graphmoreoption {
            overflow: hidden;
            max-height: 0;
            opacity: 0;
            transform: translateY(-4px);
            transition: max-height 0.35s ease, opacity 0.25s ease, transform 0.25s ease;
            pointer-events: none;
          }
          #graphmoreoption.open-panel {
            max-height: 74vh;
            opacity: 1;
            transform: translateY(0);
            overflow-y: auto;
            overflow-x: hidden;
            padding-right: 8px;
            pointer-events: auto;
          }
        `;
        document.head.appendChild(style);
      }
    ")
  })
  
  observe({
    req(!is.null(rv_current$working_df))
    
    if (is.null(input$ggplot_theme) || identical(input$ggplot_theme, "")) {
      updateSelectInput(session, "ggplot_theme", selected = "theme_grey")
    }
    
    if (has_colourpicker) {
      if (is.null(input$custom_single_color) || identical(input$custom_single_color, "")) {
        colourpicker::updateColourInput(session, "custom_single_color", value = "#1591a3")
      }
    } else {
      if (is.null(input$cboColorSingle) || identical(input$cboColorSingle, "")) {
        updateSelectInput(session, "cboColorSingle", selected = "#1591a3")
      }
    }
    
    if (is.null(input$cboColorBrewer) || identical(input$cboColorBrewer, "")) {
      updateSelectInput(session, "cboColorBrewer", selected = "Set1")
    }
  })
  
  observeEvent(rv_current$working_df, {
    req(safe_df())
    
    df <- safe_df()
    nms <- names(df)
    
    updateSelectInput(
      session, "cboXVar",
      choices = c(select_placeholder(rv_label("select_variable")), nms),
      selected = ""
    )
    updateSelectInput(
      session, "cboYVar",
      choices = c(select_placeholder(rv_label("select_variable")), names(numeric_df(df))),
      selected = ""
    )
    updateSelectInput(
      session, "cboColorVar",
      choices = c(select_placeholder(rv_label("select_variable")), names(non_numric_non_date_df(df))),
      selected = ""
    )
    update_facet_inputs(names(non_numric_non_date_df(df)))
    
    rv_custom$chart_type <- "Bar"
    rv_custom$plot_drawn <- FALSE
    rv_custom$plot_status <- rv_label("choose_chart_variables_to_begin")
    plots_custom_rv$plot_rv <- NULL
    
    close_graph_more()
  }, ignoreInit = FALSE)
  
  output$user_chart_type <- renderUI({
    req(!is.null(rv_current$working_df))
    if (!isTRUE(is_chart_mode())) return(NULL)
    if (!non_blank(input$cboXVar)) return(NULL)
    
    allowed_values <- available_chart_types()
    choices <- filter_rv_choices("user_chart_type", allowed_values)
    if (length(choices) == 0) choices <- allowed_values
    selected <- current_chart_type()
    
    if (!selected %in% unname(choices)) {
      selected <- unname(choices[1])
    }
    
    shinyWidgets::radioGroupButtons(
      inputId = "btnChartType",
      #label = tags$b(paste0(rv_label("select_visualization_style"), ":")),
      choices = choices,
      selected = selected,
      status = "success",
      justified = TRUE,
      checkIcon = list(yes = icon("check-circle"))
    )
  })
  
  observe({
    req(!is.null(rv_current$working_df))
    
    if (isTRUE(is_chart_mode()) && non_blank(input$cboXVar)) {
      valid_types <- available_chart_types()
      current_type <- current_chart_type()
      
      if (!current_type %in% valid_types && length(valid_types) > 0) {
        rv_custom$chart_type <- valid_types[1]
        updateRadioGroupButtons(session, "btnChartType", selected = valid_types[1])
      }
    }
  })
  
  observeEvent(input$btnChartType, {
    req(input$btnChartType)
    
    rv_custom$chart_type <- input$btnChartType
    
    x_choices <- x_choices_for_chart()
    y_choices <- y_choices_for_chart()
    color_choices <- color_choices_for_chart()
    facet_choices <- facet_choices_for_chart()
    
    updateSelectInput(
      session, "cboXVar",
      choices = c(select_placeholder(rv_label("select_variable")), x_choices),
      selected = keep_valid_single(input$cboXVar, x_choices)
    )
    
    updateSelectInput(
      session, "cboYVar",
      choices = c(select_placeholder(rv_label("select_variable")), y_choices),
      selected = keep_valid_single(input$cboYVar, y_choices)
    )
    
    updateSelectInput(
      session, "cboColorVar",
      choices = c(select_placeholder(rv_label("select_variable")), color_choices),
      selected = keep_valid_single(input$cboColorVar, color_choices)
    )
    
    update_facet_inputs(facet_choices)
    
    if (identical(input$btnChartType, "Pie")) {
      updateSelectInput(session, "cboYVar", selected = "")
      updateSelectInput(session, "cboFacetVar", selected = "")
      rv_custom$plot_status <- rv_label("pie_chart_uses_only_x")
    }
    
    close_graph_more()
  }, ignoreInit = TRUE)
  
  observeEvent(input$cboXVar, {
    req(!is.null(rv_current$working_df))
    
    y_choices <- y_choices_for_chart()
    color_choices <- color_choices_for_chart()
    facet_choices <- facet_choices_for_chart()
    
    updateSelectInput(
      session, "cboYVar",
      choices = c(select_placeholder(rv_label("select_variable")), y_choices),
      selected = keep_valid_single(input$cboYVar, y_choices)
    )
    
    updateSelectInput(
      session, "cboColorVar",
      choices = c(select_placeholder(rv_label("select_variable")), color_choices),
      selected = keep_valid_single(input$cboColorVar, color_choices)
    )
    
    update_facet_inputs(facet_choices)
    
    if (!non_blank(input$cboXVar)) {
      close_graph_more()
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$cboYVar, {
    req(!is.null(rv_current$working_df))
    
    x_choices <- x_choices_for_chart()
    color_choices <- color_choices_for_chart()
    facet_choices <- facet_choices_for_chart()
    
    updateSelectInput(
      session, "cboXVar",
      choices = c(select_placeholder(rv_label("select_variable")), x_choices),
      selected = keep_valid_single(input$cboXVar, x_choices)
    )
    
    updateSelectInput(
      session, "cboColorVar",
      choices = c(select_placeholder(rv_label("select_variable")), color_choices),
      selected = keep_valid_single(input$cboColorVar, color_choices)
    )
    
    update_facet_inputs(facet_choices)
    
    close_graph_more()
  }, ignoreInit = TRUE)
  
  observeEvent(input$cboColorVar, {
    req(!is.null(rv_current$working_df))
    
    x_choices <- x_choices_for_chart()
    y_choices <- y_choices_for_chart()
    facet_choices <- facet_choices_for_chart()
    
    updateSelectInput(
      session, "cboXVar",
      choices = c(select_placeholder(rv_label("select_variable")), x_choices),
      selected = keep_valid_single(input$cboXVar, x_choices)
    )
    
    updateSelectInput(
      session, "cboYVar",
      choices = c(select_placeholder(rv_label("select_variable")), y_choices),
      selected = keep_valid_single(input$cboYVar, y_choices)
    )
    
    update_facet_inputs(facet_choices)
  }, ignoreInit = TRUE)
  
  observeEvent(input$cboFacetVar, {
    req(!is.null(rv_current$working_df))
    
    x_choices <- x_choices_for_chart()
    y_choices <- y_choices_for_chart()
    color_choices <- color_choices_for_chart()
    facet_choices <- facet_choices_for_chart()
    
    updateSelectInput(
      session, "cboXVar",
      choices = c(select_placeholder(rv_label("select_variable")), x_choices),
      selected = keep_valid_single(input$cboXVar, x_choices)
    )
    
    updateSelectInput(
      session, "cboYVar",
      choices = c(select_placeholder(rv_label("select_variable")), y_choices),
      selected = keep_valid_single(input$cboYVar, y_choices)
    )
    
    updateSelectInput(
      session, "cboColorVar",
      choices = c(select_placeholder(rv_label("select_variable")), color_choices),
      selected = keep_valid_single(input$cboColorVar, color_choices)
    )
    
    update_facet_inputs(facet_choices)
  }, ignoreInit = TRUE)
  
  output$user_select_color_variable_single <- renderUI({
    req(!is.null(rv_current$working_df))
    
    if (has_colourpicker) {
      colourpicker::colourInput(
        inputId = "custom_single_color",
        label = paste0(rv_label("user_select_color_variable_single"), ":"),
        value = "#1591a3",
        allowTransparent = FALSE,
        showColour = "both",
        palette = "square"
      )
    } else {
      selectInput(
        "cboColorSingle",
        paste0(rv_label("user_select_color_variable_single"), ":"),
        choices = rv_choices("r_colors"),
        selected = "#1591a3"
      )
    }
  })
  
  output$user_select_color_parlet <- renderUI({
    req(!is.null(rv_current$working_df))
    choices <- rv_choices("user_select_color_parlet")
    if (length(choices) == 0) choices <- default_rv_choices("user_select_color_parlet")
    selected <- if ("Set1" %in% unname(choices)) "Set1" else unname(choices)[1]
    
    selectInput(
      "cboColorBrewer",
      paste0(rv_label("user_select_color_parlet"), ":"),
      choices = choices,
      selected = selected
    )
  })
  
  observe({
    is_chart <- identical(input$cboOutput %||% "Chart", "Chart")
    has_data <- !is.null(rv_current$working_df)
    has_plot <- isTRUE(has_plot_available())
    
    if (is_chart) {
      try(shinyjs::show("graphOutputs"), silent = TRUE)
      
      try(shinyjs::show("chartOptionsPanel"), silent = TRUE)
      try(shinyjs::show("chartTopPanel"), silent = TRUE)
      try(shinyjs::show("chartOptionsTitle"), silent = TRUE)
      try(shinyjs::show("chartStyleTitle"), silent = TRUE)
      
      if (has_plot) {
        try(shinyjs::show("chartStatusPanel"), silent = TRUE)
        try(shinyjs::show("chartPlotPanel"), silent = TRUE)
      } else {
        try(shinyjs::hide("chartStatusPanel"), silent = TRUE)
        try(shinyjs::hide("chartPlotPanel"), silent = TRUE)
      }
      
      if (!has_data) {
        close_graph_more()
        try(shinyjs::hide("chartStatusPanel"), silent = TRUE)
        try(shinyjs::hide("chartPlotPanel"), silent = TRUE)
      }
      
    } else {
      try(shinyjs::hide("graphOutputs"), silent = TRUE)
      try(shinyjs::hide("chartOptionsPanel"), silent = TRUE)
      try(shinyjs::hide("chartTopPanel"), silent = TRUE)
      try(shinyjs::hide("chartStatusPanel"), silent = TRUE)
      try(shinyjs::hide("chartPlotPanel"), silent = TRUE)
      try(shinyjs::hide("chartOptionsTitle"), silent = TRUE)
      try(shinyjs::hide("chartStyleTitle"), silent = TRUE)
      try(shinyjs::hide("chartMoreOptionsTitle"), silent = TRUE)
      shinyjs::hide("graphmore")
      close_graph_more()
    }
  })
  
  observe({
    req(!is.null(rv_current$working_df))
    
    ctype <- current_chart_type()
    has_x <- non_blank(input$cboXVar)
    has_plot <- isTRUE(has_plot_available())
    details_open <- isTRUE(rv_custom$graphmore_open)
    
    if (!has_x || !isTRUE(is_chart_mode())) {
      shinyjs::hide("cboYVar")
    } else {
      if (ctype %in% c("Scatterplot", "Line", "Violin", "Bar") ||
          (ctype == "Boxplot" && isTRUE(boxplot_requires_y()))) {
        shinyjs::show("cboYVar")
      } else {
        shinyjs::hide("cboYVar")
      }
    }
    
    if (!has_plot || !details_open || !isTRUE(is_chart_mode())) {
      shinyjs::hide("numBinWidth")
      shinyjs::hide("numBarWidth")
      shinyjs::hide("numLineSize")
      shinyjs::hide("cboAddSmooth")
      shinyjs::hide("rdoAddShapes")
      shinyjs::hide("rdoDisplaySeVal")
      shinyjs::hide("numConfInt")
      shinyjs::hide("cboShapes")
      shinyjs::hide("cboLineType")
      shinyjs::hide("cboLineJoin")
      shinyjs::hide("rdoAddLineType")
      shinyjs::hide("rdoAddPoints")
      shinyjs::hide("rdoSummaryTye")
      shinyjs::hide("rdoStacked")
      shinyjs::hide("rdoPltOrientation")
      shinyjs::hide("rdoOverlayDensity")
      shinyjs::hide("rdoDensityOnly")
      shinyjs::hide("rdoTransformToDoug")
      shinyjs::hide("cboColorBrewer")
      shinyjs::hide("cboColorSingle")
      shinyjs::hide("custom_single_color")
      shinyjs::hide("cboFacetVar")
    } else {
      if (ctype == "Histogram") shinyjs::show("numBinWidth") else shinyjs::hide("numBinWidth")
      if (ctype == "Bar") shinyjs::show("numBarWidth") else shinyjs::hide("numBarWidth")
      if (ctype %in% c("Line", "Scatterplot")) shinyjs::show("numLineSize") else shinyjs::hide("numLineSize")
      
      if (ctype == "Scatterplot") {
        shinyjs::show("cboAddSmooth")
        shinyjs::show("rdoAddShapes")
        shinyjs::show("rdoDisplaySeVal")
        shinyjs::show("numConfInt")
        shinyjs::show("cboShapes")
      } else {
        shinyjs::hide("cboAddSmooth")
        shinyjs::hide("rdoAddShapes")
        shinyjs::hide("rdoDisplaySeVal")
        shinyjs::hide("numConfInt")
        shinyjs::hide("cboShapes")
      }
      
      if (ctype == "Line") {
        shinyjs::show("cboLineType")
        shinyjs::show("cboLineJoin")
        shinyjs::show("rdoAddLineType")
        shinyjs::show("rdoAddPoints")
        shinyjs::show("rdoSummaryTye")
      } else {
        shinyjs::hide("cboLineType")
        shinyjs::hide("cboLineJoin")
        shinyjs::hide("rdoAddLineType")
        shinyjs::hide("rdoAddPoints")
        shinyjs::hide("rdoSummaryTye")
      }
      
      if (ctype == "Bar") {
        shinyjs::show("rdoStacked")
        shinyjs::show("rdoPltOrientation")
      } else if (ctype == "Boxplot") {
        shinyjs::hide("rdoStacked")
        shinyjs::show("rdoPltOrientation")
      } else {
        shinyjs::hide("rdoStacked")
        shinyjs::hide("rdoPltOrientation")
      }
      
      if (ctype == "Histogram") {
        shinyjs::show("rdoOverlayDensity")
        shinyjs::show("rdoDensityOnly")
      } else {
        shinyjs::hide("rdoOverlayDensity")
        shinyjs::hide("rdoDensityOnly")
      }
      
      if (ctype == "Pie") shinyjs::show("rdoTransformToDoug") else shinyjs::hide("rdoTransformToDoug")
      
      if (ctype %in% c("Bar", "Boxplot", "Violin", "Line", "Scatterplot")) {
        shinyjs::show("cboFacetVar")
      } else {
        shinyjs::hide("cboFacetVar")
      }
      
      if (ctype == "Pie" || (non_blank(input$cboColorVar) && ctype %in% c("Violin", "Boxplot", "Line", "Scatterplot", "Bar"))) {
        shinyjs::show("cboColorBrewer")
        shinyjs::hide("cboColorSingle")
        shinyjs::hide("custom_single_color")
      } else {
        shinyjs::hide("cboColorBrewer")
        if (has_colourpicker) {
          shinyjs::show("custom_single_color")
          shinyjs::hide("cboColorSingle")
        } else {
          shinyjs::show("cboColorSingle")
          shinyjs::hide("custom_single_color")
        }
      }
    }
  })
  
  observe({
    if (isTRUE(is_chart_mode()) && !is.null(rv_current$working_df)) {
      shinyjs::show("graphmore")
      shinyjs::show("graphmoreoption")
      
      if (isTRUE(rv_custom$graphmore_open)) {
        shinyjs::runjs("$('#graphmoreoption').addClass('open-panel');")
      } else {
        shinyjs::runjs("$('#graphmoreoption').removeClass('open-panel');")
      }
    } else {
      shinyjs::hide("graphmore")
      shinyjs::hide("graphmoreoption")
      shinyjs::runjs("$('#graphmoreoption').removeClass('open-panel');")
    }
  })
  
  current_single_color <- reactive({
    if (has_colourpicker) {
      safe_scalar(input$custom_single_color, "#1591a3")
    } else {
      safe_scalar(input$cboColorSingle, "#1591a3")
    }
  })
  
  build_plot <- function() {
    req(!is.null(rv_current$working_df))
    req(non_blank(input$cboXVar))
    
    chart_type <- current_chart_type()
    df <- rv_current$working_df
    theme_fun <- safe_theme_fun(input$ggplot_theme)
    
    xvar <- input$cboXVar
    yvar <- input$cboYVar
    color_var <- input$cboColorVar
    facet_var <- selected_facet_var()
    
    txt_xlab <- safe_scalar(input$txtXlab, "")
    txt_ylab <- safe_scalar(input$txtYlab, "")
    txt_title <- safe_scalar(input$txtPlotTitle, "")
    txt_legend <- safe_scalar(input$txtLegend, "")
    
    vertical_flag <- safe_bool(input$rdoPltOrientation, TRUE)
    stacked_flag <- safe_bool(input$rdoStacked, TRUE)
    overlay_density_flag <- safe_bool(input$rdoOverlayDensity, FALSE)
    density_only_flag <- safe_bool(input$rdoDensityOnly, FALSE)
    add_shapes_flag <- safe_bool(input$rdoAddShapes, FALSE)
    display_se_flag <- safe_bool(input$rdoDisplaySeVal, FALSE)
    add_line_type_flag <- safe_bool(input$rdoAddLineType, FALSE)
    add_points_flag <- safe_bool(input$rdoAddPoints, FALSE)
    doughnut_flag <- safe_bool(input$rdoTransformToDoug, FALSE)
    
    line_type_val <- safe_scalar(input$cboLineType, "solid")
    line_join_val <- safe_scalar(input$cboLineJoin, "round")
    add_smooth_val <- safe_scalar(input$cboAddSmooth, "loess")
    summary_type_val <- safe_scalar(input$rdoSummaryTye, "Total")
    
    shape_val <- suppressWarnings(as.integer(safe_scalar(input$cboShapes, 16)))
    if (is.na(shape_val)) shape_val <- 16
    
    title_pos_val <- safe_numeric(input$numplotposition, 0.5)
    title_size_val <- safe_numeric(input$numplottitlesize, 24)
    axis_title_size_val <- safe_numeric(input$numaxisTitleSize, 20)
    axis_text_size_val <- safe_numeric(input$numAxistextSize, 18)
    facet_title_size_val <- safe_numeric(input$numfacettitlesize, 20)
    data_label_size_val <- safe_numeric(input$numDataLabelSize, 5)
    axis_text_angle_val <- safe_numeric(input$xaxistextangle, 0)
    bar_width_val <- safe_numeric(input$numBarWidth, 0.6)
    bin_width_val <- safe_numeric(input$numBinWidth, NULL)
    line_size_val <- safe_numeric(input$numLineSize, 1)
    conf_int_val <- safe_numeric(input$numConfInt, 0.95)
    
    brewer_val <- safe_scalar(input$cboColorBrewer, "Set1")
    single_color_val <- current_single_color()
    
    tryCatch({
      if (!xvar %in% names(df)) return(NULL)
      
      facet_info <- prepare_facet_data(df, facet_var)
      df_plot <- facet_info$df
      facet_plot_var <- facet_info$facet_var
      n_panels <- facet_info$n_panels
      
      p <- NULL
      
      if (identical(chart_type, "Pie")) {
        x_type <- get_var_type(df, xvar)
        if (!(x_type %in% c("categorical", "date"))) return(NULL)
        if (non_blank(yvar)) return(NULL)
        
        p <- Rautoml::custom_piechart(
          df = df,
          xvar = xvar,
          plot_title = txt_title,
          transform_to_doughnut = doughnut_flag,
          title_pos = title_pos_val,
          title_size = title_size_val,
          data_label_size = data_label_size_val,
          custom_theme = theme_fun(),
          legend_title = txt_legend,
          colorbrewer = brewer_val
        )
        
        return(p)
      }
      
      if (identical(chart_type, "Line") && non_blank(yvar) && yvar %in% names(df_plot)) {
        p <- Rautoml::custom_linegraph(
          df = df_plot,
          xvar = xvar,
          yvar = yvar,
          xlab = txt_xlab,
          ylab = txt_ylab,
          line_type = line_type_val,
          plot_title = txt_title,
          line_size = line_size_val,
          line_join = line_join_val,
          colorVar = if (is_blank(color_var)) NULL else color_var,
          title_pos = title_pos_val,
          title_size = title_size_val,
          custom_theme = theme_fun(),
          axis_title_size = axis_title_size_val,
          axis_text_size = axis_text_size_val,
          addlinetype = add_line_type_flag,
          axistext_angle = axis_text_angle_val,
          default_col = single_color_val,
          legend_title = txt_legend,
          addpoints = add_points_flag,
          summary_type = summary_type_val,
          colorbrewer = brewer_val
        )
      }
      
      if (identical(chart_type, "Bar")) {
        p <- Rautoml::custom_barplot(
          df = df_plot,
          xvar = xvar,
          yvar = if (is_blank(yvar)) NULL else yvar,
          xlab = txt_xlab,
          ylab = txt_ylab,
          bar_width = bar_width_val,
          plot_title = txt_title,
          vertical = vertical_flag,
          stackedtype = stacked_flag,
          colorVar = if (is_blank(color_var)) NULL else color_var,
          title_pos = title_pos_val,
          title_size = title_size_val,
          custom_theme = theme_fun(),
          axis_title_size = axis_title_size_val,
          axis_text_size = axis_text_size_val,
          data_label_size = data_label_size_val,
          axistext_angle = axis_text_angle_val,
          legend_title = txt_legend,
          colorbrewer = brewer_val,
          default_col = single_color_val
        )
      }
      
      if (identical(chart_type, "Histogram")) {
        p <- Rautoml::custom_histogram(
          df = df_plot,
          variable = xvar,
          xlab = txt_xlab,
          ylab = txt_ylab,
          plot_title = txt_title,
          title_pos = title_pos_val,
          title_size = title_size_val,
          axis_title_size = axis_title_size_val,
          axis_text_size = axis_text_size_val,
          axistext_angle = axis_text_angle_val,
          custom_theme = theme_fun(),
          bin_width = bin_width_val,
          overlayDensisty = overlay_density_flag,
          density_only = density_only_flag,
          fill_color = single_color_val
        )
      }
      
      if (identical(chart_type, "Scatterplot") && non_blank(yvar) && yvar %in% names(df_plot)) {
        p <- Rautoml::custom_scatterplot(
          df = df_plot,
          xvar = xvar,
          yvar = yvar,
          xlab = txt_xlab,
          ylab = txt_ylab,
          addshape = add_shapes_flag,
          plot_title = txt_title,
          line_size = line_size_val,
          shapes = shape_val,
          colorVar = if (is_blank(color_var)) NULL else color_var,
          title_pos = title_pos_val,
          title_size = title_size_val,
          axis_title_size = axis_title_size_val,
          axis_text_size = axis_text_size_val,
          addsmooth = add_smooth_val,
          axistext_angle = axis_text_angle_val,
          custom_theme = theme_fun(),
          legend_title = txt_legend,
          seval = display_se_flag,
          confelev = conf_int_val,
          colorbrewer = brewer_val,
          default_col = single_color_val
        )
      }
      
      if (identical(chart_type, "Boxplot")) {
        if (isTRUE(boxplot_requires_y())) {
          if (!non_blank(yvar) || !yvar %in% names(df_plot)) return(NULL)
        } else {
          yvar <- NULL
        }
        
        p <- Rautoml::custom_boxplot(
          df = df_plot,
          xvar = xvar,
          yvar = yvar,
          xlab = txt_xlab,
          ylab = txt_ylab,
          plot_title = txt_title,
          vertical = vertical_flag,
          colorVar = if (is_blank(color_var)) NULL else color_var,
          title_pos = title_pos_val,
          title_size = title_size_val,
          axis_title_size = axis_title_size_val,
          axis_text_size = axis_text_size_val,
          axistext_angle = axis_text_angle_val,
          custom_theme = theme_fun(),
          legend_title = txt_legend,
          colorbrewer = brewer_val,
          default_col = single_color_val
        )
      }
      
      if (identical(chart_type, "Violin") && non_blank(yvar) && yvar %in% names(df_plot)) {
        p <- Rautoml::custom_violin(
          df = df_plot,
          xvar = xvar,
          yvar = yvar,
          xlab = txt_xlab,
          ylab = txt_ylab,
          plot_title = txt_title,
          colorVar = if (is_blank(color_var)) NULL else color_var,
          title_pos = title_pos_val,
          title_size = title_size_val,
          axis_title_size = axis_title_size_val,
          axis_text_size = axis_text_size_val,
          axistext_angle = axis_text_angle_val,
          custom_theme = theme_fun(),
          legend_title = txt_legend,
          colorbrewer = brewer_val,
          default_col = single_color_val
        )
      }
      
      if (is.null(p)) return(NULL)
      
      p <- add_facet_to_plot(
        p = p,
        facet_var = facet_plot_var,
        facet_title_size = facet_title_size_val,
        n_panels = n_panels
      )
      
      p
    }, error = function(e) {
      message("Plot build error [", chart_type, "]: ", e$message)
      NULL
    })
  }
  
  generate_plot <- function() {
    if (is.null(rv_current$working_df) || !non_blank(input$cboXVar)) {
      plots_custom_rv$plot_rv <- NULL
      rv_custom$plot_drawn <- FALSE
      rv_custom$plot_status <- rv_label("choose_chart_variables_to_begin")
      close_graph_more()
      return(invisible(NULL))
    }
    
    if (!isTRUE(is_chart_mode())) {
      return(invisible(plots_custom_rv$plot_rv))
    }
    
    chart_type <- current_chart_type()
    
    if (identical(chart_type, "Pie") && !isTRUE(pie_allowed())) {
      plots_custom_rv$plot_rv <- NULL
      rv_custom$plot_drawn <- FALSE
      rv_custom$plot_status <- rv_label("pie_chart_available_only")
      close_graph_more()
      return(invisible(NULL))
    }
    
    if (identical(chart_type, "Boxplot") &&
        isTRUE(boxplot_requires_y()) &&
        !non_blank(input$cboYVar)) {
      plots_custom_rv$plot_rv <- NULL
      rv_custom$plot_drawn <- FALSE
      rv_custom$plot_status <- rv_label("boxplot_needs_y")
      close_graph_more()
      return(invisible(NULL))
    }
    
    rv_custom$plot_status <- paste0(rv_label("generating"), " ", chart_type, "...")
    
    shiny::withProgress(
      message = rv_label("generating_chart"),
      detail = paste0(rv_label("building"), " ", tolower(chart_type), "..."),
      value = 0,
      {
        shiny::incProgress(0.25)
        p <- build_plot()
        shiny::incProgress(0.95)
        
        plots_custom_rv$plot_rv <- p
        rv_custom$plot_drawn <- !is.null(p)
        
        if (!is.null(p)) {
          valid_facet_choices <- facet_choices_for_chart()
          selected_wrap <- selected_facet_var()
          
          if (non_blank(selected_wrap) && selected_wrap %in% valid_facet_choices && chart_type != "Pie") {
            rv_custom$plot_status <- paste0(chart_type, " ", rv_label("ready"), ". ", rv_label("wrapped_by"), " ", selected_wrap, ".")
          } else {
            rv_custom$plot_status <- paste0(chart_type, " ", rv_label("ready"), ".")
          }
        } else {
          rv_custom$plot_status <- paste0(chart_type, " ", rv_label("could_not_be_generated_current_selection"))
          close_graph_more()
        }
      }
    )
  }
  
  plot_request <- shiny::debounce(reactive({
    list(
      x = input$cboXVar %||% "",
      y = input$cboYVar %||% "",
      color = input$cboColorVar %||% "",
      facet = selected_facet_var(),
      chart = current_chart_type(),
      title = input$txtPlotTitle %||% "",
      xlab = input$txtXlab %||% "",
      ylab = input$txtYlab %||% "",
      legend = input$txtLegend %||% "",
      theme = input$ggplot_theme %||% "theme_grey",
      orientation = input$rdoPltOrientation %||% TRUE,
      bar_width = input$numBarWidth %||% 0.6,
      bin_width = input$numBinWidth,
      line_size = input$numLineSize %||% 1,
      line_type = input$cboLineType %||% "solid",
      add_shapes = input$rdoAddShapes %||% FALSE,
      shape = input$cboShapes %||% 16,
      add_smooth = input$cboAddSmooth %||% "loess",
      display_se = input$rdoDisplaySeVal %||% FALSE,
      conf_int = input$numConfInt %||% 0.95,
      line_join = input$cboLineJoin %||% "round",
      add_linetype = input$rdoAddLineType %||% FALSE,
      add_points = input$rdoAddPoints %||% FALSE,
      summary_type = input$rdoSummaryTye %||% "Total",
      title_pos = input$numplotposition %||% 0.5,
      title_size = input$numplottitlesize %||% 24,
      axis_title_size = input$numaxisTitleSize %||% 20,
      facet_title_size = input$numfacettitlesize %||% 20,
      axis_text_size = input$numAxistextSize %||% 18,
      data_label_size = input$numDataLabelSize %||% 5,
      axis_angle = input$xaxistextangle %||% 0,
      stacked = input$rdoStacked %||% TRUE,
      add_density = input$rdoOverlayDensity %||% FALSE,
      density_only = input$rdoDensityOnly %||% FALSE,
      doughnut = input$rdoTransformToDoug %||% FALSE,
      palette = input$cboColorBrewer %||% "Set1",
      single = input$cboColorSingle %||% input$custom_single_color %||% "#1591a3"
    )
  }), 350)
  
  observeEvent(plot_request(), {
    generate_plot()
  }, ignoreInit = TRUE)
  
  output$GeneratedPlot <- renderPlot({
    req(has_plot_available())
    plots_custom_rv$plot_rv
  })
  
  output$btnchartDown <- downloadHandler(
    filename = function() {
      paste(input$btnChartType %||% "plot", format(Sys.time(), "%B %d %Y %H-%M-%S"), ".jpeg")
    },
    content = function(file) {
      req(plots_custom_rv$plot_rv)
      ggplot2::ggsave(file, plot = plots_custom_rv$plot_rv, device = "jpeg", width = 16, height = 9)
    }
  )
}