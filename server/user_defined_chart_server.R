user_defined_chart_server <- function(input, output, session, rv_current,
                                      plots_custom_rv, get_rv_labels = NULL,
                                      get_rv_choices = NULL) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
  blank <- function(x) is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(as.character(x[1]))
  yes <- function(x, default = FALSE) {
    if (blank(x)) return(default)
    tolower(as.character(x[1])) %in% c("true", "t", "1", "yes", "y", "vertical", "stacked")
  }
  number <- function(x, default) {
    ans <- suppressWarnings(as.numeric((x %||% default)[1]))
    if (is.na(ans)) default else ans
  }
  placeholder <- function(label = "Select variable") stats::setNames("", label)
  is_numeric <- function(x) is.numeric(x) || is.integer(x)
  is_date <- function(x) inherits(x, c("Date", "POSIXct", "POSIXt"))
  is_categorical <- function(x) is.factor(x) || is.character(x) || is.logical(x)
  unique_n <- function(x) length(unique(x[!is.na(x)]))

  chart_definitions <- list(
    Bar = list(x = "categorical", y = "numeric_optional"),
    Histogram = list(x = "numeric", y = "none"),
    Scatterplot = list(x = "numeric", y = "numeric_required"),
    Boxplot = list(x = "numeric", y = "categorical_optional"),
    Violin = list(x = "numeric", y = "categorical_required"),
    Line = list(x = "ordered", y = "numeric_required"),
    Pie = list(x = "categorical", y = "none"),
    Density = list(x = "numeric", y = "none")
  )

  state <- reactiveValues(
    chart_type = "Bar", ready = FALSE, generated = FALSE, error = NULL,
    status = "Choose a chart type and variables, then click Generate plot."
  )

  data_names <- reactive({
    if (is.null(rv_current$working_df)) character(0) else names(rv_current$working_df)
  })
  numeric_names <- reactive({
    df <- rv_current$working_df
    if (is.null(df)) character(0) else names(df)[vapply(df, is_numeric, logical(1))]
  })
  categorical_names <- reactive({
    df <- rv_current$working_df
    if (is.null(df)) character(0) else names(df)[vapply(df, is_categorical, logical(1))]
  })
  ordered_names <- reactive({
    df <- rv_current$working_df
    if (is.null(df)) character(0) else names(df)[vapply(df, function(x) is_numeric(x) || is_date(x) || is.factor(x), logical(1))]
  })
  low_cardinality_names <- reactive({
    df <- rv_current$working_df
    nms <- categorical_names()
    if (is.null(df) || !length(nms)) return(character(0))
    nms[vapply(df[nms], function(x) unique_n(x) <= 20, logical(1))]
  })
  chart_type <- reactive(as.character(input$btnChartType %||% state$chart_type)[1])

  x_choices <- reactive({
    def <- chart_definitions[[chart_type()]] %||% chart_definitions$Bar
    switch(def$x,
      numeric = numeric_names(), categorical = categorical_names(),
      ordered = ordered_names(), data_names()
    )
  })
  y_choices <- reactive({
    def <- chart_definitions[[chart_type()]] %||% chart_definitions$Bar
    switch(def$y,
      numeric_optional = numeric_names(), numeric_required = numeric_names(),
      categorical_optional = low_cardinality_names(),
      categorical_required = low_cardinality_names(), character(0)
    )
  })

  output$user_chart_type <- renderUI({
    req(!is.null(rv_current$working_df))
    shinyWidgets::radioGroupButtons(
      inputId = "btnChartType", label = NULL,
      choices = names(chart_definitions), selected = state$chart_type,
      justified = TRUE, status = "success"
    )
  })
  output$user_select_variable_on_x_axis <- renderUI({
    req(!is.null(rv_current$working_df))
    label <- switch(chart_type(),
      Boxplot = "Numeric variable:", Violin = "Numeric variable:",
      Density = "Numeric variable:", Histogram = "Numeric variable:",
      Bar = "Category variable:", Pie = "Category variable:",
      "X-axis variable:"
    )
    selectInput("cboXVar", label, choices = c(placeholder(), x_choices()), selected = "")
  })
  output$user_select_variable_on_y_axis <- renderUI({
    req(!is.null(rv_current$working_df))
    def <- chart_definitions[[chart_type()]]
    if (identical(def$y, "none")) return(NULL)
    label <- switch(def$y,
      categorical_optional = "Group by (optional):",
      categorical_required = "Group by:",
      numeric_optional = "Numeric measure (optional):",
      "Y-axis numeric variable:"
    )
    selectInput("cboYVar", label, choices = c(placeholder(), y_choices()), selected = "")
  })
  output$user_select_color_variable <- renderUI({
    req(!is.null(rv_current$working_df))
    if (chart_type() %in% c("Histogram", "Boxplot", "Violin")) return(NULL)
    selectInput("cboColorVar", "Colour by (optional):",
      choices = c(placeholder(), low_cardinality_names()), selected = "")
  })
  output$user_select_facet_variable <- renderUI({
    req(!is.null(rv_current$working_df))
    if (identical(chart_type(), "Pie")) return(NULL)
    selectInput("cboFacetVar", "Facet by (optional):",
      choices = c(placeholder(), low_cardinality_names()), selected = "")
  })
  output$user_select_group_variable <- renderUI({
    req(!is.null(rv_current$working_df))
    if (identical(chart_type(), "Pie")) return(NULL)
    selectInput("cboFacetVar", "Facet by (optional):",
      choices = c(placeholder(), low_cardinality_names()), selected = "")
  })
  output$user_ggthemes <- renderUI({
    selectInput("ggplot_theme", "Chart theme:",
      choices = c("Grey" = "theme_grey", "Minimal" = "theme_minimal",
                  "Classic" = "theme_classic", "Light" = "theme_light",
                  "Black and white" = "theme_bw"), selected = "theme_grey")
  })
  output$user_create <- renderUI({
    req(!is.null(rv_current$working_df))
    actionButton("btnCreatePlot", "Generate plot", class = "btn-success", icon = icon("chart-bar"))
  })
  output$custom_plot_status <- renderUI({
    colour <- if (!blank(state$error)) "#b42318" else "#5f6b76"
    div(style = paste0("margin:8px 0;color:", colour, ";"), state$error %||% state$status)
  })
  output$user_graph_more_out <- renderUI({
    req(!is.null(rv_current$working_df))
    shinyWidgets::switchInput("graphmore", label = NULL, value = isTRUE(input$graphmore),
      onLabel = "Hide details", offLabel = "Show more details",
      onStatus = "success", offStatus = "default")
  })

  observeEvent(input$graphmore, {
    shinyjs::runjs(if (isTRUE(input$graphmore))
      "$('#graphmoreoption').addClass('open-panel');" else
      "$('#graphmoreoption').removeClass('open-panel');")
  }, ignoreInit = FALSE)

  observe({
    if (identical(input$cboOutput %||% "Chart", "Chart")) {
      shinyjs::show("graphOutputs")
      shinyjs::show("chartTypePanel")
      shinyjs::show("chartPlotPanel")
      shinyjs::hide("tabOutputs")
      shinyjs::hide("tabSummaries")
    } else {
      shinyjs::hide("graphOutputs")
      shinyjs::hide("chartTypePanel")
      shinyjs::hide("chartPlotPanel")
      shinyjs::show("tabOutputs")
      shinyjs::show("tabSummaries")
      shinyjs::runjs("$('#graphmoreoption').removeClass('open-panel');")
    }
  })

  observeEvent(rv_current$working_df, {
    state$ready <- FALSE
    state$generated <- FALSE
    state$error <- NULL
    state$status <- "Choose a chart type and variables, then click Generate plot."
    plots_custom_rv$plot_rv <- NULL
  }, ignoreInit = FALSE)

  observeEvent(input$btnChartType, {
    state$chart_type <- chart_type()
    state$ready <- FALSE
    state$generated <- FALSE
    state$error <- NULL
    state$status <- "Select the required variables, then click Generate plot."
    plots_custom_rv$plot_rv <- NULL
  }, ignoreInit = TRUE)

  observeEvent(list(input$cboXVar, input$cboYVar), {
    if (isTRUE(state$generated)) {
      state$ready <- FALSE
      state$generated <- FALSE
      state$error <- NULL
      state$status <- "Chart variables changed. Click Generate plot to create the new chart."
      plots_custom_rv$plot_rv <- NULL
    }
  }, ignoreInit = TRUE)

  style_request <- shiny::debounce(reactive({
    list(
      colour = input$cboColorVar %||% "",
      facet = input$cboFacetVar %||% "",
      title = input$txtPlotTitle %||% "",
      xlab = input$txtXlab %||% "",
      ylab = input$txtYlab %||% "",
      legend = input$txtLegend %||% "",
      theme = input$ggplot_theme %||% "theme_grey",
      orientation = input$rdoPltOrientation,
      bar_width = input$numBarWidth,
      bin_width = input$numBinWidth,
      line_size = input$numLineSize,
      line_type = input$cboLineType,
      shape = input$cboShapes,
      smooth = input$cboAddSmooth,
      display_se = input$rdoDisplaySeVal,
      confidence = input$numConfInt,
      line_join = input$cboLineJoin,
      add_points = input$rdoAddPoints,
      summary = input$rdoSummaryTye,
      title_position = input$numplotposition,
      title_size = input$numplottitlesize,
      axis_title_size = input$numaxisTitleSize,
      facet_title_size = input$numfacettitlesize,
      axis_text_size = input$numAxistextSize,
      axis_angle = input$xaxistextangle,
      stacked = input$rdoStacked,
      overlay_density = input$rdoOverlayDensity,
      density_only = input$rdoDensityOnly,
      doughnut = input$rdoTransformToDoug,
      palette = input$cboColorBrewer,
      single_colour = input$cboColorSingle %||% input$custom_single_color
    )
  }), 450)

  observeEvent(style_request(), {
    if (!isTRUE(state$generated) ||
        !identical(input$cboOutput %||% "Chart", "Chart")) return()

    state$error <- NULL
    state$status <- "Updating plot automatically..."
    previous_plot <- plots_custom_rv$plot_rv
    result <- tryCatch(make_plot(), error = function(e) {
      list(ok = FALSE, error = conditionMessage(e), plot = NULL)
    })

    if (isTRUE(result$ok)) {
      plots_custom_rv$plot_rv <- result$plot
      state$ready <- TRUE
      state$status <- paste(chart_type(), "updated automatically.")
    } else {
      plots_custom_rv$plot_rv <- previous_plot
      state$ready <- !is.null(previous_plot)
      state$error <- paste("The customization could not be applied:", result$error)
      state$status <- "The previous plot has been retained."
    }
  }, ignoreInit = TRUE)

  observe({
    type <- chart_type()
    visibility <- list(
      user_transform_to_doughnut = type == "Pie",
      user_visual_orientation = type %in% c("Bar", "Boxplot", "Violin"),
      user_bar_width = type == "Bar",
      user_bin_width = type == "Histogram",
      user_line_size = type %in% c("Scatterplot", "Line"),
      user_select_line_type = type == "Line",
      user_add_shapes = type == "Scatterplot",
      user_select_shape = type == "Scatterplot",
      user_add_smooth = type == "Scatterplot",
      user_display_confidence_interval = type == "Scatterplot",
      user_level_of_confidence_interval = type == "Scatterplot",
      user_select_line_join = type == "Line",
      user_add_line_type = FALSE,
      user_add_points = type == "Line",
      user_y_variable_summary_type = type == "Bar",
      user_stacked = type == "Bar",
      user_add_density = type == "Histogram",
      user_remove_histogram = type == "Histogram",
      user_data_label_size = FALSE
    )
    for (id in names(visibility)) {
      shinyjs::toggle(id = id, condition = isTRUE(visibility[[id]]))
    }
  })

  validate_request <- function(df, type, x, y, colour, facet) {
    if (is.null(df) || nrow(df) == 0) return("The dataset contains no observations.")
    if (blank(x) || !x %in% names(df)) return("Select a valid variable for this chart.")
    def <- chart_definitions[[type]]
    if (def$x == "numeric" && !is_numeric(df[[x]])) return("This chart requires a numeric variable.")
    if (def$x == "categorical" && !is_categorical(df[[x]])) return("This chart requires a categorical variable.")
    if (def$x == "ordered" && !(is_numeric(df[[x]]) || is_date(df[[x]]) || is.factor(df[[x]]))) return("The line chart requires a numeric, date, or ordered factor X variable.")
    if (grepl("required$", def$y) && blank(y)) return(if (grepl("categorical", def$y)) "Select a grouping variable." else "Select a numeric Y variable.")
    if (!blank(y) && grepl("numeric", def$y) && (!y %in% names(df) || !is_numeric(df[[y]]))) return("The selected measure must be numeric.")
    if (!blank(y) && grepl("categorical", def$y) && (!y %in% names(df) || !is_categorical(df[[y]]))) return("The selected grouping variable must be categorical.")
    if (type == "Pie" && unique_n(df[[x]]) > 10) return("Pie charts are limited to 10 categories. Use a bar chart for this variable.")
    if (type == "Bar" && unique_n(df[[x]]) > 50) return("This variable has more than 50 categories. Choose a lower-cardinality variable.")
    if (!blank(facet) && (!facet %in% names(df) || unique_n(df[[facet]]) > 20)) return("Facet variables must contain 20 or fewer categories.")
    if (!blank(colour) && (!colour %in% names(df) || unique_n(df[[colour]]) > 20)) return("Colour variables must contain 20 or fewer categories.")
    needed <- unique(c(x, if (!blank(y)) y, if (!blank(colour)) colour, if (!blank(facet)) facet))
    complete <- stats::complete.cases(df[, needed, drop = FALSE])
    if (!any(complete)) return("No complete observations remain for the selected variables.")
    if (type %in% c("Histogram", "Density")) {
      values <- df[[x]][is.finite(df[[x]])]
      if (length(values) < 2) return(paste(type, "requires at least two finite observations."))
      if (type == "Density" && length(unique(values)) < 2) return("Density requires a numeric variable with variation.")
      if (type == "Density" && !blank(colour)) {
        group_sizes <- table(df[[colour]], useNA = "no")
        if (!length(group_sizes) || all(group_sizes < 2)) return("Each density group needs at least two observations; no eligible group was found.")
      }
    }
    if (type %in% c("Boxplot", "Violin") && !blank(y)) {
      groups <- table(df[[y]], useNA = "no")
      if (!length(groups) || all(groups < 2)) return("At least one group must contain two or more observations.")
    }
    NULL
  }

  palette_scale <- function(aesthetic, palette, n) {
    brewer_info <- RColorBrewer::brewer.pal.info
    if (palette %in% rownames(brewer_info) && n <= brewer_info[palette, "maxcolors"]) {
      if (aesthetic == "fill") ggplot2::scale_fill_brewer(palette = palette) else ggplot2::scale_colour_brewer(palette = palette)
    } else {
      if (aesthetic == "fill") ggplot2::scale_fill_viridis_d() else ggplot2::scale_colour_viridis_d()
    }
  }

  make_plot <- function() {
    df <- rv_current$working_df
    type <- chart_type()
    x <- as.character(input$cboXVar %||% "")[1]
    y <- as.character(input$cboYVar %||% "")[1]
    colour <- as.character(input$cboColorVar %||% "")[1]
    facet <- as.character(input$cboFacetVar %||% "")[1]
    def <- chart_definitions[[type]]
    if (identical(def$y, "none")) y <- ""
    if (type %in% c("Histogram", "Boxplot", "Violin", "Pie")) colour <- ""
    if (identical(type, "Pie")) facet <- ""
    error <- validate_request(df, type, x, y, colour, facet)
    if (!is.null(error)) return(list(ok = FALSE, error = error, plot = NULL))

    used <- unique(c(x, if (!blank(y)) y, if (!blank(colour)) colour, if (!blank(facet)) facet))
    d <- df[stats::complete.cases(df[, used, drop = FALSE]), , drop = FALSE]
    title <- as.character(input$txtPlotTitle %||% "")[1]
    xlab <- as.character(input$txtXlab %||% "")[1]
    ylab <- as.character(input$txtYlab %||% "")[1]
    single <- as.character(input$custom_single_color %||% input$cboColorSingle %||% "#1591a3")[1]
    palette <- as.character(input$cboColorBrewer %||% "Set1")[1]
    bar_width <- max(0.05, min(1, number(input$numBarWidth, 0.7)))
    line_size <- max(0.1, number(input$numLineSize, 1))
    line_type <- as.character(input$cboLineType %||% "solid")[1]
    point_shape <- suppressWarnings(as.integer(as.character(input$cboShapes %||% 16)[1]))
    if (is.na(point_shape)) point_shape <- 16L
    confidence_level <- max(0.5, min(0.999, number(input$numConfInt, 0.95)))
    vertical <- yes(input$rdoPltOrientation, TRUE)
    stacked <- yes(input$rdoStacked, TRUE)
    summary_type <- as.character(input$rdoSummaryTye %||% "Mean")[1]
    base <- ggplot2::ggplot(d)

    p <- switch(type,
      Bar = {
        if (blank(y)) {
          aes <- if (blank(colour)) ggplot2::aes(x = .data[[x]]) else ggplot2::aes(x = .data[[x]], fill = .data[[colour]])
          q <- base + ggplot2::geom_bar(aes, width = bar_width,
            fill = if (blank(colour)) single else NULL,
            position = if (stacked) "stack" else "dodge")
          y_default <- "Count"
        } else {
          fun <- switch(summary_type, Total = sum, Median = stats::median, Count = function(z, ...) length(z), mean)
          groups <- c(x, if (!blank(colour)) colour)
          agg <- stats::aggregate(d[[y]], d[groups], function(z) fun(z, na.rm = TRUE))
          names(agg)[ncol(agg)] <- ".value"
          aes <- if (blank(colour)) ggplot2::aes(x = .data[[x]], y = .data$.value) else ggplot2::aes(x = .data[[x]], y = .data$.value, fill = .data[[colour]])
          q <- ggplot2::ggplot(agg) + ggplot2::geom_col(
            aes, width = bar_width,
            fill = if (blank(colour)) single else NULL,
            position = if (stacked) "stack" else "dodge"
          )
          y_default <- paste(summary_type, y)
        }
        if (!vertical) q <- q + ggplot2::coord_flip()
        q + ggplot2::labs(y = if (blank(ylab)) y_default else ylab)
      },
      Histogram = {
        if (yes(input$rdoDensityOnly, FALSE)) {
          q <- base + ggplot2::geom_density(
            ggplot2::aes(x = .data[[x]]), fill = single,
            colour = "#1b4332", alpha = 0.45, linewidth = 1
          )
        } else {
          bw <- number(input$numBinWidth, NA_real_)
          hist_layer <- if (is.na(bw) || bw <= 0) {
            ggplot2::geom_histogram(ggplot2::aes(x = .data[[x]]), fill = single, colour = "white", bins = 30)
          } else {
            ggplot2::geom_histogram(ggplot2::aes(x = .data[[x]]), fill = single, colour = "white", binwidth = bw)
          }
          q <- base + hist_layer
          if (yes(input$rdoOverlayDensity, FALSE)) {
            q <- q + ggplot2::geom_density(
              ggplot2::aes(x = .data[[x]], y = ggplot2::after_stat(count)),
              colour = "#1b4332", linewidth = 1
            )
          }
        }
        q
      },
      Density = {
        if (blank(colour)) {
          base + ggplot2::geom_density(
            ggplot2::aes(x = .data[[x]]), fill = single,
            colour = "#1b4332", alpha = 0.45, linewidth = 1
          )
        } else {
          base + ggplot2::geom_density(
            ggplot2::aes(x = .data[[x]], fill = .data[[colour]],
                         colour = .data[[colour]]),
            alpha = 0.30, linewidth = 0.8
          )
        }
      },
      Scatterplot = {
        aes <- if (blank(colour)) ggplot2::aes(x = .data[[x]], y = .data[[y]]) else ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = .data[[colour]])
        q <- base + ggplot2::geom_point(aes,
          colour = if (blank(colour)) single else NULL,
          shape = point_shape, alpha = 0.8)
        smoother <- as.character(input$cboAddSmooth %||% "none")[1]
        if (smoother %in% c("lm", "loess")) q <- q + ggplot2::geom_smooth(
          aes, method = smoother, se = yes(input$rdoDisplaySeVal, TRUE),
          level = confidence_level, linewidth = line_size
        )
        q
      },
      Boxplot = {
        if (blank(y)) base + ggplot2::geom_boxplot(ggplot2::aes(x = "", y = .data[[x]]), fill = single) + ggplot2::labs(x = NULL)
        else base + ggplot2::geom_boxplot(ggplot2::aes(x = .data[[y]], y = .data[[x]], fill = .data[[y]]), show.legend = FALSE) + palette_scale("fill", palette, unique_n(d[[y]]))
      },
      Violin = base + ggplot2::geom_violin(ggplot2::aes(x = .data[[y]], y = .data[[x]], fill = .data[[y]]), trim = FALSE, show.legend = FALSE) + ggplot2::geom_boxplot(ggplot2::aes(x = .data[[y]], y = .data[[x]]), width = 0.12, outlier.shape = NA) + palette_scale("fill", palette, unique_n(d[[y]])),
      Line = {
        group <- if (blank(colour)) 1 else d[[colour]]
        aes <- if (blank(colour)) ggplot2::aes(x = .data[[x]], y = .data[[y]], group = group) else ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = .data[[colour]], group = .data[[colour]])
        q <- base + ggplot2::geom_line(aes, linewidth = line_size,
          linetype = line_type,
          lineend = as.character(input$cboLineJoin %||% "round")[1])
        if (yes(input$rdoAddPoints, FALSE)) q <- q + ggplot2::geom_point(aes)
        q
      },
      Pie = {
        counts <- as.data.frame(table(d[[x]], useNA = "no"), stringsAsFactors = FALSE)
        names(counts) <- c("category", "n")
        q <- ggplot2::ggplot(counts, ggplot2::aes(x = 2, y = .data$n, fill = .data$category)) + ggplot2::geom_col(width = 1) + ggplot2::coord_polar(theta = "y") + ggplot2::theme_void()
        if (yes(input$rdoTransformToDoug, TRUE)) q <- q + ggplot2::xlim(0.5, 2.5)
        q
      }
    )

    if (type %in% c("Boxplot", "Violin") && !vertical) {
      p <- p + ggplot2::coord_flip()
    }
    if (!blank(colour) && type %in% c("Bar", "Density", "Violin", "Pie")) p <- p + palette_scale("fill", palette, unique_n(d[[colour]]))
    if (!blank(colour) && type %in% c("Scatterplot", "Line")) p <- p + palette_scale("colour", palette, unique_n(d[[colour]]))
    if (type == "Pie") p <- p + palette_scale("fill", palette, unique_n(d[[x]]))
    if (!blank(facet) && type != "Pie") p <- p + ggplot2::facet_wrap(stats::as.formula(paste0("~`", facet, "`")))

    theme_name <- as.character(input$ggplot_theme %||% "theme_grey")[1]
    theme_fun <- if (exists(theme_name, envir = asNamespace("ggplot2"), mode = "function")) get(theme_name, envir = asNamespace("ggplot2")) else ggplot2::theme_grey
    p <- p + ggplot2::labs(title = title, x = if (blank(xlab)) x else xlab, y = if (blank(ylab)) ggplot2::waiver() else ylab, colour = input$txtLegend %||% "Legend", fill = input$txtLegend %||% "Legend") + theme_fun() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = number(input$numplotposition, 0.5), size = number(input$numplottitlesize, 24)), axis.title = ggplot2::element_text(size = number(input$numaxisTitleSize, 16)), axis.text = ggplot2::element_text(size = number(input$numAxistextSize, 12)), axis.text.x = ggplot2::element_text(angle = number(input$xaxistextangle, 0), hjust = if (number(input$xaxistextangle, 0) == 0) 0.5 else 1), strip.text = ggplot2::element_text(size = number(input$numfacettitlesize, 14)))
    list(ok = TRUE, error = NULL, plot = p)
  }

  observeEvent(input$btnCreatePlot, {
    req(identical(input$cboOutput %||% "Chart", "Chart"))
    shinyjs::disable("btnCreatePlot")
    on.exit(shinyjs::enable("btnCreatePlot"), add = TRUE)
    state$ready <- FALSE
    state$error <- NULL
    state$status <- paste("Generating", tolower(chart_type()), "plot...")

    result <- shiny::withProgress(
      message = paste("Generating", chart_type(), "plot"),
      detail = "Checking variables and chart settings...",
      value = 0,
      {
        shiny::setProgress(value = 0.10,
          detail = "Checking variables and chart settings...")
        result <- tryCatch({
          shiny::setProgress(value = 0.30,
            detail = "Building the chart...")
          built <- make_plot()
          shiny::setProgress(value = 0.90,
            detail = "Preparing the chart for display...")
          built
        }, error = function(e) {
          list(ok = FALSE, error = conditionMessage(e), plot = NULL)
        })
        shiny::setProgress(value = 1,
          detail = if (isTRUE(result$ok)) "Chart ready." else "Chart generation stopped.")
        result
      }
    )
    if (isTRUE(result$ok)) {
      plots_custom_rv$plot_rv <- result$plot
      state$ready <- TRUE
      state$generated <- TRUE
      state$error <- NULL
      state$status <- paste(chart_type(), "ready.")
    } else {
      plots_custom_rv$plot_rv <- NULL
      state$ready <- FALSE
      state$generated <- FALSE
      state$error <- paste("Plot could not be generated:", result$error)
    }
  }, ignoreInit = TRUE)

  output$GeneratedPlot <- renderPlot({
    req(identical(input$cboOutput %||% "Chart", "Chart"), state$ready, plots_custom_rv$plot_rv)
    plots_custom_rv$plot_rv
  })
  output$btnchartDown <- downloadHandler(
    filename = function() paste0(tolower(chart_type()), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpeg"),
    content = function(file) {
      req(state$ready, plots_custom_rv$plot_rv)
      ggplot2::ggsave(file, plot = plots_custom_rv$plot_rv, device = "jpeg", width = 16, height = 9, dpi = 300)
    }
  )
}
