# ---------- helpers ----------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

rv_label <- function(key) {
  fallback <- c(
    user_output_type = "Output type", user_tab_options = "Table", user_calc_var = "Select table variables",
    user_row_variable = "Group table by", user_download_table = "Download table", user_table_options = "Table options",
    user_report_numeric = "Report numeric variables as", user_numeric_summary = "Numeric summary",
    user_add_p_value = "Add p-value", user_add_confidence_interval = "Add confidence interval",
    user_drop_missing_values = "Drop missing values", user_table_caption = "Table caption",
    user_plot_options = "Chart", user_more_plot_options = "Chart options",
    user_select_variable_on_x_axis = "Select variable on x axis", user_select_variable_on_y_axis = "Select variable on y axis",
    user_plot_title = "Plot title", user_x_axis_label = "X axis label", user_y_axis_label = "Y axis label",
    user_download = "Download", user_transform_to_doughnut = "Transform pie to doughnut",
    user_select_color_variable = "Color by variable", user_select_group_variable = "Group / wrap by variable", wrap_by_variable = "Group / wrap by variable",
    user_visual_orientation = "Orientation", user_bar_width = "Bar width", user_bin_width = "Bin width", user_line_size = "Line size",
    user_select_line_type = "Line type", user_add_shapes = "Add point shapes", user_select_shape = "Point shape",
    user_add_smooth = "Smooth line", user_display_confidence_interval = "Display confidence interval", user_level_of_confidence_interval = "Confidence level",
    user_select_line_join = "Line join", user_add_line_type = "Add line type", user_add_points = "Add points",
    user_y_variable_summary_type = "Y variable summary", user_title_position = "Title position", user_size_of_plot_title = "Plot title size",
    user_axis_title_size = "Axis title size", user_facet_title_size = "Facet title size", user_axis_text_size = "Axis text size",
    user_data_label_size = "Data label size", user_x_axis_text_angle = "X axis text angle", user_legend_title = "Legend title",
    user_stacked = "Stack bars", user_add_density = "Add density", user_remove_histogram = "Show density only",
    user_select_color_variable_single = "Single color", color_single = "Single color", user_select_color_parlet = "Color palette", user_ggthemes = "Chart theme",
    select_variable = "Select variable", select_one_or_more_variables = "Select one or more variables", show_more_details = "Show More Details", hide_details = "Hide Details"
  )
  val <- NULL
  if (exists("get_rv_labels", mode = "function", inherits = TRUE)) val <- tryCatch(get_rv_labels(key), error = function(e) NULL)
  if (!is.null(val) && length(val) > 0 && !is.na(val[1]) && nzchar(trimws(as.character(val[1])))) return(as.character(val[1]))
  if (key %in% names(fallback)) fallback[[key]] else key
}


non_numeric_df <- function(df) {
  final_df <- df[, sapply(df, FUN = function(x) {
    is.character(x) || is.factor(x) || is.logical(x) || is.Date(x)
  }), drop = FALSE]
  return(final_df)
}

numeric_df <- function(df) {
  final_df <- df[, sapply(df, FUN = function(x) {
    is.numeric(x) || is.integer(x)
  }), drop = FALSE]
  return(final_df)
}

non_numeric_non_date_df <- function(df) {
  final_df <- df[, sapply(df, FUN = function(x) {
    is.character(x) || is.factor(x) || is.logical(x)
  }), drop = FALSE]
  return(final_df)
}

select_placeholder <- function(label = rv_label("select_variable")) {
  out <- list("")
  names(out) <- label
  out
}


rv_choices <- function(key) {
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
  if (exists("get_rv_choices", mode = "function")) {
    vals <- get_rv_choices(key)
    if (!is.null(vals) && length(vals) > 0) return(vals)
  }
  default_rv_choices(key)
}

# ---- compatibility stubs for old server references ----
# remove these later after server cleanup is complete

bivariate_header_label = renderUI({ NULL })
corrplot_header_label = renderUI({ NULL })
user_select_bivariate_single_color = renderUI({ NULL })
user_select_color_parlet_bivariate = renderUI({ NULL })
user_select_color_parlet_corrplot = renderUI({ NULL })
bivariate_plot_title = renderUI({ NULL })
corrplot_title = renderUI({ NULL })
user_download_autoreport = renderUI({ NULL })
user_generatebivriate = renderUI({ NULL })
user_select_corr_features = renderUI({ NULL })
user_select_bivariate_outcome = renderUI({ NULL })

# ---------- output type ----------

user_output_type = renderUI({
  radioButtons(
    "cboOutput",
    paste0(rv_label("user_output_type"), ":"),
    choices = { x <- rv_choices("user_output_type"); if (length(x) == 0) c(Chart = "Chart", Table = "Table") else x },
    selected = "Chart",
    inline = TRUE
  )
})

# ---------- table / crosstab ----------

user_tab_options = renderUI({
  h3(rv_label("user_tab_options"))
})

user_calc_var = renderUI({
  selectizeInput(
    inputId = "cboCalcVar",
    label = paste(rv_label("user_calc_var"), ":"),
    choices = NULL,
    selected = NULL,
    multiple = TRUE,
    options = list(
      placeholder = rv_label("select_one_or_more_variables"),
      plugins = list("remove_button"),
      maxItems = 5,
      closeAfterSelect = TRUE
    )
  )
})

user_row_var = renderUI({
  selectizeInput(
    inputId = "cboColVar",
    label = paste(rv_label("user_row_variable"), ":"),
    choices = NULL,
    selected = NULL,
    multiple = FALSE,
    options = list(
      placeholder = rv_label("select_variable")
    )
  )
})

# manual create removed; table is automated
usr_create_cross_tab = renderUI({
  NULL
})

user_download_table = renderUI({
  downloadBttn(
    "btnDownloadTable",
    rv_label("user_download_table"),
    block = FALSE,
    size = "md",
    color = "success"
  )
})

user_table_options = renderUI({
  h3(rv_label("user_table_options"))
})

user_report_numeric <- renderUI({
  radioButtons(
    "chkReportNumeric",
    label = paste0(rv_label("user_report_numeric"), ":"),
    choices = rv_choices("user_report_numeric"),
    selected = "mean",
    inline = FALSE,
    width = "100%"
  )
})

user_numeric_summary <- renderUI({
  radioButtons(
    "chkNumericSummary",
    label = paste0(rv_label("user_numeric_summary"), ":"),
    choices = rv_choices("user_numeric_summary"),
    selected = "sd",
    inline = FALSE,
    width = "100%"
  )
})

user_add_p_value <- renderUI({
  radioButtons(
    "rdoAddTabPValue",
    label = rv_label("user_add_p_value"),
    choices = rv_choices("yes_no_boolean"),
    selected = FALSE,
    inline = TRUE
  )
})

user_add_confidence_interval <- renderUI({
  radioButtons(
    "rdoAddTabCI",
    label = rv_label("user_add_confidence_interval"),
    choices = rv_choices("yes_no_boolean"),
    selected = FALSE,
    inline = TRUE
  )
})

user_drop_missing_values <- renderUI({
  radioButtons(
    "rdoDropTabMissingValues",
    label = rv_label("user_drop_missing_values"),
    choices = rv_choices("yes_no_boolean"),
    selected = TRUE,
    inline = TRUE
  )
})

user_table_caption <- renderUI({
  textInput("txtTabCaption", label = rv_label("user_table_caption"))
})

# ---------- chart ----------

user_plot_options = renderUI({
  h3(rv_label("user_plot_options"))
})

user_select_variable_on_x_axis = renderUI({
  selectInput(
    "cboXVar",
    paste0(rv_label("user_select_variable_on_x_axis"), ":"),
    choices = c(select_placeholder(rv_label("select_variable"))),
    selected = ""
  )
})

user_select_variable_on_y_axis = renderUI({
  selectInput(
    "cboYVar",
    paste0(rv_label("user_select_variable_on_y_axis"), ":"),
    choices = c(select_placeholder(rv_label("select_variable"))),
    selected = ""
  )
})

user_plot_title = renderUI({
  textInput("txtPlotTitle", label = rv_label("user_plot_title"))
})

user_x_axis_label = renderUI({
  textInput("txtXlab", label = rv_label("user_x_axis_label"))
})

user_y_axis_label = renderUI({
  textInput("txtYlab", label = rv_label("user_y_axis_label"))
})

# manual create removed; chart is auto-generated
user_create = renderUI({
  NULL
})

user_download = renderUI({
  downloadBttn(
    "btnchartDown",
    rv_label("user_download"),
    block = FALSE,
    size = "md",
    color = "success"
  )
})

# chart type output placeholder expected by server
user_chart_type = renderUI({
  NULL
})

# ---------- advanced chart controls ----------

user_more_plot_options = renderUI({
  h3(rv_label("user_more_plot_options"))
})

user_transform_to_doughnut = renderUI({
  radioButtons(
    "rdoTransformToDoug",
    label = rv_label("user_transform_to_doughnut"),
    choices = rv_choices("yes_no_boolean"),
    selected = TRUE,
    inline = TRUE
  )
})

user_select_color_variable = renderUI({
  selectInput(
    "cboColorVar",
    paste0(rv_label("user_select_color_variable"), ":"),
    choices = c(select_placeholder(rv_label("select_variable"))),
    selected = ""
  )
})

user_select_group_variable = renderUI({
  selectInput(
    "cboFacetVar",
    paste0(rv_label("user_select_group_variable"), ":"),
    choices = c(select_placeholder(rv_label("select_variable"))),
    selected = ""
  )
})

user_visual_orientation = renderUI({
  radioButtons(
    "rdoPltOrientation",
    label = rv_label("user_visual_orientation"),
    choices = rv_choices("user_visual_orientation"),
    selected = TRUE,
    inline = FALSE
  )
})

user_bar_width = renderUI({
  numericInput(
    inputId = "numBarWidth",
    label = rv_label("user_bar_width"),
    value = 0.6
  )
})

user_bin_width = renderUI({
  numericInput(
    inputId = "numBinWidth",
    label = rv_label("user_bin_width"),
    value = 10
  )
})

user_line_size = renderUI({
  numericInput(
    inputId = "numLineSize",
    label = rv_label("user_line_size"),
    value = 1
  )
})

user_select_line_type = renderUI({
  selectInput(
    "cboLineType",
    paste0(rv_label("user_select_line_type"), ":"),
    choices = rv_choices("user_select_line_type"),
    selected = "solid"
  )
})

user_add_shapes = renderUI({
  radioButtons(
    "rdoAddShapes",
    label = rv_label("user_add_shapes"),
    choices = rv_choices("yes_no_boolean"),
    selected = TRUE,
    inline = TRUE
  )
})

user_select_shape = renderUI({
  selectInput(
    "cboShapes",
    paste0(rv_label("user_select_shape"), ":"),
    choices = rv_choices("user_select_shape"),
    selected = 16
  )
})

user_add_smooth = renderUI({
  selectInput(
    "cboAddSmooth",
    paste0(rv_label("user_add_smooth"), ":"),
    choices = rv_choices("user_add_smooth"),
    selected = "loess"
  )
})

user_display_confidence_interval = renderUI({
  radioButtons(
    "rdoDisplaySeVal",
    label = rv_label("user_display_confidence_interval"),
    choices = rv_choices("yes_no_boolean"),
    selected = TRUE,
    inline = FALSE
  )
})

user_level_of_confidence_interval = renderUI({
  numericInput(
    inputId = "numConfInt",
    label = rv_label("user_level_of_confidence_interval"),
    value = 0.95
  )
})

user_select_line_join = renderUI({
  selectInput(
    "cboLineJoin",
    paste0(rv_label("user_select_line_join"), ":"),
    choices = rv_choices("user_select_line_join"),
    selected = "round"
  )
})

user_add_line_type = renderUI({
  radioButtons(
    "rdoAddLineType",
    label = rv_label("user_add_line_type"),
    choices = rv_choices("yes_no_boolean"),
    selected = FALSE,
    inline = TRUE
  )
})

user_add_points = renderUI({
  radioButtons(
    "rdoAddPoints",
    label = rv_label("user_add_points"),
    choices = rv_choices("yes_no_boolean"),
    selected = FALSE,
    inline = TRUE
  )
})

user_y_variable_summary_type = renderUI({
  radioButtons(
    "rdoSummaryTye",
    label = rv_label("user_y_variable_summary_type"),
    choices = rv_choices("user_y_variable_summary_type"),
    selected = "Total",
    inline = TRUE
  )
})

user_title_position = renderUI({
  numericInput(
    inputId = "numplotposition",
    label = rv_label("user_title_position"),
    value = 0.5
  )
})

user_size_of_plot_title = renderUI({
  numericInput(
    inputId = "numplottitlesize",
    label = rv_label("user_size_of_plot_title"),
    value = 24
  )
})

user_axis_title_size = renderUI({
  numericInput(
    inputId = "numaxisTitleSize",
    label = rv_label("user_axis_title_size"),
    value = 20
  )
})

user_facet_title_size = renderUI({
  numericInput(
    inputId = "numfacettitlesize",
    label = rv_label("user_facet_title_size"),
    value = 20
  )
})

user_axis_text_size = renderUI({
  numericInput(
    inputId = "numAxistextSize",
    label = rv_label("user_axis_text_size"),
    value = 18
  )
})

user_data_label_size = renderUI({
  numericInput(
    inputId = "numDataLabelSize",
    label = rv_label("user_data_label_size"),
    value = 6
  )
})

user_x_axis_text_angle = renderUI({
  numericInput(
    inputId = "xaxistextangle",
    label = rv_label("user_x_axis_text_angle"),
    value = 0
  )
})

user_legend_title = renderUI({
  textInput(
    "txtLegend",
    label = rv_label("user_legend_title"),
    value = "Legend"
  )
})

user_stacked = renderUI({
  radioButtons(
    "rdoStacked",
    label = rv_label("user_stacked"),
    choices = rv_choices("yes_no_boolean"),
    selected = TRUE,
    inline = TRUE
  )
})

user_add_density = renderUI({
  prettySwitch(
    "rdoOverlayDensity",
    label = rv_label("user_add_density"),
    value = FALSE
  )
})

user_remove_histogram = renderUI({
  prettySwitch(
    "rdoDensityOnly",
    label = rv_label("user_remove_histogram"),
    value = FALSE
  )
})

user_select_color_variable_single = renderUI({
  if (requireNamespace("colourpicker", quietly = TRUE)) {
    colourpicker::colourInput(
      inputId = "custom_single_color",
      label = rv_label("user_select_color_variable_single"),
      value = "#1591a3",
      allowTransparent = FALSE,
      showColour = "both",
      palette = "square"
    )
  } else {
    selectInput(
      "cboColorSingle",
      rv_label("user_select_color_variable_single"),
      choices = rv_choices("r_colors"),
      selected = "#1591a3"
    )
  }
})

user_select_color_parlet = renderUI({
  selectInput(
    "cboColorBrewer",
    paste0(rv_label("user_select_color_parlet"), ":"),
    choices = rv_choices("user_select_color_parlet"),
    selected = "Set1"
  )
})

# ---------- dynamic detail toggle placeholders ----------
# server fills these

user_graph_more_out = renderUI({
  NULL
})

user_tab_more_out = renderUI({
  NULL
})

# ---------- main custom visualization layout ----------
# use uiOutput("user_custom_visualization_layout") in your page

user_custom_visualization_layout = renderUI({
  tagList(
    tags$style(HTML("
      #graphmoreoption, #tabmoreoption {
        overflow: hidden;
        max-height: 0;
        opacity: 0;
        transform: translateY(-4px);
        pointer-events: none;
      }
      #graphmoreoption.open-panel, #tabmoreoption.open-panel {
        max-height: 74vh;
        opacity: 1;
        transform: translateY(0);
        pointer-events: auto;
      }
    ")),
    tags$div(
      id = "Divcustomvisiz",
      style = "width:100%;",
      
      uiOutput("user_output_type"),
      
      tags$div(
        id = "graphOutputs",
        style = "width:100%;",
        
        uiOutput("user_plot_options"),
        uiOutput("user_select_variable_on_x_axis"),
        uiOutput("user_chart_type"),
        uiOutput("user_select_variable_on_y_axis"),
        uiOutput("user_plot_title"),
        uiOutput("user_x_axis_label"),
        uiOutput("user_y_axis_label"),
        uiOutput("user_download"),
        conditionalPanel(
          condition = "input.cboOutput == 'Chart'",
          uiOutput("user_graph_more_out"),
          tags$div(
            id = "graphmoreoption",
            uiOutput("user_more_plot_options"),
            uiOutput("user_transform_to_doughnut"),
            uiOutput("user_select_color_variable"),
            uiOutput("user_select_group_variable"),
            uiOutput("user_visual_orientation"),
            uiOutput("user_bar_width"),
            uiOutput("user_bin_width"),
            uiOutput("user_line_size"),
            uiOutput("user_select_line_type"),
            uiOutput("user_add_shapes"),
            uiOutput("user_select_shape"),
            uiOutput("user_add_smooth"),
            uiOutput("user_display_confidence_interval"),
            uiOutput("user_level_of_confidence_interval"),
            uiOutput("user_select_line_join"),
            uiOutput("user_add_line_type"),
            uiOutput("user_add_points"),
            uiOutput("user_y_variable_summary_type"),
            uiOutput("user_title_position"),
            uiOutput("user_size_of_plot_title"),
            uiOutput("user_axis_title_size"),
            uiOutput("user_facet_title_size"),
            uiOutput("user_axis_text_size"),
            uiOutput("user_data_label_size"),
            uiOutput("user_x_axis_text_angle"),
            uiOutput("user_legend_title"),
            uiOutput("user_stacked"),
            uiOutput("user_add_density"),
            uiOutput("user_remove_histogram"),
            uiOutput("user_select_color_variable_single"),
            uiOutput("user_select_color_parlet"),
            uiOutput("user_ggthemes")
          )
        ),
        
        plotOutput("GeneratedPlot", width = "100%")
      ),
      
      tags$div(
        id = "tabOutputs",
        style = "width:100%; display:none;",
        
        uiOutput("user_tab_options"),
        uiOutput("user_calc_var"),
        uiOutput("user_row_var"),
        uiOutput("user_download_table"),
        conditionalPanel(
          condition = "input.cboOutput == 'Table'",
          uiOutput("user_tab_more_out"),
          tags$div(
            id = "tabmoreoption",
            uiOutput("user_table_options"),
            uiOutput("user_report_numeric"),
            uiOutput("user_numeric_summary"),
            uiOutput("user_add_p_value"),
            uiOutput("user_add_confidence_interval"),
            uiOutput("user_drop_missing_values"),
            uiOutput("user_table_caption")
          )
        ),
        
        htmlOutput("tabSummaries")
      )
    )
  )
})