
#This Function only returns non numeric dataframe
non_numric_df <- function(df){
  final_df <- df[,sapply(df, FUN =  function(x){is.character(x)||is.factor(x)||is.logical(x)||is.Date(x)})]
  return(final_df)
}

#This Function only returns numeric dataframe
numeric_df <- function(df){
  final_df <- df[,sapply(df, FUN =  function(x){is.numeric(x)||is.integer(x)})]
  return(final_df)
}

#This Function only returns numeric dataframe
non_numric_non_date_df <- function(df){
  final_df <- df[,sapply(df, FUN =  function(x){is.character(x)||is.factor(x)||is.logical(x)})]
  return(final_df)
}



user_output_type = renderUI({
  radioButtons(
    "cboOutput",
    paste0(get_rv_labels("user_output_type"), ":"),
    choices = c("Chart", "Table"), selected = "Chart",
    inline = TRUE
  )
})


user_tab_options =renderUI({
  h3(
    get_rv_labels(
      "user_tab_options"
    )
  )
})


user_calc_var = renderUI({
  selectInput("cboCalcVar", paste(get_rv_labels("user_calc_var"),":"), "", multiple = TRUE)
})



user_row_var = renderUI({
  selectInput("cboColVar", paste(get_rv_labels("user_row_variable"),":"), "")
})

usr_create_cross_tab = renderUI({
      actionBttn(
        inputId = "btnCreatetable", 
        label = get_rv_labels("usr_create_cross_tab"), 
        size = "md",
        inline = TRUE,
        block = FALSE,
        color = "success"
      
  )
})

user_download_table = renderUI({
      downloadBttn("btnDownloadTable", get_rv_labels("user_download_table")
                   , block = FALSE , size = "md"
                   , color = "success" )
})

user_table_options =renderUI({
  h3(
    get_rv_labels(
      "user_table_options"
    )
  )
})

user_report_numeric <- renderUI({
  radioButtons(
    "chkReportNumeric",
    label = paste0(get_rv_labels("user_report_numeric"), ":"),
    choices = c("mean", "median"),
    selected = "mean",
    inline = FALSE,
    width = "100%"
  )
})

user_numeric_summary <- renderUI({
  radioButtons(
  "chkNumericSummary",
  label = paste0(get_rv_labels("user_numeric_summary"), ":"),
  choices = c("sd", "min-max"),
  selected = "sd",
  inline = FALSE ,
  width = "100%",
)})

user_add_p_value <- renderUI({radioButtons(
  "rdoAddTabPValue",
  label =   get_rv_labels("user_add_p_value"),
  choices = c("Yes" = TRUE, "No" = FALSE),
  selected = FALSE,
  inline = TRUE
)})

user_add_confidence_interval <- renderUI({radioButtons(
  "rdoAddTabCI",
  label = get_rv_labels("user_add_confidence_interval"),
  choices = c("Yes" = TRUE, "No" = FALSE),
  selected = FALSE,
  inline = TRUE
)})

user_drop_missing_values <- renderUI({radioButtons(
  "rdoDropTabMissingValues",
  label = get_rv_labels("user_drop_missing_values"),
  choices = c("Yes" = TRUE, "No" = FALSE),
  selected = TRUE,
  inline = TRUE
)})


user_table_caption <- renderUI({
  textInput("txtTabCaption", label = get_rv_labels("user_table_caption"))})

user_plot_options =renderUI({
  h3(get_rv_labels("user_plot_options"))
  })

user_select_variable_on_x_axis =renderUI({
  selectInput("cboXVar", paste0(
    get_rv_labels("user_select_variable_on_x_axis")
    ,":"), "")
})
user_select_variable_on_y_axis =renderUI({
  selectInput("cboYVar",  paste0(
    get_rv_labels("user_select_variable_on_y_axis")
    ,":"), "")
})

user_plot_title =renderUI({
  textInput("txtPlotTitle", label = get_rv_labels("user_plot_title"))
})

user_x_axis_label =renderUI({
  textInput("txtXlab", label = get_rv_labels("user_x_axis_label"))
})

user_y_axis_label =renderUI({
  textInput("txtYlab", label = get_rv_labels("user_y_axis_label"))
})

user_create =renderUI({
      actionBttn(
        inputId = "btnchartOut", 
        label = get_rv_labels("user_create"), 
        size = "md",
        inline = TRUE,
        block = FALSE,
        color = "success"
  )
})

user_download =renderUI({
  downloadBttn("btnchartDown", get_rv_labels("user_download")
               , block = FALSE , size = "md"
               , color = "success" )
})



user_more_plot_options =renderUI({
  h3(get_rv_labels("user_more_plot_options"))
})

user_transform_to_doughnut =renderUI({
  radioButtons(
  "rdoTransformToDoug",
  label = get_rv_labels("user_transform_to_doughnut"),
  choices = c("Yes" = TRUE, "No" = FALSE),
  selected = TRUE,
  inline = TRUE
)
  })

user_select_color_variable =renderUI({
  selectInput("cboColorVar", 
    paste0(
      get_rv_labels("user_select_color_variable"),
  ":"), "", selected = NULL)
})
user_select_group_variable =renderUI({
  selectInput("cboFacetVar",  paste0(
    get_rv_labels("user_select_group_variable"),
    ":"), "")
})

user_visual_orientation =renderUI({
  radioButtons(
  "rdoPltOrientation",
  label = get_rv_labels("user_visual_orientation"),
  choices = c("Vertical" = TRUE, "Horizontal" = FALSE),
  selected = TRUE,
  inline = FALSE
)
})

user_bar_width =renderUI({
  numericInput(
  inputId = "numBarWidth",
  label = get_rv_labels("user_bar_width"),
  value = 0.6
)})

user_bin_width =renderUI({
  numericInput(
  inputId = "numBinWidth",
  label = get_rv_labels("user_bin_width"),
  value = 10
)})

user_line_size = renderUI({
  numericInput(
  inputId = "numLineSize",
  label = get_rv_labels("user_line_size"),
  value = 1
)
})

user_select_line_type =renderUI({
  selectInput(
  "cboLineType",
  paste0(
    get_rv_labels("user_select_line_type")
    ,
  ":"),
  choices =
    c(
      "blank",
      "solid",
      "dashed",
      "dotted",
      "dotdash",
      "longdash",
      "twodash",
      "1F",
      "F1",
      "4C88C488",
      "12345678"
    ),
  selected = "solid"
)
})

user_add_shapes =renderUI({
  radioButtons(
  "rdoAddShapes",
  label = get_rv_labels("user_add_shapes"),
  choices = c("Yes" = TRUE, "No" = FALSE),
  selected = TRUE,
  inline = TRUE
)})

user_select_shape =renderUI({
  selectInput(
  "cboShapes",
  paste0(
    get_rv_labels("user_select_shape")
    ,
    ":")
  ,
  choices =
    c(
      "square" = 0,
      "circle" = 1,
      "triangle point up" = 2,
      "plus" = 3,
      "cross" = 4,
      "diamond" = 5,
      "triangle point down" = 6,
      "square cross" = 7,
      "star" = 8,
      "diamond plus" = 9,
      "circle plus" = 10,
      "triangles up and down" = 11,
      "square plus" = 12,
      "circle cross" = 13,
      "square and triangle down" = 14,
      "filled square" = 15,
      "filled circle" = 16,
      "filled triangle point-up" = 17,
      "filled diamond" = 18,
      "solid circle" = 19,
      "bullet (smaller circle)" = 20,
      "filled circle blue" = 21,
      "filled square blue" = 22,
      "filled diamond blue" = 23,
      "filled triangle point-up blue" = 24,
      "filled triangle point down blue" = 25
    ),
  selected = 1
)
})

user_add_smooth = renderUI({
  selectInput(
  "cboAddSmooth",
  paste0(
    get_rv_labels("user_add_smooth")
    ,
    ":"),
  choices =
    c("none"="none", "auto"= "loess", "lm"="lm", "glm"="glm", "loess" = "loess", "gam"="gam", "rlm" = "rlm"),
  selected = "auto"
)})

user_display_confidence_interval =renderUI({
  radioButtons(
  "rdoDisplaySeVal",
  label = get_rv_labels("user_display_confidence_interval"),
  choices = c("Yes" = TRUE, "No" = FALSE),
  selected = TRUE,
  inline = FALSE
)})

user_level_of_confidence_interval=renderUI({
  numericInput(
  inputId = "numConfInt",
  label = get_rv_labels("user_level_of_confidence_interval"),
  value = 0.95
)})

user_tab_more_out=renderUI({
  switchInput(
    inputId = "tabmore",
    label = NULL,
    value = FALSE,
    onLabel = "Hide options",
    offLabel = "Show options",
    onStatus ="#7bc148"
    
  )})


user_graph_more_out=renderUI({
  switchInput(
    inputId = "graphmore",
    label = NULL,
    value = FALSE,
    onStatus ="#7bc148",
    onLabel = "Hide options",
    offLabel = "Show options"
    
  )})

user_select_line_join =renderUI({
  selectInput(
  "cboLineJoin",
  paste0(
    get_rv_labels("user_select_line_join")
    ,
    ":"),
  choices =
    c("round", "mitre", "bevel"),
  selected = "round"
)})

user_add_line_type =renderUI({
  radioButtons(
  "rdoAddLineType",
  label = get_rv_labels("user_add_line_type"),
  choices = c("Yes" = TRUE, "No" = FALSE),
  selected = FALSE,
  inline = TRUE
)
})

user_add_points =renderUI({
  radioButtons(
  "rdoAddPoints",
  label = get_rv_labels("user_add_points"),
  choices = c("Yes" = TRUE, "No" = FALSE),
  selected = FALSE,
  inline = TRUE
)
})

user_y_variable_summary_type =renderUI({
  radioButtons(
  "rdoSummaryTye",
  label = get_rv_labels("user_y_variable_summary_type"),
  choices = c("Total" = "Total", "Average" = "Average"),
  selected = "Total",
  inline = TRUE
)
  })

user_title_position =renderUI({
  numericInput(
  inputId= "numplotposition",
  label = get_rv_labels("user_title_position"),
  value = 0.5
)
})

user_size_of_plot_title =renderUI({
  numericInput(
  inputId = "numplottitlesize",
  label =  get_rv_labels("user_size_of_plot_title"),
  value = 24
)
})

user_axis_title_size = renderUI({
  numericInput(
  inputId = "numaxisTitleSize",
  label = get_rv_labels("user_axis_title_size"),
  value = 20
)
})

user_facet_title_size =renderUI({
  numericInput(
  inputId = "numfacettitlesize",
  label = get_rv_labels("user_facet_title_size"),
  value = 20
)
})

user_axis_text_size =renderUI({
  numericInput(
  inputId = "numAxistextSize",
  label = get_rv_labels("user_axis_text_size"),
  value = 18
)
})

user_data_label_size =renderUI({
  numericInput(
  inputId = "numDataLabelSize",
  label = get_rv_labels("user_data_label_size"),
  value = 6
)
})

user_x_axis_text_angle =renderUI({
  numericInput(
  inputId = "xaxistextangle",
  label = get_rv_labels("user_x_axis_text_angle"),
  value = 0
)
})

user_legend_title =renderUI({
  textInput("txtLegend", 
            label = get_rv_labels("user_legend_title"),
            value = "Legend")
})

user_stacked =renderUI({
  radioButtons(
  "rdoStacked",
  label = get_rv_labels("user_stacked"),
  choices = c("Yes" = TRUE, "No" = FALSE),
  selected = TRUE,
  inline = TRUE
)
})

user_add_density =renderUI({
  prettySwitch(
  "rdoOverlayDensity",
  label = get_rv_labels("user_add_density"),
  value = FALSE
)
})

user_remove_histogram =renderUI({
  prettySwitch(
  "rdoDensityOnly",
  label = get_rv_labels("user_remove_histogram"),
  value = FALSE
)})


user_select_color_variable_single =renderUI({
  selectInput(
  "cboColorSingle",
  get_rv_labels("color_single"),
  choices = colors(),
  selected = "blue")
})

user_select_color_parlet =renderUI({
  selectInput(
    "cboColorBrewer",
    paste0(get_rv_labels("user_select_color_parlet"), ":"),
    choices =
      c(
        "Accent",
        "Dark2",
        "Paired",
        "Pastel1",
        "Pastel2",
        "Set1",
        "Set2",
        "Set3",
        "BrBG",
        "PiYG",
        "PRGn",
        "PuOr",
        "RdBu",
        "RdGy",
        "RdYlBu",
        "RdYlGn",
        "Spectral",
        "Blues",
        "BuGn",
        "BuPu",
        "GnBu",
        "Greens",
        "Greys",
        "Oranges",
        "OrRd",
        "PuBu",
        "PuBuGn",
        "PuRd",
        "Purples",
        "RdPu",
        "Reds",
        "YlGn",
        "YlGnBu",
        "YlOrBr",
        "YlOrRd"
      ),
    selected = "Dark2"
  )
})
  
  user_select_color_parlet_corrplot =renderUI({
    selectInput(
      "cboColorBrewerCorrplot",
      paste0(get_rv_labels("user_select_color_parlet"), ":"),
      choices =
        c(
          "Accent",
          "Dark2",
          "Paired",
          "Pastel1",
          "Pastel2",
          "Set1",
          "Set2",
          "Set3",
          "BrBG",
          "PiYG",
          "PRGn",
          "PuOr",
          "RdBu",
          "RdGy",
          "RdYlBu",
          "RdYlGn",
          "Spectral",
          "Blues",
          "BuGn",
          "BuPu",
          "GnBu",
          "Greens",
          "Greys",
          "Oranges",
          "OrRd",
          "PuBu",
          "PuBuGn",
          "PuRd",
          "Purples",
          "RdPu",
          "Reds",
          "YlGn",
          "YlGnBu",
          "YlOrBr",
          "YlOrRd"
        ),
      selected = "Dark2"
    )
    
  })
    
    
    user_select_color_parlet_bivariate =renderUI({
      selectInput(
        "cboColorBrewerBivariate",
        paste0(get_rv_labels("user_select_color_parlet"), ":"),
        choices =
          c(
            "Accent",
            "Dark2",
            "Paired",
            "Pastel1",
            "Pastel2",
            "Set1",
            "Set2",
            "Set3",
            "BrBG",
            "PiYG",
            "PRGn",
            "PuOr",
            "RdBu",
            "RdGy",
            "RdYlBu",
            "RdYlGn",
            "Spectral",
            "Blues",
            "BuGn",
            "BuPu",
            "GnBu",
            "Greens",
            "Greys",
            "Oranges",
            "OrRd",
            "PuBu",
            "PuBuGn",
            "PuRd",
            "Purples",
            "RdPu",
            "Reds",
            "YlGn",
            "YlGnBu",
            "YlOrBr",
            "YlOrRd"
          ),
        selected = "Dark2"
      )
})
    
user_select_bivariate_single_color =renderUI({
  selectInput("cboBivariateColor", paste0(
    get_rv_labels("bivariate_single_color")
    ,":"), choices = colors(distinct =TRUE),
    selected = "forestgreen")
})  

corrplot_header_label =renderUI({
  h4(
    get_rv_labels(
      "corrplot_header_lebel"
    )
  )
})

bivariate_header_label =renderUI({
  h4(
    get_rv_labels(
      "bivariate_header_label"
    )
  )
})


bivariate_plot_title =renderUI({
  textInput(
    inputId= "txtPlotBivariateTitle",
    label = get_rv_labels("bivariate_plot_title")
  )
})


corrplot_title =renderUI({
  textInput(
    inputId= "txtPlotCorrTitle",
    label = get_rv_labels("bivariate_plot_title")
  )
})


user_download_autoreport = renderUI({
  actionBttn(inputId = "btnDownloadReportAuto", get_rv_labels("user_download_autoreport"),
             size = "md",
             inline = TRUE,
             block = FALSE,
             color = "success"
  )
})


user_generatebivriate = renderUI({
  actionBttn(inputId = "btnGenerateBivariate", get_rv_labels("user_generatebivriate"),
             size = "md",
             inline = TRUE,
             block = FALSE,
             color = "success"
  )
})


    




    