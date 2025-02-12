
fluidPage(
  fluidRow(
  h3(
    "This Section Allows the User to Generate Desired Output from the Selected Variables"
  )
),
hr(),


fluidRow(
  column(
    width = 2,
    radioButtons(
      "cboOutput",
      "Visual Type:",
      choices = c("Chart", "Table"),
      inline = TRUE
    )
  ),
  column(
    width = 10,
    radioGroupButtons(
      "btnChartType",
      justified = TRUE,
      choices = c(
        "Barplot" = "Bar",
        "Histogram" = "Histogram",
        "Scatterplot" = "Scatterplot",
        "Boxplot" = "Boxplot",
        "Lineplot" = "Line",
        "Violin plot" = "Violin",
        "Pie & Doughnut" = "Pie"
      ),
      selected = "Bar"
    ),
    align = "right"
  )
),


fluidRow(
  id = "tabOutputs",
  
  column(
    width = 3,
    
    h4("Data Options"),
    selectInput("cboSelectCountryTab", "Select Country:", choices = names(datalist)),
    selectInput("cboSelectProjectTab", "Select Project:", ""),
    selectInput("cboSelectDatasetTab", "Select Dataset:", ""),
    
    h3("Table Options"),
    br(),
    selectInput("cboCalcVar", "Select Measure/Calculation Variable:", "", multiple = TRUE),
    selectInput("cboRowVar", "Select Strata Variable:", ""),
    selectInput("cboColVar", "Select Row Variable Variable:", ""),
    
    br(),
    actionButton("btnCreatetable", "Create Cross Tab", width = "300px"),
    br(),
    br(),
    downloadButton("btnDownloadTable", "Download Table", style = "width:300px;")
  ),
  
  
  column(width = 7,
         gt::gt_output(outputId = "tabSummaries")),
  
  
  column(
    width = 2,
    h3("More Table Options"),
    br(),
    radioButtons(
      "chkReportNumeric",
      "Report Numeric:",
      choices = c("mean", "median"),
      selected = "mean",
      inline = FALSE,
      width = "100%"
    ),
    
    radioButtons(
      "chkNumericSummary",
      "Numeric Summary:",
      choices = c("sd", "min-max"),
      selected = "sd",
      inline = FALSE ,
      width = "100%",
    ),
    
    radioButtons(
      "rdoAddTabPValue",
      label = "Add P-value",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = FALSE,
      inline = TRUE
    ),
    radioButtons(
      "rdoAddTabCI",
      label = "Add Confidence Interval",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = FALSE,
      inline = TRUE
    ),
    radioButtons(
      "rdoDropTabMissingValues",
      label = "Drop Missing Values",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = TRUE,
      inline = TRUE
    ),
    
    
    textInput("txtTabCaption", label = "Table Caption")
    
    
    
  )
  
),

fluidRow(
  id = "graphOutputs",
  column(
    width = 3,
    h4("Data Options"),
    selectInput("cboSelectCountry", "Select Country:", choices = names(datalist)),
    selectInput("cboSelectProject", "Select Project:", ""),
    selectInput("cboSelectDataset", "Select Dataset:", ""),
    h4("Plot Options"),
    selectInput("cboXVar", "Select Variable on X Axis:", ""),
    selectInput("cboYVar", "Select Variable on Y Axis:", ""),
    textInput("txtPlotTitle", label = "Plot Title"),
    textInput("txtXlab", label = "X Axis Label"),
    textInput("txtYlab", label = "Y Axis Label"),
    br(),
    actionButton("btnchartOut", "Create", width = "300px"),
    br(),
    br(),
    downloadButton("btnchartDown", "Download", style = "width: 300px;")
  ),
  
  
  column(width = 7,
         plotOutput("GeneratedPlot", height = "80vh")),
  
  
  column(
    width = 2,
    h4("More Plot Options"),
    radioButtons(
      "rdoTransformToDoug",
      label = "Transform to Doughnut",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = TRUE,
      inline = TRUE
    ),
    selectInput("cboColorVar", "Select Color Variable:", ""),
    selectInput("cboFacetVar", "Select Group Variable:", ""),
    radioButtons(
      "rdoPltOrientation",
      label = "Visual Orientation",
      choices = c("Vertical" = TRUE, "Horizontal" = FALSE),
      selected = TRUE,
      inline = FALSE
    ),
    
    numericInput(
      inputId = "numBarWidth",
      label = "Bar Width",
      value = 0.6
    ),
    numericInput(
      inputId = "numBinWidth",
      label = "Bin Width",
      value = 10
    ),
    numericInput(
      inputId = "numLineSize",
      label = "Line Size",
      value = 1
    ),
    selectInput(
      "cboLineType",
      "Select Line Type:",
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
    ),
    
    radioButtons(
      "rdoAddShapes",
      label = "Add Shapes",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = TRUE,
      inline = TRUE
    ),
    
    selectInput(
      "cboShapes",
      "Select Shape:",
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
    ),
    
    selectInput(
      "cboAddSmooth",
      "Add Smooth:",
      choices =
        c("none"="none", "auto"= "loess", "lm"="lm", "glm"="glm", "loess" = "loess", "gam"="gam", "rlm" = "rlm"),
      selected = "auto"
    ),
    
    radioButtons(
      "rdoDisplaySeVal",
      label = "Display Confidence Interval",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = TRUE,
      inline = FALSE
    ),
    
    numericInput(
      inputId = "numConfInt",
      label = "Level of Confidence Interval",
      value = 0.95
    ),
    
    selectInput(
      "cboLineJoin",
      "Select Line Join:",
      choices =
        c("round", "mitre", "bevel"),
      selected = "round"
    ),
    
    radioButtons(
      "rdoAddLineType",
      label = "Add line Type",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = FALSE,
      inline = TRUE
    ),
    
    radioButtons(
      "rdoAddPoints",
      label = "Add Points",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = FALSE,
      inline = TRUE
    ),
    
    radioButtons(
      "rdoSummaryTye",
      label = "Y variable Summary Type",
      choices = c("Total" = "Total", "Average" = "Average"),
      selected = "Total",
      inline = TRUE
    ),
    
    numericInput(
      inputId = "numplotposition",
      label = "Title Position",
      value = 0.5
    ),
    numericInput(
      inputId = "numplottitlesize",
      label = "Size of Plot Title",
      value = 24
    ),
    numericInput(
      inputId = "numaxisTitleSize",
      label = "Axis Title Size",
      value = 20
    ),
    numericInput(
      inputId = "numfacettitlesize",
      label = "Facet Title Size",
      value = 20
    ),
    numericInput(
      inputId = "numAxistextSize",
      label = "Axis Text Size",
      value = 18
    ),
    numericInput(
      inputId = "numDataLabelSize",
      label = "Data Label Size",
      value = 6
    ),
    numericInput(
      inputId = "xaxistextangle",
      label = "X Axis Text Angle",
      value = 0
    ),
    textInput("txtLegend", label = "Legend Title", value = "Legend"),
    radioButtons(
      "rdoStacked",
      label = "Stacked",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = TRUE,
      inline = TRUE
    ),
    prettySwitch(
      "rdoOverlayDensity",
      label = "Add Density",
      value = FALSE
    ),
    prettySwitch(
      "rdoDensityOnly",
      label = "Remove Histogram",
      value = FALSE
    ),
    
    selectInput(
      "cboColorSingle",
      "Select Color",
      choices = colors(),
      selected = "blue"),
    
    selectInput(
      "cboColorBrewer",
      "Select Color Parlet:",
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
    
  )
  
),
hr(),
fluidRow(h4("View Data Dictionary", style = "text-align:center;")),
hr(),
fluidRow(dataTableOutput(outputId = "dataDictionary"))

)
