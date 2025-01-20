header = tags$div(
  class = "custom-header",
  style = "background-color: #7BC148; color: white; padding: 10px;",
  fluidRow(
    column(2, tags$img(src = "aphrc.png", height = "50px")),
    column(8, h3("APHRC AUTOML NO CODE PLATFORM", style = "text-align: center;")),
    column(2, uiOutput("change_language")))
)