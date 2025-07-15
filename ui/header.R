header = tags$div(
  class = "custom-header",
  style = "background-color: #7BC148; color: white; padding: 10px;",
  fluidRow(
    column(2, tags$img(src = "aphrc.png", height = "50px")),
    column(8, uiOutput("app_title")),
    column(2, uiOutput("change_language")))
)
