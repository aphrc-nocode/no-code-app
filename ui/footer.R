footer =
  customFooter <- tags$div(
    class = "custom-footer",
    style = "background-color: #7BC148; color: white; padding: 10px;",
    br(),
    fluidRow(
      column(4,uiOutput("app_footer_title")),
      column(4,uiOutput("app_footer_contact")),
      column(4,uiOutput("app_footer_all_rights"))
    ),
    br()
  )

