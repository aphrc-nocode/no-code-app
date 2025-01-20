footer =
  customFooter <- tags$div(
    class = "custom-footer",
    style = "background-color: #7BC148; color: white; padding: 10px;",
    br(),
    fluidRow(
      column(4,h4( "African Population and Health Research Center")),
      column(4, h4(HTML('<a href="mailto:example@aphrc.org" style="color: white;">Contact Us: example@aphrc.org</a>'))),
      column(4, h4(paste0("Copyright © ", format(Sys.Date(), "%Y"), ", APHRC, All Rights Reserved"), style = "text-align: right;"))
    ),
    br()
  )