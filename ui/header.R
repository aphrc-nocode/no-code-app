header = tags$div(
  class = "custom-header",
  style = "background-color: #7BC148; color: white; padding: 10px;",
  fluidRow(
    column(2, tags$img(src = "aphrc.png", height = "50px")),
    column(6, uiOutput("app_title", align="right")),
    column(2, uiOutput("change_language")),
    column(2,align="right",
           is_logged_in(
             id = app_login_config$APP_ID,
             style="display: flex; justify-content: flex-end; gap: 15px; margin-top: 20px;",
             
             br(),
             div(
               style = "display: flex; align-items: center; gap: 8px;",
               icon("user", style = "font-size: 16px; color: white;"),
               uiOutput("userName")
             )
             
          
           )
             
             )
          
    ))

