footer_styles <- tags$style(HTML("
  .custom-footer { background: #f3f5f4; color: #3f5652; border-top: 3px solid #7BC148; }
  .custom-footer h4 { color: #123E3B; }
  .custom-footer a, .custom-footer a:focus { color: #3f5652; }
  .custom-footer-bar { padding: 10px 14px; background: #e8ece9; border-top: 1px solid #dde3df; }
  .custom-footer-bar h4, .custom-footer-bar a { color: #123E3B; font-size: 15px; }
  .footer-extended { padding: 40px 24px 28px; }
  .footer-extended-inner {
    max-width: 1120px; margin: 0 auto;
    display: grid; grid-template-columns: 1.4fr 1fr 1fr; gap: 40px;
  }
  .footer-extended img { display: block; width: 260px; max-width: 100%; margin-left: -13px; }
  .footer-extended p { margin: 14px 0 0; max-width: 300px; color: #123E3B; font-size: 15px; line-height: 1.5; }
  .footer-extended h4 { margin: 0 0 14px; font-size: 18px; font-weight: 600; }
  .footer-extended a, .footer-extended a:focus {
    display: flex; align-items: center; gap: 10px;
    margin-bottom: 10px; font-size: 14px; text-decoration: none;
  }
  .footer-extended a:hover { color: #4f8a2a; }
  .footer-extended a i { width: 12px; font-size: 12px; color: #5f9e34; }
  @media (max-width: 767px) { .footer-extended-inner { grid-template-columns: 1fr; gap: 24px; } }
"))

## Shown on the Home tab only, so it stays out of the way once work starts
footer_extended <- conditionalPanel(
  condition = "input.tabs == 'homePage'"
  , class = "footer-extended"
  , uiOutput("app_footer_extended")
)

footer <- tags$div(
    class = "custom-footer",
    footer_styles,
    footer_extended,
    tags$div(
      class = "custom-footer-bar",
      fluidRow(
        column(4,uiOutput("app_footer_title")),
        column(4,uiOutput("app_footer_contact")),
        column(4,uiOutput("app_footer_all_rights"))
      )
    )
  )
