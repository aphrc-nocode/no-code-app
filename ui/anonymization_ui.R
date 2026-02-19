anonymization_quant_ui <- function() {
  shinydashboard::tabItem(
    tabName = "anonymization_quant",
    mod_quant_anon_ui("quant_anon")
  )
}

anonymization_qual_ui <- function() {
  shinydashboard::tabItem(
    tabName = "anonymization_qual",
    shiny::h3("Qualitative Anonymization"),
    shiny::p("Coming soon / integrate later.")
  )
}
