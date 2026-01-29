anonymization_quant_ui <- function() {
  tabItem(
    tabName = "anonymization_quant",
    mod_quant_anon_ui("quant_anon")
  )
}

anonymization_qual_ui <- function() {
  tabItem(
    tabName = "anonymization_qual",
    h3("Qualitative Anonymization"),
    p("Coming soon / integrate later.")
  )
}
