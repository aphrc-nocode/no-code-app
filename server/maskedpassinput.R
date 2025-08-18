maskedPasswordInput <- function(inputId, label, value = "", placeholder = NULL) {
  htmltools::tagList(
    tags$style(HTML("
      .masked-password { -webkit-text-security: disc; text-security: disc; }
    ")),
    tags$div(class="form-group shiny-input-container",
             tags$label(class="control-label", `for`=inputId, label),
             tags$input(id=inputId, type="text", class="form-control masked-password",
                        value=value, placeholder=placeholder, autocomplete="off", spellcheck="false")
    )
  )
}