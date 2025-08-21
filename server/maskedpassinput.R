maskedPasswordInput <- function(inputId, label, value = "", placeholder = NULL, width = "100%") {
  style <- if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";") else NULL
  
  htmltools::tagList(
    tags$style(HTML("
      .masked-password { -webkit-text-security: disc; text-security: disc; }
    ")),
    tags$div(
      class = "form-group shiny-input-container",
      style = style,
      tags$label(class = "control-label", `for` = inputId, label),
      tags$input(
        id = inputId,
        type = "text",
        class = "form-control masked-password",
        value = value,
        placeholder = placeholder,
        autocomplete = "off",
        spellcheck = "false"
      )
    )
  )
}
