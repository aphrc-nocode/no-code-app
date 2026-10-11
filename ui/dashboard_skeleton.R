#### ---- Dashboard loading skeleton ---- ####

## Outline of the dashboard shown while the server sets up the modules after
## login, and while a remembered user is being signed in. Fixed to the
## viewport so it does not depend on how the loader centres its content.

## status_id lets the server update the line under the skeleton; leave it
## NULL for the copy embedded in the page so the id stays unique.
dashboard_skeleton <- function(status_id = NULL) {
  bar <- function(width, class = "") {
    tags$span(class = paste("dsk-bar", class), style = sprintf("width:%s;", width))
  }
  tags$div(
    class = "dsk"
    , tags$div(
      class = "dsk-header"
      , tags$img(src = "aphrc.png", alt = "APHRC", class = "dsk-logo")
      , bar("28%", "dsk-bar-light")
      , bar("14%", "dsk-bar-light dsk-push")
    )
    , tags$div(
      class = "dsk-body"
      , tags$div(
        class = "dsk-sidebar"
        , lapply(c("70%", "55%", "62%", "48%", "66%", "52%", "58%", "44%", "60%", "50%"), function(w) {
          bar(w, "dsk-bar-dark")
        })
      )
      , tags$div(
        class = "dsk-main"
        , bar("36%", "dsk-title")
        , bar("58%")
        , tags$div(
          class = "dsk-cards"
          , tags$div(class = "dsk-card"), tags$div(class = "dsk-card"), tags$div(class = "dsk-card")
        )
        , tags$div(class = "dsk-panel")
        , tags$p(id = status_id, class = "dsk-status", landing_label("landing_loading_workspace"))
      )
    )
  )
}
