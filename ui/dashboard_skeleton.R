#### ---- Workspace loading screen ---- ####

## Shown while the server sets up the modules after login, and while a
## remembered user is being signed in. Fixed to the viewport so it does not
## depend on how the loader centres its content.

## Order matters: the server reports progress with these step names
## (see update_progress() in server.R) and the bar only moves forward.
loader_steps <- c(
  signin = "loader_step_signin"
  , workspace = "loader_step_workspace"
  , tools = "loader_step_tools"
  , ready = "loader_step_ready"
)

## name is the signed-in user's first name; leave it NULL for the copy
## embedded in the page, which is shown before the server knows the user.
dashboard_skeleton <- function(name = NULL) {
  signed_in <- !is.null(name)
  current <- if (signed_in) "workspace" else "signin"
  current_i <- match(current, names(loader_steps))
  title <- if (signed_in && length(name) == 1 && !is.na(name) && nzchar(trimws(name))) {
    gsub("{name}", trimws(name), landing_label("loader_title_name"), fixed = TRUE)
  } else {
    landing_label("loader_title")
  }
  tags$div(
    class = "ldr", `data-step` = current
    , tags$div(
      class = "ldr-header"
      , tags$img(src = "brand/automl-header-logo.svg", alt = "AutoML No-Code Platform", class = "ldr-logo")
    )
    , tags$div(
      class = "ldr-card"
      , tags$h2(class = "ldr-title", title)
      , tags$div(class = "ldr-progress", tags$span(class = "ldr-progress-fill"))
      , tags$ol(
        class = "ldr-steps", role = "status", `aria-live` = "polite"
        , lapply(seq_along(loader_steps), function(i) {
          state <- if (i < current_i) "is-done" else if (i == current_i) "is-active" else ""
          tags$li(
            class = state, `data-step` = names(loader_steps)[i]
            , tags$span(class = "ldr-mark", `aria-hidden` = "true")
            , tags$span(landing_label(loader_steps[[i]]))
          )
        })
      )
    )
  )
}
