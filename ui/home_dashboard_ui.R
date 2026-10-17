#### ---- Dashboard home ---- ####

## The Home tab: a summary of the user's work drawn by the server, with the
## objectives and components panels of the original homepage between and
## after its sections.
home_dashboard_ui <- function() {
  tagList(
    homepage_styles()
    , tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "home_dashboard.css?v=20261017q")
      , tags$script(HTML(
        "$(document).on('click', '[data-home-tab]', function (e) {
          e.preventDefault();
          Shiny.setInputValue('home_go', $(this).attr('data-home-tab'), {priority: 'event'});
          window.scrollTo(0, 0);
        });"
      ))
    )
    , uiOutput("home_dashboard")
    , uiOutput("home_recent")
    , uiOutput("home_components")
  )
}

## Links and buttons that switch to another tab of the dashboard
home_go <- function(tab, ..., class = NULL) {
  tags$a(href = "#", class = class, `data-home-tab` = tab, ...)
}

home_tile <- function(icon_name, value, label, note = NULL) {
  tags$div(
    class = "home-tile"
    , tags$span(class = "home-tile-icon", icon(icon_name))
    , tags$div(
      tags$div(class = "home-tile-value", value)
      , tags$div(class = "home-tile-label", label)
      , if (!is.null(note)) tags$div(class = "home-tile-note", note)
    )
  )
}

## Platform title and introduction, styled with the original homepage classes
## is_new : TRUE when the user has no datasets yet (first-time experience)
home_banner <- function(name, label, is_new = FALSE) {
  more_text <- label("homepage_more_details_link_text")
  more_url <- label("homepage_more_details_url")

  kicker <- if (is_new) label("home_welcome", name = name) else label("home_welcome_back", name = name)

  ## Call to action always points at upload: new user -> "Upload your first
  ## dataset", returning user -> "Upload data". (No "continue" shortcut, which
  ## could jump to a later step before any dataset is loaded this session.)
  cta <- if (is_new) {
    home_go("sourcedata", icon("upload"), label("home_start_first"), class = "home-btn home-btn-primary")
  } else {
    home_go("sourcedata", icon("upload"), label("home_upload"), class = "home-btn home-btn-primary")
  }

  tags$div(
    class = "home-banner"
    , tags$div(
      class = "home-banner-top"
      , tags$span(class = "home-banner-kicker", kicker)
      , tags$div(class = "home-banner-cta", cta)
    )
    , tags$h1(class = "aphrc-title", label("homepage_title"))
    , tags$hr(class = "aphrc-hr")
    , tags$p(class = "aphrc-intro", label("homepage_intro"))
    , if (nzchar(more_text) && nzchar(more_url)) {
      tags$a(class = "aphrc-link", href = more_url, target = "_blank", more_text, icon("arrow-up-right-from-square"))
    }
  )
}

home_date_text <- function(d) format(d, "%d %b %Y")

## Deletion date as a pill coloured by how soon it is; hovering (or tapping)
## shows how long the dataset stays on the servers
home_deletion_cell <- function(d, label, soon_days = 14) {
  if (is.na(d)) return("")
  days <- as.integer(d - Sys.Date())
  state <- if (days < 0) "overdue" else if (days <= soon_days) "soon" else "ok"
  tip <- if (days < 0) {
    label("home_deletion_tip_overdue")
  } else {
    countdown <- if (days == 0) {
      label("home_deletion_today")
    } else if (days == 1) {
      label("home_deletion_tomorrow")
    } else {
      label("home_deletion_in_days", days = days)
    }
    label("home_deletion_tip", countdown = countdown)
  }
  tags$span(
    class = paste("home-pill", paste0("home-pill-", state))
    , tabindex = "0"
    , `aria-label` = tip
    , home_date_text(d)
    , tags$span(class = "home-tip", role = "tooltip", tip)
  )
}

## Getting-started card icons in step order: outline SVG paths on a 24px grid
## (upload, chart, processor, rocket), drawn with the current text colour
home_step_icons <- c(
  '<path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><polyline points="17 8 12 3 7 8"/><line x1="12" y1="3" x2="12" y2="15"/>'
  , '<path d="M3 3v18h18"/><path d="M18 17V9"/><path d="M13 17V5"/><path d="M8 17v-3"/>'
  , '<rect x="4" y="4" width="16" height="16" rx="2"/><rect x="9" y="9" width="6" height="6"/><path d="M15 2v2M15 20v2M2 15h2M2 9h2M20 15h2M20 9h2M9 2v2M9 20v2"/>'
  , '<path d="M4.5 16.5c-1.5 1.26-2 5-2 5s3.74-.5 5-2c.71-.84.7-2.13-.09-2.91a2.18 2.18 0 0 0-2.91-.09z"/><path d="m12 15-3-3a22 22 0 0 1 2-3.95A12.88 12.88 0 0 1 22 2c0 2.72-.78 7.5-6 11a22.35 22.35 0 0 1-4 2z"/><path d="M9 12H4s.55-3.03 2-4c1.62-1.08 5 0 5 0"/><path d="M12 15v5s3.03-.55 4-2c1.08-1.62 0-5 0-5"/>'
)

home_step_svg <- function(i) {
  HTML(paste0(
    '<svg viewBox="0 0 24 24" width="22" height="22" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">'
    , home_step_icons[(i - 1) %% length(home_step_icons) + 1]
    , '</svg>'
  ))
}

## Banner, getting-started steps and summary tiles
## steps: list of list(label, tab, done)
## datasets: data.frame with name, size, uploaded (Date), deletion (Date)
## label: function(key, ...) returning the translated text
home_dashboard_view <- function(name, steps, datasets, n_models, label, is_new = FALSE) {
  next_i <- Position(function(s) !s$done, steps)
  next_step <- if (!is.na(next_i)) steps[[next_i]]

  tagList(
    home_banner(name, label, is_new = is_new)
    , tags$div(
      class = "home"
      , tags$div(
        class = "home-grid"
        , tags$section(
          class = "home-card home-steps"
          , tags$h2(label("home_steps_title"))
          , tags$p(class = "home-steps-sub", label("home_steps_subtitle"))
          , tags$ol(
            class = "home-step-list"
            , lapply(seq_along(steps), function(i) {
              step <- steps[[i]]
              locked <- !step$done && !is.null(step$needs) && !steps[[step$needs]]$done
              state <- if (step$done) "is-done" else if (locked) "is-locked" else if (identical(i, next_i)) "is-current"
              badge <- if (step$done) icon("check") else if (locked) icon("lock")
              face <- tagList(
                tags$span(
                  class = "home-step-icon"
                  , home_step_svg(i)
                  , if (!is.null(badge)) tags$span(class = "home-step-badge", badge)
                )
                , tags$span(class = "home-step-num", sprintf("%02d", i))
                , tags$span(class = "home-step-label", step$label)
                , if (nzchar(step$desc %||% "")) tags$span(class = "home-step-desc", step$desc)
              )
              tags$li(
                class = paste("home-step", state)
                , if (locked) {
                  tags$div(
                    class = "home-step-locked"
                    , tabindex = "0"
                    , `aria-disabled` = "true"
                    , `aria-label` = paste(step$label, step$locked_hint, sep = ". ")
                    , face
                    , tags$span(class = "home-tip", role = "tooltip", step$locked_hint)
                  )
                } else {
                  home_go(step$tab, face)
                }
                , if (i < length(steps)) tags$span(class = "home-step-arrow", `aria-hidden` = "true", icon("arrow-right"))
              )
            })
          )
          , if (is.null(next_step)) {
            tags$p(class = "home-steps-done", icon("circle-check"), label("home_all_done"))
          } else {
            home_go(
              next_step$tab
              , if (is_new) label("home_start_step", step = next_step$label) else label("home_next_step", step = next_step$label)
              , icon("arrow-right")
              , class = "home-btn home-btn-secondary"
            )
          }
        )
      )
    )
  )
}

## Most recent uploads with their scheduled deletion dates
home_recent_view <- function(datasets, label, recent_n = 5) {
  n_datasets <- NROW(datasets)
  recent <- if (n_datasets) {
    datasets[order(datasets$uploaded, decreasing = TRUE), , drop = FALSE][seq_len(min(recent_n, n_datasets)), , drop = FALSE]
  }

  tags$div(
    class = "home"
    , tags$section(
      class = "home-card home-recent home-framed"
      , tags$h2(label("home_recent_title"))
      , if (!n_datasets) {
        tags$div(
          class = "home-empty"
          , icon("folder-open")
          , tags$p(label("home_recent_empty"))
        )
      } else {
        tags$table(
          class = "home-table"
          , tags$thead(tags$tr(
            tags$th(class = "home-table-num", "#")
            , tags$th(label("home_col_dataset"))
            , tags$th(label("home_col_size"))
            , tags$th(label("home_col_uploaded"))
            , tags$th(label("home_col_deletion"))
          ))
          , tags$tbody(lapply(seq_len(NROW(recent)), function(i) {
            tags$tr(
              tags$td(class = "home-table-num", tags$span(class = "home-num", i))
              , tags$td(class = "home-table-name", recent$name[i])
              , tags$td(recent$size[i])
              , tags$td(home_date_text(recent$uploaded[i]))
              , tags$td(home_deletion_cell(recent$deletion[i], label))
            )
          }))
        )
      }
    )
    , tags$p(
      class = "home-footnote"
      , icon("shield-halved")
      , label("home_retention_note")
    )
  )
}

## Components panel from the original homepage (Objectives panel is intentionally omitted)
home_components_view <- function() {
  tags$div(class = "home-platform", homepage_components())
}
