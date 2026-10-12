#### ---- Dashboard home ---- ####

## The Home tab: a summary of the user's work drawn by the server, with the
## objectives and components panels of the original homepage between and
## after its sections.
home_dashboard_ui <- function() {
  tagList(
    homepage_styles()
    , tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "home_dashboard.css?v=20261012g")
      , tags$script(HTML(
        "$(document).on('click', '[data-home-tab]', function (e) {
          e.preventDefault();
          Shiny.setInputValue('home_go', $(this).attr('data-home-tab'), {priority: 'event'});
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
      tags$a(class = "aphrc-link", href = more_url, target = "_blank", more_text)
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
          class = "home-card home-steps home-framed"
          , tags$h2(label("home_steps_title"))
          , tags$hr(class = "aphrc-hr")
          , tags$ol(
            class = "home-step-list"
            , lapply(seq_along(steps), function(i) {
              step <- steps[[i]]
              locked <- !step$done && !is.null(step$needs) && !steps[[step$needs]]$done
              state <- if (step$done) "is-done" else if (locked) "is-locked" else if (identical(i, next_i)) "is-current"
              mark <- if (step$done) icon("check") else if (locked) icon("lock") else i
              tags$li(
                class = paste("home-step", state)
                , if (locked) {
                  tags$div(
                    class = "home-step-locked"
                    , tabindex = "0"
                    , `aria-disabled` = "true"
                    , `aria-label` = paste(step$label, step$locked_hint, sep = ". ")
                    , tags$span(class = "home-step-mark", mark)
                    , tags$span(class = "home-step-label", step$label)
                    , tags$span(class = "home-tip", role = "tooltip", step$locked_hint)
                  )
                } else {
                  home_go(
                    step$tab
                    , tags$span(class = "home-step-mark", mark)
                    , tags$span(class = "home-step-label", step$label)
                  )
                }
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

## Collapsible "About the platform": the original homepage objectives and
## components panels, kept verbatim but demoted into a <details> block.
## Open by default for first-time users (is_new); collapsed for returning ones.
## Components panel from the original homepage, shown as a standalone framed
## card on the home page (Objectives panel is intentionally omitted).
home_components_view <- function() {
  tags$div(class = "home-platform", homepage_components())
}
