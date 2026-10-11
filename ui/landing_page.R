#### ---- Public landing page (shown before login) ---- ####

## Built once when the app starts, straight from the labelling sheet, so the
## page paints before any server session exists. Switching language swaps the
## text in the browser from the dictionary embedded in the page.

## Temporary English until these keys exist in the labelling sheet
landing_fallbacks <- c(
  landing_nav_features   = "Features"
  , landing_nav_about    = "About"
  , landing_nav_faq      = "FAQ"
  , landing_nav_signin   = "Sign in"
  , landing_hero_kicker  = "APHRC AutoML No-Code Platform"
  , landing_hero_title   = "Build predictive models from your health data, no coding needed"
  , landing_hero_subtitle = "Point-and-click machine learning for public health researchers. Upload your data, explore it, train models and deploy them in one guided workflow."
  , landing_hero_point_code    = "No coding required"
  , landing_hero_point_privacy = "Private: data deleted after {months} months"
  , landing_hero_point_lang    = "Available in {languages}"
  , landing_auth_title   = "Sign in to your workspace"
  , landing_auth_signin  = "Sign in"
  , landing_auth_signup  = "Create account"
  , landing_auth_reset   = "Forgot password?"
  , landing_features_kicker = "What you can do"
  , landing_features_title  = "One workflow, from raw data to a deployed model"
  , landing_about_kicker = "About the project"
  , landing_faq_title    = "Frequently asked questions"
  , landing_faq_q1 = "Who is this platform for?"
  , landing_faq_a1 = "Public health researchers and analysts who want to build predictive models without writing code."
  , landing_faq_q2 = "Do I need programming skills?"
  , landing_faq_a2 = "No. Every step, from uploading data to deploying a model, is point-and-click with guidance along the way."
  , landing_faq_q3 = "What happens to my data?"
  , landing_faq_q4 = "Which file formats can I upload?"
  , landing_faq_a4 = "You can upload {formats}."
  , landing_faq_q5 = "Which languages are available?"
  , landing_faq_a5 = "The platform is available in {languages}. Use the language menu at the top of the page."
  , landing_footer_tagline = "Specifically tailored for public health researchers."
  , landing_footer_platform = "Platform"
  , landing_footer_explore  = "Explore"
  , landing_footer_platform_hint = "Sign in to open a module directly."
  , landing_loading_workspace = "Loading your workspace..."
  , menu_transfer_learning  = "Transfer Learning"
  , landing_footer_rights  = "All rights reserved."
)

## Keys already translated in the sheet, reused as they are
landing_sheet_keys <- c(
  "homepage_more_details_link_text"
  , "homepage_objectives_title"
  , paste0("homepage_obj", 1:3, "_title")
  , paste0("homepage_obj", rep(1:3, each = 2), "_bullet", 1:2)
  , paste0("homepage_comp", 1:3, "_title")
  , paste0("homepage_comp", 1:3, "_desc")
  , paste0("data_privacy_modal_point_", c("storage", "access", "retention", "control"))
  , "menu_visualize_data", "menu_machine_learning"
  , "deeplearning", "menu_omop_data", "menu_data_anonymization"
)

## Footer links into the platform. Signing in from one opens that tab.
landing_platform_links <- list(
  list(key = "homepage_comp1_title", tab = "sourcedata")
  , list(key = "menu_visualize_data", tab = "summarizeAutomatic")
  , list(key = "menu_machine_learning", tab = "setupModels")
  , list(key = "menu_transfer_learning", tab = "transfer_learning")
  , list(key = "deeplearning", tab = "cnndeep")
  , list(key = "menu_omop_data", tab = "evidenceQuality")
  , list(key = "menu_data_anonymization", tab = "anonymization_quant")
)

landing_tokens <- c(
  "{months}"      = as.character(app_privacy_config$retention_months)
  , "{formats}"   = paste(gsub(" ", "", supported_files), collapse = ", ")
  , "{languages}" = paste(language_choices, collapse = ", ")
)

landing_label <- function(key, lang = language_choices[1]) {
  sheet_value <- function(l) {
    row <- labelling_file[labelling_file$input_language == l, , drop = FALSE]
    if (!nrow(row) || !key %in% names(row)) return(NA_character_)
    as.character(row[[key]][1])
  }
  value <- sheet_value(lang)
  if (is.na(value) || !nzchar(trimws(value))) value <- sheet_value(language_choices[1])
  if (is.na(value) || !nzchar(trimws(value))) value <- unname(landing_fallbacks[key])
  if (is.na(value)) value <- ""
  for (token in names(landing_tokens)) {
    value <- gsub(token, landing_tokens[[token]], value, fixed = TRUE)
  }
  value
}

landing_dictionary <- function() {
  keys <- c(names(landing_fallbacks), landing_sheet_keys)
  dict <- lapply(language_choices, function(lang) {
    as.list(setNames(vapply(keys, landing_label, character(1), lang = lang), keys))
  })
  jsonlite::toJSON(setNames(dict, language_choices), auto_unbox = TRUE)
}

## Text node the language menu can retranslate
lt <- function(key, tag = tags$span, ...) {
  tag(`data-i18n` = key, ..., landing_label(key))
}

landing_page_ui <- function() {
  login_output <- NS(app_login_config$APP_ID, "logged_in")
  more_url <- landing_label("homepage_more_details_url")

  ## Not login::is_not_logged_in(): that panel only appears once the server
  ## reports 'FALSE', which brings back the blank wait this page removes.
  conditionalPanel(
    condition = sprintf("output['%s'] != 'TRUE'", login_output)
    , id = "landing_page"
    , `data-login-output` = login_output

    , tags$script(id = "lp-i18n", type = "application/json", HTML(landing_dictionary()))

    ## ---- Hero -------------------------------------------------------------
    , tags$section(
      class = "lp-hero"
      , tags$nav(
        class = "lp-nav"
        , tags$a(class = "lp-nav-logo", href = "#landing_page", tags$img(src = "aphrc.png", alt = "APHRC"))
        , tags$div(
          class = "lp-nav-links"
          , lt("landing_nav_features", tags$a, href = "#lp-features")
          , lt("landing_nav_about", tags$a, href = "#lp-about")
          , lt("landing_nav_faq", tags$a, href = "#lp-faq")
        )
        , tags$div(
          class = "lp-nav-actions"
          , tags$select(
            id = "lp-lang", class = "lp-lang", `aria-label` = "Language"
            , lapply(language_choices, function(l) tags$option(value = l, l))
          )
          , lt("landing_nav_signin", tags$a, class = "lp-nav-signin", href = "#lp-auth")
        )
      )
      , tags$div(
        class = "lp-hero-inner"
        , tags$div(
          class = "lp-hero-copy"
          , lt("landing_hero_kicker", tags$p, class = "lp-kicker")
          , lt("landing_hero_title", tags$h1, class = "lp-hero-title")
          , lt("landing_hero_subtitle", tags$p, class = "lp-hero-subtitle")
          , tags$ul(
            class = "lp-hero-points"
            , tags$li(icon("wand-magic-sparkles"), lt("landing_hero_point_code"))
            , tags$li(icon("shield-halved"), lt("landing_hero_point_privacy"))
            , tags$li(icon("globe"), lt("landing_hero_point_lang"))
          )
        )
        , tags$div(
          id = "lp-auth", class = "lp-auth-card"
          , lt("landing_auth_title", tags$h2, class = "lp-auth-title")
          , tags$div(
            class = "lp-auth-tabs"
            , actionButton("show_login", landing_label("landing_auth_signin"), class = "lp-auth-tab active", `data-i18n` = "landing_auth_signin", `data-form` = "login_form")
            , actionButton("show_signup", landing_label("landing_auth_signup"), class = "lp-auth-tab", `data-i18n` = "landing_auth_signup", `data-form` = "signup_form")
          )
          , tags$div(
            class = "lp-auth-body"
            , div(id = "login_form", login::login_ui(id = app_login_config$APP_ID))
            , div(id = "signup_form", style = "display:none;", login::new_user_ui(id = app_login_config$APP_ID))
            , div(id = "reset_form", style = "display:none;", login::reset_password_ui(id = app_login_config$APP_ID))
          )
          , actionLink("show_reset", landing_label("landing_auth_reset"), class = "lp-auth-reset", `data-i18n` = "landing_auth_reset", `data-form` = "reset_form")
        )
      )
    )

    ## ---- Features (the 3 platform components) ----------------------------
    , tags$section(
      id = "lp-features", class = "lp-section"
      , lt("landing_features_kicker", tags$p, class = "lp-section-kicker")
      , lt("landing_features_title", tags$h2, class = "lp-section-title")
      , tags$div(class = "lp-tricolor")
      , tags$div(
        class = "lp-grid-3"
        , lapply(list(
          list(n = 1, colour = "red", icon = "database")
          , list(n = 2, colour = "amber", icon = "gears")
          , list(n = 3, colour = "green", icon = "rocket")
        ), function(comp) {
          tags$div(
            class = "lp-feature"
            , tags$div(class = paste("lp-feature-icon", comp$colour), icon(comp$icon))
            , lt(paste0("homepage_comp", comp$n, "_title"), tags$h3)
            , lt(paste0("homepage_comp", comp$n, "_desc"), tags$p)
          )
        })
      )
    )

    ## ---- About the project (objectives) -------------------------------------
    , tags$section(
      id = "lp-about", class = "lp-section lp-section-alt"
      , lt("landing_about_kicker", tags$p, class = "lp-section-kicker")
      , lt("homepage_objectives_title", tags$h2, class = "lp-section-title")
      , tags$div(class = "lp-tricolor")
      , tags$div(
        class = "lp-tabs"
        , tags$div(
          class = "lp-tab-list", role = "tablist"
          , lapply(1:3, function(n) {
            lt(paste0("homepage_obj", n, "_title"), tags$button
              , type = "button", role = "tab"
              , class = paste("lp-tab", if (n == 1) "active")
              , `data-tab` = paste0("lp-obj-", n))
          })
        )
        , lapply(1:3, function(n) {
          tags$div(
            id = paste0("lp-obj-", n), class = paste("lp-tab-panel", if (n == 1) "active"), role = "tabpanel"
            , tags$ul(
              lt(paste0("homepage_obj", n, "_bullet1"), tags$li)
              , lt(paste0("homepage_obj", n, "_bullet2"), tags$li)
            )
          )
        })
      )
      , tags$a(class = "lp-more-link", href = more_url, target = "_blank", rel = "noopener"
        , lt("homepage_more_details_link_text"), icon("arrow-up-right-from-square"))
    )

    ## ---- FAQ ----------------------------------------------------------------
    , tags$section(
      id = "lp-faq", class = "lp-section"
      , lt("landing_faq_title", tags$h2, class = "lp-section-title")
      , tags$div(class = "lp-tricolor")
      , tags$div(
        class = "lp-faq"
        , tags$details(open = NA, lt("landing_faq_q1", tags$summary), lt("landing_faq_a1", tags$p))
        , tags$details(lt("landing_faq_q2", tags$summary), lt("landing_faq_a2", tags$p))
        , tags$details(
          lt("landing_faq_q3", tags$summary)
          , tags$ul(lapply(c("storage", "access", "retention", "control"), function(point) {
            lt(paste0("data_privacy_modal_point_", point), tags$li)
          }))
        )
        , tags$details(lt("landing_faq_q4", tags$summary), lt("landing_faq_a4", tags$p))
        , tags$details(lt("landing_faq_q5", tags$summary), lt("landing_faq_a5", tags$p))
      )
    )

    ## ---- Footer -------------------------------------------------------------
    , tags$footer(
      class = "lp-footer"
      , tags$div(
        class = "lp-footer-inner"
        , tags$div(
          class = "lp-footer-brand"
          , tags$img(src = "aphrc.png", alt = "APHRC")
          , lt("landing_footer_tagline", tags$p)
        )
        , tags$div(
          class = "lp-footer-col"
          , lt("landing_footer_platform", tags$h4)
          , lt("landing_footer_platform_hint", tags$p, class = "lp-footer-hint")
          , lapply(landing_platform_links, function(link) {
            tags$a(
              href = "#lp-auth", class = "lp-footer-module", `data-open-tab` = link$tab
              , icon("arrow-right-to-bracket")
              , lt(link$key)
            )
          })
        )
        , tags$div(
          class = "lp-footer-col"
          , lt("landing_footer_explore", tags$h4)
          , lt("landing_nav_features", tags$a, href = "#lp-features")
          , lt("landing_nav_about", tags$a, href = "#lp-about")
          , lt("landing_nav_faq", tags$a, href = "#lp-faq")
          , lt("landing_nav_signin", tags$a, href = "#lp-auth")
          , tags$a(href = more_url, target = "_blank", rel = "noopener", "Data Science Without Borders")
        )
      )
      , tags$p(class = "lp-footer-rights"
        , sprintf("\u00a9 %s APHRC. ", format(Sys.Date(), "%Y")), lt("landing_footer_rights"))
    )
  )
}

## Remembered users (login cookie present) skip the landing page and see a
## short loader while the server signs them in. Runs before first paint.
landing_head <- function() {
  tagList(
    tags$title(landing_label("landing_hero_kicker"))
    , tags$link(rel = "stylesheet", type = "text/css", href = "landing.css?v=20261011i")
    , tags$script(HTML(
      "(function(){if(/(?:^|;\\s*)aphrc=[^;]+/.test(document.cookie)){document.documentElement.classList.add('lp-has-session');}})();"
    ))
    , tags$script(src = "landing.js?v=20261011c")
  )
}

landing_session_loader <- function() {
  tags$div(id = "lp-session-loader", dashboard_skeleton())
}
