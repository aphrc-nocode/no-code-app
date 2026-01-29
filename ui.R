
.app_root <- normalizePath(getwd(), mustWork = TRUE)

# ---- Register static assets ONCE (must happen BEFORE building UI) ----
.www_dir  <- normalizePath(file.path(.app_root, "WWW"), mustWork = FALSE)
.anon_dir <- normalizePath(file.path(.app_root, "WWW", "anon_assets"), mustWork = FALSE)

#if (dir.exists(.www_dir))  shiny::addResourcePath("WWW", .www_dir)
#if (dir.exists(.anon_dir)) shiny::addResourcePath("anon_assets", .anon_dir)

# ---- Load packages FIRST ----
source(file.path(.app_root, "ui", "load_r_packages.R"))
source(file.path(.app_root, "ui", "sidebar_hover_collapse.R"))

# ---- Utilities / shared UI deps ----
source(file.path(.app_root, "R",      "shinyutilities.R"))
source(file.path(.app_root, "server", "maskedpassinput.R"))

# ---- App UI modules ----
source(file.path(.app_root, "ui", "automl_ui.R"))
source(file.path(.app_root, "ui", "automl_controls_ui.R"))
source(file.path(.app_root, "ui", "train_model_ui.R"))
source(file.path(.app_root, "ui", "deploy_model_ui.R"))


# ---- Login / header / theme / footer / homepage ----
source(file.path(.app_root, "ui", "login_credentials.R"))
source(file.path(.app_root, "ui", "headertag.R"))
source(file.path(.app_root, "ui", "appTheme.R"))
source(file.path(.app_root, "ui", "header.R"))
source(file.path(.app_root, "ui", "footer.R"))
source(file.path(.app_root, "ui", "homepage.R"))

# ---- Anonymization UI (module + tab UIs) ----
if (file.exists(file.path(.app_root, "modules", "mod_quant_anonymization.R"))) {
  source(file.path(.app_root, "modules", "mod_quant_anonymization.R"), local = FALSE)
}
if (file.exists(file.path(.app_root, "ui", "anonymization_ui.R"))) {
  source(file.path(.app_root, "ui", "anonymization_ui.R"), local = FALSE)
}

# ---- Dashboard body must be last (it uses UI functions above) ----
source(file.path(.app_root, "ui", "dashboard_body.R"))

tags$head(
  tags$link(rel="stylesheet", type="text/css",
            href="anon_assets/custom.css?v=20260129")
)


# =============================================================================
# Shinydashboard shell
# =============================================================================

aphrcSiderbar <- shinydashboard::dashboardSidebar(
  width = "20%",
  shinydashboard::sidebarMenuOutput("dynamic_meinu_aphrc")
)

# IMPORTANT: enable header so the hamburger toggle exists
aphrcHeader <- shinydashboard::dashboardHeader(disable = FALSE)

# =============================================================================
# UI
# =============================================================================

shiny::fluidPage(
  shinyjs::useShinyjs(),
  waiter::useWaiter(),
  
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "anon_assets/custom.css"),
    shiny::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"),
    sidebar_hover_collapse_assets(default_topbar_px = 110)
  ),
  
  shiny::div(
    id = "auth_wrapper1",
    
    # Logged-in header (your existing 'header' object from header.R)
    login::is_logged_in(
      id = app_login_config$APP_ID,
      header
    ),
    
    # Not logged-in auth UI
    login::is_not_logged_in(
      id = app_login_config$APP_ID,
      shiny::div(
        class = "auth-container",
        shiny::br(),
        
        shiny::div(
          class = "auth-title text-center",
          shiny::tags$img(src = "aphrc.png", height = "80px", style = "margin-bottom: 15px;"),
          shiny::tags$h3("Welcome to Nocode Platform")
        ),
        
        shiny::div(
          class = "toggle-buttons",
          shiny::actionButton("show_login",  "Login",          class = "btn btn-outline-success"),
          shiny::actionButton("show_signup", "Sign Up",        class = "btn btn-outline-success"),
          shiny::actionButton("show_reset",  "Reset Password", class = "btn btn-outline-success")
        ),
        
        shiny::div(id = "login_form",
                   login::login_ui(id = app_login_config$APP_ID)
        ),
        
        shiny::div(id = "signup_form", style = "display:none;",
                   login::new_user_ui(id = app_login_config$APP_ID)
        ),
        
        shiny::div(id = "reset_form", style = "display:none;",
                   login::reset_password_ui(id = app_login_config$APP_ID)
        )
      )
    ),
    
    # Logged-in dashboard
    login::is_logged_in(
      id = app_login_config$APP_ID,
      shinydashboard::dashboardPage(
        header  = aphrcHeader,
        sidebar = aphrcSiderbar,
        body    = aphrcBody,
        skin    = "green"
      )
    ),
    
    # Logged-in footer
    login::is_logged_in(
      id = app_login_config$APP_ID,
      footer
    )
  )
)
