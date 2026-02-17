.app_root <- normalizePath(getwd(), mustWork = TRUE)

# =============================================================================
# Static assets
# =============================================================================
.anon_dir <- file.path(.app_root, "www", "anon_assets")

if (dir.exists(.anon_dir)) {
  shiny::addResourcePath("anon_assets", .anon_dir)
}

# =============================================================================
# Load packages FIRST (this should load shiny, shinydashboard, shinyjs, waiter, login, etc)
# =============================================================================
source(file.path(.app_root, "ui", "load_r_packages.R"))

# =============================================================================
# UI helpers / assets
# =============================================================================
source(file.path(.app_root, "ui", "sidebar_hover_collapse.R"))
source(file.path(.app_root, "R",      "shinyutilities.R"))
source(file.path(.app_root, "server", "maskedpassinput.R"))

# =============================================================================
# App UI modules
# =============================================================================
source(file.path(.app_root, "ui", "automl_ui.R"))
source(file.path(.app_root, "ui", "automl_controls_ui.R"))
source(file.path(.app_root, "ui", "train_model_ui.R"))
source(file.path(.app_root, "ui", "deploy_model_ui.R"))

# =============================================================================
# Login / header / theme / footer / homepage
# =============================================================================
source(file.path(.app_root, "ui", "login_credentials.R"))
source(file.path(.app_root, "ui", "headertag.R"))
source(file.path(.app_root, "ui", "appTheme.R"))
source(file.path(.app_root, "ui", "header.R"))
source(file.path(.app_root, "ui", "footer.R"))
source(file.path(.app_root, "ui", "homepage.R"))

# =============================================================================
# Anonymization UI (module + tab UIs)
# =============================================================================
# NOTE: local=FALSE so module functions exist at global scope
.mod_quant_path <- file.path(.app_root, "modules", "mod_quant_anonymization.R")
if (file.exists(.mod_quant_path)) {
  source(.mod_quant_path, local = FALSE)
}

.anon_ui_path <- file.path(.app_root, "ui", "anonymization_ui.R")
if (file.exists(.anon_ui_path)) {
  source(.anon_ui_path, local = FALSE)
}

# =============================================================================
# Dashboard body must be last (it uses functions above)
# =============================================================================
source(file.path(.app_root, "ui", "dashboard_body.R"))

# =============================================================================
# Shinydashboard shell
# =============================================================================

aphrcSiderbar <- shinydashboard::dashboardSidebar(
  width = 240,
  shinydashboard::sidebarMenuOutput("dynamic_meinu_aphrc")
)

# IMPORTANT:
# - disable=TRUE removes the header bar entirely (no hamburger)
# - disable=FALSE keeps header bar + hamburger toggle available
aphrcHeader <- shinydashboard::dashboardHeader(disable = FALSE)

# =============================================================================
# UI object (THIS MUST BE NAMED `ui` if using shinyApp(ui=ui,...))
# =============================================================================

ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  waiter::useWaiter(),
  
  shiny::tags$head(
    # anonymization CSS in WWW/anon_assets
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "anon_assets/custom.css?v=20260129"),
    # Font Awesome (needed by anonymization module icons and your hover assets)
    shiny::tags$link(rel = "stylesheet",
                     href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"),
    
    # hover-collapse assets (your helper)
    sidebar_hover_collapse_assets(default_topbar_px = 110)
  ),
  
  shiny::div(
    id = "auth_wrapper1",
    
    # Logged-in header (your existing `header` object from ui/header.R)
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
          shiny::tags$img(src = "aphrc.png", height = "80px",
                          style = "margin-bottom: 15px;"),
          shiny::tags$h3("Welcome to Nocode Platform")
        ),
        
        shiny::div(
          class = "toggle-buttons",
          shiny::actionButton("show_login",  "Login",          class = "btn btn-outline-success"),
          shiny::actionButton("show_signup", "Sign Up",        class = "btn btn-outline-success"),
          shiny::actionButton("show_reset",  "Reset Password", class = "btn btn-outline-success")
        ),
        
        shiny::div(
          id = "login_form",
          login::login_ui(id = app_login_config$APP_ID)
        ),
        
        shiny::div(
          id = "signup_form", style = "display:none;",
          login::new_user_ui(id = app_login_config$APP_ID)
        ),
        
        shiny::div(
          id = "reset_form", style = "display:none;",
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
