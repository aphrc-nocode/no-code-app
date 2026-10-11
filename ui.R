#Only UI files and R packages should be included
#Load R packages
source(paste0(getwd(), "/ui/load_r_packages.R"))

## Language change utilities
source("R/shinyutilities.R")

source(paste0(getwd(), "/server/maskedpassinput.R"))
# U1 Add this line of code to call automl_UI from UI folder
source("ui/automl_ui.R")
# Automl controls
source("ui/automl_controls_ui.R")
source("ui/train_model_ui.R")

source(paste0(getwd(), "/ui/tooltip.R"))

# Load UI function before deploy_model_ui()
source("ui/deploy_model_ui.R")
#Load Headertag
source(paste0(getwd(), "/ui/login_credentials.R"))

source(paste0(getwd(), "/ui/headertag.R"))
#Load App Theme
source(paste0(getwd(), "/ui/appTheme.R"))
#Load Header
source(paste0(getwd(), "/ui/header.R"))
#Load Footer
source(paste0(getwd(), "/ui/footer.R"))
source(paste0(getwd(), "/ui/homepage.R"))
source(paste0(getwd(), "/ui/landing_page.R"))
source(paste0(getwd(), "/ui/dashboard_skeleton.R"))

source("ui/dashboard_body.R")
#Sidebar
aphrcSiderbar <- dashboardSidebar(
  width = "20%",
  sidebarMenuOutput("dynamic_meinu_aphrc")
)
#menuItemOutput("dynamic_meinu_aphrc"),
sidebarMenuOutput("dynamic_meinu_aphrc")
#menuItem("AutoML", tabName = "automl_tab", icon = icon("robot"))

#Body
fluidPage(
  useShinyjs(),
  useWaiter(),

  shiny::tags$head(
    landing_head(),
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "anon_custom.css?v=20260129"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css"
    )
  ),
  
    landing_session_loader(),

    div(
      id = "auth_wrapper1",
      # Hidden from first paint; the login condition reveals them once signed in
      login::is_logged_in(
        id = app_login_config$APP_ID, style = "display:none;", header
      ),
      
      aphrcHeader <- dashboardHeader(disable = TRUE),
      
      landing_page_ui(),
      
      login::is_logged_in(
        id = app_login_config$APP_ID, style = "display:none;",
        div(dashboardPage(aphrcHeader, aphrcSiderbar, aphrcBody, skin = "green"))
      ),
      
      login::is_logged_in(
        id = app_login_config$APP_ID, style = "display:none;",
        div(footer)
      )
    ))
  