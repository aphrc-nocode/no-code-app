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

source("ui/dashboard_body.R")


#Sidebar
aphrcSiderbar <- dashboardSidebar(
  width = "20%",
  #menuItemOutput("dynamic_meinu_aphrc"),
  sidebarMenuOutput("dynamic_meinu_aphrc")
  #menuItem("AutoML", tabName = "automl_tab", icon = icon("robot"))
  
)

#Body
fluidPage(
  useShinyjs(),
  
  login::is_logged_in(
    id = app_login_config$APP_ID, header),
  
  aphrcHeader <- dashboardHeader(disable = TRUE),
  
  login::is_not_logged_in(
    id = app_login_config$APP_ID,
    div(class = "auth-container",
        br(),
        div(class = "auth-title text-center",
            tags$img(src = "aphrc.png", height = "80px", style = "margin-bottom: 15px;"),
            h3("Welcome to Nocode Platform")
        ),
        
        div(class = "toggle-buttons",
            actionButton("show_login", "Login", class = "btn btn-outline-success"),
            actionButton("show_signup", "Sign Up", class = "btn btn-outline-success"),
            actionButton("show_reset", "Reset Password", class = "btn btn-outline-success")
        ),
        
        div(id = "login_form",
            login::login_ui(id = app_login_config$APP_ID)
        ),
        
        div(id = "signup_form", style = "display: none;",
            login::new_user_ui(id = app_login_config$APP_ID)
        ),
        
        div(id = "reset_form", style = "display: none;",
            login::reset_password_ui(id = app_login_config$APP_ID)
        ))),
  

    login::is_logged_in(
      id = app_login_config$APP_ID,
      div(dashboardPage(aphrcHeader, aphrcSiderbar, aphrcBody, skin = "green"))
    ),


    login::is_logged_in(
      id = app_login_config$APP_ID,
      div(footer)
    )
  
)
