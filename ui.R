#Only UI files and R packages should be included
# U1 Add this line of code to call automl_UI from UI folder
source("ui/automl_ui.R")
#Load R packages
source(paste0(getwd(), "/ui/load_r_packages.R"))
#Load Headertag
source(paste0(getwd(), "/ui/headertag.R"))
#Load App Theme
source(paste0(getwd(), "/ui/appTheme.R"))
#Load Header
source(paste0(getwd(), "/ui/header.R"))
#Load Footer
source(paste0(getwd(), "/ui/footer.R"))

#Sidebar
aphrcSiderbar <- dashboardSidebar(
  width = "20%",
  #menuItemOutput("dynamic_meinu_aphrc"),
  sidebarMenuOutput("dynamic_meinu_aphrc")
  #menuItem("AutoML", tabName = "automl_tab", icon = icon("robot"))
  
  )

#Body
source("ui/dashboard_body.R")

fluidPage(
  header,
  aphrcHeader <- dashboardHeader(disable = TRUE),
  
  dashboardPage(aphrcHeader, aphrcSiderbar, aphrcBody,skin = "green"),
  footer
)