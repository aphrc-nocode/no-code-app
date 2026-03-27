# ui/stepper_ui.R
# ============================================================================
# Workflow Stepper — horizontal progress bar in the dashboard body area
# Shows the user where they are in the end-to-end analytics workflow
# Does NOT modify the sidebar — purely additive in the body area
# ============================================================================

# Define the workflow steps and their corresponding sidebar tabNames
workflow_steps <- list(
  list(num = 1, label = "Upload",       icon = "upload",       tabs = c("sourcedata")),
  list(num = 2, label = "Explore",      icon = "search",       tabs = c("Overview", "Explore")),
  list(num = 3, label = "Transform",    icon = "exchange-alt",  tabs = c("Transform", "combineData")),
  list(num = 4, label = "Visualize",    icon = "chart-bar",    tabs = c("visualizeData", "summarizeAutomatic", "summarizeCustom")),
  list(num = 5, label = "Model Setup",  icon = "cogs",         tabs = c("setupModels", "featureEngineering")),
  list(num = 6, label = "Train",        icon = "play-circle",  tabs = c("trainModel")),
  list(num = 7, label = "Deploy",       icon = "rocket",       tabs = c("validateDeployModel", "predictClassify"))
)

# ---------------------------------------------------------------------------
# get_step_for_tab() — returns the step number for a given tabName
# ---------------------------------------------------------------------------
get_step_for_tab <- function(tab_name) {
  if (is.null(tab_name)) return(0)
  for (step in workflow_steps) {
    if (tab_name %in% step$tabs) return(step$num)
  }
  return(0)  # tab not in the workflow (e.g. homePage, deeplearning, omop)
}

# ---------------------------------------------------------------------------
# stepper_ui() — renders the HTML for the stepper bar
# ---------------------------------------------------------------------------
stepper_ui <- function() {
  tags$div(
    id = "workflow-stepper-container",
    uiOutput("workflow_stepper_bar")
  )
}

# ---------------------------------------------------------------------------
# render_stepper_html() — builds stepper HTML given the active step number
# ---------------------------------------------------------------------------
render_stepper_html <- function(active_step) {
  # If active_step is 0, the user is on a non-workflow tab (home, OMOP, etc.)
  # We still show the stepper but with no step highlighted
  
  step_elements <- list()
  
  for (i in seq_along(workflow_steps)) {
    step <- workflow_steps[[i]]
    
    # Determine CSS class
    css_class <- "stepper-step"
    if (active_step > 0 && i < active_step)  css_class <- paste(css_class, "completed")
    if (active_step > 0 && i == active_step) css_class <- paste(css_class, "active")
    
    # Circle content: checkmark for completed, number for others
    circle_content <- if (active_step > 0 && i < active_step) {
      tags$i(class = "fa fa-check", style = "font-size: 14px;")
    } else {
      as.character(step$num)
    }
    
    # The step element (clickable, navigates to first tab of that step)
    step_el <- tags$div(
      class = css_class,
      `data-tab` = step$tabs[1],  # first tab in the group
      onclick = sprintf(
        "Shiny.setInputValue('stepper_nav', '%s', {priority: 'event'});",
        step$tabs[1]
      ),
      tags$div(class = "stepper-circle", circle_content),
      tags$span(class = "stepper-label", step$label)
    )
    
    step_elements <- c(step_elements, list(step_el))
    
    # Add connector line (except after last step)
    if (i < length(workflow_steps)) {
      conn_class <- "stepper-connector"
      if (active_step > 0 && i < active_step) conn_class <- paste(conn_class, "completed")
      step_elements <- c(step_elements, list(tags$div(class = conn_class)))
    }
  }
  
  tags$div(class = "workflow-stepper", step_elements)
}
