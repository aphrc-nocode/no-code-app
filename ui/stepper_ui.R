# ui/stepper_ui.R
# ============================================================================
# Workflow Stepper — premium progress bar in the dashboard body area
# Hidden on homepage and non-workflow tabs, highlights active step
# ============================================================================

# Define the workflow steps and their corresponding sidebar tabNames
workflow_steps <- list(
  list(num = 1, label = "Upload",       tabs = c("sourcedata")),
  list(num = 2, label = "Explore",      tabs = c("Overview", "Explore")),
  list(num = 3, label = "Transform",    tabs = c("Transform", "combineData")),
  list(num = 4, label = "Visualize",    tabs = c("visualizeData", "summarizeAutomatic", "summarizeCustom")),
  list(num = 5, label = "Model Setup",  tabs = c("setupModels", "featureEngineering")),
  list(num = 6, label = "Train",        tabs = c("trainModel")),
  list(num = 7, label = "Deploy",       tabs = c("validateDeployModel", "predictClassify"))
)

# Tabs where stepper should NOT appear
non_workflow_tabs <- c("homePage", "researchQuestions", "deeplearning", "cnndeep",
                       "evidenceQuality", "achilles", "omop_visualizations",
                       "CohortConstructor", "FeatureExtraction", "addResources")

# ---------------------------------------------------------------------------
# get_step_for_tab() — returns the step number for a given tabName
# ---------------------------------------------------------------------------
get_step_for_tab <- function(tab_name) {
  if (is.null(tab_name) || tab_name %in% non_workflow_tabs) return(0)
  for (step in workflow_steps) {
    if (tab_name %in% step$tabs) return(step$num)
  }
  return(0)
}

# ---------------------------------------------------------------------------
# stepper_ui() — container for the reactive stepper bar
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
  # Hide on non-workflow pages (active_step == 0)
  if (active_step == 0) {
    return(tags$div(class = "workflow-stepper stepper-hidden"))
  }

  step_elements <- list()

  for (i in seq_along(workflow_steps)) {
    step <- workflow_steps[[i]]

    # Determine CSS class
    css_class <- "stepper-step"
    if (i < active_step)  css_class <- paste(css_class, "completed")
    if (i == active_step) css_class <- paste(css_class, "active")

    # Circle content: checkmark for completed, number for others
    circle_content <- if (i < active_step) {
      tags$i(class = "fa fa-check")
    } else {
      as.character(step$num)
    }

    # Clickable step
    step_el <- tags$div(
      class = css_class,
      onclick = sprintf(
        "Shiny.setInputValue('stepper_nav', '%s', {priority: 'event'});",
        step$tabs[1]
      ),
      tags$div(class = "stepper-circle", circle_content),
      tags$span(class = "stepper-label", step$label)
    )

    step_elements <- c(step_elements, list(step_el))

    # Connector line (except after last step)
    if (i < length(workflow_steps)) {
      conn_class <- "stepper-connector"
      if (i < active_step) conn_class <- paste(conn_class, "completed")
      step_elements <- c(step_elements, list(tags$div(class = conn_class)))
    }
  }

  tags$div(class = "workflow-stepper", step_elements)
}
