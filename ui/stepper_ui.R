# ui/stepper_ui.R
# ============================================================================
# Workflow Stepper — clean text-based progress indicator
# White background, checkmark text, section header with step count
# ============================================================================

workflow_steps <- list(
  list(num = 1, label = "Upload",       section = "Data Upload",        tabs = c("sourcedata")),
  list(num = 2, label = "Explore",      section = "Data Exploration",   tabs = c("Overview", "Explore")),
  list(num = 3, label = "Transform",    section = "Data Transformation", tabs = c("Transform", "combineData")),
  list(num = 4, label = "Visualize",    section = "Visualization",      tabs = c("visualizeData", "summarizeAutomatic", "summarizeCustom")),
  list(num = 5, label = "Model Setup",  section = "Machine Learning / Model Setup", tabs = c("setupModels", "featureEngineering")),
  list(num = 6, label = "Train",        section = "Model Training",     tabs = c("trainModel")),
  list(num = 7, label = "Deploy",       section = "Validate & Deploy",  tabs = c("validateDeployModel", "predictClassify"))
)

non_workflow_tabs <- c("homePage", "researchQuestions", "deeplearning", "cnndeep",
                       "evidenceQuality", "achilles", "omop_visualizations",
                       "CohortConstructor", "FeatureExtraction", "addResources")

get_step_for_tab <- function(tab_name) {
  if (is.null(tab_name) || tab_name %in% non_workflow_tabs) return(0)
  for (step in workflow_steps) {
    if (tab_name %in% step$tabs) return(step$num)
  }
  return(0)
}

stepper_ui <- function() {
  tags$div(
    id = "workflow-stepper-container",
    uiOutput("workflow_stepper_bar")
  )
}

render_stepper_html <- function(active_step) {
  if (active_step == 0) {
    return(tags$div(class = "workflow-stepper stepper-hidden"))
  }

  # Get current section name
  current_section <- workflow_steps[[active_step]]$section
  total_steps <- length(workflow_steps)

  # Build step items
  step_elements <- list()

  for (i in seq_along(workflow_steps)) {
    step <- workflow_steps[[i]]

    if (i < active_step) {
      # Completed
      el <- tags$span(
        class = "stepper-item completed",
        onclick = sprintf("Shiny.setInputValue('stepper_nav','%s',{priority:'event'});", step$tabs[1]),
        tags$span(class = "stepper-icon", HTML("&#10004;")),
        step$label
      )
    } else if (i == active_step) {
      # Active
      el <- tags$span(
        class = "stepper-item active",
        tags$span(class = "stepper-icon", HTML("&#9658;")),
        step$label
      )
    } else {
      # Inactive
      el <- tags$span(
        class = "stepper-item inactive",
        onclick = sprintf("Shiny.setInputValue('stepper_nav','%s',{priority:'event'});", step$tabs[1]),
        tags$span(class = "stepper-icon", HTML("&#9675;")),
        step$label
      )
    }

    step_elements <- c(step_elements, list(el))

    # Add separator (except after last)
    if (i < total_steps) {
      step_elements <- c(step_elements, list(tags$span(class = "stepper-sep", HTML("&middot;"))))
    }
  }

  tags$div(
    class = "workflow-stepper",
    # Header row
    tags$div(
      class = "stepper-header",
      tags$span(class = "stepper-section-name", current_section),
      tags$span(class = "stepper-step-count", paste0("Step ", active_step, " of ", total_steps))
    ),
    # Steps row
    tags$div(class = "stepper-items", step_elements)
  )
}
