# Workflow stepper helpers.

workflow_steps <- list(
  list(num = 1, key = "upload",    label = "Upload",      section = "Data Upload", tabs = c("sourcedata")),
  list(num = 2, key = "explore",   label = "Explore",     section = "Data Exploration", tabs = c("Overview", "Explore")),
  list(num = 3, key = "transform", label = "Transform",   section = "Data Transformation", tabs = c("Transform", "combineData")),
  list(num = 4, key = "visualize", label = "Visualize",   section = "Visualization", tabs = c("summarizeAutomatic", "summarizeCustom")),
  list(num = 5, key = "setup",     label = "Model Setup", section = "Machine Learning / Model Setup", tabs = c("setupModels", "featureEngineering")),
  list(num = 6, key = "train",     label = "Train",       section = "Model Training", tabs = c("trainModel")),
  list(num = 7, key = "deploy",    label = "Deploy",      section = "Validate & Deploy", tabs = c("validateDeployModel", "predictClassify"))
)

non_workflow_tabs <- c(
  "homePage", "homepage", "researchQuestions", "deeplearning", "cnndeep",
  "evidenceQuality", "achilles", "omop_visualizations",
  "CohortConstructor", "FeatureExtraction", "addResources",
  "anonymization_quant", "anonymization_qual"
)

get_step_for_tab <- function(tab_name) {
  if (is.null(tab_name) || tab_name %in% non_workflow_tabs) return(0L)

  for (step in workflow_steps) {
    if (tab_name %in% step$tabs) return(step$num)
  }

  0L
}

stepper_ui <- function() {
  tags$div(
    id = "workflow-stepper-container",
    uiOutput("workflow_stepper_bar")
  )
}

render_stepper_html <- function(active_step, completed_steps = integer()) {
  if (active_step == 0L) {
    return(tags$div(class = "workflow-stepper stepper-hidden"))
  }

  total_steps <- length(workflow_steps)
  current_section <- workflow_steps[[active_step]]$section
  completed_steps <- unique(as.integer(completed_steps))
  completed_steps <- completed_steps[!is.na(completed_steps) & completed_steps > 0L]

  step_elements <- lapply(seq_along(workflow_steps), function(i) {
    step <- workflow_steps[[i]]

    is_active <- identical(i, active_step)
    is_completed <- i %in% completed_steps
    state_classes <- c(
      "stepper-item",
      if (is_completed) "completed" else "inactive",
      if (is_active) "active"
    )
    classes <- paste(state_classes, collapse = " ")

    icon <- if (is_completed) {
      "&#10004;"
    } else if (is_active) {
      "&#9658;"
    } else {
      "&#9675;"
    }

    attrs <- list(
      class = classes,
      `data-step` = step$key,
      tags$span(class = "stepper-icon", HTML(icon)),
      step$label
    )

    if (!is_active) {
      attrs$onclick <- sprintf(
        "Shiny.setInputValue('stepper_nav','%s',{priority:'event'});",
        step$tabs[1]
      )
    }

    item <- do.call(tags$span, attrs)

    if (i < total_steps) {
      tagList(
        item,
        tags$span(class = "stepper-sep", HTML("&middot;"))
      )
    } else {
      item
    }
  })

  tags$div(
    class = "workflow-stepper",
    tags$div(
      class = "stepper-header",
      tags$span(class = "stepper-section-name", current_section),
      tags$span(class = "stepper-step-count", paste0("Step ", active_step, " of ", total_steps))
    ),
    tags$div(class = "stepper-items", step_elements)
  )
}
