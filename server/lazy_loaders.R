#### ---- Lazy module loaders ----------------------------------- ####
# Each loader watches the sidebar tab input and sources its module only
# when the user actually navigates to a tab that needs it. Reduces cold
# start time. Functions rely on lexical scoping for `input`, reactive
# values, and previously sourced module functions.

lazy_load_missing_data <- function() {
	loaded <- FALSE
	observeEvent(input$tabs, {
		if (!loaded && input$tabs %in% c("manageData", "transformData")) {
			loaded <<- TRUE
			transform_data_plot_missing_data_server()
		}
	}, ignoreInit = TRUE)
}

lazy_load_custom_viz <- function(plots_custom_rv) {
  loaded <- FALSE
  observeEvent(input$tabs, {
    if (!loaded && input$tabs %in% c("summarizeCustom", "visualizeData")) {
      loaded <<- TRUE
      source("server/user_defined_visualization.R", local = TRUE)
      user_defined_server(plots_custom_rv = plots_custom_rv)
    }
  }, ignoreInit = TRUE)
}

lazy_load_compare_models <- function() {
	loaded <- FALSE
	observeEvent(input$tabs, {
		if (!loaded && input$tabs %in% c("validateDeployModel", "trainModel")) {
			loaded <<- TRUE
			source("server/compare_trained_caret_models.R", local = TRUE)
			model_training_caret_train_metrics_server()
		}
	}, ignoreInit = TRUE)
}

lazy_load_deep_learning <- function() {
	loaded <- FALSE
	observeEvent(input$tabs, {
		if (!loaded && input$tabs %in% c("deeplearning", "cnndeep")) {
			loaded <<- TRUE
			source("server/deep_learning.R", local = TRUE)
			deep_learning()
		}
	}, ignoreInit = TRUE)
}
