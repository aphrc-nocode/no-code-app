caret_job_worker <- function(config_path) {
	config <- readRDS(config_path)
	if (!is.null(config$context_path)) {
		config <- c(readRDS(config$context_path), config)
	}
	setwd(config$app_dir)

	status_path <- config$status_path
	result_path <- config$result_path

	write_status <- function(status, step = status, message = NULL) {
		saveRDS(
			list(
				status = status,
				step = step,
				message = message,
				updated_at = Sys.time()
			),
			status_path
		)
	}

	rename_columns <- function(df, name_map) {
		if (is.null(df) || !is.data.frame(df)) return(df)
		matches <- match(names(df), name_map$original)
		renamed <- !is.na(matches)
		names(df)[renamed] <- name_map$safe[matches[renamed]]
		df
	}

	safe_name_map <- function(nms) {
		data.frame(
			original = nms,
			safe = make.names(nms, unique = TRUE),
			stringsAsFactors = FALSE
		)
	}

	prepare_model_safe_names <- function(config) {
		if (is.null(config$train_df) || !is.data.frame(config$train_df)) return(config)

		model_map <- safe_name_map(names(config$train_df))
		config$model_safe_name_map <- model_map
		config$outcome_original <- config$outcome

		config$train_df <- rename_columns(config$train_df, model_map)
		config$test_df <- rename_columns(config$test_df, model_map)

		outcome_idx <- match(config$outcome, model_map$original)
		if (!is.na(outcome_idx)) {
			config$outcome <- model_map$safe[[outcome_idx]]
		}
		if (!is.null(config$outcome) && config$outcome %in% names(config$train_df)) {
			config$model_formula <- reformulate(
				termlabels = setdiff(names(config$train_df), config$outcome),
				response = config$outcome
			)
		}

		if (!is.null(config$preprocessed)) {
			for (nm in c("train_df", "train_dfdf", "test_df")) {
				if (!is.null(config$preprocessed[[nm]]) && is.data.frame(config$preprocessed[[nm]])) {
					config$preprocessed[[nm]] <- rename_columns(config$preprocessed[[nm]], model_map)
				}
			}
			if (!is.null(config$preprocessed$original_df) && is.data.frame(config$preprocessed$original_df)) {
				raw_map <- safe_name_map(names(config$preprocessed$original_df))
				config$raw_safe_name_map <- raw_map
				config$preprocessed$original_df <- rename_columns(config$preprocessed$original_df, raw_map)
			}
		}

		config
	}

	write_status("running", "training")

	tryCatch({
		suppressPackageStartupMessages({
			library(Rautoml)
			library(caret)
			library(caretEnsemble)
		})

		if (!is.null(config$seed) && !is.na(config$seed)) {
			set.seed(config$seed)
		}

		if (isTRUE(config$use_cluster)) {
			Rautoml::start_cluster()
			on.exit(try(Rautoml::stop_cluster(), silent = TRUE), add = TRUE)
		}

		config <- prepare_model_safe_names(config)

		models <- Rautoml::train_caret_models(
			df = config$train_df,
			model_form = config$model_formula,
			ctrl = config$train_control,
			model_list = config$model_list,
			metric = config$metric
		)

		write_status("running", "evaluating")

		tuned_parameters <- Rautoml::get_tuned_params(models)
		control_parameters <- Rautoml::get_ctl_params(
			models = models,
			items = names(config$train_control)
		)
		train_metrics_df <- Rautoml::extract_summary(
			models,
			summary_fun = Rautoml::student_t_summary
		)

		Rautoml::save_rautoml_csv(
			object = train_metrics_df,
			name = "training_performance_metrics",
			dataset_id = config$dataset_id,
			session_name = config$session_id,
			timestamp = Sys.time(),
			output_dir = file.path(config$app_username, "outputs")
		)

		test_metrics_objs <- Rautoml::boot_estimates_multiple(
			models = models,
			df = config$test_df,
			outcome_var = config$outcome,
			problem_type = config$task,
			nreps = 100,
			model_name = NULL,
			type = "prob",
			report = config$metric,
			summary_fun = Rautoml::student_t_summary,
			save_model = TRUE,
			model_folder = file.path(config$app_username, "models"),
			recipe_folder = file.path(config$app_username, "recipes"),
			preprocesses = config$preprocessed
		)

		Rautoml::save_boot_estimates(
			boot_list = test_metrics_objs,
			dataset_id = config$dataset_id,
			session_name = config$session_id,
			timestamp = Sys.time(),
			output_dir = file.path(config$app_username, "outputs"),
			sub_dir = "test_metrics"
		)

		Rautoml::create_model_logs(
			df_name = config$dataset_id,
			session_name = config$session_id,
			outcome = config$outcome,
			framework = "Caret",
			train_result = test_metrics_objs$all,
			timestamp = Sys.time(),
			path = file.path(config$app_username, ".log_files")
		)

		post_model_metrics_objs <- Rautoml::post_model_metrics(
			models = models,
			outcome = config$outcome,
			df = config$test_df,
			task = config$task
		)

		write_status("running", "saving")

		Rautoml::save_post_metrics_plots(
			metric_list = post_model_metrics_objs,
			dataset_id = config$dataset_id,
			session_name = config$session_id,
			timestamp = Sys.time(),
			output_dir = file.path(config$app_username, "outputs")
		)

		saveRDS(
			list(
				model_id = config$model_id,
				model_label = config$model_label,
				models = models,
				tuned_parameters = tuned_parameters,
				control_parameters = control_parameters,
				train_metrics_df = train_metrics_df,
				test_metrics_objs = test_metrics_objs,
				post_model_metrics_objs = post_model_metrics_objs,
				model_safe_name_map = config$model_safe_name_map,
				raw_safe_name_map = config$raw_safe_name_map,
				completed_at = Sys.time()
			),
			result_path
		)

		write_status("completed", "ready")
		invisible(TRUE)
	}, error = function(e) {
		write_status("failed", "failed", conditionMessage(e))
		saveRDS(
			list(
				model_id = config$model_id,
				model_label = config$model_label,
				error = conditionMessage(e),
				failed_at = Sys.time()
			),
			result_path
		)
		stop(e)
	})
}
