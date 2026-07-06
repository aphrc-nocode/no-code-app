source("server/caret_job_worker.R", local = TRUE)

caret_job_manager_server <- function(
	input,
	output,
	session,
	rv_current,
	rv_ml_ai,
	rv_training_models,
	rv_train_control_caret,
	rv_training_results,
	app_username,
	get_rv_labels
) {
	job_processes <- new.env(parent = emptyenv())
	job_configs <- new.env(parent = emptyenv())
	job_results <- new.env(parent = emptyenv())
	job_table <- reactiveVal(data.frame())
	context_cache <- reactiveVal(NULL)
	context_version <- reactiveVal(0L)
	context_cache_token <- digest::digest(paste(Sys.time(), runif(1)), algo = "xxhash64")

	label <- function(key, fallback) {
		value <- tryCatch(as.character(get_rv_labels(key)), error = function(e) NULL)
		if (is.null(value) || length(value) == 0 || is.na(value[[1]]) || !nzchar(value[[1]])) {
			return(fallback)
		}
		value[[1]]
	}

	empty_jobs <- function() {
		data.frame(
			job_id = character(),
			model_id = character(),
			model_label = character(),
			status = character(),
			step = character(),
			started_at = as.POSIXct(character()),
			finished_at = as.POSIXct(character()),
			error = character(),
			result_path = character(),
			config_path = character(),
			stringsAsFactors = FALSE
		)
	}

	get_jobs <- function() {
		jobs <- job_table()
		if (is.null(jobs) || !NROW(jobs)) empty_jobs() else jobs
	}

	set_jobs <- function(jobs) {
		job_table(jobs)
	}

	has_active_run <- function(jobs = get_jobs()) {
		NROW(jobs) && any(jobs$status %in% c("queued", "running", "paused"), na.rm = TRUE)
	}

	sync_training_state <- function(jobs = get_jobs()) {
		rv_training_results$training_busy <- has_active_run(jobs)
		rv_training_results$training_completed <- NROW(jobs) && any(jobs$status == "completed", na.rm = TRUE)
	}

	clear_model_selection <- function(model_ids) {
		for (id in model_ids) {
			updatePrettyCheckbox(
				session,
				inputId = paste0("model_training_caret_models_", id, "_check"),
				value = FALSE
			)
		}
		rv_ml_ai$at_least_one_model <- FALSE
		invisible(NULL)
	}

	clear_completed_model_results <- function(model_ids) {
		result_names <- ls(job_results)
		if (!length(result_names) || !length(model_ids)) return(invisible(NULL))
		for (result_name in result_names) {
			result <- get(result_name, envir = job_results, inherits = FALSE)
			if (!is.null(result$model_id) && result$model_id %in% model_ids) {
				rm(list = result_name, envir = job_results)
			}
		}
		invisible(NULL)
	}

	update_job <- function(job_id, ...) {
		jobs <- get_jobs()
		idx <- which(jobs$job_id == job_id)
		if (!length(idx)) return(invisible(FALSE))
		values <- list(...)
		for (nm in names(values)) {
			jobs[[nm]][idx] <- values[[nm]]
		}
		set_jobs(jobs)
		sync_training_state(jobs)
		invisible(TRUE)
	}

	js_value <- function(x) {
		x <- gsub("\\\\", "\\\\\\\\", x)
		x <- gsub("'", "\\\\'", x)
		x
	}

	status_label <- function(status) {
		switch(
			status,
			queued = label("caret_jobs_status_queued", "Queued"),
			running = label("caret_jobs_status_running", "Running"),
			paused = label("caret_jobs_status_paused", "Paused"),
			stopped = label("caret_jobs_status_stopped", "Stopped"),
			completed = label("caret_jobs_status_completed", "Completed"),
			failed = label("caret_jobs_status_failed", "Failed"),
			status
		)
	}

	step_label <- function(step) {
		switch(
			step,
			queued = label("caret_jobs_step_queued", "Queued"),
			training = label("caret_jobs_step_training", "Training"),
			evaluating = label("caret_jobs_step_evaluating", "Checking performance"),
			saving = label("caret_jobs_step_saving", "Saving results"),
			ready = label("caret_jobs_step_ready", "Ready"),
			failed = label("caret_jobs_status_failed", "Failed"),
			paused = label("caret_jobs_status_paused", "Paused"),
			stopped = label("caret_jobs_status_stopped", "Stopped"),
			step
		)
	}

	job_status_label <- function(job) {
		status <- job$status[[1]]
		step <- job$step[[1]]
		if (identical(status, "running") && !is.na(step) && nzchar(step)) return(step_label(step))
		if (identical(status, "completed")) return(step_label("ready"))
		status_label(status)
	}

	status_color <- function(status) {
		switch(
			status,
			queued = "#9aa3a8",
			running = "#2196f3",
			paused = "#f6a100",
			stopped = "#777777",
			completed = "#4cae4c",
			failed = "#d9534f",
			"#9aa3a8"
		)
	}

	active_step <- function(job) {
		status <- job$status[[1]]
		step <- if (!is.null(job$step[[1]]) && !is.na(job$step[[1]]) && nzchar(job$step[[1]])) {
			job$step[[1]]
		} else {
			status
		}
		switch(
			status,
			queued = "queued",
			running = step,
			paused = step,
			stopped = step,
			completed = "ready",
			failed = step,
			step
		)
	}

	step_rank <- function(step) {
		steps <- c("queued", "training", "evaluating", "saving", "ready")
		idx <- match(step, steps)
		if (is.na(idx)) 1L else idx
	}

	stepper_icon <- function(status, current, done) {
		if (identical(status, "failed") && current) return("fa fa-exclamation-triangle")
		if (identical(status, "stopped") && current) return("fa fa-stop")
		if (identical(status, "paused") && current) return("fa fa-pause")
		if (identical(status, "running") && current) return("fa fa-spinner fa-spin")
		if (done || identical(status, "completed")) return("fa fa-check")
		if (current) return("fa fa-clock-o")
		"fa fa-circle-o"
	}

	job_stepper <- function(job) {
		status <- job$status[[1]]
		steps <- c("queued", "training", "evaluating", "saving", "ready")
		current_step <- active_step(job)
		current_rank <- step_rank(current_step)

		tags$div(
			class = "cmp-steps",
			lapply(seq_along(steps), function(i) {
				step <- steps[[i]]
				current <- identical(step, current_step)
				done <- i < current_rank || identical(status, "completed")
				step_class <- paste(
					"cmp-step",
					if (done) "is-done" else "",
					if (current) "is-current" else "",
					if (current && status %in% c("paused", "stopped", "failed")) paste0("is-", status) else ""
				)
				tags$div(
					class = step_class,
					tags$span(class = "cmp-step-icon", tags$i(class = stepper_icon(status, current, done))),
					tags$span(class = "cmp-step-label", step_label(step))
				)
			})
		)
	}

	action_button <- function(job_id, action, text, class = "btn btn-xs btn-default") {
		tags$button(
			type = "button",
			class = class,
			onclick = sprintf(
				"Shiny.setInputValue('caret_job_action', {job_id: '%s', action: '%s', nonce: Math.random()}, {priority: 'event'})",
				js_value(job_id),
				js_value(action)
			),
			text
		)
	}

	job_actions <- function(job) {
		status <- job$status
		job_id <- job$job_id
		if (identical(status, "running")) {
			tagList(
				action_button(job_id, "pause", label("caret_jobs_pause", "Pause"), "btn btn-xs btn-warning"),
				action_button(job_id, "stop", label("caret_jobs_stop", "Stop"), "btn btn-xs btn-danger")
			)
		} else if (identical(status, "queued")) {
			tagList(
				action_button(job_id, "pause", label("caret_jobs_pause", "Pause"), "btn btn-xs btn-warning"),
				action_button(job_id, "stop", label("caret_jobs_stop", "Stop"), "btn btn-xs btn-danger")
			)
		} else if (identical(status, "paused")) {
			tagList(
				action_button(job_id, "resume", label("caret_jobs_resume", "Resume"), "btn btn-xs btn-success"),
				action_button(job_id, "stop", label("caret_jobs_stop", "Stop"), "btn btn-xs btn-danger")
			)
		} else if (identical(status, "failed")) {
			tagList(
				action_button(job_id, "resume", label("caret_jobs_retry", "Retry"), "btn btn-xs btn-primary"),
				action_button(job_id, "clear", label("caret_jobs_clear", "Clear"), "btn btn-xs btn-default")
			)
		} else if (identical(status, "stopped")) {
			tagList(
				action_button(job_id, "resume", label("caret_jobs_resume", "Resume"), "btn btn-xs btn-success"),
				action_button(job_id, "clear", label("caret_jobs_clear", "Clear"), "btn btn-xs btn-default")
			)
		} else if (identical(status, "completed")) {
			action_button(job_id, "clear", label("caret_jobs_clear", "Clear"), "btn btn-xs btn-default")
		} else {
			action_button(job_id, "clear", label("caret_jobs_clear", "Clear"), "btn btn-xs btn-default")
		}
	}

	max_concurrent_jobs <- reactive({
		if (!isTRUE(input$model_training_setup_start_clusters_check)) return(1L)
		env_cap <- suppressWarnings(as.integer(Sys.getenv("NOCODE_CARET_MAX_JOBS", unset = NA_character_)))
		if (!is.na(env_cap) && env_cap >= 1L) return(env_cap)
		detected <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
		if (is.na(detected) || detected < 1L) return(2L)
		max(1L, detected - 1L)
	})

	observeEvent(rv_ml_ai$preprocessed, {
		context_version(context_version() + 1L)
		context_cache(NULL)
	}, ignoreInit = TRUE)

	# Pre-warm the context cache whenever preprocessing or the eval metric changes.
	# Uses observeEvent so reactive reads inside the handler are automatically
	# isolated — avoids "no active reactive context" errors from nested reads.
	observeEvent(
		list(rv_ml_ai$preprocessed, input$model_training_setup_eval_metric),
		{
			req(!is.null(rv_ml_ai$preprocessed))
			req(!is.null(input$model_training_setup_eval_metric))
			req(nzchar(input$model_training_setup_eval_metric))
			prewarm_dir <- file.path(app_username, ".caret_jobs", "_context_cache")
			dir.create(prewarm_dir, recursive = TRUE, showWarnings = FALSE)
			train_control <- reactiveValuesToList(rv_train_control_caret)
			tryCatch(
				get_training_context_path(prewarm_dir, train_control),
				error = function(e) NULL
			)
		},
		ignoreInit = TRUE,
		ignoreNULL = FALSE
	)

	training_context_signature <- function(train_control) {
		preprocessed <- rv_ml_ai$preprocessed
		train_df <- preprocessed$train_df
		test_df <- preprocessed$test_df
		digest::digest(
			list(
				dataset_id = rv_ml_ai$dataset_id,
				session_id = rv_ml_ai$session_id,
				outcome = rv_ml_ai$outcome,
				task = rv_ml_ai$task,
				metric = input$model_training_setup_eval_metric,
				seed = rv_ml_ai$seed_value,
				model_formula = paste(deparse(rv_ml_ai$model_formula), collapse = ""),
				train_dim = if (is.data.frame(train_df)) dim(train_df) else NULL,
				test_dim = if (is.data.frame(test_df)) dim(test_df) else NULL,
				train_names = if (is.data.frame(train_df)) names(train_df) else NULL,
				test_names = if (is.data.frame(test_df)) names(test_df) else NULL,
				context_cache_token = context_cache_token,
				context_version = context_version(),
				train_control = train_control
			),
			algo = "xxhash64"
		)
	}

	build_training_context <- function(train_control) {
		list(
			app_dir = getwd(),
			app_username = app_username,
			train_df = rv_ml_ai$preprocessed$train_df,
			test_df = rv_ml_ai$preprocessed$test_df,
			preprocessed = rv_ml_ai$preprocessed,
			model_formula = rv_ml_ai$model_formula,
			train_control = train_control,
			metric = input$model_training_setup_eval_metric,
			seed = rv_ml_ai$seed_value,
			use_cluster = FALSE,
			dataset_id = rv_ml_ai$dataset_id,
			session_id = rv_ml_ai$session_id,
			outcome = rv_ml_ai$outcome,
			task = rv_ml_ai$task
		)
	}

	get_training_context_path <- function(run_dir, train_control) {
		signature <- training_context_signature(train_control)
		cache <- context_cache()
		if (
			!is.null(cache) &&
			identical(cache$signature, signature) &&
			!is.null(cache$path) &&
			file.exists(cache$path)
		) {
			return(cache$path)
		}

		cache_dir <- file.path(app_username, ".caret_jobs", "_context_cache")
		dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
		context_path <- file.path(cache_dir, paste0(signature, ".rds"))
		if (!file.exists(context_path)) {
			saveRDS(build_training_context(train_control), context_path)
		}
		context_cache(list(signature = signature, path = context_path))
		context_path
	}

	write_training_context_path <- function(signature, training_context) {
		cache_dir <- file.path(app_username, ".caret_jobs", "_context_cache")
		dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
		context_path <- file.path(cache_dir, paste0(signature, ".rds"))
		if (!file.exists(context_path)) {
			saveRDS(training_context, context_path)
		}
		context_path
	}

	selected_model_jobs <- function(run_dir, skip_model_ids = character()) {
		models_state <- reactiveValuesToList(rv_training_models)
		model_ids <- rv_training_models$CARET_MODEL_IDS
		Filter(Negate(is.null), lapply(model_ids, function(id) {
			if (id %in% skip_model_ids) return(NULL)
			if (!isTRUE(input[[paste0("model_training_caret_models_", id, "_check")]])) return(NULL)
			model_spec <- models_state[[paste0(id, "_model")]]
			if (is.null(model_spec)) return(NULL)
			model_name <- models_state[[paste0(id, "_name")]]
			model_label <- if (!is.null(names(model_name)) && nzchar(names(model_name)[1])) {
				names(model_name)[1]
			} else {
				as.character(model_name[1])
			}
			job_id <- paste0(id, "-", digest::digest(paste(id, Sys.time(), runif(1)), algo = "xxhash32"))
			job_dir <- file.path(run_dir, job_id)
			dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
			list(
				job_id = job_id,
				model_id = id,
				model_label = model_label,
				model_list = model_spec,
				job_dir = job_dir
			)
		}))
	}

	write_job_config <- function(job, context_path) {
		config_path <- file.path(job$job_dir, "config.rds")
		result_path <- file.path(job$job_dir, "result.rds")
		status_path <- file.path(job$job_dir, "status.rds")
		config <- list(
			context_path = context_path,
			model_id = job$model_id,
			model_label = job$model_label,
			model_list = job$model_list,
			config_path = config_path,
			result_path = result_path,
			status_path = status_path
		)
		saveRDS(config, config_path)
		saveRDS(list(status = "queued", step = "queued", updated_at = Sys.time()), status_path)
		assign(job$job_id, config, envir = job_configs)
		config
	}

	start_job <- function(job_id) {
		if (!exists(job_id, envir = job_configs, inherits = FALSE)) return(FALSE)
		config <- get(job_id, envir = job_configs)
		proc <- callr::r_bg(
			func = caret_job_worker,
			args = list(config$config_path),
			stdout = file.path(dirname(config$config_path), "stdout.log"),
			stderr = file.path(dirname(config$config_path), "stderr.log"),
			supervise = TRUE
		)
		assign(job_id, proc, envir = job_processes)
		update_job(job_id, status = "running", step = "training", started_at = Sys.time(), error = "")
		TRUE
	}

	start_queued_jobs <- function() {
		jobs <- get_jobs()
		if (!NROW(jobs)) return(invisible(NULL))
		running <- sum(jobs$status == "running", na.rm = TRUE)
		available <- max(0L, max_concurrent_jobs() - running)
		if (!available) return(invisible(NULL))
		to_start <- head(jobs$job_id[jobs$status == "queued"], available)
		for (job_id in to_start) start_job(job_id)
		sync_training_state()
		invisible(NULL)
	}

	merge_job_results <- function() {
		results <- mget(ls(job_results), envir = job_results, inherits = FALSE)
		results <- results[!vapply(results, is.null, logical(1))]
		if (!length(results)) return(invisible(NULL))

		models_list <- lapply(results, `[[`, "models")
		names(models_list) <- NULL
		models <- do.call(c, models_list)
		class(models) <- unique(c("caretList", class(models)))
		rv_training_results$models <- models

		tuned <- do.call(c, lapply(results, `[[`, "tuned_parameters"))
		rv_training_results$tuned_parameters <- tuned

		rv_training_results$control_parameters     <- results[[1]]$control_parameters
		rv_training_results$model_safe_name_map   <- results[[1]]$model_safe_name_map

		train_metrics <- do.call(rbind, lapply(results, `[[`, "train_metrics_df"))
		class(train_metrics) <- unique(c("Rautomlmetric", class(train_metrics)))
		rv_training_results$train_metrics_df <- train_metrics

		test_specifics <- do.call(rbind, lapply(results, function(x) x$test_metrics_objs$specifics))
		test_all <- do.call(rbind, lapply(results, function(x) x$test_metrics_objs$all))
		test_roc <- do.call(rbind, lapply(results, function(x) x$test_metrics_objs$roc_df))
		test_metrics <- list(
			specifics = test_specifics,
			all = test_all,
			roc_df = test_roc,
			positive_cat = results[[1]]$test_metrics_objs$positive_cat
		)
		class(test_metrics) <- c("Rautomlmetric2", "list")
		rv_training_results$test_metrics_objs <- test_metrics

		post_list <- lapply(results, `[[`, "post_model_metrics_objs")
		names(post_list) <- NULL
		post_metrics <- do.call(c, post_list)
		rv_training_results$post_model_metrics_objs <- post_metrics
		invisible(NULL)
	}

	poll_jobs <- function() {
		jobs <- get_jobs()
		if (!NROW(jobs)) return(invisible(NULL))

		new_completion <- FALSE

		for (job_id in jobs$job_id[jobs$status == "running"]) {
			proc <- if (exists(job_id, envir = job_processes, inherits = FALSE)) {
				get(job_id, envir = job_processes)
			} else {
				NULL
			}
			config <- get(job_id, envir = job_configs)
			if (!is.null(proc) && proc$is_alive()) {
				status <- tryCatch(suppressWarnings(readRDS(config$status_path)), error = function(e) NULL)
				result <- tryCatch(suppressWarnings(readRDS(config$result_path)), error = function(e) NULL)
				if (!is.null(status) && identical(status$status, "failed")) {
					msg <- if (!is.null(status$message)) status$message else label("caret_jobs_status_failed", "Failed")
					if (!is.null(result$error)) msg <- result$error
					update_job(job_id, status = "failed", step = "failed", finished_at = Sys.time(), error = msg)
					next
				}
				if (!is.null(status) && identical(status$status, "completed") && !is.null(result) && is.null(result$error)) {
					assign(job_id, result, envir = job_results)
					update_job(job_id, status = "completed", step = "ready", finished_at = Sys.time(), error = "")
					new_completion <- TRUE
					next
				}
				current_step <- jobs$step[jobs$job_id == job_id][[1]]
				status_step <- status$step
				if (identical(status_step, "queued") && identical(current_step, "training")) {
					next
				}
				if (!is.null(status_step) && !identical(status_step, current_step)) {
					update_job(job_id, step = status_step)
				}
				next
			}
			if (is.null(proc)) next

			status <- tryCatch(suppressWarnings(readRDS(config$status_path)), error = function(e) NULL)
			result <- tryCatch(suppressWarnings(readRDS(config$result_path)), error = function(e) NULL)
			exit_status <- proc$get_exit_status()

			if (!is.null(status) && identical(status$status, "completed") && !is.null(result) && is.null(result$error)) {
				assign(job_id, result, envir = job_results)
				update_job(job_id, status = "completed", step = "ready", finished_at = Sys.time(), error = "")
				new_completion <- TRUE
			} else {
				msg <- if (!is.null(status$message)) status$message else paste("Process exited with status", exit_status)
				if (!is.null(result$error)) msg <- result$error
				update_job(job_id, status = "failed", step = "failed", finished_at = Sys.time(), error = msg)
			}
		}

		if (new_completion) merge_job_results()
		start_queued_jobs()
		invisible(NULL)
	}

	observeEvent(input$model_training_apply, {
		req(!is.null(rv_current$working_df))
		req(!is.null(rv_ml_ai$preprocessed))
		req(isTRUE(rv_ml_ai$at_least_one_model))

		if (isTRUE(input$model_training_setup_include_ensemble_check)) {
			showNotification(
				label("caret_jobs_ensemble_deferred", "The async queue will train the selected base models now. Ensemble training needs a dependent background job and is not included in this run."),
				type = "warning",
				duration = 8
			)
		}

		run_id <- format(Sys.time(), "%Y%m%d%H%M%S")
		run_dir <- file.path(app_username, ".caret_jobs", run_id)
		dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
		dir.create(file.path(app_username, "models"), recursive = TRUE, showWarnings = FALSE)
		dir.create(file.path(app_username, "recipes"), recursive = TRUE, showWarnings = FALSE)
		dir.create(file.path(app_username, "outputs"), recursive = TRUE, showWarnings = FALSE)
		dir.create(file.path(app_username, ".log_files"), recursive = TRUE, showWarnings = FALSE)

		selected_groups <- input$feature_engineering_perform_partition_group
		has_groups <- !is.null(selected_groups) && length(selected_groups) > 0 && !any(selected_groups %in% "")
		if (isTRUE(has_groups)) {
			if (isTRUE(!is.null(rv_ml_ai$fold_index))) {
				rv_train_control_caret$index <- Rautoml::create_grouped_index(
					rv_ml_ai$fold_index,
					k = rv_train_control_caret$number
				)
			} else {
				rv_train_control_caret$index <- NULL
			}
		}

		train_control <- reactiveValuesToList(rv_train_control_caret)

		current_jobs <- get_jobs()
		active_model_ids <- if (NROW(current_jobs)) {
			current_jobs$model_id[current_jobs$status %in% c("queued", "running", "paused")]
		} else {
			character()
		}
		selected_input_ids <- rv_training_models$CARET_MODEL_IDS[vapply(rv_training_models$CARET_MODEL_IDS, function(id) {
			isTRUE(input[[paste0("model_training_caret_models_", id, "_check")]])
		}, logical(1))]
		jobs <- selected_model_jobs(run_dir, skip_model_ids = active_model_ids)

		if (!length(jobs)) {
			showNotification(
				label("caret_jobs_no_new_models", "The selected models are already in the training queue or results panel."),
				type = "message"
			)
			clear_model_selection(selected_input_ids)
			return(invisible(NULL))
		}

		skipped_count <- sum(selected_input_ids %in% active_model_ids)
		if (skipped_count > 0) {
			showNotification(
				label("caret_jobs_duplicate_skipped", "Some selected models were already in the queue and were not added again."),
				type = "message"
			)
		}

		rerun_model_ids <- vapply(jobs, `[[`, character(1), "model_id")
		clear_completed_model_results(rerun_model_ids)
		if (NROW(current_jobs)) {
			current_jobs <- current_jobs[!(
				current_jobs$status == "completed" &
				current_jobs$model_id %in% rerun_model_ids
			), , drop = FALSE]
		}

		job_rows <- lapply(jobs, function(job) {
			data.frame(
				job_id = job$job_id,
				model_id = job$model_id,
				model_label = job$model_label,
				status = "queued",
				step = "queued",
				started_at = as.POSIXct(NA),
				finished_at = as.POSIXct(NA),
				error = "",
				result_path = file.path(job$job_dir, "result.rds"),
				config_path = file.path(job$job_dir, "config.rds"),
				stringsAsFactors = FALSE
			)
		})
		set_jobs(rbind(current_jobs, do.call(rbind, job_rows)))
		sync_training_state()
		clear_model_selection(selected_input_ids)

		target_job_ids <- vapply(jobs, `[[`, character(1), "job_id")
		session$onFlushed(function() {
			later::later(function() {
				isolate({
					current <- get_jobs()
					if (!NROW(current)) return(invisible(NULL))
					queued_ids <- current$job_id[
						current$job_id %in% target_job_ids &
						current$status %in% c("queued", "paused")
					]
					if (!length(queued_ids)) return(invisible(NULL))
					context_signature <- training_context_signature(train_control)
					training_context <- build_training_context(train_control)
					context_path <- write_training_context_path(context_signature, training_context)
					for (job in jobs) {
						if (job$job_id %in% queued_ids) write_job_config(job, context_path)
					}
					start_queued_jobs()
				})
			}, delay = 0)
		}, once = TRUE)
	}, ignoreInit = TRUE)

	observeEvent(input$caret_job_action, {
		action <- input$caret_job_action$action
		job_id <- input$caret_job_action$job_id
		jobs <- get_jobs()
		if (!NROW(jobs)) return(invisible(NULL))

		if (identical(action, "clear_completed")) {
			jobs <- jobs[jobs$status != "completed", , drop = FALSE]
			set_jobs(jobs)
			sync_training_state(jobs)
			return(invisible(NULL))
		}

		if (!(job_id %in% jobs$job_id)) return(invisible(NULL))

		status <- jobs$status[jobs$job_id == job_id][[1]]
		if (action == "pause") {
			if (identical(status, "running") && exists(job_id, envir = job_processes, inherits = FALSE)) {
				proc <- get(job_id, envir = job_processes)
				if (proc$is_alive()) proc$kill()
			}
			update_job(job_id, status = "paused", finished_at = Sys.time())
		} else if (action == "stop") {
			if (identical(status, "running") && exists(job_id, envir = job_processes, inherits = FALSE)) {
				proc <- get(job_id, envir = job_processes)
				if (proc$is_alive()) proc$kill()
			}
			update_job(job_id, status = "stopped", finished_at = Sys.time())
		} else if (action == "resume") {
			update_job(job_id, status = "queued", step = "queued", started_at = as.POSIXct(NA), finished_at = as.POSIXct(NA), error = "")
		} else if (action == "clear") {
			jobs <- jobs[jobs$job_id != job_id, , drop = FALSE]
			set_jobs(jobs)
			sync_training_state(jobs)
		}

		poll_jobs()
	}, ignoreInit = TRUE)

	observe({
		jobs <- get_jobs()
		if (NROW(jobs) && any(jobs$status %in% c("queued", "running"))) {
			invalidateLater(2000, session)
			poll_jobs()
		}
	})

	output$caret_job_queue_panel <- renderUI({
		jobs <- get_jobs()
		if (!NROW(jobs)) return(NULL)

		count <- function(status) sum(jobs$status == status, na.rm = TRUE)
		rows <- lapply(seq_len(nrow(jobs)), function(i) {
			job <- jobs[i, , drop = FALSE]
			status <- job$status[[1]]

			tags$div(
				class = "cmp-row",
				tags$div(class = "cmp-num", paste0(i, ".")),
				tags$div(class = "cmp-name", job$model_label),
				tags$div(
					class = "cmp-status",
					tags$span(class = "cmp-sdot", style = paste0("background:", status_color(status), ";")),
					tags$span(class = "cmp-slabel", job_status_label(job))
				),
				job_stepper(job),
				tags$div(class = "cmp-action", job_actions(job))
			)
		})

		tags$div(
			id = "caret-model-progress",
			class = "cmp-visible",
			tags$div(
				class = "cmp-header",
				tags$div(class = "cmp-title", label("caret_jobs_progress_title", "Training queue")),
				tags$div(
					class = "cmp-counts",
					tags$span(tags$span(class = "cmp-count-dot", style = "background:#2196f3;"), paste(count("running"), label("caret_jobs_count_running", "running"))),
					tags$span("\u2022"),
					tags$span(tags$span(class = "cmp-count-dot", style = "background:#9aa3a8;"), paste(count("queued"), label("caret_jobs_count_queued", "queued"))),
					tags$span("\u2022"),
					tags$span(tags$span(class = "cmp-count-dot", style = "background:#f6a100;"), paste(count("paused"), label("caret_jobs_count_paused", "paused"))),
					tags$span("\u2022"),
					tags$span(tags$span(class = "cmp-count-dot", style = "background:#4cae4c;"), paste(count("completed"), label("caret_jobs_count_completed", "completed"))),
					if (count("completed") > 0) {
						action_button("completed", "clear_completed", label("caret_jobs_clear_completed", "Clear completed"), "btn btn-xs btn-default")
					}
				)
			),
			tags$div(class = "cmp-list", rows),
			tags$div(
				class = "cmp-footer",
				tags$i(class = "fa fa-info-circle"),
				paste0(" ", label("caret_jobs_footer_note", "Completed models remain available for comparison and deployment."))
			)
		)
	})
	outputOptions(output, "caret_job_queue_panel", suspendWhenHidden = FALSE)
}
