# server.R

library(shiny)
library(httr2)
library(jsonlite)
library(DT)
library(dplyr)
library(shinyjs)
library(tidyr)
library(dygraphs)

# Increase max file upload size (e.g., to 10GB)
options(shiny.maxRequestSize = 10000 * 1024^2)

deep_learning <- function() {
    # Use environment variable for API URL, default to localhost for dev
    api_url <- Sys.getenv("DL_API_URL", "http://localhost:8000")
    api_key <- Sys.getenv("DL_API_KEY", "aphrc-secret-key-123")

    # ==============================================================================
    # == 1. CORE REACTIVE VALUES
    # ==============================================================================

    # --- For Live Job Polling ---
    polled_data <- reactiveVal(list(
        status = "Idle", task = "N/A", log = "", progress = list(percentage = 0, text = "Idle")
    ))
    polled_metrics <- reactiveVal(NULL)
    active_job_id <- reactiveVal(NULL)

    # --- For System Health ---
    health_data <- reactiveVal(NULL)

    # --- For Data Management ---
    data_upload_status <- reactiveVal("")
    processing_dataset_id <- reactiveVal(NULL)
    refresh_data_trigger <- reactiveVal(0) # Triggers reload of dataset lists

    # --- For Model Registry ---
    model_registry <- reactiveVal(NULL)

    # --- For History Tab ---
    history_metrics <- reactiveVal(NULL)
    history_jobs_df <- reactiveVal(NULL)
    history_poller_active <- reactiveVal(FALSE)
    historical_log_text <- reactiveVal("")

    # --- For Inference Tab ---
    obj_inference_result <- reactiveVal(list(status = "Ready", image_url = NULL, error = NULL))
    img_class_inference_result <- reactiveVal(list(status = "Ready", prediction = NULL, error = NULL))
    seg_inference_result <- reactiveVal(list(status = "Ready", image_url = NULL, error = NULL))

    # --- Table Proxies ---
    proxy_eval_table <- dataTableProxy("eval_table")
    proxy_dataset_table <- dataTableProxy("dataset_table")
    proxy_history_eval_table <- dataTableProxy("history_eval_table")


    # ==============================================================================
    # == 2. INITIALIZATION & SIDEBAR LOGIC
    # ==============================================================================

    # --- Fetch Model Registry on Startup ---
    observe({
        # Only fetch if we are actually looking at the deep learning tab
        req(input$tabs == "cnndeep")

        tryCatch(
            {
                req <- request(paste0(api_url, "/models/list"))
                resp <- req_perform(req)
                model_registry(resp_body_json(resp))
            },
            error = function(e) {
                # print(paste("Failed to fetch model registry:", e$message))
                shinyalert::shinyalert(
                    title = "Deep Learning Service Unavailable",
                    text = paste("Could not connect to the Deep Learning API at", api_url, "\nError:", e$message),
                    type = "warning"
                )
            }
        )
    })

    # --- Task Panel Switching ---
    # Show/hide the correct training UI based on the main task selector
    observe({
        task <- input$task_selector
        if (task == "object_detection") {
            shinyjs::show("obj_panel")
            shinyjs::hide("img_class_panel")
            shinyjs::hide("seg_panel")
        } else if (task == "image_classification") {
            shinyjs::hide("obj_panel")
            shinyjs::show("img_class_panel")
            shinyjs::hide("seg_panel")
        } else if (task == "semantic_segmentation") {
            shinyjs::hide("obj_panel")
            shinyjs::hide("img_class_panel")
            shinyjs::show("seg_panel")
        }
    })

    # --- Chained Dropdown Logic (Populate Architectures) ---
    observeEvent(c(model_registry(), input$task_selector), {
        req(model_registry())

        task_slug <- input$task_selector
        arch_choices <- c("Loading..." = "")

        if (task_slug == "object_detection") {
            arch_choices <- names(model_registry()$object_detection)
            updateSelectInput(session, "obj_model_arch", choices = arch_choices)
        } else if (task_slug == "image_classification") {
            arch_choices <- names(model_registry()$image_classification)
            updateSelectInput(session, "img_class_model_arch", choices = arch_choices)
        } else if (task_slug == "image_segmentation") {
            arch_choices <- names(model_registry()$image_segmentation)
            updateSelectInput(session, "seg_model_arch", choices = arch_choices)
        }
    })

    # --- Chained Dropdown Logic (Populate Checkpoints) ---
    observeEvent(input$obj_model_arch, {
        req(model_registry(), input$obj_model_arch, input$obj_model_arch != "Loading...")
        checkpoints <- model_registry()$object_detection[[input$obj_model_arch]]
        updateSelectInput(session, "obj_model_checkpoint", choices = checkpoints)
    })
    observeEvent(input$img_class_model_arch, {
        req(model_registry(), input$img_class_model_arch, input$img_class_model_arch != "Loading...")
        checkpoints <- model_registry()$image_classification[[input$img_class_model_arch]]
        updateSelectInput(session, "img_class_model_checkpoint", choices = checkpoints)
    })
    observeEvent(input$seg_model_arch, {
        req(model_registry(), input$seg_model_arch, input$seg_model_arch != "Loading...")
        checkpoints <- model_registry()$image_segmentation[[input$seg_model_arch]]
        updateSelectInput(session, "seg_model_checkpoint", choices = checkpoints)
    })


    # --- Auto-Resume Active Job Logic ---
    # Trigger ONLY once when the module loads to check for existing running jobs
    observe({
        # observeEvent(input$obj_model_checkpoint) logic removed to prevent conflict with ui/deeplearning_ui.R
        # Only run if we don't have an active job yet
        if (is.null(active_job_id())) {
            tryCatch(
                {
                    # Fetch list of ALL running jobs
                    req <- request(paste0(api_url, "/jobs/list")) %>%
                        req_url_query(status = "running")
                    resp <- req_perform(req)
                    running_jobs <- resp_body_json(resp, simplifyVector = TRUE)

                    # Check if we got a valid data frame with rows
                    if (!is.null(running_jobs) && is.data.frame(running_jobs) && nrow(running_jobs) > 0) {
                        # Pick the most recent one (assuming higher ID or just first one)
                        # The API returns them in default order (usually creation time)
                        # We'll take the first one for now.
                        resumed_job_id <- running_jobs$id[1]
                        resumed_task_type <- running_jobs$task_type[1]

                        print(paste("Auto-resuming job:", resumed_job_id))

                        # Set as active
                        active_job_id(resumed_job_id)

                        # Switch UI tab to Live Training? (Optional, might annoy user if they just want to browse)
                        # For now, just notifying via alert
                        shinyalert::shinyalert(
                            title = "Job Resumed",
                            text = paste("Detected running job:", resumed_job_id, "\nResuming live monitoring."),
                            type = "info",
                            timer = 3000
                        )
                    }
                },
                error = function(e) {
                    # Silent fail on auto-resume check
                    print(paste("Auto-resume check failed:", e$message))
                }
            )
        }
    })
    # ==============================================================================
    # == 3. "DATA MANAGEMENT" TAB LOGIC
    # ==============================================================================

    # --- Helper: Load Datasets for a specific task ---
    load_datasets_for_task <- function(task_slug) {
        tryCatch(
            {
                req <- request(paste0(api_url, "/data/list/", task_slug))
                resp_data <- resp_body_json(req_perform(req), simplifyVector = TRUE)
                if (length(resp_data) > 0 && nrow(resp_data) > 0) {
                    setNames(resp_data$id, resp_data$name)
                } else {
                    c("No datasets found" = "")
                }
            },
            error = function(e) {
                c("Error loading datasets" = "")
            }
        )
    }

    # --- Auto-refresh Dataset Dropdowns ---
    # Triggered by: 1. Task selector change, 2. Data refresh trigger
    observeEvent(c(input$task_selector, refresh_data_trigger()),
        {
            task_slug <- input$task_selector
            if (task_slug == "object_detection") {
                updateSelectInput(session, "obj_dataset_id", choices = load_datasets_for_task("object_detection"))
            } else if (task_slug == "image_classification") {
                updateSelectInput(session, "img_class_dataset_id", choices = load_datasets_for_task("image_classification"))
            } else if (task_slug == "semantic_segmentation") {
                updateSelectInput(session, "seg_dataset_id", choices = load_datasets_for_task("semantic_segmentation"))
            }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
    )

    # Manually trigger first data load on startup (after registry is loaded)
    observeEvent(model_registry(),
        {
            req(model_registry())
            refresh_data_trigger(refresh_data_trigger() + 1)
        },
        once = TRUE
    )

    # --- 3.1: Handle File Upload (Async) ---
    observeEvent(input$start_data_upload, {
        # Explicit validation instead of silent req() failure
        if (is.null(input$new_data_name) || input$new_data_name == "") {
            shinyalert::shinyalert("Missing Name", "Please enter a name for the dataset.", type = "warning")
            return()
        }
        if (is.null(input$new_data_zip)) {
            shinyalert::shinyalert("Missing File", "Please upload a file first.", type = "warning")
            return()
        }
        req(input$new_data_task_type) # SelectInput almost always has a value

        # UI Feedback - Immediate
        data_upload_status("Step 1/3: Reading file in R...")

        file_path <- input$new_data_zip$datapath
        data_name_val <- input$new_data_name
        task_type_val <- input$new_data_task_type

        # Initialize Progress Bar
        prog <- shiny::Progress$new()
        prog$set(message = "Step 2/3: Uploading to Backend...", value = 0.5, detail = "This may take a moment for large files.")
        data_upload_status("Step 2/3: Uploading file from R to Backend... Please wait.")

        # 3. ASYNC UPLOAD (Future)
        future_promise({
            # This step sends the file from R to Python. It can be slow for large files.

            # Use local variables captured outside future_promise to prevent cross-scope errors
            req <- request(paste0(api_url, "/data/upload/", task_type_val)) %>%
                req_headers("X-API-Key" = api_key) %>%
                req_body_multipart(
                    task_type = task_type_val,
                    data_name = data_name_val,
                    data_file = curl::form_file(file_path)
                ) %>%
                req_timeout(600) # 10 minutes timeout for large files

            resp <- req_perform(req)
            resp_body_json(resp)
        }) %...>% (function(resp_data) {
            # 4. SUCCESS (Back in Main Thread)
            prog$close()

            # Trigger the existing poller logic
            processing_dataset_id(resp_data$dataset_id)
            data_upload_status(paste("Upload complete. Server is now unpacking and validating (Dataset ID:", resp_data$dataset_id, ")..."))
        }) %...!% (function(error) {
            # 5. FAILURE (Back in Main Thread)
            prog$close()

            error_message <- as.character(error$message)
            if (!is.null(error$body)) {
                error_message <- paste("API Error:", error$body)
            }

            data_upload_status(paste("Error during upload:", error_message))
            shinyalert::shinyalert("Upload Failed", error_message, type = "error")
        })

        NULL
    })

    # --- Poller for Data Processing Status ---
    observe({
        ds_id <- processing_dataset_id()
        req(ds_id) # Only run if we are processing a dataset

        invalidateLater(2000, session) # Poll every 2 seconds

        tryCatch(
            {
                req_status <- request(paste0(api_url, "/data/status/", ds_id))
                resp <- req_perform(req_status)
                status_data <- resp_body_json(resp)

                if (status_data$status == "ready" || status_data$status == "failed") {
                    processing_dataset_id(NULL) # Stop polling

                    # --- AUTO-REFRESH ---
                    refresh_data_trigger(refresh_data_trigger() + 1)

                    if (status_data$status == "ready") {
                        data_upload_status(paste("Dataset processing complete!"))
                    } else {
                        data_upload_status(paste("Dataset processing failed:", status_data$error))
                    }
                } else {
                    data_upload_status(paste("Processing dataset...", status_data$status))
                }
            },
            error = function(e) {
                data_upload_status("Error polling data status.")
                processing_dataset_id(NULL) # Stop polling on error
            }
        )
    })

    # --- Data Management UI Outputs ---
    output$data_upload_status <- renderText({
        data_upload_status()
    })

    # --- Reactive: Fetch Available Datasets (Cached) ---
    datasets_reactive <- reactive({
        refresh_data_trigger() # React to the trigger
        tryCatch(
            {
                tasks <- c("object_detection", "image_classification", "semantic_segmentation")
                # Make parallel requests for better performance
                reqs <- lapply(tasks, function(task) request(paste0(api_url, "/data/list/", task)))
                resps <- httr2::req_perform_parallel(reqs, on_error = "continue")

                all_datasets <- lapply(seq_along(resps), function(i) {
                    resp <- resps[[i]]
                    if (!inherits(resp, "error") && httr2::resp_status(resp) == 200) {
                        resp_data <- httr2::resp_body_json(resp, simplifyVector = TRUE)
                        if (length(resp_data) > 0 && is.data.frame(resp_data) && nrow(resp_data) > 0) {
                            resp_data$task_type <- tasks[i]
                            return(resp_data)
                        }
                    }
                    return(NULL)
                })
                bind_rows(all_datasets)
            },
            error = function(e) {
                NULL
            }
        )
    })

    output$dataset_table <- renderDT(
        {
            df <- datasets_reactive()
            req(df)

            # Add Actions Column with Delete Button
            if (nrow(df) > 0) {
                df$actions <- sapply(df$id, function(id) {
                    as.character(tags$button(
                        class = "btn btn-danger btn-xs",
                        onclick = sprintf("Shiny.setInputValue('dl_delete_dataset_click', '%s', {priority: 'event'});", id),
                        icon("trash"), " Delete"
                    ))
                })
            }
            return(df)
        },
        escape = FALSE
    ) # Important: Allow HTML for buttons

    # --- Handle Dataset Deletion ---
    observeEvent(input$dl_delete_dataset_click, {
        ds_id <- input$dl_delete_dataset_click
        shinyalert::shinyalert(
            title = "Delete Dataset?",
            text = "Are you sure? This will permanently delete the dataset files from the server.",
            type = "warning",
            showCancelButton = TRUE,
            confirmButtonCol = "#DD6B55",
            confirmButtonText = "Yes, delete it",
            callbackR = function(x) {
                if (x) shinyjs::runjs(sprintf("Shiny.setInputValue('dl_delete_dataset_confirmed', '%s', {priority: 'event'});", ds_id))
            }
        )
    })

    observeEvent(input$dl_delete_dataset_confirmed, {
        ds_id <- input$dl_delete_dataset_confirmed
        tryCatch(
            {
                req <- request(paste0(api_url, "/data/", ds_id)) %>%
                    req_headers("X-API-Key" = api_key) %>%
                    req_method("DELETE")
                resp <- req_perform(req)
                if (resp_status(resp) == 200) {
                    shinyalert::shinyalert("Deleted!", "Dataset has been removed.", type = "success")
                    refresh_data_trigger(refresh_data_trigger() + 1) # Reload table
                } else {
                    shinyalert::shinyalert("Error", "Failed to delete dataset.", type = "error")
                }
            },
            error = function(e) {
                shinyalert::shinyalert("Error", as.character(e), type = "error")
            }
        )
    })


    # ==============================================================================
    # == 4. TRAINING JOB SUBMISSION (One per task)
    # ==============================================================================

    # --- Helper: Resets UI before starting a job ---
    reset_live_training_ui <- function(task_name) {
        replaceData(proxy_eval_table, NULL, resetPaging = TRUE, clearSelection = TRUE)
        active_job_id(NULL)
        polled_metrics(NULL)
        polled_data(list(status = "Submitting...", task = task_name, log = "Submitting job..."))

        # Redirect AND Reset Wizard
        updateTabsetPanel(session, "main_tabs", selected = "Job Manager") # Fixed Tab Name
        updateTabsetPanel(session, "wizard_tabs", selected = "step1") # Reset Wizard
        updateTextInput(session, "obj_run_name", value = "shiny-obj-run") # Optional: reset name
        # Reset other inputs if needed...
    }

    # --- 4.1: Object Detection Job ---
    observeEvent(input$start_obj_job, {
        req(input$obj_dataset_id, input$obj_model_checkpoint)
        reset_live_training_ui("Object Detection")

        tryCatch(
            {
                req <- request(paste0(api_url, "/train/object-detection")) %>%
                    req_headers("X-API-Key" = api_key) %>%
                    req_body_multipart(
                        # Common Params
                        dataset_id = as.character(input$obj_dataset_id),
                        model_checkpoint = as.character(input$obj_model_checkpoint),
                        run_name = as.character(input$obj_run_name),
                        version = as.character(input$obj_version),
                        epochs = as.character(input$obj_epochs),
                        train_batch_size = as.character(input$obj_train_batch_size),
                        max_image_size = as.character(input$obj_max_image_size),
                        seed = as.character(input$obj_seed),
                        early_stopping_patience = as.character(input$obj_early_stopping_patience),

                        # HF-Specific Params (Transformers)
                        eval_batch_size = as.character(input$obj_eval_batch_size),
                        learning_rate = as.character(input$obj_learning_rate),
                        weight_decay_hf = as.character(input$obj_weight_decay_hf),
                        gradient_accumulation_steps = as.character(input$obj_gradient_accumulation_steps),
                        gradient_checkpointing = if (isTRUE(input$obj_gradient_checkpointing)) "true" else "false",
                        early_stopping_threshold = as.character(input$obj_early_stopping_threshold),

                        # Hugging Face specific arguments
                        optimizer_hf = as.character(input$obj_optimizer_hf),
                        scheduler_hf = as.character(input$obj_scheduler_hf),
                        warmup_epochs_hf = as.character(input$obj_warmup_epochs_hf),

                        # YOLO-Specific Params
                        warmup_epochs = as.character(input$obj_warmup_epochs),
                        momentum = as.character(input$obj_momentum),
                        optimizer = as.character(input$obj_optimizer),
                        weight_decay_yolo = as.character(input$obj_weight_decay),
                        lr0 = as.character(input$obj_lr0),

                        # Augmentation
                        enable_augmentation = if (isTRUE(input$obj_enable_augment)) "true" else "false",
                        flip_prob = as.character(input$obj_flip_prob),
                        rotate_limit = as.character(input$obj_rotate_limit),
                        brightness = as.character(input$obj_brightness),
                        contrast = as.character(input$obj_contrast),
                        mosaic = as.character(input$obj_mosaic),
                        mixup = as.character(input$obj_mixup),
                        hsv_h = as.character(input$obj_hsv_h),
                        hsv_s = as.character(input$obj_hsv_s),
                        hsv_v = as.character(input$obj_hsv_v)
                    )

                resp <- req_perform(req)
                resp_data <- resp_body_json(resp)
                active_job_id(resp_data$job_id)
                polled_data(list(status = "Queued", task = "Object Detection", log = "Job is queued."))
            },
            error = function(e) {
                error_message <- as.character(e$message)
                if (!is.null(e$body)) {
                    error_message <- paste("API Error:", e$body)
                }
                polled_data(list(status = "Error", task = "Object Detection", log = error_message))
            }
        )
    })


    # --- 4.3: Image Classification Job ---
    observeEvent(input$start_img_class_job, {
        req(input$img_class_dataset_id, input$img_class_model_checkpoint)
        reset_live_training_ui("Image Classification")

        tryCatch(
            {
                req <- request(paste0(api_url, "/train/image-classification")) %>%
                    req_headers("X-API-Key" = api_key) %>%
                    req_body_multipart(
                        dataset_id = as.character(input$img_class_dataset_id),
                        model_checkpoint = as.character(input$img_class_model_checkpoint),
                        run_name = as.character(input$img_class_run_name),
                        version = as.character(input$img_class_version),
                        epochs = as.character(input$img_class_num_train_epochs),
                        learning_rate = as.character(input$img_class_learning_rate),
                        weight_decay = as.character(input$img_class_weight_decay),
                        train_batch_size = as.character(input$img_class_per_device_train_batch_size),
                        eval_batch_size = as.character(input$img_class_eval_batch_size),
                        max_image_size = as.character(input$img_class_max_image_size),
                        gradient_accumulation_steps = as.character(input$img_class_gradient_accumulation_steps),
                        gradient_checkpointing = if (isTRUE(input$img_class_gradient_checkpointing)) "true" else "false",
                        early_stopping_patience = as.character(input$img_class_early_stopping_patience),
                        early_stopping_threshold = as.character(input$img_class_early_stopping_threshold),
                        seed = as.character(input$img_class_seed),

                        # Optimizer configuration
                        optimizer = as.character(input$img_class_optimizer),
                        scheduler = as.character(input$img_class_scheduler),
                        warmup_epochs = as.character(input$img_class_warmup_epochs),

                        # Data augmentation configuration
                        enable_augmentation = if (isTRUE(input$img_class_enable_augment)) "true" else "false",
                        flip_prob = as.character(input$img_class_flip_prob),
                        rotate_limit = as.character(input$img_class_rotate_limit),
                        brightness = as.character(input$img_class_brightness),
                        contrast = as.character(input$img_class_contrast)
                    )

                resp <- req_perform(req)
                active_job_id(resp_body_json(resp)$job_id)
            },
            error = function(e) {
                polled_data(list(status = "Error", task = "Image Classification", log = as.character(e)))
            }
        )
    })

    # --- 4.4: Image Segmentation Job ---
    observeEvent(input$start_seg_job, {
        req(input$seg_dataset_id, input$seg_model_checkpoint)
        reset_live_training_ui("Image Segmentation")

        tryCatch(
            {
                req <- request(paste0(api_url, "/train/image-segmentation")) %>%
                    req_headers("X-API-Key" = api_key) %>%
                    req_body_multipart(
                        dataset_id = as.character(input$seg_dataset_id),
                        model_checkpoint = as.character(input$seg_model_checkpoint),
                        run_name = as.character(input$seg_run_name),
                        version = as.character(input$seg_version),
                        epochs = as.character(input$seg_num_train_epochs),
                        learning_rate = as.character(input$seg_learning_rate),
                        weight_decay = as.character(input$seg_weight_decay),
                        train_batch_size = as.character(input$seg_per_device_train_batch_size),
                        eval_batch_size = as.character(input$seg_eval_batch_size),
                        max_image_size = as.character(input$seg_max_image_size),
                        gradient_accumulation_steps = as.character(input$seg_gradient_accumulation_steps),
                        gradient_checkpointing = if (isTRUE(input$seg_gradient_checkpointing)) "true" else "false",
                        early_stopping_patience = as.character(input$seg_early_stopping_patience),
                        early_stopping_threshold = as.character(input$seg_early_stopping_threshold),
                        seed = as.character(input$seg_seed),

                        # Optimizer configuration
                        optimizer = as.character(input$seg_optimizer),
                        scheduler = as.character(input$seg_scheduler),
                        warmup_epochs = as.character(input$seg_warmup_epochs),

                        # Data augmentation configuration
                        enable_augmentation = if (isTRUE(input$seg_enable_augment)) "true" else "false",
                        flip_prob = as.character(input$seg_flip_prob),
                        rotate_limit = as.character(input$seg_rotate_limit),
                        brightness = as.character(input$seg_brightness),
                        contrast = as.character(input$seg_contrast)
                    )

                resp <- req_perform(req)
                active_job_id(resp_body_json(resp)$job_id)
            },
            error = function(e) {
                error_message <- as.character(e$message)
                if (!is.null(e$body)) {
                    error_message <- paste("API Error:", e$body)
                }
                polled_data(list(status = "Error", task = "Image Segmentation", log = error_message))
            }
        )
    })


    # ==============================================================================
    # == WIZARD NAVIGATION LOGIC
    # ==============================================================================
    observeEvent(input$wiz_next_1, {
        updateTabsetPanel(session, "wizard_tabs", selected = "step2")
    })

    observeEvent(input$wiz_next_2, {
        updateTabsetPanel(session, "wizard_tabs", selected = "step3")
    })
    observeEvent(input$wiz_back_2, {
        updateTabsetPanel(session, "wizard_tabs", selected = "step1")
    })

    observeEvent(input$wiz_next_3, {
        updateTabsetPanel(session, "wizard_tabs", selected = "step4")
    })
    observeEvent(input$wiz_back_3, {
        updateTabsetPanel(session, "wizard_tabs", selected = "step2")
    })

    observeEvent(input$wiz_back_4, {
        updateTabsetPanel(session, "wizard_tabs", selected = "step3")
    })

    # --- 4.4: Wizard Review Table ---
    output$wizard_review_table <- renderTable({
        task <- input$task_selector
        params <- data.frame(Parameter = character(), Value = character(), stringsAsFactors = FALSE)

        # Helper for lookup
        get_ds_name <- function(id) {
            df <- datasets_reactive()
            if (!is.null(df) && id %in% df$id) {
                return(paste0(df$name[df$id == id], " (", id, ")"))
            }
            return(id)
        }

        if (task == "object_detection") {
            params <- rbind(params, c("Task", "Object Detection"))
            params <- rbind(params, c("Dataset", get_ds_name(input$obj_dataset_id)))
            params <- rbind(params, c("Run Name", input$obj_run_name))
            params <- rbind(params, c("Version", input$obj_version))
            params <- rbind(params, c("Model Arch", input$obj_model_arch))
            params <- rbind(params, c("Checkpoint", input$obj_model_checkpoint))

            # Conditional Params (YOLO vs HF)
            is_yolo_pt <- grepl("\\.pt$", input$obj_model_checkpoint)

            params <- rbind(params, c("Epochs", as.character(input$obj_epochs)))
            params <- rbind(params, c("Train Batch Size", as.character(input$obj_train_batch_size)))

            if (is_yolo_pt) {
                params <- rbind(params, c("Optimizer", input$obj_optimizer))
                params <- rbind(params, c("Lr0", as.character(input$obj_lr0))) # Use Lr0
                params <- rbind(params, c("Momentum", as.character(input$obj_momentum)))
                params <- rbind(params, c("Weight Decay", as.character(input$obj_weight_decay)))
                params <- rbind(params, c("Warmup Epochs", as.character(input$obj_warmup_epochs)))
                params <- rbind(params, c("Mosaic", as.character(input$obj_mosaic)))
                params <- rbind(params, c("Mixup", as.character(input$obj_mixup)))
            } else {
                params <- rbind(params, c("Eval Batch Size", as.character(input$obj_eval_batch_size))) # HF Only
                params <- rbind(params, c("Learning Rate", as.character(input$obj_learning_rate))) # HF Only
                params <- rbind(params, c("Weight Decay", as.character(input$obj_weight_decay_hf)))
                params <- rbind(params, c("Optimizer", input$obj_optimizer_hf))
                params <- rbind(params, c("Scheduler", input$obj_scheduler_hf))
                params <- rbind(params, c("Warmup Epochs", as.character(input$obj_warmup_epochs_hf)))
            }

            params <- rbind(params, c("Early Stopping Patience", as.character(input$obj_early_stopping_patience)))
            params <- rbind(params, c("Seed", as.character(input$obj_seed)))
        } else if (task == "image_classification") {
            params <- rbind(params, c("Task", "Image Classification"))
            params <- rbind(params, c("Dataset", get_ds_name(input$img_class_dataset_id)))
            params <- rbind(params, c("Run Name", input$img_class_run_name))
            params <- rbind(params, c("Version", input$img_class_version))
            params <- rbind(params, c("Model Arch", input$img_class_model_arch))
            params <- rbind(params, c("Checkpoint", input$img_class_model_checkpoint))
            params <- rbind(params, c("Epochs", as.character(input$img_class_num_train_epochs)))
            params <- rbind(params, c("Train Batch Size", as.character(input$img_class_per_device_train_batch_size)))
            params <- rbind(params, c("Eval Batch Size", as.character(input$img_class_eval_batch_size)))
            params <- rbind(params, c("Learning Rate", as.character(input$img_class_learning_rate)))
            params <- rbind(params, c("Weight Decay", as.character(input$img_class_weight_decay)))
            params <- rbind(params, c("Max Image Size", as.character(input$img_class_max_image_size)))
            params <- rbind(params, c("Seed", as.character(input$img_class_seed)))
        } else if (task == "semantic_segmentation") {
            params <- rbind(params, c("Task", "Semantic Segmentation"))
            params <- rbind(params, c("Dataset", get_ds_name(input$seg_dataset_id)))
            params <- rbind(params, c("Run Name", input$seg_run_name))
            params <- rbind(params, c("Version", input$seg_version))
            params <- rbind(params, c("Model Arch", input$seg_model_arch))
            params <- rbind(params, c("Checkpoint", input$seg_model_checkpoint))
            params <- rbind(params, c("Epochs", as.character(input$seg_num_train_epochs)))
            params <- rbind(params, c("Train Batch Size", as.character(input$seg_per_device_train_batch_size)))
            params <- rbind(params, c("Eval Batch Size", as.character(input$seg_eval_batch_size)))
            params <- rbind(params, c("Learning Rate", as.character(input$seg_learning_rate)))
            params <- rbind(params, c("Weight Decay", as.character(input$seg_weight_decay)))
            params <- rbind(params, c("Max Image Size", as.character(input$seg_max_image_size)))
            params <- rbind(params, c("Seed", as.character(input$seg_seed)))
        }

        if (nrow(params) > 0) colnames(params) <- c("Parameter", "Value")
        return(params)
    })

    # ==============================================================================
    # == 5. "LIVE TRAINING" TAB LOGIC
    # ==============================================================================

    # --- 5.1: Job Polling & Status Display ---
    observe({
        job_id <- active_job_id()
        current_status <- polled_data()$status

        if (!is.null(job_id) && !(current_status %in% c("completed", "failed", "Error", "Polling Error"))) {
            invalidateLater(1000, session)

            # Poll for Status & Log
            tryCatch(
                {
                    req_status <- request(paste0(api_url, "/status/", job_id))
                    resp_status <- req_perform(req_status)
                    if (resp_status(resp_status) == 200) {
                        polled_data(resp_body_json(resp_status))
                    }
                },
                error = function(e) {
                    current_data <- polled_data()
                    current_data$status <- "Polling Error"
                    polled_data(current_data)
                }
            )

            # Poll for Metrics
            tryCatch(
                {
                    req_metrics <- request(paste0(api_url, "/metrics/", job_id))
                    resp_metrics <- req_perform(req_metrics)
                    if (resp_status(resp_metrics) == 200) {
                        polled_metrics(resp_body_json(resp_metrics))
                    }
                },
                error = function(e) {
                    polled_metrics(NULL)
                }
            )
        }
    })

    # --- 5.2: Job Status Panel Outputs ---
    output$job_task_display <- renderText({
        polled_data()$task
    })
    output$job_id_display <- renderText({
        ifelse(is.null(active_job_id()), "None", active_job_id())
    })
    output$job_status_display <- renderText({
        polled_data()$status
    })

    # --- 5.2.b: Job Progress Bar ---
    output$job_progress_ui <- renderUI({
        pdata <- polled_data()
        if (is.null(pdata) || is.null(pdata$progress)) {
            return(NULL)
        }

        pct <- pdata$progress$percentage
        txt <- pdata$progress$text
        if (is.null(pct)) pct <- 0
        if (is.null(txt)) txt <- "Starting..."

        tags$div(
            tags$p(strong("Progress: "), txt),
            tags$div(
                class = "progress",
                tags$div(
                    class = "progress-bar progress-bar-success progress-bar-striped active",
                    role = "progressbar",
                    `aria-valuenow` = pct,
                    `aria-valuemin` = "0",
                    `aria-valuemax` = "100",
                    style = paste0("width: ", pct, "%"),
                    span(class = "sr-only", paste0(pct, "% Complete"))
                )
            )
        )
    })

    # --- 5.3: Metrics Table Panel Output ---
    output$eval_table <- renderDT({
        metrics_list <- polled_metrics()
        req(metrics_list, length(metrics_list) > 0)

        metrics_df <- bind_rows(metrics_list)

        if (nrow(metrics_df) > 0) {
            display_df <- metrics_df %>%
                select(epoch, starts_with("eval_"), starts_with("test_")) %>%
                pivot_longer(
                    cols = starts_with("eval_") | starts_with("test_"),
                    names_to = "metric_name",
                    values_to = "value",
                    values_drop_na = TRUE
                ) %>%
                separate(metric_name, into = c("step", "metric"), sep = "_", extra = "merge") %>%
                pivot_wider(
                    names_from = metric,
                    values_from = value,
                    values_fn = first
                ) %>%
                select(any_of(c("step", "epoch", "loss", "map", "wer", "cer", "mean_iou")), everything())

            datatable(
                display_df,
                options = list(
                    pageLength = 5,
                    scrollX = TRUE,
                    searching = FALSE,
                    autoWidth = TRUE,
                    class = "cell-border stripe"
                ),
                rownames = FALSE
            )
        } else {
            return(NULL)
        }
    })

    # --- 5.4: Full Log Panel Output ---
    output$log_output <- renderText({
        log_text <- polled_data()$log
        if (is.null(log_text) || nchar(log_text) == 0) {
            return("No log output...")
        }
        return(log_text)
    })

    output$historical_log_output <- renderText({
        log_text <- historical_log_text()
        if (is.null(log_text) || nchar(log_text) == 0) {
            return("No log loaded. Click a job request to load.")
        }
        return(log_text)
    })

    # --- 5.5: Plotting Logic for "Live Training" Tab ---
    metrics_for_plotting <- reactive({
        metrics_list <- polled_metrics()
        req(metrics_list, length(metrics_list) > 0)

        bind_rows(metrics_list) %>%
            filter(!is.na(epoch)) %>%
            mutate(plot_group = ceiling(epoch)) %>% # Group fractional steps into next integer epoch
            group_by(plot_group) %>%
            summarise(
                epoch = max(plot_group),
                across(starts_with(c("eval_", "train_")), ~ last(na.omit(.))),
                .groups = "drop"
            ) %>%
            filter(epoch > 0) %>%
            arrange(epoch)
    })

    output$is_job_running <- reactive({
        job <- selected_job_data()
        !is.null(job) && job$status == "running"
    })
    outputOptions(output, "is_job_running", suspendWhenHidden = FALSE)

    last_metric_choices <- reactiveVal(NULL)

    observe({
        # Update metric selector choices dynamically without re-rendering the input
        df <- tryCatch(metrics_for_plotting(), error = function(e) NULL)

        if (!is.null(df) && nrow(df) > 0) {
            metric_names <- names(df)[sapply(df, is.numeric) & !names(df) %in% c("epoch", "step", "runtime", "samples_per_second", "steps_per_second", "plot_group")]

            # Check if choices actually changed
            current_choices <- last_metric_choices()
            if (!identical(metric_names, current_choices)) {
                last_metric_choices(metric_names)

                # Smart default selection
                default_metric <- "eval_loss"
                if (!"eval_loss" %in% metric_names && length(metric_names) > 0) {
                    default_metric <- metric_names[1]
                }

                # Preserve current selection if valid
                selected_val <- isolate(input$selected_metric)
                if (is.null(selected_val) || !selected_val %in% metric_names) {
                    selected_val <- default_metric
                }

                updateSelectInput(session, "selected_metric",
                    choices = metric_names,
                    selected = selected_val
                )
            }
        }
    })

    output$dynamic_metric_plot <- renderDygraph({
        df <- tryCatch(metrics_for_plotting(), error = function(e) NULL)

        if (is.null(df) || nrow(df) == 0) {
            return(dygraph(data.frame(x = c(0), y = c(0)), main = "Waiting for Epoch 1") %>%
                dyOptions(drawGrid = FALSE, drawAxes = FALSE, drawYAxis = FALSE, drawXAxis = FALSE) %>%
                dyAxis("x", label = "Epoch"))
        }

        req(input$selected_metric)
        req(input$selected_metric %in% names(df))

        metric_to_plot <- input$selected_metric

        if (!"epoch" %in% names(df) || !metric_to_plot %in% names(df)) {
            return(dygraph(data.frame(x = c(0), y = c(0)), main = "Metric data not yet available") %>%
                dyOptions(drawGrid = FALSE, drawAxes = FALSE, drawYAxis = FALSE, drawXAxis = FALSE))
        }

        plot_data <- df[, c("epoch", metric_to_plot)]

        dygraph(plot_data, main = paste(metric_to_plot, "vs. Epoch")) %>%
            dySeries(metric_to_plot, label = metric_to_plot) %>%
            dyAxis("x", label = "Epoch", valueRange = c(0, max(df$epoch, na.rm = TRUE) + 1)) %>%
            dyRangeSelector() %>%
            dyOptions(
                stackedGraph = FALSE,
                fillGraph = FALSE,
                stepPlot = FALSE,
                drawPoints = TRUE,
                pointSize = 4,
                connectSeparatedPoints = TRUE
            ) %>%
            dyLegend(show = "always", width = 200)
    })


    # ==============================================================================
    # == 6. JOB MANAGER LOGIC (Unified History & Live)
    # ==============================================================================

    # --- 6.1: Fetch Jobs based on Filters ---
    observe({
        # React to tab switching, job completion, and filter changes
        input$main_tabs
        polled_data()
        input$jm_task_filter
        input$jm_status_filter
        input$jm_show_entries
        refresh_data_trigger() # Trigger refresh on actions like delete

        # Auto-refresh list every 5 seconds to show status updates
        invalidateLater(5000, session)

        tryCatch(
            {
                query_params <- list()
                if (!is.null(input$jm_task_filter) && input$jm_task_filter != "All") {
                    query_params$task_type <- input$jm_task_filter
                }
                if (!is.null(input$jm_status_filter) && input$jm_status_filter != "All") {
                    query_params$status <- input$jm_status_filter
                }

                req <- request(paste0(api_url, "/jobs/list"))
                if (length(query_params) > 0) {
                    req <- req_url_query(req, !!!query_params)
                }

                resp <- req_perform(req)
                jobs_df <- resp_body_json(resp, simplifyVector = TRUE)

                if (length(jobs_df) > 0 && is.data.frame(jobs_df) && nrow(jobs_df) > 0) {
                    history_jobs_df(jobs_df) # Reuse existing reactive val
                } else {
                    history_jobs_df(NULL)
                }
            },
            error = function(e) {
                history_jobs_df(NULL)
            }
        )
    })

    # --- Handle Stale Data (Clear Selection on Filter Change) ---
    observeEvent(input$jm_task_filter, {
        selected_job_data(NULL)
        active_job_id(NULL) # Stop polling
    })

    # --- 6.2: Render Job Manager Table ---
    output$job_manager_table <- renderDT({
        df <- history_jobs_df()

        if (is.null(df) || nrow(df) == 0) {
            return(NULL)
        }

        # -- Apply Filters --
        # Task Filter
        if (!is.null(input$jm_task_filter) && input$jm_task_filter != "All") {
            df <- df[df$task_type == input$jm_task_filter, ]
        }

        # Status Filter (Case-insensitive)
        if (!is.null(input$jm_status_filter) && input$jm_status_filter != "All") {
            # Backend statuses: 'completed', 'running', 'failed', 'queued'
            # Filter options: 'Completed', 'Running', 'Failed'
            df <- df[tolower(df$status) == tolower(input$jm_status_filter), ]
        }


        # Add Actions column
        df$Actions <- vapply(seq_len(nrow(df)), function(i) {
            status <- df$status[i]
            id <- df$id[i]
            if (status %in% c("running", "queued")) {
                as.character(tags$button(
                    class = "btn btn-danger btn-xs",
                    onclick = sprintf("Shiny.setInputValue('dl_cancel_job_id', '%s', {priority: 'event'});", id),
                    "Cancel"
                ))
            } else {
                as.character(tags$button(
                    class = "btn btn-default btn-xs",
                    onclick = sprintf("Shiny.setInputValue('dl_delete_job_click', '%s', {priority: 'event'});", id),
                    icon("trash"), " Delete"
                ))
            }
        }, FUN.VALUE = character(1))

        # Add Explicit Select Column (Checkbox behaving as toggle)
        current_sel <- selected_job_data()
        sel_id <- if (!is.null(current_sel)) current_sel$id else ""

        df$Select <- vapply(df$id, function(id) {
            checked <- if (id == sel_id) "checked" else ""
            sprintf('<input type="checkbox" name="job_sel" onclick="Shiny.setInputValue(\'dl_job_toggle\', \'%s\', {priority: \'event\'});" %s>', id, checked)
        }, FUN.VALUE = character(1))

        display_df <- df %>%
            select(Select, Actions, Status = status, Name = run_name, Version = version, Task = task_type, ID = id) %>%
            arrange(desc(ID))

        datatable(
            display_df,
            escape = FALSE,
            selection = "none", # Disable native row selection
            options = list(
                pageLength = as.integer(input$jm_show_entries), scrollX = TRUE, dom = "t,p",
                columnDefs = list(list(className = "dt-center", targets = 0))
            )
        )
    })

    # --- 6.3: Handle Job Selection (Explicit Toggle) ---
    selected_job_data <- reactiveVal(NULL)

    observeEvent(input$dl_job_toggle, {
        toggled_id <- input$dl_job_toggle
        req(toggled_id)

        current_sel <- selected_job_data()

        # If clicking the currently selected job -> Deselect
        if (!is.null(current_sel) && current_sel$id == toggled_id) {
            selected_job_data(NULL)
            active_job_id(NULL)
            history_metrics(NULL)
            updateSelectInput(session, "history_job_selector", selected = character(0))
            return()
        }

        # Else -> Select new job
        df <- history_jobs_df()
        req(df)
        selected_row <- df[df$id == toggled_id, ]
        req(nrow(selected_row) > 0)

        selected_job_data(selected_row)

        if (selected_row$status == "running") {
            # ... (existing polling logic)
            active_job_id(selected_row$id)
            history_poller_active(FALSE)
        } else {
            # ... (existing completed logic)
            active_job_id(NULL)

            # Fetch Historical Metrics & Logs
            tryCatch(
                {
                    req <- request(paste0(api_url, "/metrics/", selected_row$id))
                    resp <- req_perform(req)
                    if (resp_status(resp) == 200) history_metrics(resp_body_json(resp))

                    req_log <- request(paste0(api_url, "/jobs/", selected_row$id, "/log"))
                    resp_log <- req_perform(req_log)
                    if (resp_status(resp_log) == 200) historical_log_text(resp_body_json(resp_log)$log)
                },
                error = function(e) {
                    print(paste("Error fetching historical data:", e$message))
                }
            )
        }
    })

    # --- Auto-Update Selected Job Details from history_jobs_df ---
    # This ensures that when a running job completes in the background (and history_jobs_df refreshes),
    # the details panel updates to show "completed" status/buttons without re-selection.
    observe({
        input$jm_task_filter # Dep
        df <- history_jobs_df()
        current_sel <- selected_job_data()

        req(df, current_sel)

        # Find updated row for current selection
        updated_row <- df[df$id == current_sel$id, ]

        if (nrow(updated_row) == 1) {
            # Only update if status changed (avoid infinite loops/flicker)
            if (updated_row$status != current_sel$status) {
                # Update local state
                selected_job_data(updated_row)

                # If status flipped to completed/failed, ensure polling logic reacts
                if (updated_row$status %in% c("completed", "failed", "error")) {
                    active_job_id(NULL) # Stop polling

                    # Trigger metrics fetch for final results
                    tryCatch(
                        {
                            req <- request(paste0(api_url, "/metrics/", updated_row$id))
                            resp <- req_perform(req)
                            if (resp_status(resp) == 200) history_metrics(resp_body_json(resp))

                            req_log <- request(paste0(api_url, "/jobs/", updated_row$id, "/log"))
                            resp_log <- req_perform(req_log)
                            if (resp_status(resp_log) == 200) historical_log_text(resp_body_json(resp_log)$log)
                        },
                        error = function(e) {}
                    )
                }
            }
        }
    })


    # --- 6.4: Dynamic Details Panel ---
    output$job_manager_details_ui <- renderUI({
        selected <- selected_job_data()
        polled <- polled_data()

        # Use polled data if available and matches selected job (Real-time source)
        if (!is.null(selected) && !is.null(polled) && !is.null(polled$id) && polled$id == selected$id) {
            job <- polled
            # Debug Print
            if (job$status == "completed") print(paste("UI REFRESH: Job", job$id, "detected as COMPLETED from polled source"))
        } else {
            job <- selected
        }

        if (is.null(job)) {
            return(box(width = NULL, title = "Job Details", status = "success", solidHeader = TRUE, "Select a job from the list to view details."))
        }

        if (job$status == "running") {
            # LIVE VIEW
            tagList(
                box(
                    width = NULL, title = paste("Running:", job$run_name), status = "success", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(
                        column(6, strong("Status:"), textOutput("job_status_display", inline = TRUE)),
                        column(6, strong("ID:"), job$id)
                    ),
                    br(),
                    uiOutput("job_progress_ui")
                ),
                # Live Metrics moved to static UI to prevent re-rendering glitches
                box(
                    width = NULL, title = "Logs", status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    verbatimTextOutput("log_output")
                )
            )
        } else {
            # HISTORICAL VIEW (Completed/Failed)

            # Extract config/details
            details_ui <- tagList()
            if (!is.null(job$details) && length(job$details) > 0) {
                # jobs_df$details is likely a dataframe of 1 row if simplified, or a list
                d <- job$details
                if (is.data.frame(d)) d <- as.list(d)

                # Filter keys
                all_keys <- names(d)[!names(d) %in% c("script_name", "output_dir", "log_file")]

                # Detect Model Type for UI Filtering
                model_chk <- if (!is.null(d$model_checkpoint)) tolower(d$model_checkpoint) else ""
                is_yolo <- (grepl("yolo", model_chk) && !grepl("yolos", model_chk)) || grepl("rtdetr", model_chk)

                valid_keys <- all_keys

                if (is_yolo) {
                    # YOLO: Exclude HF params
                    exclude_hf <- c(
                        "learning_rate", "optimizer_hf", "scheduler_hf", "warmup_epochs_hf", "eval_batch_size",
                        "gradient_accumulation_steps", "gradient_checkpointing", "early_stopping_threshold", "weight_decay_hf"
                    )
                    valid_keys <- valid_keys[!valid_keys %in% exclude_hf]
                } else {
                    # HF: Exclude YOLO params
                    exclude_yolo <- c("lr0", "momentum", "weight_decay_yolo", "mosaic", "mixup", "hsv_h", "hsv_s", "hsv_v")
                    # Also exclude "optimizer" if "optimizer_hf" is present (cleaner)
                    if ("optimizer_hf" %in% names(d)) exclude_yolo <- c(exclude_yolo, "optimizer")
                    valid_keys <- valid_keys[!valid_keys %in% exclude_yolo]
                }

                if (length(valid_keys) > 0) {
                    mid <- ceiling(length(valid_keys) / 2)
                    keys_col1 <- valid_keys[1:mid]
                    keys_col2 <- valid_keys[(mid + 1):length(valid_keys)]

                    render_item <- function(k) {
                        val <- d[[k]]
                        if (length(val) > 1) val <- paste(val, collapse = ", ")
                        # Handle NULL or empty
                        if (is.null(val) || length(val) == 0) val <- "N/A"
                        p(strong(paste0(tools::toTitleCase(gsub("_", " ", k)), ":")), as.character(val))
                    }

                    col1_ui <- lapply(keys_col1, render_item)
                    col2_ui <- lapply(keys_col2, render_item)

                    details_ui <- fluidRow(
                        column(6, do.call(tagList, col1_ui)),
                        column(6, do.call(tagList, col2_ui))
                    )
                }
            }

            details_buttons <- tagList()
            if (job$status == "completed") {
                details_buttons <- tagList(
                    br(),
                    downloadButton("download_job_result", "Download Outputs", class = "btn-success", style = "width: 100%; margin-top: 10px; color: white;")
                )
            }

            tagList(
                box(
                    width = NULL, title = paste("Job Details:", job$run_name), status = "info", solidHeader = TRUE, collapsible = TRUE,
                    p(strong("ID:"), job$id),
                    p(strong("Status:"), job$status),
                    p(strong("Task:"), job$task_type),
                    # p(strong("Version:"), job$version), # Moved to dynamic details
                    hr(),
                    h5(strong("Configuration:")),
                    details_ui,
                    details_buttons
                ),
                box(
                    width = NULL, title = "Historical Metrics", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("history_metric_selector_ui"),
                    dygraphOutput("history_metric_plot", height = "300px")
                ),
                box(
                    width = NULL, title = "Evaluation Results", status = "success", solidHeader = TRUE, collapsible = TRUE,
                    DTOutput("history_eval_table")
                ),
                box(
                    width = NULL, title = "Logs (Historical)", status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                    verbatimTextOutput("historical_log_output")
                )
            )
        }
    })

    # --- Download Handler ---
    output$download_job_result <- downloadHandler(
        filename = function() {
            job <- selected_job_data()
            if (is.null(job)) {
                return("model.zip")
            }
            paste0(job$run_name, "-", job$task_type, ".zip")
        },
        content = function(file) {
            job <- selected_job_data()
            req(job)

            # Construct API URL
            down_url <- paste0(api_url, "/jobs/", job$id, "/download")

            # Use httr2 to download with API Key
            tryCatch(
                {
                    req <- request(down_url) %>%
                        req_headers("X-API-Key" = api_key) %>%
                        req_timeout(600)

                    res <- req_perform(req, path = file)

                    if (resp_status(res) != 200) {
                        stop(paste("Download failed:", resp_body_string(res)))
                    }
                },
                error = function(e) {
                    shinyalert::shinyalert("Error", paste("Download failed:", e$message), type = "error")
                }
            )
        }
    )

    # --- 6.1.6: Handle Job Cancellation ---
    observeEvent(input$dl_cancel_job_id, {
        job_id <- input$dl_cancel_job_id
        req(job_id)

        shinyalert::shinyalert(
            title = "Cancelling Job...",
            text = "Sending request to stop the training job...",
            closeOnEsc = FALSE,
            closeOnClickOutside = FALSE,
            showConfirmButton = FALSE,
            timer = 10000, # Failsafe: auto-close after 10s if stuck
            type = "info"
        )

        tryCatch(
            {
                req <- request(paste0(api_url, "/jobs/cancel/", job_id)) %>%
                    req_headers("X-API-Key" = api_key) %>%
                    req_method("POST") %>%
                    req_timeout(5) # 5 second timeout

                resp <- req_perform(req)

                if (resp_status(resp) == 200) {
                    # Success
                    shinyalert::shinyalert(
                        title = "Job Cancelled",
                        text = paste("Job", job_id, "has been cancelled."),
                        type = "success",
                        timer = 2000,
                        showConfirmButton = FALSE
                    )
                    # Refresh table
                    refresh_data_trigger(refresh_data_trigger() + 1)
                } else {
                    shinyalert::shinyalert(
                        title = "Error",
                        text = paste("Failed to cancel job. Status:", resp_status(resp)),
                        type = "error",
                        showConfirmButton = TRUE
                    )
                }
            },
            error = function(e) {
                shinyalert::shinyalert(
                    title = "Communication Error",
                    text = paste("Could not connect to cancel endpoint:", e$message),
                    type = "error",
                    showConfirmButton = TRUE
                )
            }
        )
    })

    # --- 6.1.7: Handle Job Deletion (Click -> Confirm -> Delete) ---
    observeEvent(input$dl_delete_job_click, {
        job_id <- input$dl_delete_job_click
        req(job_id)

        shinyalert::shinyalert(
            title = "Confirm Deletion",
            text = paste("Are you sure you want to delete Job", job_id, "?\nThis will remove the job record AND all output files/logs permanently."),
            type = "warning",
            showCancelButton = TRUE,
            confirmButtonText = "Yes, delete it",
            cancelButtonText = "No, keep it",
            callbackR = function(x) {
                if (x) {
                    # User confirmed, trigger actual deletion event
                    shinyjs::runjs(sprintf("Shiny.setInputValue('dl_delete_job_confirmed', '%s', {priority: 'event'});", job_id))
                }
            }
        )
    })

    observeEvent(input$dl_delete_job_confirmed, {
        job_id <- input$dl_delete_job_confirmed
        req(job_id)

        tryCatch(
            {
                req <- request(paste0(api_url, "/jobs/", job_id)) %>%
                    req_headers("X-API-Key" = api_key) %>%
                    req_method("DELETE")
                resp <- req_perform(req)

                if (resp_status(resp) == 200) {
                    shinyalert::shinyalert("Deleted!", "Job and files have been deleted.", type = "success")
                    refresh_data_trigger(refresh_data_trigger() + 1)
                    selected_job_data(NULL)
                    active_job_id(NULL)
                } else {
                    shinyalert::shinyalert("Error", "Failed to delete job.", type = "error")
                }
            },
            error = function(e) {
                shinyalert::shinyalert("Error", paste("Communication error:", e$message), type = "error")
            }
        )
    })

    # --- 6.2: Fetch Metrics when user selects a historical job ---
    observeEvent(input$history_job_selector, {
        job_id <- input$history_job_selector

        history_poller_active(FALSE) # Deactivate poller by default

        if (!is.null(job_id) && nchar(job_id) > 0) {
            tryCatch(
                {
                    req <- request(paste0(api_url, "/metrics/", job_id))
                    resp <- req_perform(req)
                    history_metrics(resp_body_json(resp))
                },
                error = function(e) {
                    history_metrics(NULL)
                }
            )

            # --- Poller Activation Logic ---
            req(history_jobs_df())
            job_info <- history_jobs_df() %>% filter(id == job_id)
            if (nrow(job_info) > 0 && job_info$status == "running") {
                print(paste("Activating poller for running job:", job_id))
                history_poller_active(TRUE) # Activate poller
            }
            # --- End Poller Logic ---
        } else {
            # If job_id is "" or NULL (e.g., "No jobs found"), clear the metrics.
            history_metrics(NULL)
        }
    })

    # --- 6.3: Reactive for Historical Plot Data ---
    history_metrics_for_plotting <- reactive({
        metrics_list <- history_metrics()
        req(metrics_list, length(metrics_list) > 0)

        df <- bind_rows(metrics_list)

        # Robust handling for 'epoch'
        if ("epoch" %in% names(df)) {
            df <- df %>%
                filter(!is.na(epoch)) %>%
                mutate(plot_group = ceiling(epoch)) %>%
                group_by(plot_group) %>%
                summarise(
                    epoch = max(plot_group),
                    # Use last(na.omit(.)) to capture valid data even if scattered across rows
                    across(everything(), ~ last(na.omit(.))),
                    .groups = "drop"
                ) %>%
                filter(epoch > 0) %>%
                arrange(epoch)
        } else if ("step" %in% names(df)) {
            df$epoch <- df$step # Fallback to step
            df <- df %>% arrange(epoch)
        } else {
            # Fallback: create pseudo-epoch index
            df$epoch <- 1:nrow(df)
        }

        df %>%
            select(where(is.numeric)) # Select numeric columns for plotting
    })

    # --- 6.4: Render Historical Metrics Table ---
    output$history_eval_table <- renderDT({
        metrics_list <- history_metrics()
        req(metrics_list, length(metrics_list) > 0)

        metrics_df <- bind_rows(metrics_list)

        if (nrow(metrics_df) > 0) {
            # Check if we have any eval or test columns to pivot
            has_eval_cols <- any(grepl("^eval_", names(metrics_df)))
            has_test_cols <- any(grepl("^test_", names(metrics_df)))

            if (!has_eval_cols && !has_test_cols) {
                return(NULL)
            }

            display_df <- tryCatch(
                {
                    metrics_df %>%
                        select(epoch, starts_with("eval_"), starts_with("test_")) %>%
                        pivot_longer(
                            cols = starts_with("eval_") | starts_with("test_"),
                            names_to = "metric_name",
                            values_to = "value",
                            values_drop_na = TRUE
                        ) %>%
                        separate(metric_name, into = c("step", "metric"), sep = "_", extra = "merge") %>%
                        pivot_wider(
                            names_from = metric,
                            values_from = value,
                            values_fn = first
                        ) %>%
                        select(any_of(c("step", "epoch", "loss", "map", "wer", "cer", "mean_iou")), everything())
                },
                error = function(e) {
                    return(NULL)
                }
            )

            if (is.null(display_df)) {
                return(NULL)
            }

            datatable(
                display_df,
                options = list(pageLength = 5, scrollX = TRUE, searching = FALSE, autoWidth = TRUE),
                rownames = FALSE
            )
        } else {
            return(NULL)
        }
    })

    # --- 6.5: Render Historical Plot UI (Dropdown) ---
    output$history_metric_selector_ui <- renderUI({
        df <- tryCatch(history_metrics_for_plotting(), error = function(e) NULL)

        if (is.null(df) || nrow(df) == 0) {
            return(p("No evaluation metrics found for this job."))
        }

        metric_names <- names(df)[sapply(df, is.numeric) & !names(df) %in% c("epoch", "step", "runtime", "samples_per_second", "steps_per_second", "plot_group")]

        default_metric <- "eval_loss"
        if (!"eval_loss" %in% metric_names && length(metric_names) > 0) {
            default_metric <- metric_names[1]
        }

        selected_val <- input$history_selected_metric
        if (is.null(selected_val) || !selected_val %in% metric_names) {
            selected_val <- default_metric
        }

        selectInput("history_selected_metric", "Select Metric to Plot:",
            choices = metric_names,
            selected = selected_val
        )
    })


    # ==============================================================================
    # == 7. INFERENCE LOGIC
    # ==============================================================================

    # --- 7.1: Fetch Completed Jobs for Inference Dropdown ---
    observe({
        # Trigger when Inference tab is active
        req(input$main_tabs == "Inference")

        tryCatch(
            {
                # Fetch ALL completed jobs (no other filters to ensure we get all candidates)
                req <- request(paste0(api_url, "/jobs/list")) %>%
                    req_url_query(status = "completed")
                resp <- req_perform(req)
                all_completed <- resp_body_json(resp, simplifyVector = TRUE)

                if (length(all_completed) > 0 && is.data.frame(all_completed) && nrow(all_completed) > 0) {
                    # Filter for Object Detection
                    obj_runs <- all_completed %>%
                        filter(task_type == "object_detection") %>%
                        pull(run_name)
                    updateSelectInput(session, "infer_run_name", choices = obj_runs, selected = if (length(obj_runs) > 0) obj_runs[1] else "")

                    # Filter for Image Class
                    img_runs <- all_completed %>%
                        filter(task_type == "image_classification") %>%
                        pull(run_name)
                    updateSelectInput(session, "infer_img_class_run_name", choices = img_runs, selected = if (length(img_runs) > 0) img_runs[1] else "")

                    # Filter for Segmentation
                    seg_runs <- all_completed %>%
                        filter(task_type == "semantic_segmentation") %>%
                        pull(run_name)
                    updateSelectInput(session, "infer_seg_run_name", choices = seg_runs, selected = if (length(seg_runs) > 0) seg_runs[1] else "")
                } else {
                    # No completed jobs found
                    updateSelectInput(session, "infer_run_name", choices = "No completed jobs", selected = "")
                    updateSelectInput(session, "infer_img_class_run_name", choices = "No completed jobs", selected = "")
                    updateSelectInput(session, "infer_seg_run_name", choices = "No completed jobs", selected = "")
                }
            },
            error = function(e) {
                print(paste("Error fetching inference runs:", e$message))
            }
        )
    })

    # --- 6.6: Render Historical Plot ---
    output$history_metric_plot <- renderDygraph({
        df <- tryCatch(history_metrics_for_plotting(), error = function(e) NULL)

        if (is.null(df) || nrow(df) == 0) {
            return(dygraph(data.frame(x = c(0), y = c(0)), main = "No Metric Data") %>%
                dyOptions(drawGrid = FALSE, drawAxes = FALSE, drawYAxis = FALSE, drawXAxis = FALSE) %>%
                dyAxis("x", label = "Epoch"))
        }

        req(input$history_selected_metric)
        req(input$history_selected_metric %in% names(df))

        metric_to_plot <- input$history_selected_metric

        if (!"epoch" %in% names(df) || !metric_to_plot %in% names(df)) {
            return(dygraph(data.frame(x = c(0), y = c(0)), main = "Metric data not yet available") %>%
                dyOptions(drawGrid = FALSE, drawAxes = FALSE, drawYAxis = FALSE, drawXAxis = FALSE))
        }

        plot_data <- df[, c("epoch", metric_to_plot)]

        dygraph(plot_data, main = paste(metric_to_plot, "vs. Epoch")) %>%
            dySeries(metric_to_plot, label = metric_to_plot) %>%
            dyAxis("x", label = "Epoch", valueRange = c(0, max(df$epoch, na.rm = TRUE) + 1)) %>%
            dyRangeSelector() %>%
            dyOptions(
                stackedGraph = FALSE,
                fillGraph = FALSE,
                stepPlot = FALSE,
                drawPoints = TRUE,
                pointSize = 4
            ) %>%
            dyLegend(show = "always", width = 200)
    })

    # --- 6.7: Poller for selected running job in History tab ---
    observe({
        # Only run if:
        req(
            history_poller_active() == TRUE,
            input$main_tabs == "Training History",
            !is.null(input$history_job_selector),
            nchar(input$history_job_selector) > 0
        )

        # Poll every 3 seconds
        invalidateLater(3000, session)

        job_id <- input$history_job_selector
        print(paste("History Poller: Fetching metrics for", job_id))

        tryCatch(
            {
                req <- request(paste0(api_url, "/metrics/", job_id))
                resp <- req_perform(req)
                if (resp_status(resp) == 200) {
                    history_metrics(resp_body_json(resp))
                }

                # Check if job is still running
                req_status <- request(paste0(api_url, "/status/", job_id))
                resp_status <- req_perform(req_status)
                if (resp_status(resp_status) == 200) {
                    status_data <- resp_body_json(resp_status)
                    if (status_data$status != "running") {
                        print(paste("History Poller: Job", job_id, "is no longer running. Deactivating poller."))
                        history_poller_active(FALSE)
                        # Refresh the job list dropdown to show "completed"
                        observeEvent(model_registry(),
                            {
                                req(model_registry())
                                refresh_data_trigger(refresh_data_trigger() + 1)
                            },
                            once = TRUE
                        )
                    }
                }
            },
            error = function(e) {
                print(paste("History Poller Error:", e$message))
                history_poller_active(FALSE) # Stop polling on error
            }
        )
    })


    # ==============================================================================
    # == 7. "INFERENCE" TAB LOGIC
    # ==============================================================================

    # --- 7.0: Populate Runs for Prediction ---
    observeEvent(input$inference_task_selector, {
        task <- input$inference_task_selector
        req(task)

        tryCatch(
            {
                req <- request(paste0(api_url, "/jobs/list")) %>%
                    req_headers("X-API-Key" = api_key)
                resp <- req_perform(req)
                if (resp_status(resp) == 200) {
                    jobs <- resp_body_json(resp, simplifyVector = TRUE)

                    # Filter by Task and Status=Completed
                    if (!is.null(jobs) && is.data.frame(jobs) && nrow(jobs) > 0) {
                        jobs <- jobs[jobs$task_type == task & jobs$status == "completed", ]

                        if (nrow(jobs) > 0) {
                            # Create choices: run_name (id)
                            choices <- setNames(jobs$run_name, paste0(jobs$run_name, " (", substr(jobs$id, 1, 8), ")"))

                            # Update specific input
                            if (task == "object_detection") {
                                updateSelectInput(session, "infer_run_name", choices = choices)
                            } else if (task == "image_classification") {
                                updateSelectInput(session, "infer_img_class_run_name", choices = choices)
                            } else if (task == "semantic_segmentation") {
                                updateSelectInput(session, "infer_seg_run_name", choices = choices)
                            }
                        } else {
                            # No completed jobs
                            msg <- "No completed jobs found"
                            if (task == "object_detection") {
                                updateSelectInput(session, "infer_run_name", choices = c("No jobs found" = ""))
                            } else if (task == "image_classification") {
                                updateSelectInput(session, "infer_img_class_run_name", choices = c("No jobs found" = ""))
                            } else if (task == "semantic_segmentation") updateSelectInput(session, "infer_seg_run_name", choices = c("No jobs found" = ""))
                        }
                    }
                }
            },
            error = function(e) {
                print(paste("Error fetching runs:", e$message))
            }
        )
    })

    # --- Inference Event Debounce Logic ---
    debounced_infer_obj_run_name <- reactive(input$infer_run_name) %>% debounce(500)
    debounced_infer_img_class_run_name <- reactive(input$infer_img_class_run_name) %>% debounce(500)
    debounced_infer_seg_run_name <- reactive(input$infer_seg_run_name) %>% debounce(500)

    # --- 7.1: Inference Checkpoint Finders ---
    observeEvent(debounced_infer_obj_run_name(), {
        run_name <- debounced_infer_obj_run_name()
        req(nchar(run_name) > 2) # Require at least 3 chars
        tryCatch(
            {
                req <- request(paste0(api_url, "/checkpoints")) %>%
                    req_url_query(run_name = run_name, task_type = "object_detection")
                resp <- req_perform(req)
                if (resp_status(resp) == 200) {
                    checkpoints <- resp_body_json(resp, simplifyVector = TRUE)
                    if (length(checkpoints) > 0) {
                        # Create friendly relative labels: run-ver/checkpoint
                        # Extract part after 'model_outputs/'
                        lbls <- sub(".*model_outputs/", "", checkpoints)

                        # Add explicit "best.pt" context for YOLO if it's the root dir
                        lbls <- sapply(lbls, function(stat) {
                            if (!grepl("weights", stat) && !grepl("checkpoint-", stat)) paste0(stat, "/best.pt") else stat
                        })
                        names(checkpoints) <- lbls

                        # FILTER: STRICTLY show ONLY Final Models (exclude intermediate checkpoints)
                        keep_mask <- !grepl("checkpoint-", checkpoints)
                        checkpoints <- checkpoints[keep_mask]
                    }
                    updateSelectInput(session, "infer_checkpoint_dropdown", choices = checkpoints)
                }
            },
            error = function(e) {
                updateSelectInput(session, "infer_checkpoint_dropdown", choices = c("Error finding checkpoints"))
            }
        )
    })


    observeEvent(debounced_infer_img_class_run_name(), {
        run_name <- debounced_infer_img_class_run_name()
        req(nchar(run_name) > 2)
        tryCatch(
            {
                req <- request(paste0(api_url, "/checkpoints")) %>%
                    req_url_query(run_name = run_name, task_type = "image_classification")
                checkpoints <- resp_body_json(req_perform(req), simplifyVector = TRUE)
                if (length(checkpoints) > 0) {
                    # Create friendly relative labels: run-ver/checkpoint
                    lbls <- sub(".*model_outputs/", "", checkpoints)
                    lbls <- sapply(lbls, function(stat) {
                        if (!grepl("checkpoint-", stat)) paste0(stat, "/best_model") else stat
                    })
                    names(checkpoints) <- lbls

                    # FILTER: Remove intermediate checkpoints
                    checkpoints <- checkpoints[!grepl("checkpoint-", checkpoints)]
                }
                updateSelectInput(session, "infer_img_class_checkpoint_dropdown", choices = checkpoints)
            },
            error = function(e) {}
        )
    })

    observeEvent(debounced_infer_seg_run_name(), {
        run_name <- debounced_infer_seg_run_name()
        req(nchar(run_name) > 2)
        tryCatch(
            {
                req <- request(paste0(api_url, "/checkpoints")) %>%
                    req_url_query(run_name = run_name, task_type = "semantic_segmentation")
                checkpoints <- resp_body_json(req_perform(req), simplifyVector = TRUE)
                if (length(checkpoints) > 0) {
                    # Create friendly relative labels: run-ver/checkpoint
                    lbls <- sub(".*model_outputs/", "", checkpoints)
                    lbls <- sapply(lbls, function(stat) {
                        if (!grepl("checkpoint-", stat)) paste0(stat, "/best_model") else stat
                    })
                    names(checkpoints) <- lbls

                    # FILTER: Remove intermediate checkpoints
                    checkpoints <- checkpoints[!grepl("checkpoint-", checkpoints)]
                }
                updateSelectInput(session, "infer_seg_checkpoint_dropdown", choices = checkpoints)
            },
            error = function(e) {}
        )
    })

    # --- 7.2: Inference Job Submission (One per task) ---

    observeEvent(input$start_obj_inference, {
        req(input$infer_obj_image_upload)
        req(input$infer_checkpoint_dropdown)
        obj_inference_result(list(status = "Running...", image_url = NULL, error = NULL))
        tryCatch(
            {
                req <- request(paste0(api_url, "/inference/object-detection")) %>%
                    req_headers("X-API-Key" = api_key) %>%
                    req_body_multipart(
                        image = curl::form_file(input$infer_obj_image_upload$datapath),
                        model_checkpoint = input$infer_checkpoint_dropdown,
                        threshold = as.character(input$infer_obj_threshold),
                        iou = as.character(input$infer_obj_iou),
                        max_det = as.character(input$infer_obj_max_det),
                        imgsz = as.character(input$infer_obj_imgsz),
                        classes = as.character(input$infer_obj_classes)
                    )
                resp <- req_perform(req)
                resp_data <- resp_body_json(resp)
                obj_inference_result(list(status = "Success", image_url = resp_data$output_url, error = NULL))
            },
            error = function(e) {
                error_message <- as.character(e$message)
                if (!is.null(e$body)) {
                    error_message <- paste("API Error:", e$body)
                }
                obj_inference_result(list(status = "Error", image_url = NULL, error = error_message))
            }
        )
    })


    observeEvent(input$start_img_class_inference, {
        req(input$infer_img_class_upload, input$infer_img_class_checkpoint_dropdown)
        img_class_inference_result(list(status = "Running...", prediction = "Processing...", error = NULL))
        tryCatch(
            {
                req <- request(paste0(api_url, "/inference/image-classification")) %>%
                    req_body_multipart(
                        image = curl::form_file(input$infer_img_class_upload$datapath),
                        model_checkpoint = input$infer_img_class_checkpoint_dropdown
                    )
                resp_data <- resp_body_json(req_perform(req))
                img_class_inference_result(list(status = "Success", prediction = resp_data$prediction, error = NULL))
            },
            error = function(e) {
                img_class_inference_result(list(status = "Error", prediction = NULL, error = as.character(e)))
            }
        )
    })

    observeEvent(input$start_seg_inference, {
        req(input$infer_seg_image_upload)
        req(input$infer_seg_checkpoint_dropdown)
        seg_inference_result(list(status = "Running...", image_url = NULL, error = NULL))
        tryCatch(
            {
                req <- request(paste0(api_url, "/inference/image-segmentation")) %>%
                    req_body_multipart(
                        image = curl::form_file(input$infer_seg_image_upload$datapath),
                        model_checkpoint = input$infer_seg_checkpoint_dropdown
                    )
                resp <- req_perform(req)
                resp_data <- resp_body_json(resp)
                seg_inference_result(list(status = "Success", image_url = resp_data$output_url, error = NULL))
            },
            error = function(e) {
                error_message <- as.character(e$message)
                if (!is.null(e$body)) {
                    error_message <- paste("API Error:", e$body)
                }
                seg_inference_result(list(status = "Error", image_url = NULL, error = error_message))
            }
        )
    })

    # --- 7.3: Inference UI Outputs ---

    output$inference_status_ui <- renderUI({
        res <- obj_inference_result()
        if (res$status == "Running...") {
            tags$div(class = "alert alert-info", "Running inference...")
        } else if (res$status == "Error") {
            tags$div(class = "alert alert-danger", HTML(paste("<strong>Error:</strong>", res$error)))
        }
    })
    output$inference_image_output <- renderImage(
        {
            res <- obj_inference_result()
            req(res$status == "Success", res$image_url)
            image_url <- paste0(api_url, res$image_url)
            temp_file <- tempfile(fileext = ".jpg")
            download.file(image_url, temp_file, mode = "wb")
            list(src = temp_file, contentType = "image/jpeg", alt = "Inference Result")
        },
        deleteFile = TRUE
    )


    output$img_class_inference_status_ui <- renderUI({
        res <- img_class_inference_result()
        if (res$status == "Running...") {
            tags$div(class = "alert alert-info", "Running inference...")
        } else if (res$status == "Error") tags$div(class = "alert alert-danger", HTML(paste("<strong>Error:</strong>", res$error)))
    })
    output$img_class_prediction_output <- renderText({
        img_class_inference_result()$prediction
    })

    output$seg_inference_status_ui <- renderUI({
        res <- seg_inference_result()
        if (res$status == "Running...") {
            tags$div(class = "alert alert-info", "Running inference...")
        } else if (res$status == "Error") {
            tags$div(class = "alert alert-danger", HTML(paste("<strong>Error:</strong>", res$error)))
        }
    })
    output$seg_inference_image_output <- renderImage(
        {
            res <- seg_inference_result()
            req(res$status == "Success", res$image_url)
            image_url <- paste0(api_url, res$image_url)
            temp_file <- tempfile(fileext = ".jpg")
            download.file(image_url, temp_file, mode = "wb")
            list(src = temp_file, contentType = "image/jpeg", alt = "Inference Result")
        },
        deleteFile = TRUE
    )


    # ==============================================================================
    # == 8. SYSTEM HEALTH MONITORING
    # ==============================================================================


    observe({
        # Poll every 5 seconds
        invalidateLater(5000, session)

        # Only poll if we are on the deep learning tab
        # req(input$tabs == "cnndeep") # Optional, saves bandwidth if other tabs open

        tryCatch(
            {
                req <- request(paste0(api_url, "/system/health"))
                resp <- req_perform(req)
                data <- resp_body_json(resp)
                health_data(data)
            },
            error = function(e) {
                health_data(NULL)
            }
        )
    })

    output$status_cpu_box <- renderInfoBox({
        d <- health_data()
        val <- if (is.null(d)) "..." else paste0(d$cpu$percent, "%")
        infoBox("CPU Usage", val, icon = icon("microchip"), color = "aqua", fill = TRUE)
    })

    output$status_ram_box <- renderInfoBox({
        d <- health_data()
        val <- if (is.null(d)) "..." else paste0(d$ram$percent, "%")
        infoBox("RAM Usage", val, icon = icon("memory"), color = "purple", fill = TRUE)
    })

    output$status_disk_box <- renderInfoBox({
        d <- health_data()
        val <- if (is.null(d)) "..." else paste0(d$disk$percent, "%")
        infoBox("Disk Usage", val, icon = icon("hdd"), color = "yellow", fill = TRUE)
    })

    output$status_gpu_box <- renderInfoBox({
        d <- health_data()
        if (is.null(d) || length(d$gpu) == 0) {
            infoBox("GPU", "None", icon = icon("desktop"), color = "red", fill = TRUE)
        } else {
            # Show utilization of the first GPU
            first_gpu <- d$gpu[[1]]
            val <- paste0(first_gpu$utilization, "%")
            infoBox(first_gpu$name, val, icon = icon("bolt"), color = "green", fill = TRUE)
        }
    })
}
