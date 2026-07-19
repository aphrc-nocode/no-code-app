# server.R

# Increase max file upload size (e.g., to 2GB)
options(shiny.maxRequestSize = 3000000*1024^2)

deep_learning = function() {

    # Deep-learning API endpoint and key are configurable via environment.
    # Defaults preserve current behaviour; set DL_API_URL / DL_API_KEY in .env
    # for other deployments. The key must match the API's API_KEY env var and is
    # required by the training/upload/inference endpoints.
    api_url <- Sys.getenv("DL_API_URL", "http://23.135.236.5:3186")
    api_key <- Sys.getenv("DL_API_KEY", "aphrc-secret-key-123")

    # Helper: build a request to the DL API with the auth header attached.
    dl_request <- function(path) {
        request(paste0(api_url, path)) %>%
            req_headers("X-API-Key" = api_key)
    }
    
    # ==============================================================================
    # == 1. CORE REACTIVE VALUES
    # ==============================================================================
    
    # --- For Live Job Polling ---
    polled_data <- reactiveVal(list(
        status = "Idle", task = "N/A", log = "", progress = list(percentage = 0, text = "Idle")
    ))
    polled_metrics <- reactiveVal(NULL)
    active_job_id <- reactiveVal(NULL)
    
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
	 ## FIXME: This should only happen if deeplearning module is activated
    observe({
        tryCatch({
            req <- request(paste0(api_url, "/models/list"))
            resp <- req_perform(req)
            model_registry(resp_body_json(resp))
        }, error = function(e) {
            # print(paste("Failed to fetch model registry:", e$message))
            # TODO: Show a fatal error modal to the user
        })
    })

    # --- Task Panel Switching ---
    # Show/hide the correct training UI based on the main task selector
    observe({
        task <- input$task_selector
        if (task == "object_detection") {
            shinyjs::show("obj_panel"); shinyjs::hide("img_class_panel"); shinyjs::hide("seg_panel")
        } else if (task == "image_classification") {
            shinyjs::hide("obj_panel"); shinyjs::show("img_class_panel"); shinyjs::hide("seg_panel")
        } else if (task == "image_segmentation") {
            shinyjs::hide("obj_panel"); shinyjs::hide("img_class_panel"); shinyjs::show("seg_panel")
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

    
    # ==============================================================================
    # == 3. "DATA MANAGEMENT" TAB LOGIC
    # ==============================================================================

    # --- Helper: Load Datasets for a specific task ---
    load_datasets_for_task <- function(task_slug) {
        tryCatch({
            req <- request(paste0(api_url, "/data/list/", task_slug))
            resp_data <- resp_body_json(req_perform(req), simplifyVector = TRUE)
            if (length(resp_data) > 0 && nrow(resp_data) > 0) {
                setNames(resp_data$id, resp_data$name)
            } else {
                c("No datasets found" = "")
            }
        }, error = function(e) {
            c("Error loading datasets" = "")
        })
    }
    
    # --- Auto-refresh Dataset Dropdowns ---
    # Triggered by: 1. Task selector change, 2. Data refresh trigger
    observeEvent(c(input$task_selector, refresh_data_trigger()), {
        task_slug <- input$task_selector
        if (task_slug == "object_detection") {
            updateSelectInput(session, "obj_dataset_id", choices = load_datasets_for_task("object_detection"))
        } else if (task_slug == "image_classification") {
            updateSelectInput(session, "img_class_dataset_id", choices = load_datasets_for_task("image_classification"))
        } else if (task_slug == "image_segmentation") {
            updateSelectInput(session, "seg_dataset_id", choices = load_datasets_for_task("image_segmentation"))
        }
    }, ignoreNULL = TRUE, ignoreInit = TRUE) 
    
    # Manually trigger first data load on startup (after registry is loaded)
    observeEvent(model_registry(), {
         req(model_registry()) 
         refresh_data_trigger(refresh_data_trigger() + 1)
    }, once = TRUE)

    
    # --- Handle Dataset Upload Button ---
    observeEvent(input$start_data_upload, {
        req(input$new_data_zip, input$new_data_name, input$new_data_task_type)
        data_upload_status("Uploading...")
        tryCatch({
            req <- dl_request(paste0("/data/upload/", input$new_data_task_type)) %>%
                req_body_multipart(
                    data_name = input$new_data_name,
                    data_file = curl::form_file(input$new_data_zip$datapath, type = "application/zip")
                )
            resp <- req_perform(req)
            resp_data <- resp_body_json(resp)
            # Start the poller
            processing_dataset_id(resp_data$dataset_id) 
            data_upload_status(paste("Success! Dataset", input$new_data_name, "is processing..."))
            
        }, error = function(e) {
            error_message <- as.character(e$message)
            if(!is.null(e$body)) { error_message <- paste("API Error:", e$body) }
            data_upload_status(paste("Error:", error_message))
        })
    })
    
    # --- Poller for Data Processing Status ---
    observe({
        ds_id <- processing_dataset_id()
        req(ds_id) # Only run if we are processing a dataset
        
        invalidateLater(2000, session) # Poll every 2 seconds
        
        tryCatch({
            req_status <- request(paste0(api_url, "/data/status/", ds_id))
            resp <- req_perform(req_status)
            status_data <- resp_body_json(resp)
            
            if (status_data$status == "ready" || status_data$status == "failed") {
                processing_dataset_id(NULL) # Stop polling
                
                # --- AUTO-REFRESH ---
                refresh_data_trigger(refresh_data_trigger() + 1)
                
                if(status_data$status == "ready") {
                    data_upload_status(paste("Dataset processing complete!"))
                } else {
                    data_upload_status(paste("Dataset processing failed:", status_data$error))
                }
                
            } else {
                data_upload_status(paste("Processing dataset...", status_data$status))
            }
        }, error = function(e) {
            data_upload_status("Error polling data status.")
            processing_dataset_id(NULL) # Stop polling on error
        })
    })
    
    # --- Data Management UI Outputs ---
    output$data_upload_status <- renderText({ data_upload_status() })

    output$dataset_table <- renderDT({
        refresh_data_trigger() # React to the trigger
        
        tryCatch({
            tasks <- c("object_detection", "image_classification", "image_segmentation")
            all_datasets <- lapply(tasks, function(task) {
                req <- request(paste0(api_url, "/data/list/", task))
                resp_data <- resp_body_json(req_perform(req), simplifyVector = TRUE)
                if (length(resp_data) > 0 && nrow(resp_data) > 0) {
                    resp_data$task_type <- task
                    return(resp_data)
                }
                return(NULL)
            })
            bind_rows(all_datasets)
        }, error = function(e) {
            data.frame(name = "Error loading dataset list.", task_type = e$message)
        })
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
        updateTabsetPanel(session, "main_tabs", selected = "Live Training")
    }

    # --- 4.1: Object Detection Job ---
    observeEvent(input$start_obj_job, {
        req(input$obj_dataset_id, input$obj_model_checkpoint)
        reset_live_training_ui("Object Detection")

        tryCatch({
            req <- dl_request("/train/object-detection") %>%
                req_body_multipart(
                    # Common Params
                    dataset_id = as.character(input$obj_dataset_id),
                    model_checkpoint = as.character(input$obj_model_checkpoint),
                    run_name = as.character(input$obj_run_name),
                    version = as.character(input$obj_version),
                    epochs = as.character(input$obj_epochs),
                    train_batch_size = as.character(input$obj_train_batch_size),
                    eval_batch_size = as.character(input$obj_eval_batch_size),
                    seed = as.character(input$obj_seed),
                    early_stopping_patience = as.character(input$obj_early_stopping_patience),
                    max_image_size = as.character(input$obj_max_image_size),
                    
                    # HF-Specific Params
                    learning_rate = as.character(input$obj_learning_rate),
                    weight_decay_hf = as.character(input$obj_weight_decay),
                    gradient_accumulation_steps = as.character(input$obj_gradient_accumulation_steps),
                    gradient_checkpointing = as.character(input$obj_gradient_checkpointing),
                    early_stopping_threshold = as.character(input$obj_early_stopping_threshold),
                    
                    # YOLO-Specific Params
                    warmup_epochs = as.character(input$obj_yolo_warmup_epochs),
                    lr0 = as.character(input$obj_yolo_lr0),
                    momentum = as.character(input$obj_yolo_momentum),
                    optimizer = as.character(input$obj_yolo_optimizer),
                    weight_decay_yolo = as.character(input$obj_yolo_weight_decay)
                )
            
            resp <- req_perform(req)
            resp_data <- resp_body_json(resp)
            active_job_id(resp_data$job_id)
            polled_data(list(status = "Queued", task = "Object Detection", log = "Job is queued."))

        }, error = function(e) {
            error_message <- as.character(e$message)
            if(!is.null(e$body)) { error_message <- paste("API Error:", e$body) }
            polled_data(list(status = "Error", task = "Object Detection", log = error_message))
        })
    })
    
    # --- 4.2: Image Classification Job ---
    observeEvent(input$start_img_class_job, {
        req(input$img_class_dataset_id, input$img_class_model_checkpoint)
        reset_live_training_ui("Image Classification")

        tryCatch({
            req <- dl_request("/train/image-classification") %>%
                req_body_multipart(
                    dataset_id = as.character(input$img_class_dataset_id),
                    model_checkpoint = as.character(input$img_class_model_checkpoint),
                    run_name = as.character(input$img_class_run_name),
                    version = as.character(input$img_class_version),
                    epochs = as.character(input$img_class_epochs),
                    learning_rate = as.character(input$img_class_learning_rate),
                    weight_decay = as.character(input$img_class_weight_decay),
                    train_batch_size = as.character(input$img_class_train_batch_size),
                    eval_batch_size = as.character(input$img_class_eval_batch_size),
                    max_image_size = as.character(input$img_class_max_image_size),
                    gradient_accumulation_steps = as.character(input$img_class_grad_accum),
                    gradient_checkpointing = as.character(input$img_class_grad_check),
                    seed = as.character(input$img_class_seed),
                    early_stopping_patience = as.character(input$img_class_early_stop),
                    is_presplit = as.character(input$img_class_is_presplit),
                    train_ratio = as.character(input$img_class_train_ratio),
                    dev_ratio = as.character(input$img_class_dev_ratio)
                )
            
            resp <- req_perform(req)
            active_job_id(resp_body_json(resp)$job_id)
        }, error = function(e) {
            polled_data(list(status = "Error", task = "Image Classification", log = as.character(e)))
        })
    })
    
    # --- 4.3: Image Segmentation Job ---
    observeEvent(input$start_seg_job, {
        req(input$seg_dataset_id, input$seg_model_checkpoint)
        reset_live_training_ui("Image Segmentation")

        tryCatch({
            req <- dl_request("/train/image-segmentation") %>%
                req_body_multipart(
                    dataset_id = as.character(input$seg_dataset_id),
                    model_checkpoint = as.character(input$seg_model_checkpoint),
                    run_name = as.character(input$seg_run_name),
                    version = as.character(input$seg_version),
                    epochs = as.character(input$seg_epochs),
                    learning_rate = as.character(input$seg_learning_rate),
                    weight_decay = as.character(input$seg_weight_decay),
                    train_batch_size = as.character(input$seg_train_batch_size),
                    eval_batch_size = as.character(input$seg_eval_batch_size),
                    max_image_size = as.character(input$seg_max_image_size),
                    gradient_accumulation_steps = as.character(input$seg_grad_accum),
                    gradient_checkpointing = as.character(input$seg_grad_check),
                    seed = as.character(input$seg_seed),
                    early_stopping_patience = as.character(input$seg_early_stop),
                    is_presplit = as.character(input$seg_is_presplit),
                    train_ratio = as.character(input$seg_train_ratio),
                    dev_ratio = as.character(input$seg_dev_ratio)
                )
            
            resp <- req_perform(req)
            active_job_id(resp_body_json(resp)$job_id)
        }, error = function(e) {
            error_message <- as.character(e$message)
            if(!is.null(e$body)) { error_message <- paste("API Error:", e$body) }
            polled_data(list(status = "Error", task = "Image Segmentation", log = error_message))
        })
    })


    # ==============================================================================
    # == 5. "LIVE TRAINING" TAB LOGIC
    # ==============================================================================

    # --- 5.1: Job Polling & Status Display ---
    observe({
        job_id <- active_job_id()
        current_status <- polled_data()$status
        
        # Only poll if we have an active job that isn't finished
        if (!is.null(job_id) && !(current_status %in% c("completed", "failed", "Error", "Polling Error"))) {
            invalidateLater(2000, session)
            
            # Poll for Status & Log
            tryCatch({
                req_status <- request(paste0(api_url, "/status/", job_id))
                resp_status <- req_perform(req_status)
                if (resp_status(resp_status) == 200) {
                    polled_data(resp_body_json(resp_status))
                }
            }, error = function(e) {
                current_data <- polled_data()
                current_data$status <- "Polling Error"
                polled_data(current_data)
            })
            
            # Poll for Metrics
            tryCatch({
                req_metrics <- request(paste0(api_url, "/metrics/", job_id))
                resp_metrics <- req_perform(req_metrics)
                if (resp_status(resp_metrics) == 200) {
                    polled_metrics(resp_body_json(resp_metrics))
                }
            }, error = function(e) { 
                polled_metrics(NULL) 
            })
        }
    })
    
    # --- 5.2: Job Status Panel Outputs ---
    output$job_task_display <- renderText({ polled_data()$task })
    output$job_id_display <- renderText({ ifelse(is.null(active_job_id()), "None", active_job_id()) })
    output$job_status_display <- renderText({ polled_data()$status })
    
    # --- 5.3: Metrics Table Panel Output ---
    output$eval_table <- renderDT({
        metrics_list <- polled_metrics()
        req(metrics_list, length(metrics_list) > 0)
        
        metrics_df <- bind_rows(metrics_list)
        
        if (nrow(metrics_df) > 0) {
            display_df <- metrics_df %>%
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
                    class = 'cell-border stripe'
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

    # --- 5.5: Plotting Logic for "Live Training" Tab ---
    metrics_for_plotting <- reactive({
        metrics_list <- polled_metrics()
        req(metrics_list, length(metrics_list) > 0)

        bind_rows(metrics_list) %>%
            filter(!is.na(epoch)) %>%
            filter(if_any(everything(), ~ !is.na(.))) %>%
            select(starts_with("eval_"), epoch) %>%
            arrange(epoch) %>%
            distinct(epoch, .keep_all = TRUE)
    })
    
    output$metric_selector_ui <- renderUI({
        df <- tryCatch(metrics_for_plotting(), error = function(e) NULL)
        
        if (is.null(df) || nrow(df) == 0) {
            return(p("Waiting for first evaluation epoch to complete..."))
        }
        
        metric_names <- names(df)[sapply(df, is.numeric) & !names(df) %in% c("epoch", "step", "runtime", "samples_per_second", "steps_per_second")]
        
        default_metric <- "eval_loss"
        if (!"eval_loss" %in% metric_names && length(metric_names) > 0) {
            default_metric <- metric_names[1]
        }
        
        selected_val <- input$selected_metric
        if (is.null(selected_val) || !selected_val %in% metric_names) {
            selected_val <- default_metric
        }
        
        selectInput("selected_metric", "Select Metric to Plot:", 
                    choices = metric_names, 
                    selected = selected_val) 
    })
    
    output$dynamic_metric_plot <- renderDygraph({
        df <- tryCatch(metrics_for_plotting(), error = function(e) NULL)
        
        if (is.null(df) || nrow(df) == 0) {
             return(dygraph(data.frame(x=c(0), y=c(0)), main = "Waiting for Epoch 1") %>%
               dyOptions(drawGrid = FALSE, drawAxes = FALSE, drawYAxis = FALSE, drawXAxis = FALSE) %>%
               dyAxis("x", label = "Epoch"))
        }
        
        req(input$selected_metric) 
        req(input$selected_metric %in% names(df))
        
        metric_to_plot <- input$selected_metric
        
        if (!"epoch" %in% names(df) || !metric_to_plot %in% names(df)) {
             return(dygraph(data.frame(x=c(0), y=c(0)), main = "Metric data not yet available") %>%
               dyOptions(drawGrid = FALSE, drawAxes = FALSE, drawYAxis = FALSE, drawXAxis = FALSE))
        }

        plot_data <- df[, c("epoch", metric_to_plot)]
        
        dygraph(plot_data, main = paste(metric_to_plot, "vs. Epoch")) %>%
            dySeries(metric_to_plot, label = metric_to_plot) %>%
            dyAxis("x", label = "Epoch", valueRange = c(0, max(df$epoch, na.rm = TRUE) + 1)) %>%
            dyRangeSelector() %>%
            dyOptions(stackedGraph = FALSE, 
                      fillGraph = FALSE,
                      stepPlot = FALSE,
                      drawPoints = TRUE,
                      pointSize = 4) %>% 
            dyLegend(show = "always", width = 200)
    })
    
    
    # ==============================================================================
    # == 6. "TRAINING HISTORY" TAB LOGIC
    # ==============================================================================

    # --- 6.1: Populate the Job Selector Dropdown (with Filters) ---
    observe({
        # React to tab switching, job completion, and filter changes
        input$main_tabs
        polled_data() 
        input$history_task_filter
        input$history_status_filter
        
        tryCatch({
            
            query_params <- list()
            if (input$history_task_filter != "all") {
                query_params$task_type <- input$history_task_filter
            }
            if (input$history_status_filter != "all") {
                query_params$status <- input$history_status_filter
            }
            
            req <- request(paste0(api_url, "/jobs/list"))
            if (length(query_params) > 0) {
                req <- req_url_query(req, !!!query_params)
            }
                
            resp <- req_perform(req)
            jobs_list_raw <- resp_body_json(resp, simplifyVector = FALSE)
            
            if (length(jobs_list_raw) > 0) {
                
                jobs_df <- bind_rows(lapply(jobs_list_raw, function(job) {
                    data.frame(
                        id = job$id,
                        task_type = job$task_type,
                        status = job$status,
                        run_name = ifelse(is.null(job$details$run_name), "N/A", job$details$run_name)
                    )
                }))

                history_jobs_df(jobs_df)

                job_names <- paste0(
                    jobs_df$run_name, 
                    " (", jobs_df$task_type, " | ID: ", substr(jobs_df$id, 1, 8), ") - ", 
                    jobs_df$status
                )
                job_choices <- setNames(jobs_df$id, job_names)
                
                updateSelectInput(session, "history_job_selector", choices = job_choices)
                
            } else {
                updateSelectInput(session, "history_job_selector", choices = c("No jobs found" = ""))
                history_jobs_df(NULL)
            }
        }, error = function(e) {
            updateSelectInput(session, "history_job_selector", choices = c("Error loading jobs" = ""))
            history_jobs_df(NULL)
        })
    })

    # --- 6.2: Fetch Metrics when user selects a historical job ---
    observeEvent(input$history_job_selector, {
        job_id <- input$history_job_selector
        
        history_poller_active(FALSE) # Deactivate poller by default
        
        if (!is.null(job_id) && nchar(job_id) > 0) {
            tryCatch({
                req <- request(paste0(api_url, "/metrics/", job_id))
                resp <- req_perform(req)
                history_metrics(resp_body_json(resp))
            }, error = function(e) {
                history_metrics(NULL)
            })
            
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

        bind_rows(metrics_list) %>%
            filter(!is.na(epoch)) %>% # <--- THIS IS THE FIX
            filter(if_any(everything(), ~ !is.na(.))) %>%
            select(starts_with("eval_"), epoch) %>%
            arrange(epoch) %>%
            distinct(epoch, .keep_all = TRUE)
    })

    # --- 6.4: Render Historical Metrics Table ---
    output$history_eval_table <- renderDT({
        metrics_list <- history_metrics()
        req(metrics_list, length(metrics_list) > 0)
        
        metrics_df <- bind_rows(metrics_list)
        
        if (nrow(metrics_df) > 0) {
            display_df <- metrics_df %>%
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
        
        metric_names <- names(df)[sapply(df, is.numeric) & !names(df) %in% c("epoch", "step", "runtime", "samples_per_second", "steps_per_second")]
        
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
                    selected = selected_val)
    })

    # --- 6.6: Render Historical Plot ---
    output$history_metric_plot <- renderDygraph({
        df <- tryCatch(history_metrics_for_plotting(), error = function(e) NULL)
        
        if (is.null(df) || nrow(df) == 0) {
             return(dygraph(data.frame(x=c(0), y=c(0)), main = "No Metric Data") %>%
               dyOptions(drawGrid = FALSE, drawAxes = FALSE, drawYAxis = FALSE, drawXAxis = FALSE) %>%
               dyAxis("x", label = "Epoch"))
        }

        req(input$history_selected_metric) 
        req(input$history_selected_metric %in% names(df))
        
        metric_to_plot <- input$history_selected_metric
        
        if (!"epoch" %in% names(df) || !metric_to_plot %in% names(df)) {
             return(dygraph(data.frame(x=c(0), y=c(0)), main = "Metric data not yet available") %>%
               dyOptions(drawGrid = FALSE, drawAxes = FALSE, drawYAxis = FALSE, drawXAxis = FALSE))
        }

        plot_data <- df[, c("epoch", metric_to_plot)]
        
        dygraph(plot_data, main = paste(metric_to_plot, "vs. Epoch")) %>%
            dySeries(metric_to_plot, label = metric_to_plot) %>%
            dyAxis("x", label = "Epoch", valueRange = c(0, max(df$epoch, na.rm = TRUE) + 1)) %>%
            dyRangeSelector() %>%
            dyOptions(stackedGraph = FALSE, 
                      fillGraph = FALSE,
                      stepPlot = FALSE,
                      drawPoints = TRUE,
                      pointSize = 4) %>% 
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
        
        tryCatch({
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
                    # Refresh the job list dropdown to show "completed".
                    # (Bump the trigger directly; registering an observer here would
                    # leak a new observer on every poll tick.)
                    refresh_data_trigger(refresh_data_trigger() + 1)
                }
            }
            
        }, error = function(e) {
            print(paste("History Poller Error:", e$message))
            history_poller_active(FALSE) # Stop polling on error
        })
    })
    
    
    # ==============================================================================
    # == 7. "INFERENCE" TAB LOGIC
    # ==============================================================================
    
    # --- 7.1: Inference Checkpoint Finders ---
    observeEvent(input$infer_run_name, {
        run_name <- input$infer_run_name
        if (nchar(run_name) > 2) { 
            tryCatch({
                req <- request(paste0(api_url, "/checkpoints")) %>%
                    req_url_query(run_name = run_name, task_type = "object_detection")
                resp <- req_perform(req)
                if (resp_status(resp) == 200) {
                    checkpoints <- resp_body_json(resp, simplifyVector = TRUE)
                    updateSelectInput(session, "infer_checkpoint_dropdown", choices = checkpoints)
                }
            }, error = function(e) {
                updateSelectInput(session, "infer_checkpoint_dropdown", choices = c("Error finding checkpoints"))
            })
        }
    })
    
    observeEvent(input$infer_img_class_run_name, {
        run_name <- input$infer_img_class_run_name
        if (nchar(run_name) > 2) { 
            tryCatch({
                req <- request(paste0(api_url, "/checkpoints")) %>%
                    req_url_query(run_name = run_name, task_type = "image_classification")
                checkpoints <- resp_body_json(req_perform(req), simplifyVector = TRUE)
                updateSelectInput(session, "infer_img_class_checkpoint_dropdown", choices = checkpoints)
            }, error = function(e) { })
        }
    })
    
    observeEvent(input$infer_seg_run_name, {
        run_name <- input$infer_seg_run_name
        if (nchar(run_name) > 2) { 
            tryCatch({
                req <- request(paste0(api_url, "/checkpoints")) %>%
                    req_url_query(run_name = run_name, task_type = "image_segmentation")
                checkpoints <- resp_body_json(req_perform(req), simplifyVector = TRUE)
                updateSelectInput(session, "infer_seg_checkpoint_dropdown", choices = checkpoints)
            }, error = function(e) { })
        }
    })

    # --- 7.2: Inference Job Submission (One per task) ---
    
    observeEvent(input$start_obj_inference, {
        req(input$infer_obj_image_upload); req(input$infer_checkpoint_dropdown)
        obj_inference_result(list(status = "Running...", image_url = NULL, error = NULL))
        tryCatch({
            req <- dl_request("/inference/object-detection") %>%
                req_body_multipart(
                    image = curl::form_file(input$infer_obj_image_upload$datapath), 
                    model_checkpoint = input$infer_checkpoint_dropdown,
                    threshold = as.character(input$infer_obj_threshold),
                    iou = as.character(input$infer_obj_iou),
                    max_det = as.character(input$infer_obj_max_det)
                )
            resp <- req_perform(req)
            resp_data <- resp_body_json(resp)
            obj_inference_result(list(status = "Success", image_url = resp_data$output_url, error = NULL))
        }, error = function(e) {
            error_message <- as.character(e$message)
            if(!is.null(e$body)) { error_message <- paste("API Error:", e$body) }
            obj_inference_result(list(status = "Error", image_url = NULL, error = error_message))
        })
    })

    observeEvent(input$start_img_class_inference, {
        req(input$infer_img_class_upload, input$infer_img_class_checkpoint_dropdown)
        img_class_inference_result(list(status = "Running...", prediction = "Processing...", error = NULL, image_url = NULL))
        tryCatch({
            if (isTRUE(input$infer_img_class_explain)) {
                # Grad-CAM: returns prediction, confidence, and a heatmap URL.
                req <- dl_request("/explain/image-classification") %>%
                    req_body_multipart(
                        image = curl::form_file(input$infer_img_class_upload$datapath),
                        model_checkpoint = input$infer_img_class_checkpoint_dropdown
                    )
                resp_data <- resp_body_json(req_perform(req))
                pred_text <- resp_data$prediction
                if (!is.null(resp_data$confidence)) {
                    pred_text <- sprintf("%s (confidence: %.1f%%)", pred_text, 100 * as.numeric(resp_data$confidence))
                }
                img_class_inference_result(list(status = "Success", prediction = pred_text,
                                                error = NULL, image_url = resp_data$output_url))
            } else {
                req <- dl_request("/inference/image-classification") %>%
                    req_body_multipart(
                        image = curl::form_file(input$infer_img_class_upload$datapath),
                        model_checkpoint = input$infer_img_class_checkpoint_dropdown
                    )
                resp_data <- resp_body_json(req_perform(req))
                img_class_inference_result(list(status = "Success", prediction = resp_data$prediction,
                                                error = NULL, image_url = NULL))
            }
        }, error = function(e) {
            img_class_inference_result(list(status = "Error", prediction = NULL, error = as.character(e), image_url = NULL))
        })
    })
    
    observeEvent(input$start_seg_inference, {
        req(input$infer_seg_image_upload); req(input$infer_seg_checkpoint_dropdown)
        seg_inference_result(list(status = "Running...", image_url = NULL, error = NULL))
        tryCatch({
            req <- dl_request("/inference/image-segmentation") %>%
                req_body_multipart(
                    image = curl::form_file(input$infer_seg_image_upload$datapath), 
                    model_checkpoint = input$infer_seg_checkpoint_dropdown
                )
            resp <- req_perform(req)
            resp_data <- resp_body_json(resp)
            seg_inference_result(list(status = "Success", image_url = resp_data$output_url, error = NULL))
        }, error = function(e) {
            error_message <- as.character(e$message)
            if(!is.null(e$body)) { error_message <- paste("API Error:", e$body) }
            seg_inference_result(list(status = "Error", image_url = NULL, error = error_message))
        })
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
    output$inference_image_output <- renderImage({
        res <- obj_inference_result()
        req(res$status == "Success", res$image_url)
        image_url <- paste0(api_url, res$image_url)
        temp_file <- tempfile(fileext = ".jpg")
        download.file(image_url, temp_file, mode = "wb")
        list(src = temp_file, contentType = 'image/jpeg', alt = "Inference Result")
    }, deleteFile = TRUE)

    output$img_class_inference_status_ui <- renderUI({
        res <- img_class_inference_result()
        if (res$status == "Running...") tags$div(class = "alert alert-info", "Running inference...")
        else if (res$status == "Error") tags$div(class = "alert alert-danger", HTML(paste("<strong>Error:</strong>", res$error)))
    })
    output$img_class_prediction_output <- renderText({
        img_class_inference_result()$prediction
    })
    # Grad-CAM heatmap overlay (only populated when "Explain" was requested).
    output$img_class_explain_output <- renderImage({
        res <- img_class_inference_result()
        req(res$status == "Success", res$image_url)
        image_url <- paste0(api_url, res$image_url)
        temp_file <- tempfile(fileext = ".png")
        download.file(image_url, temp_file, mode = "wb")
        list(src = temp_file, contentType = 'image/png', alt = "Grad-CAM heatmap")
    }, deleteFile = TRUE)

    output$seg_inference_status_ui <- renderUI({
        res <- seg_inference_result()
        if (res$status == "Running...") {
            tags$div(class = "alert alert-info", "Running inference...")
        } else if (res$status == "Error") {
            tags$div(class = "alert alert-danger", HTML(paste("<strong>Error:</strong>", res$error)))
        }
    })
    output$seg_inference_image_output <- renderImage({
        res <- seg_inference_result()
        req(res$status == "Success", res$image_url)
        image_url <- paste0(api_url, res$image_url)
        temp_file <- tempfile(fileext = ".jpg")
        download.file(image_url, temp_file, mode = "wb")
        list(src = temp_file, contentType = 'image/jpeg', alt = "Inference Result")
    }, deleteFile = TRUE)

}
