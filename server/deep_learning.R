# server.R

library(shiny)
library(httr2)
library(jsonlite)
library(DT)
library(dplyr)
library(shinyjs)
library(tidyr) 

deep_learning=function() {
    
    api_url <- "http://127.0.0.1:8000"
    
    polled_data <- reactiveVal(list(
        status = "Idle", task = "N/A", log = "", progress = list(percentage = 0, text = "Idle")
    ))
    polled_metrics <- reactiveVal(NULL)
    active_job_id <- reactiveVal(NULL)

    obj_inference_result <- reactiveVal(list(status = "Ready", image_url = NULL, error = NULL))
    asr_inference_result <- reactiveVal(list(status = "Ready", transcription = NULL, error = NULL))

    proxy_eval_table <- dataTableProxy("eval_table")

    observe({
        if (input$task_selector == "Object Detection") {
            shinyjs::show("obj_panel")
            shinyjs::hide("asr_panel")
        } else {
            shinyjs::hide("obj_panel")
            shinyjs::show("asr_panel")
        }
    })

    asr_checkpoints <- list(
        "Whisper" = c("openai/whisper-tiny", "openai/whisper-base", "openai/whisper-small"),
        "XLS-R" = c("facebook/wav2vec2-xls-r-300m", "facebook/wav2vec2-xls-r-1b"),
        "Wav2Vec2-BERT" = c("facebook/w2v-bert-2.0")
    )
    
    observeEvent(input$asr_model_arch, {
        checkpoints <- asr_checkpoints[[input$asr_model_arch]]
        updateSelectInput(session, "asr_model_checkpoint", choices = checkpoints)
    }, ignoreNULL = FALSE)
    
    start_job <- function(endpoint, payload, task_name) {
        replaceData(proxy_eval_table, NULL, resetPaging = TRUE, clearSelection = TRUE)
        active_job_id(NULL)
        polled_metrics(NULL)
        polled_data(list(status = "Submitting...", task = task_name, log = "Submitting job...", progress = list(percentage = 0, text = "Submitting...")))
        
        tryCatch({
            req <- request(paste0(api_url, endpoint)) %>%
                req_headers("Content-Type" = "application/json") %>%
                req_body_json(payload, auto_unbox = TRUE)
            
            resp <- req_perform(req)

            if(resp_is_error(resp)) {
                body_text <- try(resp_body_string(resp), silent = TRUE)
                if (inherits(body_text, "try-error")) {
                    body_text <- "(Could not retrieve error body)"
                }
                stop(paste("API returned an error:", resp_status_desc(resp), "\nDetails:\n", body_text))
            }
            
            resp_data <- resp_body_json(resp)
            active_job_id(resp_data$job_id)
            polled_data(list(status = "Queued", task = task_name, log = "Job is queued.", progress = list(percentage = 0, text = "Queued")))

        }, error = function(e) {
            polled_data(list(status = "Error", task = task_name, log = as.character(e$message), progress = list(percentage = 0, text = "Error")))
        })
    }
    
    observeEvent(input$start_obj_job, {
        payload <- list(
            data_dir = input$obj_data_dir,
            model_checkpoint = input$obj_model_checkpoint,
            run_name = input$obj_run_name,
            version = input$obj_version,
            epochs = input$obj_epochs,
            learning_rate = input$obj_learning_rate,
            weight_decay = input$obj_weight_decay,
            train_batch_size = input$obj_train_batch_size,
            eval_batch_size = input$obj_eval_batch_size,
            gradient_accumulation_steps = input$obj_gradient_accumulation_steps,
            max_image_size = input$obj_max_image_size,
            seed = input$obj_seed,
            num_proc = input$obj_num_proc,
            fp16 = TRUE, 
            push_to_hub = input$obj_push_to_hub,
            log_to_wandb = input$obj_log_to_wandb,
            wandb_project = input$obj_wandb_project,
            wandb_entity = input$obj_wandb_entity,
            early_stopping_patience = input$obj_early_stopping_patience,
            early_stopping_threshold = input$obj_early_stopping_threshold
        )
        start_job("/train/object-detection", payload, "Object Detection")
    })
    
    observeEvent(input$start_asr_job, {
        outlier_val <- input$outlier_std_devs
        if (is.null(outlier_val) || is.na(outlier_val) || !is.numeric(outlier_val)) {
            outlier_val <- 2.0 
        }

        payload <- list(
            data_dir = input$asr_data_dir,
            run_name = input$asr_run_name,
            version = input$asr_version,
            model_checkpoint = input$asr_model_checkpoint,
            language = input$asr_language,
            language_code = input$asr_language_code,
            speaker_id_column = input$asr_speaker_id_column,
            text_column = input$asr_text_column,
            target_sampling_rate = input$asr_target_sampling_rate,
            min_duration_s = input$asr_min_duration_s,
            max_duration_s = input$asr_max_duration_s,
            min_transcript_len = input$asr_min_transcript_len,
            max_transcript_len = input$asr_max_transcript_len,
            apply_outlier_filtering = input$asr_apply_outlier_filtering,
            outlier_std_devs = outlier_val,
            is_presplit = input$asr_is_presplit,
            speaker_disjointness = input$asr_speaker_disjointness,
            train_ratio = input$asr_train_ratio,
            dev_ratio = input$asr_dev_ratio,
            epochs = input$asr_epochs,
            learning_rate = input$asr_learning_rate,
            lr_scheduler_type = input$asr_lr_scheduler_type,
            warmup_ratio = input$asr_warmup_ratio,
            train_batch_size = input$asr_train_batch_size,
            eval_batch_size = input$asr_eval_batch_size,
            gradient_accumulation_steps = input$asr_gradient_accumulation_steps,
            optimizer = input$asr_optimizer,
            early_stopping_patience = input$asr_early_stopping_patience,
            early_stopping_threshold = input$asr_early_stopping_threshold,
            push_to_hub = input$asr_push_to_hub,
            hub_model_id = input$asr_hub_model_id,
            hub_private_repo = input$asr_hub_private_repo,
            log_to_wandb = input$asr_log_to_wandb,
            wandb_project = input$asr_wandb_project,
            wandb_entity = input$asr_wandb_entity,
            seed = input$asr_seed,
            num_proc = input$asr_num_proc
        )
        start_job("/train/asr", payload, "ASR")
    })

    observeEvent(input$infer_run_name, {
        run_name <- input$infer_run_name
        if (nchar(run_name) > 2) { 
            tryCatch({
                req <- request(paste0(api_url, "/checkpoints")) %>%
                    req_url_query(run_name = run_name)
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
    
    observeEvent(input$infer_asr_run_name, {
        run_name <- input$infer_asr_run_name
        if (nchar(run_name) > 2) {
            tryCatch({
                req <- request(paste0(api_url, "/checkpoints")) %>%
                    req_url_query(run_name = run_name)
                resp <- req_perform(req)
                if (resp_status(resp) == 200) {
                    checkpoints <- resp_body_json(resp, simplifyVector = TRUE)
                    updateSelectInput(session, "infer_asr_checkpoint_dropdown", choices = checkpoints)
                }
            }, error = function(e) {
                updateSelectInput(session, "infer_asr_checkpoint_dropdown", choices = c("Error finding checkpoints"))
            })
        }
    })

    observeEvent(input$start_obj_inference, {
        req(input$infer_obj_image_upload); req(input$infer_checkpoint_dropdown)
        obj_inference_result(list(status = "Running...", image_url = NULL, error = NULL))
        tryCatch({
            req <- request(paste0(api_url, "/inference/object-detection")) %>%
                req_body_multipart(
                    image = curl::form_file(input$infer_obj_image_upload$datapath, type = input$infer_obj_image_upload$type), 
                    model_checkpoint = input$infer_checkpoint_dropdown,
                    threshold = as.character(input$infer_obj_threshold)
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

    observeEvent(input$start_asr_inference, {
        req(input$infer_asr_audio_upload)
        req(input$infer_asr_checkpoint_dropdown)
        
        asr_inference_result(list(status = "Running...", transcription = "Processing...", error = NULL))
        
        tryCatch({
            req <- request(paste0(api_url, "/inference/asr")) %>%
                req_body_multipart(
                    audio = curl::form_file(input$infer_asr_audio_upload$datapath, type = input$infer_asr_audio_upload$type),
                    model_checkpoint = input$infer_asr_checkpoint_dropdown
                )
            resp <- req_perform(req)
            resp_data <- resp_body_json(resp)
            asr_inference_result(list(status = "Success", transcription = resp_data$transcription, error = NULL))
        }, error = function(e) {
            error_message <- as.character(e$message)
            if(!is.null(e$body)) { error_message <- paste("API Error:", e$body) }
            asr_inference_result(list(status = "Error", transcription = NULL, error = error_message))
        })
    })
    
    observe({
        job_id <- active_job_id()
        current_status <- polled_data()$status
        
        if (!is.null(job_id) && !(current_status %in% c("completed", "failed", "Error", "Polling Error"))) {
            invalidateLater(2000, session)
            
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
    
    output$job_task_display <- renderText({ polled_data()$task })
    output$job_id_display <- renderText({ ifelse(is.null(active_job_id()), "None", active_job_id()) })
    output$job_status_display <- renderText({ polled_data()$status })
    
    observe({
        progress_val <- polled_data()$progress$percentage
        if (!is.null(progress_val)) {
            runjs(paste0("document.getElementById('progress-bar').style.width = '", progress_val, "%';"))
        }
    })
    
    output$progress_text <- renderText({
        polled_data()$progress$text
    })

    output$eval_table <- renderDT({
        metrics_list <- polled_metrics()
        if (is.null(metrics_list) || length(metrics_list) == 0) return(NULL)
        
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
                    values_from = value
                ) %>%
                select(any_of(c("step", "epoch", "loss", "map", "wer", "cer")), everything())

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

    output$log_output <- renderText({
        log_text <- polled_data()$log
        if (is.null(log_text) || nchar(log_text) == 0) {
            return("No log output...")
        }
        return(log_text)
    })

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

    output$asr_inference_status_ui <- renderUI({
        res <- asr_inference_result()
        if (res$status == "Running...") {
            tags$div(class = "alert alert-info", "Running inference...")
        } else if (res$status == "Error") {
            tags$div(class = "alert alert-danger", HTML(paste("<strong>Error:</strong>", res$error)))
        }
    })

    output$asr_transcription_output <- renderText({
        res <- asr_inference_result()
        if (is.null(res$transcription)) {
            "Upload an audio file and click 'Run Inference' to see the transcription here."
        } else {
            res$transcription
        }
    })
}