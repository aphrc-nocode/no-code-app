# CNN Server Logic
# This file contains all server-side logic for the CNN functionality

deeplearning_cnn <- function(input, output, session) {
  # Helper function for null coalescing
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # API URL configuration
  CNN_API_URL <- Sys.getenv("CNN_API_URL", "http://localhost:9000")
  
  # Helper function for robust base64 decoding
  decode_base64_image <- function(base64_string) {
    if (is.null(base64_string) || base64_string == "") {
      return(NULL)
    }
    
    tryCatch({
      # Remove data URL prefix if present
      if (grepl("^data:image/", base64_string)) {
        base64_string <- sub("^data:image/[^;]+;base64,", "", base64_string)
      }
      
      # Decode base64
      raw_data <- jsonlite::base64_dec(base64_string)
      
      # Create temporary file
      temp_file <- tempfile(fileext = ".png")
      writeBin(raw_data, temp_file)
      
      return(temp_file)
    }, error = function(e) {
      print(paste("Base64 decode error:", e$message))
      return(NULL)
    })
  }
  
  # API Status Check
  observeEvent(input$check_status, {
    tryCatch({
      response <- httr2::request(paste0(CNN_API_URL, "/health")) |> 
        httr2::req_perform()
      status_code <- httr2::resp_status(response)
      
      if (status_code == 200) {
        output$api_status <- renderText({
          "API Status: Connected and Healthy"
        })
      } else {
        output$api_status <- renderText({
          paste("API Status: HTTP Error -", status_code)
        })
      }
    }, error = function(e) {
      output$api_status <- renderText({
        paste("API Status: Connection Failed -", e$message)
      })
    })
  })
  
  # MLflow Server Start
  observeEvent(input$start_mlflow, {
    tryCatch({
      response <- httr2::request(paste0(CNN_API_URL, "/mlflow/start-server")) |> 
        httr2::req_body_json(list()) |>
        httr2::req_perform()
      
      result <- httr2::resp_body_json(response)
      output$mlflow_output <- renderText({
        paste("MLflow Server:", result$message)
      })
    }, error = function(e) {
      output$mlflow_output <- renderText({
        paste("MLflow Error:", e$message)
      })
    })
  })
  
  # Create Pipeline
  observeEvent(input$create_pipeline, {
    # Validate inputs
    if (is.null(input$pipeline_name) || input$pipeline_name == "") {
      output$create_output <- renderText({
        "Pipeline creation failed: Pipeline name is required"
      })
      return()
    }
    
    # Parse image_size - convert "224, 224" to [224, 224]
    image_size_clean <- input$image_size
    if (grepl(",", image_size_clean)) {
      # Remove spaces and split by comma
      image_size_parts <- strsplit(gsub("\\s+", "", image_size_clean), ",")[[1]]
      image_size_array <- as.numeric(image_size_parts)
    } else {
      # Single number, assume square
      image_size_array <- c(as.numeric(image_size_clean), as.numeric(image_size_clean))
    }
    
    # Configuration matching the exact API schema
    pipeline_config <- list(
      name = input$pipeline_name,
      task_type = input$task_type,
      architecture = input$architecture,
      num_classes = as.numeric(input$num_classes),
      batch_size = as.numeric(input$batch_size),
      learning_rate = as.numeric(input$learning_rate),
      epochs = as.numeric(input$epochs),
      image_size = image_size_array,
      augmentation_enabled = input$augmentation,
      early_stopping = input$early_stopping,
      patience = 5,  # Default value
      use_hf_transformers = FALSE,  # Default for non-transformer models
      hf_model_checkpoint = "",  # Empty string for non-HF models
      feature_extraction_only = FALSE,  # Default
      dataset_source = "local"  # Default
    )
    
    # Only add segmentation_type for segmentation tasks
    if (input$task_type == "image_segmentation") {
      pipeline_config$segmentation_type <- "semantic"
    }
    
    # Debug: Print what we're sending
    print("Creating pipeline with config:")
    print(str(pipeline_config))
    
    tryCatch({
      response <- httr2::request(paste0(CNN_API_URL, "/pipelines")) |> 
        httr2::req_body_json(pipeline_config) |>
        httr2::req_perform()
      
      status_code <- httr2::resp_status(response)
      print(paste("Response status:", status_code))
      
      if (status_code >= 200 && status_code < 300) {
        result <- httr2::resp_body_json(response)
        output$create_output <- renderText({
          paste("Pipeline created successfully! ID:", result$id %||% "Unknown")
        })
      } else {
        # Get detailed error information
        response_body <- tryCatch({
          httr2::resp_body_json(response)
        }, error = function(e) {
          list(error = httr2::resp_body_string(response))
        })
        
        print("Error response:")
        print(str(response_body))
        
        # Try to extract validation errors
        error_msg <- "Unknown error"
        if (!is.null(response_body$error)) {
          error_msg <- response_body$error
        } else if (!is.null(response_body$detail)) {
          error_msg <- response_body$detail
        } else if (!is.null(response_body$message)) {
          error_msg <- response_body$message
        }
        
        # Check for validation errors specifically
        if (!is.null(response_body$detail) && is.list(response_body$detail)) {
          validation_errors <- response_body$detail
          error_msg <- paste("Validation errors:")
          for (field in names(validation_errors)) {
            error_msg <- paste(error_msg, paste(field, ":", paste(validation_errors[[field]], collapse = ", ")))
          }
        }
        
        output$create_output <- renderText({
          paste("Pipeline creation failed:", "HTTP", status_code, "-", error_msg)
        })
      }
    }, error = function(e) {
      print(paste("Pipeline creation error:", e$message))
      output$create_output <- renderText({
        paste("Pipeline creation failed:", e$message)
      })
    })
  })
  
  # Refresh Dashboard Jobs
  observeEvent(input$refresh_dashboard_jobs, {
    tryCatch({
      response <- httr2::request(paste0(CNN_API_URL, "/pipelines")) |> 
        httr2::req_perform()
      
      jobs <- httr2::resp_body_json(response)
      
      # Debug: Print what we received
      print("Received jobs data:")
      print(str(jobs))
      
      # Handle different response formats
      if (is.null(jobs) || length(jobs) == 0) {
        # Empty response
        output$dashboard_jobs_table <- DT::renderDataTable({
          DT::datatable(data.frame(Message = "No jobs found"), options = list(pageLength = 10))
        })
      } else if (is.data.frame(jobs)) {
        # Already a data frame
        output$dashboard_jobs_table <- DT::renderDataTable({
          DT::datatable(jobs, options = list(pageLength = 10))
        })
      } else if (is.list(jobs) && !is.data.frame(jobs)) {
        # Convert list to data frame - handle nested structure
        if (length(jobs) > 0) {
          # Extract key fields from each job
          jobs_data <- lapply(jobs, function(job) {
            list(
              ID = job$id %||% "",
              Name = job$pipeline_config$name %||% "",
              Task_Type = job$pipeline_config$task_type %||% "",
              Architecture = job$pipeline_config$architecture %||% "",
              Status = job$status %||% "",
              Created_At = job$created_at %||% "",
              Classes = job$pipeline_config$num_classes %||% 0,
              Epochs = job$pipeline_config$epochs %||% 0
            )
          })
          
          jobs_df <- do.call(rbind, lapply(jobs_data, data.frame, stringsAsFactors = FALSE))
          output$dashboard_jobs_table <- DT::renderDataTable({
            DT::datatable(jobs_df, options = list(pageLength = 10))
          })
        } else {
          output$dashboard_jobs_table <- DT::renderDataTable({
            DT::datatable(data.frame(Message = "No jobs found"), options = list(pageLength = 10))
          })
        }
      } else {
        # Try to convert to data frame
        output$dashboard_jobs_table <- DT::renderDataTable({
          DT::datatable(data.frame(jobs), options = list(pageLength = 10))
        })
      }
    }, error = function(e) {
      print(paste("Jobs refresh error:", e$message))
      output$dashboard_jobs_table <- DT::renderDataTable({
        DT::datatable(data.frame(Error = paste("Failed to load jobs:", e$message)), options = list(pageLength = 10))
      })
    })
  })
  
  # Current Job Status
  observeEvent(input$refresh_current_job, {
    tryCatch({
      response <- httr2::request(paste0(CNN_API_URL, "/pipelines")) |> 
        httr2::req_perform()
      
      jobs <- httr2::resp_body_json(response)
      if (length(jobs) > 0) {
        latest_job <- jobs[[1]]
        output$current_job_status <- renderText({
          paste("Latest Job:", latest_job$name, "- Status:", latest_job$status)
        })
      } else {
        output$current_job_status <- renderText({
          "No jobs found"
        })
      }
    }, error = function(e) {
      output$current_job_status <- renderText({
        paste("Error fetching job status:", e$message)
      })
    })
  })
  
  # Upload Dataset
  observeEvent(input$upload_dataset, {
    if (is.null(input$dataset_file)) {
      output$upload_dataset_output <- renderText({
        "Please select a dataset file to upload"
      })
      return()
    }
    
    if (is.null(input$upload_job_dropdown) || input$upload_job_dropdown == "") {
      output$upload_dataset_output <- renderText({
        "Please select a job first."
      })
      return()
    }
    
    file_path <- input$dataset_file$datapath
    file_name <- input$dataset_file$name
    file_size <- file.info(file_path)$size
    
    if (!file.exists(file_path)) {
      output$upload_dataset_output <- renderText({
        "Error: Selected file does not exist."
      })
      return()
    }
    
    # Show file size info
    file_size_mb <- round(file_size / (1024^2), 2)
    if (file_size_mb > 500) {
      output$upload_dataset_output <- renderText({
        paste("Error: File size (", file_size_mb, "MB) exceeds maximum limit of 500MB.")
      })
      return()
    }
    
    tryCatch({
      # Choose the correct endpoint based on dataset type
      if (input$is_coco_format_upload) {
        # COCO format object detection dataset
        upload_url <- paste0(CNN_API_URL, "/upload-detection-dataset/", input$upload_job_dropdown)
      } else {
        # Regular classification dataset
        upload_url <- paste0(CNN_API_URL, "/upload-dataset/", input$upload_job_dropdown)
      }
      
      # Create multipart form data with timeout for large files
      if (input$is_coco_format_upload) {
        # For COCO format, use the detection endpoint (no additional parameters needed)
        response <- httr::POST(
          upload_url,
          body = list(
            file = httr::upload_file(file_path, type = "application/zip")
          ),
          encode = "multipart",
          httr::timeout(300)  # 5 minute timeout for large uploads
        )
      } else {
        # For classification datasets, specify file_type = "zip"
        response <- httr::POST(
          upload_url,
          body = list(
            file = httr::upload_file(file_path, type = "application/zip"),
            file_type = "zip"
          ),
          encode = "multipart",
          httr::timeout(300)  # 5 minute timeout for large uploads
        )
      }
      
      if (httr::status_code(response) == 200) {
        result <- httr::content(response, "parsed")
        message <- result$message %||% "Upload completed"
        
        # After successful upload, we need to link the dataset to the job
        # The dataset ID is the same as the job ID when uploaded directly
        job_id <- input$upload_job_dropdown
        dataset_id <- job_id  # When uploading directly, dataset_id = job_id
        
        # Link the dataset to the job
        link_response <- tryCatch({
          link_url <- paste0(CNN_API_URL, "/pipelines/", job_id, "/dataset/", dataset_id)
          httr::POST(link_url)
        }, error = function(e) {
          list(status_code = 500, content = paste("Link error:", e$message))
        })
        
        if (is.list(link_response) && link_response$status_code == 500) {
          # Handle link error
          output$upload_dataset_output <- renderText({
            paste0(
              "⚠️ Dataset uploaded but linking failed!\n",
              "Job ID: ", job_id, "\n",
              "File: ", file_name, " (", file_size_mb, "MB)\n",
              "Upload Message: ", message, "\n",
              "Link Error: ", link_response$content, "\n",
              "\n🔗 Please try linking manually in the 'Link Dataset to Job' section."
            )
          })
        } else if (httr::status_code(link_response) == 200) {
          # Success - both upload and link worked
          output$upload_dataset_output <- renderText({
            paste0(
              "✅ Dataset uploaded and linked successfully!\n",
              "Job ID: ", job_id, "\n",
              "File: ", file_name, " (", file_size_mb, "MB)\n",
              "Format: ", if(input$is_coco_format_upload) "COCO Object Detection" else "Image Classification", "\n",
              "Upload Message: ", message, "\n",
              "\n🚀 Your dataset is now ready for training!"
            )
          })
        } else {
          # Link failed
          link_error <- httr::content(link_response, "text")
          output$upload_dataset_output <- renderText({
            paste0(
              "⚠️ Dataset uploaded but linking failed!\n",
              "Job ID: ", job_id, "\n",
              "File: ", file_name, " (", file_size_mb, "MB)\n",
              "Upload Message: ", message, "\n",
              "Link Error: ", httr::status_code(link_response), " - ", link_error, "\n",
              "\n🔗 Please try linking manually in the 'Link Dataset to Job' section."
            )
          })
        }
      } else {
        error_content <- httr::content(response, "text")
        output$upload_dataset_output <- renderText({
          paste("❌ Upload failed:", httr::status_code(response), "-", error_content)
        })
      }
    }, error = function(e) {
      if (grepl("timeout", e$message, ignore.case = TRUE)) {
        output$upload_dataset_output <- renderText({
          "⏱️ Upload timeout: The file is too large or the connection is slow."
        })
      } else {
        output$upload_dataset_output <- renderText({
          paste("❌ Upload error:", e$message)
        })
      }
    })
  })
  
  # Link Dataset
  observeEvent(input$link_dataset, {
    tryCatch({
      response <- httr2::request(paste0(CNN_API_URL, "/datasets/link")) |> 
        httr2::req_body_json(list(
          job_id = input$pending_job_dropdown,
          dataset_id = input$dataset_dropdown
        )) |>
        httr2::req_perform()
      
      result <- httr2::resp_body_json(response)
      output$link_output <- renderText({
        paste("Dataset linked successfully:", result$message)
      })
    }, error = function(e) {
      output$link_output <- renderText({
        paste("Dataset linking failed:", e$message)
      })
    })
  })
  
  # Start Training
  observeEvent(input$start_training_btn, {
    tryCatch({
      response <- httr2::request(paste0(CNN_API_URL, "/pipelines/", input$trainable_job_dropdown, "/train")) |> 
        httr2::req_body_json(list()) |>
        httr2::req_perform()
      
      result <- httr2::resp_body_json(response)
      output$training_output <- renderText({
        paste("Training started successfully:", result$message)
      })
    }, error = function(e) {
      output$training_output <- renderText({
        paste("Training start failed:", e$message)
      })
    })
  })
  
  # Display uploaded image when file is selected
  observeEvent(input$predict_image, {
    if (!is.null(input$predict_image)) {
      file_path <- input$predict_image$datapath
      file_name <- input$predict_image$name
      file_size <- file.info(file_path)$size
      
      if (file.exists(file_path)) {
        # Display the image
        output$prediction_image <- renderImage({
          list(src = file_path,
               contentType = 'image/jpeg',
               width = "100%",
               height = "auto",
               alt = "Uploaded image for prediction")
        }, deleteFile = FALSE)
        
        # Display image information
        output$image_info <- renderText({
          paste0("File: ", file_name, "\n",
                "Size: ", round(file_size / 1024, 2), " KB")
        })
      }
    }
  })
  
  # Make Prediction
  observeEvent(input$make_prediction, {
    if (is.null(input$predict_image)) {
      output$prediction_output <- renderText({
        "Please select an image for prediction"
      })
      return()
    }
    
    if (is.null(input$predict_job_dropdown) || input$predict_job_dropdown == "") {
      output$prediction_output <- renderText({
        "Please select a trained model first."
      })
      return()
    }
    
    file_path <- input$predict_image$datapath
    file_name <- input$predict_image$name
    
    if (!file.exists(file_path)) {
      output$prediction_output <- renderText({
        "Error: Selected image file does not exist."
      })
      return()
    }
    
    tryCatch({
      # Prepare the prediction request
      predict_url <- paste0(CNN_API_URL, "/predict/", input$predict_job_dropdown)
      
      # Try sending confidence threshold as URL parameter instead of form data
      predict_url_with_params <- paste0(predict_url, "?confidence_threshold=", input$confidence_threshold)
      
      # Upload image for prediction
      response <- httr::POST(
        predict_url_with_params,
        body = list(
          file = httr::upload_file(file_path, type = "image/jpeg"),
          confidence_threshold = as.numeric(input$confidence_threshold)
        ),
        encode = "multipart"
      )
      
      if (httr::status_code(response) == 200) {
        result <- httr::content(response, "parsed")
        
        # Format prediction results
        output_text <- paste0(
          "Prediction Results for: ", file_name, "\n",
          "Model: ", input$predict_job_dropdown, "\n",
          "Confidence Threshold: ", input$confidence_threshold, "\n\n"
        )
        
        # Determine task type and handle accordingly
        task_type <- result$task_type %||% "unknown"
        processing_time <- result$processing_time %||% 0
        
        if (grepl("image_classification", task_type, ignore.case = TRUE)) {
          # Handle Image Classification
          if (!is.null(result$predictions)) {
            predictions <- result$predictions
            if (is.list(predictions) && length(predictions) > 0) {
              output_text <- paste0(output_text, "Classification Results:\n")
              output_text <- paste0(output_text, sprintf("Processing Time: %.3fs\n\n", processing_time))
              
              for (i in seq_along(predictions)) {
                pred <- predictions[[i]]
                class_name <- pred$class_name %||% pred$class %||% pred$label %||% "Unknown"
                confidence <- pred$confidence %||% pred$score %||% pred$probability %||% 0
                
                # Check if confidence is already in percentage format (>1) or decimal format (0-1)
                if (confidence > 1) {
                  confidence_display <- confidence
                } else {
                  confidence_display <- confidence * 100
                }
                
                output_text <- paste0(
                  output_text,
                  sprintf("- %s: %.2f%% confidence\n", class_name, confidence_display)
                )
              }
            } else {
              output_text <- paste0(output_text, "No predictions above confidence threshold.")
            }
          } else {
            output_text <- paste0(output_text, "No prediction data returned.")
          }
          
        } else if (grepl("object_detection", task_type, ignore.case = TRUE)) {
          # Handle Object Detection
          # Handle annotated image if available
          if (!is.null(result$annotated_image) && result$annotated_image != "") {
            tryCatch({
              # Decode base64 image and save it temporarily
              img_data <- base64enc::base64decode(result$annotated_image)
              temp_file <- tempfile(fileext = ".jpg")
              writeBin(img_data, temp_file)
              
              # Display the annotated image
              output$prediction_image <- renderImage({
                list(src = temp_file,
                     contentType = 'image/jpeg',
                     width = "100%",
                     height = "auto",
                     alt = "Annotated image with detections")
              }, deleteFile = TRUE)
            }, error = function(e) {
              print("Error displaying annotated image:", e$message)
            })
          }
          
          if (!is.null(result$detections)) {
            detections <- result$detections
            num_detections <- length(detections)
            
            output_text <- paste0(output_text, "Object Detection Results:\n")
            output_text <- paste0(output_text, sprintf("Objects Found: %d\n", num_detections))
            output_text <- paste0(output_text, sprintf("Processing Time: %.3fs\n\n", processing_time))
            
            if (num_detections > 0) {
              output_text <- paste0(output_text, "Detected Objects:\n")
              
              # Group detections by class and show summary
              class_counts <- table(sapply(detections, function(d) d$class_name %||% d$class %||% "Unknown"))
              
              for (class_name in names(class_counts)) {
                count <- class_counts[[class_name]]
                # Get highest confidence for this class
                class_detections <- detections[sapply(detections, function(d) (d$class_name %||% d$class %||% "Unknown") == class_name)]
                max_conf <- max(sapply(class_detections, function(d) d$confidence %||% 0))
                
                if (max_conf > 1) {
                  conf_display <- max_conf
                } else {
                  conf_display <- max_conf * 100
                }
                
                output_text <- paste0(
                  output_text,
                  sprintf("• %s: %d object%s detected (max confidence: %.1f%%)\n", 
                         class_name, count, ifelse(count > 1, "s", ""), conf_display)
                )
              }
              
              output_text <- paste0(output_text, "\nSee the annotated image for bounding box locations.")
            } else {
              output_text <- paste0(output_text, "No objects detected above confidence threshold.")
            }
          } else {
            output_text <- paste0(output_text, "No detection data returned.")
          }
          
        } else {
          # Generic handling for unknown task types
          if (!is.null(result$predictions)) {
            predictions <- result$predictions
            if (is.list(predictions) && length(predictions) > 0) {
              output_text <- paste0(output_text, "Predictions:\n")
              
              for (i in seq_along(predictions)) {
                pred <- predictions[[i]]
                class_name <- pred$class_name %||% pred$class %||% pred$label %||% "Unknown"
                confidence <- pred$confidence %||% pred$score %||% pred$probability %||% 0
                
                if (confidence > 1) {
                  confidence_display <- confidence
                } else {
                  confidence_display <- confidence * 100
                }
                
                output_text <- paste0(
                  output_text,
                  sprintf("- %s: %.2f%% confidence\n", class_name, confidence_display)
                )
              }
            }
          } else if (!is.null(result$detections)) {
            detections <- result$detections
            output_text <- paste0(output_text, sprintf("Detections found: %d\n", length(detections)))
          } else {
            output_text <- paste0(output_text, "No prediction or detection data returned.")
          }
        }
        
        output$prediction_output <- renderText({
          output_text
        })
        
      } else {
        error_content <- httr::content(response, "text")
        output$prediction_output <- renderText({
          paste("Prediction failed:", httr::status_code(response), "-", error_content)
        })
      }
    }, error = function(e) {
      output$prediction_output <- renderText({
        paste("Prediction error:", e$message)
      })
    })
  })
  
  # Refresh Jobs List
  observeEvent(input$refresh_jobs, {
    tryCatch({
      response <- httr2::request(paste0(CNN_API_URL, "/pipelines")) |> 
        httr2::req_perform()
      
      jobs <- httr2::resp_body_json(response)
      
      # Convert to proper data frame format
      if (is.null(jobs) || length(jobs) == 0) {
        output$jobs_table <- DT::renderDataTable({
          DT::datatable(data.frame(Message = "No jobs found"), options = list(pageLength = 15))
        })
      } else if (is.list(jobs) && !is.data.frame(jobs)) {
        if (length(jobs) > 0) {
          # Extract key fields from each job
          jobs_data <- lapply(jobs, function(job) {
            list(
              ID = job$id %||% "",
              Name = job$pipeline_config$name %||% "",
              Task_Type = job$pipeline_config$task_type %||% "",
              Architecture = job$pipeline_config$architecture %||% "",
              Status = job$status %||% "",
              Created_At = job$created_at %||% "",
              Classes = job$pipeline_config$num_classes %||% 0,
              Epochs = job$pipeline_config$epochs %||% 0
            )
          })
          
          jobs_df <- do.call(rbind, lapply(jobs_data, data.frame, stringsAsFactors = FALSE))
          output$jobs_table <- DT::renderDataTable({
            DT::datatable(jobs_df, options = list(pageLength = 15))
          })
        } else {
          output$jobs_table <- DT::renderDataTable({
            DT::datatable(data.frame(Message = "No jobs found"), options = list(pageLength = 15))
          })
        }
      } else {
        output$jobs_table <- DT::renderDataTable({
          DT::datatable(jobs, options = list(pageLength = 15))
        })
      }
    }, error = function(e) {
      output$jobs_table <- DT::renderDataTable({
        DT::datatable(data.frame(Error = paste("Failed to load jobs:", e$message)))
      })
    })
  })
  
  # Get Job Details
  observeEvent(input$get_job_details, {
    if (input$job_status_id == "") {
      output$job_details_output <- renderText({
        "Please enter a job ID"
      })
      return()
    }
    
    tryCatch({
      response <- httr2::request(paste0(CNN_API_URL, "/pipelines/", input$job_status_id)) |> 
        httr2::req_perform()
      
      job_details <- httr2::resp_body_json(response)
      output$job_details_output <- renderText({
        paste("Job Details:", jsonlite::toJSON(job_details, pretty = TRUE))
      })
    }, error = function(e) {
      output$job_details_output <- renderText({
        paste("Error fetching job details:", e$message)
      })
    })
  })
  
  # Refresh Datasets
  observeEvent(input$refresh_datasets, {
    tryCatch({
      response <- httr::GET(paste0(CNN_API_URL, "/datasets/available"))
      
      if (httr::status_code(response) == 200) {
        datasets <- httr::content(response, "parsed")
        
        # Handle different response structures
        if (is.null(datasets) || length(datasets) == 0) {
          output$datasets_table <- DT::renderDataTable({
            DT::datatable(data.frame(Message = "No datasets found in the system. Upload a dataset first."))
          })
        } else if (!is.list(datasets)) {
          output$datasets_table <- DT::renderDataTable({
            DT::datatable(data.frame(Message = paste("Unexpected data format received:", class(datasets))))
          })
        } else {
          # Safely extract dataset information
          dataset_data <- lapply(datasets, function(dataset) {
            if (!is.list(dataset)) {
              return(list(ID = "N/A", Name = "N/A", Classes = 0, Task_Type = "Unknown", Items = 0, Format = "Unknown"))
            }
            
            id <- if (is.null(dataset$id)) "N/A" else as.character(dataset$id)
            name <- if (is.null(dataset$name)) id else as.character(dataset$name)
            classes <- if (is.null(dataset$classes)) 0 else length(dataset$classes)
            task_type <- if (is.null(dataset$task_type)) "Unknown" else as.character(dataset$task_type)
            items <- if (is.null(dataset$item_count)) 0 else as.numeric(dataset$item_count)
            format <- if (is.null(dataset$is_coco_format) || !dataset$is_coco_format) "Standard" else "COCO"
            
            return(list(ID = id, Name = name, Classes = classes, Task_Type = task_type, Items = items, Format = format))
          })
          
          # Convert to data frame
          datasets_df <- data.frame(
            ID = sapply(dataset_data, function(x) x$ID),
            Name = sapply(dataset_data, function(x) x$Name),
            Classes = sapply(dataset_data, function(x) x$Classes),
            Task_Type = sapply(dataset_data, function(x) x$Task_Type),
            Items = sapply(dataset_data, function(x) x$Items),
            Format = sapply(dataset_data, function(x) x$Format),
            stringsAsFactors = FALSE
          )
          
          output$datasets_table <- DT::renderDataTable({
            DT::datatable(datasets_df, options = list(pageLength = 15, scrollX = TRUE))
          })
        }
      } else {
        output$datasets_table <- DT::renderDataTable({
          DT::datatable(data.frame(Error = paste("API Error:", httr::status_code(response))))
        })
      }
    }, error = function(e) {
      output$datasets_table <- DT::renderDataTable({
        DT::datatable(data.frame(Error = paste("Connection Error:", e$message, "- Check if API server is running at", CNN_API_URL)))
      })
    })
  })
  
  # Delete Job
  observeEvent(input$delete_job_btn, {
    tryCatch({
      response <- httr2::request(paste0(CNN_API_URL, "/pipelines/", input$delete_job_dropdown)) |> 
        httr2::req_method("DELETE") |>
        httr2::req_perform()
      
      result <- httr2::resp_body_json(response)
      output$delete_output <- renderText({
        paste("Job deleted successfully:", result$message)
      })
    }, error = function(e) {
      output$delete_output <- renderText({
        paste("Job deletion failed:", e$message)
      })
    })
  })
  
  # Update dropdown lists dynamically
  update_dropdowns <- function() {
    tryCatch({
      # Update job dropdowns
      response <- httr2::request(paste0(CNN_API_URL, "/pipelines")) |> 
        httr2::req_perform()
      
      jobs <- httr2::resp_body_json(response)
      job_choices <- setNames(lapply(jobs, function(j) j$id), lapply(jobs, function(j) j$pipeline_config$name))
      
      updateSelectInput(session, "upload_job_dropdown", choices = job_choices)
      updateSelectInput(session, "pending_job_dropdown", choices = job_choices)
      updateSelectInput(session, "trainable_job_dropdown", choices = job_choices)
      updateSelectInput(session, "predict_job_dropdown", choices = job_choices)
      updateSelectInput(session, "delete_job_dropdown", choices = job_choices)
      
      # Update dataset dropdown
      datasets_response <- httr2::request(paste0(CNN_API_URL, "/datasets/available")) |> 
        httr2::req_perform()
      
      datasets <- httr2::resp_body_json(datasets_response)
      dataset_choices <- setNames(lapply(datasets, function(d) d$id), lapply(datasets, function(d) d$name))
      
      updateSelectInput(session, "dataset_dropdown", choices = dataset_choices)
    }, error = function(e) {
      print(paste("Failed to update dropdowns:", e$message))
    })
  }
  
  # Update architecture choices based on task type
  observe({
    if (input$task_type == "image_classification") {
      updateSelectInput(session, "architecture",
                       choices = list(
                         "ResNet-18" = "resnet18",
                         "ResNet-50" = "resnet50",
                         "VGG-16" = "vgg16",
                         "MobileNet" = "mobilenet",
                         "EfficientNet" = "efficientnet"
                       ),
                       selected = "resnet18")
    } else if (input$task_type == "object_detection") {
      updateSelectInput(session, "architecture",
                       choices = list(
                         "Faster R-CNN" = "faster_rcnn"
                       ),
                       selected = "faster_rcnn")
    }
  })
  
  # Initialize dropdowns on session start
  observe({
    update_dropdowns()
  })
  
  # Refresh dropdowns when refresh buttons are clicked
  observeEvent(input$refresh_upload_jobs, update_dropdowns())
  observeEvent(input$refresh_pending_jobs, update_dropdowns())
  observeEvent(input$refresh_trainable_jobs, update_dropdowns())
  observeEvent(input$refresh_prediction_models, update_dropdowns())
  observeEvent(input$refresh_delete_jobs, update_dropdowns())
  observeEvent(input$refresh_datasets_dropdown, update_dropdowns())
}
