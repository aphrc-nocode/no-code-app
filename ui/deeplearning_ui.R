deeplearning_ui = function() {
    tabItem(tabName = "cnntransformers",
		fluidRow(
			column(width=4,
				h4("Task Configuration"),
				selectInput("task_selector", "Select Task:",
					choices = c("Object Detection", "ASR")
				),

				# --- Object Detection Training UI ---
				shinyjs::hidden(
					div(
						id = "obj_panel",
						h5("Object Detection Training", style="font-weight:bold; margin-top:20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
						collapsible_panel("Paths & Naming", open = TRUE,
							textInput("obj_data_dir", "Data Directory", "/path/to/obj/dataset"),
							textInput("obj_model_checkpoint", "Model Checkpoint", "facebook/detr-resnet-50"),
							textInput("obj_run_name", "Run Name", "shiny-obj-run"),
							textInput("obj_version", "Version", "1.0.0")
						),
						collapsible_panel("Training Parameters",
							numericInput("obj_epochs", "Epochs", 5, min = 1),
							numericInput("obj_learning_rate", "Learning Rate", 5e-5, step = 1e-6),
							numericInput("obj_weight_decay", "Weight Decay", 1e-4, step = 1e-5),
							numericInput("obj_train_batch_size", "Train Batch Size", 8, min = 1),
							numericInput("obj_eval_batch_size", "Eval Batch Size", 8, min = 1),
							numericInput("obj_gradient_accumulation_steps", "Gradient Accumulation", 1, min = 1),
							numericInput("obj_max_image_size", "Max Image Size", 600, min = 128)
						),
						collapsible_panel("Saving & Early Stopping",
							numericInput("obj_early_stopping_patience", "Early Stopping Patience", 5),
							numericInput("obj_early_stopping_threshold", "Early Stopping Threshold", 0.0, step = 1e-4)
						),
						collapsible_panel("Execution & Reproducibility",
							numericInput("obj_seed", "Seed", 42),
							numericInput("obj_num_proc", "Number of Processes", 4, min = 0)
						),
						collapsible_panel("Hub & Logging",
							checkboxInput("obj_push_to_hub", "Push to Hub", FALSE),
							checkboxInput("obj_log_to_wandb", "Log to W&B", FALSE),
							textInput("obj_wandb_project", "W&B Project", ""),
							textInput("obj_wandb_entity", "W&B Entity", "")
						),
						actionButton("start_obj_job", "Start Object Detection Job", class = "btn-primary", style="margin-top: 15px; width: 100%;")
					)
				),

				# --- ASR Training UI ---
				shinyjs::hidden(
					 div(
						  id = "asr_panel",
						  h5("ASR Training", style="font-weight:bold; margin-top:20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
						  collapsible_panel("Paths & Naming", open = TRUE,
								textInput("asr_data_dir", "Data Directory", "/path/to/asr/dataset"),
								selectInput("asr_model_arch", "Model Architecture",
												choices = c("Whisper", "XLS-R", "Wav2Vec2-BERT")),
								selectInput("asr_model_checkpoint", "Model Checkpoint", choices = NULL),
								textInput("asr_run_name", "Run Name", "shiny-asr-run"),
								textInput("asr_version", "Version", "1.0.0")
						  ),
						  collapsible_panel("Dataset & Language",
								textInput("asr_language", "Language", "english"),
								textInput("asr_language_code", "Language Code", "en"),
								textInput("asr_speaker_id_column", "Speaker ID Column", "speaker_id"),
								textInput("asr_text_column", "Text Column", "sentence")
						  ),
						  collapsible_panel("Preprocessing & Filtering",
								numericInput("asr_target_sampling_rate", "Target Sampling Rate", 16000),
								numericInput("asr_min_duration_s", "Min Duration (s)", 1.0),
								numericInput("asr_max_duration_s", "Max Duration (s)", 30.0),
								numericInput("asr_min_transcript_len", "Min Transcript Length", 10),
								numericInput("asr_max_transcript_len", "Max Transcript Length", 300),
								checkboxInput("asr_apply_outlier_filtering", "Apply Outlier Filtering", FALSE),
								numericInput("asr_outlier_std_devs", "Outlier Std Devs", 2.0)
						  ),
						  collapsible_panel("Data Splitting",
								checkboxInput("asr_is_presplit", "Is Presplit?", TRUE),
								checkboxInput("asr_speaker_disjointness", "Speaker Disjoint Split", TRUE),
								numericInput("asr_train_ratio", "Train Ratio", 0.8, min = 0, max = 1),
								numericInput("asr_dev_ratio", "Dev Ratio", 0.1, min = 0, max = 1)
						  ),
						  collapsible_panel("Training Parameters",
								numericInput("asr_epochs", "Epochs", 5, min = 1),
								numericInput("asr_learning_rate", "Learning Rate", 3e-4, step = 1e-5),
								selectInput("asr_lr_scheduler_type", "LR Scheduler", choices = c("linear", "cosine", "constant")),
								numericInput("asr_warmup_ratio", "Warmup Ratio", 0.1),
								numericInput("asr_train_batch_size", "Train Batch Size", 16),
								numericInput("asr_eval_batch_size", "Eval Batch Size", 16),
								numericInput("asr_gradient_accumulation_steps", "Gradient Accumulation", 1),
								selectInput("asr_optimizer", "Optimizer", choices = c("adamw_torch", "adamw_hf", "adafactor"))
						  ),
						  collapsible_panel("Saving & Early Stopping",
								numericInput("asr_early_stopping_patience", "Early Stopping Patience", 5),
								numericInput("asr_early_stopping_threshold", "Early Stopping Threshold", 1e-3)
						  ),
						  collapsible_panel("Hub & Logging",
								checkboxInput("asr_push_to_hub", "Push to Hub", FALSE),
								textInput("asr_hub_model_id", "Hub Model ID", ""),
								checkboxInput("asr_hub_private_repo", "Private Hub Repo", TRUE),
								checkboxInput("asr_log_to_wandb", "Log to W&B", FALSE),
								textInput("asr_wandb_project", "W&B Project", ""),
								textInput("asr_wandb_entity", "W&B Entity", "")
						  ),
						  collapsible_panel("Execution & Advanced",
								numericInput("asr_seed", "Seed", 42),
								numericInput("asr_num_proc", "Number of Processes", 4)
						  ),
						  actionButton("start_asr_job", "Start ASR Job", class = "btn-success", style="margin-top: 15px; width: 100%;")
					 )
				)
			),

			column(width=8,
				tabsetPanel(
					id = "main_tabs",
					type = "tabs",
					tabPanel("Training Status",
						h4("Job Status", style="margin-top:20px;"),
						wellPanel(
							strong("Task:"), textOutput("job_task_display", inline = TRUE), br(),
							strong("Job ID:"), textOutput("job_id_display", inline = TRUE), br(),
							strong("Status:"), textOutput("job_status_display", inline = TRUE)
						),
						h4("Training Progress"),
						div(
							style = "background-color: #e9ecef; border-radius: .25rem; height: 20px; margin-bottom: 1rem;",
							div(id = "progress-bar", style = "background-color: #007bff; height: 100%; width: 0%; border-radius: .25rem; transition: width .6s ease;")
						),
						textOutput("progress_text"),
						hr(),
						h4("Evaluation Results"),
						DTOutput("eval_table"),
						hr(),
						tags$details(
							style = "margin-top: 15px;",
							tags$summary("View Full Log", style = "cursor: pointer; font-weight: bold;"),
							div(
							  style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 10px; margin-top: 5px; max-height: 400px; overflow-y: auto; font-family: monospace; white-space: pre-wrap;",
							  verbatimTextOutput("log_output")
							)
						)
					),

					tabPanel("Inference",
						conditionalPanel(
							condition = "input.task_selector == 'Object Detection'",
							h4("Object Detection Inference", style="margin-top:20px;"),
							wellPanel(
								 textInput("infer_run_name", "Enter Run Name to Find Checkpoints", ""),
								 selectInput("infer_checkpoint_dropdown", "Select Checkpoint", choices = NULL),
								 fileInput("infer_obj_image_upload", "Upload Image for Detection", accept = c('image/png', 'image/jpeg', 'image/jpg')),
								 sliderInput("infer_obj_threshold", "Confidence Threshold", min = 0.1, max = 1.0, value = 0.5, step = 0.05),
								 actionButton("start_obj_inference", "Run Inference", class = "btn-info", style="margin-top: 10px;")
							),
							hr(),
							h5("Inference Result"),
							uiOutput("inference_status_ui"),
							imageOutput("inference_image_output", height = "auto")
						),
						conditionalPanel(
							condition = "input.task_selector == 'ASR'",
							h4("ASR Inference", style="margin-top:20px;"),
							wellPanel(
								 textInput("infer_asr_run_name", "Enter Run Name to Find Checkpoints", ""),
								 selectInput("infer_asr_checkpoint_dropdown", "Select Checkpoint", choices = NULL),
								 fileInput("infer_asr_audio_upload", "Upload Audio File", accept = c('audio/wav', 'audio/mp3', 'audio/flac')),
								 actionButton("start_asr_inference", "Run Inference", class = "btn-info", style="margin-top: 10px;")
							),
							hr(),
							h5("Transcription Result"),
							uiOutput("asr_inference_status_ui"),
							div(
							  style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin-top: 5px; min-height: 100px; font-size: 1.1em;",
							  textOutput("asr_transcription_output")
							)
						)
					)
				)
			)
		))
	
  
# #CNN
#     tabItem(tabName = "dashboard",
#             fluidRow(
#               box(
#                 title = "API Status", status = "primary", solidHeader = TRUE, width = 6,
#                 actionButton("check_status", "Check API Status", class = "btn-primary"),
#                 br(), br(),
#                 verbatimTextOutput("api_status")
#               ),
#               box(
#                 title = "MLflow Server", status = "info", solidHeader = TRUE, width = 6,
#                 actionButton("start_mlflow", "Start MLflow Server", class = "btn-info"),
#                 br(), br(),
#                 verbatimTextOutput("mlflow_output")
#               )
#             ),
#             fluidRow(
#               box(
#                 title = "All Jobs Overview", status = "success", solidHeader = TRUE, width = 12,
#                 actionButton("refresh_dashboard_jobs", "Refresh Jobs List", class = "btn-success"),
#                 br(), br(),
#                 DT::dataTableOutput("dashboard_jobs_table")
#               )
#             ),
#             fluidRow(
#               box(
#                 title = "Quick Info", status = "warning", solidHeader = TRUE, width = 12,
#                 h4("Welcome to the No-Code AI Platform"),
#                 p("This R Shiny interface provides full functionality for the FastAPI backend."),
#                 p("Available features:"),
#                 tags$ul(
#                   tags$li("Dashboard: Check API status and view all jobs"),
#                   tags$li("Create Pipeline: Set up new ML training pipelines"),
#                   tags$li("Train Model: Upload datasets and start training"),
#                   tags$li("Make Predictions: Use trained models for inference"),
#                   tags$li("View Jobs: Monitor all training jobs"),
#                   tags$li("View Datasets: Browse available datasets"),
#                   tags$li("Delete Job: Remove unwanted jobs")
#                 ),
#                 div(class = "success-box",
#                     strong("Ready: "), 
#                     "Full functionality available with proper HTTP requests using the 'httr' package. ",
#                     "All features including file uploads, training, and predictions are supported."
#                 )
#               )
#             )
#     )
#     
#     # Create Pipeline Tab
#     tabItem(tabName = "create",
#             fluidRow(
#               box(
#                 title = "Create New Pipeline", status = "primary", solidHeader = TRUE, width = 12,
#                 fluidRow(
#                   column(6,
#                          textInput("pipeline_name", "Pipeline Name", value = "My Image Classifier"),
#                          selectInput("task_type", "Task Type", 
#                                      choices = list("Image Classification" = "image_classification",
#                                                     "Object Detection" = "object_detection"),
#                                      selected = "image_classification"),
#                          selectInput("architecture", "Model Architecture",
#                                      choices = list("ResNet-18" = "resnet18",
#                                                     "ResNet-50" = "resnet50",
#                                                     "VGG-16" = "vgg16",
#                                                     "MobileNet" = "mobilenet",
#                                                     "EfficientNet" = "efficientnet"),
#                                      selected = "resnet18"),
#                          numericInput("num_classes", "Number of Classes", value = 2, min = 2, max = 1000)
#                   ),
#                   column(6,
#                          numericInput("batch_size", "Batch Size", value = 8, min = 1, max = 128),
#                          numericInput("epochs", "Epochs", value = 5, min = 1, max = 1000),
#                          numericInput("learning_rate", "Learning Rate", value = 0.001, min = 0.0001, max = 1, step = 0.0001),
#                          textInput("image_size", "Image Size (width, height)", value = "224, 224")
#                   )
#                 ),
#                 fluidRow(
#                   column(6,
#                          checkboxInput("augmentation", "Enable Data Augmentation", value = TRUE)
#                   ),
#                   column(6,
#                          checkboxInput("early_stopping", "Enable Early Stopping", value = TRUE)
#                   )
#                 ),
#                 br(),
#                 actionButton("create_pipeline", "Create Pipeline", class = "btn-primary btn-lg"),
#                 br(), br(),
#                 verbatimTextOutput("create_output")
#               )
#             )
#     )
#     
#     # Train Model Tab
#     tabItem(tabName = "train",
#             fluidRow(
#               box(
#                 title = "Current Job Status", status = "info", solidHeader = TRUE, width = 12,
#                 p("Shows the most recently created job ready for training"),
#                 actionButton("refresh_current_job", "Refresh Current Job", class = "btn-info"),
#                 br(), br(),
#                 verbatimTextOutput("current_job_status")
#               )
#             ),
#             fluidRow(
#               box(
#                 title = "Upload Dataset to Job", status = "success", solidHeader = TRUE, width = 12,
#                 div(class = "success-box",
#                     strong("File Upload Ready: "),
#                     "Upload dataset files directly to a specific job. Maximum file size: 500MB. ",
#                     "Select a job first, then upload your dataset ZIP file."
#                 ),
#                 fluidRow(
#                   column(6,
#                          h4("Job Selection"),
#                          selectInput("upload_job_dropdown", "Select Job for Dataset Upload", choices = list()),
#                          actionButton("refresh_upload_jobs", "Refresh Jobs", class = "btn-info"),
#                          br(), br(),
#                          checkboxInput("is_coco_format_upload", "COCO Format Dataset (Object Detection)", value = FALSE)
#                   ),
#                   column(6,
#                          h4("File Upload"),
#                          fileInput("dataset_file", "Choose Dataset ZIP File",
#                                    accept = c(".zip"),
#                                    multiple = FALSE),
#                          p("Supported formats (Max 500MB):"),
#                          tags$ul(
#                            tags$li("ZIP files with image folders"),
#                            tags$li("For Classification: folders with class subfolders"),
#                            tags$li("For Object Detection: COCO format structure")
#                          )
#                   )
#                 ),
#                 br(),
#                 actionButton("upload_dataset", "Upload Dataset to Job", class = "btn-success btn-lg"),
#                 br(), br(),
#                 verbatimTextOutput("upload_dataset_output")
#               )
#             ),
#             fluidRow(
#               box(
#                 title = "Link Dataset to Job", status = "primary", solidHeader = TRUE, width = 12,
#                 p("Connect a pending job to a dataset (either newly uploaded or existing)"),
#                 fluidRow(
#                   column(6,
#                          selectInput("pending_job_dropdown", "Select Pending Job", choices = list()),
#                          actionButton("refresh_pending_jobs", "Refresh Pending Jobs", class = "btn-info")
#                   ),
#                   column(6,
#                          selectInput("dataset_dropdown", "Select Dataset", choices = list()),
#                          actionButton("refresh_datasets_dropdown", "Refresh Datasets", class = "btn-success")
#                   )
#                 ),
#                 actionButton("link_dataset", "Link Dataset to Job", class = "btn-primary"),
#                 br(), br(),
#                 verbatimTextOutput("link_output")
#               )
#             ),
#             fluidRow(
#               box(
#                 title = "Start Training", status = "warning", solidHeader = TRUE, width = 12,
#                 p("Start training jobs that have datasets linked"),
#                 selectInput("trainable_job_dropdown", "Select Job Ready for Training", choices = list()),
#                 actionButton("refresh_trainable_jobs", "Refresh Trainable Jobs", class = "btn-info"),
#                 br(), br(),
#                 actionButton("start_training_btn", "Start Training", class = "btn-warning btn-lg"),
#                 br(), br(),
#                 verbatimTextOutput("training_output")
#               )
#             )
#     )
#     
#     # Make Predictions Tab
#     tabItem(tabName = "predict",
#             fluidRow(
#               box(
#                 title = "Model Selection", status = "primary", solidHeader = TRUE, width = 12,
#                 selectInput("predict_job_dropdown", "Select Trained Model", choices = list()),
#                 actionButton("refresh_prediction_models", "Refresh Available Models", class = "btn-info"),
#                 br(), br(),
#                 verbatimTextOutput("prediction_models_status")
#               )
#             ),
#             fluidRow(
#               box(
#                 title = "Image Upload & Prediction", status = "success", solidHeader = TRUE, width = 12,
#                 fluidRow(
#                   column(6,
#                          h4("Upload Image"),
#                          fileInput("prediction_image", "Choose Image File",
#                                    accept = c(".jpg", ".jpeg", ".png", ".bmp", ".tiff"),
#                                    multiple = FALSE),
#                          p("Supported formats: JPG, PNG, BMP, TIFF")
#                   ),
#                   column(6,
#                          h4("Prediction Settings"),
#                          sliderInput("confidence_threshold", 
#                                      "Confidence Threshold", 
#                                      value = 0.5, min = 0.1, max = 0.95, step = 0.05,
#                                      post = "%"),
#                          p(class = "help-text", style = "font-size: 12px; color: #666;",
#                            "Higher values show fewer, more confident detections. Lower values show more detections but may include false positives."),
#                          checkboxInput("show_probabilities", "Show All Class Probabilities", value = TRUE)
#                   )
#                 ),
#                 br(),
#                 actionButton("make_prediction", "Make Prediction", class = "btn-primary btn-lg"),
#                 br(), br(),
#                 fluidRow(
#                   column(6,
#                          h4("Prediction Results"),
#                          verbatimTextOutput("prediction_output")
#                   ),
#                   column(6,
#                          h4("Uploaded Image"),
#                          imageOutput("prediction_image_display", height = "400px"),
#                          br(),
#                          textOutput("image_info")
#                   )
#                 )
#               )
#             )
#     )
#     
#     # Jobs Tab
#     tabItem(tabName = "jobs",
#             fluidRow(
#               box(
#                 title = "All Jobs", status = "info", solidHeader = TRUE, width = 12,
#                 actionButton("refresh_jobs", "Refresh Jobs List", class = "btn-info"),
#                 br(), br(),
#                 DT::dataTableOutput("jobs_table")
#               )
#             ),
#             fluidRow(
#               box(
#                 title = "Job Details", status = "success", solidHeader = TRUE, width = 12,
#                 textInput("job_status_id", "Job ID", placeholder = "Enter Job ID to view details"),
#                 actionButton("get_job_details", "Get Job Status", class = "btn-success"),
#                 br(), br(),
#                 verbatimTextOutput("job_details_output")
#               )
#             )
#     )
#     
#     # Datasets Tab
#     tabItem(tabName = "datasets",
#             fluidRow(
#               box(
#                 title = "Available Datasets", status = "success", solidHeader = TRUE, width = 12,
#                 actionButton("refresh_datasets", "Refresh Datasets", class = "btn-success"),
#                 br(), br(),
#                 DT::dataTableOutput("datasets_table")
#               )
#             )
#     )
#     
#     # Delete Job Tab
#     tabItem(tabName = "delete",
#             fluidRow(
#               box(
#                 title = "Delete Job", status = "danger", solidHeader = TRUE, width = 12,
#                 div(class = "warning-box",
#                     strong("Warning: "),
#                     "Deleting a job will permanently remove all associated data including trained models, datasets, and logs. This action cannot be undone."
#                 ),
#                 selectInput("delete_job_dropdown", "Select Job to Delete", choices = list()),
#                 actionButton("refresh_delete_jobs", "Refresh Jobs List", class = "btn-info"),
#                 br(), br(),
#                 actionButton("delete_job_btn", "Delete Selected Job", class = "btn-danger btn-lg"),
#                 br(), br(),
#                 verbatimTextOutput("delete_output")
#               )
#             )
#     )
#   
  
  
  
  
  
  
}
