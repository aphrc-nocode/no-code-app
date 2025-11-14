# ui.R

# Helper function to create a collapsible section
collapsible_panel <- function(title, ..., open = FALSE) {
  tags$details(
    open = if (open) NA else NULL,
    tags$summary(title, style = "font-weight: bold; cursor: pointer; margin-top: 15px;"),
    div(style = "padding: 10px; border: 1px solid #ddd; border-radius: 5px; margin-top: 5px;", ...
    )
  )
}


deeplearning_ui = function() {
    tabItem(tabName = "cnndeep",
		fluidRow(
			useShinyjs(),
			
			sidebarLayout(
				sidebarPanel(
					width = 4,
					h4("Task Configuration"),
					selectInput("task_selector", "Select Task:",
								choices = c("Object Detection" = "object_detection", 
											"ASR" = "asr", 
											"Image Classification" = "image_classification",
											"Image Segmentation" = "image_segmentation")
					),
					
					# --- Object Detection Training UI ---
					shinyjs::hidden(
						div(
							id = "obj_panel",
							h5("Object Detection Training", style="font-weight:bold; margin-top:20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
							collapsible_panel("Paths & Naming", open = TRUE,
								selectInput("obj_dataset_id", "Select Dataset", choices = c("Loading..." = "")),
								selectInput("obj_model_arch", "Select Architecture", choices = c("Loading..." = "")),
								selectInput("obj_model_checkpoint", "Select Checkpoint", choices = NULL),
								textInput("obj_run_name", "Run Name", "shiny-obj-run"),
								textInput("obj_version", "Version", "1.0.0")
							),
							# --- Training Parameters (Conditional) ---
							# These are for Transformers-based models
							conditionalPanel(
								condition = "input.obj_model_arch != 'YOLO'",
								collapsible_panel("Training Parameters (Transformers)", open = FALSE,
									numericInput("obj_learning_rate", "Learning Rate", 5e-5, step = 1e-6),
									numericInput("obj_weight_decay", "Weight Decay", 1e-4, step = 1e-5),
									numericInput("obj_gradient_accumulation_steps", "Gradient Accumulation", 1, min = 1),
									numericInput("obj_max_grad_norm", "Max Gradient Norm", 1.0, min = 0.1, step = 0.1)
								)
							),
							# --- NEW: YOLO-specific Parameters ---
							conditionalPanel(
								condition = "input.obj_model_arch == 'YOLO'",
								collapsible_panel("Training Parameters (YOLO)", open = FALSE,
									# --- ADDED NEW INPUTS ---
									numericInput("obj_yolo_warmup_epochs", "Warmup Epochs", 3.0, min = 0, step = 0.1),
									numericInput("obj_yolo_lr0", "Initial Learning Rate (lr0)", 0.01, min = 0, step = 0.001),
									numericInput("obj_yolo_momentum", "Momentum", 0.937, min = 0, step = 0.001),
									selectInput("obj_yolo_optimizer", "Optimizer", choices = c("auto", "SGD", "Adam", "AdamW")),
									numericInput("obj_yolo_weight_decay", "Weight Decay", 0.0005, min = 0, step = 1e-5)
								)
							),
							# --- Common Training Parameters ---
							collapsible_panel("Common Training Parameters", open = FALSE,
								numericInput("obj_epochs", "Epochs", 5, min = 1),
								numericInput("obj_train_batch_size", "Train Batch Size", 8, min = 1),
								numericInput("obj_eval_batch_size", "Eval Batch Size", 8, min = 1),
								numericInput("obj_max_image_size", "Max Image Size", 600, min = 128)
							),
							collapsible_panel("Execution & Reproducibility", open = FALSE,
								numericInput("obj_seed", "Seed", 42),
								numericInput("obj_num_proc", "Number of Processes", 4, min = 0),
								checkboxInput("obj_force_preprocess", "Force Data Pre-processing", value = FALSE),
								conditionalPanel(
									condition = "input.obj_model_arch != 'YOLO'",
									checkboxInput("obj_gradient_checkpointing", "Enable Gradient Checkpointing (Saves Memory)", value = FALSE),
									checkboxInput("obj_fp16", "Use FP16 Precision (Unstable)", value = TRUE)
								)
							),
							collapsible_panel("Saving & Early Stopping", open = FALSE,
								numericInput("obj_early_stopping_patience", "Early Stopping Patience", 5),
								conditionalPanel(
									condition = "input.obj_model_arch != 'YOLO'",
									numericInput("obj_early_stopping_threshold", "Early Stopping Threshold", 0.0, step = 1e-4)
								)
							),
							collapsible_panel("Hub & Logging", open = FALSE,
								checkboxInput("obj_push_to_hub", "Push to Hub", FALSE),
								conditionalPanel(
									condition = "input.obj_push_to_hub == true",
									textInput("obj_hub_user_id", "Hub User/Org Name", "")
								),
								checkboxInput("obj_log_to_wandb", "Log to W&B", FALSE),
								conditionalPanel(
									condition = "input.obj_log_to_wandb == true",
									textInput("obj_wandb_project", "W&B Project", ""),
									textInput("obj_wandb_entity", "W&B Entity", "")
								)
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
								selectInput("asr_dataset_id", "Select Dataset", choices = c("Loading..." = "")),
								selectInput("asr_model_arch", "Select Architecture", choices = c("Loading..." = "")),
								selectInput("asr_model_checkpoint", "Select Checkpoint", choices = NULL),
								textInput("asr_run_name", "Run Name", "shiny-asr-run"),
								textInput("asr_version", "Version", "1.0.0")
							),
							collapsible_panel("Data Splitting", open = FALSE,
								checkboxInput("asr_is_presplit", "Is Data Pre-Split?", TRUE),
								conditionalPanel(
									condition = "input.asr_is_presplit == false",
									checkboxInput("asr_speaker_disjointness", "Ensure Speaker Disjoint Split (if not pre-split)", FALSE),
									numericInput("asr_train_ratio", "Train Ratio", 0.8, min = 0, max = 1),
									numericInput("asr_dev_ratio", "Dev Ratio", 0.1, min = 0, max = 1),
									numericInput("asr_test_ratio", "Test Ratio", 0.1, min = 0, max = 1)
								)
							),
							collapsible_panel("Dataset & Language", open = FALSE,
								conditionalPanel(
									condition = "input.asr_model_arch == 'Whisper'",
									textInput("asr_language", "Language (for Whisper)", "english"),
									textInput("asr_language_code", "Language Code (for Whisper)", "en")
								),
								textInput("asr_speaker_id_column", "Speaker ID Column (for disjoint split)", ""),
								textInput("asr_text_column", "Text/Transcript Column", "sentence")
							),
							collapsible_panel("Preprocessing & Filtering", open = FALSE,
								numericInput("asr_target_sampling_rate", "Target Sampling Rate", 16000),
								numericInput("asr_min_duration_s", "Min Duration (s)", 1.0),
								numericInput("asr_max_duration_s", "Max Duration (s)", 30.0),
								numericInput("asr_min_transcript_len", "Min Transcript Length", 10),
								numericInput("asr_max_transcript_len", "Max Transcript Length", 300),
								checkboxInput("asr_apply_outlier_filtering", "Apply Outlier Filtering", TRUE),
								conditionalPanel(
									condition = "input.asr_apply_outlier_filtering == true",
									numericInput("asr_outlier_std_devs", "Outlier Std Devs", 2.0)
								)
							),
							collapsible_panel("Training Parameters", open = FALSE,
								numericInput("asr_max_train_hours", "Max Train Hours (Optional)", value = NA, min = 0),
								numericInput("asr_epochs", "Epochs", 5, min = 1),
								numericInput("asr_learning_rate", "Learning Rate", 3e-4, step = 1e-5),
								selectInput("asr_lr_scheduler_type", "LR Scheduler", choices = c("linear", "cosine", "constant")),
								numericInput("asr_warmup_ratio", "Warmup Ratio", 0.1),
								numericInput("asr_train_batch_size", "Train Batch Size", 16),
								numericInput("asr_eval_batch_size", "Eval Batch Size", 16),
								numericInput("asr_gradient_accumulation_steps", "Gradient Accumulation", 1),
								selectInput("asr_optimizer", "Optimizer", choices = c("adamw_torch", "adamw_hf", "adafactor"))
							),
							collapsible_panel("Execution & Reproducibility", open = FALSE,
								numericInput("asr_seed", "Seed", 42),
								numericInput("asr_num_proc", "Number of Processes", 8),
								checkboxInput("asr_gradient_checkpointing", "Enable Gradient Checkpointing", value = FALSE)
							),
							collapsible_panel("Saving & Early Stopping", open = FALSE,
								numericInput("asr_early_stopping_patience", "Early Stopping Patience", 5),
								numericInput("asr_early_stopping_threshold", "Early Stopping Threshold", 1e-3)
							),
							collapsible_panel("Hub & Logging", open = FALSE,
								checkboxInput("asr_push_to_hub", "Push to Hub", FALSE),
								conditionalPanel(
									condition = "input.asr_push_to_hub == true",
									textInput("asr_hub_user_id", "Hub User/Org Name", ""),
									checkboxInput("asr_hub_private_repo", "Private Hub Repo", FALSE)
								),
								checkboxInput("asr_log_to_wandb", "Log to W&B", FALSE),
								conditionalPanel(
									condition = "input.asr_log_to_wandb == true",
									textInput("asr_wandb_project", "W&B Project", ""),
									textInput("asr_wandb_entity", "W&B Entity", "")
								)
							),
							actionButton("start_asr_job", "Start ASR Job", class = "btn-success", style="margin-top: 15px; width: 100%;")
						)
					),

					# -- Image Classification UI --
					shinyjs::hidden(
						div(
							id = "img_class_panel",
							h5("Image Classification Training", style="font-weight:bold; margin-top:20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
							collapsible_panel("Paths & Naming", open = TRUE,
								selectInput("img_class_dataset_id", "Select Dataset", choices = c("Loading..." = "")),
								selectInput("img_class_model_arch", "Select Architecture", choices = c("Loading..." = "")),
								selectInput("img_class_model_checkpoint", "Select Checkpoint", choices = NULL),
								textInput("img_class_run_name", "Run Name", "shiny-img-class-run"),
								textInput("img_class_version", "Version", "1.0.0")
							),
							collapsible_panel("Data Splitting", open = FALSE,
								checkboxInput("img_class_is_presplit", "Is Data Pre-Split?", value = TRUE),
								conditionalPanel(
									condition = "input.img_class_is_presplit == false",
									numericInput("img_class_train_ratio", "Train Ratio", 0.8, min = 0, max = 1),
									numericInput("img_class_dev_ratio", "Dev Ratio", 0.1, min = 0, max = 1)
								)
							),
							collapsible_panel("Training Parameters", open = FALSE,
								numericInput("img_class_epochs", "Epochs", 5, min = 1),
								numericInput("img_class_learning_rate", "Learning Rate", 2e-5, step = 1e-6),
								numericInput("img_class_weight_decay", "Weight Decay", 0.01, step = 1e-3),
								numericInput("img_class_train_batch_size", "Train Batch Size", 32, min = 1),
								numericInput("img_class_eval_batch_size", "Eval Batch Size", 32, min = 1),
								numericInput("img_class_max_image_size", "Max Image Size", 224, min = 64),
								numericInput("img_class_grad_accum", "Gradient Accumulation", 1, min = 1)
							),
							collapsible_panel("Execution & Reproducibility", open = FALSE,
								numericInput("img_class_seed", "Seed", 42),
								numericInput("img_class_num_proc", "Number of Processes", 4, min = 0),
								checkboxInput("img_class_grad_check", "Enable Gradient Checkpointing", value = FALSE),
								checkboxInput("img_class_fp16", "Use FP16 Precision", value = TRUE)
							),
							collapsible_panel("Saving & Early Stopping", open = FALSE,
								numericInput("img_class_early_stop", "Early Stopping Patience", 3)
							),
							collapsible_panel("Hub & Logging", open = FALSE,
								checkboxInput("img_class_push_to_hub", "Push to Hub", FALSE),
								conditionalPanel(
									condition = "input.img_class_push_to_hub == true",
									textInput("img_class_hub_user_id", "Hub User/Org Name", "")
								),
								checkboxInput("img_class_log_to_wandb", "Log to W&B", FALSE),
								conditionalPanel(
									condition = "input.img_class_log_to_wandb == true",
									textInput("img_class_wandb_project", "W&B Project", ""),
									textInput("img_class_wandb_entity", "W&B Entity", "")
								)
							),
							actionButton("start_img_class_job", "Start Image Classification Job", class = "btn-warning", style="margin-top: 15px; width: 100%;")
						)
					),
					
					# --- Image Segmentation Training UI ---
					shinyjs::hidden(
						div(
							id = "seg_panel",
							h5("Image Segmentation Training", style="font-weight:bold; margin-top:20px; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
							collapsible_panel("Paths & Naming", open = TRUE,
								selectInput("seg_dataset_id", "Select Dataset", choices = c("Loading..." = "")),
								selectInput("seg_model_arch", "Select Architecture", choices = c("Loading..." = "")),
								selectInput("seg_model_checkpoint", "Select Checkpoint", choices = NULL),
								textInput("seg_run_name", "Run Name", "shiny-seg-run"),
								textInput("seg_version", "Version", "1.0.0")
							),
							collapsible_panel("Data Splitting", open = FALSE,
								checkboxInput("seg_is_presplit", "Is Data Pre-Split?", value = TRUE),
								conditionalPanel(
									condition = "input.seg_is_presplit == false",
									numericInput("seg_train_ratio", "Train Ratio", 0.8, min = 0, max = 1),
									numericInput("seg_dev_ratio", "Dev Ratio", 0.1, min = 0, max = 1)
								)
							),
							collapsible_panel("Training Parameters", open = FALSE,
								numericInput("seg_epochs", "Epochs", 10, min = 1),
								numericInput("seg_learning_rate", "Learning Rate", 6e-5, step = 1e-6),
								numericInput("seg_weight_decay", "Weight Decay", 0.01, step = 1e-3),
								numericInput("seg_train_batch_size", "Train Batch Size", 8, min = 1),
								numericInput("seg_eval_batch_size", "Eval Batch Size", 8, min = 1),
								numericInput("seg_max_image_size", "Image Size", 512, min = 64),
								numericInput("seg_grad_accum", "Gradient Accumulation", 1, min = 1)
							),
							collapsible_panel("Execution & Reproducibility", open = FALSE,
								numericInput("seg_seed", "Seed", 42),
								numericInput("seg_num_proc", "Number of Processes", 4, min = 0),
								checkboxInput("seg_grad_check", "Enable Gradient Checkpointing", value = FALSE),
								checkboxInput("seg_fp16", "Use FP16 Precision", value = TRUE)
							),
							collapsible_panel("Saving & Early Stopping", open = FALSE,
								numericInput("seg_early_stop", "Early Stopping Patience", 5)
							),
							collapsible_panel("Hub & Logging", open = FALSE,
								checkboxInput("seg_push_to_hub", "Push to Hub", FALSE),
								conditionalPanel(
									condition = "input.seg_push_to_hub == true",
									textInput("seg_hub_user_id", "Hub User/Org Name", "")
								),
								checkboxInput("seg_log_to_wandb", "Log to W&B", FALSE),
								conditionalPanel(
									condition = "input.seg_log_to_wandb == true",
									textInput("seg_wandb_project", "W&B Project", ""),
									textInput("seg_wandb_entity", "W&B Entity", "")
								)
							),
							actionButton("start_seg_job", "Start Image Segmentation Job", class = "btn-info", style="margin-top: 15px; width: 100%;")
						)
					)
				),
				
				mainPanel(
					width = 8,
					tabsetPanel(
						id = "main_tabs",
						type = "tabs",
						
						tabPanel("Data Management",
							h4("Register a New Dataset", style="margin-top:20px;"),
							sidebarLayout(
								sidebarPanel(
									width = 4,
									textInput("new_data_name", "Dataset Name (e.g., 'my-coco-dataset')"),
									selectInput("new_data_task_type", "Task Type",
												choices = c("Object Detection" = "object_detection",
															"ASR" = "asr",
															"Image Classification" = "image_classification",
															"Image Segmentation" = "image_segmentation")
									),
									fileInput("new_data_zip", "Upload Data (zip file)", accept = ".zip"),
									actionButton("start_data_upload", "Upload and Process Dataset", class = "btn-primary", style="width: 100%;"),
									br(),
									textOutput("data_upload_status")
								),
								mainPanel(
									width = 8,
									collapsible_panel("View Registered Datasets", open = TRUE,
										p("This table shows datasets that are processed and ready for training."),
										DTOutput("dataset_table")
									)
								)
							)
						),
						
						tabPanel("Live Training", 
							
							tags$details(
								open = TRUE,
								tags$summary("Job Status", style = "font-weight: bold; cursor: pointer; margin-top: 20px;"),
								wellPanel(
									strong("Task:"), textOutput("job_task_display", inline = TRUE), br(),
									strong("Job ID:"), textOutput("job_id_display", inline = TRUE), br(),
									strong("Status:"), textOutput("job_status_display", inline = TRUE)
								)
							),
							
							tags$details(
								open = FALSE,
								tags$summary("Full Log", style = "font-weight: bold; cursor: pointer; margin-top: 15px;"),
								wellPanel(
								style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 10px; margin-top: 5px; max-height: 400px; overflow-y: auto; font-family: monospace; white-space: pre-wrap;",
								verbatimTextOutput("log_output")
								)
							),

							tags$details(
								open = FALSE,
								tags$summary("Metrics Table", style = "font-weight: bold; cursor: pointer; margin-top: 15px;"),
								wellPanel(
									DTOutput("eval_table")
								)
							),

							h4("Live Metric Visualization", style="margin-top: 20px;"),
							wellPanel(
								uiOutput("metric_selector_ui")
							),
							dygraphOutput("dynamic_metric_plot", height = "600px")
						),

						tabPanel("Training History",
							h4("Browse Past Training Jobs", style="margin-top:20px;"),
							wellPanel(
								fluidRow(
									column(6,
										selectInput("history_task_filter", "Filter by Task:",
													choices = c("All" = "all",
																"Object Detection" = "object_detection",
																"ASR" = "asr",
																"Image Classification" = "image_classification",
																"Image Segmentation" = "image_segmentation")
										)
									),
									column(6,
										selectInput("history_status_filter", "Filter by Status:",
													choices = c("Completed" = "completed",
																"All" = "all",
																"Failed" = "failed",
																"Running" = "running",
																"Queued" = "queued"),
													selected = "completed"
										)
									)
								),
								selectInput("history_job_selector", "Select a Job to Review:", choices = c("Loading jobs..." = "")),
								textOutput("history_job_details")
							),
							
							collapsible_panel("Historical Evaluation Results", open = TRUE,
								DTOutput("history_eval_table")
							),
							
							collapsible_panel("Historical Metrics Plot", open = TRUE,
								wellPanel(
									uiOutput("history_metric_selector_ui")
								),
								dygraphOutput("history_metric_plot", height = "600px")
							)
						),
						
						tabPanel("Inference",
							h4("Inference", style="margin-top:20px;"),
							selectInput("inference_task_selector", "Select Inference Task:",
										choices = c("Object Detection" = "object_detection",
													"ASR" = "asr",
													"Image Classification" = "image_classification",
													"Image Segmentation" = "image_segmentation")
							),
							hr(),

							conditionalPanel(
								condition = "input.inference_task_selector == 'object_detection'",
								h4("Object Detection Inference"),
								wellPanel(
									textInput("infer_run_name", "Enter Run Name to Find Checkpoints", ""),
									selectInput("infer_checkpoint_dropdown", "Select Checkpoint", choices = NULL),
									fileInput("infer_obj_image_upload", "Upload Image for Detection", accept = c('image/png', 'image/jpeg', 'image/jpg')),
									sliderInput("infer_obj_threshold", "Confidence Threshold", min = 0.01, max = 1.0, value = 0.25, step = 0.01),
									conditionalPanel(
										condition = "input.infer_checkpoint_dropdown && input.infer_checkpoint_dropdown.includes('yolo11')",
										numericInput("infer_obj_iou", "IoU Threshold (NMS)", 0.7, min = 0.01, max = 1.0, step = 0.05),
										numericInput("infer_obj_max_det", "Max Detections", 300, min = 1)
									),		

									actionButton("start_obj_inference", "Run Inference", class = "btn-info", style="margin-top: 10px;")
								),
								hr(),
								h5("Inference Result"),
								uiOutput("inference_status_ui"),
								imageOutput("inference_image_output", height = "auto")
							),
							conditionalPanel(
								condition = "input.inference_task_selector == 'asr'",
								h4("ASR Inference"),
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
							),
							conditionalPanel(
								condition = "input.inference_task_selector == 'image_classification'",
								h4("Image Classification Inference"),
								wellPanel(
									textInput("infer_img_class_run_name", "Enter Run Name to Find Checkpoints", ""),
									selectInput("infer_img_class_checkpoint_dropdown", "Select Checkpoint", choices = NULL),
									fileInput("infer_img_class_upload", "Upload Image for Classification"),
									actionButton("start_img_class_inference", "Run Inference", class = "btn-info", style="margin-top: 10px;")
								),
								hr(),
								h5("Prediction"),
								uiOutput("img_class_inference_status_ui"),
								div(
								style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; margin-top: 5px; min-height: 50px; font-size: 1.1em;",
								textOutput("img_class_prediction_output")
								)
							),
							
							conditionalPanel(
								condition = "input.inference_task_selector == 'image_segmentation'",
								h4("Image Segmentation Inference"),
								wellPanel(
									textInput("infer_seg_run_name", "Enter Run Name to Find Checkpoints", ""),
									selectInput("infer_seg_checkpoint_dropdown", "Select Checkpoint", choices = NULL),
									fileInput("infer_seg_image_upload", "Upload Image for Segmentation", accept = c('image/png', 'image/jpeg', 'image/jpg')),
									actionButton("start_seg_inference", "Run Inference", class = "btn-info", style="margin-top: 10px;")
								),
								hr(),
								h5("Inference Result (Overlay)"),
								uiOutput("seg_inference_status_ui"),
								imageOutput("seg_inference_image_output", height = "auto")
							)
						)
					)
				)
			)
		)
	)
	
  
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
