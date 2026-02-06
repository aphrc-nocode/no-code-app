cnn_ui = function() {
	tabItem(tabName = "cnn_deep",
			fluidRow(
				box(
					width = 12,
					title = "CNN",
					status = "success",
					solidHeader = TRUE,
					collapsible = FALSE,
					tabsetPanel(
						id = "cnn_tabs",
						type = "tabs",
						tabPanel(
							"Dashboard",
							fluidRow(
								box(
									width = 6,
									title = "API Status",
									status = "success",
									solidHeader = TRUE,
									actionButton("check_status", "Check API Status", class = "btn-success"),
									br(),
									br(),
									verbatimTextOutput("api_status")
								),
								box(
									width = 6,
									title = "MLflow Server",
									status = "success",
									solidHeader = TRUE,
									actionButton("start_mlflow", "Start MLflow Server", class = "btn-success"),
									br(),
									br(),
									verbatimTextOutput("mlflow_output")
								)
							),
							fluidRow(
								box(
									width = 12,
									title = "All Jobs Overview",
									status = "success",
									solidHeader = TRUE,
									actionButton("refresh_dashboard_jobs", "Refresh Jobs List", class = "btn-success"),
									br(),
									br(),
									DT::dataTableOutput("dashboard_jobs_table")
								)
							)
						),
						tabPanel(
							"Create Pipeline",
							fluidRow(
								box(
									width = 12,
									title = "Create New Pipeline",
									status = "success",
									solidHeader = TRUE,
									fluidRow(
										column(
											6,
											textInput("pipeline_name", "Pipeline Name", value = "My Image Classifier"),
											selectInput(
												"task_type",
												"Task Type",
												choices = list(
													"Image Classification" = "image_classification",
													"Object Detection" = "object_detection"
												),
												selected = "image_classification"
											),
											selectInput(
												"architecture",
												"Model Architecture",
												choices = list(
													"Image Classification" = list(
														"ResNet-18" = "resnet18",
														"ResNet-50" = "resnet50",
														"VGG-16" = "vgg16",
														"MobileNet" = "mobilenet",
														"EfficientNet" = "efficientnet"
													),
													"Object Detection" = list(
														"Faster R-CNN" = "faster_rcnn"
													)
												),
												selected = "resnet18"
											),
											numericInput("num_classes", "Number of Classes", value = 2, min = 2, max = 1000)
										),
										column(
											6,
											numericInput("batch_size", "Batch Size", value = 8, min = 1, max = 128),
											numericInput("epochs", "Epochs", value = 5, min = 1, max = 1000),
											numericInput("learning_rate", "Learning Rate", value = 0.001, min = 0.0001, max = 1, step = 0.0001),
											textInput("image_size", "Image Size (width, height)", value = "224, 224")
										)
									),
									fluidRow(
										column(6, checkboxInput("augmentation", "Enable Data Augmentation", value = TRUE)),
										column(6, checkboxInput("early_stopping", "Enable Early Stopping", value = TRUE))
									),
									br(),
									actionButton("create_pipeline", "Create Pipeline", class = "btn-success btn-lg"),
									br(),
									br(),
									verbatimTextOutput("create_output")
								)
							)
						),
						tabPanel(
							"Train Model",
							fluidRow(
								box(
									width = 12,
									title = "Current Job Status",
									status = "success",
									solidHeader = TRUE,
									p("Shows the most recently created job ready for training"),
									actionButton("refresh_current_job", "Refresh Current Job", class = "btn-success"),
									br(),
									br(),
									verbatimTextOutput("current_job_status")
								)
							),
							fluidRow(
								box(
									width = 12,
									title = "Upload Dataset to Job",
									status = "success",
									solidHeader = TRUE,
									fluidRow(
										column(
											6,
											h4("Job Selection"),
											selectInput("upload_job_dropdown", "Select Job for Dataset Upload", choices = list()),
											actionButton("refresh_upload_jobs", "Refresh Jobs", class = "btn-success"),
											br(),
											br(),
											checkboxInput("is_coco_format_upload", "COCO Format Dataset (Object Detection)", value = FALSE)
										),
										column(
											6,
											h4("File Upload"),
											fileInput("dataset_file", "Choose Dataset ZIP File", accept = c(".zip"), multiple = FALSE)
										)
									),
									actionButton("upload_dataset", "Upload Dataset to Job", class = "btn-success btn-lg"),
									br(),
									br(),
									verbatimTextOutput("upload_dataset_output")
								)
							),
							fluidRow(
								box(
									width = 12,
									title = "Link Dataset to Job",
									status = "success",
									solidHeader = TRUE,
									fluidRow(
										column(
											6,
											selectInput("pending_job_dropdown", "Select Pending Job", choices = list()),
											actionButton("refresh_pending_jobs", "Refresh Pending Jobs", class = "btn-success")
										),
										column(
											6,
											selectInput("dataset_dropdown", "Select Dataset", choices = list()),
											actionButton("refresh_datasets_dropdown", "Refresh Datasets", class = "btn-success")
										)
									),
									actionButton("link_dataset", "Link Dataset to Job", class = "btn-success"),
									br(),
									br(),
									verbatimTextOutput("link_output")
								)
							),
							fluidRow(
								box(
									width = 12,
									title = "Start Training",
									status = "success",
									solidHeader = TRUE,
									selectInput("trainable_job_dropdown", "Select Job Ready for Training", choices = list()),
									actionButton("refresh_trainable_jobs", "Refresh Trainable Jobs", class = "btn-success"),
									br(),
									br(),
									actionButton("start_training_btn", "Start Training", class = "btn-success btn-lg"),
									br(),
									br(),
									verbatimTextOutput("training_output")
								)
							)
						),
						tabPanel(
							"Make Predictions",
							fluidRow(
								box(
									width = 12,
									title = "Model Selection",
									status = "success",
									solidHeader = TRUE,
									selectInput("predict_job_dropdown", "Select Trained Model", choices = list()),
									actionButton("refresh_prediction_models", "Refresh Available Models", class = "btn-success"),
									br(),
									br(),
									verbatimTextOutput("prediction_models_status")
								)
							),
							fluidRow(
								box(
									width = 12,
									title = "Image Upload & Prediction",
									status = "success",
									solidHeader = TRUE,
									fluidRow(
										column(
											6,
											h4("Upload Image"),
											fileInput("predict_image", "Choose Image File", accept = c('image/png', 'image/jpeg', 'image/jpg')),
											p("Supported formats: JPG, PNG, BMP, TIFF")
										),
										column(
											6,
											h4("Prediction Settings"),
											sliderInput(
												"confidence_threshold",
												"Confidence Threshold",
												min = 0.1,
												max = 0.95,
												value = 0.5,
												step = 0.05,
												post = "%"
											),
											p(class = "help-text", style = "font-size: 12px; color: #666;",
											  "Higher values show fewer, more confident detections. Lower values show more detections but may include false positives."),
											checkboxInput("show_probabilities", "Show All Class Probabilities", value = TRUE)
										)
									),
									br(),
									actionButton("make_prediction", "Make Prediction", class = "btn-success btn-lg"),
									br(), br(),
									fluidRow(
										column(
											6,
											h4("Prediction Results"),
											verbatimTextOutput("prediction_output")
										),
										column(
											6,
											h4("Uploaded Image"),
											imageOutput("prediction_image", height = "auto"),
											br(),
											textOutput("image_info")
										)
									)
								)
							)
						),
						tabPanel(
							"View Jobs",
							fluidRow(
								box(
									width = 12,
									title = "All Jobs",
									status = "success",
									solidHeader = TRUE,
									actionButton("refresh_jobs", "Refresh Jobs List", class = "btn-success"),
									br(),
									br(),
									DT::dataTableOutput("jobs_table")
								)
							),
							fluidRow(
								box(
									width = 12,
									title = "Job Details",
									status = "success",
									solidHeader = TRUE,
									textInput("job_status_id", "Job ID", placeholder = "Enter Job ID to view details"),
									actionButton("get_job_details", "Get Job Status", class = "btn-success"),
									br(),
									br(),
									verbatimTextOutput("job_details_output")
								)
							)
						),
						tabPanel(
							"View Datasets",
							fluidRow(
								box(
									width = 12,
									title = "Available Datasets",
									status = "success",
									solidHeader = TRUE,
									actionButton("refresh_datasets", "Refresh Datasets List", class = "btn-success"),
									br(),
									br(),
									DT::dataTableOutput("datasets_table")
								)
							)
						),
						tabPanel(
							"Delete Job",
							fluidRow(
								box(
									width = 12,
									title = "Delete Job",
									status = "success",
									solidHeader = TRUE,
									selectInput("delete_job_dropdown", "Select Job to Delete", choices = list()),
									actionButton("refresh_delete_jobs", "Refresh Jobs List", class = "btn-success"),
									br(),
									br(),
									actionButton("delete_job_btn", "Delete Selected Job", class = "btn-success btn-lg"),
									br(),
									br(),
									verbatimTextOutput("delete_output")
								)
							)
						)
					)
				)
			)
	)
}
