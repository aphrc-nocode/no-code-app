# ui.R

library(shiny)
library(DT)
library(shinyjs)
library(dygraphs)

# Helper function to create a collapsible section
collapsible_panel <- function(title, ..., open = FALSE) {
    tags$details(
        open = if (open) NA else NULL,
        tags$summary(title, style = "font-weight: bold; cursor: pointer; margin-top: 15px;"),
        div(style = "padding: 10px; border: 1px solid #ddd; border-radius: 5px; margin-top: 5px;", ...)
    )
}

# Helper to create label with help icon
label_with_help <- function(label, tooltip) {
    tagList(
        label,
        tags$i(class = "fa fa-question-circle text-info", style = "margin-left: 5px;", title = tooltip)
    )
}

deeplearning_ui <- function() {
    tabItem(
        tabName = "cnndeep",
        fluidRow(
            useShinyjs(),
            fluidRow(
                column(
                    width = 12,
                    box(
                        title = "System Health", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = NULL,
                        fluidRow(
                            infoBoxOutput("status_cpu_box", width = 3),
                            infoBoxOutput("status_ram_box", width = 3),
                            infoBoxOutput("status_disk_box", width = 3),
                            infoBoxOutput("status_gpu_box", width = 3)
                        )
                    )
                )
            ),

            # --- Main Tabset Interface ---
            tabsetPanel(
                id = "main_tabs",

                # --- Tab 1: Job Manager (Dashboard) ---
                tabPanel(
                    "Job Manager",
                    fluidRow(
                        column(
                            width = 4,
                            box(
                                width = NULL, title = "Job Explorer", status = "success", solidHeader = TRUE,
                                fluidRow(
                                    column(4, selectInput("jm_task_filter", label_with_help("Task", "Filter jobs by task type."),
                                        choices = c("All", "Object Detection" = "object_detection", "Image Classification" = "image_classification", "Semantic Segmentation" = "semantic_segmentation")
                                    )),
                                    column(4, selectInput("jm_status_filter", label_with_help("Status", "Filter jobs by execution status."),
                                        choices = c("All", "Running", "Completed", "Failed")
                                    )),
                                    column(4, selectInput("jm_show_entries", label_with_help("Show", "Number of entries to show."),
                                        choices = c(5, 10, 25, 50, 100), selected = 5
                                    ))
                                ),
                                DTOutput("job_manager_table")
                            )
                        ),
                        column(
                            width = 8,
                            # Static container for Live Metrics (only shown when running) - Prevents dropdown glitches
                            conditionalPanel(
                                condition = "output.is_job_running == true",
                                box(
                                    width = NULL, title = "Live Metrics", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                    selectInput("selected_metric", "Select Metric to Plot:", choices = NULL),
                                    dygraphOutput("dynamic_metric_plot", height = "300px")
                                )
                            ),
                            uiOutput("job_manager_details_ui")
                        )
                    )
                ),

                # --- Tab 2: New Training (Wizard) ---
                tabPanel(
                    "New Training",
                    fluidRow(
                        column(
                            width = 10, offset = 1,

                            # --- Wizard Progress Indicator ---
                            div(
                                style = "margin-bottom: 20px; text-align: center;",
                                h3("Training Configuration")
                            ),
                            box(
                                width = NULL, solidHeader = TRUE, status = "success", title = uiOutput("wizard_step_title"),

                                # --- Wizard Steps (Hidden Tabset) ---
                                tabsetPanel(
                                    id = "wizard_tabs", type = "hidden", selected = "step1",

                                    # === STEP 1: Task & Data ===
                                    tabPanel(
                                        "step1",
                                        selectInput("task_selector", label_with_help("Select Task", "Choose the Deep Learning task you want to perform."),
                                            choices = c(
                                                "Object Detection" = "object_detection",
                                                "Image Classification" = "image_classification",
                                                "Semantic Segmentation" = "semantic_segmentation"
                                            )
                                        ),
                                        hr(style = "border-color: #7BC148;"),
                                        # -- Object Detection Data --
                                        conditionalPanel(
                                            condition = "input.task_selector == 'object_detection'",
                                            selectInput("obj_dataset_id", label_with_help("Select Dataset", "Choose a registered dataset for training."), choices = c("Loading..." = ""))
                                        ),

                                        # -- Image Classification Data --
                                        conditionalPanel(
                                            condition = "input.task_selector == 'image_classification'",
                                            selectInput("img_class_dataset_id", label_with_help("Select Dataset", "Choose a registered dataset for training."), choices = c("Loading..." = ""))
                                        ),
                                        # -- Segmentation Data --
                                        conditionalPanel(
                                            condition = "input.task_selector == 'semantic_segmentation'",
                                            selectInput("seg_dataset_id", label_with_help("Select Dataset", "Choose a registered dataset for training."), choices = c("Loading..." = ""))
                                        ),
                                        br(),
                                        div(class = "text-right", actionButton("wiz_next_1", "Model Setup", icon = icon("arrow-right"), class = "btn btn-success"))
                                    ),

                                    # === STEP 2: Model & Architecture ===
                                    tabPanel(
                                        "step2",

                                        # -- Object Detection Model --
                                        conditionalPanel(
                                            condition = "input.task_selector == 'object_detection'",
                                            selectInput("obj_model_arch", label_with_help("Select Architecture", "Base model architecture to fine-tune."), choices = c("Loading..." = "")),
                                            selectInput("obj_model_checkpoint", label_with_help("Select Checkpoint", "Specific pre-trained weights to start from."), choices = NULL),
                                            textInput("obj_run_name", label_with_help("Run Name", "Unique identifier for this training run."), "shiny-obj-run"),
                                            textInput("obj_version", label_with_help("Version", "Version string for model tracking."), "1.0.0")
                                        ),

                                        # -- Image Classification Model --
                                        conditionalPanel(
                                            condition = "input.task_selector == 'image_classification'",
                                            selectInput("img_class_model_arch", label_with_help("Select Architecture", "Base model architecture to fine-tune."), choices = c("Loading..." = "")),
                                            selectInput("img_class_model_checkpoint", label_with_help("Select Checkpoint", "Specific pre-trained weights to start from."), choices = NULL),
                                            textInput("img_class_run_name", label_with_help("Run Name", "Unique identifier for this training run."), "shiny-img-run"),
                                            textInput("img_class_version", label_with_help("Version", "Version string for model tracking."), "1.0.0")
                                        ),
                                        # -- Segmentation Model --
                                        conditionalPanel(
                                            condition = "input.task_selector == 'semantic_segmentation'",
                                            selectInput("seg_model_arch", label_with_help("Select Architecture", "Base model architecture to fine-tune."), choices = c("Loading..." = "")),
                                            selectInput("seg_model_checkpoint", label_with_help("Select Checkpoint", "Specific pre-trained weights to start from."), choices = NULL),
                                            textInput("seg_run_name", label_with_help("Run Name", "Unique identifier for this training run."), "shiny-seg-run"),
                                            textInput("seg_version", label_with_help("Version", "Version string for model tracking."), "1.0.0")
                                        ),
                                        br(),
                                        div(
                                            class = "text-right",
                                            actionButton("wiz_back_2", "Task & Data", icon = icon("arrow-left"), class = "btn btn-outline-success"),
                                            actionButton("wiz_next_2", "Hyperparameters", icon = icon("arrow-right"), class = "btn btn-success")
                                        )
                                    ),

                                    # === STEP 3: Hyperparameters ===
                                    tabPanel(
                                        "step3",

                                        # -- Object Detection Params --
                                        conditionalPanel(
                                            condition = "input.task_selector == 'object_detection'",
                                            collapsible_panel("Basic Configuration",
                                                open = TRUE,
                                                numericInput("obj_epochs", label_with_help("Epochs", "Number of complete passes through the training dataset."), 5, min = 1),
                                                numericInput("obj_train_batch_size", label_with_help("Train Batch Size", "Number of training samples per batch."), 8, min = 1)
                                            ),
                                            collapsible_panel("Advanced Configuration",
                                                open = FALSE,
                                                numericInput("obj_max_image_size", label_with_help("Max Image Size", "Resize images to this dimension."), 640, min = 64),
                                                numericInput("obj_seed", label_with_help("Random Seed", "Seed for reproducibility."), 42),
                                                numericInput("obj_early_stopping_patience", label_with_help("Early Stopping Patience", "Epochs to wait before stopping if no improvement."), 5, min = 1),

                                                # HF Specific
                                                conditionalPanel(
                                                    condition = "input.obj_model_checkpoint.indexOf('.pt') == -1",
                                                    numericInput("obj_eval_batch_size", label_with_help("Eval Batch Size", "Batch size for validation."), 8, min = 1),
                                                    numericInput("obj_learning_rate", label_with_help("Learning Rate", "Step size for optimizer."), 5e-5, step = 1e-6),
                                                    numericInput("obj_weight_decay_hf", label_with_help("Weight Decay", "Regularization factor for Transformers."), 1e-4, step = 0.0001),
                                                    numericInput("obj_gradient_accumulation_steps", label_with_help("Gradient Accumulation", "Steps to accumulate gradients before update."), 1, min = 1),
                                                    checkboxInput("obj_gradient_checkpointing", label_with_help("Gradient Checkpointing", "Save memory at cost of speed."), value = FALSE),
                                                    numericInput("obj_early_stopping_threshold", label_with_help("Early Stop Threshold", "Minimum change to qualify as improvement."), 0.0, step = 0.001),

                                                    # Added for Production Audit (YOLOS/HF Support)
                                                    selectInput("obj_optimizer_hf", label_with_help("Optimizer", "Optimizer algorithm."), choices = c("AdamW" = "adamw_torch", "SGD" = "sgd", "Adafactor" = "adafactor"), selected = "adamw_torch"),
                                                    selectInput("obj_scheduler_hf", label_with_help("LR Scheduler", "Learning rate schedule."), choices = c("Linear" = "linear", "Cosine" = "cosine", "Constant" = "constant"), selected = "linear"),
                                                    numericInput("obj_warmup_epochs_hf", label_with_help("Warmup Epochs", "Epochs to warm up learning rate."), 1.0, min = 0.0, step = 0.1)
                                                ),

                                                # Ultralytics/YOLO Specific
                                                conditionalPanel(
                                                    condition = "input.obj_model_checkpoint.indexOf('.pt') > -1 || input.obj_model_checkpoint.indexOf('rtdetr') > -1",
                                                    numericInput("obj_weight_decay", label_with_help("Weight Decay", "Regularization factor."), 0.0005, step = 0.0001),
                                                    selectInput("obj_optimizer", label_with_help("Optimizer", "Optimizer algorithm."), choices = c("auto", "sgd", "adam", "adamw"), selected = "auto"),
                                                    numericInput("obj_warmup_epochs", label_with_help("Warmup Epochs", "Number of epochs for learning rate warmup."), 3.0, min = 0.0),
                                                    numericInput("obj_momentum", label_with_help("Momentum", "Momentum factor for SGD."), 0.937, step = 0.001),
                                                    numericInput("obj_lr0", label_with_help("Initial Learning Rate", "Initial learning rate."), 0.01, step = 0.001)
                                                ),
                                                br(),
                                                h4("Data Augmentation"),
                                                checkboxInput("obj_enable_augment", "Enable Augmentation", value = FALSE),
                                                conditionalPanel(
                                                    condition = "input.obj_enable_augment == true",
                                                    wellPanel(
                                                        style = "background-color: #f9f9f9; border-left: 3px solid #7BC148;",
                                                        numericInput("obj_flip_prob", label_with_help("Horizontal Flip Prob.", "Probability of flipping image horizontally."), 0.5, min = 0.0, max = 1.0, step = 0.1),
                                                        numericInput("obj_rotate_limit", label_with_help("Rotation Limit", "Random rotation in range (-limit, +limit)."), 15, min = 0, max = 180),
                                                        numericInput("obj_brightness", label_with_help("Brightness Jitter", "Random brightness adjustment factor (0-1)."), 0.2, min = 0.0, max = 1.0, step = 0.1),
                                                        numericInput("obj_contrast", label_with_help("Contrast Jitter", "Random contrast adjustment factor (0-1)."), 0.2, min = 0.0, max = 1.0, step = 0.1),

                                                        # YOLO Specific Augmentations
                                                        conditionalPanel(
                                                            condition = "input.obj_model_checkpoint.indexOf('.pt') > -1 || input.obj_model_checkpoint.indexOf('rtdetr') > -1",
                                                            numericInput("obj_mosaic", label_with_help("Mosaic Prob.", "Probability of Mosaic augmentation (0-1)."), 1.0, min = 0.0, max = 1.0, step = 0.1),
                                                            numericInput("obj_mixup", label_with_help("Mixup Prob.", "Probability of Mixup augmentation (0-1)."), 0.0, min = 0.0, max = 1.0, step = 0.1),
                                                            numericInput("obj_hsv_h", label_with_help("HSV-H Gain", "Hue adjustment gain."), 0.015, step = 0.001),
                                                            numericInput("obj_hsv_s", label_with_help("HSV-S Gain", "Saturation adjustment gain."), 0.7, step = 0.1),
                                                            numericInput("obj_hsv_v", label_with_help("HSV-V Gain", "Value (Brightness) adjustment gain."), 0.4, step = 0.1)
                                                        )
                                                    )
                                                )
                                            )
                                        ),

                                        # -- Image Classification Params --
                                        conditionalPanel(
                                            condition = "input.task_selector == 'image_classification'",
                                            collapsible_panel("Training Parameters",
                                                open = TRUE,
                                                numericInput("img_class_num_train_epochs", label_with_help("Epochs", "Number of complete passes through the training dataset."), 5, min = 1),
                                                numericInput("img_class_per_device_train_batch_size", label_with_help("Train Batch Size", "Number of training samples per batch."), 16, min = 1)
                                            ),
                                            collapsible_panel("Advanced Configuration",
                                                open = FALSE,
                                                numericInput("img_class_eval_batch_size", label_with_help("Eval Batch Size", "Batch size for validation."), 16, min = 1),
                                                numericInput("img_class_learning_rate", label_with_help("Learning Rate", "Step size for optimizer."), 2e-5, step = 1e-6),
                                                numericInput("img_class_weight_decay", label_with_help("Weight Decay", "Regularization factor."), 0.01, step = 0.001),
                                                numericInput("img_class_max_image_size", label_with_help("Max Image Size", "Resize images to this dimension (square)."), 224, min = 32),
                                                numericInput("img_class_gradient_accumulation_steps", label_with_help("Gradient Accumulation", "Steps to accumulate gradients before update."), 1, min = 1),
                                                checkboxInput("img_class_gradient_checkpointing", label_with_help("Gradient Checkpointing", "Save memory at cost of speed."), value = FALSE),
                                                numericInput("img_class_early_stopping_patience", label_with_help("Early Stopping Patience", "Epochs to wait without improvement."), 3, min = 1),
                                                numericInput("img_class_early_stopping_threshold", label_with_help("Early Stop Threshold", "Minimum change to qualify as improvement."), 0.0, step = 0.001),
                                                numericInput("img_class_seed", label_with_help("Random Seed", "Seed for reproducibility."), 42),

                                                # Added for Production Audit
                                                selectInput("img_class_optimizer", label_with_help("Optimizer", "Optimizer algorithm."), choices = c("AdamW" = "adamw_torch", "SGD" = "sgd", "Adafactor" = "adafactor"), selected = "adamw_torch"),
                                                selectInput("img_class_scheduler", label_with_help("LR Scheduler", "Learning rate schedule."), choices = c("Linear" = "linear", "Cosine" = "cosine", "Constant" = "constant"), selected = "linear"),
                                                numericInput("img_class_warmup_epochs", label_with_help("Warmup Epochs", "Epochs to warm up learning rate."), 1.0, min = 0.0, step = 0.1),
                                                br(),
                                                h4("Data Augmentation"),
                                                checkboxInput("img_class_enable_augment", "Enable Augmentation", value = FALSE),
                                                conditionalPanel(
                                                    condition = "input.img_class_enable_augment == true",
                                                    wellPanel(
                                                        style = "background-color: #f9f9f9; border-left: 3px solid #7BC148;",
                                                        numericInput("img_class_flip_prob", label_with_help("Horizontal Flip Prob.", "Probability of flipping image horizontally."), 0.5, min = 0.0, max = 1.0, step = 0.1),
                                                        numericInput("img_class_rotate_limit", label_with_help("Rotation Limit", "Random rotation in range (-limit, +limit)."), 15, min = 0, max = 180),
                                                        numericInput("img_class_brightness", label_with_help("Brightness Jitter", "Random brightness adjustment factor (0-1)."), 0.2, min = 0.0, max = 1.0, step = 0.1),
                                                        numericInput("img_class_contrast", label_with_help("Contrast Jitter", "Random contrast adjustment factor (0-1)."), 0.2, min = 0.0, max = 1.0, step = 0.1)
                                                    )
                                                )
                                            )
                                        ),
                                        # -- Segmentation Params --
                                        conditionalPanel(
                                            condition = "input.task_selector == 'semantic_segmentation'",
                                            collapsible_panel("Training Parameters",
                                                open = TRUE,
                                                numericInput("seg_num_train_epochs", label_with_help("Epochs", "Number of complete passes through the training dataset."), 10, min = 1),
                                                numericInput("seg_per_device_train_batch_size", label_with_help("Train Batch Size", "Number of training samples per batch."), 4, min = 1)
                                            ),
                                            collapsible_panel("Advanced Configuration",
                                                open = FALSE,
                                                numericInput("seg_eval_batch_size", label_with_help("Eval Batch Size", "Batch size for validation."), 4, min = 1),
                                                numericInput("seg_learning_rate", label_with_help("Learning Rate", "Step size for optimizer."), 6e-5, step = 1e-6),
                                                numericInput("seg_weight_decay", label_with_help("Weight Decay", "Regularization factor."), 0.01, step = 0.001),
                                                numericInput("seg_max_image_size", label_with_help("Max Image Size", "Resize images to this dimension (square)."), 512, min = 128),
                                                numericInput("seg_gradient_accumulation_steps", label_with_help("Gradient Accumulation", "Steps to accumulate gradients before update."), 1, min = 1),
                                                checkboxInput("seg_gradient_checkpointing", label_with_help("Gradient Checkpointing", "Save memory at cost of speed."), value = FALSE),
                                                numericInput("seg_early_stopping_patience", label_with_help("Early Stopping Patience", "Epochs to wait without improvement."), 5, min = 1),
                                                numericInput("seg_early_stopping_threshold", label_with_help("Early Stop Threshold", "Minimum change to qualify as improvement."), 0.0, step = 0.001),
                                                numericInput("seg_seed", label_with_help("Random Seed", "Seed for reproducibility."), 42),

                                                # Added for Production Audit
                                                selectInput("seg_optimizer", label_with_help("Optimizer", "Optimizer algorithm."), choices = c("AdamW" = "adamw_torch", "SGD" = "sgd", "Adafactor" = "adafactor"), selected = "adamw_torch"),
                                                selectInput("seg_scheduler", label_with_help("LR Scheduler", "Learning rate schedule."), choices = c("Linear" = "linear", "Cosine" = "cosine", "Constant" = "constant"), selected = "linear"),
                                                numericInput("seg_warmup_epochs", label_with_help("Warmup Epochs", "Epochs to warm up learning rate."), 1.0, min = 0.0, step = 0.1),
                                                br(),
                                                h4("Data Augmentation"),
                                                checkboxInput("seg_enable_augment", "Enable Augmentation", value = FALSE),
                                                conditionalPanel(
                                                    condition = "input.seg_enable_augment == true",
                                                    wellPanel(
                                                        style = "background-color: #f9f9f9; border-left: 3px solid #7BC148;",
                                                        numericInput("seg_flip_prob", label_with_help("Horizontal Flip Prob.", "Probability of flipping image horizontally."), 0.5, min = 0.0, max = 1.0, step = 0.1),
                                                        numericInput("seg_rotate_limit", label_with_help("Rotation Limit", "Random rotation in range (-limit, +limit)."), 15, min = 0, max = 180),
                                                        numericInput("seg_brightness", label_with_help("Brightness Jitter", "Random brightness adjustment factor (0-1)."), 0.2, min = 0.0, max = 1.0, step = 0.1),
                                                        numericInput("seg_contrast", label_with_help("Contrast Jitter", "Random contrast adjustment factor (0-1)."), 0.2, min = 0.0, max = 1.0, step = 0.1)
                                                    )
                                                )
                                            )
                                        ),
                                        br(),
                                        div(
                                            class = "text-right",
                                            actionButton("wiz_back_3", "Model Setup", icon = icon("arrow-left"), class = "btn btn-outline-success"),
                                            actionButton("wiz_next_3", "Review & Launch", icon = icon("arrow-right"), class = "btn btn-success")
                                        )
                                    ),

                                    # === STEP 4: Review & Launch ===
                                    tabPanel(
                                        "step4",
                                        box(
                                            width = NULL, status = "success",
                                            h5("Ready to Start Training"),
                                            p("Please review your settings."),
                                            p("You can go back to change them."),
                                            tableOutput("wizard_review_table"), # Dynamic Review Table

                                            # Launch Buttons
                                            conditionalPanel(
                                                condition = "input.task_selector == 'object_detection'",
                                                actionButton("start_obj_job", "Launch Object Detection", class = "btn btn-success btn-lg btn-block", icon = icon("rocket"))
                                            ),
                                            conditionalPanel(
                                                condition = "input.task_selector == 'image_classification'",
                                                actionButton("start_img_class_job", "Launch Image Classification", class = "btn btn-success btn-lg btn-block", icon = icon("rocket"))
                                            ),
                                            conditionalPanel(
                                                condition = "input.task_selector == 'semantic_segmentation'",
                                                actionButton("start_seg_job", "Launch Segmentation Job", class = "btn btn-success btn-lg btn-block", icon = icon("rocket"))
                                            ),
                                        ),
                                        br(),
                                        div(
                                            class = "text-right",
                                            actionButton("wiz_back_4", "Hyperparameters", icon = icon("arrow-left"), class = "btn btn-outline-success")
                                        )
                                    )
                                ) # End wizard tabset
                            ) # End box
                        ) # End column
                    ) # End fluidRow
                ), # End New Training tabPanel

                # --- Tab: Data Management ---
                tabPanel(
                    "Data Management",
                    # h4 removed as per request to remove redundancy
                    fluidRow(
                        column(
                            width = 4,
                            box(
                                width = NULL, title = tagList(icon("upload"), "Register a New Dataset"), status = "success", solidHeader = FALSE,
                                textInput("new_data_name", label_with_help("Dataset Name", "Unique name for the dataset.")),
                                selectInput("new_data_task_type", label_with_help("Task Type", "The type of Deep Learning task this data is for."),
                                    choices = c(
                                        "Object Detection" = "object_detection",
                                        "Image Classification" = "image_classification",
                                        "Semantic Segmentation" = "semantic_segmentation"
                                    )
                                ),
                                fileInput("new_data_zip", label_with_help("Upload Data", "Zip (Images) or CSV/Parquet (Tabular/Timeseries)."), accept = c(".zip", ".csv", ".parquet")),
                                actionButton("start_data_upload", "Register & Process", class = "btn btn-success", style = "width: 100%;"),
                                br(),
                                textOutput("data_upload_status")
                            )
                        ),
                        column(
                            width = 8,
                            box(
                                width = NULL, title = "Registered Datasets", status = "success", solidHeader = FALSE,
                                p("This table shows datasets that are processed and ready for training."),
                                DTOutput("dataset_table")
                            )
                        )
                    )
                ),

                # --- Tab: Prediction ---
                tabPanel(
                    "Prediction",
                    br(),
                    tabsetPanel(
                        id = "inference_task_selector",
                        type = "pills",
                        tabPanel(
                            title = "Object Detection",
                            value = "object_detection",
                            br(),
                            fluidRow(
                                column(
                                    5,
                                    box(
                                        width = NULL, title = "Configuration", status = "success", solidHeader = TRUE,
                                        selectInput("infer_run_name", label_with_help("Select Run", "Choose a completed training run."), choices = NULL),
                                        selectInput("infer_checkpoint_dropdown", label_with_help("Select Checkpoint", "Choose a model checkpoint."), choices = NULL),
                                        fileInput("infer_obj_image_upload", label_with_help("Upload Image", "Image to detect objects in."), accept = c("image/png", "image/jpeg", "image/jpg")),
                                        sliderInput("infer_obj_threshold", label_with_help("Confidence Threshold", "Minimum confidence for detections."), min = 0.01, max = 1.0, value = 0.25, step = 0.01),
                                        conditionalPanel(
                                            condition = "input.infer_checkpoint_dropdown && (input.infer_checkpoint_dropdown.indexOf('.pt') > -1 || input.infer_checkpoint_dropdown.indexOf('rtdetr') > -1)",
                                            numericInput("infer_obj_iou", label_with_help("IoU Threshold (NMS)", "Intersection over Union threshold for Non-Max Suppression."), 0.7, min = 0.01, max = 1.0, step = 0.05),
                                            numericInput("infer_obj_max_det", label_with_help("Max Detections", "Maximum number of objects to detect per image."), 300, min = 1),
                                            # New Params
                                            numericInput("infer_obj_imgsz", label_with_help("Image Size", "Inference image size (pixels)."), 640, min = 32),
                                            textInput("infer_obj_classes", label_with_help("Classes Filter", "Filter by class IDs (comma-separated, e.g. '0, 2'). Leave empty for all."), "")
                                        ),
                                        actionButton("start_obj_inference", "Run Inference", class = "btn btn-success", style = "margin-top: 10px; width: 100%;")
                                    )
                                ),
                                column(
                                    7,
                                    box(
                                        width = NULL, title = "Result", status = "success", solidHeader = TRUE,
                                        uiOutput("inference_status_ui"),
                                        imageOutput("inference_image_output", height = "auto")
                                    )
                                )
                            )
                        ),
                        tabPanel(
                            title = "Image Classification",
                            value = "image_classification",
                            br(),
                            fluidRow(
                                column(
                                    5,
                                    box(
                                        width = NULL, title = "Configuration", status = "success", solidHeader = TRUE,
                                        selectInput("infer_img_class_run_name", label_with_help("Select Run", "Choose a completed training run."), choices = NULL),
                                        selectInput("infer_img_class_checkpoint_dropdown", label_with_help("Checkpoint", "Choose a model checkpoint."), choices = NULL),
                                        fileInput("infer_img_class_upload", label_with_help("Upload Image", "Image to classify.")),
                                        actionButton("start_img_class_inference", "Run Inference", class = "btn btn-success", style = "margin-top: 10px; width: 100%;")
                                    )
                                ),
                                column(
                                    7,
                                    box(
                                        width = NULL, title = "Prediction", status = "success", solidHeader = TRUE,
                                        uiOutput("img_class_inference_status_ui"),
                                        verbatimTextOutput("img_class_prediction_output", placeholder = TRUE)
                                    )
                                )
                            )
                        ),
                        tabPanel(
                            title = "Semantic Segmentation",
                            value = "semantic_segmentation",
                            br(),
                            fluidRow(
                                column(
                                    5,
                                    box(
                                        width = NULL, title = "Configuration", status = "success", solidHeader = TRUE,
                                        selectInput("infer_seg_run_name", label_with_help("Select Run", "Choose a completed training run."), choices = NULL),
                                        selectInput("infer_seg_checkpoint_dropdown", label_with_help("Checkpoint", "Choose a model checkpoint."), choices = NULL),
                                        fileInput("infer_seg_image_upload", label_with_help("Upload Image", "Image to segment."), accept = c("image/png", "image/jpeg", "image/jpg")),
                                        actionButton("start_seg_inference", "Run Inference", class = "btn btn-success", style = "margin-top: 10px; width: 100%;")
                                    )
                                ),
                                column(
                                    7,
                                    box(
                                        width = NULL, title = "Segmentation Overlay", status = "success", solidHeader = TRUE,
                                        uiOutput("seg_inference_status_ui"),
                                        imageOutput("seg_inference_image_output", height = "auto")
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
}
