tl_collapsible_box <- function(title, ..., open = TRUE) {
  tags$details(
    open  = if (isTRUE(open)) NA else NULL,
    class = "tl-box tl-collapsible",
    tags$summary(class = "tl-header", title),
    div(class = "tl-body", ...)
  )
}

transfer_learning_ui <- function(id = "transfer_learning") {
  ns <- NS(id)

  tabItem(
    tabName = "transfer_learning",

    tags$head(tags$style(HTML("
      .tl-box { background-color: white; border-radius: 8px; margin-bottom: 18px;
                box-shadow: 0 1px 3px rgba(0,0,0,0.08); }
      .tl-header { background-color: #00a65a; color: white; padding: 10px 14px;
                   font-weight: 600; border-top-left-radius: 8px; border-top-right-radius: 8px; }
      .tl-body { padding: 15px; }
      .tl-note { background-color: #e8f5e9; border-left: 4px solid #00a65a;
                 padding: 10px 12px; margin-bottom: 14px; border-radius: 4px; }
      .tl-outcome-badge { display: inline-block; background-color: #f4f4f4; border: 1px solid #ddd;
                          border-radius: 4px; padding: 6px 12px; font-weight: 600; color: #333;
                          margin-top: 4px; margin-bottom: 10px; }
      .tl-download-bar { margin-bottom: 12px; padding: 8px 0 6px 0; border-bottom: 1px solid #f0f0f0; }
      .tl-download-bar .btn { background-color: #00a65a; color: #ffffff !important;
                               border-color: #008d4c; font-size: 13px; }
      .tl-download-bar .btn:hover { background-color: #008d4c; color: #ffffff !important; }
      #shiny-notification-panel { top: 0 !important; bottom: auto !important;
                                   left: 50% !important; right: auto !important;
                                   transform: translateX(-50%); width: 420px; }
      #shiny-notification-panel .progress-bar { background-color: #00a65a !important; }
      #shiny-notification-panel .shiny-notification { background-color: #ffffff;
        border-top: 3px solid #00a65a; border-radius: 0 0 6px 6px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15); }
      #shiny-notification-panel .shiny-notification-message { color: #333; font-weight: 500; }
      .tl-collapsible > summary { cursor: pointer; list-style: none; outline: none; }
      .tl-collapsible > summary::-webkit-details-marker { display: none; }
      .tl-collapsible > summary::before { content: '\\25B8  '; }
      .tl-collapsible[open] > summary::before { content: '\\25BE  '; }
    "))),

    # ── No-data message ──────────────────────────────────────────────────────
    conditionalPanel(
      condition = sprintf("output['%s'] !== true", ns("data_ready_flag")),
      div(class = "tl-box",
          div(class = "tl-header", "Transfer Learning"),
          div(class = "tl-body",
              h4("No active dataset found"),
              p("Please upload data, select it, and review it in the Overview section before using Transfer Learning.")))
    ),

    # ── Full workflow ─────────────────────────────────────────────────────────
    conditionalPanel(
      condition = sprintf("output['%s'] === true", ns("data_ready_flag")),
      tagList(
        fluidRow(column(12,
          h3("Transfer Learning Workflow"),
          div(class = "tl-note",
              "Select the trained source model, apply it to the active target dataset, analyse shift and performance, compare with retrained models, fine-tune, and export a report.")
        )),

        tabsetPanel(
          id = ns("transfer_learning_steps"),

          # ==================================================================
          # Step 1: Source Model
          # ==================================================================
          tabPanel("Step 1: Source Model", br(),
            fluidRow(column(12,
              div(class = "tl-box",
                div(class = "tl-header", "Select Trained Source Model"),
                div(class = "tl-body",
                  fluidRow(
                    column(4, uiOutput(ns("tl_dataset_ui"))),
                    column(4, uiOutput(ns("tl_outcome_ui"))),
                    column(4, uiOutput(ns("tl_session_ui")))
                  ),
                  fluidRow(
                    column(4, uiOutput(ns("tl_metric_ui"))),
                    column(4, uiOutput(ns("tl_model_ui"))),
                    column(4, conditionalPanel(
                      condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                      sliderInput(ns("class_threshold"), "Classification threshold",
                                  min = 0.10, max = 0.90, value = 0.50, step = 0.01)
                    ))
                  )
                )
              )
            )),
            fluidRow(
              column(4, div(class = "tl-box",
                div(class = "tl-header", "Model Metadata"),
                div(class = "tl-body", tableOutput(ns("model_metadata_table"))))),
              column(4, div(class = "tl-box",
                div(class = "tl-header", "Source Model Requirements"),
                div(class = "tl-body", tableOutput(ns("source_schema_table"))))),
              column(4, div(class = "tl-box",
                div(class = "tl-header", "Source Model Details"),
                div(class = "tl-body", tableOutput(ns("source_hyperparam_table")))))
            )
          ),

          # ==================================================================
          # Step 2: Target Data + Compatibility + Shift
          # ==================================================================
          tabPanel("Step 2: Target Data & Compatibility", br(),

            # Active dataset info
            fluidRow(
              column(4,
                div(class = "tl-box",
                  div(class = "tl-header", "Active Dataset"),
                  div(class = "tl-body",
                    p("This module uses the currently active dataset from the no-code platform."),
                    verbatimTextOutput(ns("active_dataset_info")),
                    div(class = "tl-note",
                        strong("Outcome variable for evaluation:"), br(),
                        div(class = "tl-outcome-badge", textOutput(ns("active_outcome_display"), inline = TRUE))),
                    br(),
                    radioButtons(ns("outcome_type"), "Outcome type",
                                 choices = c("Binary classification" = "classification",
                                             "Regression (continuous outcome)" = "regression"),
                                 selected = "classification", inline = FALSE),
                    br(),
                    uiOutput(ns("source_scoring_status_ui")), br(),
                    actionButton(ns("run_source_only"), "Run Source Model Only",
                                 class = "btn-success", width = "100%")
                  )
                )
              ),
              column(8,
                div(class = "tl-box",
                  div(class = "tl-header", "Target Data Preview"),
                  div(class = "tl-body", tableOutput(ns("target_data_preview"))))
              )
            ),

            # IMPROVEMENT 1: Schema Compatibility Check
            fluidRow(column(12,
              div(class = "tl-box",
                div(class = "tl-header",
                    tags$i(class = "fa fa-check-circle"), " Schema Compatibility Check"),
                div(class = "tl-body",
                  p("Checks that the active target dataset is compatible with the source model's expected inputs."),
                  div(class = "tl-download-bar",
                      downloadButton(ns("dl_schema_check"), "Download CSV", icon = icon("download"))),
                  DT::dataTableOutput(ns("schema_check_table"))
                )
              )
            )),

            # IMPROVEMENT 2: Distribution Shift Detection
            fluidRow(column(12,
              div(class = "tl-box",
                div(class = "tl-header",
                    tags$i(class = "fa fa-chart-bar"), " Distribution Shift Detection"),
                div(class = "tl-body",
                  p("Detects covariate shift between source training distribution and target dataset. PSI > 0.1 = minor shift, PSI > 0.2 = major shift."),
                  tabsetPanel(
                    tabPanel("Shift Table", br(),
                      div(class = "tl-download-bar",
                          downloadButton(ns("dl_shift_table"), "Download CSV", icon = icon("download"))),
                      DT::dataTableOutput(ns("shift_summary_table"))
                    ),
                    tabPanel("PSI Plot", br(),
                      div(class = "tl-download-bar",
                          downloadButton(ns("dl_shift_plot"), "Download PNG", icon = icon("download"))),
                      plotOutput(ns("shift_plot"), height = 380)
                    )
                  )
                )
              )
            ))
          ),

          # ==================================================================
          # Step 3: Scoring & Analysis
          # ==================================================================
          tabPanel("Step 3: Scoring & Analysis", br(),

            # Metrics table
            fluidRow(column(12,
              div(class = "tl-box",
                div(class = "tl-header", "Metrics on Target Dataset (with 95% Bootstrap CI)"),
                div(class = "tl-body",
                  conditionalPanel(
                    condition = sprintf("output['%s'] === true && output['%s'] === true", ns("source_ready"), ns("source_single_selected")),
                    div(class = "tl-download-bar",
                        downloadButton(ns("dl_source_metrics"), "Download Metrics CSV", icon = icon("download")))
                  ),
                  conditionalPanel(
                    condition = sprintf("output['%s'] === true", ns("source_single_selected")),
                    tableOutput(ns("source_only_metrics"))
                  ),
                  uiOutput(ns("source_multi_metrics_ui"))
                )
              )
            )),

            # Evaluation plots
            fluidRow(column(12,
              div(class = "tl-box",
                div(class = "tl-header", "Evaluation Plots"),
                div(class = "tl-body",
                  tabsetPanel(
                    id = ns("source_only_plots_tabs"), type = "tabs",

                    # ---------- Classification-only tabs ----------
                    tabPanel("ROC Curve", br(),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                        conditionalPanel(
                          condition = sprintf("output['%s'] === true", ns("source_single_selected")),
                          conditionalPanel(condition = sprintf("output['%s'] === true", ns("source_ready")),
                            div(class = "tl-download-bar",
                                downloadButton(ns("dl_source_roc"), "Download PNG", icon = icon("download")))),
                          plotOutput(ns("source_only_roc_plot"), height = 330)
                        ),
                        conditionalPanel(
                          condition = sprintf("output['%s'] !== true", ns("source_single_selected")),
                          div(class = "tl-download-bar",
                              downloadButton(ns("dl_source_multi_roc"), "Download PNG", icon = icon("download"))),
                          plotOutput(ns("source_multi_roc_plot"), height = 330)
                        )
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'regression'", ns("outcome_type")),
                        div(class = "alert alert-info", tags$i(class = "fa fa-info-circle"),
                            " ROC Curve is only available for binary classification problems.")
                      )
                    ),
                    tabPanel("Confusion Matrix", br(),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                        conditionalPanel(condition = sprintf("output['%s'] === true", ns("source_ready")),
                          div(class = "tl-download-bar",
                              downloadButton(ns("dl_source_conf_mat"), "Download PNG", icon = icon("download")))),
                        plotOutput(ns("source_only_conf_mat_plot"), height = 360)
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'regression'", ns("outcome_type")),
                        div(class = "alert alert-info", tags$i(class = "fa fa-info-circle"),
                            " Confusion Matrix is only available for binary classification problems.")
                      )
                    ),
                    tabPanel("Calibration", br(),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                        conditionalPanel(condition = sprintf("output['%s'] === true", ns("source_ready")),
                          div(class = "tl-download-bar",
                              downloadButton(ns("dl_source_calibration"), "Download PNG", icon = icon("download")))),
                        plotOutput(ns("source_only_calibration_plot"), height = 330)
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'regression'", ns("outcome_type")),
                        div(class = "alert alert-info", tags$i(class = "fa fa-info-circle"),
                            " Calibration plot is only available for binary classification problems.")
                      )
                    ),
                    tabPanel("Prediction by Outcome", br(),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                        conditionalPanel(condition = sprintf("output['%s'] === true", ns("source_ready")),
                          div(class = "tl-download-bar",
                              downloadButton(ns("dl_source_boxplot"), "Download PNG", icon = icon("download")))),
                        plotOutput(ns("source_only_boxplot"), height = 330)
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'regression'", ns("outcome_type")),
                        div(class = "alert alert-info", tags$i(class = "fa fa-info-circle"),
                            " This plot is only available for binary classification problems.")
                      )
                    ),

                    # ---------- Regression-only tabs ----------
                    tabPanel("Actual vs Predicted", br(),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'regression'", ns("outcome_type")),
                        div(class = "tl-note", tags$i(class = "fa fa-info-circle"),
                            " Each dot is one observation. The dashed line is perfect prediction; the orange line is a linear fit."),
                        conditionalPanel(condition = sprintf("output['%s'] === true", ns("source_ready")),
                          div(class = "tl-download-bar",
                              downloadButton(ns("dl_reg_actual_vs_pred"), "Download PNG", icon = icon("download")))),
                        plotOutput(ns("reg_actual_vs_pred_plot"), height = 360)
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                        div(class = "alert alert-info", tags$i(class = "fa fa-info-circle"),
                            " Actual vs Predicted plot is only available for regression problems.")
                      )
                    ),
                    tabPanel("Residuals Plot", br(),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'regression'", ns("outcome_type")),
                        div(class = "tl-note", tags$i(class = "fa fa-info-circle"),
                            " Residuals vs fitted values. Patterns suggest non-linearity or heteroscedasticity."),
                        conditionalPanel(condition = sprintf("output['%s'] === true", ns("source_ready")),
                          div(class = "tl-download-bar",
                              downloadButton(ns("dl_reg_residuals"), "Download PNG", icon = icon("download")))),
                        plotOutput(ns("reg_residuals_plot"), height = 360)
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                        div(class = "alert alert-info", tags$i(class = "fa fa-info-circle"),
                            " Residuals plot is only available for regression problems.")
                      )
                    ),
                    tabPanel("Residual Distribution", br(),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'regression'", ns("outcome_type")),
                        div(class = "tl-note", tags$i(class = "fa fa-info-circle"),
                            " Histogram of residuals. Centred near zero with roughly symmetric shape is desirable."),
                        conditionalPanel(condition = sprintf("output['%s'] === true", ns("source_ready")),
                          div(class = "tl-download-bar",
                              downloadButton(ns("dl_reg_residual_dist"), "Download PNG", icon = icon("download")))),
                        plotOutput(ns("reg_residual_dist_plot"), height = 340)
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                        div(class = "alert alert-info", tags$i(class = "fa fa-info-circle"),
                            " Residual distribution is only available for regression problems.")
                      )
                    ),

                    # ---------- Shared tabs (both modes) ----------
                    tabPanel("Feature Importance", br(),
                      div(class = "tl-note",
                          tags$i(class = "fa fa-info-circle"),
                          " Model-agnostic permutation importance. For classification: drop in AUC; for regression: increase in RMSE. Larger bars = more important."),
                      tabsetPanel(
                        tabPanel("Variable Importance Plot", br(),
                          conditionalPanel(condition = sprintf("output['%s'] === true", ns("fi_ready")),
                            div(class = "tl-download-bar",
                                downloadButton(ns("dl_feature_importance_plot"), "Download PNG", icon = icon("download")))),
                          plotOutput(ns("feature_importance_plot"), height = 380)
                        ),
                        tabPanel("Variable Importance Ranking", br(),
                          div(class = "tl-note",
                              tags$i(class = "fa fa-info-circle"),
                              " Ranked table of feature importance scores."),
                          tableOutput(ns("feature_importance_table_ranked"))
                        )
                      )
                    ),

                    tabPanel("SHAP Values", br(),
                      div(class = "tl-note",
                          tags$i(class = "fa fa-info-circle"),
                          " SHAP values quantify each predictor's contribution to individual predictions (model-agnostic, Monte-Carlo permutation). Computed on up to 100 rows for performance."),
                      fluidRow(
                        column(6, uiOutput(ns("shap_status_ui"))),
                        column(6, actionButton(ns("run_shap"), "Compute SHAP Values",
                                               class = "btn-success", width = "100%"))
                      ),
                      fluidRow(
                        column(12,
                          radioButtons(ns("shap_plot_style"), "Plot style for Variable Importance / Beeswarm / Waterfall / Force",
                                       choices = c("Default (shared with Train/Test Model pipeline)" = "rautoml",
                                                   "Custom (this app's own colours)" = "custom"),
                                       selected = "rautoml", inline = TRUE),
                          uiOutput(ns("shap_rautoml_note"))
                        )
                      ),
                      br(),
                      tabsetPanel(
                        tabPanel("SHAP-Based Variable Importance", br(),
                          conditionalPanel(condition = sprintf("output['%s'] === true", ns("shap_ready")),
                            div(class = "tl-download-bar",
                                downloadButton(ns("dl_shap_plot"), "Download PNG", icon = icon("download")))),
                          plotOutput(ns("shap_summary_plot"), height = 380)
                        ),
                        tabPanel("Beeswarm Plot", br(),
                          div(class = "tl-note",
                              tags$i(class = "fa fa-info-circle"),
                              " Each dot is one observation. Horizontal position = SHAP value; colour = normalised feature value (dark = high, light = low)."),
                          conditionalPanel(condition = sprintf("output['%s'] === true", ns("shap_ready")),
                            div(class = "tl-download-bar",
                                downloadButton(ns("dl_shap_beeswarm"), "Download PNG", icon = icon("download")))),
                          plotOutput(ns("shap_beeswarm_plot"), height = 420)
                        ),
                        tabPanel("Dependency Plot", br(),
                          div(class = "tl-note",
                              tags$i(class = "fa fa-info-circle"),
                              " Shows how a single feature's value affects the model output (SHAP value). Select a numeric feature below."),
                          uiOutput(ns("shap_dep_feature_ui")),
                          conditionalPanel(condition = sprintf("output['%s'] === true", ns("shap_ready")),
                            div(class = "tl-download-bar",
                                downloadButton(ns("dl_shap_dependency"), "Download PNG", icon = icon("download")))),
                          plotOutput(ns("shap_dependency_plot"), height = 380)
                        ),
                        tabPanel("Waterfall Plot", br(),
                          div(class = "tl-note",
                              tags$i(class = "fa fa-info-circle"),
                              " Shows the SHAP contributions for a single observation (top 15 features by absolute contribution)."),
                          conditionalPanel(condition = sprintf("input['%s'] == 'custom'", ns("shap_plot_style")),
                            div(class = "tl-note", "Use the slider to select an observation."),
                            uiOutput(ns("shap_obs_slider_ui"))),
                          conditionalPanel(condition = sprintf("input['%s'] == 'rautoml'", ns("shap_plot_style")),
                            div(class = "tl-note", "Default style shows a fixed representative observation (matches the Train/Test Model pipeline; no observation picker).")),
                          conditionalPanel(condition = sprintf("output['%s'] === true", ns("shap_ready")),
                            div(class = "tl-download-bar",
                                downloadButton(ns("dl_shap_waterfall"), "Download PNG", icon = icon("download")))),
                          plotOutput(ns("shap_waterfall_plot"), height = 400)
                        ),
                        tabPanel("Force Plot", br(),
                          div(class = "tl-note",
                              tags$i(class = "fa fa-info-circle"),
                              " Shows the top features pushing the prediction up or down for a single observation."),
                          conditionalPanel(condition = sprintf("input['%s'] == 'custom'", ns("shap_plot_style")),
                            div(class = "tl-note", "Use the slider to select an observation."),
                            uiOutput(ns("shap_obs_slider_ui2"))),
                          conditionalPanel(condition = sprintf("input['%s'] == 'rautoml'", ns("shap_plot_style")),
                            div(class = "tl-note", "Default style shows a fixed representative observation (matches the Train/Test Model pipeline; no observation picker).")),
                          conditionalPanel(condition = sprintf("output['%s'] === true", ns("shap_ready")),
                            div(class = "tl-download-bar",
                                downloadButton(ns("dl_shap_force"), "Download PNG", icon = icon("download")))),
                          plotOutput(ns("shap_force_plot"), height = 400)
                        )
                      )
                    ),

                    tabPanel("Threshold Optimisation", br(),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                        div(class = "tl-note",
                            tags$i(class = "fa fa-info-circle"),
                            " Move the classification threshold slider in Step 1 to update the vertical line."),
                        conditionalPanel(condition = sprintf("output['%s'] === true", ns("source_ready")),
                          div(class = "tl-download-bar",
                              downloadButton(ns("dl_threshold_plot"), "Download PNG", icon = icon("download")))),
                        plotOutput(ns("threshold_plot"), height = 380)
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'regression'", ns("outcome_type")),
                        div(class = "alert alert-info", tags$i(class = "fa fa-info-circle"),
                            " Threshold optimisation is only available for binary classification problems.")
                      )
                    ),

                    tabPanel("Subgroup Performance", br(),
                      div(class = "tl-note",
                          tags$i(class = "fa fa-info-circle"),
                          " Select a grouping variable to see per-subgroup performance. Classification: AUC / sensitivity / specificity. Regression: RMSE / MAE."),
                      fluidRow(
                        column(6, uiOutput(ns("subgroup_var_ui"))),
                        column(6, br(),
                               actionButton(ns("run_subgroup"), "Run Subgroup Analysis",
                                            class = "btn-success", width = "100%"))
                      ),
                      br(),
                      fluidRow(
                        column(6,
                          conditionalPanel(condition = sprintf("output['%s'] === true", ns("subgroup_ready")),
                            div(class = "tl-download-bar",
                                downloadButton(ns("dl_subgroup_table"), "Download CSV", icon = icon("download")))),
                          tableOutput(ns("subgroup_table"))
                        ),
                        column(6,
                          conditionalPanel(condition = sprintf("output['%s'] === true", ns("subgroup_ready")),
                            div(class = "tl-download-bar",
                                downloadButton(ns("dl_subgroup_plot"), "Download PNG", icon = icon("download")))),
                          plotOutput(ns("subgroup_plot"), height = 330)
                        )
                      )
                    )
                  )
                )
              )
            ))
          ),

          # ==================================================================
          # Step 4: Comparison
          # ==================================================================
          tabPanel("Step 4: Comparison", br(),

            # Redirect to ML
            fluidRow(column(12,
              div(class = "tl-box",
                div(class = "tl-header", "Retrain Through Main Machine Learning Workflow"),
                div(class = "tl-body",
                  p("Retrain using the existing Machine Learning workflow. After training, return here to compare."),
                  actionButton(ns("go_to_initialize"), "Go to Machine Learning Initialize", class = "btn-success"),
                  actionButton(ns("refresh_retrained_models"), "Refresh Retrained Models",  class = "btn-info")
                )
              )
            )),

            # Model selection table
            fluidRow(column(12,
              div(class = "tl-box",
                div(class = "tl-header", "Select Retrained Models for Comparison"),
                div(class = "tl-body",
                  fluidRow(
                    column(6, uiOutput(ns("comparison_metric_picker"))),
                    column(6, uiOutput(ns("comparison_session_filter")))
                  ),
                  br(),
                  DT::dataTableOutput(ns("comparison_model_table")), br(),
                  uiOutput(ns("compare_scoring_status_ui")), br(),
                  uiOutput(ns("compare_scoring_errors_ui")), br(),
                  actionButton(ns("compare_selected_models"), "Compare Selected Models",
                               class = "btn-success", width = "100%")
                )
              )
            )),

            # IMPROVEMENT 5: Fine-tuning — collapsible, hidden until switched on, then gated behind a small-dataset question
            fluidRow(column(12,
              tl_collapsible_box(
                tagList(tags$i(class = "fa fa-magic"), " Fine-tuning / Domain Adaptation"),
                open = TRUE,
                shinyWidgets::materialSwitch(
                  inputId = ns("finetune_enabled"),
                  label   = "Enable Fine-tuning / Domain Adaptation",
                  status  = "success", value = FALSE, right = TRUE
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns("finetune_enabled")),
                  tagList(
                    div(class = "tl-note",
                        tags$i(class = "fa fa-info-circle"),
                        " Fine-tuning is a lightweight alternative to full retraining, intended for small target datasets where retraining from scratch is unreliable. If your dataset is large enough to retrain, use the model comparison section above instead."),
                    radioButtons(ns("dataset_size_check"),
                                 "Is your target dataset small (e.g. fewer than ~200 rows)?",
                                 choices = c("Yes" = "yes", "No" = "no"),
                                 selected = character(0), inline = TRUE),

                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'no'", ns("dataset_size_check")),
                      div(class = "alert alert-info",
                          tags$i(class = "fa fa-info-circle"),
                          " Fine-tuning is hidden because your dataset is not small. Use ",
                          strong("Compare Selected Models"), " above to retrain a full model instead.")
                    ),

                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'yes'", ns("dataset_size_check")),
                      tagList(
                        br(),
                        fluidRow(
                          column(6,
                            radioButtons(ns("finetune_mode"), "Adaptation mode",
                                         choices = c("Probability recalibration only" = "prob_only",
                                                     "Full adaptation (prob + all features)" = "full"),
                                         selected = "prob_only")
                          ),
                          column(6, br(), br(),
                            uiOutput(ns("finetune_status_ui")),
                            actionButton(ns("run_finetune"), "Run Fine-tuning",
                                         class = "btn-success", width = "100%")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(6,
                            conditionalPanel(condition = sprintf("output['%s'] === true", ns("finetune_ready")),
                              div(class = "tl-download-bar",
                                  downloadButton(ns("dl_finetune_metrics"), "Download CSV", icon = icon("download")))),
                            tableOutput(ns("finetune_metrics_table"))
                          ),
                          column(6,
                            conditionalPanel(condition = sprintf("output['%s'] === true", ns("finetune_ready")),
                              div(class = "tl-download-bar",
                                  downloadButton(ns("dl_finetune_barplot"), "Download PNG", icon = icon("download")))),
                            plotOutput(ns("finetune_barplot"), height = 330)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )),

            # Comparison metrics — only appears once retraining/comparison has been run
            conditionalPanel(
              condition = sprintf("output['%s'] === true", ns("compare_ready")),
              fluidRow(column(12,
                tl_collapsible_box(
                  "Performance Metrics Comparison (with 95% Bootstrap CI)",
                  open = TRUE,
                  div(class = "tl-download-bar",
                      downloadButton(ns("dl_comparison_metrics"), "Download CSV", icon = icon("download"))),
                  textOutput(ns("comparison_note")), br(),
                  tableOutput(ns("source_vs_retrained_metrics"))
                )
              ))
            ),

            # Comparison plots — only appears once retraining/comparison has been run
            conditionalPanel(
              condition = sprintf("output['%s'] === true", ns("compare_ready")),
              fluidRow(column(12,
                tl_collapsible_box(
                  "Comparison Plots",
                  open = TRUE,
                  tabsetPanel(
                    id = ns("comparison_plots_tabs"), type = "tabs",
                    tabPanel("ROC Curves", br(),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                        div(class = "tl-download-bar",
                            downloadButton(ns("dl_comparison_roc"), "Download PNG", icon = icon("download"))),
                        plotOutput(ns("source_vs_retrained_roc_plot"), height = 430)
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'regression'", ns("outcome_type")),
                        div(class = "alert alert-info", tags$i(class = "fa fa-info-circle"),
                            " ROC Curves are only available for binary classification problems.")
                      )
                    ),
                    tabPanel("Metrics Bar Chart", br(),
                      div(class = "tl-download-bar",
                          downloadButton(ns("dl_comparison_barplot"), "Download PNG", icon = icon("download"))),
                      plotOutput(ns("source_vs_retrained_barplot"), height = 380)
                    ),
                    tabPanel("Calibration Comparison", br(),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                        div(class = "tl-note",
                            tags$i(class = "fa fa-info-circle"),
                            " A well-calibrated model lies close to the diagonal. Deviation indicates over- or under-confidence."),
                        div(class = "tl-download-bar",
                            downloadButton(ns("dl_comparison_calibration"), "Download PNG", icon = icon("download"))),
                        plotOutput(ns("comparison_calibration_plot"), height = 430)
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'regression'", ns("outcome_type")),
                        div(class = "alert alert-info", tags$i(class = "fa fa-info-circle"),
                            " Calibration Comparison is only available for binary classification problems.")
                      )
                    ),
                    tabPanel("Residuals Comparison", br(),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'regression'", ns("outcome_type")),
                        div(class = "tl-note", tags$i(class = "fa fa-info-circle"),
                            " Residuals (actual − predicted) vs predicted, source model vs each selected retrained model."),
                        div(class = "tl-download-bar",
                            downloadButton(ns("dl_comparison_residuals"), "Download PNG", icon = icon("download"))),
                        plotOutput(ns("comparison_residuals_plot"), height = 430)
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'classification'", ns("outcome_type")),
                        div(class = "alert alert-info", tags$i(class = "fa fa-info-circle"),
                            " Residuals Comparison is only available for regression problems.")
                      )
                    )
                  )
                )
              ))
            )
          ),

          # ==================================================================
          # Step 5: Export Report (IMPROVEMENT 7)
          # ==================================================================
          tabPanel("Step 5: Export Report", br(),

            fluidRow(column(12,
              div(class = "tl-box",
                div(class = "tl-header",
                    tags$i(class = "fa fa-file-alt"), " Generate Transfer Learning Report"),
                div(class = "tl-body",
                  div(class = "tl-note",
                      tags$i(class = "fa fa-info-circle"),
                      " Select the sections to include in your HTML report. Only sections where results have been computed will be rendered."),
                  br(),
                  checkboxGroupInput(
                    ns("report_sections"),
                    "Sections to include:",
                    choices = c(
                      "Schema compatibility check"    = "schema",
                      "Distribution shift (PSI)"      = "shift",
                      "Source model performance"      = "source",
                      "Model comparison"              = "compare",
                      "Fine-tuning results"           = "finetune",
                      "Subgroup performance"          = "subgroup"
                    ),
                    selected = c("schema", "shift", "source", "compare"),
                    inline   = FALSE
                  ),
                  br(),
                  div(class = "tl-download-bar",
                      downloadButton(ns("dl_transfer_report"), "Generate & Download HTML Report",
                                     icon = icon("file-download")))
                )
              )
            ))
          )

        ) # end tabsetPanel
      )   # end tagList
    )     # end conditionalPanel
  )       # end tabItem
}
