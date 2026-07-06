train_all_model_ui = function() {
	tabItem(tabName = "trainModel",
		tags$head(
			tags$style(HTML("
				/* ---- Prevent Shiny recalculating opacity on training outputs ---- */
				#caret_job_queue_panel.recalculating,
				#caret_job_queue_panel .recalculating,
				#model_training_caret_models_ui.recalculating,
				#model_training_apply.recalculating,
				#model_training_caret_train_metrics.recalculating {
					opacity: 1 !important;
				}

				/* ---- Model Training Progress Panel ---- */
				#caret-model-progress {
					display: none;
					margin: 0 15px 18px 15px;
					border: 1px solid #7bc148;
					border-radius: 3px;
					background: #fff;
					overflow: hidden;
				}
				#caret-model-progress.cmp-visible {
					display: block;
				}
				.cmp-header {
					padding: 10px 15px;
					display: flex;
					justify-content: space-between;
					align-items: center;
					min-height: 44px;
					background: #bde0a3;
					border-bottom: 1px solid #7bc148;
				}
				.cmp-title {
					font-size: 15px;
					font-weight: 700;
					color: #1a3a1a;
				}
				.cmp-counts {
					font-size: 13px;
					color: #555;
					display: flex;
					align-items: center;
					gap: 10px;
					min-height: 24px;
					white-space: nowrap;
				}
				.cmp-count-dot {
					display: inline-block;
					width: 8px;
					height: 8px;
					border-radius: 50%;
					margin-right: 3px;
					vertical-align: middle;
				}
				.cmp-list {
					padding: 0 15px 2px;
				}
				.cmp-row {
					display: grid;
					grid-template-columns: 28px minmax(180px, 260px) 170px minmax(560px, 1fr) 170px;
					align-items: center;
					gap: 12px;
					padding: 7px 0;
					min-height: 43px;
					border-bottom: 1px solid #f2f7ec;
				}
				.cmp-row:last-child {
					border-bottom: none;
				}
				.cmp-num {
					font-size: 13px;
					color: #aaa;
					text-align: right;
					font-weight: 500;
				}
				.cmp-name {
					font-size: 14px;
					font-weight: 500;
					color: #1a1a1a;
					overflow: hidden;
					text-overflow: ellipsis;
					white-space: nowrap;
				}
				.cmp-status {
					font-size: 13px;
					display: flex;
					align-items: center;
					gap: 6px;
					color: #444;
					min-width: 0;
				}
				.cmp-sdot {
					display: inline-block;
					width: 8px;
					height: 8px;
					border-radius: 50%;
					background: #2196f3;
					flex-shrink: 0;
				}
				.cmp-steps {
					display: grid;
					grid-template-columns: repeat(5, minmax(90px, 1fr));
					gap: 6px;
					align-items: center;
					min-width: 560px;
				}
				.cmp-step {
					display: flex;
					align-items: center;
					gap: 5px;
					min-width: 0;
					color: #9aa3a8;
					font-size: 12px;
					white-space: nowrap;
				}
				.cmp-step-icon {
					width: 18px;
					height: 18px;
					border-radius: 50%;
					border: 1px solid #c7d1d5;
					display: inline-flex;
					align-items: center;
					justify-content: center;
					flex-shrink: 0;
					background: #fff;
					font-size: 10px;
				}
				.cmp-step-label {
					overflow: hidden;
					text-overflow: ellipsis;
				}
				.cmp-step.is-done {
					color: #3f8f3f;
				}
				.cmp-step.is-done .cmp-step-icon {
					background: #4cae4c;
					border-color: #4cae4c;
					color: #fff;
				}
				.cmp-step.is-current {
					color: #1f5f8f;
					font-weight: 700;
				}
				.cmp-step.is-current .cmp-step-icon {
					background: #2196f3;
					border-color: #2196f3;
					color: #fff;
				}
				.cmp-step.is-paused {
					color: #a66b00;
				}
				.cmp-step.is-paused .cmp-step-icon {
					background: #f6a100;
					border-color: #f6a100;
					color: #fff;
				}
				.cmp-step.is-stopped {
					color: #555;
				}
				.cmp-step.is-stopped .cmp-step-icon {
					background: #777;
					border-color: #777;
					color: #fff;
				}
				.cmp-step.is-failed {
					color: #a94442;
				}
				.cmp-step.is-failed .cmp-step-icon {
					background: #d9534f;
					border-color: #d9534f;
					color: #fff;
				}
				.cmp-action {
					display: flex;
					gap: 5px;
					justify-content: flex-start;
					flex-wrap: wrap;
					min-width: 150px;
					min-height: 28px;
				}
				.cmp-action .btn {
					min-width: 46px;
				}
				.cmp-footer {
					padding: 6px 15px;
					font-size: 12px;
					color: #888;
					border-top: 1px solid #e6f0d8;
					background: #fff;
				}
				"))
			),
			fluidRow(
				conditionalPanel(
					condition = "input.modelling_framework_choices == 'Caret'",
					column(width = 12,
						uiOutput("caret_job_queue_panel")
					),
				column(width = 12,
					uiOutput("model_training_setup_presetup")
				),
				column(width = 12,
					uiOutput("model_training_caret_models_ui")
				),
				column(width = 12,
					uiOutput("model_training_caret_train_metrics")
				)
			),
			conditionalPanel(
				condition = "input.modelling_framework_choices == 'Pycaret'",
				column(width = 12,
					train_model_ui("train_model")
				)
			)
		)
	)
}
