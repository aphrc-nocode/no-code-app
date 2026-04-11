train_all_model_ui = function() {
	tabItem(tabName = "trainModel",
		tags$head(
			tags$style(HTML("
				#caret-training-status {
					display: none;
					margin: 0 0 16px 0;
					padding: 12px 14px;
					border-radius: 4px;
					border: 1px solid #d9e5d2;
					background: #ffffff;
					box-shadow: none;
				}
				#caret-training-status.is-running {
					display: block;
					border-color: #b9d98f;
				}
				#caret-training-status.is-success {
					display: block;
					border-color: #7bc148;
				}
				#caret-training-status.is-error {
					display: block;
					border-color: #d9534f;
					background: #ffffff;
				}
				.caret-training-status__inner {
					display: grid;
					grid-template-columns: minmax(0, 1fr) auto;
					gap: 12px;
					align-items: center;
				}
				.caret-training-status__text {
					font-size: 14px;
					font-weight: 600;
					color: #23412c;
				}
				.caret-training-status__meta {
					font-size: 12px;
					color: #5d6d5f;
					margin-top: 4px;
				}
				.caret-training-status__badge {
					padding: 5px 10px;
					border: 1px solid #9ccf63;
					border-radius: 4px;
					font-size: 11px;
					font-weight: 700;
					letter-spacing: 0.08em;
					color: #2d5a17;
					background: #f4faec;
					text-transform: uppercase;
				}
				.caret-training-status__bar {
					margin-top: 10px;
					height: 6px;
					border-radius: 2px;
					background: rgba(123, 193, 72, 0.14);
					overflow: hidden;
				}
				.caret-training-status__bar-fill {
					width: 42%;
					height: 100%;
					background: repeating-linear-gradient(
						-45deg,
						#7bc148,
						#7bc148 12px,
						#a8d56f 12px,
						#a8d56f 24px
					);
					animation: caretTrainingSlide 1s linear infinite;
				}
				#caret-training-status.is-success .caret-training-status__badge {
					border-color: #6eb15c;
					color: #245a24;
					background: #f1f8ee;
				}
				#caret-training-status.is-success .caret-training-status__bar-fill {
					width: 100%;
					animation: none;
					background: #4cae4c;
				}
				#caret-training-status.is-error .caret-training-status__badge {
					border-color: #d9534f;
					color: #8b2f2b;
					background: #fff3f2;
				}
				#caret-training-status.is-error .caret-training-status__bar-fill {
					width: 100%;
					animation: none;
					background: #d9534f;
				}
				@keyframes caretTrainingSlide {
					0% { transform: translateX(-18px); }
					100% { transform: translateX(18px); }
				}
			")),
			tags$script(HTML("
				(function () {
					function setTrainingStatus(state, text, meta) {
						var panel = document.getElementById('caret-training-status');
						if (!panel) return;
						panel.classList.remove('is-running', 'is-success', 'is-error');
						if (!state || state === 'hidden') {
							panel.style.display = 'none';
							return;
						}
						panel.style.display = 'block';
						panel.classList.add('is-' + state);
						var textEl = document.getElementById('caret-training-status-text');
						var metaEl = document.getElementById('caret-training-status-meta');
						var badgeEl = document.getElementById('caret-training-status-badge');
						if (textEl && typeof text === 'string') textEl.textContent = text;
						if (metaEl) metaEl.textContent = meta || '';
						if (badgeEl) {
							badgeEl.textContent = state === 'success' ? 'Done' : (state === 'error' ? 'Stopped' : 'Running');
						}
					}

					document.addEventListener('click', function (e) {
						var btn = e.target.closest && e.target.closest('#model_training_apply');
						if (!btn) return;
						setTrainingStatus('running', 'Model training in progress...', 'Training is running inside this page. You can still view the current screen.');
					});

					Shiny.addCustomMessageHandler('caretTrainingStatus', function (msg) {
						setTrainingStatus(msg && msg.state, msg && msg.text, msg && msg.meta);
					});
				})();
			"))
		),
		fluidRow(
			conditionalPanel(
				condition = "input.modelling_framework_choices == 'Caret'",
				column(width = 12
					, div(
						id = "caret-training-status",
						div(
							class = "caret-training-status__inner",
							div(
								div(id = "caret-training-status-text", class = "caret-training-status__text", "Training models status"),
								div(id = "caret-training-status-meta", class = "caret-training-status__meta", ""),
								div(
									class = "caret-training-status__bar",
									div(class = "caret-training-status__bar-fill")
								)
							),
							div(id = "caret-training-status-badge", class = "caret-training-status__badge", "Running")
						)
					)
				)
				, column(width = 12
					, uiOutput("model_training_setup_presetup")

				)
				, column(width=12
					, uiOutput("model_training_caret_models_ui")
				)
				, column(width=12
					, uiOutput("model_training_caret_train_metrics")
				)
			),
			conditionalPanel(
				condition = "input.modelling_framework_choices == 'Pycaret'",		
				column(width=12
					, train_model_ui("train_model")
				)
			)
		)
	)
}
