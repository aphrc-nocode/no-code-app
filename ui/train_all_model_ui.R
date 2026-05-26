train_all_model_ui = function() {
	tabItem(tabName = "trainModel",
		tags$head(
			tags$style(HTML("
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
					grid-template-columns: 26px minmax(160px, 220px) 130px 1fr 55px 80px;
					align-items: center;
					gap: 12px;
					padding: 7px 0;
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
				}
				.cmp-status {
					font-size: 13px;
					display: flex;
					align-items: center;
					gap: 6px;
					color: #444;
				}
				.cmp-sdot {
					display: inline-block;
					width: 8px;
					height: 8px;
					border-radius: 50%;
					background: #2196f3;
					flex-shrink: 0;
				}
				.cmp-bar-wrap {
					height: 8px;
					background: #e5eedd;
					border-radius: 4px;
					overflow: hidden;
				}
				.cmp-bar {
					height: 100%;
					border-radius: 4px;
					background: #7bc148;
					width: 0;
					background-size: 28px 28px;
					transition: width 0.4s ease;
				}
				.cmp-bar.cmp-running {
					width: 70%;
					background-image: repeating-linear-gradient(
						-45deg,
						#7bc148 0, #7bc148 10px,
						#a3d46e 10px, #a3d46e 20px
					);
					animation: cmpSlide 0.9s linear infinite;
				}
				.cmp-bar.cmp-done {
					width: 100% !important;
					background-image: none;
					background: #4cae4c;
					animation: none;
				}
				.cmp-bar.cmp-error {
					width: 100% !important;
					background-image: none;
					background: #d9534f;
					animation: none;
				}
				@keyframes cmpSlide {
					from { background-position: 0 0; }
					to   { background-position: 28px 0; }
				}
				.cmp-pct {
					font-size: 13px;
					color: #666;
					text-align: right;
				}
				.cmp-badge-done {
					padding: 3px 8px;
					border: 1px solid #6eb15c;
					border-radius: 4px;
					font-size: 11px;
					font-weight: 700;
					color: #245a24;
					background: #f1f8ee;
					letter-spacing: 0.04em;
					display: inline-block;
					cursor: pointer;
					transition: background 0.15s, color 0.15s;
				}
				.cmp-badge-done:hover {
					background: #6eb15c;
					color: #fff;
				}
				.cmp-badge-error {
					padding: 3px 8px;
					border: 1px solid #d9534f;
					border-radius: 4px;
					font-size: 11px;
					font-weight: 700;
					color: #8b2f2b;
					background: #fff3f2;
					letter-spacing: 0.04em;
					display: inline-block;
					cursor: pointer;
					transition: background 0.15s, color 0.15s;
				}
				.cmp-badge-error:hover {
					background: #d9534f;
					color: #fff;
				}
				.cmp-footer {
					padding: 6px 15px;
					font-size: 12px;
					color: #888;
					border-top: 1px solid #e6f0d8;
					background: #fff;
				}
			")),
			tags$script(HTML("
				(function () {
					// Read a label from the hidden labels div (falls back to key if not found)
					function lbl(key) {
						var el = document.getElementById('cmp-labels-data');
						return el ? (el.getAttribute('data-' + key) || key) : key;
					}

					function cmpInit(models) {
						var panel = document.getElementById('caret-model-progress');
						if (!panel || !models || !models.length) return;

						var list = panel.querySelector('.cmp-list');
						list.innerHTML = '';

						models.forEach(function (m, i) {
							var row = document.createElement('div');
							row.className = 'cmp-row';
							row.innerHTML =
								'<div class=\"cmp-num\">' + (i + 1) + '.</div>' +
								'<div class=\"cmp-name\">' + (m.name || '') + '</div>' +
								'<div class=\"cmp-status\"><span class=\"cmp-sdot\"></span><span class=\"cmp-slabel\">' + lbl('status-training') + '</span></div>' +
								'<div class=\"cmp-bar-wrap\"><div class=\"cmp-bar cmp-running\"></div></div>' +
								'<div class=\"cmp-pct cmp-pct-val\">&mdash;</div>' +
								'<div class=\"cmp-action\"></div>';
							list.appendChild(row);
						});

						panel.querySelector('#cmp-count-training').textContent = models.length + ' ' + lbl('word-training');
						panel.querySelector('#cmp-count-done').textContent = '0 ' + lbl('word-completed');
						panel.classList.add('cmp-visible');
					}

					function cmpComplete(success) {
						var panel = document.getElementById('caret-model-progress');
						if (!panel) return;

						var rows = panel.querySelectorAll('.cmp-row');
						var n = rows.length;

						rows.forEach(function (row) {
							var bar    = row.querySelector('.cmp-bar');
							var sdot   = row.querySelector('.cmp-sdot');
							var slabel = row.querySelector('.cmp-slabel');
							var pct    = row.querySelector('.cmp-pct-val');
							var action = row.querySelector('.cmp-action');

							bar.classList.remove('cmp-running');

							if (success) {
								bar.classList.add('cmp-done');
								sdot.style.background = '#4cae4c';
								slabel.textContent    = lbl('status-completed');
								pct.textContent       = '100%';
								action.innerHTML      = '<div class=\"cmp-badge-done\">' + lbl('badge-done') + '</div>';
							} else {
								bar.classList.add('cmp-error');
								sdot.style.background = '#d9534f';
								slabel.textContent    = lbl('status-stopped');
								pct.textContent       = '';
								action.innerHTML      = '<div class=\"cmp-badge-error\">' + lbl('badge-failed') + '</div>';
							}
						});

						if (success) {
							panel.querySelector('#cmp-count-training').textContent = '0 ' + lbl('word-training');
							panel.querySelector('#cmp-count-done').textContent     = n + ' ' + lbl('word-completed');
						}
					}

					// On button click: read pre-computed model list from hidden div and show panel
					document.addEventListener('click', function (e) {
						var btn = e.target.closest && e.target.closest('#model_training_apply');
						if (!btn) return;
						var dataEl = document.getElementById('caret-selected-models-data');
						if (!dataEl) return;
						var models = [];
						try { models = JSON.parse(dataEl.getAttribute('data-models') || '[]'); } catch (ex) {}
						cmpInit(models);
					});

					Shiny.addCustomMessageHandler('caretModelProgressComplete', function (msg) {
						cmpComplete(msg && msg.success === true);
					});

					// Click DONE or FAILED badge → dismiss that row, hide panel if empty
					function dismissBadgeRow(selector) {
						document.addEventListener('click', function (e) {
							var badge = e.target.closest && e.target.closest(selector);
							if (!badge) return;
							var panel = document.getElementById('caret-model-progress');
							if (!panel) return;
							var row = badge.closest('.cmp-row');
							if (row) row.remove();
							var remaining = panel.querySelectorAll('.cmp-row').length;
							if (remaining === 0) {
								panel.classList.remove('cmp-visible');
							} else {
								panel.querySelector('#cmp-count-done').textContent = remaining + ' ' + lbl('word-completed');
							}
						});
					}
					dismissBadgeRow('.cmp-badge-done');
					dismissBadgeRow('.cmp-badge-error');
				})();
			"))
		),
		fluidRow(
			conditionalPanel(
				condition = "input.modelling_framework_choices == 'Caret'",
				column(width = 12,
					uiOutput("caret_selected_models_json"),
					uiOutput("cmp_labels_json")
				),
				column(width = 12,
					div(
						id = "caret-model-progress",
						div(class = "cmp-header",
							div(class = "cmp-title", uiOutput("cmp_panel_title_ui")),
							div(class = "cmp-counts",
								tags$span(
									tags$span(class = "cmp-count-dot", style = "background:#2196f3;"),
									tags$span(id = "cmp-count-training", "0 training")
								),
								tags$span("•"),
								tags$span(
									tags$span(class = "cmp-count-dot", style = "background:#4cae4c;"),
									tags$span(id = "cmp-count-done", "0 completed")
								)
							)
						),
						div(class = "cmp-list"),
						div(class = "cmp-footer", uiOutput("cmp_footer_ui"))
					)
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
