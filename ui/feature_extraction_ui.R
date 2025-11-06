feature_extraction_ui = function() {
	tabItem(
		tabName = "FeatureExtraction",
		fluidPage(
			tags$style(
				HTML("
					table thead tr th {
					background-color: #4CAF50;
					color: white;
					}
					.btn-success {
					background-color: #228B22 !important;
					border-color: #228B22 !important;
					color: white !important;
					font-weight: bold;
					}
				")
			),

			# --- PAGE TITLE ---
			titlePanel("Feature Extraction Tool"),

			# --- "Click to connect" message just below the title ---
			conditionalPanel(
			  condition = "output.dbConnected == false",
			  uiOutput("schema_feature")  # this renders the button if not connected
			),

			# --- MAIN CONTENT (shown only when connected) ---
			conditionalPanel(
				condition = "output.dbConnected == true",
				sidebarLayout(
					sidebarPanel(
						uiOutput("cdm_schema_ui"),
						uiOutput("results_schema_ui"),
						uiOutput("cohort_table_ui"),
						uiOutput("domain_choices_ui"),

						textInput("output_csv", "Output CSV Path:",
									 placeholder = "Enter file name, e.g. features.csv"),
						actionButton("extract_features", "Extract Features", class = "btn-success")
					),

					mainPanel(
						h4("Instructions"),
						p("1. Select schema and cohort info (connection already established)."),
						p("2. Run feature extraction and download the CSV."),
						h4("CDM Table Record Summary"),
						# Download button only appears when summary is generated
						conditionalPanel(
						  condition = "output.summaryAvailable == true",
						  downloadButton("download_cdm_summary", "Download CSV", class = "btn-success")
						),
						br(), br(),
						DT::dataTableOutput("domain_summary"),
						verbatimTextOutput("feature_extract_log")
					)
				)
			)
		)
	)
}
