cohort_constructor_ui = function() {
	tabItem(
		tabName = "CohortConstructor",

		# --- STEP 1: CDM REFERENCE CREATION ---
		box(
			title = tagList(icon("server"), "CDM Reference"),
			width = 12,
			status = "success",
			solidHeader = TRUE,
			collapsible = FALSE,  # not collapsible
			uiOutput("schema_cohort"),
			fluidRow(
				column(4, uiOutput("CDMConnName")),
				column(4, uiOutput("CDMSchemaName")),
				column(4, uiOutput("ResultSchemaName"))
			),
			fluidRow(column(3, offset = 9, br(), uiOutput("CreateCDMID"), align = "right"))
		),

		# --- STEP 2: COHORT CREATION (Hidden until CDM reference created) ---
		conditionalPanel(
			condition = "output.cdmCreated == true",
			box(
				title = tagList(icon("users"), "Cohort Creation (The vocabulary table should be under the cdm schema)"),
				width = 12,
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = FALSE,
				fluidRow(
					column(4, uiOutput("ConceptKeyword")),
					column(4, uiOutput("CohortNameID")),
					column(4, uiOutput("CohortDateID"))
				),
				fluidRow(column(4, offset = 8, br(), uiOutput("GenerateCohortID"), br(), align = "right")),
				hr(),
				h4("Cohort Summary"),
				box(
					title = "Cohort Summary Table",
					width = 12,
					status = "success",
					solidHeader = TRUE,
					collapsible = TRUE,
					collapsed = TRUE,
					conditionalPanel(
						condition = "output.summaryAvailable == true",
						downloadButton("download_summary", "Download Summary (CSV)")
					),
					tags$style(HTML("
					#cohort_summary thead th {
					background-color: #28a745;
					color: white;
					text-align: center;
					}
					")),
					DT::dataTableOutput("cohort_summary")
				),
				# --- STEP 3: INTERACTIVE PLOTS ---
				box(
					title = tagList(icon("chart-bar"), "Cohort Plots"),
					width = 12,
					status = "success",
					solidHeader = TRUE,
					collapsible = TRUE,
					collapsed = FALSE,
					conditionalPanel(
						condition = "output.plotsAvailable == true",
						downloadButton("download_plots", "Download Plots (ZIP)")
					),
					fluidRow(
						column(6, plotlyOutput("Gender_plot", height = "300px")),
						column(6, plotlyOutput("age_group_plot", height = "300px"))
					),
					fluidRow(
						column(6, plotlyOutput("Race_plot", height = "300px")),
						column(6, plotlyOutput("Ethnicity_plot", height = "300px"))
					)
				)
			)
		)
	)
}
