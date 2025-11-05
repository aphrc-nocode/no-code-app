omop_visualizations_ui = function() {
	tabItem(
		tabName = "omop_visualizations",

		# ===== Row 1: Schema & Version Selection =====
		fluidRow(
			box(
				title = "Schema & Version Selection"
				, status = "success"
				, solidHeader = TRUE
				, width = 12
				, collapsible = TRUE
				, uiOutput("schema_selection")
				, uiOutput("generate_summaries")
			)
		),

		# ===== Row 2: General Summaries =====
		fluidRow(
			box(
				title = "Summary Dashboard",
				status = "success",
				solidHeader = TRUE,
				width = 12,
				collapsible = TRUE,
				tabsetPanel(
					tabPanel("General Info",
						fluidRow(
							box(title = "CDM Source Info",
								width = 9,
								uiOutput("cdm_source_info")
							)
						)
					),

					tabPanel("Record Counts",
						fluidRow(
							box(title = "CDM Table Record Counts",
								width = 12,
								DTOutput("cdm_table_summaries")
							)
						)
					),

					tabPanel("Unmapped Concepts",
						fluidRow(
							box(title = "Unmapped / Non-standard Concepts",
								width = 12,
								DTOutput("nonstandard_concepts")
							)
						)
					),

					tabPanel("Domain Distribution",
						fluidRow(
							box(title = "Concept Domain Distribution",
								 width = 12,
								 plotlyOutput("domain_distribution")
							)
						)
					)
				)
			)
		),

		# ===== Row 3: Table-specific Summaries =====
		fluidRow(
			column(width = 3,
				box(title = "CDM Table Selection",
				  status = "success",
					solidHeader = TRUE,
					width = 12,
					collapsible = TRUE,
					uiOutput("omop_cdm_tables")
				)
			),

			column(width = 9,
				tabsetPanel(
					tabPanel("Table-specific Analysis",
					
						# Person table analysis----
						conditionalPanel(condition = "input.selected_cdm_table == 'person'",
							fluidRow(
								box(title = "Age Summary",
									status = "info",
									solidHeader = TRUE,
									width = 12,
									tableOutput("age_summary")
								)
							),

							fluidRow(
								box(title = "Gender Distribution",
									status = "primary",
									solidHeader = TRUE,
									width = 6,
									plotlyOutput("gender_plot")
								),
								box(title = "Race Distribution",
									status = "primary",
									solidHeader = TRUE,
									width = 6,
									plotlyOutput("race_plot")
								)
							)
						),

						conditionalPanel(condition = "input.selected_cdm_table == 'location'",
							fluidRow(
								box(title = "Location Summary",
									status = "info",
									solidHeader = TRUE,
									width = 12,
									dataTableOutput("location_table")
								)
							)
						),
						
						# Care site table analysis
						conditionalPanel(condition = "input.selected_cdm_table == 'care_site'",
							fluidRow(
								box(title = "Care Site Summary",
									status = "info",
									solidHeader = TRUE,
									width = 12,
									dataTableOutput("care_table")
								)
							)
						),

						# Visit occurrence table analysis
						conditionalPanel(condition = "input.selected_cdm_table == 'visit_occurrence'",
						  fluidRow(
							 box(title = "Visit Occurrence Summary",
								  status = "info",
									 solidHeader = TRUE,
									 width = 12,
									 dataTableOutput("summary_table")
								  )
							 ),
						  fluidRow(
							 box(title = "Visits Over Time",
								  status = "primary",
									 solidHeader = TRUE,
									 width = 12,
									 plotlyOutput("visit_time_plot")
								  )
							 ),
							fluidRow(
								box(title = "Visits by Type",
									status = "primary",
									solidHeader = TRUE,
									width = 12,
									plotlyOutput("visit_type_plot")
								)
							)
						),

						# Provider table analysis
						conditionalPanel(condition = "input.selected_cdm_table == 'provider'",
							fluidRow(
									box(title = "Provider Summary",
									status = "info",
									solidHeader = TRUE,
									width = 12,
									dataTableOutput("summary_table_FIXME_NOW")
								)
							)
						),

						# Observation table analysis
						conditionalPanel(condition = "input.selected_cdm_table == 'observation'",
							fluidRow(
								box(title = "Observation Concepts",
									status = "info",
									solidHeader = TRUE,
									width = 12,
									dataTableOutput("observation_table")
								)
							),
							fluidRow(
								box(title = "Value as Concept Distribution",
									status = "primary",
									solidHeader = TRUE,
									width = 12,
									uiOutput("observation_value_ui")
								)
							)
						),

						# Measurement table analysis
						conditionalPanel(condition = "input.selected_cdm_table == 'measurement'",
							fluidRow(
								box(title = "Measurement Concepts",
									status = "info",
									solidHeader = TRUE,
									width = 12,
									dataTableOutput("measurement_table")
								)
							),
							fluidRow(
								box(title = "Value as Concept Distribution",
									status = "primary",
									solidHeader = TRUE,
									width = 12,
									uiOutput("measurement_value_ui")
								)
							)
						),

						# Condition occurrence table analysis
						conditionalPanel(condition = "input.selected_cdm_table == 'condition_occurrence'",
							fluidRow(
								box(title = "Condition Concepts",
									status = "info",
									solidHeader = TRUE,
									width = 12,

									# Gender filter
									fluidRow(
										column(width = 4,
											uiOutput("gender_filter_ui")
										)
									),
									dataTableOutput("condition_table")
								)
							),
							fluidRow(
								box(title = "Value as Concept Distribution",
									status = "primary",
									solidHeader = TRUE,
									width = 12,
									uiOutput("condition_value_ui")
								)
							)
						)
					)
				)
			)
		)
	)
}
