summarize_automatic_ui = function() {
	tabItem(tabName = "summarizeAutomatic",
		fluidRow(
			htmlOutput("visualize_auto_data_title")
		),
		div(id = "DivvisualizationMenu",
			box(
				title = htmlOutput("bivariate_header_label"),
				status = "success",
				solidHeader = TRUE,
				width = 12,
				collapsible = TRUE,
				collapsed = FALSE,
				hr(),
				fluidRow(
					column(
						width = 3,
						uiOutput("user_select_bivariate_outcome"),
						uiOutput("user_select_Bivariate_features"),
						uiOutput("user_select_color_parlet_bivariate"),
						htmlOutput("bivariate_plot_title"),
						uiOutput("user_generatebivriate")
					),
					column(
						width = 9,
						plotOutput("BivariatePlotOutput", height = "600px")
					)
				)
			),
			br(),
			box(
				title = uiOutput("corrplot_header_label"),
				status = "success",
				solidHeader = TRUE,
				collapsible = TRUE,
				collapsed = FALSE,
				width = 12,
				hr(),
				fluidRow(column(
					width = 3,
					uiOutput("user_select_corr_features"),
					uiOutput("user_select_color_parlet_corrplot")),
					column(width=9,
						plotOutput("CorrPlotOutput")
					)
				)
			),
			br(),
			fluidRow(column(width = 3, offset = 9, uiOutput("user_download_autoreport")))
		)
	)
}
