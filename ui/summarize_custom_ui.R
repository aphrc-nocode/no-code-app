summarize_custom_ui = function() {
	tabItem(tabName = "summarizeCustom",
	        fluidRow(
	          column(width = 2,
	                 htmlOutput("visualize_data_title")
	                 , uiOutput("user_output_type"),
	                 
	                 div(id = "tabOutputs",
	                     uiOutput("user_tab_options"),
	                     br(),
	                     uiOutput("user_calc_var"),
	                     uiOutput("user_row_var"),
	                     #uiOutput("user_strata_var"),
	                     br(),
	                     uiOutput("usr_create_cross_tab"),
	                     br(),
	                     uiOutput("user_download_table")
	                 ),
	                 
	                 div(
	                   id = "graphOutputs",
	                   uiOutput("user_plot_options"),
	                   uiOutput("user_select_variable_on_x_axis"),
	                   uiOutput("user_select_variable_on_y_axis"),
	                   uiOutput("user_plot_title"),
	                   uiOutput("user_x_axis_label"),
	                   uiOutput("user_y_axis_label"),
	                   br(),
	                   uiOutput("user_create"),
	                   br(),
	                   br(),
	                   uiOutput("user_download")
	                 ),align = "left"
	                 
	          ),
	          column(
	            width = 8,
	            uiOutput("user_chart_type"),
	            uiOutput("tabSummaries"),
	            plotOutput("GeneratedPlot", height = "65vh"),
	            align = "center"),
	          column(
	            width = 2,
	            uiOutput("user_tab_more_out"),
	            uiOutput("user_graph_more_out"),
	            div(id = "tabmoreoption",
	                uiOutput("user_table_options"),
	                br(),
	                uiOutput("user_report_numeric"),
	                uiOutput("user_add_p_value"),
	                uiOutput("user_add_confidence_interval"),
	                uiOutput("user_drop_missing_values"),
	                uiOutput("user_numeric_summary"),
	                uiOutput("user_table_caption")
	            ),
	            
	            div(id = "graphmoreoption",
	                uiOutput("user_more_plot_options"),
	                uiOutput("user_transform_to_doughnut"),
	                uiOutput("user_select_color_variable"),
	                uiOutput("user_select_group_variable"),
	                uiOutput("user_ggthemes"),
	                uiOutput("user_visual_orientation"),
	                uiOutput("user_bar_width"),
	                uiOutput("user_line_size"),
	                uiOutput("user_select_line_type"),
	                uiOutput("user_add_shapes"),
	                uiOutput("user_select_shape"),
	                uiOutput("user_add_smooth"),
	                uiOutput("user_display_confidence_interval"),
	                uiOutput("user_level_of_confidence_interval"),
	                
	                uiOutput("user_select_line_join"),
	                uiOutput("user_add_line_type"),
	                
	                uiOutput("user_add_points"),
	                uiOutput("user_y_variable_summary_type"),
	                uiOutput("user_title_position"),
	                uiOutput("user_size_of_plot_title"),
	                uiOutput("user_axis_title_size"),
	                uiOutput("user_facet_title_size"),
	                uiOutput("user_axis_text_size"),
	                uiOutput("user_data_label_size"),
	                uiOutput("user_x_axis_text_angle"),
	                uiOutput("user_legend_title"),
	                uiOutput("user_stacked"),
	                uiOutput("user_add_density"),
	                uiOutput("user_remove_histogram"),
	                uiOutput("user_select_color_variable_single"),
	                uiOutput("user_select_color_parlet")
	                
	            ),
	            align = "right")
	        ),
	        
	        fluidRow(br(),DT::DTOutput("dfPreview"))
	)
}


