
user_defined_server <- function() {
	observeEvent(c(input$manage_data_apply,input$tabs,input$change_language), {
		if (!is.null(rv_current$working_df)) {
			#shinyjs::show("cboOutput")
		  output$user_output_type = user_output_type
		  output$user_chart_type = user_chart_type
		  output$user_tab_options = user_tab_options
		  output$user_calc_var = user_calc_var
		 # output$user_strata_var = user_strata_var
		  output$user_row_var = user_row_var
		  output$usr_create_cross_tab = usr_create_cross_tab
		  output$user_download_table = user_download_table
		  
		  output$user_table_options = user_table_options
		  output$user_report_numeric = user_report_numeric
		  output$user_add_p_value = user_add_p_value
		  output$user_add_confidence_interval = user_add_confidence_interval
		  output$user_drop_missing_values = user_drop_missing_values
		  output$user_table_caption = user_table_caption
		  
		  output$user_plot_options = user_plot_options
		  output$user_select_variable_on_x_axis = user_select_variable_on_x_axis
		  output$user_select_variable_on_y_axis = user_select_variable_on_y_axis
		  output$user_plot_title = user_plot_title
		  output$user_x_axis_label = user_x_axis_label
		  output$user_y_axis_label = user_y_axis_label
		  output$user_create = user_create
		  output$user_download = user_download
		  
		  output$user_more_plot_options = user_more_plot_options
		  output$user_transform_to_doughnut = user_transform_to_doughnut
		  output$user_select_color_variable = user_select_color_variable
		  output$user_select_group_variable = user_select_group_variable
		  output$user_visual_orientation = user_visual_orientation
		  output$user_bar_width = user_bar_width
		  output$user_line_size = user_line_size
		  output$user_select_line_type = user_select_line_type
		  output$user_add_shapes = user_add_shapes
		  
		  output$user_select_shape = user_select_shape
		  output$user_add_smooth = user_add_smooth
		  output$user_display_confidence_interval = user_display_confidence_interval
		  output$user_level_of_confidence_interval = user_level_of_confidence_interval
		  output$user_select_line_join = user_select_line_join
		  output$user_add_line_type = user_add_line_type
		  output$user_add_points = user_add_points
		  output$user_y_variable_summary_type = user_y_variable_summary_type
		  output$user_title_position = user_title_position
		  
		  output$user_size_of_plot_title = user_size_of_plot_title
		  output$user_axis_title_size = user_axis_title_size
		  output$user_facet_title_size = user_facet_title_size
		  output$user_axis_text_size = user_axis_text_size
		  output$user_data_label_size = user_data_label_size
		  output$user_x_axis_text_angle = user_x_axis_text_angle
		  output$user_legend_title = user_legend_title
		  output$user_stacked = user_stacked
		  output$user_add_density = user_add_density
		  output$user_remove_histogram = user_remove_histogram
		  output$user_select_color_variable_single = user_select_color_variable_single
		  output$user_select_color_parlet = user_select_color_parlet
		  output$user_numeric_summary = user_numeric_summary
		  output$user_tab_more_out = user_tab_more_out
		  output$user_graph_more_out = user_graph_more_out
		  updateSwitchInput(session = session , inputId = "tabmore", value = FALSE)
		  updateSwitchInput(session = session , inputId = "graphmore", value = FALSE)
		  updateRadioButtons(session = session, inputId = "cboOutput", selected = "Chart")
		  
		  output$bivariate_header_label = bivariate_header_label
		  output$corrplot_header_label = corrplot_header_label
		  output$user_select_corr_features = user_select_corr_features
		  output$user_select_Bivariate_features = user_select_Bivariate_features
		  
		  # output$user_select_bivariate_single_color = user_select_bivariate_single_color
		  output$user_select_bivariate_outcome = user_select_bivariate_outcome
		  #output$user_select_color_parlet_bivariate = user_select_color_parlet_bivariate
		  output$user_select_color_parlet_corrplot = user_select_color_parlet_corrplot
		  output$bivariate_plot_title = bivariate_plot_title
		  output$corrplot_title = corrplot_title
		  
		} else {
		  output$user_output_type = NULL
		  output$user_chart_type = NULL
		  output$user_tab_options = NULL
		  output$user_calc_var = NULL
		  #output$user_strata_var = NULL
		  output$user_row_var = NULL
		  output$usr_create_cross_tab = NULL
		  output$user_download_table = NULL
		  
		  output$user_table_options = NULL
		  output$user_report_numeric = NULL
		  output$user_add_p_value = NULL
		  output$user_add_confidence_interval = NULL
		  output$user_drop_missing_values = NULL
		  output$user_table_caption = NULL
		  
		  output$user_plot_options = NULL
		  output$user_select_variable_on_x_axis = NULL
		  output$user_select_variable_on_y_axis = NULL
		  output$user_plot_title = NULL
		  output$user_x_axis_label = NULL
		  output$user_y_axis_label = NULL
		  output$user_create = NULL
		  output$user_download = NULL
		  
		  output$user_more_plot_options = NULL
		  output$user_transform_to_doughnut = NULL
		  output$user_select_color_variable = NULL
		  output$user_select_group_variable = NULL
		  output$user_visual_orientation = NULL
		  output$user_bar_width = NULL
		  output$user_line_size = NULL
		  output$user_select_line_type = NULL
		  output$user_add_shapes = NULL
		  
		  output$user_select_shape = NULL
		  output$user_add_smooth = NULL
		  output$user_display_confidence_interval = NULL
		  output$user_level_of_confidence_interval = NULL
		  output$user_select_line_join = NULL
		  output$user_add_line_type = NULL
		  output$user_add_points = NULL
		  output$user_y_variable_summary_type = NULL
		  output$user_title_position = NULL
		  
		  output$user_size_of_plot_title = NULL
		  output$user_axis_title_size = NULL
		  output$user_facet_title_size = NULL
		  output$user_axis_text_size = NULL
		  output$user_data_label_size = NULL
		  output$user_x_axis_text_angle = NULL
		  output$user_legend_title = NULL
		  output$user_stacked = NULL
		  output$user_add_density = NULL
		  output$user_remove_histogram = NULL
		  output$user_select_color_variable_single = NULL
		  output$user_select_color_parlet = NULL
		  output$user_numeric_summary =NULL
		  output$user_tab_more_out = NULL
		  output$user_graph_more_out = NULL
		  
		  output$bivariate_header_label = NULL
		  output$corrplot_header_label = NULL
		  output$user_select_corr_features = NULL
		  output$user_select_Bivariate_features = NULL
		  
		  #output$user_select_bivariate_single_color = NULL
		  output$user_select_bivariate_outcome = NULL
		  #output$user_select_color_parlet_bivariate = NULL
		  output$user_select_color_parlet_corrplot = NULL
		  output$bivariate_plot_title = NULL
		  output$corrplot_title = NULL
		  
		}
	})
  
  
  
  
  ################Automatic visualization
  
  observe({
    req(rv_current$working_df)
    req(input$cboBivariateOutcome)
    
    if (is.numeric(rv_current$working_df[[input$cboBivariateOutcome]])) {
      output$user_select_bivariate_single_color = user_select_bivariate_single_color
      output$user_select_color_parlet_bivariate = NULL
      #shinyjs::show("cboBivariateColor")
      # shinyjs::hide("cboColorBrewerBivariate")
    } else {
      #shinyjs::hide("cboBivariateColor")
      # shinyjs::show("cboColorBrewerBivariate")
      output$user_select_bivariate_single_color = NULL
      output$user_select_color_parlet_bivariate = user_select_color_parlet_bivariate
    }
  })
  
  
  # When features are chosen, update outcome (exclude features)
  observe({
    req(rv_current$working_df)
    req(input$cboBivariateFeatures)
    if(!all(is.null(input$cboBivariateFeatures)) & !all(is.null(input$cboBivariateOutcome)) & any(input$cboBivariateOutcome%in%input$cboBivariateFeatures)){
      other_cols <- setdiff(names(rv_current$working_df), input$cboBivariateFeatures)
      updateSelectInput(session, "cboBivariateOutcome", choices = other_cols, selected = character(0))
    }
    
  })
  
  
  
observe({
    req(rv_current$working_df)
    req(input$cboBivariateOutcome)
    if(!all(is.null(input$cboBivariateFeatures)) & !all(is.null(input$cboBivariateOutcome)) & any(input$cboBivariateOutcome%in%input$cboBivariateFeatures)){
      other_cols <- setdiff(names(rv_current$working_df), input$cboBivariateOutcome)
      updateSelectInput(session, "cboBivariateFeatures", choices = other_cols, selected = character(0))
    }
    
  })
  
  # General population of select inputs
  observe({
    req(rv_current$working_df)
    updateSelectInput(session, "cboCorrFeatures", choices = names(numeric_df(rv_current$working_df)), selected = names(numeric_df(rv_current$working_df)))
    updateSelectInput(session, "cboBivariateFeatures", choices = names(rv_current$working_df), selected = character(0))
    updateSelectInput(session, "cboBivariateOutcome", choices = names(rv_current$working_df), selected = character(0))
  })
  
  # Reactive correlation plot
  plot_corr <- eventReactive(c(input$cboCorrFeatures, input$cboColorBrewerCorrplot), {
    req(rv_current$working_df)
    Rautoml::custom_corrplot(
      df = rv_current$working_df,
      features = input$cboCorrFeatures,
      colorbrewer = input$cboColorBrewerCorrplot
    )
  })
  
  output$CorrPlotOutput <- renderPlot({
    plot_corr()
  })
  
  
  
  
  

  output$dfPreview <- DT::renderDataTable({
    req(rv_current$working_df)
    data.table::data.table(head(rv_current$working_df,n=10))
                  }, options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    scrollY=TRUE,
                    searching = FALSE
                  ), class = 'display nowrap')


  observeEvent(input$cboOutput,{
      if (isTRUE(input$cboOutput == "Chart")) {
        
        updateSelectInput(session, "cboXVar", choices = names(rv_current$working_df), selected = "")
        
        updateSelectInput(session, "cboYVar", choices = names(numeric_df(rv_current$working_df)) , selected = "")
        
        updateSelectInput(session, "cboColorVar", choices = names(non_numric_non_date_df(rv_current$working_df)), selected = "")
        updateSelectInput(session, "cboFacetVar", choices = names(non_numric_non_date_df(rv_current$working_df)), selected = "")
        
        
      } else if(input$cboOutput == "Table"){
        
        updateSelectInput(session, "cboColVar", choices = names(rv_current$working_df), selected = "")
        updateSelectInput(session, "cboCalcVar", choices = names(rv_current$working_df), selected = "")
        
      }
    
  })
  

  
  observeEvent(c(input$cboOutput, input$btnChartType), {
    
    output$tabSummaries = NULL
    output$GeneratedPlot = NULL
    rv_current$plot_rv = NULL
    rv_current$tab_rv = NULL
    
  })
  
  #c(input$cboOutput, input$btnchartOut,input$btnChartType),
  observe({
    if(isTRUE(is.null(rv_current$plot_rv))){
      updateActionButton(session = session, inputId = "btnchartOut", label = get_rv_labels("user_create"))
      shinyjs::disable("btnchartDown")
    }else{
      updateActionButton(session = session, inputId = "btnchartOut", label = get_rv_labels("update_chart_button"))
      shinyjs::enable("btnchartDown")
    }
    
  })

  
  observe({
    if(isTRUE(is.null(rv_current$tab_rv))){
      updateActionButton(session = session, inputId = "btnCreatetable", label = get_rv_labels("usr_create_cross_tab"))
      shinyjs::disable("btnDownloadTable")
    }else{
      updateActionButton(session = session, inputId = "btnCreatetable", label = get_rv_labels("update_tab_button"))
      shinyjs::enable("btnDownloadTable")
    }
    
  })
  
  observeEvent(input$tabmore, {
    if(input$tabmore==1){
      shinyjs::show("tabmoreoption")
    }else{
      shinyjs::hide("tabmoreoption")
    }
  })
  
  observeEvent(input$graphmore, {
    if(input$graphmore==1){
      shinyjs::show("graphmoreoption")
    }else{
      shinyjs::hide("graphmoreoption")
    }
  })
  
  
  # observeEvent(c(input$cboXVar,input$cboYVar,input$cboColorVar,input$cboFacetVar,input$btnChartType,
  #                input$manage_data_apply,input$tabs,input$change_language),{
  #                  
  # if(isTRUE(!is.null(rv_current$tab_rv))){                 
  #   if(input$cboXVar==""|is.null(input$cboXVar)){
  #     shinyjs::disable("btnchartOut")
  #   }else{
  #     shinyjs::enable("btnchartOut")
  #   }
  #   }else{
  #     shinyjs::disable("btnchartOut")
  #   }
  #   
  # })
  
  observeEvent(input$cboOutput, {
    if (isTRUE(input$cboOutput == "Table")) {
      shinyjs::hide("btnChartType")
      shinyjs::hide("graphOutputs")
      shinyjs::show("tabOutputs")
      updateSwitchInput(session = session, inputId = "graphmore", value = 0)
      shinyjs::hide("graphmore")
      shinyjs::show("tabmore")
    } else if (isTRUE(input$cboOutput == "Chart")){
      shinyjs::show("btnChartType")
      shinyjs::show("graphOutputs")
      shinyjs::hide("tabOutputs")
      updateSwitchInput(session = session, inputId = "tabmore", value = 0)
      shinyjs::show("graphmore")
      shinyjs::hide("tabmore")
    } else {
      shinyjs::hide("btnChartType")
      shinyjs::hide("graphOutputs")
      shinyjs::hide("tabOutputs")
      shinyjs::hide("graphmore")
      shinyjs::hide("tabmore")
      updateSwitchInput(session = session, inputId = "tabmore", value = 0)
      updateSwitchInput(session = session, inputId = "graphmore", value = 0)

    }
  })

  
  observeEvent(input$btnChartType, {
    if (input$btnChartType == "Pie") {
      shinyjs::show("cboFacetVar")
      shinyjs::hide("cboColorVar")
      shinyjs::show("numfacettitlesize")
      shinyjs::show("rdoTransformToDoug")
      
      shinyjs::hide("cboYVar")
      shinyjs::hide("txtXlab")
      shinyjs::hide("txtYlab")
      shinyjs::hide("xaxistextangle")
    } else {
      shinyjs::hide("cboFacetVar")
      shinyjs::show("cboColorVar")
      shinyjs::hide("numfacettitlesize")
      shinyjs::hide("rdoTransformToDoug")
      
      shinyjs::show("cboYVar")
      shinyjs::show("txtXlab")
      shinyjs::show("txtYlab")
      shinyjs::show("xaxistextangle")
    }
    
  })
  
  observeEvent(input$btnChartType, {
    if (input$btnChartType == "Bar") {
      shinyjs::show("numBarWidth")
      shinyjs::show("rdoStacked")
    } else {
      shinyjs::hide("numBarWidth")
      shinyjs::hide("rdoStacked")
    }
  })
  
  observeEvent(c(input$btnChartType,input$rdoAddLineType), {
    if (input$btnChartType == "Line") {
      shinyjs::show("numLineSize")
      shinyjs::show("cboLineJoin")
      shinyjs::show("rdoAddLineType")
      shinyjs::show("rdoAddPoints")
      shinyjs::show("rdoSummaryTye")
    } else if (input$btnChartType == "Scatterplot") {
      shinyjs::show("numLineSize")
    }
    
    else {
      shinyjs::hide("numLineSize")
      shinyjs::hide("cboLineJoin")
      shinyjs::hide("rdoAddLineType")
      shinyjs::hide("rdoAddPoints")
      shinyjs::hide("rdoSummaryTye")
      
    }
    
  })
  
  observeEvent(input$rdoAddLineType, {
    if (input$rdoAddLineType == TRUE) {
      shinyjs::show("cboLineType")
    } else{
      shinyjs::hide("cboLineType")
    }
    
  }
  )

  
  observeEvent(input$btnChartType,
               {
                 if (input$btnChartType %in% c("Pie", "Histogram", "Line", "Scatterplot")) {
                   shinyjs::hide("rdoPltOrientation")
                 } else{
                   shinyjs::show("rdoPltOrientation")
                 }
               })
  
  observeEvent(input$btnChartType, {
    if (input$btnChartType %in% c("Pie", "Histogram")) {
      shinyjs::hide("cboYVar")
    } else{
      shinyjs::show("cboYVar")
    }
  })
  
  observeEvent(input$btnChartType, {
    if (input$btnChartType %in% c("Pie", "Histogram")) {
      shinyjs::hide("cboYVar")
    } else{
      shinyjs::show("cboYVar")
    }
  })
  
  observeEvent(input$btnChartType, {
    if (input$btnChartType == "Histogram") {
      shinyjs::show("numBinWidth")
      shinyjs::show("rdoDensityOnly")
      shinyjs::show("rdoOverlayDensity")
      shinyjs::hide("cboColorVar")
      
    } else {
      shinyjs::hide("numBinWidth")
      shinyjs::hide("rdoDensityOnly")
      shinyjs::hide("rdoOverlayDensity")
      
    }
  })
  
  observeEvent(input$cboColorVar, {
    if (input$cboColorVar == "" || is.null(input$cboColorVar) ||
        is.null(input$cboColorVar) || input$btnChartType == "Histogram") {
      shinyjs::show("cboColorSingle")
      shinyjs::hide("cboColorBrewer")
    } else{
      shinyjs::hide("cboColorSingle")
      shinyjs::show("cboColorBrewer")
    }
  })
  
  observeEvent(input$btnChartType, {
    if (input$btnChartType == "Scatterplot") {
      shinyjs::show("rdoAddShapes")
      shinyjs::show("numLineSize")
      if (input$rdoAddShapes == TRUE) {
        shinyjs::show("cboShapes")
      } else{
        shinyjs::hide("cboShapes")
      }
      
      shinyjs::show("cboAddSmooth")
      
      
      if (input$cboAddSmooth != "none") {
        shinyjs::show("rdoDisplaySeVal")
        
        if (input$rdoDisplaySeVal == TRUE) {
          shinyjs::show("numConfInt")
        } else{
          shinyjs::hide("numConfInt")
        }
        
      } else{
        shinyjs::hide("rdoDisplaySeVal")
        shinyjs::hide("numConfInt")
      }
      
      
    } else if (input$btnChartType == "Line") {
      shinyjs::show("numLineSize")
    }
    
    else {
      shinyjs::hide("rdoAddShapes")
      shinyjs::hide("cboAddSmooth")
      shinyjs::hide("numLineSize")
      shinyjs::hide("cboShapes")
      shinyjs::hide("rdoDisplaySeVal")
      shinyjs::hide("numConfInt")
      
      
    }
    
  })
  
  observe({
    if (isTRUE(!is.null(rv_current$working_df))) {
      nams <- names(rv_current$working_df)
      updateSelectInput(inputId = "cboSelectDataset",
                        choices = nams,
                        selected = nams[1])
      
    }
    
    
    if (isTRUE(!is.null(rv_current$working_df))) {
      nams <- names(rv_current$working_df)
      updateSelectInput(inputId = "cboSelectDatasetTab",
                        choices = nams,
                        selected = nams[1])
    }
  })
  
  
  
  observeEvent(input$cboOutput,{
    if(isTRUE(!is.null(rv_current$working_df))){
    if (input$cboOutput == "Chart") {
      
      if(input$btnChartType %in% c("Bar","Pie", "Boxplot", "Violin")){
        updateSelectInput(session, "cboXVar", choices = names(non_numric_df(rv_current$working_df)), selected = "")
        
      }else{
        updateSelectInput(session, "cboXVar", choices = names(rv_current$working_df), selected = "")
      }
      
      updateSelectInput(session, "cboYVar", choices = names(numeric_df(rv_current$working_df)), selected = "")
      
      updateSelectInput(session, "cboColorVar", choices = names(non_numric_non_date_df(rv_current$working_df)), selected = "")
      updateSelectInput(session, "cboFacetVar", choices = names(non_numric_non_date_df(rv_current$working_df)), selected = "")
      
    }else if (input$cboOutput == "Table"){
      updateSelectInput(session, "cboColVar", choices = names(rv_current$working_df), selected = "")
      updateSelectInput(session, "cboCalcVar", choices = names(rv_current$working_df), selected = "")
    }
    }
  })
  
  
  
  observeEvent(input$btnChartType, {
    if (isTRUE(!is.null(rv_current$working_df))) {
      if (input$btnChartType %in% c("Bar", "Pie", "Boxplot", "Violin")) {
        updateSelectInput(session,
                          "cboXVar",
                          choices = names(non_numric_df(rv_current$working_df)),
                          selected = "")
        
      } else{
        updateSelectInput(
          session,
          "cboXVar",
          choices = names(rv_current$working_df),
          selected = ""
        )
        
      }
    }
    
  })
  
  
  
  observeEvent(input$cboXVar,{
    if(isTRUE(!is.null(rv_current$working_df))){
      if(!is.null(input$cboXVar) && input$cboXVar!= ""){
        current_y_var <- input$cboYVar
        current_color_var <- input$cboColorVar
        current_facet_var <- input$cboFacetVar
        
        df_selected <- rv_current$working_df
        
        updateSelectInput(session, "cboYVar", choices = setdiff(names(numeric_df(df_selected)),input$cboXVar)  , selected = current_y_var)
        
        updateSelectInput(session, "cboColorVar", choices = setdiff(names(non_numric_non_date_df(df_selected)),input$cboXVar), selected = current_color_var)
        updateSelectInput(session, "cboFacetVar", choices = setdiff(names(non_numric_non_date_df(df_selected)),input$cboXVar), selected = current_facet_var)
      }
    }

    
  })
  
  
  observeEvent(input$cboYVar,{
    
    if(isTRUE(!is.null(rv_current$working_df))){
      if(!is.null(input$cboYVar) && input$cboYVar!=""){
        
        current_x_var <- input$cboXVar
        current_color_var <- input$cboColorVar
        current_facet_var <- input$cboFacetVar
        
        
        df_selected <- rv_current$working_df
        updateSelectInput(session, "cboXVar", choices = setdiff(names(df_selected),input$cboYVar) , selected = current_x_var)
        updateSelectInput(session, "cboColorVar", choices = setdiff(names(non_numric_non_date_df(df_selected)),input$cboYVar), selected = current_color_var)
        updateSelectInput(session, "cboFacetVar", choices = setdiff(names(non_numric_non_date_df(df_selected)),input$cboYVar), selected = current_facet_var)
      }
      
    }
    
  })
  
  
  observeEvent(input$cboColorVar,{
    
    if(isTRUE(!is.null(rv_current$working_df))){
      
      current_x_var <- input$cboXVar
      current_y_var <- input$cboYVar
      current_facet_var <- input$cboFacetVar
      
      
      if(!is.null(input$cboColorVar) && input$cboColorVar!=""){
        df_selected <- rv_current$working_df
        updateSelectInput(session, "cboXVar", choices = setdiff(names(df_selected),input$cboColorVar) , selected = current_x_var)
        
        updateSelectInput(session, "cboYVar", choices = setdiff(names(numeric_df(df_selected)),input$cboColorVar)  , selected = current_y_var)
        
        updateSelectInput(session, "cboFacetVar", choices = setdiff(names(non_numric_non_date_df(df_selected)),input$cboColorVar), selected = current_facet_var)
      }
    }
    

    
  })
  
  
  observeEvent(input$cboFacetVar,{
    
    if(isTRUE(!is.null(rv_current$working_df))){
      
      current_x_var <- input$cboXVar
      current_y_var <- input$cboYVar
      current_color_var <- input$cboColorVar
      
      
      if(!is.null(input$cboFacetVar) && input$cboFacetVar!=""){
        df_selected <- rv_current$working_df
        updateSelectInput(session, "cboXVar", choices = setdiff(names(df_selected),input$cboFacetVar) , selected = current_x_var)
        
        updateSelectInput(session, "cboYVar", choices = setdiff(names(numeric_df(df_selected)),input$cboFacetVar)  , selected = current_y_var)
        
        updateSelectInput(session, "cboColorVar", choices = setdiff(names(non_numric_non_date_df(df_selected)),input$cboFacetVar), selected = current_color_var)
      }
    }
    
   
    
  })
  
  
  
  
  observeEvent(input$cboCalcVar,{
    
    if(isTRUE(!is.null(rv_current$working_df))){
      current_col_var <- input$cboColVar
      
      if(!is.null(input$cboCalcVar)){
        updateSelectInput(session, "cboColVar", choices = setdiff(names(rv_current$working_df),input$cboCalcVar), selected = current_col_var)
      }
    }
    

    
  })
  
  
  observeEvent(input$cboColVar,{
    if(isTRUE(!is.null(rv_current$working_df))){
      current_calc_var <- input$cboCalcVar
      
      if(!is.null(input$cboColVar)&& input$cboColVar!=""){
        updateSelectInput(session, "cboCalcVar", choices = setdiff(names(rv_current$working_df),input$cboColVar), selected = current_calc_var)
      }
      
    }
    
  })
  

  observeEvent(input$btnchartOut,
               {
                 shinyalert::shinyalert(
                   html = TRUE,
                   title =paste0("<span style='color:#7bc148;'><h4>", get_rv_labels("waiting_time"), "</h4></span>"),
                   text = paste0("<span style='color: #7bc148;'><h5>",get_rv_labels("plot_loading"),"</h5></span>"),
                   closeOnClickOutside = FALSE,
                   showConfirmButton = FALSE,
                   showCancelButton = FALSE,
                   imageUrl = "https://upload.wikimedia.org/wikipedia/commons/c/c7/Loading_2.gif",
                   closeOnEsc = FALSE)
                 
              
                 if(isTRUE(!is.null(rv_current$working_df))){
                   
                   if(!is.null(input$cboXVar)&& input$cboXVar!=""){
                     if(input$btnChartType == "Boxplot"){
                       plt<- Rautoml::custom_boxplot(df = rv_current$working_df,
                                                     xvar = input$cboXVar,  yvar = input$cboYVar, xlab = input$txtXlab,
                                                     ylab = input$txtYlab, plot_title = input$txtPlotTitle,
                                                     vertical = input$rdoPltOrientation, colorVar =  input$cboColorVar,
                                                     title_pos = input$numplotposition, title_size= input$numplottitlesize,
                                                     axis_title_size = input$numaxisTitleSize, axis_text_size = input$numAxistextSize,
                                                     axistext_angle = input$xaxistextangle,
                                                     legend_title = input$txtLegend, colorbrewer = input$cboColorBrewer, default_col = input$cboColorSingle
                       )
                       rv_current$plot_rv=plt
                     } else if(input$btnChartType == "Violin"){
                       plt<- Rautoml::custom_violin(df = rv_current$working_df,
                                                    xvar = input$cboXVar,  yvar = input$cboYVar, xlab = input$txtXlab,
                                                    ylab = input$txtYlab, plot_title = input$txtPlotTitle,
                                                    vertical = input$rdoPltOrientation, colorVar =  input$cboColorVar,
                                                    title_pos = input$numplotposition, title_size= input$numplottitlesize,
                                                    axis_title_size = input$numaxisTitleSize, axis_text_size = input$numAxistextSize,
                                                    axistext_angle = input$xaxistextangle,
                                                    legend_title = input$txtLegend, colorbrewer = input$cboColorBrewer, default_col = input$cboColorSingle
                       )
                       rv_current$plot_rv=plt
                       
                     } else if(input$btnChartType == "Histogram"){
                       plt<- Rautoml::custom_histogram(df = rv_current$working_df,
                                                       variable = input$cboXVar, 
                                                       xlab = input$txtXlab, ylab = input$txtYlab, plot_title = input$txtPlotTitle,
                                                       title_pos = input$numplotposition, title_size= input$numplottitlesize,
                                                       axis_title_size = input$numaxisTitleSize, axis_text_size = input$numAxistextSize,
                                                       axistext_angle = input$xaxistextangle,
                                                       bin_width = input$numBinWidth, overlayDensisty = input$rdoOverlayDensity, 
                                                       density_only = input$rdoDensityOnly, fill_color = input$cboColorSingle
                       )
                       rv_current$plot_rv=plt
                       
                     }else if(input$btnChartType == "Line"){
                       plt<- Rautoml::custom_linegraph(df = rv_current$working_df,
                                                       xvar = input$cboXVar,  yvar = input$cboYVar, xlab = input$txtXlab,
                                                       ylab = input$txtYlab, line_type = input$cboLineType, plot_title = input$txtPlotTitle,
                                                       line_size= input$numLineSize, line_join = input$cboLineJoin, colorVar =  input$cboColorVar,
                                                       title_pos = input$numplotposition, title_size= input$numplottitlesize,
                                                       axis_title_size = input$numaxisTitleSize, axis_text_size = input$numAxistextSize,
                                                       addlinetype = input$rdoAddLineType, axistext_angle = input$xaxistextangle, default_col = input$cboColorSingle,
                                                       legend_title = input$txtLegend,addpoints = input$rdoAddPoints, summary_type = input$rdoSummaryTye, colorbrewer = input$cboColorBrewer
                       )
                       rv_current$plot_rv=plt
                       
                       
                     }else if(input$btnChartType == "Scatterplot"){
                       plt<- Rautoml::custom_scatterplot(df = rv_current$working_df,
                                                         xvar = input$cboXVar,  yvar = input$cboYVar, xlab = input$txtXlab,
                                                         ylab = input$txtYlab, addshape = as.logical(input$rdoAddShapes), plot_title = input$txtPlotTitle,
                                                         line_size= input$numLineSize, shapes = as.integer(input$cboShapes), colorVar =  input$cboColorVar,
                                                         title_pos = input$numplotposition, title_size= input$numplottitlesize,
                                                         axis_title_size = input$numaxisTitleSize, axis_text_size = input$numAxistextSize,
                                                         addsmooth = input$cboAddSmooth, axistext_angle = input$xaxistextangle,
                                                         legend_title = input$txtLegend, seval =as.logical(input$rdoDisplaySeVal),
                                                         confelev = input$numConfInt, colorbrewer = input$cboColorBrewer, default_col = input$cboColorSingle
                       )
                       rv_current$plot_rv=plt
                       
                     }else if(input$btnChartType == "Bar"){
                       plt<- Rautoml::custom_barplot(df = rv_current$working_df,
                                                     xvar = input$cboXVar,  yvar = input$cboYVar, xlab = input$txtXlab,
                                                     ylab = input$txtYlab, bar_width = input$numBarWidth, plot_title = input$txtPlotTitle,
                                                     vertical = input$rdoPltOrientation, stackedtype = input$rdoStacked, colorVar =  input$cboColorVar,
                                                     title_pos = input$numplotposition, title_size= input$numplottitlesize,
                                                     axis_title_size = input$numaxisTitleSize, axis_text_size = input$numAxistextSize,
                                                     data_label_size = input$numDataLabelSize, axistext_angle = input$xaxistextangle,
                                                     legend_title = input$txtLegend, colorbrewer = input$cboColorBrewer, default_col = input$cboColorSingle
                       )
                       rv_current$plot_rv=plt
                     }else if(input$btnChartType == "Pie"){
                       plt<- Rautoml::custom_piechart(df = rv_current$working_df,
                                                      xvar = input$cboXVar,plot_title = input$txtPlotTitle,transform_to_doughnut = input$rdoTransformToDoug,
                                                      facet_var = input$cboFacetVar, facet_title_size = input$numfacettitlesize,
                                                      title_pos = input$numplotposition, title_size= input$numplottitlesize,
                                                      data_label_size = input$numDataLabelSize,
                                                      legend_title = input$txtLegend, colorbrewer = input$cboColorBrewer
                       )
                       }else{
                         plt<-NULL
                   }
                     }else{
                     plt<-NULL
                   } 
                   
                    rv_current$plot_rv=plt
                   
                   output$GeneratedPlot <- renderPlot({plot(plt)})
                   shinyalert::closeAlert()
                   #shinyjs::show("btnUpdatePlot")
                   output$btnchartDown <- downloadHandler(
                     
                     filename = function() {
                       paste(input$btnChartType,format(Sys.time(), "%B %d %Y %H:%M:%S"), ".jpeg")
                     },
                     
                     content = function(file) {
                       ggsave(file, plot = plt, device = "jpeg", width = 16, height = 9)
                       
                     })
                 }
                
                 
               })
  
  # observe({
  #   if(is.null(input$cboXVar)||is.null(input$cboXVar=="")){
  #     shinyjs::disable("btnchartOut")
  #     }else{
  #       shinyjs::enable("btnchartOut")
  #     }
  #   
  # })
  
  
  observe({
    if(is.null(input$cboCalcVar)|| length(c(input$cboCalcVar))>5){
      shinyjs::disable(
        "btnCreatetable"
      )
    }else{
      shinyjs::enable("btnCreatetable")
    }
    
  })
  
  
  observeEvent(input$cboCalcVar,{
    if(isTRUE(length(input$cboCalcVar)>5)){
      showModal(modalDialog(
        h3(get_rv_labels("max_var_limit")),
        style = "text-align: center;color: red",
        h5(get_rv_labels("max_var_limit_description")),
        footer = tagList(
          actionButton("dismissBtn", "Close")
        )
      ))
    }
  })
  
  
  observeEvent(input$dismissBtn, {
    removeModal()
    runjs("$('#cboCalcVar').val('');")
  })

  
  observeEvent(input$btnCreatetable,{
    shinyalert::shinyalert(
      html = TRUE,
      title =paste0("<span style='color: #7bc148;'><h4>", get_rv_labels("waiting_time"), "</h4></span>"),
      text = paste0("<span style='color: #7bc148;'><h5>",get_rv_labels("table_loading"),"</h5></span>"),
      closeOnClickOutside = FALSE,
      showConfirmButton = FALSE,
      showCancelButton = FALSE,
      imageUrl = "https://upload.wikimedia.org/wikipedia/commons/c/c7/Loading_2.gif",
      closeOnEsc = FALSE)
    
    
    if(isTRUE(!is.null(rv_current$working_df))){
        tab <- Rautoml::custom_crosstab(df = rv_current$working_df
                       , vars =  c(input$cboCalcVar)
                       , by =if(input$cboColVar=="") NULL else input$cboColVar
                       , add.p=input$rdoAddTabPValue
                       , add.ci=input$rdoAddTabCI
                       , report_numeric = input$chkReportNumeric
                       , numeric_summary = input$chkNumericSummary
                       , drop_na=input$rdoDropTabMissingValues
                       , caption= input$txtTabCaption)%>%as_flex_table()
        }else{
          tab = NULL
        }
    rv_current$tab_rv = tab
    
    output$tabSummaries <- renderUI({
      div(
        style = "overflow-x:auto; width:100%;",
        flextable::htmltools_value(tab))
        })
    shinyalert::closeAlert()
    
    output$btnDownloadTable <- downloadHandler(
      filename = function() {
        paste0("summary_table_", format(Sys.Date(), "%B %d %Y"), ".docx")
      },
      content = function(file) {
        doc <- read_docx() %>%
          body_add_flextable(value = tab)
        
        print(doc, target = file)  
      },
      contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    )
    
  })
  
  
}
