user_defined_server <- function() {
  
  observeEvent(input$cboOutput, {
    if (input$cboOutput == "Table") {
      shinyjs::hide("btnChartType")
      shinyjs::hide("graphOutputs")
      shinyjs::show("tabOutputs")
    } else {
      shinyjs::show("btnChartType")
      shinyjs::show("graphOutputs")
      shinyjs::hide("tabOutputs")
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
  
  observeEvent(input$btnChartType, {
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
      
      if (input$rdoAddLineType == TRUE) {
        shinyjs::show("cboLineType")
      } else{
        shinyjs::hide("cboLineType")
      }
    }
    
  })
  
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
    if (input$cboColorVar == "" ||
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
  
  
  #temp
  
  #This Function only returns non numeric dataframe
  non_numric_df <- function(df){
    final_df <- df[,sapply(df, FUN =  function(x){is.character(x)||is.factor(x)||is.logical(x)||is.Date(x)})]
    return(final_df)
  }
  
  #This Function only returns numeric dataframe
  numeric_df <- function(df){
    final_df <- df[,sapply(df, FUN =  function(x){is.numeric(x)||is.integer(x)})]
    return(final_df)
  }
  
  #This Function only returns numeric dataframe
  non_numric_non_date_df <- function(df){
    final_df <- df[,sapply(df, FUN =  function(x){is.character(x)||is.factor(x)||is.logical(x)})]
    return(final_df)
  }
  
  
  observeEvent(input$cboRowVar,{
    
    if(isTRUE(!is.null(rv_current$working_df))){
      current_calc_var <- input$cboCalcVar
      current_col_var <- input$cboColVar
      
      if(!is.null(input$cboRowVar)|| input$cboRowVar !=""){
        df_selected <- rv_current$working_df
        updateSelectInput(session, "cboCalcVar", choices = setdiff(names(df_selected),input$cboRowVar), selected = current_calc_var)
        updateSelectInput(session, "cboColVar", choices = setdiff(names(non_numric_df(df_selected)),input$cboRowVar), selected = current_col_var)
      }
      
    }
    
  })
  
  
  observeEvent(input$cboCalcVar,{
    
    if(isTRUE(!is.null(rv_current$working_df))){
      current_Row_var <- input$cboRowVar
      current_col_var <- input$cboColVar
      
      if(!is.null(input$cboCalcVar)||input$cboCalcVar!=""){
        df_selected <- rv_current$working_df
        updateSelectInput(session, "cboRowVar", choices = setdiff(names(df_selected),input$cboCalcVar), selected = current_Row_var)
        updateSelectInput(session, "cboColVar", choices = setdiff(names(non_numric_df(df_selected)),input$cboCalcVar), selected = current_col_var)
      }
    }
    

    
  })
  
  
  observeEvent(input$cboColVar,{
    if(isTRUE(!is.null(rv_current$working_df))){
      current_Row_var <- input$cboRowVar
      current_calc_var <- input$cboCalcVar
      
      if(!is.null(input$cboColVar)||input$cboColVar!=""){
        df_selected <- rv_current$working_df
        updateSelectInput(session, "cboRowVar", choices = setdiff(names(df_selected),input$cboColVar), selected = current_Row_var)
        updateSelectInput(session, "cboCalcVar", choices = setdiff(names(non_numric_df(df_selected)),input$cboColVar), selected = current_calc_var)
      }
      
    }
    

    
    
  })
  
  observeEvent(input$cboOutput,{
    
    if(isTRUE(!is.null(rv_current$working_df))){
      if (input$cboOutput == "Chart") {
        
        df_selected <- rv_current$working_df
        
        updateSelectInput(session, "cboXVar", choices = names(df_selected), selected = "")
        
        
        updateSelectInput(session, "cboYVar", choices = names(numeric_df(df_selected)) , selected = "")
        
        updateSelectInput(session, "cboColorVar", choices = names(non_numric_non_date_df(df_selected)), selected = "")
        updateSelectInput(session, "cboFacetVar", choices = names(non_numric_non_date_df(df_selected)), selected = "")
        
        
      } else if(input$cboOutput == "Table"){
        
        df_selected <- rv_current$working_df
        
        updateSelectInput(session, "cboRowVar", choices = names(non_numric_df(df_selected)), selected = "")
        updateSelectInput(session, "cboColVar", choices = names(non_numric_df(df_selected)), selected = "")
        updateSelectInput(session, "cboCalcVar", choices = names(df_selected), selected = "")
        
      }
    }
  
  })
  
  
  
  
  observeEvent(input$btnchartOut,
               {
                 if(isTRUE(!is.null(rv_current$working_df))){
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
                   }else if(input$btnChartType == "Pie"){
                     plt<- Rautoml::custom_piechart(df = rv_current$working_df,
                                     xvar = input$cboXVar,plot_title = input$txtPlotTitle,transform_to_doughnut = input$rdoTransformToDoug,
                                     facet_var = input$cboFacetVar, facet_title_size = input$numfacettitlesize,
                                     title_pos = input$numplotposition, title_size= input$numplottitlesize,
                                     data_label_size = input$numDataLabelSize,
                                     legend_title = input$txtLegend, colorbrewer = input$cboColorBrewer
                     )
                   }
                   
                   output$GeneratedPlot <- renderPlot({plot(plt)})
                   shinyjs::show("btnUpdatePlot")
                   output$btnchartDown <- downloadHandler(
                     
                     filename = function() {
                       paste(input$btnChartType,format(Sys.time(), "%B %d %Y %H:%M:%S"), ".jpeg")
                     },
                     
                     content = function(file) {
                       ggsave(file, plot = plt, device = "jpeg", width = 16, height = 9)
                       
                     })
                 }
                
                 
               })
  
  
  observeEvent(input$btnUpdatePlot,
               {
                 if(isTRUE(!is.null(rv_current$working_df))){
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
                   }else if(input$btnChartType == "Pie"){
                     plt<- Rautoml::custom_piechart(df = rv_current$working_df,
                                                    xvar = input$cboXVar,plot_title = input$txtPlotTitle,transform_to_doughnut = input$rdoTransformToDoug,
                                                    facet_var = input$cboFacetVar, facet_title_size = input$numfacettitlesize,
                                                    title_pos = input$numplotposition, title_size= input$numplottitlesize,
                                                    data_label_size = input$numDataLabelSize,
                                                    legend_title = input$txtLegend, colorbrewer = input$cboColorBrewer
                     )
                   }
                   
                   output$GeneratedPlot <- renderPlot({plot(plt)})
                   shinyjs::show("btnUpdatePlot")
                   output$btnchartDown <- downloadHandler(
                     
                     filename = function() {
                       paste(input$btnChartType,format(Sys.time(), "%B %d %Y %H:%M:%S"), ".jpeg")
                     },
                     
                     content = function(file) {
                       ggsave(file, plot = plt, device = "jpeg", width = 16, height = 9)
                       
                     })
                 }
               
                 
               })
  
  
  
  tabsum <- eventReactive(input$btnCreatetable,{
    if(isTRUE(!is.null(rv_current$working_df))){
      
      if((all(!is.null(input$cboCalcVar) & input$cboCalcVar!="")) && (is.null(input$cboColVar)||input$cboColVar=="") && (is.null(input$cboRowVar)||input$cboRowVar=="")){
        
        tab <- Rautoml::custom_crosstab(df = rv_current$working_df
                       , vars =  c(input$cboCalcVar)
                       , add.p=input$rdoAddTabPValue
                       , add.ci=input$rdoAddTabCI
                       , report_numeric = input$chkReportNumeric
                       , numeric_summary = input$chkNumericSummary
                       , drop_na=input$rdoDropTabMissingValues
                       , caption= input$txtTabCaption)%>%as_gt()
        
      }else if((all(!is.null(input$cboCalcVar) & input$cboCalcVar!="")) &&(!is.null(input$cboColVar)&&input$cboColVar!="") && (!is.null(input$cboRowVar)&&input$cboRowVar!="")){
        tab <- Rautoml::custom_crosstab(df = rv_current$working_df, by =  input$cboColVar
                       , strata= input$cboRowVar
                       , vars =  c(input$cboCalcVar)
                       , add.p=input$rdoAddTabPValue
                       , add.ci=input$rdoAddTabCI
                       , report_numeric = input$chkReportNumeric
                       , numeric_summary = input$chkNumericSummary
                       , drop_na=input$rdoDropTabMissingValues
                       , caption= input$txtTabCaption)%>%as_gt()
      }else if((all(!is.null(input$cboCalcVar) & input$cboCalcVar!="")) && (!is.null(input$cboColVar)&&input$cboColVar!="") && (is.null(input$cboRowVar)||input$cboRowVar=="")){
        
        tab<- Rautoml::custom_crosstab(df = rv_current$working_df, by =  input$cboColVar
                      , vars = c(input$cboCalcVar)
                      , add.p=input$rdoAddTabPValue
                      , add.ci=input$rdoAddTabCI
                      , report_numeric = input$chkReportNumeric
                      , numeric_summary = input$chkNumericSummary
                      , drop_na=input$rdoDropTabMissingValues
                      , caption= input$txtTabCaption)%>%as_gt()
        
      }else if((all(!is.null(input$cboCalcVar) & input$cboCalcVar!="")) && (is.null(input$cboColVar)||input$cboColVar=="") && (!is.null(input$cboRowVar) && input$cboRowVar!="")){
        
        tab<-  Rautoml::custom_crosstab(df = rv_current$working_df, vars =  c(input$cboCalcVar)
                       , strata= input$cboRowVar
                       , add.p=input$rdoAddTabPValue
                       , add.ci=input$rdoAddTabCI
                       , report_numeric = input$chkReportNumeric
                       , numeric_summary = input$chkNumericSummary
                       , drop_na=input$rdoDropTabMissingValues
                       , caption= input$txtTabCaption)%>%as_gt()
        
      }
      
      return(tab)
    }
    
  })
  
  ##Downloading Tables
  
  tabsumimage <- reactive({
    
    outfile <- tempfile(fileext = ".png")
    
    gtsave(data = tabsum(), 
           filename = outfile)
    
    outfile
    
  })
  
  
  output$tabSummaries <- gt::render_gt(tabsum())
  output$btnDownloadTable <- downloadHandler(
    filename = paste0("Table",format(Sys.Date(), "%B %d %Y"), ".png"),
    
    content = function(file) {
      file.copy(tabsumimage(), file)
      
    },
    contentType = 'image/png'
  )
  
  
}