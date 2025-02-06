#' A custom APHRC Line Graph Plotter
#' 
#' This function plots a flexible line graph that can be customized based on various parameters.
#' 
#' @param df A data frame containing the data to be plotted.
#' @param xvar The variable to be plotted on the x-axis.
#' @param yvar The variable to be plotted on the y-axis (optional).
#' @param colorVar A variable to color the lines by (optional).
#' @param line_size The size of the line.
#' @param line_type The type of the line (e.g., "solid", "dashed").
#' @param line_join The type of line join (e.g., "round", "mitre").
#' @param addlinetype Whether to add different line types based on the colorVar (default FALSE).
#' @param plot_title Title of the plot.
#' @param ggplot2::xlab Label for the x-axis.
#' @param ggplot2::ylab Label for the y-axis.
#' @param title_pos Position of the title (default 0.5).
#' @param title_size Size of the plot title.
#' @param default_col Default color for the plot lines.
#' @param axis_title_size Size of the axis titles.
#' @param axis_text_size Size of the axis text.
#' @param data_label_size Size of the data labels.
#' @param axistext_angle Angle of the axis text.
#' @param legend_title Title of the legend.
#' @param addpoints Whether to add points to the line graph (default FALSE).
#' @param summary_type Type of summary ("Total" or "Average").
#' @param colorbrewer Color palette for the graph (default "Dark2").
#' 
#' @return A ggplot2::ggplot object of the line graph.
#' @import ggplot2::ggplot2
#' @import dplyr
#' @export

# Function to plot a flexible line graph
line_graph <- function(df, xvar, yvar = NULL, colorVar = NULL, line_size = 0.9,line_type = "solid", line_join = "round", addlinetype = FALSE,
                       plot_title = NULL, xlab = NULL, ylab = NULL, title_pos = 0.5, title_size = 25, default_col = "#0077B6",
                       axis_title_size = 20, axis_text_size = 16, data_label_size = 10, axistext_angle = 0, legend_title = "", addpoints = FALSE, summary_type = "Total",colorbrewer = "Dark2") {
  
  vars_col <-  c(xvar, yvar, colorVar)
  existing_vars <- c()
  for(var in vars_col) {
    if(var %in% names(df)) {
      existing_vars <- c(existing_vars, var)
    }
  }
  
  df <- na.omit(df[, existing_vars])
  
  
  if(addlinetype ==FALSE){
    
    
    if((is.null(yvar) || yvar == "") && (is.null(colorVar)|| colorVar == "")){
      df <- as.data.frame(df[, xvar])
      names(df) <- "xvar1"
      
      
      df_line <- df%>%dplyr::group_by(xvar1)%>%dplyr::summarise(total =n())%>%dplyr::arrange(xvar1)
      
      if(addpoints==FALSE){
        p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
          ggplot2::geom_line(stat = "identity",  linetype = "solid", 
                    ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join
          )
      }
      else{
        p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
          ggplot2::geom_line(stat = "identity",  linetype = "solid", 
                    ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join)+geom_point(
                      color = default_col, size = line_size+1
                    )
      }
    }
    
    else if ((is.null(yvar) || yvar == "") && (!is.null(colorVar) && colorVar != "")){
      df <- as.data.frame(df[, c(xvar, colorVar)])
      names(df) <- c("xvar1", "colorvar1")
      df$colorvar1 <- as.factor( df$colorvar1)
      df_line <- df%>%dplyr::group_by(xvar1, colorvar1)%>%dplyr::summarise(total =n())%>%dplyr::arrange(xvar1)
      
      if(addpoints==FALSE){
        p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, group = colorvar1))+
          ggplot2::geom_line(stat = "identity",  linetype = line_type, linewidth = line_size, linejoin = line_join
          )+
          ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
            is.null(legend_title), "Legend", legend_title
          )))
      }
      else{
        p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, group = colorvar1))+
          ggplot2::geom_line(stat = "identity",  linetype = line_type, linewidth = line_size, linejoin = line_join)+geom_point(
            size = line_size+1
          )+
          ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
            is.null(legend_title), "Legend", legend_title
          )))
      }
      
    }
    
    else if ((!is.null(yvar) || yvar != "") && (is.null(colorVar) || colorVar == "")){
      df <- as.data.frame(df[, c(xvar, yvar)])
      names(df) <- c("xvar1", "yvar1")
      
      if(summary_type == "Total"){
        df_line <- df%>%dplyr::group_by(xvar1)%>%dplyr::summarise(total = sum(yvar1, na.rm = TRUE))%>%dplyr::arrange(xvar1)
        
        if(addpoints==FALSE){
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
            ggplot2::geom_line(stat = "identity",   linetype = "solid", 
                      ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join
            )
        }
        else{
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
            ggplot2::geom_line(stat = "identity",   linetype = "solid", 
                      ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join)+geom_point(
                        color = default_col, size = line_size+1
                      )
        }
      }
      
      else if(summary_type=="Average"){
        df_line <- df%>%dplyr::group_by(xvar1)%>%dplyr::summarise(total = mean(yvar1, na.rm=TRUE))%>%dplyr::arrange(xvar1)
        
        if(addpoints==FALSE){
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
            ggplot2::geom_line(stat = "identity",  linetype = "solid", 
                      ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join
            )
        }
        else{
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
            ggplot2::geom_line(stat = "identity",   linetype = "solid", 
                      ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join)+geom_point(
                        color = default_col, size = line_size+1
                      )
        }
      }
      
      
    }
    
    
    ###########
    
    else if ((!is.null(yvar) || yvar != "") && (!is.null(colorVar) && colorVar != "")){
      df <- as.data.frame(df[, c(xvar, yvar, colorVar)])
      names(df) <- c("xvar1", "yvar1", "colorvar1")
      df$colorvar1 <- as.factor( df$colorvar1)
      
      if(summary_type == "Total"){
        df_line <- df%>%dplyr::group_by(xvar1, colorvar1)%>%dplyr::summarise(total = sum(yvar1, na.rm = TRUE))%>%dplyr::arrange(xvar1)
        
        if(addpoints==FALSE){
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, group = colorvar1))+
            ggplot2::geom_line(stat = "identity",  linetype = line_type, linewidth = line_size, linejoin = line_join
            )+
            ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
              is.null(legend_title), "Legend", legend_title
            )))
        }
        else{
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, group = colorvar1))+
            ggplot2::geom_line(stat = "identity",  linetype = line_type, linewidth = line_size, linejoin = line_join)+geom_point(
              size = line_size+1
            )+
            ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
              is.null(legend_title), "Legend", legend_title
            )))
        }
      }
      
      else if(summary_type=="Average"){
        df_line <- df%>%dplyr::group_by(xvar1, colorvar1)%>%dplyr::summarise(total = mean(yvar1, na.rm=TRUE))%>%dplyr::arrange(xvar1)
        
        if(addpoints==FALSE){
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, group = colorvar1))+
            ggplot2::geom_line(stat = "identity",  linetype = line_type, linewidth = line_size, linejoin = line_join
            )+
            ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
              is.null(legend_title), "Legend", legend_title
            )))
        }
        else{
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, group = colorvar1))+
            ggplot2::geom_line(stat = "identity",  linetype = line_type, linewidth = line_size, linejoin = line_join)+geom_point(
              size = line_size+1
            )+
            ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
              is.null(legend_title), "Legend", legend_title
            )))
        }
      }
    }
    
    
  }
  else if(addlinetype==TRUE){
    
    #########groupby linetype
    if((is.null(yvar) || yvar == "") && (is.null(colorVar)|| colorVar == "")){
      df <- as.data.frame(df[, xvar])
      names(df) <- "xvar1"
      
      
      df_line <- df%>%dplyr::group_by(xvar1)%>%dplyr::summarise(total =n())%>%dplyr::arrange(xvar1)
      
      if(addpoints==FALSE){
        p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
          ggplot2::geom_line(stat = "identity",  linetype = line_type, 
                    ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join
          )
      }
      else{
        p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
          ggplot2::geom_line(stat = "identity",  linetype = line_type, 
                    ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join)+geom_point(
                      color = default_col, size = line_size+1
                    )
      }
    }
    
    else if ((is.null(yvar) || yvar == "") && (!is.null(colorVar) && colorVar != "")){
      df <- as.data.frame(df[, c(xvar, colorVar)])
      names(df) <- c("xvar1", "colorvar1")
      df$colorvar1 <- as.factor(df$colorvar1)
      df_line <- df%>%dplyr::group_by(xvar1, colorvar1)%>%dplyr::summarise(total =n())%>%dplyr::arrange(xvar1)
      
      if(addpoints==FALSE){
        p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, group = colorvar1))+
          ggplot2::geom_line(stat = "identity", linewidth = line_size, linejoin = line_join, ggplot2::aes(linetype = colorvar1)
          )+
          ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
            is.null(legend_title), "Legend", legend_title
          )))
      }
      else{
        p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, group = colorvar1))+
          ggplot2::geom_line(stat = "identity", linewidth = line_size, linejoin = line_join,
                    ggplot2::aes(linetype = colorvar1))+geom_point(
                      size = line_size+1
                    )+
          ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
            is.null(legend_title), "Legend", legend_title
          )))
      }
      
    }
    
    else if ((!is.null(yvar) || yvar != "") && (is.null(colorVar) || colorVar == "")){
      df <- as.data.frame(df[, c(xvar, yvar)])
      names(df) <- c("xvar1", "yvar1")
      
      if(summary_type == "Total"){
        df_line <- df%>%dplyr::group_by(xvar1)%>%dplyr::summarise(total = sum(yvar1, na.rm = TRUE))%>%dplyr::arrange(xvar1)
        
        if(addpoints==FALSE){
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
            ggplot2::geom_line(stat = "identity",  linetype = line_type, 
                      ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join
            )
        }
        else{
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
            ggplot2::geom_line(stat = "identity",  linetype = line_type, 
                      ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join)+geom_point(
                        color = default_col, size = line_size+1
                      )
        }
      }
      
      else if(summary_type=="Average"){
        df_line <- df%>%dplyr::group_by(xvar1)%>%dplyr::summarise(total = mean(yvar1, na.rm=TRUE))%>%dplyr::arrange(xvar1)
        
        if(addpoints==FALSE){
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
            ggplot2::geom_line(stat = "identity",  linetype = line_type, 
                      ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join
            )
        }
        else{
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total))+
            ggplot2::geom_line(stat = "identity",  linetype = line_type, 
                      ggplot2::aes(group = 1), color = default_col, linewidth = line_size, linejoin = line_join)+geom_point(
                        color = default_col, size = line_size+1
                      )
        }
      }
      
      
    }
    
    
    ###########
    
    else if ((!is.null(yvar) || yvar != "") && (!is.null(colorVar) && colorVar != "")){
      df <- as.data.frame(df[, c(xvar, yvar, colorVar)])
      names(df) <- c("xvar1", "yvar1", "colorvar1")
      df$colorvar1 <- as.factor( df$colorvar1)
      if(summary_type == "Total"){
        df_line <- df%>%dplyr::group_by(xvar1, colorvar1)%>%dplyr::summarise(total = sum(yvar1, na.rm = TRUE))%>%dplyr::arrange(xvar1)
        
        if(addpoints==FALSE){
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, linetype = colorvar1))+
            ggplot2::geom_line(stat = "identity", linewidth = line_size, linejoin = line_join
            )+
            ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
              is.null(legend_title), "Legend", legend_title
            )))
        }
        else{
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, group = colorvar1))+
            ggplot2::geom_line(stat = "identity",  ggplot2::aes(linetype = colorvar1), linewidth = line_size, linejoin = line_join)+geom_point(
              size = line_size+1
            )+
            ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
              is.null(legend_title), "Legend", legend_title
            )))
        }
      }
      
      else if(summary_type=="Average"){
        df_line <- df%>%dplyr::group_by(xvar1, colorvar1)%>%dplyr::summarise(total = mean(yvar1, na.rm=TRUE))%>%dplyr::arrange(xvar1)
        
        if(addpoints==FALSE){
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, group = colorvar1))+
            ggplot2::geom_line(stat = "identity",  ggplot2::aes(linetype = colorvar1), linewidth = line_size, linejoin = line_join
            )+
            ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
              is.null(legend_title), "Legend", legend_title
            )))
        }
        else{
          p <- ggplot2::ggplot(data = df_line, mapping = ggplot2::aes(x = xvar1, y = total, color = colorvar1, group = colorvar1))+
            ggplot2::geom_line(stat = "identity",  ggplot2::aes(linetype = colorvar1), linewidth = line_size, linejoin = line_join)+geom_point(
              size = line_size+1
            )+
            ggplot2::guides(color = ggplot2::guide_legend(title = ifelse(
              is.null(legend_title), "Legend", legend_title
            )))
        }
      }
    }
    
  }
  
  p <-
    p +ggplot2::theme_minimal()+ ggplot2::xlab(ifelse(!is.null(xlab), xlab, "")) + ggplot2::ylab(ifelse(!is.null(ylab), ylab, "")) +
    ggplot2::ggtitle(ifelse(!is.null(plot_title), plot_title, "")) + ggplot2::labs(caption = paste0("Copyright © ", year(Sys.Date())," , APHRC, All Rights Reserved")) + ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = as.numeric(title_pos),
        size = as.integer(title_size),
        colour = "black"
      ),plot.caption = ggplot2::element_text(hjust=1, color = "#808080"),
      axis.text.x = ggplot2::element_text(size = as.integer(axis_text_size), colour = "black", angle = as.numeric(axistext_angle)),
      axis.text.y = ggplot2::element_text(size = as.integer(axis_text_size), colour = "black"),
      axis.title = ggplot2::element_text(size = as.integer(axis_title_size), colour = "black"),
      legend.position = ifelse(is.null(colorVar), "none", "bottom")
      
    )+ggplot2::scale_color_brewer(palette=colorbrewer)
  
  
  return(p)
}

#Test
#line_graph(df = mtcars, xvar = "gear", line_join = "mitre", addpoints = FALSE, line_size = 2, colorVar = "cyl", yvar = "mpg", summary_type = "Average", addlinetype = TRUE)

