#' @title Custom Scatter Plot Function
#' @description Generates a customized scatter plot with optional smoothing, color grouping, and shape differentiation.
#' @param df Data frame containing the variables for plotting.
#' @param xvar Name of the x-axis variable.
#' @param yvar Name of the y-axis variable.
#' @param colorVar Optional variable for coloring points.
#' @param line_size Numeric value for point size and line width.
#' @param addsmooth String specifying smoothing method (e.g., "lm", "loess").
#' @param seval Logical; whether to show confidence interval.
#' @param addshape Logical; whether to differentiate points by shape.
#' @param shapes Numeric value specifying point shape.
#' @param plot_title Optional title of the plot.
#' @param xlab Label for x-axis.
#' @param ylab Label for y-axis.
#' @param title_pos Numeric value for title position.
#' @param title_size Numeric value for title size.
#' @param default_col Default color for points.
#' @param axis_title_size Numeric value for axis title size.
#' @param axis_text_size Numeric value for axis text size.
#' @param axistext_angle Numeric value for x-axis text angle.
#' @param legend_title Optional title for the legend.
#' @param confelev Confidence level for smoothing confidence interval.
#' @param colorbrewer Color palette for grouped variables.
#' @return A ggplot2 object representing the scatter plot.
#' @examples
#' scatterplot_custom(df, "mpg", "hp", colorVar = "cyl", addsmooth = "lm")
#' @export
scatterplot_custom <- function(df, xvar, yvar, colorVar=NULL, line_size = 0.9, addsmooth = "none", seval = TRUE, addshape=TRUE, shapes = 1,
                               plot_title = NULL, xlab = NULL, ylab = NULL, title_pos = 0.5, title_size = 25, default_col = "#0077B6",
                               axis_title_size = 20, axis_text_size = 16, axistext_angle = 0, legend_title = "", confelev = .95, colorbrewer = "Dark2"){
  
  vars_col <-  c(xvar, yvar, colorVar)
  existing_vars <- c()
  for(var in vars_col) {
    if(var %in% names(df)) {
      existing_vars <- c(existing_vars, var)
    }
  }
  
  df <- na.omit(df[, existing_vars])
  
  if(is.null(colorVar) || colorVar=="" && !is.null(xvar) && xvar!="" && !is.null(yvar) && yvar!=""){
    
    df1 <- df[, c(xvar, yvar)]
    names(df1) <- c("xvar1", "yvar1")
    
    if(addsmooth == "none"){
      p <- ggplot2::ggplot(data = df1, ggplot2::aes(x= xvar1, y = yvar1))+
        ggplot2::geom_point(shape = shapes, colour = default_col, size = line_size+.5)
    } else if(addsmooth != "none"){
      
      p <- ggplot2::ggplot(data = df1, ggplot2::aes(x= xvar1, y = yvar1))+
        ggplot2::geom_point(shape = shapes, colour = default_col, size = line_size+.5)+
        ggplot2::geom_smooth(se = seval, method = addsmooth, colour = default_col, linewidth = line_size, level = confelev)
    }
  }
  else if(!is.null(colorVar) && colorVar!="" && addshape==FALSE && !is.null(xvar) && xvar!="" && !is.null(yvar) && yvar!=""){
    df1 <- df[, c(xvar, yvar, colorVar)]
    names(df1) <- c("xvar1", "yvar1", "colorvar1")
    df1$colorvar1 <- as.factor(df1$colorvar1 )
    if(addsmooth == "none"){
      p <- ggplot2::ggplot(data = df1, ggplot2::aes(x= xvar1, y = yvar1, fill = colorvar1))+
        ggplot2::geom_point(size = line_size+.5)+
        ggplot2::guides(fill = ggplot2::guide_legend(title = ifelse(
          is.null(legend_title), "Legend", legend_title
        )))+ggplot2::scale_fill_brewer(palette=colorbrewer)
    } else if(addsmooth != "none"){
      p <- ggplot2::ggplot(data = df1, ggplot2::aes(x= xvar1, y = yvar1, fill = colorvar1))+
        ggplot2::geom_point(size = line_size+.5)+
        ggplot2::geom_smooth(se = seval, method = addsmooth, formula = y~x, linewidth = line_size, level = confelev)+
        ggplot2::guides(fill = ggplot2::guide_legend(title = ifelse(
          is.null(legend_title), "Legend", legend_title
        )))+ggplot2::scale_fill_brewer(palette=colorbrewer)
    }
  }
  
  p <- p + ggplot2::theme_minimal() +
    ggplot2::xlab(ifelse(!is.null(xlab), xlab, "")) + 
    ggplot2::ylab(ifelse(!is.null(ylab), ylab, "")) +
    ggplot2::ggtitle(ifelse(!is.null(plot_title), plot_title, "")) +
    ggplot2::labs(caption = paste0("Copyright © ", lubridate::year(Sys.Date())," , APHRC, All Rights Reserved")) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = as.numeric(title_pos), size = as.integer(title_size), colour = "black"),
      plot.caption = ggplot2::element_text(hjust=1, color = "#808080"),
      axis.text.x = ggplot2::element_text(size = as.integer(axis_text_size), colour = "black", angle = as.numeric(axistext_angle)),
      axis.text.y = ggplot2::element_text(size = as.integer(axis_text_size), colour = "black"),
      axis.title = ggplot2::element_text(size = as.integer(axis_title_size), colour = "black"),
      legend.position = ifelse(is.null(colorVar), "none", "bottom")
    )
  
  return(p)
}




