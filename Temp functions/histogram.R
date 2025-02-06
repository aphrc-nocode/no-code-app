#' @title Custom Histogram Plot
#' @description Creates a histogram with optional density overlay and customization options.
#' @param df A data frame containing the variable to plot.
#' @param variable The name of the variable to be plotted.
#' @param bin_width The width of histogram bins. Defaults to 10.
#' @param title The title of the plot (deprecated, use plot_title instead).
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param density_only Logical, if TRUE, plots only the density curve.
#' @param fill_color The fill color of the histogram bars. Defaults to "#0077B6".
#' @param border_color The border color of the histogram bars. Defaults to "white".
#' @param overlayDensisty Logical, if TRUE, overlays a density plot on the histogram.
#' @param plot_title Title of the plot.
#' @param axis_text_size Size of the axis text. Defaults to 18.
#' @param axis_title_size Size of the axis titles. Defaults to 24.
#' @param title_size Size of the plot title. Defaults to 28.
#' @param axistext_angle Angle of x-axis text labels. Defaults to 0.
#' @param title_pos Horizontal position of the title (0 to 1). Defaults to 0.5.
#' @return A ggplot2 object representing the histogram.
#' @import ggplot2
#' @export
histogram <- function(df, variable, bin_width = 10, title = NULL, xlab = NULL, ylab = NULL,density_only = FALSE,
                      fill_color = "#0077B6", border_color = "white", overlayDensisty = FALSE, plot_title = NULL,
                      axis_text_size = 18, axis_title_size = 24, title_size = 28, axistext_angle = 0, title_pos =.5) {
  
  df <- stats::na.omit(df[, c(variable), drop=FALSE])
  
  if(is.numeric(df[[variable]])){
    if (is.null(bin_width)) {
      iqr <- stats::IQR(df[,variable])
      bins <- 2 * iqr / (nrow(df) ^ (1/3))
      bins <- ifelse(is.finite(bins) && bins > 0, bins, 1)
    }else{
      bins = bin_width
    }
    
    if(density_only==TRUE & overlayDensisty==TRUE){
      df <- as.data.frame(df[,variable])
      names(df) <- "xvar"
      
      hist_p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = xvar))+
        ggplot2::geom_density(color = "black", fill = fill_color)
    } else if (density_only==FALSE & overlayDensisty==TRUE){
      df <- as.data.frame(df[,variable])
      names(df) <- "xvar"
      
      hist_p <- ggplot2::ggplot(df, ggplot2::aes(x=xvar)) + 
        ggplot2::geom_histogram(ggplot2::aes(y=..density..), color = border_color, fill = fill_color, binwidth = bins)+
        ggplot2::geom_density(color ="orange")
    } else if(density_only==TRUE & overlayDensisty==FALSE){
      df <- as.data.frame(df[,variable])
      names(df) <- "xvar"
      
      hist_p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = xvar))+
        ggplot2::geom_histogram(color = border_color, fill = fill_color, binwidth = bins)
    } else{
      df <- as.data.frame(df[,variable])
      names(df) <- "xvar"
      
      hist_p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = xvar))+
        ggplot2::geom_histogram(color = border_color, fill = fill_color, binwidth = bins)
    }
    
    hist_p <-
      hist_p+ggplot2::theme_minimal()+ ggplot2::xlab(ifelse(!is.null(xlab), xlab, "")) + ggplot2::ylab(ifelse(!is.null(ylab), ylab, "")) +
      ggplot2::ggtitle(ifelse(!is.null(plot_title), plot_title, "")) + ggplot2::labs(caption = paste0("Copyright © ", lubridate::year(Sys.Date())," , APHRC, All Rights Reserved")) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = as.numeric(title_pos),
          size = as.integer(title_size),
          colour = "black"
        ),plot.caption = ggplot2::element_text(hjust=1, color = "#808080"),
        axis.text.x = ggplot2::element_text(size = as.integer(axis_text_size), colour = "black", angle = as.numeric(axistext_angle)),
        axis.text.y = ggplot2::element_text(size = as.integer(axis_text_size), colour = "black"),
        axis.title = ggplot2::element_text(size = as.integer(axis_title_size), colour = "black")
      )
    
  } else{
    hist_p <- ggplot2::ggplot()
  }
  
  return(hist_p)
}
