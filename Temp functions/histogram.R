
histogram <- function(df, variable, bin_width = 10, title = NULL, xlab = NULL, ylab = NULL,density_only = FALSE,
                      fill_color = "#0077B6", border_color = "white", overlayDensisty = FALSE, plot_title = NULL,
                      axis_text_size = 18, axis_title_size = 24, title_size = 28, axistext_angle = 0, title_pos =.5) {
  
  df <- na.omit(df[, c(variable), drop=FALSE])
  
    if(is.numeric(df[[variable]])){
      if (is.null(bin_width)) {
        # Use the Freedman-Diaconis rule for bin width calculation
        iqr <- IQR(df[,variable])
        bins <- 2 * iqr / (nrow(df) ^ (1/3))
        bins <- ifelse(is.finite(bins) && bins > 0, bins, 1)
      }else{
        bins = bin_width
      }
      
      if(density_only==TRUE & overlayDensisty==TRUE){
        df <- as.data.frame(df[,variable])
        names(df) <- "xvar"
        
        hist_p <- ggplot(data = df, mapping = aes(x = xvar))+
          geom_density(color = "black", fill = fill_color)
      } else if (density_only==FALSE & overlayDensisty==TRUE){
        df <- as.data.frame(df[,variable])
        names(df) <- "xvar"
        
        hist_p <- ggplot(df, aes(x=xvar)) + 
          geom_histogram(aes(y=..density..), color = border_color, fill = fill_color, binwidth = bins)+
          geom_density(color ="orange")
      } else if(density_only==TRUE & overlayDensisty==FALSE){
        df <- as.data.frame(df[,variable])
        names(df) <- "xvar"
        
        hist_p <- ggplot(data = df, mapping = aes(x = xvar))+
          geom_histogram(color = border_color, fill = fill_color, binwidth = bins)
      } else{
        df <- as.data.frame(df[,variable])
        names(df) <- "xvar"
        
        hist_p <- ggplot(data = df, mapping = aes(x = xvar))+
          geom_histogram(color = border_color, fill = fill_color, binwidth = bins)
      }

      hist_p <-
        hist_p+theme_minimal()+ xlab(ifelse(!is.null(xlab), xlab, "")) + ylab(ifelse(!is.null(ylab), ylab, "")) +
        ggtitle(ifelse(!is.null(plot_title), plot_title, "")) + labs(caption = paste0("Copyright © ", year(Sys.Date())," , APHRC, All Rights Reserved")) +
        theme(
          plot.title = element_text(
            hjust = as.numeric(title_pos),
            size = as.integer(title_size),
            colour = "black"
          ),plot.caption = element_text(hjust=1, color = "#808080"),
          axis.text.x = element_text(size = as.integer(axis_text_size), colour = "black", angle = as.numeric(axistext_angle)),
          axis.text.y = element_text(size = as.integer(axis_text_size), colour = "black"),
          axis.title = element_text(size = as.integer(axis_title_size), colour = "black")
          
        )
      
    } else{
      hist_p <- ggplot()
    }
 
  return(hist_p)
  
}

#test
#histogram(df = mtcars, variable = "mpg", density_only = TRUE, overlayDensisty = TRUE)

