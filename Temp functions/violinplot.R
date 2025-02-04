
library(ggplot2)
violin_custom <- function(df, xvar, yvar, colorVar = NULL,plot_title = NULL, vertical = TRUE, xlab = NULL, ylab = NULL, title_pos = 0.5, title_size = 25, default_col = "#0077B6",
                          axis_title_size = 20, axis_text_size = 16, axistext_angle = 0, legend_title = "", colorbrewer = "Dark2") {
  
  
  vars_col <-  c(xvar, yvar, colorVar)
  existing_vars <- c()
  for(var in vars_col) {
    if(var %in% names(df)) {
      existing_vars <- c(existing_vars, var)
    }
  }
  
  df <- na.omit(df[, existing_vars])
  
  if( yvar!="" && !is.null(yvar)){
    if(vertical ==TRUE){
      
      if((!is.null(xvar) && xvar !="") && (is.null(colorVar)||colorVar =="") &&  (!is.null(yvar) && yvar !="")){
        df1 <- as.data.frame(df[, c(yvar, xvar)])
        names(df1) <- c("yvar1", "xvar1")
        df1$yvar1 <- as.numeric(df1$yvar1)
        df1$xvar1 <- as.factor(df1$xvar1)
        
        p <- ggplot(data = df1 , aes(y= yvar1, x = xvar1))+
          geom_violin(fill = default_col, trim = FALSE)
      }
      
      else if((!is.null(colorVar) && colorVar !="") && (!is.null(yvar) &&yvar !="") &&  (is.null(xvar) || xvar =="")){
        df1 <- as.data.frame(df[, c(yvar, colorVar)])
        names(df1) <- c("yvar1", "colorvar1")
        df1$yvar1 <- as.numeric(df1$yvar1)
        df1$colorvar1 <- as.factor(df1$colorvar1)
        
        p <- ggplot(data = df1 , aes(y= yvar1, fill = colorvar1))+
          geom_violin(trim=FALSE)+
          guides(color = guide_legend(title = ifelse(
            is.null(legend_title), "Legend", legend_title
          )))+scale_fill_brewer(palette=colorbrewer)
      }
      else if((!is.null(colorVar) && colorVar !="") && (!is.null(xvar) && xvar !="") && (!is.null(yvar) && yvar !="")){
        df1 <- as.data.frame(df[, c(xvar, yvar, colorVar)])
        names(df1) <- c("xvar1","yvar1", "colorvar1")
        df1$yvar1 <- as.numeric(df1$yvar1)
        df1$colorvar1 <- as.factor(df1$colorvar1)
        df1$xvar1 <- as.factor(df1$xvar1)
        
        p <- ggplot(data = df1 , aes(y= yvar1, x= xvar1, fill = colorvar1))+
          geom_violin(trim=FALSE)+
          guides(color = guide_legend(title = ifelse(
            is.null(legend_title), "Legend", legend_title
          )))+scale_fill_brewer(palette=colorbrewer)
      }
      
    }else if (vertical == FALSE){
      if((!is.null(yvar) && yvar !="") && (is.null(colorVar)||colorVar =="") &&  (!is.null(xvar) && xvar !="")){
        df1 <- as.data.frame(df[, c(yvar, xvar)])
        names(df1) <- c("yvar1", "xvar1")
        df1$yvar1 <- as.numeric(df1$yvar1)
        df1$xvar1 <- as.factor(df1$xvar1)
        
        p <- ggplot(data = df1 , aes(y= yvar1, x = xvar1))+
          geom_violin(trim=FALSE, fill = default_col)+coord_flip()
      }
      
      else if((!is.null(colorVar) && colorVar !="") && (is.null(xvar)||xvar =="") &&  (!is.null(yvar) && yvar !="")){
        df1 <- as.data.frame(df[, c(yvar, colorVar)])
        names(df1) <- c("yvar1", "colorvar1")
        df1$yvar1 <- as.numeric(df1$yvar1)
        df1$colorvar1 <- as.factor(df1$colorvar1)
        
        p <- ggplot(data = df1 , aes(y= yvar1, fill = colorvar1))+
          geom_violin(trim=FALSE)+guides(fill = guide_legend(title = ifelse(
            is.null(legend_title), "Legend", legend_title
          )))+coord_flip()+scale_fill_brewer(palette=colorbrewer)
      }
      else if((!is.null(colorVar) && colorVar !="") && (!is.null(xvar) && xvar !="") &&  (!is.null(yvar) && yvar !="")){
        df1 <- as.data.frame(df[, c(xvar, yvar, colorVar)])
        names(df1) <- c("xvar1","yvar1", "colorvar1")
        df1$yvar1 <- as.numeric(df1$yvar1)
        df1$colorvar1 <- as.factor(df1$colorvar1)
        df1$xvar1 <- as.factor(df1$xvar1)
        
        p <- ggplot(data = df1 , aes(y= yvar1, x= xvar1, group = colorvar1))+
          geom_violin(trim=FALSE, aes(fill = colorvar1))+guides(fill = guide_legend(title = ifelse(
            is.null(legend_title), "Legend", legend_title
          )))+coord_flip()+scale_fill_brewer(palette=colorbrewer)
      }
      
    }
    
    p <-
      p +theme_minimal()+ xlab(ifelse(!is.null(xlab), xlab, "")) + ylab(ifelse(!is.null(ylab), ylab, "")) +
      ggtitle(ifelse(!is.null(plot_title), plot_title, "")) +labs(caption = paste0("Copyright © ", year(Sys.Date())," ,APHRC, All Rights Reserved")) +
      theme(
        plot.title = element_text(
          hjust = as.numeric(title_pos),
          size = as.integer(title_size),
          colour = "black"
        ),plot.caption = element_text(hjust=1, color = "#808080"),
        axis.text.x = element_text(size = as.integer(axis_text_size), colour = "black", angle = as.numeric(axistext_angle)),
        axis.text.y = element_text(size = as.integer(axis_text_size), colour = "black"),
        axis.title = element_text(size = as.integer(axis_title_size), colour = "black"),
        legend.position = ifelse(is.null(colorVar), "none", "bottom")
        
      )
  } else{
    p <- ggplot()
  }
  
  
  return(p)
}  

#violin_custom(df = mtcars, xvar = "cyl",vertical = TRUE, yvar = NULL)
#violin_custom(df = mtcars, yvar = "mpg", colorVar = "gear", xvar = "carb", vertical = TRUE )
