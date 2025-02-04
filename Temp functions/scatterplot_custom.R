
scatterplot_custom <- function(df, xvar, yvar, colorVar=NULL, line_size = 0.9,addsmooth = "none", seval =TRUE,addshape=TRUE, shapes = 1,
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
  
  if(is.null(colorVar)||colorVar=="" && !is.null(xvar) && xvar!="" && !is.null(yvar) && yvar!=""){
    
    df1 <- df[, c(xvar, yvar)]
    names(df1) <- c("xvar1", "yvar1")
    
    if(addsmooth == "none"){
      p <- ggplot(data = df1, aes(x= xvar1, y = yvar1))+
        geom_point(shape = shapes, colour = default_col, size = line_size+.5)
    } else if(addsmooth != "none"){
      
      p <- ggplot(data = df1, aes(x= xvar1, y = yvar1))+
        geom_point(shape = shapes, colour = default_col, size = line_size+.5)+geom_smooth(se = seval, method = addsmooth, colour = default_col, linewidth = line_size, level = confelev)
    }
  }
  else if(!is.null(colorVar) && colorVar!="" && addshape==FALSE && !is.null(xvar) && xvar!="" && !is.null(yvar) && yvar!=""){
    df1 <- df[, c(xvar, yvar, colorVar)]
    names(df1) <- c("xvar1", "yvar1", "colorvar1")
    df1$colorvar1 <- as.factor(df1$colorvar1 )
    if(addsmooth == "none"){
      p <- ggplot(data = df1, aes(x= xvar1, y = yvar1, fill = colorvar1))+
        geom_point(size = line_size+.5)+
        guides(fill = guide_legend(title = ifelse(
          is.null(legend_title), "Legend", legend_title
        )))+scale_fill_brewer(palette=colorbrewer)
    } else if(addsmooth != "none"){
      
      p <- ggplot(data = df1, aes(x= xvar1, y = yvar1, fill = colorvar1))+
        geom_point(size = line_size+.5)+geom_smooth(se = seval, method = addsmooth, formula = y~x,
                                                    linewidth = line_size, level = confelev)+
        guides(fill = guide_legend(title = ifelse(
          is.null(legend_title), "Legend", legend_title
        )))+scale_fill_brewer(palette=colorbrewer)
    }
    
    
  }
  
  else if(!is.null(colorVar) && colorVar!="" && addshape==TRUE && !is.null(xvar) && xvar!="" && !is.null(yvar) && yvar!=""){
    df1 <- df[, c(xvar, yvar, colorVar)]
    names(df1) <- c("xvar1", "yvar1", "colorvar1")
    df1$colorvar1 <- as.factor(df1$colorvar1 )
    if(addsmooth == "none"){
      p <- ggplot(data = df1, aes(x= xvar1, y = yvar1, fill = colorvar1, shape = colorvar1))+
        geom_point(size = line_size+.5)+
        guides(fill = guide_legend(title = ifelse(
          is.null(legend_title), "Legend", legend_title
        )))+scale_fill_brewer(palette=colorbrewer)
    } else if (addsmooth != "none") {
      
      p <- ggplot(data = df1, aes(x= xvar1, y = yvar1, fill = colorvar1, shape = colorvar1))+
        geom_point(size = line_size+.5)+geom_smooth(se = seval, method = addsmooth, formula = y~x,
                                                    linewidth = line_size, level = confelev)+
        guides(fill = guide_legend(title = ifelse(
          is.null(legend_title), "Legend", legend_title
        )))+scale_fill_brewer(palette=colorbrewer)
    }
    
    
  }
  
  p <-
    p +theme_minimal()+ xlab(ifelse(!is.null(xlab), xlab, "")) + ylab(ifelse(!is.null(ylab), ylab, "")) +
    ggtitle(ifelse(!is.null(plot_title), plot_title, "")) + labs(caption = paste0("Copyright © ", year(Sys.Date())," , APHRC, All Rights Reserved")) + theme(
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
  
  return(p)
}



