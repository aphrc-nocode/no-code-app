#' Bar Plot Function
#'
#' @description Generates a bar plot with various customization options.
#' @param df A data frame containing the data to be plotted.
#' @param xvar The categorical variable for the x-axis.
#' @param yvar The numerical variable for the y-axis (optional).
#' @param vertical Logical; whether the bars should be vertical (default: TRUE).
#' @param stackedtype Logical; whether the bars should be stacked (default: TRUE).
#' @param colorVar A categorical variable for coloring bars (optional).
#' @param bar_width Width of the bars (default: 0.9).
#' @param default_col Default fill color if colorVar is not provided (default: "blue").
#' @param plot_title Title of the plot (optional).
#' @param xlab Label for the x-axis (optional).
#' @param ylab Label for the y-axis (default: "Percentage").
#' @param title_pos Positioning of the title (default: 0.5).
#' @param title_size Size of the title text (default: 25).
#' @param axis_title_size Size of the axis titles (default: 20).
#' @param axis_text_size Size of the axis text (default: 16).
#' @param data_label_size Size of the data labels (default: 10).
#' @param axistext_angle Angle of x-axis text labels (default: 0).
#' @param legend_title Title of the legend (default: "").
#' @param colorbrewer Color palette from RColorBrewer (default: "Dark2").
#' 
#' @return A ggplot2::ggplot2 bar plot object.
#' 
#' @import ggplot2::ggplot2 dplyr tidyr RColorBrewer
#' @export
barplot <- function(df, xvar, yvar = NULL, vertical = TRUE, stackedtype = TRUE, colorVar = NULL, bar_width = 0.9,default_col = "blue",
                    plot_title = NULL, xlab = NULL, ylab = "Percentage", title_pos = 0.5, title_size = 25,
                    axis_title_size = 20, axis_text_size = 16, data_label_size = 10, axistext_angle = 0, legend_title = "", colorbrewer = "Dark2") {
  
  
  vars_col <-  c(xvar, yvar, colorVar)
  existing_vars <- c()
  for(var in vars_col) {
    if(var %in% names(df)) {
      existing_vars <- c(existing_vars, var)
    }
  }
  
  df <- na.omit(df[, existing_vars, drop=FALSE])
  
  if((is.null(yvar) || yvar == "") && (is.null(colorVar)|| colorVar == "") && vertical==TRUE){
    df_proportions <- df %>% 
      dplyr::group_by(.data[[xvar]]) %>% 
      dplyr::summarise(total = n()) %>% filter(!is.na(total))%>%
      dplyr::mutate(
        prop1 = round(total / sum(total) * 100, 2),
        prop = paste0(round((total / sum(total)) * 100, 2), "%")
      )
    
    names(df_proportions) <- c("xvar1","total", "prop1","prop")
    
    p <- ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y = prop1)) +
      ggplot2::geom_bar(stat = "identity", fill = default_col) +
      ggplot2::geom_text(
        ggplot2::aes(x = xvar1, y = prop1 + 1.5, label = prop),
        size =  as.integer(data_label_size)
      )
    
  }
  
  else if((is.null(yvar) || yvar == "") && (is.null(colorVar)|| colorVar == "") && vertical==FALSE){
    df_proportions <- df%>%group_by(.data[[xvar]])%>%
      dplyr::summarise(total = n())%>%dplyr::filter(!is.na(total))%>%dplyr::mutate(prop1 = round(total/sum(total)*100,2),
                                                              prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <- c("xvar1","total", "prop1","prop")
    
    p <-  ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y=prop1))+ggplot2::geom_bar(stat = "identity", fill = default_col)+
      ggplot2::geom_text(ggplot2::aes(x = xvar1, y = prop1 + 3, label =prop),
                size = as.integer(data_label_size))+ylim(c(NA, max(df_proportions$prop1) + 5))+ggplot2::coord_flip() 
    
    
    
  }
  
  else if((is.null(yvar) || yvar == "") && !is.null(colorVar) && stackedtype==TRUE && vertical==TRUE){
    df_proportions <- df%>%group_by(.data[[xvar]], .data[[colorVar]])%>%
      dplyr::summarise(total = n())%>%dplyr::filter(!is.na(total))%>%dplyr::mutate(prop1 = round(total/sum(total)*100,2),
                                                              prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <- c("xvar1", "var2","total", "prop1","prop")
    
    p <- ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y=prop1, fill = var2))+
      ggplot2::geom_bar(stat = "identity")+
      ggplot2::geom_text(ggplot2::aes(x = xvar1, y = prop1+1, label = prop), 
                position = position_stack(vjust = 0.5), size = as.integer(data_label_size))+
      ggplot2::guides(fill=ggplot2::guide_legend(title= ifelse(is.null(legend_title), "Legend", legend_title)))
    
    
    
  }
  
  
  else if((is.null(yvar) || yvar == "") && !is.null(colorVar) && stackedtype==TRUE && vertical==FALSE){
    df_proportions <- df%>%dplyr::group_by(.data[[xvar]], .data[[colorVar]])%>%
      dplyr::summarise(total = n())%>%dplyr::filter(!is.na(total))%>%dplyr::mutate(prop1 = round(total/sum(total)*100,2),
                                                              prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <- c("xvar1", "var2","total", "prop1","prop")
    
    p <- ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y=prop1, fill = var2))+
      ggplot2::geom_bar(stat = "identity")+
      ggplot2::geom_text(ggplot2::aes(x = xvar1, y = prop1+1, label = prop), 
                position = position_stack(vjust = 0.5), size = as.integer(data_label_size))+ggplot2::coord_flip()+
      ggplot2::guides(fill=ggplot2::guide_legend(title=ifelse(is.null(legend_title), "Legend", legend_title)))
    
    
  }
  
  else if((is.null(yvar) || yvar == "") && !is.null(colorVar) && stackedtype==FALSE && vertical==TRUE){
    
    df_proportions <-
      df %>% dplyr::group_by(.data[[xvar]], .data[[colorVar]]) %>%
      dplyr::summarise(total = n()) %>%dplyr::filter(!is.na(total))%>% dplyr::mutate(prop1 = round(total / sum(total) *
                                                                                100, 2),
                                                                prop = paste0(round((total / sum(
                                                                  total
                                                                )) * 100, 2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <-
      ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y = prop1, group = var2))+
      ggplot2::geom_bar(
        stat = "identity",
        ggplot2::aes(fill = var2),
        position = "dodge",
        width = as.numeric(bar_width)
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = prop, group = var2),
        vjust = 0,
        position = ggplot2::position_dodge(width  = as.numeric(bar_width)),
        size = as.integer(data_label_size)
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))
    
    
  }
  
  
  else if((is.null(yvar) || yvar == "") && !is.null(colorVar) && stackedtype==FALSE && vertical==FALSE){
    
    df_proportions <-
      df %>% dplyr::group_by(.data[[xvar]], .data[[colorVar]]) %>%
      dplyr::summarise(total = n()) %>%dplyr::filter(!is.na(total))%>% dplyr::mutate(prop1 = round(total / sum(total) *
                                                                                100, 2),
                                                                prop = paste0(round((total / sum(
                                                                  total
                                                                )) * 100, 2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <-
      ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y = prop1, group = var2))+
      ggplot2::geom_bar(
        stat = "identity",
        ggplot2::aes(fill = var2),
        position = "dodge",
        width = as.numeric(bar_width)
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = prop, group = var2),
        hjust = 0,
        position = ggplot2::position_dodge(width  = as.numeric(bar_width)),
        size = as.integer(data_label_size)
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))+ylim(c(NA, max(df_proportions$prop1)+5))+ggplot2::coord_flip()
    
    
  }
  
  
  
  else if((!is.null(yvar) && yvar!="")  && (is.null(colorVar)||colorVar=="") && vertical==TRUE ){
    
    df_proportions <- df%>%dplyr::group_by(.data[[xvar]])%>%
      dplyr::summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%dplyr::filter(!is.na(total))%>%dplyr::mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "total", "prop1", "prop")
    
    p <- ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y = prop1))+ggplot2::geom_bar(stat = "identity", fill= default_col)+
      ggplot2::geom_text(ggplot2::aes(x = xvar1, y = prop1+2, label = prop), size = as.integer(data_label_size))
    
    
  }
  
  else if((!is.null(yvar) && yvar!="")  && (is.null(colorVar)||colorVar=="") && vertical==FALSE ){
    
    df_proportions <- df%>%dplyr::group_by(.data[[xvar]])%>%
      dplyr::summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%dplyr::filter(!is.na(total))%>%dplyr::mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "total", "prop1", "prop")
    
    p <- ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y = prop1))+ggplot2::geom_bar(stat = "identity", fill= default_col)+
      ggplot2::geom_text(ggplot2::aes(x = xvar1, y = prop1+2, label = prop), size = as.integer(data_label_size), vjust =.5, hjust = 0)+ggplot2::coord_flip()
    
    
  }
  
  else if(!is.null(yvar) && yvar !="" && !is.null(colorVar) && colorVar != "" && stackedtype==TRUE && vertical==TRUE){
    df_proportions <- df%>%dplyr::group_by(.data[[xvar]], .data[[colorVar]])%>%
      dplyr::summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%dplyr::filter(!is.na(total))%>%dplyr::mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <- ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y=prop1, fill = var2))+ggplot2::geom_bar(stat = "identity", width = as.numeric(bar_width))+
      ggplot2::geom_text(ggplot2::aes(x = xvar1, y = prop1+2, label = prop), 
                position = position_stack(vjust = .5), size = as.integer(data_label_size))+
      ggplot2::guides(fill = ggplot2::guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))
    
  } 
  
  
  else if(!is.null(yvar) && yvar !="" && !is.null(colorVar) && colorVar != "" && stackedtype==TRUE && vertical==FALSE){
    df_proportions <- df%>%group_by(.data[[xvar]], .data[[colorVar]])%>%
      summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%filter(!is.na(total))%>%mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <- ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y=prop1, fill = var2))+ggplot2::geom_bar(stat = "identity", width = as.numeric(bar_width))+
      ggplot2::geom_text(ggplot2::aes(x = xvar1, y = prop1+1, label = prop), 
                position = position_stack(vjust = .5), size = as.integer(data_label_size))+ggplot2::coord_flip()+
      ggplot2::guides(fill = ggplot2::guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))
    
  }
  
  
  else if(!is.null(yvar) && yvar !="" && !is.null(colorVar) && colorVar != "" && stackedtype==FALSE && vertical==TRUE){
    df_proportions <- df%>%group_by(.data[[xvar]], .data[[colorVar]])%>%
      summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%filter(!is.na(total))%>%mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <- ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y=prop1, fill = var2))+ggplot2::geom_bar(stat = "identity", position = "dodge")+
      ggplot2::geom_text(ggplot2::aes(label = prop),size = as.integer(data_label_size),
                position = ggplot2::position_dodge(width = as.numeric(bar_width)), vjust = -0.5)+
      ggplot2::guides(fill = ggplot2::guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))
    
  } 
  
  else if(!is.null(yvar) && yvar !="" && !is.null(colorVar) && colorVar != "" && stackedtype==FALSE && vertical==FALSE){
    df_proportions <- df%>%dplyr::group_by(.data[[xvar]], .data[[colorVar]])%>%
      dplyr::summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%dplyr::filter(!is.na(total))%>%dplyr::mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <-
      ggplot2::ggplot(data = df_proportions, ggplot2::aes(x = xvar1, y = prop1, group = var2))+
      ggplot2::geom_bar(
        stat = "identity",
        ggplot2::aes(fill = var2),
        position = "dodge",
        width = as.numeric(bar_width)
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = prop, group = var2),
        hjust = 0,
        position = ggplot2::position_dodge(width  = as.numeric(bar_width)),
        size = as.integer(data_label_size)
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))+ylim(c(NA, max(df_proportions$prop1)+5)) +ggplot2::coord_flip()
    
  } 
  
  p <-
    p +ggplot2::theme_minimal()+ xlab(ifelse(!is.null(xlab), xlab, "")) + ylab(ifelse(!is.null(ylab), ylab, "")) +
    ggtitle(ifelse(!is.null(plot_title), plot_title, "")) + ggplot2::labs(caption = paste0("Copyright © ", year(Sys.Date())," ,APHRC, All Rights Reserved")) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = as.numeric(title_pos),
        size = as.integer(title_size),
        colour = "black"
      ),plot.caption = ggplot2::element_text(hjust=1, color = "#808080"),
      axis.text.x = ggplot2::element_text(size = as.integer(axis_text_size), colour = "black", angle = as.numeric(axistext_angle)),
      axis.text.y = ggplot2::element_text(size = as.integer(axis_text_size), colour = "black"),
      axis.title = ggplot2::element_text(size = as.integer(axis_title_size), colour = "black"),
      legend.position = ifelse(is.null(yvar) && is.null(colorVar), "none", "bottom")
      
    )+scale_fill_brewer(palette=colorbrewer)
  
  return(p)
}


