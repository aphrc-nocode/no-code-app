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
      group_by(.data[[xvar]]) %>% 
      summarise(total = n()) %>% filter(!is.na(total))%>%
      mutate(
        prop1 = round(total / sum(total) * 100, 2),
        prop = paste0(round((total / sum(total)) * 100, 2), "%")
      )
    
    names(df_proportions) <- c("xvar1","total", "prop1","prop")
    
    p <- ggplot(data = df_proportions, aes(x = xvar1, y = prop1)) +
      geom_bar(stat = "identity", fill = default_col) +
      geom_text(
        aes(x = xvar1, y = prop1 + 1.5, label = prop),
        size =  as.integer(data_label_size)
      )
    
  }
  
  else if((is.null(yvar) || yvar == "") && (is.null(colorVar)|| colorVar == "") && vertical==FALSE){
    df_proportions <- df%>%group_by(.data[[xvar]])%>%
      summarise(total = n())%>%filter(!is.na(total))%>%mutate(prop1 = round(total/sum(total)*100,2),
                                                              prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <- c("xvar1","total", "prop1","prop")
    
    p <-  ggplot(data = df_proportions, aes(x = xvar1, y=prop1))+geom_bar(stat = "identity", fill = default_col)+
      geom_text(aes(x = xvar1, y = prop1 + 3, label =prop),
                size = as.integer(data_label_size))+ylim(c(NA, max(df_proportions$prop1) + 5))+coord_flip() 
    
    
    
  }
  
  else if((is.null(yvar) || yvar == "") && !is.null(colorVar) && stackedtype==TRUE && vertical==TRUE){
    df_proportions <- df%>%group_by(.data[[xvar]], .data[[colorVar]])%>%
      summarise(total = n())%>%filter(!is.na(total))%>%mutate(prop1 = round(total/sum(total)*100,2),
                                                              prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <- c("xvar1", "var2","total", "prop1","prop")
    
    p <- ggplot(data = df_proportions, aes(x = xvar1, y=prop1, fill = var2))+
      geom_bar(stat = "identity")+
      geom_text(aes(x = xvar1, y = prop1+1, label = prop), 
                position = position_stack(vjust = 0.5), size = as.integer(data_label_size))+
      guides(fill=guide_legend(title= ifelse(is.null(legend_title), "Legend", legend_title)))
    
    
    
  }
  
  
  else if((is.null(yvar) || yvar == "") && !is.null(colorVar) && stackedtype==TRUE && vertical==FALSE){
    df_proportions <- df%>%group_by(.data[[xvar]], .data[[colorVar]])%>%
      summarise(total = n())%>%filter(!is.na(total))%>%mutate(prop1 = round(total/sum(total)*100,2),
                                                              prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <- c("xvar1", "var2","total", "prop1","prop")
    
    p <- ggplot(data = df_proportions, aes(x = xvar1, y=prop1, fill = var2))+
      geom_bar(stat = "identity")+
      geom_text(aes(x = xvar1, y = prop1+1, label = prop), 
                position = position_stack(vjust = 0.5), size = as.integer(data_label_size))+coord_flip()+
      guides(fill=guide_legend(title=ifelse(is.null(legend_title), "Legend", legend_title)))
    
    
  }
  
  else if((is.null(yvar) || yvar == "") && !is.null(colorVar) && stackedtype==FALSE && vertical==TRUE){
    
    df_proportions <-
      df %>% group_by(.data[[xvar]], .data[[colorVar]]) %>%
      summarise(total = n()) %>%filter(!is.na(total))%>% mutate(prop1 = round(total / sum(total) *
                                                                                100, 2),
                                                                prop = paste0(round((total / sum(
                                                                  total
                                                                )) * 100, 2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <-
      ggplot(data = df_proportions, aes(x = xvar1, y = prop1, group = var2))+
      geom_bar(
        stat = "identity",
        aes(fill = var2),
        position = "dodge",
        width = as.numeric(bar_width)
      ) +
      geom_text(
        aes(label = prop, group = var2),
        vjust = 0,
        position = position_dodge(width  = as.numeric(bar_width)),
        size = as.integer(data_label_size)
      ) +
      guides(fill = guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))
    
    
  }
  
  
  else if((is.null(yvar) || yvar == "") && !is.null(colorVar) && stackedtype==FALSE && vertical==FALSE){
    
    df_proportions <-
      df %>% group_by(.data[[xvar]], .data[[colorVar]]) %>%
      summarise(total = n()) %>%filter(!is.na(total))%>% mutate(prop1 = round(total / sum(total) *
                                                                                100, 2),
                                                                prop = paste0(round((total / sum(
                                                                  total
                                                                )) * 100, 2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <-
      ggplot(data = df_proportions, aes(x = xvar1, y = prop1, group = var2))+
      geom_bar(
        stat = "identity",
        aes(fill = var2),
        position = "dodge",
        width = as.numeric(bar_width)
      ) +
      geom_text(
        aes(label = prop, group = var2),
        hjust = 0,
        position = position_dodge(width  = as.numeric(bar_width)),
        size = as.integer(data_label_size)
      ) +
      guides(fill = guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))+ylim(c(NA, max(df_proportions$prop1)+5))+coord_flip()
    
    
  }
  
  
  
  else if((!is.null(yvar) && yvar!="")  && (is.null(colorVar)||colorVar=="") && vertical==TRUE ){
    
    df_proportions <- df%>%group_by(.data[[xvar]])%>%
      summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%filter(!is.na(total))%>%mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "total", "prop1", "prop")
    
    p <- ggplot(data = df_proportions, aes(x = xvar1, y = prop1))+geom_bar(stat = "identity", fill= default_col)+
      geom_text(aes(x = xvar1, y = prop1+2, label = prop), size = as.integer(data_label_size))
    
    
  }
  
  else if((!is.null(yvar) && yvar!="")  && (is.null(colorVar)||colorVar=="") && vertical==FALSE ){
    
    df_proportions <- df%>%group_by(.data[[xvar]])%>%
      summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%filter(!is.na(total))%>%mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "total", "prop1", "prop")
    
    p <- ggplot(data = df_proportions, aes(x = xvar1, y = prop1))+geom_bar(stat = "identity", fill= default_col)+
      geom_text(aes(x = xvar1, y = prop1+2, label = prop), size = as.integer(data_label_size), vjust =.5, hjust = 0)+coord_flip()
    
    
  }
  
  else if(!is.null(yvar) && yvar !="" && !is.null(colorVar) && colorVar != "" && stackedtype==TRUE && vertical==TRUE){
    df_proportions <- df%>%group_by(.data[[xvar]], .data[[colorVar]])%>%
      summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%filter(!is.na(total))%>%mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <- ggplot(data = df_proportions, aes(x = xvar1, y=prop1, fill = var2))+geom_bar(stat = "identity", width = as.numeric(bar_width))+
      geom_text(aes(x = xvar1, y = prop1+2, label = prop), 
                position = position_stack(vjust = .5), size = as.integer(data_label_size))+
      guides(fill = guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))
    
  } 
  
  
  else if(!is.null(yvar) && yvar !="" && !is.null(colorVar) && colorVar != "" && stackedtype==TRUE && vertical==FALSE){
    df_proportions <- df%>%group_by(.data[[xvar]], .data[[colorVar]])%>%
      summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%filter(!is.na(total))%>%mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <- ggplot(data = df_proportions, aes(x = xvar1, y=prop1, fill = var2))+geom_bar(stat = "identity", width = as.numeric(bar_width))+
      geom_text(aes(x = xvar1, y = prop1+1, label = prop), 
                position = position_stack(vjust = .5), size = as.integer(data_label_size))+coord_flip()+
      guides(fill = guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))
    
  }
  
  
  else if(!is.null(yvar) && yvar !="" && !is.null(colorVar) && colorVar != "" && stackedtype==FALSE && vertical==TRUE){
    df_proportions <- df%>%group_by(.data[[xvar]], .data[[colorVar]])%>%
      summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%filter(!is.na(total))%>%mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <- ggplot(data = df_proportions, aes(x = xvar1, y=prop1, fill = var2))+geom_bar(stat = "identity", position = "dodge")+
      geom_text(aes(label = prop),size = as.integer(data_label_size),
                position = position_dodge(width = as.numeric(bar_width)), vjust = -0.5)+
      guides(fill = guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))
    
  } 
  
  else if(!is.null(yvar) && yvar !="" && !is.null(colorVar) && colorVar != "" && stackedtype==FALSE && vertical==FALSE){
    df_proportions <- df%>%group_by(.data[[xvar]], .data[[colorVar]])%>%
      summarise(total = sum(.data[[yvar]], na.rm = TRUE))%>%filter(!is.na(total))%>%mutate(prop1 = round(total/sum(total)*100,2),
                                                                                           prop = paste0(round((total/sum(total))*100,2), "%"))
    
    names(df_proportions) <-
      c("xvar1", "var2", "total", "prop1", "prop")
    
    p <-
      ggplot(data = df_proportions, aes(x = xvar1, y = prop1, group = var2))+
      geom_bar(
        stat = "identity",
        aes(fill = var2),
        position = "dodge",
        width = as.numeric(bar_width)
      ) +
      geom_text(
        aes(label = prop, group = var2),
        hjust = 0,
        position = position_dodge(width  = as.numeric(bar_width)),
        size = as.integer(data_label_size)
      ) +
      guides(fill = guide_legend(title = ifelse(
        is.null(legend_title), "Legend", legend_title
      )))+ylim(c(NA, max(df_proportions$prop1)+5)) +coord_flip()
    
  } 
  
  p <-
    p +theme_minimal()+ xlab(ifelse(!is.null(xlab), xlab, "")) + ylab(ifelse(!is.null(ylab), ylab, "")) +
    ggtitle(ifelse(!is.null(plot_title), plot_title, "")) + labs(caption = paste0("Copyright © ", year(Sys.Date())," ,APHRC, All Rights Reserved")) +
    theme(
      plot.title = element_text(
        hjust = as.numeric(title_pos),
        size = as.integer(title_size),
        colour = "black"
      ),plot.caption = element_text(hjust=1, color = "#808080"),
      axis.text.x = element_text(size = as.integer(axis_text_size), colour = "black", angle = as.numeric(axistext_angle)),
      axis.text.y = element_text(size = as.integer(axis_text_size), colour = "black"),
      axis.title = element_text(size = as.integer(axis_title_size), colour = "black"),
      legend.position = ifelse(is.null(yvar) && is.null(colorVar), "none", "bottom")
      
    )+scale_fill_brewer(palette=colorbrewer)
  
  return(p)
}


