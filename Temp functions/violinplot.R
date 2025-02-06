#' Create a Customized Violin Plot
#'
#' @description This function generates a violin plot using ggplot2, with options for color, orientation, and titles.
#' @param df A data frame containing the data.
#' @param xvar A string specifying the x-axis variable.
#' @param yvar A string specifying the y-axis variable.
#' @param colorVar (Optional) A string specifying the color grouping variable.
#' @param plot_title (Optional) A string specifying the plot title.
#' @param vertical A logical indicating whether the plot should be vertical (default = TRUE).
#' @param xlab (Optional) A string specifying the x-axis label.
#' @param ylab (Optional) A string specifying the y-axis label.
#' @param title_pos A numeric value for the horizontal position of the title (default = 0.5).
#' @param title_size An integer specifying the title font size (default = 25).
#' @param default_col A string specifying the default fill color (default = "#0077B6").
#' @param axis_title_size An integer specifying the axis title font size (default = 20).
#' @param axis_text_size An integer specifying the axis text font size (default = 16).
#' @param axistext_angle A numeric value specifying the angle of the x-axis text (default = 0).
#' @param legend_title A string specifying the legend title (default = "").
#' @param colorbrewer A string specifying the color palette from RColorBrewer (default = "Dark2").
#' @return A ggplot object representing the violin plot.
#' @import ggplot2 RColorBrewer
#' @export

violin_custom <- function(df, xvar, yvar, colorVar = NULL, plot_title = NULL, vertical = TRUE, 
                          xlab = NULL, ylab = NULL, title_pos = 0.5, title_size = 25, default_col = "#0077B6",
                          axis_title_size = 20, axis_text_size = 16, axistext_angle = 0, legend_title = "", 
                          colorbrewer = "Dark2") {
  
  vars_col <- c(xvar, yvar, colorVar)
  existing_vars <- vars_col[vars_col %in% names(df)]
  df <- na.omit(df[, existing_vars, drop = FALSE])
  
  if (!is.null(yvar) && yvar != "") {
    
    if (vertical) {
      if ((!is.null(xvar) && xvar != "") && (is.null(colorVar) || colorVar == "")) {
        df1 <- data.frame(yvar1 = as.numeric(df[[yvar]]), xvar1 = as.factor(df[[xvar]]))
        p <- ggplot2::ggplot(data = df1, ggplot2::aes(y = yvar1, x = xvar1)) +
          ggplot2::geom_violin(fill = default_col, trim = FALSE)
      } else if ((!is.null(colorVar) && colorVar != "") && (is.null(xvar) || xvar == "")) {
        df1 <- data.frame(yvar1 = as.numeric(df[[yvar]]), colorvar1 = as.factor(df[[colorVar]]))
        p <- ggplot2::ggplot(data = df1, ggplot2::aes(y = yvar1, fill = colorvar1)) +
          ggplot2::geom_violin(trim = FALSE) +
          ggplot2::guides(fill = ggplot2::guide_legend(title = legend_title)) +
          ggplot2::scale_fill_brewer(palette = colorbrewer)
      } else {
        df1 <- data.frame(xvar1 = as.factor(df[[xvar]]), yvar1 = as.numeric(df[[yvar]]), colorvar1 = as.factor(df[[colorVar]]))
        p <- ggplot2::ggplot(data = df1, ggplot2::aes(y = yvar1, x = xvar1, fill = colorvar1)) +
          ggplot2::geom_violin(trim = FALSE) +
          ggplot2::guides(fill = ggplot2::guide_legend(title = legend_title)) +
          ggplot2::scale_fill_brewer(palette = colorbrewer)
      }
    } else {
      if ((!is.null(yvar) && yvar != "") && (is.null(colorVar) || colorVar == "") && (!is.null(xvar) && xvar != "")) {
        df1 <- data.frame(yvar1 = as.numeric(df[[yvar]]), xvar1 = as.factor(df[[xvar]]))
        p <- ggplot2::ggplot(data = df1, ggplot2::aes(y = yvar1, x = xvar1)) +
          ggplot2::geom_violin(trim = FALSE, fill = default_col) + ggplot2::coord_flip()
      } else if ((!is.null(colorVar) && colorVar != "") && (is.null(xvar) || xvar == "")) {
        df1 <- data.frame(yvar1 = as.numeric(df[[yvar]]), colorvar1 = as.factor(df[[colorVar]]))
        p <- ggplot2::ggplot(data = df1, ggplot2::aes(y = yvar1, fill = colorvar1)) +
          ggplot2::geom_violin(trim = FALSE) + ggplot2::guides(fill = ggplot2::guide_legend(title = legend_title)) +
          ggplot2::coord_flip() + ggplot2::scale_fill_brewer(palette = colorbrewer)
      } else {
        df1 <- data.frame(xvar1 = as.factor(df[[xvar]]), yvar1 = as.numeric(df[[yvar]]), colorvar1 = as.factor(df[[colorVar]]))
        p <- ggplot2::ggplot(data = df1, ggplot2::aes(y = yvar1, x = xvar1, fill = colorvar1)) +
          ggplot2::geom_violin(trim = FALSE) + ggplot2::guides(fill = ggplot2::guide_legend(title = legend_title)) +
          ggplot2::coord_flip() + ggplot2::scale_fill_brewer(palette = colorbrewer)
      }
    }
    
    p <- p +
      ggplot2::theme_minimal() +
      ggplot2::xlab(ifelse(!is.null(xlab), xlab, "")) +
      ggplot2::ylab(ifelse(!is.null(ylab), ylab, "")) +
      ggplot2::ggtitle(ifelse(!is.null(plot_title), plot_title, "")) +
      ggplot2::labs(caption = paste0("Copyright © ", format(Sys.Date(), "%Y"), " , APHRC, All Rights Reserved")) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = title_pos, size = title_size, colour = "black"),
        plot.caption = ggplot2::element_text(hjust = 1, color = "#808080"),
        axis.text.x = ggplot2::element_text(size = axis_text_size, colour = "black", angle = axistext_angle),
        axis.text.y = ggplot2::element_text(size = axis_text_size, colour = "black"),
        axis.title = ggplot2::element_text(size = axis_title_size, colour = "black"),
        legend.position = ifelse(is.null(colorVar), "none", "bottom")
      )
  } else {
    p <- ggplot2::ggplot()
  }
  
  return(p)
}


#violin_custom(df = mtcars, xvar = "cyl",vertical = TRUE, yvar = NULL)
#violin_custom(df = mtcars, yvar = "mpg", colorVar = "gear", xvar = "carb", vertical = TRUE )
