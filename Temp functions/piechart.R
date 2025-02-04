#' Generate a Pie or Doughnut Chart
#'
#' @param df A data frame containing the data.
#' @param xvar A string specifying the categorical variable for the pie chart.
#' @param plot_title The title of the plot. Default is "Pie Chart".
#' @param title_pos Position of the title (default = 0.5).
#' @param title_size Size of the title text (default = 28).
#' @param facet_var An optional string specifying a facet variable.
#' @param facet_title_size Size of facet labels (default = 24).
#' @param legend_title Title for the legend (default = NULL).
#' @param transform_to_doughnut Logical. If TRUE, creates a doughnut chart (default = FALSE).
#' @param data_label_size Size of data labels inside the pie chart (default = 4).
#' @param colorbrewer Color palette from RColorBrewer (default = "Paired").
#'
#' @return A ggplot2 pie or doughnut chart.
#' @import ggplot2 dplyr
#' @importFrom RColorBrewer brewer.pal
#' @export
pie_chart <- function(df, xvar, plot_title = "Pie Chart", title_pos = 0.5, title_size = 28, 
                      facet_var = NULL, facet_title_size = 24, legend_title = NULL, 
                      transform_to_doughnut = FALSE, data_label_size = 4, colorbrewer = "Paired") {
  
  # Check if the xvar exists in df
  if (!xvar %in% names(df)) {
    stop("Error: The variable ", xvar, " is not found in the dataset.")
  }
  
  # Filter relevant columns and remove missing values
  vars_col <- c(xvar, facet_var)
  existing_vars <- vars_col[vars_col %in% names(df)]
  df <- na.omit(df[, existing_vars, drop = FALSE])
  
  # Convert to factor
  df[[xvar]] <- as.factor(df[[xvar]])
  
  # Compute proportions
  df_prop <- df %>%
    dplyr::filter(!is.na(.data[[xvar]])) %>%
    dplyr::group_by(.data[[xvar]]) %>%
    dplyr::summarise(total = dplyr::n()) %>%
    dplyr::mutate(prop = (total / sum(total)) * 100) %>%
    dplyr::arrange(desc(.data[[xvar]])) %>%
    dplyr::mutate(lab.ypos = cumsum(prop) - 0.6 * prop)
  
  # Base plot
  p <- ggplot2::ggplot(df_prop, ggplot2::aes(x = 2, y = prop, fill = .data[[xvar]])) +
    ggplot2::geom_bar(stat = "identity", color = "#d3d3d3") +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::geom_text(ggplot2::aes(y = lab.ypos, label = paste0(round(prop), "%")), 
                       color = "black", size = as.integer(data_label_size))
  
  # Convert to doughnut if requested
  if (transform_to_doughnut) {
    p <- p + ggplot2::xlim(.8, 2.5)
  }
  
  # Apply theme and labels
  p <- p + ggplot2::theme_minimal() +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = ifelse(is.null(legend_title), "Legend", legend_title))) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = title_pos, size = title_size, colour = "black"),
      legend.position = "bottom",
      axis.line = ggplot2::element_blank(), 
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_brewer(palette = colorbrewer)
  
  # Handle facets
  if (!is.null(facet_var) && facet_var %in% names(df)) {
    df[[facet_var]] <- as.factor(df[[facet_var]])
    
    df_prop <- df %>%
      dplyr::group_by(.data[[facet_var]], .data[[xvar]]) %>%
      dplyr::summarise(total = dplyr::n()) %>%
      dplyr::mutate(prop = (total / sum(total)) * 100) %>%
      dplyr::arrange(desc(.data[[xvar]])) %>%
      dplyr::mutate(lab.ypos = cumsum(prop) - 0.6 * prop)
    
    p <- ggplot2::ggplot(df_prop, ggplot2::aes(x = 2, y = prop, fill = .data[[xvar]])) +
      ggplot2::geom_bar(stat = "identity", color = "#d3d3d3") +
      ggplot2::coord_polar(theta = "y", start = 0) +
      ggplot2::geom_text(ggplot2::aes(y = lab.ypos, label = paste0(round(prop), "%")), 
                         color = "black", size = as.integer(data_label_size)) +
      ggplot2::facet_wrap(. ~ .data[[facet_var]], ncol = 2) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = facet_title_size, colour = "black"))
    
    if (transform_to_doughnut) {
      p <- p + ggplot2::xlim(.8, 2.5)
    }
  }
  p <- p+ggplot2::labs(caption = paste0("Copyright © ", year(Sys.Date())," , APHRC, All Rights Reserved"))
  return(p)
}

# Example usage:
plot(pie_chart(df = mtcars, xvar = "gear", transform_to_doughnut = TRUE))
#plot(pie_chart(df = mtcars, xvar = "gear", transform_to_doughnut = TRUE, facet_var = "carb"))
