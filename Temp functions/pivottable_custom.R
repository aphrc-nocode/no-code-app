library(dplyr)
library(tidyr)

createPivotTable <- function(df, row_var, col_var) {
  
  pivot_data <- df %>%
    pivot_longer(cols = matches(value_var),
                 names_to = col_var,
                 values_to = "Value") %>%
    pivot_wider(names_from = col_var,
                values_from = "Value")
  
  return(pivot_data)
}
