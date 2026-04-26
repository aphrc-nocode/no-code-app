source("server/user_defined_chart_server.R", local = TRUE)
source("server/user_defined_table_server.R", local = TRUE)

user_defined_server <- function(plots_custom_rv) {
  user_defined_chart_server(
    input = input,
    output = output,
    session = session,
    rv_current = rv_current,
    plots_custom_rv = plots_custom_rv
  )
  
  user_defined_table_server(
    input = input,
    output = output,
    session = session,
    rv_current = rv_current,
    plots_custom_rv = plots_custom_rv
  )
}