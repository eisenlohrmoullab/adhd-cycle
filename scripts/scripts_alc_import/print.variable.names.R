print.variable.names <- function(df) {
  variable_names <- names(df)
  formatted_list <- paste(variable_names, collapse = ", ")
  cat("\n", "-------- Names of variables in the dataset --------   ", "\n", formatted_list)
}