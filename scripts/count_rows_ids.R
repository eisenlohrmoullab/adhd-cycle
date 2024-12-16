count_rows_ids <- function(df) {
  cat("-------The number of unique IDs in the raw dataset-------")
  cat("\n", "Number of rows: ", nrow(df), "\n")
  cat("\n", "Number of unique ids: ", n_distinct(df$id), "\n")
  
  # Get a list of unique IDs in the dataset
  id_list <- unique(df$id)
  
  # Print the number of unique IDs and the list of unique IDs in a sentence
  cat("\n", "The id values are:", "\n", "\n", paste(id_list, collapse = ", "), "\n", "\n")
}