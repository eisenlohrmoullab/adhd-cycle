convert_date_if_needed <- function(df, date_column) {
  # Check if the date column is in Date format
  if (!inherits(df[[date_column]], "Date")) {
    df <- df %>%
      mutate(!!date_column := lubridate::mdy(.data[[date_column]])) # Convert to Date format if not already in Date format
  }
  return(df)
}
