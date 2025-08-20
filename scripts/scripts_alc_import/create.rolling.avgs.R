create.rolling.avgs <- function(df, vars) {
  # Capture the list of variable names as character
  vars <- as.character(vars)
  
  df <- df %>%
    group_by(id) %>%
    filter(n() > 0) %>% # Remove any empty groups
    mutate(across(all_of(vars), ~ ifelse(is.na(.), NA_real_, suppressWarnings(as.numeric(.))))) %>% # Ensure the variables are numeric
    mutate(
      across(all_of(vars), ~ rollapply(., 3, function(x) mean(x, na.rm = TRUE), align = "center", fill = NA, partial = TRUE), .names = "{.col}.3roll"),
      across(all_of(vars), ~ rollapply(., 5, function(x) mean(x, na.rm = TRUE), align = "center", fill = NA, partial = TRUE), .names = "{.col}.5roll")
    )
  
  return(df)
}
