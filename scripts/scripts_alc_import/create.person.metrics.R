# Create a function to calculate person-level metrics for a given variable
create.person.metrics <- function(df, var, id) {
  # Capture the variable name
  var <- enquo(var)
  
  # Create person means for the specified variable, grouped by the id
  df <- df %>%
    group_by({{ id }}) %>%
    mutate(!!paste0(quo_name(var), ".m") := mean(!!var, na.rm = TRUE)) %>%
    ungroup()
  
  # Create person standard deviations for the specified variable, grouped by the id
  df <- df %>%
    group_by({{ id }}) %>%
    mutate(!!paste0(quo_name(var), ".sd") := sd(!!var, na.rm = TRUE)) %>%
    ungroup()
  
  # Create person deviations from the mean for the specified variable
  df <- df %>%
    mutate(!!paste0(quo_name(var), ".d") := (!!var) - .data[[paste0(quo_name(var), ".m")]])
  
  # Create person standardized values (z-scores) for the specified variable
  df <- df %>%
    mutate(!!paste0(quo_name(var), ".zd") := (.data[[quo_name(var)]] - .data[[paste0(quo_name(var), ".m")]]) / .data[[paste0(quo_name(var), ".sd")]])
  
  # Return the modified dataframe with the new metrics
  return(df)
}
