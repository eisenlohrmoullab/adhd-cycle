# Make sure to install the 'slider' package first if you haven't already
# install.packages("slider")
library(dplyr)
library(slider)

preprocess_outcome <- function(data, outcome) {
  outcome_log <- paste0(outcome, "_log")
  outcome_log_d <- paste0(outcome, "_log.d")
  outcome_roll <- paste0(outcome, "_log.d.roll")
  
  data %>%
    mutate(id = as.factor(id)) %>%
    mutate(!!outcome_log := log(.data[[outcome]] + 1)) %>%
    group_by(id) %>%
    mutate(!!outcome_log_d := .data[[outcome_log]] - mean(.data[[outcome_log]], na.rm = TRUE)) %>%
    # Add the 5-day centered rolling average using slider with a partial mean.
    # The .before and .after arguments specify the window size (5 days total).
    mutate(!!outcome_roll := slider::slide_dbl(
      .x = .data[[outcome_log_d]],
      .f = mean,
      .before = 2,
      .after = 2,
      .partial = TRUE,
      .na_rm = TRUE
    )) %>%
    ungroup()
}