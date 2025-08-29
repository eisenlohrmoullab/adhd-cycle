library(tidyverse)
library(dplyr)


subset_by_phase <- function(data, outcome, time_var = "scaled_cycleday_impute", min_obs = 5) {
  library(dplyr)
  library(tidyr)
  
  # 1️⃣ Add phase variable
  data <- data %>%
    mutate(
      phase = case_when(
        .data[[time_var]] >= -1 & .data[[time_var]] < 0 ~ "luteal",
        .data[[time_var]] >= 0 & .data[[time_var]] <= 1 ~ "follicular",
        TRUE ~ NA_character_
      )
    )
  
  # 2️⃣ Count non-missing observations by phase
  id_counts <- data %>%
    filter(!is.na(.data[[outcome]]), !is.na(phase)) %>%
    group_by(id, phase) %>%
    summarise(n_obs = n(), .groups = "drop") %>%
    pivot_wider(names_from = phase, values_from = n_obs, values_fill = 0)
  
  # 3️⃣ Keep only IDs with minimum observations in both phases
  valid_ids <- id_counts %>%
    filter(luteal >= min_obs, follicular >= min_obs) %>%
    pull(id)
  
  excluded_ids <- setdiff(id_counts$id, valid_ids)
  
  # 4️⃣ Subset data
  data_subset <- data %>%
    filter(id %in% valid_ids)
  
  # 5️⃣ Report excluded IDs
  if (length(excluded_ids) > 0) {
    message(glue::glue("⚠️ Excluded IDs (insufficient observations in one or both phases): {length(excluded_ids)}"))
  } else {
    message("✅ All IDs meet the minimum criteria!")
  }
  
  # 6️⃣ Return both
  return(list(
    data_subset = data_subset,
    excluded_ids = excluded_ids
  ))
}