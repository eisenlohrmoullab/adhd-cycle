library(tidyverse)
library(dplyr)
library(glue)

subset_by_phase_cyclic <- function(data, outcome, time_var = "cyclic_time_impute", min_obs = 5) {
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
    pivot_wider(names_from = phase, values_from = n_obs, values_fill = list(n_obs = 0))
  
  # Ensure both phase columns exist
  if (!"luteal" %in% names(id_counts)) {
    id_counts$luteal <- 0
  }
  if (!"follicular" %in% names(id_counts)) {
    id_counts$follicular <- 0
  }
  
  # 3️⃣ Keep only IDs with minimum observations in both phases
  valid_ids <- id_counts %>%
    filter(luteal >= min_obs, follicular >= min_obs) %>%
    pull(id)
  
  all_ids <- unique(data$id)
  excluded_ids <- setdiff(all_ids, valid_ids)
  
  # 4️⃣ Subset data
  data_subset <- data %>%
    filter(id %in% valid_ids)
  
  # 5️⃣ Report excluded IDs
  if (length(excluded_ids) > 0) {
    message(glue("⚠️ Excluded {length(excluded_ids)} IDs (insufficient observations in one or both phases)."))
  } else {
    message("✅ All IDs meet the minimum criteria!")
  }
  
  # 6️⃣ Return both
  return(list(
    data_subset = data_subset,
    excluded_ids = excluded_ids
  ))
}