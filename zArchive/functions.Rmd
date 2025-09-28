# -----------------------------------------------------------------------------
# Custom Functions for the ADHDCYCLE Pipeline
# -----------------------------------------------------------------------------

## standardize_index_names(): Robustly standardizes common variable names
# This function is used during data loading to ensure key columns
# have consistent names across all raw files.
standardize_index_names <- function(df) {
  df %>%
    rename_with(~ case_when(
      grepl("\\b([Dd]ate.?rated)\\b", .x) ~ "daterated",
      grepl("^ID$", .x, ignore.case = TRUE) ~ "id",
      grepl("\\b([Ee]strogen|[Ee]stradiol|[Ee]2)\\b", .x) ~ "E2",
      grepl("\\b([Pp]rogesterone|[Pp]4)\\b", .x) ~ "P4",
      TRUE ~ .x
    ))
}