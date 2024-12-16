pmdf <- dplyr::bind_rows(pm_qual, pm_rc)
#vis_miss(pmdf) # Merged well, no big gaps
# Add _PM suffix to all variables that start with Crave_ in pm_rc
pmdf <- pmdf %>%
  rename_with(~ paste0(., "_PM"), starts_with("Crave_"))

# Print a description of the script
cat("The 'pmdf' dataset has been created by merging the 'pm_qual' and 'pm_rc' datasets. The 'Crave_' variables in the pmdf dataset have been renamed to include the '_PM' suffix to avoid conflicts with the AM items of the same name", "\n", "\n")

count_rows_ids(pmdf)

print.variable.names(pmdf)