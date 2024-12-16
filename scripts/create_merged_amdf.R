# Combine am_qual and am_rc by adding observations (rows) rather than merging columns
amdf <- dplyr::bind_rows(am_qual, am_rc)

#vis_miss(amdf) # Merged well, no gaps

# Add placeholder rows for missing combinations of id and daterated
amdf <- amdf %>%
  group_by(id) %>%
  complete(daterated = seq.Date(min(daterated), max(daterated), by = "day")) %>%
  ungroup()

nrow(amdf) #2233 (2565 with placeholders)

#count_rows_ids(amdf)

#Set the Crave_ variables as numeric
amdf <- amdf %>%
  mutate(across(starts_with("Crave_"), as.numeric))


# Add _PM suffix to all variables that start with Crave_ in amdf
amdf <- amdf %>%
  rename_with(~ paste0(., "_AM"), starts_with("Crave_"))

print.variable.names(amdf)

# Check for duplicates and remove
amdf <- amdf %>%
  distinct()

count_rows_ids(amdf)

# Print description of what happened in this script
cat("The amdf dataset was created by combining the am_qual and am_rc datasets. The Crave_ variables were set as numeric and _AM was added as a suffix to their variable names. Duplicates were removed from the amdf dataset.")
