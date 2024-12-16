
# Load necessary libraries
library(dplyr)

# Create a summary table for first period, ovulation, and second period
summary_table <- cycledf %>%
  group_by(id) %>%
  summarize(
    firstperiod = min(daterated[StartPeriod == 1], na.rm = TRUE),
    ovulation = min(daterated[ovpos == 1], na.rm = TRUE),
    secondperiod = min(daterated[StartPeriod == 1 & daterated > firstperiod], na.rm = TRUE)
  ) %>%
  ungroup()

# Display the table
summary_table

# Output this as a csv to the location "output_folder" already specified
write.csv(summary_table, file = paste0(output_folder, "/cycle_summary_table.csv"), row.names = FALSE)