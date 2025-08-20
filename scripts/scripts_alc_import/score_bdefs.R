
# Create a mean score for these daily items across each row and make sure to account for missing values ("Diff_Inhib", "Diff_Stop", "Diff_Change", "Immed_Payoff", "Res_Urge", "Diff_LongRew", "Diff_ResRew")

df <- df %>%
  mutate(BDEFS = rowMeans(select(., all_of(bdefs_vars)), na.rm = TRUE))

# Check the distribution of the BDEFS scores

hist(df$BDEFS)

# Save the plot to the output_folder
ggsave(filename = paste0(output_folder, "/BDEFS_histogram.png"), width = 10, height = 6)