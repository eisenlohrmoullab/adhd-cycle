# Create a list to store plots
plots <- list()

# Get unique ids
ids <- unique(df$id)

# Loop through each id and create a plot
for (i in ids) {
  
  # Subset data for the specific id
  df_subset <- df %>% filter(id == i)
  
  # Arrange by daterated
  df_subset <- df_subset %>% arrange(daterated)
  
  # Standardize LH, E2, and P4 for this id and apply a 5-day rolling mean
  df_subset <- df_subset %>% 
    mutate(
      LH_std = (LH - mean(LH, na.rm = TRUE)) / sd(LH, na.rm = TRUE),
      E2_std = (E2 - mean(E2, na.rm = TRUE)) / sd(E2, na.rm = TRUE),
      P4_std = (P4 - mean(P4, na.rm = TRUE)) / sd(P4, na.rm = TRUE)
    ) %>%
    mutate(
      LH_smooth = zoo::rollapply(LH_std, width = 6, FUN = mean, fill = NA, align = "center", partial = TRUE),
      E2_smooth = zoo::rollapply(E2_std, width = 6, FUN = mean, fill = NA, align = "center", partial = TRUE),
      P4_smooth = zoo::rollapply(P4_std, width = 6, FUN = mean, fill = NA, align = "center", partial = TRUE)
    )
  
  # Create the plot with smoothed standardized LH, E2, and P4
  p <- ggplot(df_subset, aes(x = daterated)) +
    geom_line(aes(y = LH_smooth, color = "LH"), size = 1) +
    geom_line(aes(y = E2_smooth, color = "E2"), size = 1) +
    geom_line(aes(y = P4_smooth, color = "P4"), size = 1) +
    geom_vline(data = df_subset %>% filter(TAEMPeriodStart == 1), aes(xintercept = as.numeric(daterated)), color = "red", linewidth = 1, linetype="dashed") +
    geom_vline(data = df_subset %>% filter(visual_LH_peak == 1), aes(xintercept = as.numeric(daterated)), color = "blue", linewidth = 1, linetype="dashed") +
    geom_vline(data = df_subset %>% filter(TAEMOvPos == 1), aes(xintercept = as.numeric(daterated)), color = "green", linewidth = 1, linetype="dashed") +
    geom_point(data = df_subset %>% filter(!is.na(drink_today)), aes(y = 0), color = "black", size = 2) + # Black dots at y = 0
    labs(title = paste("ID:", i), x = "Date Rated", y = "Standardized Hormone Levels") +
    scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d") + # Show every date on x-axis
    theme_classic(base_size = 24) +
    theme(
      plot.title = element_text(size = 28, face = "bold"),
      axis.title.x = element_text(size = 30),
      axis.title.y = element_text(size = 30),
      axis.text = element_text(size = 20),
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
      legend.title = element_text(size = 30),
      legend.text = element_text(size = 30)
    ) +
    scale_color_manual(values = c("LH" = "forestgreen", "E2" = "gold", "P4" = "salmon"), name = "Hormone")
  
  # Save each plot to the list
  plots[[as.character(i)]] <- p
}

# Save all plots to output_folder
for (i in names(plots)) {
  ggsave(filename = paste0(output_folder, "/plot_id4_", i, ".png"), plot = plots[[i]], bg = "white")
}