#Create day of week variables including categorical, sin, and cos variables
df <- df %>%
  mutate(day_of_week = weekdays(as.Date(daterated)),
         day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
         day_of_week_sin = sin(2 * pi * as.numeric(day_of_week) / 7),
         day_of_week_cos = cos(2 * pi * as.numeric(day_of_week) / 7))

plot_day_of_week_effects <- function(df, outcome_vars) {
  # Set output directory path from the environment variable
  output_folder_path <- output_folder
  
  # Ensure day_of_week is an ordered factor
  df <- df %>%
    mutate(day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
  
  # Define sine and cosine positions for each day (without labels)
  label_positions <- data.frame(
    day_of_week_sin = sin(2 * pi * (0:6) / 7),
    day_of_week_cos = cos(2 * pi * (0:6) / 7)
  )
  
  # Create output directory if it doesn’t exist
  if (!dir.exists(output_folder_path)) {
    dir.create(output_folder_path, recursive = TRUE)
  }
  
  # Helper function to plot smooth effect (sin/cos) for a given outcome
  plot_smooth_effect <- function(df, outcome_var, x_var, title_suffix, output_filename) {
    outcome_sym <- sym(outcome_var)
    x_sym <- sym(x_var)
    
    plot <- ggplot(df %>% filter(!is.na(!!x_sym) & !is.na(!!outcome_sym)), 
                   aes(x = !!x_sym, y = !!outcome_sym)) +
      geom_smooth(method = "loess") +
      labs(
        title = paste("Effects of Day of Week on", outcome_var, title_suffix),
        x = paste("Day of Week", title_suffix),
        y = outcome_var
      ) +
      theme_minimal()
    
    # Save the plot
    ggsave(filename = file.path(output_folder_path, output_filename), plot = plot, width = 10, height = 6)
  }
  
  # Loop over each outcome variable to generate the plots
  for (outcome_var in outcome_vars) {
    # Plot for day_of_week_sin
    plot_smooth_effect(df, outcome_var, "day_of_week_sin", "(sin)",
                       paste0("day_of_week_sin_", outcome_var, ".png"))
    
    # Plot for day_of_week_cos
    plot_smooth_effect(df, outcome_var, "day_of_week_cos", "(cos)",
                       paste0("day_of_week_cos_", outcome_var, ".png"))
    
    # Calculate weekly average or proportion for bar plot
    weekly_summary <- df %>%
      group_by(day_of_week) %>%
      summarize(mean_outcome = mean(!!sym(outcome_var), na.rm = TRUE)) %>%
      ungroup()
    
    # Plot the weekly average/proportion by day of the week (bar plot)
    plot_weekly <- ggplot(weekly_summary, aes(x = day_of_week, y = mean_outcome)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(
        title = paste("Weekly Average of", outcome_var, "by Day of the Week"),
        x = "Day of the Week",
        y = paste("Average", outcome_var)
      ) +
      theme_minimal()
    
    # Save the bar plot for weekly summary by day of the week
    ggsave(filename = file.path(output_folder_path, paste0("weekly_summary_", outcome_var, ".png")), 
           plot = plot_weekly, width = 10, height = 6)
  }
  
  return("Plots generated and saved successfully.")
}
