library(dplyr)
library(tidyverse)
library(mgcv)
library(gamm4)
library(ggplot2)
library(zoo)


compare_bic <- function(
    data,
    outcome,
    centering = "menses",
    smm_results,
    k_smooth = 10,
    save_dir = NULL
) {
  library(ggplot2)
  library(glue)
  library(gamm4)
  library(dplyr)
  
  # Choose the time variable based on centering
  time_var <- ifelse(centering == "menses", "scaled_cycleday_impute", "scaled_cycleday_imp_ov")
  
  message(glue(">> Computing baseline (g=1) model for outcome = {outcome}, centering = {centering}"))
  
  ## Fit single-group model
  formula_g1 <- as.formula(glue("{outcome}_log.d ~ s({time_var}, k = {k_smooth})"))
  
  gamm_fit_g1 <- gamm4(
    formula_g1,
    random = as.formula(glue("~ (1 + {time_var} | id)")),
    data = data,
    na.action = na.omit
  )
  
  # Compute log-likelihood, edf, and BIC
  llk_1group <- logLik(gamm_fit_g1$mer)
  df_1group <- sum(summary(gamm_fit_g1$gam)$edf) + 1
  n <- nrow(data)
  bic_1group <- -2 * llk_1group + log(n) * df_1group
  bic_1group_val <- as.numeric(bic_1group)
  message(glue(">> Baseline BIC (g=1): {round(bic_1group_val, 2)}"))
  
  ## Extract BIC values from smm_results
  if (is.null(smm_results$bic_table)) {
    stop("smm_results does not include bic_table. Did you pass the correct object?")
  }
  
  smm_table <- smm_results$bic_table %>% arrange(groups)
  
  ## Combine into one comparison table
  all_bic_table <- data.frame(
    groups = c(1, smm_table$groups),
    BIC = c(bic_1group_val, smm_table$best_BIC)
  )
  
  ## Plot
  bic_plot <- ggplot(all_bic_table, aes(x = groups, y = BIC)) +
    geom_line() + 
    geom_point(size = 2) +
    labs(
      title = glue("{outcome} SMM ({centering}-centered) Comparison by BIC"),
      x = "Number of Groups",
      y = "Minimum BIC"
    ) +
    scale_x_continuous(
      breaks = seq(min(all_bic_table$groups), max(all_bic_table$groups), by = 1)
    ) +
    theme_minimal()
  
  print(bic_plot)
  
  ## Save outputs if directory specified
  if (!is.null(save_dir)) {
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
    }
    
    # Build base filename
    base_name <- glue("{outcome}_{centering}_bic_comparison")
    
    # Save CSV
    csv_path <- file.path(save_dir, paste0(base_name, ".csv"))
    write.csv(all_bic_table, csv_path, row.names = FALSE)
    message(glue("✅ Saved BIC comparison table: {csv_path}"))
    
    # Save plot
    png_path <- file.path(save_dir, paste0(base_name, ".png"))
    ggsave(png_path, plot = bic_plot, width = 7, height = 5)
    message(glue("✅ Saved BIC comparison plot: {png_path}"))
  }
  
  ## Return both
  return(list(
    bic_table = all_bic_table,
    bic_plot = bic_plot
  ))
}
