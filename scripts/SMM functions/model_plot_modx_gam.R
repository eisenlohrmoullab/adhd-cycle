library(dplyr)
library(tidyverse)
library(mgcv)
library(gamm4)
library(ggplot2)
library(zoo)



model_plot_modx_gam <- function(
    data,
    outcome,
    smm_result,
    centering = "menses",
    save_dir,
    k_smooth = 10
) {
  library(glue)
  library(dplyr)
  library(ggplot2)
  library(mgcv)
  
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  
  # Determine time variable
  time_var <- if (centering == "ovulation") "scaled_cycleday_imp_ov" else "scaled_cycleday_impute"
  
  # Track GCV values
  gcv_results <- data.frame(
    group_size = integer(),
    GCV = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Iterate over all group sizes in smm result
  all_g <- names(smm_result$all_results)
  
  for (g in all_g) {
    message(glue(">> Processing grouping for g = {g}"))
    
    # 1️⃣ Extract class assignments
    class_df <- smm_result$all_results[[g]]$class
    class_df <- unique(class_df)
    
    # 2️⃣ Merge onto main data
    data_with_group <- data %>%
      dplyr::select(id, all_of(c(outcome, paste0(outcome, "_log"), time_var))) %>%
      left_join(class_df, by = "id") %>%
      filter(!is.na(group))
    
    # Make sure group is a factor
    data_with_group$group <- as.factor(data_with_group$group)
    
    # 3️⃣ Fit GAM
    gam_formula <- as.formula(glue(
      "{outcome}_log ~ s(id, bs = 're') + s({time_var}, id, bs = 're') + group + s({time_var}, by = group)"
    ))
    
    gam_fit <- mgcv::gam(
      formula = gam_formula,
      data = data_with_group,
      method = "REML"
    )
    
    # 4️⃣ Compute GCV
    gcv_val <- gam_fit$gcv.ubre
    message(glue("  GAM GCV (g={g}): {round(gcv_val, 3)}"))
    
    # Store in results dataframe
    gcv_results <- rbind(
      gcv_results,
      data.frame(group_size = as.integer(g), GCV = as.numeric(gcv_val))
    )
    
    # 5️⃣ Save model as RDS
    saveRDS(gam_fit, file = file.path(save_dir, glue("{outcome}_g{g}_GAM_model.rds")))
    
    # 6️⃣ Save summary as TXT
    summ_text <- capture.output(summary(gam_fit))
    writeLines(summ_text, con = file.path(save_dir, glue("{outcome}_g{g}_GAM_summary.txt")))
    
    # 7️⃣ Build prediction grid
    group_levels <- sort(unique(data_with_group$group))
    pred_grid <- expand.grid(
      temp_time = seq(-1, 1, length.out = 100),
      group = factor(group_levels, levels = group_levels),
      id = 0
    )
    names(pred_grid)[names(pred_grid) == "temp_time"] <- time_var
    
    # 8️⃣ Predictions
    pred <- marginaleffects::predictions(
      gam_fit,
      newdata = pred_grid,
      type = "response",
      transform = function(x) exp(x) - 1
    )
    
    pred_grid$estimate <- pred$estimate
    pred_grid$conf.low <- pred$conf.low
    pred_grid$conf.high <- pred$conf.high
    
    # 9️⃣ Build prediction plot
    x_breaks <- seq(-1, 1, by = 0.5)
    x_labels <- if (centering == "menses") {
      c("0%L", "50%L", "Menses Onset", "50%F", "Ovulation")
    } else {
      c("Menses Onset", "50%F", "Ovulation", "50%L", "100%L")
    }
    
    rects <- data.frame(
      xmin = c(-0.04, 0.92),
      xmax = c(0.04, 1)
    )
    
    p <- ggplot(pred_grid, aes_string(x = time_var, y = "estimate", color = "group")) +
      geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                inherit.aes = FALSE, fill = c("grey70", "grey87"), alpha = 0.2, color = NA, show.legend = FALSE) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
                  alpha = 0.2, color = NA, show.legend = FALSE) +
      geom_line(size = 0.9) +
      scale_x_continuous(
        limits = c(-1, 1),
        breaks = x_breaks,
        labels = x_labels
      ) +
      labs(
        x = "",
        y = glue("{outcome} (back-transformed)"),
        title = glue("Model-Implied Curves for {outcome} with Group Moderator (g={g})")
      ) +
      theme_minimal()
    
    # 10️⃣ Save prediction plot
    ggsave(
      filename = file.path(save_dir, glue("{outcome}_g{g}_GAM_plot.png")),
      plot = p,
      width = 8,
      height = 6,
      dpi = 300
    )
    
    message(glue("✅ Done with g = {g}. Files saved in {save_dir}"))
  }
  
  ## 11️⃣ Save GCV results table as CSV
  gcv_csv_path <- file.path(save_dir, glue("{outcome}_GCV_summary.csv"))
  write.csv(gcv_results, gcv_csv_path, row.names = FALSE)
  message(glue("✅ GCV summary saved as {gcv_csv_path}"))
  
  ## 12️⃣ Plot GCV vs Groups
  gcv_plot <- ggplot(gcv_results, aes(x = group_size, y = GCV)) +
    geom_line() +
    geom_point() +
    labs(
      title = glue("GCV by Number of Groups for {outcome}"),
      x = "Number of Groups",
      y = "GCV (gam)"
    ) +
    scale_x_continuous(
      breaks = seq(min(gcv_results$group_size), max(gcv_results$group_size), by = 1)) +
    theme_minimal()
  
  gcv_plot_path <- file.path(save_dir, glue("{outcome}_GCV_plot.png"))
  ggsave(
    filename = gcv_plot_path,
    plot = gcv_plot,
    width = 8,
    height = 6,
    dpi = 300
  )
  message(glue("✅ GCV plot saved as {gcv_plot_path}"))
  
  message("✅✅ All group GAMs processed and saved!")
  
  ## Return
  return(list(
    gcv_table = gcv_results,
    gcv_plot = gcv_plot
  ))
}
