library(dplyr)
library(tidyverse)
library(mgcv)
library(gamm4)
library(ggplot2)
library(zoo)



model_plot_pain_gam <- function(
    data,
    class_df,
    symptom,
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
  
    data$id = as.factor(data$id)
    class_df$id = as.factor(class_df$id)
    # 2️⃣ Merge onto main data
    data_with_group <- data %>%
      dplyr::select(id, physicalpain, time_var) %>%
      left_join(class_df, by = "id") %>%
      filter(!is.na(group))
    
    g = max(as.numeric(data_with_group$group), na.rm = T)
    
    # Make sure group is a factor
    data_with_group$group <- as.factor(data_with_group$group)
    
    data_with_group$physicalpain_log = log(data_with_group$physicalpain + 1)
    
    # 3️⃣ Fit GAM
    gam_formula <- as.formula(glue(
      "physicalpain_log ~ s(id, bs = 're') + s({time_var}, id, bs = 're') + group + s({time_var}, by = group)"
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
    saveRDS(gam_fit, file = file.path(save_dir, glue("{symptom}_physpain_g{g}_GAM_model.rds")))
    
    # 6️⃣ Save summary as TXT
    summ_text <- capture.output(summary(gam_fit))
    writeLines(summ_text, con = file.path(save_dir, glue("{symptom}_physpain_g{g}_GAM_summary.txt")))
    
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
        y = glue("Physical Pain (back-transformed)"),
        title = glue("Model-Implied Values for Physical Pain with {symptom} Group Moderator (g={g})")
      ) +
      theme_minimal()
    
    # 10️⃣ Save prediction plot
    ggsave(
      filename = file.path(save_dir, glue("{symptom}_physpain_g{g}_GAM.png")),
      plot = p,
      width = 8,
      height = 6,
      dpi = 300
    )
    
  
  ## 11️⃣ Save GCV results table as CSV
  gcv_csv_path <- file.path(save_dir, glue("{symptom}_physpain_GCV_summary.csv"))
  write.csv(gcv_results, gcv_csv_path, row.names = FALSE)
  message(glue("✅ GCV summary saved as {gcv_csv_path}"))
  
  
  ## Return
  return(list(
    gcv_table = gcv_results,
    gam_plot = p
  ))
}
