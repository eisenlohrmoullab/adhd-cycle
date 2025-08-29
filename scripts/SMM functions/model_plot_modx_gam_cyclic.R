library(dplyr)
library(tidyverse)
library(mgcv)
library(gamm4)
library(ggplot2)
library(zoo)
library(marginaleffects)


model_plot_modx_gam_cyclic <- function(
    data,
    outcome,
    time_var, # New argument
    smm_result,
    centering = "menses",
    save_dir,
    k_smooth = 10, 
    log_var = TRUE,
    show_CI = TRUE
) {
  library(glue)
  library(dplyr)
  library(ggplot2)
  library(mgcv)
  
  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  
  ## --------- choose subfolder by centering for all saves ----------
  if (!is.null(save_dir)) {
    date_folder <- format(Sys.Date(), "%Y%m%d")
    centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
    
    # New path: save_dir/YYYYMMDD/centering_folder/outcome
    sub_dir <- file.path(save_dir, date_folder, centering_folder, outcome)
    
    dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  } else {
    sub_dir <- NULL
  }
  
  
  ## Determine variable suffix
  var_suffix <- if (log_var) "_log" else ""
  full_outcome <- paste0(outcome, var_suffix)
  
  # Track GCV values
  gcv_results <- data.frame(
    group_size = character(),
    GCV = numeric(),
    stringsAsFactors = FALSE
  )
  
  ## 🔹 Baseline model without group
  base_formula <- as.formula(glue(
    "{full_outcome} ~ s(id, bs = 're') + s({time_var}, id, bs = c('re', 'cc')) + s({time_var}, bs = 'cc')"
  ))
  
  base_fit <- mgcv::gam(
    formula = base_formula,
    data = data,
    method = "REML"
  )
  
  base_gcv <- base_fit$gcv.ubre
  gcv_results <- rbind(
    gcv_results,
    data.frame(group_size = "none", GCV = base_gcv)
  )
  
  # Save baseline model and summary
  if (!is.null(sub_dir)) {
    saveRDS(base_fit, file = file.path(sub_dir, glue("{full_outcome}_noGroup_GAM_model.rds")))
    writeLines(capture.output(summary(base_fit)), file.path(sub_dir, glue("{full_outcome}_noGroup_GAM_summary.txt")))
  }
  
  # Iterate over all group sizes in smm result
  all_g <- names(smm_result$all_results)
  
  for (g in all_g) {
    message(glue(">> Processing grouping for g = {g}"))
    
    # 1️⃣ Extract class assignments
    class_df <- smm_result$all_results[[g]]$class
    class_df <- unique(class_df)
    
    # 2️⃣ Merge onto main data
    data_with_group <- data %>%
      dplyr::select(id, all_of(c(outcome, paste0(outcome, "_log"), full_outcome, time_var))) %>%
      left_join(class_df, by = "id") %>%
      filter(!is.na(group))
    
    # Make sure group is a factor
    data_with_group$group <- as.factor(data_with_group$group)
    
    # 3️⃣ Fit GAM
    gam_formula <- as.formula(glue(
      "{full_outcome} ~ s(id, bs = 're') + s({time_var}, id, bs = c('re', 'cc')) + group + s({time_var}, by = group, bs = c('cc'))"
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
      data.frame(group_size = as.character(g), GCV = gcv_val)
    )
    
    # 5️⃣ Save model as RDS
    if (!is.null(sub_dir)){
      saveRDS(gam_fit, file = file.path(sub_dir, glue("{full_outcome}_g{g}_GAM_model.rds")))
    }
    
    # 6️⃣ Save summary as TXT
    summ_text <- capture.output(summary(gam_fit))
    if (!is.null(sub_dir)){
      writeLines(summ_text, con = file.path(sub_dir, glue("{full_outcome}_g{g}_GAM_summary.txt")))
    }
    
    # 7️⃣ Build prediction grid
    group_levels = sort(unique(data_with_group$group))
    pred_grid <- expand.grid(
      temp_time = seq(-1, 1, length.out = 100),
      group = factor(group_levels, levels = group_levels),
      id = "new" # Use a placeholder for population-level predictions
    )
    names(pred_grid)[names(pred_grid) == "temp_time"] <- time_var
    
    # 8️⃣ Predictions
    pred <- marginaleffects::predictions(
      gam_fit,
      newdata = pred_grid,
      type = "response",
      exclude = "s(id)", # Exclude random effect for id
      transform_post = if (log_var) exp else NULL
    )
    
    pred_grid$estimate <- pred$estimate
    pred_grid$conf.low <- pred$conf.low
    pred_grid$conf.high <- pred$conf.high
    
    
    # 9️⃣ Build prediction plot
    x_breaks <- seq(-1, 1, by = 0.5)
    x_labels <- if (centering == "menses") {
      c("Ovulation", "50%L", "Menses Onset", "50%F", "Ovulation")
    } else {
      c("Menses Onset", "50%F", "Ovulation", "50%L", "Menses Onset")
    }
    
    rect_data <- if (centering == "menses") {
      data.frame(
        xmin = c(-0.04, 0.92, -1),
        xmax = c(0.04, 1, -0.92),
        fill = c("grey70", "grey87", "grey87")
      )
    } else {
      data.frame(
        xmin = c(-0.04, -1, 0.92),
        xmax = c(0.04, -0.92, 1),
        fill = c("grey87", "grey70", "grey70")
      )
    }
    
    group_n <- data_with_group %>%
      group_by(group) %>%
      summarise(n_id = n_distinct(id), .groups = "drop") %>%
      mutate(
        group_label = paste0("Group ", group, " (N=", n_id, ")"),
        group = as.factor(group)
      )
    
    # Merge into prediction grid
    pred_grid <- pred_grid %>%
      left_join(group_n, by = "group")
    
    p <- ggplot(pred_grid, aes_string(x = time_var, y = "estimate", color = "group_label")) +
      geom_rect(
        data = rect_data,
        inherit.aes = FALSE,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        fill = rect_data$fill,
        alpha = 0.2,
        color = "white",
        show.legend = FALSE
      ) +
      geom_ribbon(
        aes(ymin = conf.low, ymax = conf.high, fill = group_label),
        alpha = if (show_CI) 0.2 else 0,
        color = NA,
        show.legend = FALSE
      ) +
      geom_line(size = 0.9) +
      scale_x_continuous(
        limits = c(-1, 1),
        breaks = x_breaks,
        labels = x_labels
      ) +
      labs(
        x = "",
        y = glue("{outcome}"),
        title = glue("Model-Implied Curves for {outcome} with Group Moderator (g={g})"),
        color = "Group",
        fill = "Group"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    
    # 10️⃣ Save prediction plot
    if (!is.null(sub_dir)){
      ggsave(
        filename = file.path(sub_dir, glue("{full_outcome}_g{g}_GAM_plot.png")),
        plot = p,
        width = 8,
        height = 6,
        dpi = 300
      )
    }
    
    message(glue("✅ Done with g = {g}. Files saved in {sub_dir}"))
  }
  
  ## 11️⃣ Save GCV results table as CSV
  if (!is.null(sub_dir)){
    gcv_csv_path <- file.path(sub_dir, glue("{outcome}_GCV_summary.csv"))
    write.csv(gcv_results, gcv_csv_path, row.names = FALSE)
    message(glue("✅ GCV summary saved as {gcv_csv_path}"))
  }
  
  ## 12️⃣ Plot GCV vs Groups
  gcv_results$group_size <- factor(gcv_results$group_size, levels = c("none", sort(as.numeric(setdiff(gcv_results$group_size, "none")))))
  gcv_plot <- ggplot(gcv_results, aes(x = group_size, y = GCV, group = 1)) +
    geom_line() +
    geom_point() +
    labs(
      title = glue("GCV by Number of Groups for {outcome}"),
      x = "Number of Groups",
      y = "GCV (gam)"
    ) +
    theme_minimal()
  
  if (!is.null(sub_dir)){
    gcv_plot_path <- file.path(sub_dir, glue("{outcome}_GCV_plot.png"))
    ggsave(
      filename = gcv_plot_path,
      plot = gcv_plot,
      width = 8,
      height = 6,
      dpi = 300
    )
    message(glue("✅ GCV plot saved as {gcv_plot_path}"))
  }
  message("✅✅ All group GAMs processed and saved!")
  
  ## Return
  return(list(
    gcv_table = gcv_results,
    gcv_plot = gcv_plot
  ))
}