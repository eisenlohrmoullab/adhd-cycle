
library(dplyr)
library(tidyverse)
library(mgcv)
library(gamm4)
library(ggplot2)
library(zoo)


run_smm <- function(
    data,
    outcome,
    g = 2,
    d_inter = 20,
    plot = TRUE,
    seed = 123,
    centering = "menses", 
    save_dir = NULL
) {
  library(dplyr)
  library(ggplot2)
  library(glue)
  library(mgcv)
  library(gamm4)
  
  set.seed(seed)
  
  `%>%` <- dplyr::`%>%`
  
  ## ========= MULTI-g WRAPPER ========= ##
  if (length(g) > 1) {
    message(glue(">> Running multiple group sizes: {paste(g, collapse=', ')}"))
    
    all_results <- list()
    bic_table <- data.frame(groups = integer(), best_BIC = numeric())
    
    for (gval in g) {
      message(glue(">>> Fitting SMM for g = {gval} <<<"))
      res <- run_smm(
        data = data,
        outcome = outcome,
        g = gval,
        d_inter = d_inter,
        plot = plot,
        seed = seed,
        centering = centering,
        save_dir = save_dir
      )
      
      all_results[[as.character(gval)]] <- res
      best_BIC <- min(res$BIC, na.rm = TRUE)
      bic_table <- rbind(bic_table, data.frame(groups = gval, best_BIC = best_BIC))
      
      # ======== SAVE OUTPUTS FOR EACH g ========
      if (!is.null(save_dir)) {
        if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
        
        ## 1️⃣ Save class membership
        write.csv(
          unique(res$class),
          file = file.path(save_dir, glue("{outcome}_g{gval}_class.csv")),
          row.names = FALSE
        )
        
        ## 2️⃣ Save plots
        plots_list <- res$plots
        if (!is.null(plots_list$plot_roll)) {
          ggsave(
            filename = file.path(save_dir, glue("{outcome}_g{gval}_roll.png")),
            plot = plots_list$plot_roll,
            width = 7, height = 5
          )
        }
        if (!is.null(plots_list$plot_centered)) {
          ggsave(
            filename = file.path(save_dir, glue("{outcome}_g{gval}_centered.png")),
            plot = plots_list$plot_centered,
            width = 7, height = 5
          )
        }
        if (!is.null(plots_list$plot_mean)) {
          ggsave(
            filename = file.path(save_dir, glue("{outcome}_g{gval}_mean.png")),
            plot = plots_list$plot_mean,
            width = 7, height = 5
          )
        }
      }
    }
    
    # ======== SAVE BIC TABLE ========
    if (!is.null(save_dir)) {
      write.csv(
        bic_table,
        file = file.path(save_dir, glue("{outcome}_bic_table.csv")),
        row.names = FALSE
      )
    }
    
    return(list(
      all_results = all_results,
      bic_table = bic_table
    ))
  }
  
  ## ========= SINGLE-g CASE ========= ##
  message(glue(">>> Fitting SMM for g = {g} <<<"))
  
  ## Choose centering
  time_var <- if (centering == "ovulation") "scaled_cycleday_imp_ov" else "scaled_cycleday_impute"
  if (!time_var %in% names(data)) {
    stop(glue("Time variable {time_var} not found in data!"))
  }
  
  ## Subset columns
  outcome_cols <- c(
    outcome,
    paste0(outcome, ".d"),
    paste0(outcome, ".d.roll"),
    paste0(outcome, "_log.d")
  )
  keep_cols <- intersect(c("id", time_var, outcome_cols), names(data))
  df <- dplyr::select(data, all_of(keep_cols))
  
  ## K-means initialization
  grid <- setNames(data.frame(seq(-1, 1, length.out = 20)), time_var)
  id_vec <- unique(df$id)
  trajectory_matrix <- matrix(NA, nrow = length(id_vec), ncol = 20)
  
  for (i in seq_along(id_vec)) {
    person_data <- df %>% filter(id == id_vec[i])
    if (nrow(person_data) >= 10) {
      fit <- tryCatch(
        mgcv::gam(
          as.formula(glue("{outcome}_log.d ~ s({time_var})")),
          data = person_data
        ),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        preds <- predict(fit, newdata = grid)
        trajectory_matrix[i, ] <- preds
      }
    }
  }
  
  ## Valid IDs
  valid_rows <- which(rowSums(is.na(trajectory_matrix)) < ncol(trajectory_matrix))
  trajectory_matrix <- trajectory_matrix[valid_rows, ]
  id_vec <- id_vec[valid_rows]
  
  if (length(id_vec) < g) {
    stop(glue("Not enough participants with valid trajectories for g = {g}. Needed {g}, got {length(id_vec)}."))
  }
  
  km_res <- kmeans(scale(trajectory_matrix), centers = g)
  cluster_map <- data.frame(id = id_vec, group = as.character(km_res$cluster))
  df <- left_join(df, cluster_map, by = "id")
  
  ## EM
  LLK <- numeric(d_inter)
  BIC <- numeric(d_inter)
  
  for (i in 1:d_inter) {
    models <- list()
    preds <- matrix(NA, nrow = nrow(df), ncol = g)
    
    for (k in 1:g) {
      dat_k <- df %>% filter(group == k)
      if (n_distinct(dat_k$id) < 2) {
        warning(glue("Group {k} has <2 participants. Stopping."))
        return(NULL)
      }
      models[[k]] <- gamm4::gamm4(
        as.formula(glue("{outcome}_log.d ~ s({time_var})")),
        random = as.formula(glue("~ (1 + {time_var} | id)")),
        data = dat_k,
        na.action = na.omit
      )
      preds[, k] <- predict(models[[k]]$gam, newdata = df)
    }
    
    resids <- (df[[paste0(outcome, "_log.d")]] - preds)^2
    resid_sums <- lapply(1:g, function(k) {
      aggregate(resids[, k], by = list(id = df$id), FUN = sum, na.rm = TRUE)
    })
    resid_df <- Reduce(function(x, y) merge(x, y, by = "id"), resid_sums)
    colnames(resid_df)[-1] <- paste0("RSS_group", 1:g)
    group_map <- data.frame(
      id = resid_df$id,
      group = apply(resid_df[, -1], 1, which.min)
    )
    df <- left_join(df %>% dplyr::select(-group), group_map, by = "id")
    
    logliks <- sapply(models, function(m) logLik(m$mer))
    LLK[i] <- sum(logliks)
    n_params <- sum(sapply(models, function(m) sum(summary(m$gam)$edf) + 1))
    BIC[i] <- -2 * LLK[i] + log(nrow(df)) * n_params
    
    if (any(table(df$group) <= 20)) {
      warning("At least one group has ≤20 observations. Stopping early.")
      break
    }
  }
  
  final_class <- df %>% dplyr::select(id, group)
  
  ## Plotting
  p_roll <- p_centered <- p_mean <- NULL
  if (plot) {
    df$group <- as.factor(df$group)
    df <- df %>%
      mutate(
        cycleday_perc = (.data[[time_var]] + 1) / 2,
        cycleday_5perc = round(cycleday_perc / 0.05) * 0.05
      )
    
    x_breaks <- seq(0, 1, by = 0.05)
    x_labels <- if (centering == "menses") {
      c("0%L", rep("", 4), "50%L", rep("", 4), "Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation")
    } else {
      c("Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation", rep("", 4), "50%L", rep("", 4), "100%L")
    }
    
    summary_roll <- df %>%
      group_by(group, cycleday_5perc) %>%
      summarise(
        mean_roll = mean(.data[[paste0(outcome, ".d.roll")]], na.rm = TRUE),
        se = sd(.data[[paste0(outcome, ".d.roll")]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[paste0(outcome, ".d.roll")]]))),
        .groups = "drop"
      )
    
    summary_centered <- df %>%
      group_by(group, cycleday_5perc) %>%
      summarise(
        mean_centered = mean(.data[[paste0(outcome, ".d")]], na.rm = TRUE),
        se = sd(.data[[paste0(outcome, ".d")]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[paste0(outcome, ".d")]]))),
        .groups = "drop"
      )
    
    summary_mean <- df %>%
      group_by(group, cycleday_5perc) %>%
      summarise(
        mean_outcome = mean(.data[[outcome]], na.rm = TRUE),
        se = sd(.data[[outcome]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[outcome]]))),
        .groups = "drop"
      )
    
    p_roll <- ggplot(summary_roll, aes(x = cycleday_5perc, y = mean_roll, color = group)) +
      geom_line(size = 0.9) +
      geom_ribbon(aes(ymin = mean_roll - se, ymax = mean_roll + se, fill = group), alpha = 0.2, color = NA) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      labs(title = paste(outcome, "Rolling Avg (G =", g, ")"), x = "Percentage of Phase Elapsed", y = "Rolling Mean Centered") +
      theme_minimal()
    
    p_centered <- ggplot(summary_centered, aes(x = cycleday_5perc, y = mean_centered, color = group)) +
      geom_line(size = 0.9) +
      geom_ribbon(aes(ymin = mean_centered - se, ymax = mean_centered + se, fill = group), alpha = 0.2, color = NA) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      labs(title = paste(outcome, ": Person-Centered (G =", g, ")"), x = "Percentage of Phase Elapsed", y = "Mean Centered") +
      theme_minimal()
    
    p_mean <- ggplot(summary_mean, aes(x = cycleday_5perc, y = mean_outcome, color = group)) +
      geom_line(size = 0.9) +
      geom_ribbon(aes(ymin = mean_outcome - se, ymax = mean_outcome + se, fill = group), alpha = 0.2, color = NA) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      labs(title = paste(outcome, ": Mean (G =", g, ")"), x = "Percentage of Phase Elapsed", y = "Mean Outcome") +
      theme_minimal()
    
    print(p_roll)
    print(p_centered)
    print(p_mean)
  }
  
  return(list(
    class = final_class,
    LLK = LLK,
    BIC = BIC,
    summary = df %>%
      group_by(group) %>%
      summarise(
        n = n_distinct(id),
        mean_outcome = mean(.data[[outcome]], na.rm = TRUE),
        sd_outcome = sd(.data[[outcome]], na.rm = TRUE),
        .groups = "drop"
      ),
    plots = list(
      plot_roll = p_roll,
      plot_centered = p_centered,
      plot_mean = p_mean
    )
  ))
}





