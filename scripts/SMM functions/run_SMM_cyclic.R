library(dplyr)
library(tidyverse)
library(mgcv)
library(gamm4)
library(ggplot2)
library(zoo)
library(glue)

run_smm_cyclic <- function(
    data,
    outcome,
    time_var,
    g = 2,
    d_inter = 20,
    plot = TRUE,
    seed = 123,
    centering = "menses", 
    save_dir = NULL,
    k_smooth = 10, 
    log_var = TRUE
) {
  
  set.seed(seed)
  `%>%` <- dplyr::`%>%`
  
  ## --------- create nested folder structure: date / centering / outcome ----------
  if (!is.null(save_dir)) {
    date_folder <- format(Sys.Date(), "%Y%m%d")
    centering_folder <- if (centering == "ovulation") "ovulation_centered" else "menses_centered"
    
    # Final path: save_dir/YYYYMMDD/centering_folder/outcome
    sub_dir <- file.path(save_dir, date_folder, centering_folder, outcome)
    
    dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  } else {
    sub_dir <- NULL
  }
  
  
  ## Determine variable suffix for modeling
  var_suffix <- if (log_var) "_log.d" else ".d"
  full_outcome <- paste0(outcome, var_suffix)
  
  ## ========= MULTI-g WRAPPER ========= ##
  if (length(g) > 1) {
    message(glue(">> Running multiple group sizes: {paste(g, collapse=', ')}"))
    all_results <- list()
    bic_table <- data.frame(groups = integer(), best_BIC = numeric())
    for (gval in g) {
      message(glue(">>> Fitting SMM for g = {gval} <<<"))
      res <- run_smm_cyclic(
        data = data, outcome = outcome, time_var = time_var, g = gval, 
        d_inter = d_inter, plot = plot, seed = seed, centering = centering, 
        save_dir = save_dir, k_smooth = k_smooth, log_var = log_var
      )
      all_results[[as.character(gval)]] <- res
      best_BIC <- min(res$BIC, na.rm = TRUE)
      bic_table <- rbind(bic_table, data.frame(groups = gval, best_BIC = best_BIC))
      
      ## save outputs for this g
      if (!is.null(sub_dir) && !is.null(res)) {
        write.csv(unique(res$class),
                  file = file.path(sub_dir, glue("{outcome}_g{gval}_class.csv")),
                  row.names = FALSE)
        plots_list <- res$plots
        if (!is.null(plots_list$plot_roll)) {
          ggsave(file.path(sub_dir, glue("{outcome}_g{gval}_roll.png")),
                 plots_list$plot_roll, width = 7, height = 5)
        }
        if (!is.null(plots_list$plot_centered)) {
          ggsave(file.path(sub_dir, glue("{outcome}_g{gval}_centered.png")),
                 plots_list$plot_centered, width = 7, height = 5)
        }
        if (!is.null(plots_list$plot_mean)) {
          ggsave(file.path(sub_dir, glue("{outcome}_g{gval}_mean.png")),
                 plots_list$plot_mean, width = 7, height = 5)
        }
      }
    }
    if (!is.null(sub_dir)) {
      write.csv(bic_table,
                file = file.path(sub_dir, glue("{outcome}_bic_table.csv")),
                row.names = FALSE)
    }
    return(list(all_results = all_results, bic_table = bic_table))
  }
  
  ## ========= SINGLE-g CASE ========= ##
  message(glue(">>> Fitting SMM for g = {g} <<<"))
  
  if (!time_var %in% names(data)) {
    stop(glue("Time variable '{time_var}' not found in data!"))
  }
  
  ## Define expected column names based on log_var
  centered_col <- paste0(outcome, if(log_var) "_log.d" else ".d")
  rolling_col <- paste0(outcome, if(log_var) "_log.d.roll" else ".d.roll")
  
  outcome_cols <- c(outcome, centered_col, rolling_col)
  keep_cols <- intersect(c("id", time_var, outcome_cols), names(data))
  df <- dplyr::select(data, dplyr::all_of(keep_cols))
  
  knots_list <- setNames(list(c(-1, 1)), time_var)
  
  grid <- setNames(data.frame(seq(-1, 1, length.out = 20)), time_var)
  id_vec <- unique(df$id)
  trajectory_matrix <- matrix(NA, nrow = length(id_vec), ncol = 20)
  
  for (i in seq_along(id_vec)) {
    person_data <- df %>% dplyr::filter(id == id_vec[i])
    if (nrow(person_data) >= 10) {
      fit <- tryCatch(
        mgcv::gam(
          as.formula(glue("{full_outcome} ~ s({time_var}, k = {k_smooth})")),
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
  
  valid_rows <- which(rowSums(is.na(trajectory_matrix)) < ncol(trajectory_matrix))
  trajectory_matrix <- trajectory_matrix[valid_rows, ]
  id_vec <- id_vec[valid_rows]
  
  if (length(id_vec) < g) {
    stop(glue("Not enough participants with valid trajectories for g = {g}. Needed {g}, got {length(id_vec)}."))
  }
  
  km_res <- kmeans(scale(trajectory_matrix), centers = g)
  cluster_map <- data.frame(id = id_vec, group = as.character(km_res$cluster))
  df <- dplyr::left_join(df, cluster_map, by = "id")
  
  LLK <- numeric(d_inter)
  BIC <- numeric(d_inter)
  
  for (i in 1:d_inter) {
    models <- list()
    preds <- matrix(NA, nrow = nrow(df), ncol = g)
    
    for (k in 1:g) {
      dat_k <- df %>% dplyr::filter(group == k)
      if (dplyr::n_distinct(dat_k$id) < 2) {
        warning(glue("Group {k} has <2 participants. Stopping."))
        return(NULL)
      }
      models[[k]] <- gamm4::gamm4(
        as.formula(glue("{full_outcome} ~ s({time_var}, bs = 'cc', k = {k_smooth})")),
        random = as.formula(glue("~ (1 + {time_var} | id)")),
        data = dat_k,
        na.action = na.omit,
        knots = knots_list
      )
      preds[, k] <- predict(models[[k]]$gam, newdata = df)
    }
    
    resids <- (df[[full_outcome]] - preds)^2
    resid_sums <- lapply(1:g, function(k) {
      stats::aggregate(resids[, k], by = list(id = df$id), FUN = sum, na.rm = TRUE)
    })
    resid_df <- Reduce(function(x, y) merge(x, y, by = "id"), resid_sums)
    colnames(resid_df)[-1] <- paste0("RSS_group", 1:g)
    
    group_map <- data.frame(
      id = resid_df$id,
      group = as.character(apply(resid_df[, -1], 1, which.min))
    )
    df <- df %>% dplyr::select(-any_of("group")) %>%
      dplyr::left_join(group_map, by = "id")
    
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
  
  grp_counts <- df %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      n = dplyr::n(),
      N = dplyr::n_distinct(id),
      .groups = "drop"
    )
  df <- df %>%
    dplyr::left_join(grp_counts, by = "group") %>%
    dplyr::mutate(
      group_lab = factor(
        paste0(group, " (N=", N, ", n=", n, ")"),
        levels = paste0(
          sort(unique(group)),
          " (N=", grp_counts$N[match(sort(unique(group)), grp_counts$group)],
          ", n=", grp_counts$n[match(sort(unique(group)), grp_counts$group)],
          ")"
        )
      )
    )
  
  p_roll <- p_centered <- p_mean <- NULL
  if (plot) {
    df <- df %>%
      dplyr::mutate(
        cycleday_perc = (.data[[time_var]] + 1) / 2,
        cycleday_5perc = round(cycleday_perc / 0.05) * 0.05
      )
    
    x_breaks <- seq(0, 1, by = 0.05)
    x_labels <- if (centering == "menses") {
      c("Ovulation", rep("", 4), "50%L", rep("", 4), "Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation")
    } else {
      c("Menses Onset", rep("", 4), "50%F", rep("", 4), "Ovulation", rep("", 4), "50%L", rep("", 4), "Menses Onset")
    }
    
    # --- ‼️ CORRECTED CODE IS HERE ‼️ ---
    # Use the correct column names defined earlier for summarization
    summary_roll <- df %>%
      dplyr::group_by(group_lab, cycleday_5perc) %>%
      dplyr::summarise(
        mean_roll = mean(.data[[rolling_col]], na.rm = TRUE),
        se = sd(.data[[rolling_col]], na.rm = TRUE) /
          sqrt(sum(!is.na(.data[[rolling_col]]))),
        .groups = "drop"
      ) 
    
    summary_roll <- dplyr::bind_rows(
      summary_roll,
      summary_roll %>%
        dplyr::filter(cycleday_5perc == 1) %>%
        dplyr::mutate(cycleday_5perc = 0)
    )
    
    summary_centered <- df %>%
      dplyr::group_by(group_lab, cycleday_5perc) %>%
      dplyr::summarise(
        mean_centered = mean(.data[[centered_col]], na.rm = TRUE),
        se = sd(.data[[centered_col]], na.rm = TRUE) /
          sqrt(sum(!is.na(.data[[centered_col]]))),
        .groups = "drop"
      ) 
    
    summary_centered <- dplyr::bind_rows(
      summary_centered,
      summary_centered %>%
        dplyr::filter(cycleday_5perc == 1) %>%
        dplyr::mutate(cycleday_5perc = 0)
    )
    # --- END CORRECTION ---
    
    summary_mean <- df %>%
      dplyr::group_by(group_lab, cycleday_5perc) %>%
      dplyr::summarise(
        mean_outcome = mean(.data[[outcome]], na.rm = TRUE),
        se = sd(.data[[outcome]], na.rm = TRUE) /
          sqrt(sum(!is.na(.data[[outcome]]))),
        .groups = "drop"
      ) 
    
    summary_mean <- dplyr::bind_rows(
      summary_mean,
      summary_mean %>%
        dplyr::filter(cycleday_5perc == 1) %>%
        dplyr::mutate(cycleday_5perc = 0)
    )
    
    p_roll <- ggplot(summary_roll, aes(x = cycleday_5perc, y = mean_roll, color = group_lab, fill = group_lab)) +
      geom_line(size = 0.9) +
      geom_ribbon(aes(ymin = mean_roll - se, ymax = mean_roll + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      labs(title = paste(outcome, "Rolling Avg (G =", g, ")"), x = "Percentage of Phase Elapsed", y = "Rolling Mean Centered", color = "Group") +
      theme_minimal()
    
    p_centered <- ggplot(summary_centered, aes(x = cycleday_5perc, y = mean_centered, color = group_lab, fill = group_lab)) +
      geom_line(size = 0.9) +
      geom_ribbon(aes(ymin = mean_centered - se, ymax = mean_centered + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      labs(title = paste(outcome, ": Person-Centered (G =", g, ")"), x = "Percentage of Phase Elapsed", y = "Mean Centered", color = "Group") +
      theme_minimal()
    
    p_mean <- ggplot(summary_mean, aes(x = cycleday_5perc, y = mean_outcome, color = group_lab, fill = group_lab)) +
      geom_line(size = 0.9) +
      geom_ribbon(aes(ymin = mean_outcome - se, ymax = mean_outcome + se), alpha = 0.2, color = NA, show.legend = FALSE) +
      scale_x_continuous(breaks = x_breaks, labels = x_labels, limits = c(0, 1)) +
      labs(title = paste(outcome, ": Mean (G =", g, ")"), x = "Percentage of Phase Elapsed", y = "Mean Outcome", color = "Group") +
      theme_minimal()
    
    if (!is.null(sub_dir)) {
      ggsave(file.path(sub_dir, glue("{outcome}_g{g}_roll.png")),     p_roll,     width = 7, height = 5)
      ggsave(file.path(sub_dir, glue("{outcome}_g{g}_centered.png")), p_centered, width = 7, height = 5)
      ggsave(file.path(sub_dir, glue("{outcome}_g{g}_mean.png")),     p_mean,     width = 7, height = 5)
      write.csv(unique(final_class), file = file.path(sub_dir, glue("{outcome}_g{g}_class.csv")), row.names = FALSE)
      write.csv(data.frame(iter = 1:length(BIC[BIC != 0]), BIC = BIC[BIC != 0]),
                file = file.path(sub_dir, glue("{outcome}_g{g}_BIC_trace.csv")),
                row.names = FALSE)
    }
  }
  
  return(list(
    class = final_class,
    LLK = LLK,
    BIC = BIC,
    summary = df %>%
      dplyr::distinct(id, .keep_all = TRUE) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(
        n = dplyr::n(),
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