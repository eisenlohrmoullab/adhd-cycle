# ===================================================================
# 1. Load Required Libraries
# ===================================================================
library(tidyverse)
library(rmcorr)
library(performance)
library(lme4)
library(ggplot2)

# ===================================================================
# 2. Define the Heatmap Generation Function (with improvement)
# ===================================================================
generate_correlation_heatmap <- function(
    data, outcome_vars, id_var,
    sig.level = 0.05,
    highlight_diag = TRUE,
    show_legend = TRUE,
    label_size = 4.6,
    diag_label_size = 6,
    subtitle_text = "Upper: Between-person (Spearman); Lower: Within-person (rmcorr); Diagonal: ICC"
) {
  # This line MUST be first to define the number of variables.
  n_vars <- length(outcome_vars)

  results_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars,
                           dimnames = list(outcome_vars, outcome_vars))
  pval_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars,
                        dimnames = list(outcome_vars, outcome_vars))

  # --- Between-person (upper triangle) ---
  person_means <- data %>%
    group_by(!!sym(id_var)) %>%
    summarise(across(all_of(outcome_vars), ~ mean(., na.rm = TRUE)), .groups = "drop")

  between_corr <- suppressWarnings(
    cor(person_means[, outcome_vars], method = "spearman", use = "pairwise.complete.obs")
  )

  for (i in 1:(n_vars - 1)) for (j in (i + 1):n_vars) {
    x <- person_means[[outcome_vars[i]]]
    y <- person_means[[outcome_vars[j]]]
    if (sum(!is.na(x) & !is.na(y)) > 2) {
      test <- suppressWarnings(cor.test(x, y, method = "spearman"))
      results_matrix[i, j] <- between_corr[i, j]
      pval_matrix[i, j] <- test$p.value
    }
  }

  # --- ICC (diagonal) ---
  for (i in 1:n_vars) {
    var <- outcome_vars[i]
    # Backticks handle variable names with spaces
    fml <- as.formula(paste0("`", var, "` ~ 1 + (1|`", id_var, "`)"))
    if (sum(!is.na(data[[var]])) > 1) {
      model_fit <- try(lmer(fml, data = data), silent = TRUE)
      if (!inherits(model_fit, "try-error")) {
        icc_val <- performance::icc(model_fit)$ICC_adjusted
        results_matrix[i, i] <- icc_val
        pval_matrix[i, i] <- 0
      }
    }
  }

  # --- Within-person (lower triangle) ---
  for (i in 2:n_vars) for (j in 1:(i - 1)) {
    var1 <- outcome_vars[i]; var2 <- outcome_vars[j]
    rmcorr_data <- data %>% select(all_of(c(id_var, var1, var2))) %>% drop_na()
    if (nrow(rmcorr_data) > 2) {
      rmcorr_result <- rmcorr::rmcorr(participant = id_var, measure1 = var1, measure2 = var2, dataset = rmcorr_data)
      results_matrix[i, j] <- rmcorr_result$r
      pval_matrix[i, j] <- rmcorr_result$p
    }
  }

  # --- Reorder by clustering ---
  within_corr_matrix <- results_matrix
  within_corr_matrix[upper.tri(within_corr_matrix, diag = TRUE)] <- 0
  dist_matrix <- as.dist(1 - abs(within_corr_matrix))
  hclust_obj <- hclust(dist_matrix, method = "ward.D2")
  corr_order <- rownames(within_corr_matrix)[hclust_obj$order]

  results_matrix_ordered <- results_matrix[corr_order, corr_order]
  pval_matrix_ordered    <- pval_matrix[corr_order, corr_order]

  # --- Long form for plotting ---
  plot_df <- as.data.frame(as.table(results_matrix_ordered))
  names(plot_df) <- c("Var1", "Var2", "value")
  plot_df$p.value <- as.vector(pval_matrix_ordered)
  plot_df$Var1 <- factor(plot_df$Var1, levels = rev(corr_order))
  plot_df$Var2 <- factor(plot_df$Var2, levels = corr_order)

  no_leading_zero <- function(x) {
    ifelse(is.na(x), NA_character_,
           sub("^(-?)0\\.", "\\1.", sprintf("%.2f", x)))
  }

  plot_df <- plot_df %>%
    mutate(
      is_diag = as.character(Var1) == as.character(Var2),
      is_sig  = p.value < sig.level,
      label   = no_leading_zero(value)
    )

  diag_df    <- plot_df %>% filter(is_diag)
  sig_df     <- plot_df %>% filter(!is_diag & is_sig)
  nonsig_df  <- plot_df %>% filter(!is_diag & !is_sig)

  # --- Plot ---
  p <- ggplot() +
    geom_tile(data = plot_df %>% filter(!is_diag),
              aes(x = Var2, y = Var1, fill = value), color = "gray70", linewidth = 0.5) +
    geom_tile(data = diag_df, aes(x = Var2, y = Var1),
              fill = "gray92", color = "gray70", linewidth = 0.5) +
    scale_fill_gradient2(
      low = "#6D9EC1", mid = "white", high = "#E46726",
      midpoint = 0, limits = c(-1, 1), name = "Correlation"
    ) +
    labs(
      title = "Multilevel Correlation Matrix of Daily Outcomes",
      subtitle = subtitle_text
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "bold", size = 20),
      plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 12, margin = margin(b = 6)),
      axis.text.x   = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.text.y   = element_text(face = "bold"),
      axis.title    = element_blank(),
      legend.position = if (show_legend) "right" else "none",
      panel.grid.major = element_blank()
    )

  p <- p +
    geom_text(data = sig_df, aes(x = Var2, y = Var1, label = label),
              fontface = "bold", color = "black", size = label_size, na.rm = TRUE) +
    geom_text(data = nonsig_df, aes(x = Var2, y = Var1, label = label),
              color = "gray40", size = label_size, na.rm = TRUE)

  if (highlight_diag && nrow(diag_df) > 0) {
    p <- p +
      geom_text(data = diag_df, aes(x = Var2, y = Var1, label = label),
                fontface = "bold", color = "#4B0082", size = diag_label_size, na.rm = TRUE)
  }

  return(p)
}