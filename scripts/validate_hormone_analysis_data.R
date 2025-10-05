#!/usr/bin/env Rscript
# Quick validation script to check if data and required variables exist
# for the hormone analysis files

cat("==========================================\n")
cat("ADHD Cycle Hormone Analysis Validation\n")
cat("==========================================\n\n")

# Check if data file exists
data_file <- "data/adhd_daily_scaled_20250929.rdata"
cat("Checking for data file:", data_file, "\n")

if (file.exists(data_file)) {
  cat("✓ Data file found\n\n")
  
  # Load the data
  cat("Loading data...\n")
  load(data_file)
  
  # Check if cycle_df_scaled exists
  if (exists("cycle_df_scaled")) {
    cat("✓ cycle_df_scaled object loaded\n\n")
    
    # Check dimensions
    cat("Dataset dimensions:\n")
    cat("  Rows:", nrow(cycle_df_scaled), "\n")
    cat("  Columns:", ncol(cycle_df_scaled), "\n\n")
    
    # Check for required predictor variables
    cat("Checking for required predictor variables:\n")
    required_predictors <- c("E2.5roll.d", "P4.5roll.d", "id")
    
    for (var in required_predictors) {
      if (var %in% names(cycle_df_scaled)) {
        cat("  ✓", var, "\n")
      } else {
        cat("  ✗", var, "NOT FOUND\n")
      }
    }
    cat("\n")
    
    # Check for outcome variables
    cat("Checking for outcome variables:\n")
    outcomes <- c(
      "CSS_Inatt", "CSS_HypImp", "CSS_Inatt_Count", "CSS_Hyp_Count", 
      "CSS_Imp_Count", "score_pinball", "score_robot", 
      "BDEFS_WM_avg", "BDEFS_RI_avg",
      "DRSP_1", "DRSP_2", "DRSP_3", "DRSP_4", "DRSP_5", "DRSP_6",
      "DRSP_7", "DRSP_8", "DRSP_9", "DRSP_10", "DRSP_11", "DRSP_12",
      "DRSP_13", "DRSP_14", "DRSP_15", "DRSP_16", "DRSP_17", "DRSP_18",
      "DRSP_19", "DRSP_20", "DRSP_21", "DRSP_22", "DRSP_23"
    )
    
    found_outcomes <- 0
    missing_outcomes <- character()
    
    for (outcome in outcomes) {
      if (outcome %in% names(cycle_df_scaled)) {
        found_outcomes <- found_outcomes + 1
      } else {
        missing_outcomes <- c(missing_outcomes, outcome)
      }
    }
    
    cat("  Found:", found_outcomes, "out of", length(outcomes), "outcomes\n")
    
    if (length(missing_outcomes) > 0) {
      cat("\n  Missing outcomes:\n")
      for (outcome in missing_outcomes) {
        cat("    ✗", outcome, "\n")
      }
    }
    cat("\n")
    
    # Check for complete cases with predictors
    cat("Checking data availability:\n")
    complete_e2 <- sum(complete.cases(cycle_df_scaled[, c("E2.5roll.d", "id")]))
    complete_p4 <- sum(complete.cases(cycle_df_scaled[, c("P4.5roll.d", "id")]))
    complete_both <- sum(complete.cases(cycle_df_scaled[, c("E2.5roll.d", "P4.5roll.d", "id")]))
    
    cat("  Complete cases with E2.5roll.d:", complete_e2, "\n")
    cat("  Complete cases with P4.5roll.d:", complete_p4, "\n")
    cat("  Complete cases with both:", complete_both, "\n\n")
    
    # Summary statistics for predictors
    if ("E2.5roll.d" %in% names(cycle_df_scaled) && "P4.5roll.d" %in% names(cycle_df_scaled)) {
      cat("Predictor summary statistics:\n")
      cat("\n  E2.5roll.d:\n")
      print(summary(cycle_df_scaled$E2.5roll.d))
      cat("\n  P4.5roll.d:\n")
      print(summary(cycle_df_scaled$P4.5roll.d))
      cat("\n")
    }
    
    # Check number of subjects
    if ("id" %in% names(cycle_df_scaled)) {
      n_subjects <- length(unique(cycle_df_scaled$id))
      cat("Number of unique participants:", n_subjects, "\n\n")
    }
    
    cat("==========================================\n")
    cat("✓ Validation complete!\n")
    cat("==========================================\n\n")
    cat("The data appears ready for analysis.\n")
    cat("You can now run:\n")
    cat("  - ADHDCYCLE_7_GAMM_Hormones.Rmd for GAMM analysis\n")
    cat("  - ADHDCYCLE_8_MLM_Hormones.Rmd for MLM analysis\n\n")
    
  } else {
    cat("✗ cycle_df_scaled object not found in data file\n")
    cat("The data file should contain an object named 'cycle_df_scaled'\n\n")
  }
  
} else {
  cat("✗ Data file not found\n\n")
  cat("Please ensure the cleaned dataset is available at:\n")
  cat("  ", data_file, "\n\n")
  cat("Or update the 'data_file' path in:\n")
  cat("  - ADHDCYCLE_7_GAMM_Hormones.Rmd\n")
  cat("  - ADHDCYCLE_8_MLM_Hormones.Rmd\n\n")
}

cat("See ANALYSIS_DOCUMENTATION.md for complete details.\n")
