# --- 1. SETUP & PARAMETERS ---

# Load the data file
load("~/CLEAR Lab Repositories/adhd-cycle/data/adhd_daily_scaled_20250929.RData")

# Define the base folder for all outputs
output_folder_base <- "~/Library/CloudStorage/Box-Box/00 - CLEAR Lab (Locked Folders)/02 - Data Management, Analysis, and Papers/Studies_Projects/CYCLEADHD/03_analytic_projects/CYCADHD_PRIMARY/03_code_dataedits_output"
smm_output_folder <- file.path(output_folder_base, "SMM_outputs") # Simplified folder name

# Choose your time variables
menses_time_variable <- "cyclic_time_impute"
ovulation_time_variable <- "cyclic_time_imp_ov"

# <-- 1. Your complete list of outcomes is defined here.
# Names = technical variable names in the dataframe
# Values = pretty labels for plots
outcomes <- c(
  "CSS_Inatt" = "CSS Inattention Sx Severity",
  "CSS_HypImp" = "CSS Hyperactivity/Impulsivity Sx Severity",
  "CSS_Inatt_Count" = "CSS Inattention Sx Count",
  "CSS_Hyp_Count" = "CSS Hyperactivity Sx Count",
  "CSS_Imp_Count" = "CSS Impulsivity Sx Count",
  "score_pinball" = "Working Memory Score (Pinball)",
  "score_robot" = "Response Inhibition (Robot Factory)",
  "BDEFS_WM_avg" = "Working Memory Sx (BDEFS)",
  "BDEFS_RI_avg" = "Response Inhibition Sx (BDEFS)",
  "DRSP_1" = "Depressed Mood",
  "DRSP_2" = "Hopelessness",
  "DRSP_3" = "Worthlessness/Guilt",
  "DRSP_4" = "Anxiety",
  "DRSP_5" = "Mood Swings",
  "DRSP_6" = "Rejection Sensitivity",
  "DRSP_7" = "Irritability",
  "DRSP_8" = "Interpersonal Conflicts",
  "DRSP_9" = "Less Interest in Activities",
  "DRSP_10" = "Difficulty Concentrating",
  "DRSP_11" = "Lethargy/Fatigue",
  "DRSP_12" = "Increased Appetite/Overate",
  "DRSP_13" = "Food Cravings",
  "DRSP_14" = "Hypersomnia",
  "DRSP_15" = "Insomnia",
  "DRSP_16" = "Overwhelmed/Couldn't Cope",
  "DRSP_17" = "Felt Out of Control",
  "DRSP_18" = "Breast Tenderness",
  "DRSP_19" = "Swelling/Bloating",
  "DRSP_20" = "Headache",
  "DRSP_21" = "Joint/Muscle Pain",
  "DRSP_22" = "Work Impairment",
  "DRSP_23" = "Relationship Impairment",
  "E2" = "Estradiol",
  "P4" = "Progesterone",
  "LH" = "Luteinizing Hormone",
  "UPPS_NU_avg" = "UPPS Negative Urgency",
  "UPPS_PU_avg" = "UPPS Positive Urgency",
  "UPPS_Premed_avg" = "UPPS (Lack of) Premeditation",
  "UPPS_Persev_avg" = "UPPS (Lack of) Perseverance",
  "UPPS_Sens_avg" = "UPPS Sensation Seeking",
  "DEBQ_Total" = "DEBQ Total Score"
)


# --- 2. AUTOMATED ANALYSIS LOOP ---
analysis_combinations <- expand.grid(
  outcome = names(outcomes),
  centering = c("menses", "ovulation"),
  stringsAsFactors = FALSE
)

all_smm_results <- list()
all_gam_results <- list()

for (i in 1:nrow(analysis_combinations)) {
  current_outcome <- analysis_combinations$outcome[i]
  current_centering <- analysis_combinations$centering[i]

  # <-- 3. The loop now gets the pretty label for the current variable.
  current_plot_label <- outcomes[current_outcome]

  # Determine the time variable name
  current_time_var <- ifelse(current_centering == "menses", menses_time_variable, ovulation_time_variable)

  message(paste0("\n🚀 Starting analysis for: '", current_plot_label, "' (", current_centering, "-centered)"))

  preprocessed_data <- preprocess_outcome(data = df_scaled, outcome = current_outcome)

  subset_result <- subset_by_phase_cyclic(
    data = preprocessed_data,
    outcome = current_outcome,
    time_var = current_time_var,
    min_obs = 5 # Using 5 as per your last example
  )
  data_for_analysis <- subset_result$data_subset

  # Skip if there's not enough data after subsetting
  if (nrow(data_for_analysis) < 20) {
    message(paste0("⚠️ Skipping '", current_plot_label, "' due to insufficient data after subsetting."))
    next
  }

  smm_results <- run_smm_cyclic(
    data = data_for_analysis,
    outcome = current_outcome,
    time_var = current_time_var,
    plot_label = current_plot_label, # <-- ...and passes it to the functions.
    g = 2:5, # Using 2:5 as per your last example
    centering = current_centering,
    save_dir = smm_output_folder
  )

  list_name <- paste0(current_outcome, "_", current_centering)
  all_smm_results[[list_name]] <- smm_results

  # Only run GAMs if SMM was successful
  if (!is.null(smm_results)) {
    gams <- model_plot_modx_gam_cyclic(
      data = data_for_analysis,
      outcome = current_outcome,
      time_var = current_time_var,
      smm_result = smm_results,
      plot_label = current_plot_label, # <-- ...and passes it to the functions.
      centering = current_centering,
      save_dir = smm_output_folder
    )
    all_gam_results[[list_name]] <- gams
  }

  message(paste0("✅ Finished analysis for: '", current_plot_label, "' (", current_centering, "-centered)"))
}

message("\n🏁 ALL OUTCOMES PROCESSED! 🏁")