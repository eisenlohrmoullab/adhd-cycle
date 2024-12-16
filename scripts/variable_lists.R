# Identifiers
identifiers <- c("id", "daterated")

# DRSP Variables
drsp_vars <- c(
  "Dep", "Hopeless", "Worthless", "Anxious", "MoodSwing", "RejSens", 
  "Angry", "Conflicts", "Loss_Interest", "Diff_Conc", "Tired", 
  "OverEat", "Sp_Food_Crav", "High_Sleep", "Diff_Sleep", "Overwhelm", 
  "Loss_Control", "Breast_Tend", "Breast_Swell", "Headache", 
  "JM_Pain", "Loss_Prod", "Int_Relat"
)

# BDEFS Variables
bdefs_vars <- c("Diff_Inhib", "Diff_Stop", "Diff_Change", "Immed_Payoff", 
                "Res_Urge", "Diff_LongRew", "Diff_ResRew")

# Cycle Variables
cyclevars <- c("ovpos", "firstdayofperiod", "cycleday", "cycledaywzero", "midluteal_count", "perimenstrual_count", 
               "midfol_count", "periov_count", "cyclephase_count", "midluteal_LH", "perimenstrual_LH", 
               "midfol_LH", "periov_LH", "earlylut_LH", "sumdummy", "cyclephase_LH", 
               "cyclephase_LHnames", "E2", "P4", "LH", "removed_from_horm_analysis", 
               "m2mcount", "mcyclength", "cycle_incomplete", "cyclenum", "lutdaycount1", 
               "lut_incomplete1", "foldaycount", "fol_incomplete")

# Time Variables
timevars <- c("visitnum", "day_in_study", "TubeNumber")

# Daily Variables
alldailyvars <- c("drink_today", "drink_today_bin", "drink_today", "fourplustoday", "Crave_Alc_AM", "Crave_Alc_PM", "Dep", "Hopeless", 
                  "Worthless", "Anxious", "MoodSwing", "RejSens", "Angry", "Conflicts", 
                  "Loss_Interest", "Diff_Conc", "Tired", "OverEat", "Sp_Food_Crav", "High_Sleep", 
                  "Diff_Sleep", "Overwhelm", "Loss_Control", "Breast_Tend", "Breast_Swell", 
                  "Headache", "JM_Pain", "Loss_Prod", "Int_Relat", "Diff_Inhib", "Diff_Stop", 
                  "Diff_Change", "Immed_Payoff", "Res_Urge", "Diff_LongRew", "Diff_ResRew", "BDEFS")

# Core DVs
core_dvs <- c( "drink_today_bin", "drink_today", "fourplustoday_bin", "fourplustoday", "BDEFS", "Dep", "Anxious", "RejSens", "Angry", "Conflicts", "Overwhelm", "JM_Pain")

# Hormone List
hormlist <- c("E2", "P4", "LH")

# Combine daily vars and hormone list
coredailyvars <- c(core_dvs, hormlist)
alldailyvars <- c(alldailyvars, hormlist)

# Define suffixes
suffixes <- c(".3roll", ".5roll")
log_simp_suff <- c(".log")
log_suffixes <- c(".3roll.log", ".5roll.log")
log_zd_suffixes <- c(".3roll.log.zd", ".5roll.log.zd")
log_d_suffixes <- c(".3roll.log.d", ".5roll.log.d")

# Generate lists with suffixes
alldailyvars_log <- unlist(lapply(log_simp_suff, function(suffix) paste0(alldailyvars, suffix)))
alldailyvars_roll <- unlist(lapply(suffixes, function(suffix) paste0(alldailyvars, suffix)))
alldailyvars_roll_d <- unlist(lapply(suffixes, function(suffix) paste0(alldailyvars_roll, suffix, ".d")))
alldailyvars_roll_zd <- unlist(lapply(suffixes, function(suffix) paste0(alldailyvars_roll, suffix, ".zd")))
alldailyvars_roll_log <- unlist(lapply(log_suffixes, function(suffix) paste0(alldailyvars, suffix)))
alldailyvars_roll_log_zd <- unlist(lapply(log_zd_suffixes, function(suffix) paste0(alldailyvars, suffix)))
alldailyvars_roll_log_d <- unlist(lapply(log_d_suffixes, function(suffix) paste0(alldailyvars, suffix)))
alldailyvars_d <- unlist(lapply(suffixes, function(suffix) paste0(alldailyvars, suffix, ".d")))
alldailyvars_zd <- unlist(lapply(suffixes, function(suffix) paste0(alldailyvars, suffix, ".zd")))

