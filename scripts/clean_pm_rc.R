# for pm_qual, use lubridate to parse "DateTime" which looks like "6/14/21 5:02" into lubridate::mdy
pm_rc$daterated <- as.Date(lubridate::mdy_hm(pm_rc$DateTime))

# Save "before noon" completions as day prior
pm_rc[c('datepart', 'timepart')] <- str_split_fixed(pm_rc$DateTime, ' ', 2)
pm_rc <- pm_rc %>% select(-DateTime)
pm_rc$timepart <- lubridate::hm(pm_rc$timepart)
pm_rc <- pm_rc %>% mutate(pm_rc, b4noon = ifelse(timepart < lubridate::hm("12:00"),1, 0)) 

pm_rc <- pm_rc %>%
  mutate(daterated = case_when(
    b4noon == 1 ~ daterated -1,
    TRUE ~ daterated
  ))

#Confirm the date format
#str(pm_rc$daterated) #Date

#Check rate of date
#range(pm_rc$daterated, na.rm=TRUE) #2021-08-12 to 2023-05-29

# Drop observations where id or daterated are missing
pm_rc <- pm_rc %>%
  filter(!is.na(id), !is.na(daterated))

# Rename columns starting with "Urg_" or "Crav_" to start with "Crave_"
pm_rc <- pm_rc %>%
  rename_with(~ gsub("^(Urg_|Crav_)", "Crave_", .), matches("^(Urg_|Crav_)"))

#View(pm_rc)

# Recode "0 No Urge" to 0 and "10 Extreme Urge" to 10, and format as integer for all columns starting with "Crave_"
pm_rc <- pm_rc %>%
  mutate(across(starts_with("Crave_"), ~ as.integer(ifelse(. == "0 No Urge", 0, 
                                                           ifelse(. == "10 Extreme Urge", 10, .)))))

# RECODE DRSP VARS at values

# Define the recoding as a named vector
recode_levels <- c(
  "Not at All" = 1, 
  "Minimal" = 2, 
  "Mild" = 3, 
  "Moderate" = 4, 
  "Severe" = 5, 
  "Extreme" = 6
)

# Recode variables in the drsp_vars list
pm_rc <- pm_rc %>%
  mutate(across(all_of(drsp_vars), ~ recode(., !!!recode_levels)))

#View(pm_rc)

# RECODE BDEFS VARS at values

# Define the recoding as a named vector
recode_levels_bdefs <- c(
  "Never or Rarely" = 0, 
  "Sometimes" = 1, 
  "Often" = 2
)

pm_rc <- pm_rc %>%
  mutate(across(all_of(bdefs_vars), ~ as.numeric(recode(., !!!recode_levels_bdefs, .default = as.numeric(.)))))

# Keep only variables to use
pm_rc <- pm_rc %>% select(id, daterated, datepart, timepart, Diff_Inhib, Diff_Stop, Diff_Change, Immed_Payoff, Res_Urge, Diff_LongRew, Diff_ResRew, Dep, Hopeless, Worthless, Anxious, MoodSwing, RejSens, Angry, Conflicts, Loss_Interest, Diff_Conc, Tired, OverEat, Sp_Food_Crav, High_Sleep, Diff_Sleep, Overwhelm, Loss_Control, Breast_Tend, Breast_Swell, Headache, JM_Pain, Loss_Prod, Int_Relat, Crave_Alc, Crave_Nic, Crave_Mar)

#View(pm_rc)

#Check the number of rows
#nrow(pm_rc) #534

# Visualize Missing Data
#vis_miss(pm_rc) 

# View the Dataset
#View(pm_rc)

#str(pm_rc)

#count_rows_ids(pm_rc)

#print.variable.names(pm_rc)

#Set the Crave_ variables as numeric
pm_rc <- pm_rc %>%
  mutate(across(starts_with("Crave_"), as.numeric))

# Print what happened in this script. 
cat("\n", "The date format has been confirmed, observations with missing id or daterated have been removed, columns starting with 'Urg_' or 'Crav_' have been renamed to start with 'Crave_', and the 'Crave_' variables have been recoded to numeric. The DRSP and BDEFS variables have been recoded to numeric values. The dataset has been reduced to only include the variables to be used in the analysis.", "\n")