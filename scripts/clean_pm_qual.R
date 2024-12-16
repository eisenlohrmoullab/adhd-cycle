# for pm_qual, use lubridate to parse "DateTime" which looks like "6/14/21 5:02" into lubridate::mdy
pm_qual$daterated <- as.Date(lubridate::mdy_hm(pm_qual$DateTime))

# Save "before noon" completions as day prior
pm_qual[c('datepart', 'timepart')] <- str_split_fixed(pm_qual$DateTime, ' ', 2)
pm_qual <- pm_qual %>% select(-DateTime)
pm_qual$timepart <- lubridate::hm(pm_qual$timepart)
pm_qual <- pm_qual %>% mutate(pm_qual, b4noon = ifelse(timepart < lubridate::hm("12:00"),1, 0)) 

pm_qual <- pm_qual %>%
  mutate(daterated = case_when(
    b4noon == 1 ~ daterated -1,
    TRUE ~ daterated
  ))

#Confirm the date format
#str(pm_qual$daterated) #Date

#Check rate of date
#range(pm_qual$daterated, na.rm=TRUE) #2021-03-12 to 2021-09-09

# Drop observations where id or daterated are missing
pm_qual <- pm_qual %>%
  filter(!is.na(id), !is.na(daterated))

# Rename columns starting with "Urg_" or "Crav_" to start with "Crave_"
pm_qual <- pm_qual %>%
  rename_with(~ gsub("^(Urg_|Crav_)", "Crave_", .), matches("^(Urg_|Crav_)"))

# Set all variable levels to be whatever integer is included in the response and remove the rest for thse variables: Diff_Inhib, Diff_Stop, Diff_Change, Immed_Payoff, Res_Urge, Diff_LongRew, Diff_ResRew, Dep, Hopeless, Worthless, Anxious, MoodSwing, RejSens, Angry, Conflicts, Loss_Interest, Diff_Conc, Tired, OverEat, Sp_Food_Crav, High_Sleep, Diff_Sleep, Overwhelm, Loss_Control, Breast_Tend, Breast_Swell, Headache, JM_Pain, Loss_Prod, Int_Relat

pm_qual <- pm_qual %>%
  mutate(across(c(Diff_Inhib, Diff_Stop, Diff_Change, Immed_Payoff, Res_Urge, Diff_LongRew, Diff_ResRew, Dep, Hopeless, Worthless, Anxious, MoodSwing, RejSens, Angry, Conflicts, Loss_Interest, Diff_Conc, Tired, OverEat, Sp_Food_Crav, High_Sleep, Diff_Sleep, Overwhelm, Loss_Control, Breast_Tend, Breast_Swell, Headache, JM_Pain, Loss_Prod, Int_Relat), ~ as.integer(gsub("\\D", "", .))))


#print.variable.names(pm_qual)

# Keep only variables to use
pm_qual <- pm_qual %>% select(id, daterated, datepart, timepart, Diff_Inhib, Diff_Stop, Diff_Change, Immed_Payoff, Res_Urge, Diff_LongRew, Diff_ResRew, Dep, Hopeless, Worthless, Anxious, MoodSwing, RejSens, Angry, Conflicts, Loss_Interest, Diff_Conc, Tired, OverEat, Sp_Food_Crav, High_Sleep, Diff_Sleep, Overwhelm, Loss_Control, Breast_Tend, Breast_Swell, Headache, JM_Pain, Loss_Prod, Int_Relat, Crave_Alc, Crave_Nic, Crave_Mar)

#View(pm_qual)

#Check the number of rows
#nrow(pm_qual) #534

# Visualize Missing Data
vis_miss(pm_qual) 

# View the Dataset
#View(pm_qual)

#print.variable.names(pm_qual)

#Set the Crave_ variables as numeric
pm_qual <- pm_qual %>%
  mutate(across(starts_with("Crave_"), as.numeric))

# Print a description of what happened in this script. 
cat("\n", "\n", "\n", "The 'DateTime' variable was parsed into 'daterated' and 'timepart' variables. 'b4noon' was created to indicate if the time was before noon. 'daterated' was adjusted to the day prior for observations before noon. Columns starting with 'Urg_' or 'Crav_' were renamed to start with 'Crave_'. All variables were set to be the integer included in the response and the rest of the response was removed for the following variables: Diff_Inhib, Diff_Stop, Diff_Change, Immed_Payoff, Res_Urge, Diff_LongRew, Diff_ResRew, Dep, Hopeless, Worthless, Anxious, MoodSwing, RejSens, Angry, Conflicts, Loss_Interest, Diff_Conc, Tired, OverEat, Sp_Food_Crav, High_Sleep, Diff_Sleep, Overwhelm, Loss_Control, Breast_Tend, Breast_Swell, Headache, JM_Pain, Loss_Prod, Int_Relat. The dataset was then filtered to keep only the variables to use. The 'Crave_' variables were set as numeric.", "\n")