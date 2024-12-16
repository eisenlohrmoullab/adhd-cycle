# for am_rc, use lubridate to parse "DateTime" which looks like "6/14/21 5:02" into lubridate::mdy
am_rc$daterated <- as.Date(lubridate::mdy_hm(am_rc$DateTime))

# Drop the DateTime and DateTime.1 columns
am_rc <- am_rc %>% select(-DateTime)

#Confirm the date format
#str(am_rc$daterated) 

#Check rate of date
#range(am_rc$daterated, na.rm=TRUE) #2021-08-12 to 2023-05-30

# Drop observations where id or daterated are missing
am_rc <- am_rc %>%
  filter(!is.na(id), !is.na(daterated))

# Rename columns starting with "Urg_" or "Crav_" to start with "Crave_"
am_rc <- am_rc %>%
  rename_with(~ gsub("^(Urg_|Crav_)", "Crave_", .), matches("^(Urg_|Crav_)"))

# Code Alc_Drunk such that Yes=1 and No=0 and set as factor
am_rc$Alc_Drunk <- as.factor(ifelse(am_rc$Alc_Drunk == "Yes", 1, 0))
#str(am_rc$Alc_Drunk) # Factor with 0 and 1

# Inspect and clean Alc_Amt
#str(am_rc$Alc_Amt) #Number
#range(am_rc$Alc_Amt, na.rm=TRUE) #.25 to 18

# Inspect and clean Alc_Time ------------------------- leave out for now
#str(am_rc$Alc_Time) #character, TODO clean

# Inspect and clean Alc_Dur -------------------------- leave out for now
#str(am_rc$Alc_Dur) #character, TODO clean

# I discovered that in this dataset the 0 No Urge and 10 Extreme Urge were being coded as characters so this addresses that: 
# Recode "0 No Urge" to 0 and "10 Extreme Urge" to 10, and format as integer for all columns starting with "Crave_"
am_rc <- am_rc %>%
  mutate(across(starts_with("Crave_"), ~ as.integer(ifelse(. == "0 No Urge", 0, 
                                                           ifelse(. == "10 Extreme Urge", 10, .)))))

# Inspect and clean Crave_Alc
#str(am_rc$Crave_Alc) #integer
#range(am_rc$Crave_Alc, na.rm=TRUE) #0 to 9

# Inspect and clean Crave_Nic
#str(am_rc$Crave_Nic) #integer
#range(am_rc$Crave_Nic, na.rm=TRUE) #0 to 8

# Inspect and clean Crave_Mar
#str(am_rc$Crave_Mar) #integer
#range(am_rc$Crave_Mar, na.rm=TRUE) #0 to 1

# Keep only id, daterated, Alc_Amt, Crave_Alc, Alc_Drunk, Crave_Nic, Crave_Mar
am_rc <- am_rc %>% select(id, daterated, Alc_Amt, Crave_Alc, Alc_Drunk, Alc_Time, Alc_Dur, Crave_Nic, Crave_Mar)

#Check the number of rows
#nrow(am_rc) #551

# View the Dataset
#View(am_rc)

# Keep only id, daterated, Alc_Amt, Crave_Alc, Alc_Drunk
#print.variable.names(am_rc)

#If Crave_Alc is not missing but Alc_Amt is missing, set Alc_Amt to 0
am_rc$Alc_Amt[!is.na(am_rc$Crave_Alc) & is.na(am_rc$Alc_Amt)] <- 0


# Visualize Missing Data
vis_miss(am_rc) 

# Print a description of what happened in this script
cat("\n", "\n", "The script cleaned the am_rc dataset by parsing the DateTime column into a date format, dropping the DateTime column, renaming columns starting with 'Urg_' or 'Crav_' to start with 'Crave_', recoding '0 No Urge' to 0 and '10 Extreme Urge' to 10, and keeping only the id, daterated, Alc_Amt, Crave_Alc, Alc_Drunk, Crave_Nic, and Crave_Mar columns. It also set Alc_Drunk as a factor and recoded 'Yes' to 1 and 'No' to 0. Additionally, it set Alc_Amt to 0 if Crave_Alc is not missing but Alc_Amt is missing.")
