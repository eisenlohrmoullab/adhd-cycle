
# for am_qual, use lubridate to parse "DateTime" which looks like "6/14/21 5:02" into lubridate::mdy
am_qual$daterated <- as.Date(lubridate::mdy_hm(am_qual$DateTime))

# Drop the DateTime and DateTime.1 columns
am_qual <- am_qual %>% select(-DateTime, -DateTime.1)

#Confirm the date format
#str(am_qual$daterated) 

#Check rate of date
#range(am_qual$daterated, na.rm=TRUE) #2021-01-22 to 2021-09-09

# Drop observations where id or daterated are missing
am_qual <- am_qual %>%
  filter(!is.na(id), !is.na(daterated))

# Rename columns starting with "Urg_" or "Crav_" to start with "Crave_"
am_qual <- am_qual %>%
  rename_with(~ gsub("^(Urg_|Crav_)", "Crave_", .), matches("^(Urg_|Crav_)"))

# Code Alc_Drunk such that Yes=1 and No=0 and set as factor
am_qual$Alc_Drunk <- as.factor(ifelse(am_qual$Alc_Drunk == "Yes", 1, 0))
#str(am_qual$Alc_Drunk)

# Inspect and clean Alc_Amt
#str(am_qual$Alc_Amt) #Number
range(am_qual$Alc_Amt, na.rm=TRUE) #0 to 12

# Inspect and clean Alc_Time ------------------------- leave out for now
#str(am_qual$Alc_Time) #character, TODO clean

# Inspect and clean Alc_Dur -------------------------- leave out for now
#str(am_qual$Alc_Dur) #character, TODO clean

# Inspect and clean Crave_Alc
#str(am_qual$Crave_Alc) #integer
#range(am_qual$Crave_Alc, na.rm=TRUE) #0 to 10

# Inspect and clean Crave_Nic
#str(am_qual$Crave_Nic) #integer
#range(am_qual$Crave_Nic, na.rm=TRUE) #0 to 4

# Inspect and clean Crave_Mar
#str(am_qual$Crave_Mar) #integer
#range(am_qual$Crave_Mar, na.rm=TRUE) #0 to 10

# Keep only id, daterated, Alc_Amt, Crave_Alc, Alc_Drunk, Crave_Nic, Crave_Mar
am_qual <- am_qual %>% select(id, daterated, Alc_Amt, Crave_Alc, Alc_Drunk, Alc_Time, Alc_Dur, Crave_Nic, Crave_Mar)

#Check the number of rows
#nrow(am_qual) #551

# Visualize Missing Data
#vis_miss(am_qual) 

# View the Dataset
#View(am_qual)

# Keep only id, daterated, Alc_Amt, Crave_Alc, Alc_Drunk
#print.variable.names(am_qual)

# Print a description of what happened in this script
cat("\n","\n", "The script cleaned the am_qual dataset by parsing the 'DateTime' column into a date format, dropping the 'DateTime' and 'DateTime.1' columns, removing observations with missing id or daterated, renaming columns starting with 'Urg_' or 'Crav_' to start with 'Crave_', coding 'Alc_Drunk' such that 'Yes'=1 and 'No'=0, and keeping only the 'id', 'daterated', 'Alc_Amt', 'Crave_Alc', and 'Alc_Drunk' columns.\n")
