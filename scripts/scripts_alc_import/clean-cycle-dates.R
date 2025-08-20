
################################################ Cleaning Cycle Data


# Import wide cycle dates

# ---- Load the Cycle Dates Dataset ----

rawwidecycledatesdf <- read_csv("~/Library/CloudStorage/Box-Box/00 - CLEAR Lab (Locked Folders)/02 - Data Management, Analysis, and Papers/Studies_Projects/UKALC/02_datasets/UKALC_DAILY/03_cleaned_data/ukalc_taemcycledates_wide.csv", show_col_types = FALSE)

widecycledatesdf <- rawwidecycledatesdf

# Set id as factor for merges
widecycledatesdf$id <- as.factor(widecycledatesdf$id)

# Drop ...5
widecycledatesdf <- widecycledatesdf %>% select(-...5)

#View(cycledatesdf)

print.variable.names(widecycledatesdf)


# Identify ids with missing period1 and period2 start dates and create lists @ids_missing_period1 and @ids_missing_period2

# Create a list of ids where period1_present is 0 that we might want to impute with survey first date value (@ids_missing_period1)
ids_missing_period1 <- widecycledatesdf$id[widecycledatesdf$period1_present == 0]
cat("\n", "\n", "IDs missing period 1: ", paste(ids_missing_period1, collapse = ", "), "\n")

# Create a list of ids where period2_present is 0 that we might want to impute with survey first date value (@ids_missing_period2)
ids_missing_period2 <- widecycledatesdf$id[widecycledatesdf$period2_present == 0]
# Use cat to print the lists
cat("\n", "IDs missing period 2: ", paste(ids_missing_period2, collapse = ", "), "\n")


# ---- Save a Dataset with First and Last Survey Obs for Each Pt ----

#Take the datasets for AM and PF (amdf and pmdf), identify the first and last daterated value for each id in both datasets
last_survey_date_am <- amdf %>% group_by(id) %>% summarize(last_survey_date_am = max(daterated))
last_survey_date_pm <- pmdf %>% group_by(id) %>% summarize(last_survey_date_pm = max(daterated))
first_survey_date_am <- amdf %>% group_by(id) %>% summarize(first_survey_date_am = min(daterated))
first_survey_date_pm <- pmdf %>% group_by(id) %>% summarize(first_survey_date_pm = min(daterated))

# Merge all of these dates into a single dataset
survey_dates <- left_join(last_survey_date_am, last_survey_date_pm, by = "id") %>%
  left_join(first_survey_date_am, by = "id") %>%
  left_join(first_survey_date_pm, by = "id")

# Set id as factor 
survey_dates$id <- as.factor(survey_dates$id)

#View(survey_dates)


# Merge surveydates into widecycledatesdf

# ---- Load the Cycle Dates Dataset ----

widecycledatesdf <- widecycledatesdf %>%
  left_join(survey_dates, by = "id")

#View(widecycledatesdf)

#print.variable.names(widecycledatesdf)

#count_rows_ids(widecycledatesdf)





# For ids in list "ids_missing_period1", replace period1_start with first_survey_date_am
widecycledatesdf$period1[widecycledatesdf$id %in% no_first_period_ids$id] <- widecycledatesdf$first_survey_date_am[widecycledatesdf$id %in% no_first_period_ids$id]

# For ids in list "ids_missing_period2", replace period2_start with last_survey_date_pm
widecycledatesdf$period2[widecycledatesdf$id %in% no_second_period_ids$id] <- widecycledatesdf$last_survey_date_pm[widecycledatesdf$id %in% no_second_period_ids$id]


# Check for cycle lengths

# After imputation of survey dates-- For each id, calculate the days between Period1 and Period2
widecycledatesdf <- widecycledatesdf %>%
  mutate(cycle_length = as.numeric(difftime(period2, period1, units = "days")))

# Print range of cycle dates
#range(widecycledatesdf$cycle_length, na.rm = TRUE) # 23 to 38
# Print with cat
cat("Range of cycle lengths: ", range(widecycledatesdf$cycle_length, na.rm = TRUE), "\n")

# Check for missing cycle lengths
sum(is.na(widecycledatesdf$cycle_length)) # 0

# Print ids with missing lengths
ids_missing_cycle_length <- widecycledatesdf$id[is.na(widecycledatesdf$cycle_length)]
cat("\n", "IDs without necessary cycle bookends even after imputing start and end survey dates: ", paste(ids_missing_cycle_length, collapse = ", "), "\n")

# View the dataset only for ids_missing_cycle_length
#View(widecycledatesdf %>% filter(id %in% ids_missing_cycle_length))

# Confirm that these people don't have any survey data in amdf or pmdf
#View(amdf %>% filter(id %in% ids_missing_cycle_length)) # CONFIRMED, no data

# how many participants have a cycle length? Print their ids. 
ids_with_cycle_length <- widecycledatesdf$id[!is.na(widecycledatesdf$cycle_length)]
cat("\n", "IDs with cycle length: ", paste(ids_with_cycle_length, collapse = ", "), "\n")
# Count



# Ok, so there are no missing cycle lengths for people with ANY survey data, and the cycle lengths are reasonable. Therefore, I am going to drop the ids with no cycle length from the widecycledatesdf dataset and the df (survey) datasets



# Drop ids from widecycledatesdf that have no cycle length
widecycledatesdf <- widecycledatesdf %>% filter(!is.na(cycle_length))

#View(widecycledatesdf)

# Drop ids from df survey dataset that have no cycle length
df <- df %>% filter(id %in% ids_with_cycle_length)

# Merge widecycledatesdf with df
df <- df %>%
  left_join(widecycledatesdf, by = "id")

#count_rows_ids(df)

#vis_miss(df)

# So there are 64 ids with cycle lengths. Now, I will create a new PeriodStart variable daterated in the df dataset based on the first period1_start and period2_start values for each id in the widecycledatesdf dataset.

# Set TAEMPeriodStart to 1 for period1 and period2 start dates, and TAEMOvPos to 1 for any_ov_date


# Create a new variable called TAEMPeriodStart in the df dataset as a factor variable
df$TAEMPeriodStart <- as.factor(NA)
#str(df$TAEMPeriodStart)

df$TAEMOvPos <- as.factor(NA)
#str(df$TAEMOvPos)

#str(df$any_ov_date)
#str(df$period1)
#str(df$period2)
#str(df$daterated)

# Step 1: Initialize TAEMPeriodStart and TAEMOvPos based on daterated
df <- df %>%
  group_by(id) %>%
  mutate(
    TAEMPeriodStart = case_when(
      daterated == period1 ~ 1,
      daterated == period2 ~ 1,
      TRUE ~ 0 # Set to 0 for all other dates
    ),
    TAEMOvPos = if_else(daterated %in% any_ov_date, 1, 0)
  ) %>%
  ungroup()

# Step 2: Define a function to add missing dates for a specified date column
add_missing_dates <- function(df, date_col, target_col) {
  # Identify missing dates for each id
  missing_dates <- df %>%
    distinct(id, !!sym(date_col)) %>%
    rename(daterated = !!sym(date_col)) %>%
    anti_join(df, by = c("id", "daterated")) %>%
    mutate(!!sym(target_col) := 1)  # Set target column to 1 for missing rows
  
  # Combine missing dates back with original data
  df <- df %>%
    bind_rows(missing_dates) %>%
    arrange(id, daterated) %>%
    # Fill target column with 0 where it wasn't marked as 1
    mutate(!!sym(target_col) := ifelse(is.na(get(target_col)), 0, get(target_col)))
  
  return(df)
}

# Step 3: Apply the function to add missing dates for period1, period2, and any_ov_date
df <- add_missing_dates(df, "period1", "TAEMPeriodStart")
df <- add_missing_dates(df, "period2", "TAEMPeriodStart")
df <- add_missing_dates(df, "any_ov_date", "TAEMOvPos")

# Step 4: Remove rows with missing id or daterated
df <- df %>%
  filter(!is.na(id), !is.na(daterated)) %>%
  arrange(id, daterated)

# Print a description of what happened in this script
cat("\n", "The TAEMPeriodStart and TAEMOvPos variables have been created in the 'df' dataset based on the period1, period2, and any_ov_date values in the 'widecycledatesdf' dataset.", "\n")

#View(widecycledatesdf)

#hist(widecycledatesdf$cycle_length)


# Print list of ids in the dataset
#ids_in_dataset <- df$id %>% unique() %>% paste(collapse = ", ")

#ids_in_dataset

# View observations in the dataset where cycle_length is <21 or >35
#View(widecycledatesdf %>% filter(cycle_length < 21 | cycle_length > 35))

# ----- drop participant 185 from the dataset because only has a few days
df <- df %>% filter(id != 185)

#------ Remove both 152 from the dataset because very weird, hormones don't make sense-- 36 day cyc, pregnant? 
df <- df %>% filter(id != 152)

# ----- drop participant 181 from the dataset because of an 18 day cycle
df <- df %>% filter(id != 181)

# ----- For id 140 on daterated 2022-02-08, set TAEMOvPos to 1
# For id 140, set TAEMOvPos to 0 for all dates
df <- df %>%
  mutate(TAEMOvPos = ifelse(id == 140, 0, TAEMOvPos)) # Set all to 0
df <- df %>%
  mutate(TAEMOvPos = ifelse(id == 140 & daterated == "2022-02-08", 1, TAEMOvPos))

# For id 148, set TAEMOvPos to 1 for daterated 2022-02-08
df <- df %>%
  mutate(TAEMOvPos = ifelse(id == 148, 0, TAEMOvPos))
df <- df %>%
  mutate(TAEMOvPos = ifelse(id == 148 & daterated == "2022-05-29", 1, TAEMOvPos))

#For id 136, set TAEMPeriodStart to 0 on daterated 2022-01-01 and set TAEMPeriodStart to 1 on 2021-12-23
df <- df %>%
  mutate(TAEMPeriodStart = ifelse(id == 136 & daterated == "2022-01-01", 0, TAEMPeriodStart))
df <- df %>%
  mutate(TAEMOvPos = ifelse(id == 136 & daterated == "2021-12-23", 1, TAEMOvPos))


# View observations in the dataset where cycle_length is <21 or >35
#View(widecycledatesdf %>% filter(cycle_length < 21 | cycle_length > 35))

# After imputation of survey dates-- For each id, calculate the days between Period1 and Period2
df <- df %>%
  mutate(cycle_length = as.numeric(difftime(period2, period1, units = "days")))
# Print range of cycle dates
#range(df$cycle_length, na.rm = TRUE) # 23 to 38
# View only observations where cycle_length is <21 or >35
#View(df %>% filter(cycle_length < 21 | cycle_length > 35))

ids_with_cycle_length <- df$id[!is.na(df$cycle_length)]
cat("\n", "IDs with cycle length: ", paste(ids_with_cycle_length, collapse = ", "), "\n")
# Count


# how many unique ids have a cycle length? Print their ids. 
ids_with_final_cycle_length <- unique(df$id[!is.na(df$cycle_length)])
cat("\n", "IDs with FINAL cycle length: ", paste(ids_with_final_cycle_length, collapse = ", "), "\n")
# Count

#n_distinct(ids_with_final_cycle_length)

hist(df$cycle_length)

# Write a summary of what this code does. 
cat("\n", "The script cleaned the widecycledatesdf dataset by dropping the ...5 column, identifying ids with missing period1 and period2 start dates, creating lists of ids missing period1 and period2, imputing missing period1 and period2 start dates with the first and last survey dates for each id, calculating the cycle length for each id, and dropping ids with missing cycle lengths from the widecycledatesdf dataset and the df dataset. The script also created TAEMPeriodStart and TAEMOvPos variables in the df dataset based on the period1, period2, and any_ov_date values in the widecycledatesdf dataset.", "\n")