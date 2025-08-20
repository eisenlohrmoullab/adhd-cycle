
################################################
#            Cleaning Cycle Data
################################################

# ---- Setup ----
library(readr)
library(dplyr)
library(tidyr)
library(naniar)   # for vis_miss()

# ---- Load the Cycle Dates Dataset ----
rawwidecycledatesdf <- read_csv(
  "~/Library/CloudStorage/Box-Box/00 - CLEAR Lab (Locked Folders)/
   02 - Data Management, Analysis, and Papers/
   Studies_Projects/UKALC/02_datasets/UKALC_DAILY/
   03_cleaned_data/ukalc_taemcycledates_wide.csv",
  show_col_types = FALSE
)

widecycledatesdf <- rawwidecycledatesdf %>%
  select(-...5) %>%                # drop stray col
  mutate(id = as.factor(id))       # ensure id is a factor

# ---- Identify IDs Missing Period Starts ----
ids_missing_period1 <- widecycledatesdf %>%
  filter(period1_present == 0) %>%
  pull(id)

ids_missing_period2 <- widecycledatesdf %>%
  filter(period2_present == 0) %>%
  pull(id)

cat("IDs missing period1:", paste(ids_missing_period1, collapse = ", "), "\n")
cat("IDs missing period2:", paste(ids_missing_period2, collapse = ", "), "\n\n")

# ---- Compute First/Last Survey Dates from AM/PM Datasets ----
# (assumes amdf and pmdf exist with columns id, daterated)

last_survey_date_am  <- amdf %>% group_by(id) %>% summarize(last_survey_date_am  = max(daterated))
first_survey_date_am <- amdf %>% group_by(id) %>% summarize(first_survey_date_am = min(daterated))

last_survey_date_pm  <- pmdf %>% group_by(id) %>% summarize(last_survey_date_pm  = max(daterated))
first_survey_date_pm <- pmdf %>% group_by(id) %>% summarize(first_survey_date_pm = min(daterated))

survey_dates <- last_survey_date_am %>%
  left_join(last_survey_date_pm,  by = "id") %>%
  left_join(first_survey_date_am, by = "id") %>%
  left_join(first_survey_date_pm, by = "id") %>%
  mutate(id = as.factor(id))

# ---- Merge Survey Dates into widecycledatesdf ----
widecycledatesdf <- widecycledatesdf %>%
  left_join(survey_dates, by = "id")

# ---- Impute Missing Period1/Period2 from Survey Dates ----
widecycledatesdf <- widecycledatesdf %>%
  mutate(
    period1 = if_else(id %in% ids_missing_period1, first_survey_date_am, period1),
    period2 = if_else(id %in% ids_missing_period2, last_survey_date_pm,  period2)
  )

# ---- Calculate Cycle Length ----
widecycledatesdf <- widecycledatesdf %>%
  mutate(cycle_length = as.numeric(difftime(period2, period1, units = "days")))

cat("Range of cycle lengths:",
    range(widecycledatesdf$cycle_length, na.rm = TRUE), "\n")
cat("Missing cycle_length count:",
    sum(is.na(widecycledatesdf$cycle_length)), "\n\n")

# ---- Filter Out IDs Without a Valid Cycle ----
valid_ids <- widecycledatesdf %>%
  filter(!is.na(cycle_length)) %>%
  pull(id)

widecycledatesdf <- widecycledatesdf %>%
  filter(id %in% valid_ids)

# ---- Prepare Combined df (AM+PM) ----
df <- df %>%
  filter(id %in% valid_ids) %>%
  left_join(
    widecycledatesdf %>% select(id, period1, period2, any_ov_date, cycle_length),
    by = "id"
  )

# ---- Create TAEM Flags ----
df <- df %>%
  group_by(id) %>%
  mutate(
    TAEMPeriodStart = as.integer(daterated == period1 | daterated == period2),
    TAEMOvPos       = as.integer(daterated %in% any_ov_date)
  ) %>%
  ungroup()

# ---- Helper to Add Missing-Date Rows for Flags ----
add_missing_dates <- function(dat, date_col, flag_col) {
  missing_rows <- dat %>%
    distinct(id, !!sym(date_col)) %>%
    rename(daterated = !!sym(date_col)) %>%
    anti_join(dat, by = c("id", "daterated")) %>%
    mutate(!!sym(flag_col) := 1L)
  bind_rows(dat, missing_rows) %>%
    arrange(id, daterated) %>%
    mutate(!!sym(flag_col) := replace_na(!!sym(flag_col), 0L))
}

df <- df %>%
  add_missing_dates("period1",     "TAEMPeriodStart") %>%
  add_missing_dates("period2",     "TAEMPeriodStart") %>%
  add_missing_dates("any_ov_date", "TAEMOvPos") %>%
  filter(!is.na(id), !is.na(daterated))

# ---- Drop Problematic Participants ----
df <- df %>% filter(!id %in% c(185, 152, 181))

# ---- Manual OvPos & PeriodStart Tweaks ----
df <- df %>%
  mutate(
    TAEMOvPos = case_when(
      id == 140 & daterated == as.Date("2022-02-08") ~ 1L,
      id == 140                                    ~ 0L,
      id == 148 & daterated == as.Date("2022-05-29") ~ 1L,
      id == 148                                    ~ 0L,
      TRUE                                         ~ TAEMOvPos
    ),
    TAEMPeriodStart = case_when(
      id == 136 & daterated == as.Date("2022-01-01") ~ 0L,
      TRUE                                          ~ TAEMPeriodStart
    ),
    TAEMOvPos = case_when(
      id == 136 & daterated == as.Date("2021-12-23") ~ 1L,
      TRUE                                          ~ TAEMOvPos
    )
  )

# ---- Final Cycle Length & Diagnostics ----
df <- df %>%
  mutate(cycle_length = as.numeric(difftime(period2, period1, units = "days")))

cat("Final IDs with cycle length:\n",
    paste(unique(df$id[!is.na(df$cycle_length)]), collapse = ", "), "\n")

hist(df$cycle_length,
     main = "Distribution of Cycle Lengths",
     xlab = "Days between Period1 and Period2")

# ---- Summary ----
cat(
  "\nSummary of script: This script loads and cleans wide-format cycle date data,\n",
  "drops irrelevant columns, identifies and imputes missing period start dates\n",
  "using first/last survey observations, computes cycle lengths, filters out\n",
  "participants without valid cycles, merges cycle date info into the combined\n",
  "AM/PM dataset, creates TAEMPeriodStart and TAEMOvPos indicator flags (including\n",
  "adding rows for missing dates), applies manual corrections for specific IDs,\n",
  "recalculates cycle lengths, and finally plots the distribution of cycle lengths.\n",
  sep = ""
)

