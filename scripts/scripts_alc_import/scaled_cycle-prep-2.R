# ---------------------------------------------------------------------------------- CYCLE PREP SECTION

# Create dichotomous mensdayone/mensdayonefirst to indicate first day of period

# Rename variables
df <- df %>%
  rename(PosLHTest = TAEMOvPos)

df <- df %>%
  filter(!is.na(id))

df <- df %>%
  arrange(id, daterated)

df$StartPeriod <- as.numeric(df$TAEMPeriodStart)

df <- df %>%
  mutate(mensdayone = case_when(is.na(StartPeriod) ~ NA_real_, StartPeriod == 1 ~ 1, TRUE ~ 0))

# Eliminate all but the first menses onset day in a new variable called "mensdayonefirst"

df <- df %>%
  arrange(id, daterated) %>%
  group_by(id) %>%
  mutate(
    mensdayonefirst = case_when(
      is.na(mensdayone) ~ NA,
      mensdayone == 1 & (lag(mensdayone, order_by = daterated) != 1 | is.na(lag(mensdayone, order_by = daterated))) ~ 1,
      row_number() == 1 & mensdayone == 1 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup()

# Create "LHposday" (0/1/NA) that represents the positive LH day.

df$PosLHTest <- as.numeric(df$PosLHTest)

df <- df %>%
  mutate(LHposday = case_when(is.na(PosLHTest) ~ NA_real_, PosLHTest == 1 ~ 1, TRUE ~ 0))

# Eliminate all but the first positive Ov day in a new variable called "LHposdayfirst"
df <- df %>%
  arrange(id, daterated) %>%
  group_by(id) %>%
  mutate(LHposdayfirst = case_when(
    is.na(LHposday) ~ NA_real_,
    LHposday == 1 & lag(LHposday, order_by = daterated) != 1 ~ 1,
    TRUE ~ 0
  )) %>%
  ungroup()

# Code Cycle Day Forward + Backward count from menses onset

df$A <- df$mensdayonefirst

cycleCount <- function(x) {
  inds <- which(x == 1)
  if (!length(inds))
    return(rep(NA, length(x)))
  num <- lapply(inds, function(i) {
    num <- seq_along(x) - i
    num[num >= 0] <- num[num >= 0] + 1
    num[num < -15 | num > 11] <- NA
    num
  })
  do.call(coalesce, num)
}

df <- df %>% group_by(id) %>%
  mutate(cycleday = cycleCount(A))

# Review Data, looking for cycleday=0 and removing those who never had any menses onsets

# Check for observations with cycle day of 0 (which is incorrect)
df %>% filter(cycleday == 0) %>% pull(id) %>% unique() -> ids_w_no_menses_onset

# View the data for these IDs
filter(df, id %in% ids_w_no_menses_onset) %>% dplyr::select(id, daterated, StartPeriod, mensdayonefirst, cycleday, LHposdayfirst) %>% View()

# Remove rows with specified IDs (optional)
df <- df %>% filter(!(id %in% ids_w_no_menses_onset))

# Make daycountLH, based on positive LH test (LH test day = 0)
df$L <- df$LHposdayfirst

LHCount <- function(x) {
  inds <- which(x == 1)
  if (!length(inds))
    return(rep(NA, length(x)))
  num <- lapply(inds, function(i) {
    num <- seq_along(x) - i
    num[num < -7 | num > 15] <- NA
    num
  })
  do.call(coalesce, num)
}

df <- df %>% group_by(id) %>%
  mutate(daycountLH = LHCount(L)) %>%
  dplyr::select(-L)

# Group the dataset by 'id', sort each group by 'daterated', and add a new column 'folmax' with NA values.
df <- df %>%
  group_by(id) %>%
  arrange(daterated, .by_group = TRUE) %>%
  mutate(folmax = NA)

# Create a new column 'A' to indicate menses onset.
df <- df %>%
  mutate(A = ifelse(is.na(cycleday), 0, ifelse(cycleday == 1, 1, 0)))

# Calculate 'ovtoday' by lagging the 'LHposdayfirst' within each group.
df <- df %>%
  mutate(ovtoday = dplyr::lag(LHposdayfirst, default = 0))

# Calculate Menses-to-Menses Cycle Lengths
source("scripts/calculate_mcyclength.R")
df <- calculate_mcyclength(df)

df$mcyclength <- ifelse(df$mcyclength == -Inf, NA, df$mcyclength)
df$cycle_incomplete <- ifelse(is.na(df$cyclenum), 1, df$cycle_incomplete)

# Ensure 'scaled_cycleday' and 'scaled_cycleday_ov' exist before using them
# Adding placeholder calculation for 'scaled_cycleday' and 'scaled_cycleday_ov'
df <- df %>%
  mutate(
    scaled_cycleday = ifelse(is.na(cycleday), NA_real_, (cycleday - min(cycleday, na.rm = TRUE)) / (max(cycleday, na.rm = TRUE) - min(cycleday, na.rm = TRUE))),
    scaled_cycleday_ov = ifelse(is.na(daycountLH), NA_real_, (daycountLH - min(daycountLH, na.rm = TRUE)) / (max(daycountLH, na.rm = TRUE) - min(daycountLH, na.rm = TRUE)))
  )

# Rounding Percents for LUTEAL-FOLLICULAR ORDER
df <- df %>%
  mutate(cycleday_perc = (scaled_cycleday + 1) / 2) %>%
  mutate(cycleday_perc_round = round(cycleday_perc, 1)) %>%
  mutate(cycleday_10perc = round(cycleday_perc / 0.10) * 0.10) %>%
  mutate(cycleday_5perc = round(cycleday_perc / 0.05) * 0.05) %>%
  mutate(cycleday_2perc = round(cycleday_perc / 0.02) * 0.02)

# Rounding Percents for FOLLICULAR-LUTEAL ORDER
df <- df %>%
  mutate(cycledayov_perc = (scaled_cycleday_ov + 1) / 2) %>%
  mutate(cycledayov_perc_round = round(cycledayov_perc, 1)) %>%
  mutate(cycledayov_10perc = round(cycledayov_perc / 0.10) * 0.10) %>%
  mutate(cycledayov_5perc = round(cycledayov_perc / 0.05) * 0.05) %>%
  mutate(cycledayov_2perc = round(cycledayov_perc / 0.02) * 0.02)

# Print description of what happened in this script
cat("Scaled cycle day variables have been created.\n")
