# Import new dataset with visual LH peaks
# ---- Load raw cycle data ---
visLHpeaks <- read_csv("~/Library/CloudStorage/Box-Box/00 - CLEAR Lab (Locked Folders)/02 - Data Management, Analysis, and Papers/Studies_Projects/UKALC/02_datasets/UKALC_DAILY/03_cleaned_data/id_TubeNumber_visual_LH_peak.csv", show_col_types = FALSE)

# Print names of variables in visLHpeaks
print.variable.names(visLHpeaks)
#str(visLHpeaks)

#View(visLHpeaks)
#Set the id as a factor
visLHpeaks$id <- as.factor(visLHpeaks$id)
visLHpeaks$visual_LH_peak <- as.numeric(visLHpeaks$visual_LH_peak)

table(visLHpeaks$visual_LH_peak)

# Set missing values of visual_LH_peak to 0
visLHpeaks$visual_LH_peak[is.na(visLHpeaks$visual_LH_peak)] <- 0

#str(df$TubeNumber)


# Merge visLHpeaks with df by id and TubeNumber
df <- df %>%
  left_join(visLHpeaks, by = c("id", "TubeNumber"))

df %>% dplyr::select(id, TubeNumber, TAEMOvPos, visual_LH_peak) %>% View()

# How many ids have a total sum of 0 for TAEMOvPos? 
df %>%
  group_by(id) %>%
  summarize(sum_TAEMOvPos = sum(TAEMOvPos)) %>%
  filter(sum_TAEMOvPos == 0) %>%
  nrow() # 0

# Please list the ids with no positive ovulation tests. 
df %>%
  group_by(id) %>%
  summarize(sum_TAEMOvPos = sum(TAEMOvPos)) %>%
  filter(sum_TAEMOvPos == 0) %>%
  pull(id) %>% unique() %>% paste(collapse = ", ") 

# Save a list of the ideas with no positive ovulation tests
ids_no_ov_pos <- df %>%
  group_by(id) %>%
  summarize(sum_TAEMOvPos = sum(TAEMOvPos)) %>%
  filter(sum_TAEMOvPos == 0) %>%
  pull(id) %>% unique()