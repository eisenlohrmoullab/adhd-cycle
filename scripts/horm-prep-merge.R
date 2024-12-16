# ---- Load Hormone Data ----
rawhormdf <- read_xlsx("~/Library/CloudStorage/Box-Box/00 - CLEAR Lab (Locked Folders)/02 - Data Management, Analysis, and Papers/Studies_Projects/UKALC/02_datasets/UKALC_HORM/02_data_prep_workspace/Salivary_Results_For_Import.xlsx")

# ---- Create a copy for manipulation ----
hormdf <- rawhormdf

# ---- Separate id and day_in_study ----
# This is necessary because the ID and day_in_study are combined in the raw data
hormdf <- separate(hormdf, col = Sample, into = c("id", "day_in_study"), sep = "-")


# rename "tube number" to "TubeNumber"
hormdf <- rename(hormdf, TubeNumber = `tube number`)

#print.variable.names(hormdf)

#View(hormdf)

hormdf$id <- gsub("[^0-9]", "", hormdf$id)

# ---- Standardize Names ----
hormdf <- standardize_variable_names(hormdf)


# --- Setting Variable Formats ----
# Convert id to a character variable for merging
#str(hormdf$id)
hormdf$id <- as.factor(hormdf$id)
#str(hormdf$id)

#Print unique ids
unique(hormdf$id)

table(hormdf$id)

# Convert TubeNumber to a numeric variable for merging
hormdf$TubeNumber <- as.numeric(hormdf$TubeNumber)
hormdf$E2 <- as.numeric(hormdf$E2)
hormdf$P4 <- as.numeric(hormdf$P4)
hormdf$LH <- as.numeric(hormdf$LH)


# ---- Remove missing rows ----
# Remove rows where all values are NA
hormdf <- hormdf[rowSums(is.na(hormdf)) != ncol(hormdf), ]
# Remove rows with NA in key columns (e.g., id, TubeNumber)
hormdf <- hormdf %>%
  filter(!is.na(id), !is.na(TubeNumber))

# ---- Add placeholder rows for missing TubeNumbers ----
hormdf <- hormdf %>%
  group_by(id) %>%
  complete(TubeNumber = seq(min(TubeNumber, na.rm = TRUE), max(TubeNumber, na.rm = TRUE), by = 1)) %>%
  ungroup()

hormdf$TubeNumber <- as.numeric(hormdf$TubeNumber)

# ---- Sort df by id and TubeNumber ----
hormdf <- hormdf %>%
  arrange(id, TubeNumber)


# Need to match the hormdf and df on TubeNumber by creating a new variable daterated in the hormdf dataset based on the first morning daterated value for each id in the amdf dataset

# ---- Create a new variable daterated in the hormdf dataset based on the first morning daterated value for each id in the amdf dataset ----

# Save the first daterated value for each id in the amdf dataset as a trait date variable called first_day_in_study
first_day_in_study <- amdf %>%
  group_by(id) %>%
  summarize(first_day_in_study = min(daterated)) %>%
  ungroup()

# Merge the first_day_in_study variable into the hormdf dataset
hormdf <- hormdf %>%
  left_join(first_day_in_study, by = "id")

# Calculate daterated in the hormdf dataset as the first_day_in_study + TubeNumber - 1
hormdf <- hormdf %>%
  mutate(daterated = first_day_in_study + TubeNumber - 1)

# Check to see if it worked 
#View(hormdf)

vis_miss(hormdf)




# ---- View all Raw Dataset Variable Names ----
#print.variable.names(hormdf)
#print.variable.names(df)
#str(hormdf$daterated)

# When daterated is missing from a row in the hormdf dataset at the end of an id's date range, fill in the missing daterated values using TubeNumber as a counting index day variable within each id
hormdf <- hormdf %>%
  group_by(id) %>%
  fill(daterated) %>% # Fill in missing daterated values using TubeNumber as a counting index day variable within each id
  ungroup()

# ---- Sort df by id and daterated ----
hormdf <- hormdf %>%
  arrange(id, daterated)

# ---- View all Raw Dataset Variable Names ----
##print.variable.names(hormdf)

# ---- Count rows and unique ids ----
#count_rows_ids(hormdf)

# ---- View the dataset ----
#View(hormdf)


# Merge together the hormone data with the main dataset

# Merge hormdf with df by id and daterated only including observations that are in both datasets

#str(df$id)
#str(hormdf$id)
#str(df$daterated)
#str(hormdf$daterated)

df <- df %>%
  left_join(hormdf, by = c("id", "daterated"))

#View(df)

# Sort df by id and daterated
df <- df %>%
  arrange(id, daterated)

print.variable.names(df)
count_rows_ids(df)
vis_miss(df)

# how many nonmissing E2 values for each participant id? 
df %>%
  group_by(id) %>%
  summarize(n_E2 = sum(!is.na(E2))) %>%
  arrange(n_E2) %>% View()

#Create a histogram of n_E2
df %>%
  group_by(id) %>%
  summarize(n_E2 = sum(!is.na(E2))) %>%
  ggplot(aes(x = n_E2)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Number of Non-Missing E2 Values per Participant", x = "Number of Non-Missing E2 Values", y = "Count") +
  theme_minimal()

ggsave(filename = paste0(output_folder, "/n_E2_histogram.png"), width = 6, height = 4)

# Print a description of what happened in this script. 
cat("\n", "The hormone data was imported and merged with the main dataset. The id and day_in_study were separated. The TubeNumber was converted to a numeric variable. Missing rows were removed. Placeholder rows were added for missing TubeNumbers. The dataset was sorted by id and TubeNumber. A new variable daterated was created in the hormone dataset based on the first morning daterated value for each id in the main dataset. Missing daterated values were filled in using TubeNumber as a counting index day variable within each id. The hormone data was merged with the main dataset by id and daterated. The dataset was sorted by id and daterated. The number of nonmissing E2 values for each participant id was calculated and visualized in a histogram.")
