# R Script to Merge Group Assignment Files and Create Factor Variables

# set working directory to where the CSV files are located

setwd("~/CLEAR Lab Repositories/adhd-cycle/data/cyclic_time_imp_groups/raw_files/")  # Change this to your actual path



# --- 1. Setup: List of all files ---
# List of the 54 CSV files to be processed.
file_list <- c(
  "BDEFS_RI_avg_g3_class_final.csv", "BDEFS_WM_avg_g2_class_final.csv",
  "CSS_Hyp_Count_g3_class_final.csv", "CSS_HypImp_g2_class_final.csv",
  "CSS_HypImp_g3_class_final.csv", "CSS_Imp_Count_g3_class_final.csv",
  "CSS_Imp_Count_g4_class_final.csv", "CSS_Inatt_Count_g3_class_final.csv",
  "CSS_Inatt_Count_g4_class_final.csv", "CSS_Inatt_g3_class_final.csv",
  "CSS_Inatt_g4_class_final.csv", "DRSP_1_g2_class_final.csv",
  "DRSP_1_g4_class_final.csv", "DRSP_2_g3_class_final.csv",
  "DRSP_2_g4_class_final.csv", "DRSP_3_g3_class_final.csv",
  "DRSP_4_g3_class_final.csv", "DRSP_4_g5_class_final.csv",
  "DRSP_5_g3_class_final.csv", "DRSP_6_g4_class_final.csv",
  "DRSP_7_g2_class_final.csv", "DRSP_9_g3_class_final.csv",
  "DRSP_9_g4_class_final.csv", "DRSP_10_g3_class_final.csv",
  "DRSP_10_g4_class_final.csv", "DRSP_11_g3_class_final.csv",
  "DRSP_12_g3_class_final.csv", "DRSP_13_g3_class_final.csv",
  "DRSP_13_g4_class_final.csv", "DRSP_14_g3_class_final.csv",
  "DRSP_14_g4_class_final.csv", "DRSP_15_g3_class_final.csv",
  "DRSP_15_g4_class_final.csv", "DRSP_16_g3_class_final.csv",
  "DRSP_16_g4_class_final.csv", "DRSP_17_g3_class_final.csv",
  "DRSP_17_g4_class_final.csv", "DRSP_18_g2_class_final.csv",
  "DRSP_18_g3_class_final.csv", "DRSP_19_g2_class_final.csv",
  "DRSP_19_g3_class_final.csv", "DRSP_19_g4_class_final.csv",
  "DRSP_20_g3_class_final.csv", "DRSP_20_g4_class_final.csv",
  "DRSP_21_g4_class_final.csv", "DRSP_22_g2_class_final.csv",
  "DRSP_23_g2_class_final.csv", "DRSP_23_g3_class_final.csv",
  "DRSP_23_g4_class_final.csv", "DRSP_23_g5_class_final.csv",
  "P4_g4_class_final.csv", "score_pinball_g2_class_final.csv",
  "score_robot_g2_class_final.csv", "score_robot_g3_class_final.csv"
)

# --- 2. Processing: Read each file and create a factor variable ---
# We will store each processed data frame in a list.
list_of_processed_dfs <- lapply(file_list, function(file_name) {
  
  # Handle cases where a file might be missing
  if (!file.exists(file_name)) {
    warning(paste("File not found:", file_name))
    return(NULL)
  }
  
  # Read the CSV file
  df <- read.csv(file_name, stringsAsFactors = FALSE)
  
  # Create a unique column name from the filename (e.g., "BDEFS_RI_avg_g3")
  new_col_name <- gsub("_class_final\\.csv$", "", file_name)
  
  # Rename the 'group' column to our new unique name
  names(df)[names(df) == "group"] <- new_col_name
  
  # Convert the new column to a factor
  df[[new_col_name]] <- as.factor(df[[new_col_name]])
  
  # Return the data frame which now contains 'id' and the new factor column
  return(df)
})

# Remove any NULL elements that resulted from missing files
list_of_processed_dfs <- Filter(Negate(is.null), list_of_processed_dfs)


# --- 3. Merging: Combine all processed data frames ---
# Use Reduce to iteratively merge all data frames in the list by the 'id' column.
# 'all = TRUE' performs a full outer join to ensure all IDs are kept.
merged_groups_cti <- Reduce(
  function(x, y) merge(x, y, by = "id", all = TRUE),
  list_of_processed_dfs
)


# --- 4. Note on NAs ---
# The full outer join will create NAs for IDs that were not present in a given file.
# For factor variables, NA is the correct way to represent missing data, so we
# will leave them as they are instead of converting them to 0.


# --- 5. Output: View and save the final dataset ---
# Print the first few rows and dimensions of the final merged data frame
cat("Merging complete.\n")
cat("Dimensions of the final data frame: ", nrow(merged_groups_cti), "rows and", ncol(merged_groups_cti), "columns.\n\n")
print(head(merged_groups_cti))

# Save the final data frame to a new CSV file
write.csv(merged_groups_cti, "merged_groups_cti.csv", row.names = FALSE)
 cat("\nFinal merged data saved to 'merged_groups_cti.csv'\n")

 # save as RDS for future use
 saveRDS(merged_groups_cti, "merged_groups_cti.rds")
 cat("Final merged data also saved to 'merged_groups_cti.rds'\n")