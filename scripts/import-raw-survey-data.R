
# ---- Load AM Qualtrics Dataset ---- 

raw_am_qual <- read.csv("~/Library/CloudStorage/Box-Box/00 - CLEAR Lab (Locked Folders)/02 - Data Management, Analysis, and Papers/Studies_Projects/UKALC/02_datasets/UKALC_DAILY/02_data_prep_workspace/AM_Qualtrics_csv.csv") 

am_qual <- raw_am_qual

#View(raw_am_qual)

# ---- Load the AM RedCap Dataset ----

raw_am_rc <- read.csv("~/Library/CloudStorage/Box-Box/00 - CLEAR Lab (Locked Folders)/02 - Data Management, Analysis, and Papers/Studies_Projects/UKALC/02_datasets/UKALC_DAILY/02_data_prep_workspace/AM_REDCap_csv.csv")

am_rc <- raw_am_rc

#View(raw_am_rc)

# ---- Load the PM Qualtrics Dataset ----

raw_pm_qual <- read.csv("~/Library/CloudStorage/Box-Box/00 - CLEAR Lab (Locked Folders)/02 - Data Management, Analysis, and Papers/Studies_Projects/UKALC/02_datasets/UKALC_DAILY/02_data_prep_workspace/PM_Qualtrics_csv.csv")

pm_qual <- raw_pm_qual

#View(raw_pm_qual)

# ---- Load the PM RedCap Dataset ----

raw_pm_rc <- read.csv("~/Library/CloudStorage/Box-Box/00 - CLEAR Lab (Locked Folders)/02 - Data Management, Analysis, and Papers/Studies_Projects/UKALC/02_datasets/UKALC_DAILY/02_data_prep_workspace/PM_REDCap_csv.csv")

pm_rc <- raw_pm_rc

#View(raw_pm_rc)


# Remove all non-numeric characters from the ID column
am_rc$ID <- gsub("[^0-9]", "", am_rc$ID)
am_qual$ID <- gsub("[^0-9]", "", am_qual$ID)
pm_rc$ID <- gsub("[^0-9]", "", pm_rc$ID)
pm_qual$ID <- gsub("[^0-9]", "", pm_qual$ID)

# List all unique values of the variable id in am_qual
unique(am_qual$ID)
unique(am_rc$ID)
unique(pm_qual$ID)
unique(pm_rc$ID)

#Rename id columns
am_qual <- rename_id_daterated_E2_P4(am_qual)
am_rc <- rename_id_daterated_E2_P4(am_rc)
pm_qual <- rename_id_daterated_E2_P4(pm_qual)
pm_rc <- rename_id_daterated_E2_P4(pm_rc)

# Reformat the id columns to factor
am_qual$id <- as.factor(am_qual$id)
am_rc$id <- as.factor(am_rc$id)
pm_qual$id <- as.factor(pm_qual$id)
pm_rc$id <- as.factor(pm_rc$id)

# Print a description of what happened in this script
cat("The script imported the raw survey data from Qualtrics and RedCap for the AM and PM surveys. It then removed all non-numeric characters from the ID columns and renamed the ID columns to 'id'. Finally, it reformatted the id columns to factor.")


