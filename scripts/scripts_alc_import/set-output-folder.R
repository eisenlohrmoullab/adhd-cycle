# Create a new output folder with the current date
current_date <- format(Sys.Date(), "%Y%m%d")  # Produces date as YYYYMMDD
output_folder <- paste0(
  "~/Library/CloudStorage/Box-Box/00 - CLEAR Lab (Locked Folders)/02 - Data Management, Analysis, and Papers/Studies_Projects/UKALC/03_analytic_projects/UKALC-Primary ms/02_datasets_output/",
  current_date
) # Define the output folder with the current date
# State the output folder
cat("Output folder set as: ", output_folder, "\n")