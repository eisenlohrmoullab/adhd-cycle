# FIGURING OUT LAST DATE FOR EACH PERSON FOR PERIOD IMPUTATION


# Output a csv to the output_folder that tells me the last completed survey date (daterated) available for each id in amdf
write.csv(pmdf %>% group_by(id) %>% summarize(last_survey_date = max(daterated)), file = paste0(output_folder, "/last_survey_date_pm.csv"), row.names = FALSE)

#Take the datasets for AM and PF (amdf and pmdf), identify the last daterated value for each id in both datasets
last_survey_date_am <- amdf %>% group_by(id) %>% summarize(last_survey_date_am = max(daterated))
last_survey_date_pm <- pmdf %>% group_by(id) %>% summarize(last_survey_date_pm = max(daterated))
#Merge them together by id
last_survey_date <- left_join(last_survey_date_am, last_survey_date_pm, by = "id")

#Save the merged dataset to the output folder
write.csv(last_survey_date, file = paste0(output_folder, "/last_survey_date_am_pm.csv"), row.names = FALSE)
