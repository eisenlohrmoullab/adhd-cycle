df<- amdf %>%
  full_join(pmdf, by = c("id", "daterated"))

vis_miss(df) 

count_rows_ids(df)

#View(UKAlcd)

only_in_amdf <- setdiff(names(amdf), names(pmdf))
only_in_pmdf <- setdiff(names(pmdf), names(amdf))

print(list(only_in_amdf = only_in_amdf, only_in_pmdf = only_in_pmdf))