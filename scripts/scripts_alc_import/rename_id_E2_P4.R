rename_id_daterated_E2_P4 <- function(df) {

  # Standardize ID column names for people
  df <- df %>%
    rename_with(~ gsub("([Ss]ubject.?[Ii][Dd]|[Pp]articipant.?[Ii][Dd]|[Rr]ecord.?[Ii][Dd]|[Uu]ser.?[Ii][Dd]|[Pp]atient.?[Ii][Dd]|[Cc]ase.?[Ii][Dd]|[Rr]espondent.?[Ii][Dd]|[Ii]dentifier|[Ii][Dd]_number|[Uu]nique[Ii][Dd]|[Kk]ey[Ii][Dd]|[Ee]ntry[Ii][Dd]|[Ii][Dd])", "id", .), 
                matches("subject|Subject|participant|Participant|record|Record|user|User|patient|Patient|case|Case|respondent|Respondent|Identifier|ID|id"))
  
  # Standardize estradiol/estrogen-related column names to "E2" with word boundaries
  df <- df %>%
    rename_with(~ gsub("\\b([Ee]strogen|[Ee]stradiol|[Ee]2)\\b", "E2", .), 
                matches("\\bestrogen\\b|\\bEstradiol\\b|\\bE2\\b"))
  
  # Standardize progesterone-related column names to "P4" with word boundaries
  df <- df %>%
    rename_with(~ gsub("\\b([Pp]rogesterone|[Pp]4)\\b", "P4", .), 
                matches("\\bprogesterone\\b|\\bProgesterone\\b|\\bP4\\b"))
  
  return(df)
}