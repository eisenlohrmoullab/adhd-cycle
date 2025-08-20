
# This function calculates the length of menses-to-menses cycles and identifies incomplete cycles

calculate_mcyclength <- function(df) {
  # Calculate m2mcount: This counts from 1 menses onset to the next to give the length of a menses-to-menses cycle
  df <- df %>%
    group_by(id) %>%
    arrange(daterated, .by_group = TRUE) %>%
    mutate(
      m2mcount = NA,
      mcyclength = NA,
      cycle_incomplete = 0
    )
  
  for (i in seq_len(nrow(df))) {
    if (!is.na(df$A[i]) && df$A[i] == 1) {
      df$m2mcount[i] <- 1
      j <- i + 1
      
      while (j <= nrow(df) &&
             df$id[j] == df$id[i] && (is.na(df$A[j]) || df$A[j] != 1)) {
        df$m2mcount[j] <- df$m2mcount[j - 1] + 1
        j <- j + 1
      }
      
      if (j <= nrow(df) &&
          df$id[j] == df$id[i] && !is.na(df$A[j]) && df$A[j] == 1) {
        df$m2mcount[j] <- df$m2mcount[j - 1] + 1
      }
    }
  }
  
  # Identify incomplete cycles: This identifies instances where a complete menses-to-menses cycle was not captured
  df <- df %>%
    mutate(cycle_incomplete = ifelse(!is.na(m2mcount) &
                                       (is.na(lead(m2mcount)) & id != lead(id)), 1, 0))
  
  # New condition: Set cycle_incomplete = 1 if m2mcount restarts when id changes
  df <- df %>%
    group_by(id) %>%
    mutate(
      cycle_incomplete = ifelse(
        id != lag(id, default = first(id)) & m2mcount == 1, 1, cycle_incomplete
      )
    ) %>%
    ungroup()
  
  # Propagate cycle_incomplete within each group of m2mcount
  df <- df %>%
    group_by(id) %>%
    mutate(cycle_group = cumsum(!is.na(m2mcount) & m2mcount == 1)) %>%
    group_by(id, cycle_group) %>%
    mutate(
      cycle_incomplete = ifelse(any(cycle_incomplete == 1), 1, 0),
      # Fix: handle cases where all values in m2mcount are NA
      mcyclength = ifelse(all(is.na(m2mcount)), NA, max(m2mcount, na.rm = TRUE))
    ) %>%
    ungroup() %>%
    select(-cycle_group)
  
  df$cycle_incomplete = ifelse(is.na(df$cycle_incomplete), 1, 0)
  df$cycle_incomplete = ifelse(is.na(df$m2mcount), NA, df$cycle_incomplete)
  
  # Calculate cyclenum: calculates number of menses-to-menses cycles within a person
  df <- df %>%
    group_by(id) %>%
    mutate(cyclenum = cumsum(!is.na(m2mcount) & m2mcount == 1 & cycle_incomplete == 0)) %>%
    ungroup()
  
  df <- df %>%
    group_by(id) %>%
    mutate(cyclenum = ifelse(
      cycle_incomplete == 1,
      NA,
      cumsum(!is.na(m2mcount) & m2mcount == 1 & cycle_incomplete == 0)
    )) %>%
    ungroup()
  
  return(df)
}