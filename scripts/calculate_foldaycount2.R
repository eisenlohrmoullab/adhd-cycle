calculate_foldaycount <- function(df, ovtoday) {
  # Initialize the foldaycount and fol_incomplete columns
  df$foldaycount <- NA
  df$fol_incomplete <- NA
  
  foldaycount <- NA
  fol_incomplete <- NA
  last_id <- NULL
  active_count <- FALSE  # Track if foldaycount is currently active
  count_started_row <- NA  # Track the starting row of a foldaycount stretch
  
  for (i in 1:nrow(df)) {
    
    # If id changes or this is the first row, reset foldaycount and fol_incomplete
    if (is.null(last_id) || is.na(df$id[i]) || (!is.na(last_id) && last_id != df$id[i])) {
      if (active_count) {
        # Mark the whole stretch as incomplete if stopped by id change
        df$fol_incomplete[count_started_row:(i-1)] <- 1
      }
      foldaycount <- ifelse(df$A[i] == 1, 0, NA)
      active_count <- !is.na(foldaycount)  # Start counting if foldaycount is initialized
      fol_incomplete <- NA  # Reset fol_incomplete when id changes
      count_started_row <- ifelse(!is.na(foldaycount), i, NA)  # Record start row
    } else if (!is.na(foldaycount)) {
      foldaycount <- foldaycount + 1
    }
    
    # If foldaycount is active and stopped because ovtoday == 1 (indicating ovulation)
    if (active_count && i > 1 && !is.na(ovtoday[i]) && ovtoday[i] == 1) {
      foldaycount <- NA
      df$fol_incomplete[count_started_row:(i-1)] <- 0  # Mark stretch as complete due to ovulation
      active_count <- FALSE  # Stop counting due to ovulation
      fol_incomplete <- 0
    }
    
    # If A == 1, start or reset the count
    if (!is.na(df$A[i]) && df$A[i] == 1) {
      foldaycount <- 0
      active_count <- TRUE  # Start counting
      count_started_row <- i  # Record start row of the count
    }
    
    # Assign foldaycount and fol_incomplete to the current row
    df$foldaycount[i] <- foldaycount
    df$fol_incomplete[i] <- ifelse(is.na(fol_incomplete), NA, fol_incomplete)
    
    # Update last_id for the next iteration, but only if it's not missing
    if (!is.na(df$id[i])) {
      last_id <- df$id[i]
    }
  }
  
  return(df)
}