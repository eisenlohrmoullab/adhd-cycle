calculate_foldaycount <- function(df, ovtoday) {
  foldaycount <- NA
  fol_incomplete <- NA
  last_id <- NULL
  active_count <- FALSE  # Track if foldaycount is currently active
  count_started_row <- NULL  # Track the starting row of a foldaycount stretch
  
  for (i in 1:nrow(df)) {
    # If id changes, reset foldaycount and fol_incomplete
    if (is.null(last_id) || is.na(df$id[i]) || last_id != df$id[i]) {
      if (active_count) {
        # Mark the whole stretch as incomplete if stopped by id change
        df$fol_incomplete[count_started_row:i-1] <- 1
      }
      foldaycount <- ifelse(df$A[i] == 1, 0, NA)
      active_count <- !is.na(foldaycount)  # Start counting if foldaycount is initialized
      fol_incomplete <- NA  # Reset fol_incomplete when id changes
      count_started_row <- ifelse(!is.na(foldaycount), i, NA)  # Record start row
    } else if (!is.na(foldaycount)) {
      foldaycount <- foldaycount + 1
    }
    
    # If foldaycount is active and stopped because of ovtoday == 1, set fol_incomplete = 0 for the whole stretch
    if (!is.na(foldaycount) && i >= 2 && !is.na(ovtoday[i]) && ovtoday[i - 1] == 1) {
      foldaycount <- NA
      df$fol_incomplete[count_started_row:i-1] <- 0  # Mark stretch as complete due to ovulation
      active_count <- FALSE  # Stop counting due to ovulation
      fol_incomplete <- 0
    }
    
    # If A == 1, start or reset the count
    if (df$A[i] == 1) {
      foldaycount <- 0
      active_count <- TRUE  # Start counting
      count_started_row <- i  # Record start row of the count
    }
    
    # If foldaycount is actively counting and stops because of id change
    if (!is.na(last_id) && !is.na(df$id[i]) && last_id != df$id[i] && active_count) {
      df$fol_incomplete[count_started_row:i-1] <- 1  # Mark as incomplete because the count stopped due to id change
      active_count <- FALSE  # Stop counting as the id changed
    }
    
    # Assign foldaycount and fol_incomplete to the current row
    df$foldaycount[i] <- foldaycount
    if (is.na(fol_incomplete)) {
      df$fol_incomplete[i] <- NA
    }
    
    # Update last_id for the next iteration
    last_id <- df$id[i]
  }
  
  return(df)
}