calculate_lutdaycount <- function(df, ovtoday) {
  last_id <- NULL
  lutdaycount1 <- rep(NA, nrow(df))
  lut_incomplete1 <- rep(NA, nrow(df))  # Initialize lut_incomplete
  count_started_row <- NA  # Track the starting row of a lutdaycount1 stretch
  active_count <- FALSE  # Track if lutdaycount1 is currently counting
  
  for (i in 1:nrow(df)) {
    # If id changes, reset lutdaycount1 and lut_incomplete
    if (is.null(last_id) || last_id != df$id[i]) {
      if (active_count) {
        # Mark the whole stretch as incomplete if stopped by id change
        lut_incomplete1[count_started_row:(i - 1)] <- 1
      }
      lutdaycount1[i] <- ifelse(ovtoday[i] == 1, 0, NA)
      active_count <- !is.na(lutdaycount1[i])  # Start counting if lutdaycount1 is initialized
      count_started_row <- ifelse(!is.na(lutdaycount1[i]), i, NA)  # Record start row
    } else if (!is.na(lutdaycount1[i - 1])) {
      lutdaycount1[i] <- lutdaycount1[i - 1] + 1
    }
    
    # If lutdaycount1 is active and stops because of A == 1, set lut_incomplete = 0 for the entire stretch
    if (!is.na(lutdaycount1[i]) && !is.na(df$A[i]) && df$A[i] == 1) {
      lutdaycount1[i] <- NA
      lut_incomplete1[count_started_row:(i - 1)] <- 0  # Mark stretch as complete due to A == 1
      active_count <- FALSE  # Stop counting due to A == 1
    } else if (ovtoday[i] == 1) {
      lutdaycount1[i] <- 0
      active_count <- TRUE  # Start counting at ovtoday == 1
      count_started_row <- i  # Record start row of the count
    }
    
    # Assign the current row to last_id for the next iteration
    last_id <- df$id[i]
  }
  
  # Update the dataset with lutdaycount1 and lut_incomplete
  df$lutdaycount1 <- lutdaycount1
  df$lut_incomplete1 <- lut_incomplete1
  
  return(df)
}