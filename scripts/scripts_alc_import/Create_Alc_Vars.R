# Set Alc_Amt to 0 if Crave_Alc_AM is not missing but Alc_Amt is missing
df$Alc_Amt[!is.na(df$Crave_Alc_AM) & is.na(df$Alc_Amt)] <- 0

# Create lagged and derived variables
df <- df %>%
  group_by(id) %>%  # Group by id for lagging
  arrange(daterated) %>%  # Ensure data is ordered by date for correct lagging
  mutate(
    Alc_Amt_today = dplyr::lead(Alc_Amt, n = 1), 
    Alc_Dur_today = dplyr::lead(Alc_Dur, n = 1), 
    Alc_Time_today = dplyr::lead(Alc_Time, n = 1),
    Alc_Drunk_today = dplyr::lead(Alc_Drunk, n = 1),
    
    # Create the drink_today variable based on Alc_Amt_today
    drink_today = case_when(
      is.na(Alc_Amt_today) ~ NA_real_,   # If Alc_Amt_today is missing, set drink_today to NA
      Alc_Amt_today > 0 ~ 1,             # If Alc_Amt_today > 0, set drink_today to 1
      Alc_Amt_today == 0 ~ 0             # Explicitly set days with Alc_Amt_today == 0 to 0 (No)
    )
  ) %>%
  ungroup() %>%  # Ungroup after lagging
  mutate(
    drink_today = factor(drink_today, levels = c(0, 1), labels = c("No", "Yes")),  # Convert to factor with labels
    
    # Create fourplustoday variable, handling NA values
    fourplustoday = case_when(
      is.na(Alc_Amt_today) ~ NA_character_,  # If Alc_Amt_today is missing, set fourplustoday to NA
      Alc_Amt_today >= 4 ~ "Yes",            # If Alc_Amt_today >= 4, set fourplustoday to "Yes"
      Alc_Amt_today < 4 ~ "No"               # If Alc_Amt_today < 4, set fourplustoday to "No"
    ),
    
    # Convert fourplustoday to factor
    fourplustoday = factor(fourplustoday, levels = c("No", "Yes"))
  )

# Create Numerical (non-factor) binary vars (0, 1)

df <- df %>%
  mutate(drink_today_bin = as.numeric(drink_today) - 1, 
         fourplustoday_bin = as.numeric(fourplustoday) - 1, 
  )


# Print narrative of script actions
cat("Created Alc_Amt_today, Alc_Dur_today, Alc_Time_today, Alc_Drunk_today, drink_today (drink_today_bin), and fourplustoday (fourplustoday_bin) variables.\n")

# View relevant variables for checking
# df %>% arrange(id, daterated) %>% dplyr::select(id, daterated, Alc_Amt, Alc_Amt_today, drink_today, fourplustoday) %>% View()

