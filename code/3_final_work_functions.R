# Function to calculate the mean PAS, success rate, and contrast for each participant
calculate_participant_means <- function(df) {
  # Check if the dataframe contains the required columns
  required_columns <- c("participant", "PASkey.keys", "WMresp_acc", "contrast")
  missing_columns <- setdiff(required_columns, colnames(df))
  
  if (length(missing_columns) > 0) {
    return(paste("Error: The following required columns are missing from the dataframe:", 
                 paste(missing_columns, collapse = ", ")))
  }
  
  # Compute the means for each participant
  result <- df |> 
    group_by(participant) |>  
    summarise(
      mean_PAS = mean(PASkey.keys, na.rm = TRUE),       # Calculate mean PAS
      mean_success = mean(WMresp_acc, na.rm = TRUE),    # Calculate mean success rate
      mean_contrast = mean(contrast, na.rm = TRUE)      # Calculate mean contrast
    ) |> 
    mutate(across(starts_with("mean_"), round, 3))  # Round all mean values to 3 decimal places
  
  # Return the computed dataframe
  return(result)
}

# Apply the function to the dataset and store the result
participant_summary <- calculate_participant_means(df)

# Print the result
print(head(participant_summary, 3))
#      participant    mean_PAS  mean_success   mean_contrast
#1           1        1.49         0.756           0.061
#2           2        1.58         0.581           0.110 
#3           3        1.57         0.525           0.058


# Bonus: Function to compute standard errors (SE) for PAS and contrast using data.table
compute_se <- function(dt) {
  result <- dt[, lapply(.SD, function(x) round(sd(x) / sqrt(.N), 3)), 
               by = participant, 
               .SDcols = c("PASkey.keys", "contrast")]
  
  setnames(result, old = c("PASkey.keys", "contrast"), 
           new = c("se_PAS", "se_contrast"))
  
  return(result)  
}

# Convert df to data.table and apply the function
df_dt <- as.data.table(df)
se_results <- compute_se(df_dt)

# Print the result
print(head(se_results, 3))

#       participant  se_PAS    se_contrast

#1:           1      0.026       0.003
#2:           2      0.031       0.003
#3:           3      0.026       0.002


