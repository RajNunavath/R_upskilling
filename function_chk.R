
new_function <- function(var) {
  # Create the frequency table
  freq_table <- as.data.frame(table(adsl[[var]]))
  # Rename the columns to match the SAS output format (VarName, Frequency)
  colnames(freq_table) <- c("Value", "Frequency")
  # Add a column to identify the variable name
  freq_table$Variable <- var
  # Return the frequency table
  return(freq_table)
}
# Call the function for different variables and store the results in a list
freq_list <- list()
# For variables SEX and NAME
freq_list$SEX <- new_function("SEX")
freq_list$RACE <- new_function("RACE")
# Combine the results into one data frame
combined_freq_data <- bind_rows(freq_list$SEX, freq_list$RACE)






# Print the combined data set
print(combined_freq_data)
new_function <- function(var, var1) {
  # Calculate summary statistics grouped by the variable 'var1'
  summary_stats <- adsl %>%
    group_by(.data[[var1]]) %>%
    mutate(
      n = as.character(n()),
      mean = format(round(mean(.data[[var]], na.rm = TRUE)),nsmall=2),
      median = format(median(.data[[var]], na.rm = TRUE),nsmall=2),
      min = as.character(min(.data[[var]], na.rm = TRUE)),
      max = as.character(max(.data[[var]], na.rm = TRUE)),
      std = format(round(sd(.data[[var]], na.rm = TRUE)),nsmall=3),
    ) %>% 
    distinct(n,mean,median,min,std,max) %>% 
    select(n, mean, median, std, min, max, var1) %>%
    pivot_longer(cols = c(n, mean, median, std, min, max), names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = var1, values_from = value)
  
  
  
  # Return the summary statistics as a new data frame
  return(summary_stats)
}

# Call the function for 'age' grouped by 'sex'
summary_age_sex <- new_function(var = "AGE", var1 = "TRT01P")
summary_age_sex1 <- new_function(var = "TRTDURD", var1 = "TRT01P")


# Print the result
print(summary_age_sex)






chk <- tot_ %>%
  group_by(TRT01P,SEX) %>%              # Group by dynamic column names
  summarise(count = n(), .groups = 'drop') %>%             # Count observations in each group
  mutate(percent = count / sum(count) * 100) %>%           # Calculate percentage
  select(-percent) 

# Merge data
mer <- bign %>%
  left_join(chk, by = "trt01p") %>%
  mutate(per = paste0(count, "(", round(count / denom * 100, 1), ")"))

# Sort data
mer <- mer %>%
  arrange(sex)

# Transpose data
chk1 <- chk %>%
  pivot_wider(names_from = trt01p, values_from = per)