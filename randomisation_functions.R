
randomize_binary_column_1 <- function(df, column_name, percentage, prefix = "rand_1_") {
  # Make a copy of the original data frame to avoid modifying it
  df_modified <- df
  
  # Find indices of 1s in the specified column
  ones_indices <- which(df_modified[[column_name]] == 1)
  
  # Determine the number of cases to change to 0 (specified percentage of the total number of 1s)
  num_to_change <- round(length(ones_indices) * percentage)
  
  # Sample indices to change
  indices_to_change <- sample(ones_indices, num_to_change)
  
  # Create a new column with randomized values
  new_column_name <- paste0(prefix, column_name, percentage)
  df_modified[[new_column_name]] <- df_modified[[column_name]]
  df_modified[[new_column_name]][indices_to_change] <- 0
  
  # Number of 1s changed
  ones_changed <- sum(df_modified[[new_column_name]][indices_to_change] == 0)
  
  # Rows that have been changed
  changed_rows <- df_modified[indices_to_change, ]
  
  return(list(num_to_change, df_modified, ones_changed, changed_rows))
}


randomize_binary_column_0 <- function(df, column_name, percentage, prefix = "rand_0_") {
  # Make a copy of the original data frame to avoid modifying it
  df_modified <- df
  
  # Find indices of 1s in the specified column
  ones_indices <- which(df_modified[[column_name]] == 0)
  
  # Determine the number of cases to change to 0 (specified percentage of the total number of 1s)
  num_to_change <- round(length(ones_indices) * percentage)
  
  # Sample indices to change
  indices_to_change <- sample(ones_indices, num_to_change)
  
  # Create a new column with randomized values
  new_column_name <- paste0(prefix, column_name, percentage)
  df_modified[[new_column_name]] <- df_modified[[column_name]]
  df_modified[[new_column_name]][indices_to_change] <- 1
  
  # Number of 1s changed
  ones_changed <- sum(df_modified[[new_column_name]][indices_to_change] == 1)
  
  # Rows that have been changed
  changed_rows <- df_modified[indices_to_change, ]
  
  return(list(num_to_change, df_modified, ones_changed, changed_rows))
}