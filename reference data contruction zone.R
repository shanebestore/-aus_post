#Some good progress was made here. I would stick with this methodology for the time being
#For the moment though it was decided that we just build out 4 different blocks to get us the CSV for tomorrow
# Initialize lists to store row and column indices

selected_cols <- list()

# Iterate over each row in the dataframe
for (i in 1:nrow(output_a)) {
  # Extract the value of uplift_service and CHARGE.ZONE for the current row
  current_uplift_service <- output_a$uplift_service[i]
  current_charge_zone <- as.character(output_a$CHARGE.ZONE[i])
  
  # Check for missing values in uplift_service
  if (is.na(current_uplift_service)) {
    # If uplift_service is missing, append NA to the indices lists
    selected_cols[[i]] <- NA
    next  # Skip to the next iteration
  }
  
  # Select the appropriate dataframe based on uplift_service value
  if (current_uplift_service == "Regular.VIC") {
    reference_data <- cz_post_feb_eparcel_regular_ex_mel
  } else {
    reference_data <- cz_post_feb_eparcel_regular_ex_syd
  }
  
  # Filter reference data based on weight_category_max
  col_name_max <- as.character(output_a$weight_category_max[i])
  selected_cols[[i]] <- which(colnames(reference_data) == col_name_max)
  
  
}

# Combine row and column indices for each row

col_index_max <- unlist(selected_cols)


output_a <-cbind(output_a, (cbind(col_index_max)))

# Assuming your data frame is named 'output_a' and you want to duplicate the column named 'row_index_max'

duplicate_column <- output_a$CHARGE.ZONE

# Step 2: Rename the duplicated column
names(duplicate_column) <- "row_index_max"

# Step 3: Add the duplicated and renamed column to the original data frame
output_a_2 <- cbind(output_a, duplicate_column)


###### second part ###########
extract_charge_value_max <- function(row_index_max, col_index_max, reference_data) {
  # Extract value from charge dataset
  charge_value <- reference_data[row_index_max, col_index_max]
  return(charge_value)
}

# Iterate over each row in the dataframe
for (i in 1:nrow(output_a)) {
  # Extract the value of row_index_max and col_index_max for the current row
  row_index_max <- output_a$row_index_max[i]
  col_index_max <- output_a$col_index_max[i]
  
  # Check for missing values in row_index_max or col_index_max
  if (is.na(row_index_max) || is.na(col_index_max) || row_index_max == "" || col_index_max == "") {
    output_a$charge_value_max[i] <- NA  # If either is missing, assign NA to charge_value_max
  } else {
    # Select the appropriate dataframe based on the condition (you may need to modify this part based on your conditions)
    if (output_a$uplift_service[i] == "Regular.VIC") {
      reference_data <- cz_post_feb_eparcel_regular_ex_mel
    } else {
      reference_data <- cz_post_feb_eparcel_regular_ex_syd
    }
    
    # Extract charge value using the modified function
    output_a$charge_value_max[i] <- extract_charge_value_max(row_index_max, col_index_max, reference_data)
  }
}

write.csv(output_a, file = "output_a_2.csv")
