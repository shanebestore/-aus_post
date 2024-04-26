# Initialize vectors to store indices
col_index_uplift <- numeric(nrow(output_all_services))
row_index_uplift <- numeric(nrow(output_all_services))

# Iterate over each row
for (i in 1:nrow(output_all_services)) {
  if (output_all_services$DESCRIPTION[i] %in% c("Parcel Post with Signature", "Express Post with Signature", "EPARCEL WINE STD", 
                                                "Express Courier International (eParcel)", "PACK AND TRACK INTERNATIONAL")) {
    
    # Determine uplift_service based on description and region
    if (output_all_services$DESCRIPTION[i] == "eParcel Return To Sender" & output_all_services$REGION[i] == "VIC") {
      uplift_service <- "Regular.VIC"
    } else if (output_all_services$DESCRIPTION[i] == "Express Post eparcel returns" & output_all_services$REGION[i] == "NSW") {
      uplift_service <- "Express.NSW"
    } else {
      uplift_service <- NA
    }
    
    # For rows with specified DESCRIPTION, find column and row indices
    col_name_uplift <- as.character(uplift_service)
    col_match <- which(colnames(customer_uplift_march_24) == col_name_uplift)
    row_name_uplift <- as.character(output_all_services$customer_code[i])
    row_match <- which(rownames(customer_uplift_march_24) == row_name_uplift)
    
    # Check if the column index exists, otherwise assign NA
    if (length(col_match) == 0) {
      col_index_uplift[i] <- NA
    } else {
      col_index_uplift[i] <- col_match
    }
    
    # Check if the row index exists, otherwise assign NA
    if (length(row_match) == 0) {
      row_index_uplift[i] <- NA
    } else {
      row_index_uplift[i] <- row_match
    }
    
  } else {
    # For other rows, assign NA to indices
    col_index_uplift[i] <- NA
    row_index_uplift[i] <- NA
  }
}
