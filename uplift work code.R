# Initialize vectors to store indices
col_index_uplift <- numeric(nrow(output_all_services))
row_index_uplift <- numeric(nrow(output_all_services))

# Iterate over each row
for (i in 1:nrow(output_all_services)) {
  if (output_all_services$DESCRIPTION[i] %in% c("eParcel Return To Sender",
                                                "eParcel Post Return", "eParcel Call For Return", "Express Post eparcel returns","International Returns AIR",
                                                "International Returns Express")) {
    
    # Determine uplift_service based on description and region
    uplift_service <- ifelse(
      output_all_services$DESCRIPTION[i] %in% c("eParcel Return To Sender", "eParcel Post Return", "eParcel Call For Return") & 
        output_all_services$REGION[i] == "VIC", "Regular.VIC",
      ifelse(
        output_all_services$DESCRIPTION[i] %in% c("eParcel Return To Sender", "eParcel Post Return", "eParcel Call For Return") & 
          output_all_services$REGION[i] == "NSW", "Regular.NSW",
        ifelse(
          output_all_services$DESCRIPTION[i] == "Express Post eparcel returns" & output_all_services$REGION[i] == "VIC", "Express.VIC",
          ifelse(
            output_all_services$DESCRIPTION[i] == "Express Post eparcel returns" & output_all_services$REGION[i] == "NSW", "Express.NSW",
            ifelse(
              output_all_services$DESCRIPTION[i] %in% c("International Returns AIR", "International Returns Express"), "International", NA
            )
          )
        )
      )
    )
    
    # Find column and row indices
    col_name_uplift <- as.character(uplift_service)
    col_match <- which(colnames(customer_uplift_march_24) == col_name_uplift)
    row_name_uplift <- as.character(output_all_services$customer_code[i])
    row_match <- which(rownames(customer_uplift_march_24) == row_name_uplift)
    
    # Assign indices or NA
    col_index_uplift[i] <- ifelse(length(col_match) == 0, NA, col_match)
    row_index_uplift[i] <- ifelse(length(row_match) == 0, NA, row_match)
    
  } else {
    # Assign existing uplift service and indices or NA
    uplift_service <- output_all_services$uplift_service[i]
    col_index_uplift[i] <- ifelse(length(col_match) == 0, NA, col_match)
    row_index_uplift[i] <- ifelse(length(row_match) == 0, NA, row_match)
  }
}

# Combine the indices with the original dataframe
output_all_services_2 <- cbind(output_all_services, row_index_uplift, col_index_uplift)




write.csv(output_all_services_2, file = "output_all_services_2.csv")
