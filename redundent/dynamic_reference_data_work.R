data_frames <- list(
  "Regular.VIC" = cz_post_feb_eparcel_regular_ex_mel,
  'Express.VIC' = cz_post_feb_eparcel_express_ex_mel#,
#  'Regular.NSW' = cz_post_feb_eparcel_regular_ex_syd,
#  'Express.NSW' = cz_post_feb_eparcel_express_ex_syd,
  # Add more data frames as needed
#  'Placeholder' = NULL  # Placeholder for unknown values
)

# Select the appropriate data frame
selected_data_frame <- data_frames[output_a$uplift_service]

# Modify your existing code to use selected_data_frame instead of cz_post_feb_eparcel_regular_ex_mel
col_name_max <- as.character(output_a$weight_category_max)
col_index_max <- unlist(lapply(output_a$weight_category_max, function(col_name_max) {
  which(colnames(selected_data_frame) == col_name_max)
}))

row_name_max <- as.character(output_a$CHARGE.ZONE)
row_index_max <- unlist(lapply(row_name_max, function(row_name_max) {
  index <- which(rownames(selected_data_frame) == row_name_max)
  if (length(index) == 0) {
    NA
  } else {
    index
  }
}))


output_a_2 <-cbind(output_a, (cbind(row_index_max, col_index_max)))