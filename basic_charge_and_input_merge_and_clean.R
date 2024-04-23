# combine the 4 parts together
#output_a_3_combined  <- rbind(output_a_3_reg_VIC, output_b_3_express_VIC, output_c_3_reg_NSW,  output_d_3_express_NSW)
#basic_charges_post_feb<- output_a_3_combined [order(output_a_3_combined$BILLING.DOC), ]
#write.csv(basic_charges_post_feb, file = "basic_charges_post_feb.csv")
# merging the original input file and the post feb basic charges

#the above is redundent
#Take what we want from the DF and put it into the input file
# Rename column in bill data frame
names(bill)[names(bill) == "ARTICLE.ID"] <- "article_id"

# Rename column in output_all_services_2 data frame
names(output_all_services_2)[names(output_all_services_2) == "ARTICLE.ID"] <- "article_id"

# Specify columns to merge from output_all_services_2
merge_cols <- c("uplift_service", "DESCRIPTION", "BILLING.DOC", "article_id", "base_charge_incgst", "base_charge_exgst", "base_charge_tax", "charge_value_uplift", "uplift_figure_exgst", "charge_to_custo_exgst", "cubic_weight", "max_weight", "CHARGE.ZONE", "fuel_surcharge", "fuel_gst", "sec_mng_chrg", "sec_mng_gst", "over_max_limits_fee", "weight_category_max")

# Select only the merge_cols from output_all_services_2
selected_output_all_services_2 <- output_all_services_2[, merge_cols]

# Merge bill and selected columns from output_all_services_2 by "article_id"
final_output <- merge(bill, selected_output_all_services_2, by = c("article_id", "BILLING.DOC", "DESCRIPTION"), all = TRUE)


# Find the position of "AVG..UNIT.PRICE" column
avg_unit_price_index <- which(names(final_output) == "AMOUNT.INCL.TAX")

# Insert new columns after "AVG..UNIT.PRICE"
final_output <- cbind(final_output[, 1:avg_unit_price_index],
                      final_output[, c("base_charge_incgst","base_charge_exgst", "base_charge_tax", "charge_value_uplift", "uplift_figure_exgst", "charge_to_custo_exgst")],
                      final_output[, (avg_unit_price_index + 1):ncol(final_output)])


# Find the position of "AVG..UNIT.PRICE" column
fuel_gst_index <- which(names(final_output) == "FUEL.GST")

# Insert new columns after "AVG..UNIT.PRICE"
final_output <- cbind(final_output[, 1:fuel_gst_index  ],
                      final_output[, c("fuel_surcharge" ,"fuel_gst", "sec_mng_chrg", "sec_mng_gst", "over_max_limits_fee")],
                      final_output[, (fuel_gst_index  + 1):ncol(final_output)])


# Find the position of "AVG..UNIT.PRICE" column
billed_weight_index <- which(names(final_output) == "BILLED.WEIGHT")

# Insert new columns after "AVG..UNIT.PRICE"
final_output <- cbind(final_output[, 1:billed_weight_index  ], 
                      final_output[, c("cubic_weight", "max_weight", "weight_category_max", "uplift_service")],
                      final_output[, (billed_weight_index  + 1):ncol(final_output)])


# Write final_output to a CSV file
write.csv(final_output, file = "final_output.csv")

# Get the current date
#current_date <- Sys.Date()

# Format the date to your desired format
#formatted_date <- format(current_date, "%Y-%m-%d")

# Generate the file name without the date
#output_file <- "output.csv"

# Append the date at the end of the file name
#final_output_with_date <- paste0(gsub(".csv", "", output_file), "_", formatted_date, ".csv")

# Write your data to the CSV file with the generated file name
#write.csv(final_output, file = final_output_with_date)





