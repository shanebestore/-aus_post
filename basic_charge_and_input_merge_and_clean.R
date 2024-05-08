#### Generate billing doc output file (line by line comparison) ####
# join the selected columns onto the billing doc in the correct places
# this creates the billing_doc_output which is a just the original output will the new columns added

# rename the article_id
names(bill)[names(bill) == "ARTICLE.ID"] <- "article_id"
names(output_all_services_2)[names(output_all_services_2) == "ARTICLE.ID"] <- "article_id"

# Specify columns to merge from output_all_services_2
merge_cols <- c("customer_code2", "customer_code", "service", "uplift", "DESCRIPTION", "BILLING.DOC", "article_id", 
                "base_charge_incgst", "base_charge_exgst", "base_charge_tax", "charge_value_uplift", "uplift_figure_exgst", 
                "charge_to_custo_exgst", "cubic_weight", "max_weight", "fuel_surcharge", "fuel_gst", "sec_mng_chrg", "sec_mng_gst", 
                "over_max_limits_fee", "weight_category_max", "warnings", "is_gst_free_zone")

selected_output_all_services_2 <- output_all_services_2[, merge_cols]

# Merge bill and selected columns from output_all_services_2 by the unique identifier 
billing_doc_output <- merge(bill, selected_output_all_services_2, by = c("article_id", "BILLING.DOC", "DESCRIPTION"), all = TRUE)

# Insert new columns after "AVG..UNIT.PRICE"
avg_unit_price_index <- which(names(billing_doc_output ) == "AMOUNT.INCL.TAX")
billing_doc_output  <- cbind(billing_doc_output [, 1:avg_unit_price_index],
                      billing_doc_output [, c("base_charge_incgst","base_charge_exgst", "base_charge_tax", "charge_value_uplift", "uplift_figure_exgst", "charge_to_custo_exgst", "warnings")],
                      billing_doc_output [, (avg_unit_price_index + 1):ncol(billing_doc_output )])


# Insert new columns after "FUEL.GST"
fuel_gst_index <- which(names(billing_doc_output ) == "FUEL.GST")
billing_doc_output  <- cbind(billing_doc_output [, 1:fuel_gst_index  ],
                      billing_doc_output [, c("fuel_surcharge" ,"fuel_gst", "sec_mng_chrg", "sec_mng_gst", "over_max_limits_fee")],
                      billing_doc_output [, (fuel_gst_index  + 1):ncol(billing_doc_output )])


# Insert new columns after "BILLED.WEIGHT"
billed_weight_index <- which(names(billing_doc_output ) == "BILLED.WEIGHT")
billing_doc_output  <- cbind(billing_doc_output [, 1:billed_weight_index  ], 
                      billing_doc_output [, c("cubic_weight", "max_weight", "weight_category_max", "service", "uplift")],
                      billing_doc_output [, (billed_weight_index  + 1):ncol(billing_doc_output )])


# Quick descrepancy check 

discrepancy <- function(billing_doc_output) {
# Check if the rounded value of AMOUNT.EXCL.TAX is equal to base_charge_exgst
  billing_doc_output$discrepancy <- ifelse(
  round(billing_doc_output$AMOUNT.INCL.TAX, 2) == round(billing_doc_output$base_charge_incgst, 2),
   "no",
   "yes"
  )
  return(billing_doc_output)
}
billing_doc_output <- discrepancy(billing_doc_output)

# generate the file path
output_folder <- file.path(getwd(), paste0("output_billing_dates_", predefined_text))
if (!file.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

file_name <- paste0("billing_doc_output_", predefined_text, ".csv")
full_file_path <- file.path(output_folder, file_name)

write.csv(billing_doc_output, file = full_file_path, row.names = FALSE)

#### aggregation block ####

#create international_charge_zone here for the time being at least
billing_doc_output$intl_charge_zone <- billing_doc_output$CHARGE.ZONE

# Produce the output for in the right structure this will be the basis for the aggregation and calculation files
desired_order <- c(
  "customer_code", "NAME_1", "MAILING.STATEMENT.NO.", "ASSIGNMENT.NO." , "SERVICE.DATE" , "DESCRIPTION",
  "BILLING.DATE", "CONSIGNMENT.ID", "article_id", "LODGEMENT.DATE", "ACTUAL.WEIGHT", "ACTUAL.UNIT", "ACTUAL.LENGTH",
  "ACTUAL.WIDTH", "ACTUAL.HEIGHT", "ACTUAL.UNIT.TYPE", 	
   "DECLARED.UNIT.TYPE", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH",	"DECLARED.WIDTH",
  "DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "FROM.NAME", 	"FROM.ADDRESS",	"FROM.CITY",	"FROM.STATE",	"FROM.POSTAL.CODE",
 "TO.NAME",	"TO.ADDRESS",	"TO.CITY",	"TO.STATE",	"TO.POSTAL.CODE", "CUST.REF.1",	"CUST.REF.2",	"BILLED.LENGTH", "BILLED.WIDTH",
 "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "CHARGE.CODE", "RECEIVING.COUNTRY", "intl_charge_zone", "CHARGE.ZONE", "service", "QTY", "AMOUNT.INCL.TAX", 
 "AMOUNT.EXCL.TAX", "base_charge_incgst", "base_charge_exgst", "uplift_figure_exgst", "charge_to_custo_exgst", "fuel_surcharge", "FUEL.SURCHARGE..", 
 "SMC.FEE", "sec_mng_chrg", "over_max_limits_fee", "BILLING.DOC", "is_gst_free_zone"#, "OVER.MAX.LIMITS.FEE"
)
# Reorder the columns in final_output
agg_block <- billing_doc_output [, desired_order]

#### sum the fuel_surcharge ####

# Calculate the sum of fuel_surcharge per BILLING.DOC
fuel_surcharge_per_billing_doc <- agg_block %>%
  filter(is_gst_free_zone == 'No') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_fuel_surcharge = sum(fuel_surcharge, na.rm = TRUE))

# Merge the total_fuel_surcharge back to the original dataframe
agg_block <- left_join(agg_block, fuel_surcharge_per_billing_doc, by = "BILLING.DOC")

# Update base_charge_exgst with the total fuel surcharge where DESCRIPTION is "AP Parcels Domestic Fuel Surcharge"
agg_block <- agg_block %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Parcels Domestic Fuel Surcharge",
                                    total_fuel_surcharge,
                                    base_charge_exgst)) %>%
  select(-total_fuel_surcharge)

#### sum the fuel_surcharg tax free ####

# Calculate the sum of fuel_surcharge per BILLING.DOC where is_gst_free is 'yes'
fuel_surcharge_per_billing_doc <- agg_block %>%
  filter(is_gst_free_zone == 'Yes') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_fuel_surcharge = sum(fuel_surcharge, na.rm = TRUE))

# Merge the total_fuel_surcharge back to the original dataframe
agg_block <- left_join(agg_block, fuel_surcharge_per_billing_doc, by = "BILLING.DOC")

# Update base_charge_exgst with the total fuel surcharge where DESCRIPTION is "AP Parcels Domestic Fuel Surcharge"
agg_block <- agg_block %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Parcels Domestic Fuel Surchg Tax Free",
                                    total_fuel_surcharge,
                                    base_charge_exgst)) %>%
  select(-total_fuel_surcharge)

#### sum the security management fee ####

# Calculate the sum of sec_mng_chrg per BILLING.DOC
sec_mng_chrg_per_billing_doc <- agg_block %>%
  filter(is_gst_free_zone == 'No') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_sec_mng_chrg = sum(sec_mng_chrg, na.rm = TRUE))

# Merge the total_sec_mng_chrg back to the original dataframe
agg_block <- left_join(agg_block, sec_mng_chrg_per_billing_doc, by = "BILLING.DOC")

# Update base_charge_incgst with the total sec_mng_chrg where DESCRIPTION is "AP Security Mgt Charge"
agg_block <- agg_block %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Security Mgt Charge",
                                    total_sec_mng_chrg,
                                    base_charge_exgst)) %>%
  select(-total_sec_mng_chrg)

#### sum the security AP Security Mgt Charge Tax Free ####

# Calculate the sum of sec_mng_chrg per BILLING.DOC
sec_mng_chrg_per_billing_doc <- agg_block %>%
  filter(is_gst_free_zone == 'Yes') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_sec_mng_chrg = sum(sec_mng_chrg, na.rm = TRUE))

# Merge the total_sec_mng_chrg back to the original dataframe
agg_block <- left_join(agg_block, sec_mng_chrg_per_billing_doc, by = "BILLING.DOC")

# Update base_charge_incgst with the total sec_mng_chrg where DESCRIPTION is "AP Security Mgt Charge"
agg_block <- agg_block %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Security Mgt Charge Tax Free",
                                    total_sec_mng_chrg,
                                    base_charge_exgst)) %>%
  select(-total_sec_mng_chrg)

#### bring across the services that we are not touching ####
agg_block$base_charge_incgst <- ifelse(agg_block$DESCRIPTION %in% c("APGL NZ Express w/Signature", 
                                                     "More to Pay", 
                                                     "On Demand Afternoon", 
                                                     "On Demand Return to Sender", 
                                                     "On Demand Tonight", 
                                                     "STC Parcels Domestic Fuel Surcharge", 
                                                     "Duties and Taxes Admin Fee (DDP)",
                                                     "Delivered Duty Paid",
                                                     "AP International Line Haul Surcharge",
                                                     "International  Returns AIR",
                                                     "Lodgement Management Fee",
                                                     "Unmanifest Article"),
                                       agg_block$AMOUNT.INCL.TAX,
                                       agg_block$base_charge_incgst)

agg_block$base_charge_exgst <- ifelse(agg_block$DESCRIPTION %in% c("APGL NZ Express w/Signature", 
                                                     "More to Pay", 
                                                     "On Demand Afternoon", 
                                                     "On Demand Return to Sender", 
                                                     "On Demand Tonight", 
                                                     "STC Parcels Domestic Fuel Surcharge", 
                                                     "Duties and Taxes Admin Fee (DDP)",
                                                     "Delivered Duty Paid",
                                                     "AP International Line Haul Surcharge",
                                                     "International  Returns AIR",
                                                     "Lodgement Management Fee",
                                                     "Unmanifest Article"),
                                      agg_block$AMOUNT.EXCL.TAX,
                                      agg_block$base_charge_exgst)

agg_block$avg_unit_price_ex_gst <- agg_block$QTY * agg_block$base_charge_exgst

# write to the agg block if needed
#agg_block <- agg_block %>%
#  filter(BILLING.DOC == 7823757264)

#file_name <- paste0("ap_post_supply_", predefined_text, ".csv")
#write.csv(agg_block, file = file_name)

#### build ap_post_supply ####

desired_order <- c(
  "customer_code", "NAME_1", "MAILING.STATEMENT.NO.", "ASSIGNMENT.NO.", "SERVICE.DATE", "DESCRIPTION",
  "BILLING.DATE", "CONSIGNMENT.ID", "article_id", "LODGEMENT.DATE", "ACTUAL.WEIGHT", "ACTUAL.UNIT", "ACTUAL.LENGTH",
  "ACTUAL.WIDTH", "ACTUAL.HEIGHT", "ACTUAL.UNIT.TYPE", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH",	"DECLARED.WIDTH",
  "DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "FROM.NAME", 	"FROM.ADDRESS",	"FROM.CITY",	"FROM.STATE",	"FROM.POSTAL.CODE",
  "TO.NAME",	"TO.ADDRESS",	"TO.CITY",	"TO.STATE",	"TO.POSTAL.CODE", "CUST.REF.1",	"CUST.REF.2",	"BILLED.LENGTH", "BILLED.WIDTH",
  "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "CHARGE.CODE", "intl_charge_zone", "RECEIVING.COUNTRY", "CHARGE.ZONE", "service", "QTY","AMOUNT.EXCL.TAX", 
  "avg_unit_price_ex_gst",   "base_charge_exgst")
# Reorder the columns in final_output
ap_post_supply <- agg_block [, desired_order]



# Create a new folder in the specified directory
folder_name <- paste0("ap_post_supply_", predefined_text, ".csv")
# Replace invalid characters in folder name
clean_folder_name <- gsub("[^A-Za-z0-9._-]", "_", folder_name)
new_folder_path <- file.path("C:/Users/shaneb/Desktop/aus_repo_2/-aus_post", clean_folder_name)
dir.create(new_folder_path, showWarnings = FALSE)



# Get unique values of NAME_1
unique_names <- unique(agg_block$NAME_1)

# Loop through each unique NAME_1 value
for (name in unique_names) {
  # Subset data frame for current NAME_1 value
  subset_data <- agg_block[agg_block$NAME_1 == name, ]
  
  # Reorder the columns in final_output
  subset_data <- subset_data[, desired_order]
  
  # Replace invalid characters in name
  clean_name <- gsub("[^A-Za-z0-9._-]", "_", name)
  
  # Generate file name with folder path
  file_name <- file.path(new_folder_path, paste0("ap_post_supply_", predefined_text, "_", clean_name, ".csv"))
  
  # Write subset to CSV
  write.csv(subset_data, file = file_name, row.names = FALSE)
}



##### ap post supply consolodated ###
output_folder <- file.path(getwd(), paste0("output_billing_dates_", predefined_text))
if (!file.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

file_name <- paste0("ap_post_supply_consolidated", predefined_text, ".csv")
full_file_path <- file.path(output_folder, file_name)

write.csv(ap_post_supply, file = full_file_path, row.names = FALSE)







