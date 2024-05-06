#### join the columns onto the billing doc in the correct places ####
# this creates the billing_doc_output which is a just the original output will the new columns added
# rename the article_id
names(bill)[names(bill) == "ARTICLE.ID"] <- "article_id"
names(output_all_services_2)[names(output_all_services_2) == "ARTICLE.ID"] <- "article_id"

# Specify columns to merge from output_all_services_2
merge_cols <- c("customer_code2", "customer_code", "service", "uplift", "DESCRIPTION", "BILLING.DOC", "article_id", 
                "base_charge_incgst", "base_charge_exgst", "base_charge_tax", "charge_value_uplift", "uplift_figure_exgst", 
                "charge_to_custo_exgst", "cubic_weight", "max_weight", "fuel_surcharge", "fuel_gst", "sec_mng_chrg", "sec_mng_gst", 
                "over_max_limits_fee", "weight_category_max", "warnings", "is_gst_free_zone")

# Select only the merge_cols from output_all_services_2
selected_output_all_services_2 <- output_all_services_2[, merge_cols]

# Merge bill and selected columns from output_all_services_2 by the unique identifier 
billing_doc_output <- merge(bill, selected_output_all_services_2, by = c("article_id", "BILLING.DOC", "DESCRIPTION"), all = TRUE)

# Find the position of "AVG..UNIT.PRICE" column
avg_unit_price_index <- which(names(billing_doc_output ) == "AMOUNT.INCL.TAX")

# Insert new columns after "AVG..UNIT.PRICE"
billing_doc_output  <- cbind(billing_doc_output [, 1:avg_unit_price_index],
                      billing_doc_output [, c("base_charge_incgst","base_charge_exgst", "base_charge_tax", "charge_value_uplift", "uplift_figure_exgst", "charge_to_custo_exgst", "warnings")],
                      billing_doc_output [, (avg_unit_price_index + 1):ncol(billing_doc_output )])


# Find the position of "FUEL.GST" column
fuel_gst_index <- which(names(billing_doc_output ) == "FUEL.GST")

# Insert new columns after "FUEL.GST"
billing_doc_output  <- cbind(billing_doc_output [, 1:fuel_gst_index  ],
                      billing_doc_output [, c("fuel_surcharge" ,"fuel_gst", "sec_mng_chrg", "sec_mng_gst", "over_max_limits_fee")],
                      billing_doc_output [, (fuel_gst_index  + 1):ncol(billing_doc_output )])


# Find the position of "BILLED.WEIGHT" column
billed_weight_index <- which(names(billing_doc_output ) == "BILLED.WEIGHT")

# Insert new columns after "BILLED.WEIGHT"
billing_doc_output  <- cbind(billing_doc_output [, 1:billed_weight_index  ], 
                      billing_doc_output [, c("cubic_weight", "max_weight", "weight_category_max", "service", "uplift")],
                      billing_doc_output [, (billed_weight_index  + 1):ncol(billing_doc_output )])


#### Quick descrepancy check ####

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


file_name <- paste0("billing_doc_output_", predefined_text, ".csv")

write.csv(billing_doc_output , file = file_name)


#### build ap_post_supply (final_output_v2) ####
#create international_charge_zone here for the time being at least
billing_doc_output$intl_charge_zone <- billing_doc_output$CHARGE.ZONE


# Produce the output for in the right structure for the output file. This will also be the basis for the aggregation and calculation files so the ends will have to come off
#
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
ap_post_supply <- billing_doc_output [, desired_order]

#### sum the fuel_surcharge ####

# Calculate the sum of fuel_surcharge per BILLING.DOC
fuel_surcharge_per_billing_doc <- ap_post_supply %>%
  filter(is_gst_free_zone == 'No') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_fuel_surcharge = sum(fuel_surcharge, na.rm = TRUE))

# Merge the total_fuel_surcharge back to the original dataframe
ap_post_supply <- left_join(ap_post_supply, fuel_surcharge_per_billing_doc, by = "BILLING.DOC")

# Update base_charge_exgst with the total fuel surcharge where DESCRIPTION is "AP Parcels Domestic Fuel Surcharge"
ap_post_supply <- ap_post_supply %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Parcels Domestic Fuel Surcharge",
                                    total_fuel_surcharge,
                                    base_charge_exgst)) %>%
  select(-total_fuel_surcharge)


#### sum the fuel_surcharg tax free ####

# Calculate the sum of fuel_surcharge per BILLING.DOC where is_gst_free is 'yes'
fuel_surcharge_per_billing_doc <- ap_post_supply %>%
  filter(is_gst_free_zone == 'Yes') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_fuel_surcharge = sum(fuel_surcharge, na.rm = TRUE))

# Merge the total_fuel_surcharge back to the original dataframe
ap_post_supply <- left_join(ap_post_supply, fuel_surcharge_per_billing_doc, by = "BILLING.DOC")

# Update base_charge_exgst with the total fuel surcharge where DESCRIPTION is "AP Parcels Domestic Fuel Surcharge"
ap_post_supply <- ap_post_supply %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Parcels Domestic Fuel Surchg Tax Free",
                                    total_fuel_surcharge,
                                    base_charge_exgst)) %>%
  select(-total_fuel_surcharge)

#### sum the security manaegement fee ####

# Calculate the sum of sec_mng_chrg per BILLING.DOC
sec_mng_chrg_per_billing_doc <- ap_post_supply %>%
  filter(is_gst_free_zone == 'No') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_sec_mng_chrg = sum(sec_mng_chrg, na.rm = TRUE))

# Merge the total_sec_mng_chrg back to the original dataframe
ap_post_supply <- left_join(ap_post_supply, sec_mng_chrg_per_billing_doc, by = "BILLING.DOC")

# Update base_charge_incgst with the total sec_mng_chrg where DESCRIPTION is "AP Security Mgt Charge"
ap_post_supply <- ap_post_supply %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Security Mgt Charge",
                                    total_sec_mng_chrg,
                                    base_charge_exgst)) %>%
  select(-total_sec_mng_chrg)

#### sum the security AP Security Mgt Charge Tax Free ####

# Calculate the sum of sec_mng_chrg per BILLING.DOC
sec_mng_chrg_per_billing_doc <- ap_post_supply %>%
  filter(is_gst_free_zone == 'Yes') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_sec_mng_chrg = sum(sec_mng_chrg, na.rm = TRUE))

# Merge the total_sec_mng_chrg back to the original dataframe
ap_post_supply <- left_join(ap_post_supply, sec_mng_chrg_per_billing_doc, by = "BILLING.DOC")

# Update base_charge_incgst with the total sec_mng_chrg where DESCRIPTION is "AP Security Mgt Charge"
ap_post_supply <- ap_post_supply %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Security Mgt Charge Tax Free",
                                    total_sec_mng_chrg,
                                    base_charge_exgst)) %>%
  select(-total_sec_mng_chrg)



#### bring across the services that we are not touching ####
ap_post_supply$base_charge_incgst <- ifelse(ap_post_supply$DESCRIPTION %in% c("APGL NZ Express w/Signature", 
                                                     "More to Pay", 
                                                     "On Demand Afternoon", 
                                                     "On Demand Return to Sender", 
                                                     "On Demand Tonight", 
                                                     "STC Parcels Domestic Fuel Surcharge", 
                                                     "Duties and Taxes Admin Fee (DDP)",
                                                     "Delivered Duty Paid",
                                                     "Unmanifest Article"),
                                            ap_post_supply$AMOUNT.INCL.TAX,
                                            ap_post_supply$base_charge_incgst)

ap_post_supply$base_charge_exgst <- ifelse(ap_post_supply$DESCRIPTION %in% c("APGL NZ Express w/Signature", 
                                                     "More to Pay", 
                                                     "On Demand Afternoon", 
                                                     "On Demand Return to Sender", 
                                                     "On Demand Tonight", 
                                                     "STC Parcels Domestic Fuel Surcharge", 
                                                     "Duties and Taxes Admin Fee (DDP)",
                                                     "Delivered Duty Paid",
                                                     "Unmanifest Article"),
                                           ap_post_supply$AMOUNT.EXCL.TAX,
                                           ap_post_supply$base_charge_exgst)

#########

#ap_post_supply <- ap_post_supply %>%
#  filter(BILLING.DOC == 7823757264)

file_name <- paste0("ap_post_supply_", predefined_text, ".csv")

write.csv(ap_post_supply, file = file_name)



###########


# Get the current date
#current_date <- Sys.Date()

# Format the date to your desired format
#formatted_date <- format(current_date, "%Y-%m-%d")

# Generate the file name without the date
#output_file <- "output.csv"

# Append the date at the end of the file name
#final_output_v1_with_date <- paste0(gsub(".csv", "", output_file), "_", formatted_date, ".csv")

# Write your data to the CSV file with the generated file name
#write.csv(final_output_v1, file = final_output_v1_with_date)





