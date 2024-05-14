#### Generate billing doc output file (line by line comparison) ####
# join the selected columns onto the billing doc in the correct places
# this creates the billing_doc_output which is a just the original output withe the new columns added. Great for looking at individual lines

# rename the article_id
names(bill)[names(bill) == "ARTICLE.ID"] <- "article_id"
names(output_all_services_2)[names(output_all_services_2) == "ARTICLE.ID"] <- "article_id"

# Specify columns to merge from output_all_services_2
merge_cols <- c( "service", "uplift", "DESCRIPTION", "BILLING.DOC", "article_id", 
                "base_charge_incgst", "base_charge_exgst", "base_charge_tax", "charge_value_uplift", "uplift_figure_exgst", 
                "charge_to_custo_exgst", "cubic_weight", "max_weight", "fuel_surcharge","fuel_surcharge_uplifted", "fuel_gst","fuel_surchrg_uplift_gst", "sec_mng_chrg", "sec_mng_chrg_uplifted", "sec_mng_gst", "sec_mng_uplifted_gst",
                "over_max_limits_fee", "weight_category_max", "warnings", "is_gst_free_zone")
#"customer_code2", "customer_code",
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

# Quick descrepancy check for testing purposes
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

# remove duplicates and reorder 
  desired_order <- c("article_id", "BILLING.DOC", "DESCRIPTION", "CUSTOMER", "NAME_1", "NAME_2", "NAME_3", "STREET", "CITY", "REGION", "POST.CODE", 
  "TELEPHONE", "FAX.NUMBER", "MAILING.STATEMENT.NO.", "ASSIGNMENT.NO.", "SERVICE.DATE", "WORK.CENTRE", "WORK.CENTRE.NAME", "CUSTOMER.REF", "CUSTOMER.REFDOC", 
  "ITEM", "MATERIAL", "QTY", "AVG..UNIT.PRICE", "AMOUNT.INCL.TAX", "base_charge_incgst", "base_charge_exgst", "discrepancy", "base_charge_tax", "charge_value_uplift", 
  "uplift_figure_exgst", "charge_to_custo_exgst", "warnings", "TAX.CODE", "TAX.AMOUNT", "AMOUNT.EXCL.TAX", "INVOICE.TOTAL", "TOTAL.QTY", "BILLING.CURRENCY", 
  "EXCHANGE.RATE", "LOCAL.CURRENCY", "FUEL.SURCHARGE..", "FUEL.SURCHARGE.DISC", "FUEL.GST", "fuel_surcharge","fuel_surcharge_uplifted", "fuel_gst","fuel_surchrg_uplift_gst", 
  "sec_mng_chrg", "sec_mng_chrg_uplifted", "sec_mng_gst", "sec_mng_uplifted_gst",
  "over_max_limits_fee", "MHS.FEE", "MHS.DISCOUNT", "MHS.GST", "SMC.FEE", "SMC.DISCOUNT", "SMC.GST", "INTL.SURCHARGE", "INTL.SURCHARGE.MANIFEST", "INVOICE.NO", 
  "BILLING.DATE", "SALES.ORDER", "SALES.ORDER.ITEM", "PAYER", "PAYER.NAME", "CONSIGNMENT.ID", "LODGEMENT.DATE", "ACTUAL.WEIGHT", "ACTUAL.UNIT", "ACTUAL.LENGTH", 
  "ACTUAL.WIDTH", "ACTUAL.HEIGHT", "ACTUAL.UNIT.TYPE", "DECLARED.WEIGHT", "DECLARED.UNIT", "DECLARED.LENGTH", "DECLARED.WIDTH", "DECLARED.HEIGHT", 
  "DECLARED.UNIT.TYPE", "FROM.NAME", "FROM.ADDRESS", "FROM.CITY", "FROM.STATE", "FROM.POSTAL.CODE", "FROM.EMAIL.ADDRESS", "TO.NAME", "TO.ADDRESS", "TO.CITY", 
  "TO.STATE", "TO.POSTAL.CODE", "TO.EMAIL.ADDRESS", "RECORD.COUNT", "TOT.AMOUNT.EXCL.TAX", "CUST.REF.1", "CUST.REF.2", "BILLED.LENGTH", "BILLED.WIDTH", 
  "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "cubic_weight", "max_weight", "weight_category_max", "service", "uplift", "INTERNATIONAL.SURCHARGE.RATE", 
  "CHARGE.CODE", "CHARGE.ZONE", "ATO.DESPATCH.REFERENCE.NUMBER", "RECEIVING.COUNTRY", "SIGNATURE.ON.DELIVERY", "TRANSIT.COVER", "CAPTURE.ID", 
  "UNMANIFESTED.ARTICLE", "RETURN.TO.SENDER", "LODGEMENT.ZONE", "DESTINATION.ZONE", "CUST.REF.3", "WINE...ALCOHOL", "PEAK.FEE", "PEAK.FEE.DISCOUNT", 
  "PEAK.FEE.GST", "OVER.MAX.LIMITS.FEE", "OVER.MAX.LIMITS.FEE.DISCOUNT", "OVER.MAX.LIMITS.FEE.GST", "INTERNATIONAL.UNMANIFESTED.FEE", "customer_code2", 
  "customer_code", "is_gst_free_zone")

# Reorder the columns in final_output
billing_doc_output <- billing_doc_output [, desired_order]




#### sum for the aggregation lines ----

# Calculate the sum of fuel_surcharge_uplifted per BILLING.DOC
fuel_surcharge_per_billing_doc <- billing_doc_output %>%
  filter(is_gst_free_zone == 'No') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_fuel_surcharge = sum(fuel_surcharge_uplifted, na.rm = TRUE))

# Merge the total_fuel_surcharge back to the original dataframe
billing_doc_output <- left_join(billing_doc_output, fuel_surcharge_per_billing_doc, by = "BILLING.DOC")

# Update base_charge_exgst with the total fuel surcharge where DESCRIPTION is "AP Parcels Domestic Fuel Surcharge"
billing_doc_output <- billing_doc_output %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Parcels Domestic Fuel Surcharge",
                                    total_fuel_surcharge,
                                    base_charge_exgst)) %>% select(-total_fuel_surcharge)


#### sum the fuel_surcharg tax free ---
fuel_surcharge_per_billing_doc <- billing_doc_output %>%
  filter(is_gst_free_zone == 'Yes') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_fuel_surcharge = sum(fuel_surcharge_uplifted, na.rm = TRUE))

billing_doc_output <- left_join(billing_doc_output, fuel_surcharge_per_billing_doc, by = "BILLING.DOC")

billing_doc_output <- billing_doc_output %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Parcels Domestic Fuel Surchg Tax Free",
                                    total_fuel_surcharge,
                                    base_charge_exgst)) %>% select(-total_fuel_surcharge)

#### sum the security management fee ----
sec_mng_chrg_per_billing_doc <- billing_doc_output %>%
  filter(is_gst_free_zone == 'No') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_sec_mng_chrg = sum(sec_mng_chrg_uplifted, na.rm = TRUE))

billing_doc_output <- left_join(billing_doc_output, sec_mng_chrg_per_billing_doc, by = "BILLING.DOC")

billing_doc_output <- billing_doc_output %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Security Mgt Charge",
                                    total_sec_mng_chrg,
                                    base_charge_exgst)) %>% select(-total_sec_mng_chrg)

#### sum the security AP Security Mgt Charge Tax Free ----

sec_mng_chrg_per_billing_doc <- billing_doc_output %>%
  filter(is_gst_free_zone == 'Yes') %>%
  group_by(BILLING.DOC) %>%
  summarise(total_sec_mng_chrg = sum(sec_mng_chrg_uplifted, na.rm = TRUE))

billing_doc_output <- left_join(billing_doc_output, sec_mng_chrg_per_billing_doc, by = "BILLING.DOC")

billing_doc_output <- billing_doc_output %>%
  mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Security Mgt Charge Tax Free",
                                    total_sec_mng_chrg,
                                    base_charge_exgst)) %>% select(-total_sec_mng_chrg)

#### bring across the services that we are not touching ----
billing_doc_output$base_charge_incgst <- ifelse(billing_doc_output$DESCRIPTION %in% c(
  "More to Pay",
  "On Demand Return to Sender", 
  "STC Parcels Domestic Fuel Surcharge", 
  "Duties and Taxes Admin Fee (DDP)",
  "Delivered Duty Paid",
  "AP International Line Haul Surcharge",
  "International  Returns AIR",
  "Lodgement Management Fee",
  "Unmanifest Article"),
  billing_doc_output$AMOUNT.INCL.TAX,
  billing_doc_output$base_charge_incgst)

billing_doc_output$base_charge_exgst <- ifelse(billing_doc_output$DESCRIPTION %in% c(
  "More to Pay",
  "On Demand Return to Sender", 
  "STC Parcels Domestic Fuel Surcharge", 
  "Duties and Taxes Admin Fee (DDP)",
  "Delivered Duty Paid",
  "AP International Line Haul Surcharge",
  "International  Returns AIR",
  "Lodgement Management Fee",
  "Unmanifest Article"),
  billing_doc_output$AMOUNT.EXCL.TAX,
  billing_doc_output$base_charge_exgst)



# quick fix for charge to customer. Will have to revise this down the line
#billing_doc_output$avg_unit_price_charge_to_custo_ex_gst <- billing_doc_output$QTY * billing_doc_output$charge_to_custo_exgst
max_charge <- pmax(billing_doc_output$charge_to_custo_exgst, billing_doc_output$base_charge_exgst, na.rm = TRUE)
max_charge[is.na(max_charge)] <- 0

billing_doc_output$charge_to_custo_exgst <- max_charge
billing_doc_output$avg_unit_price_charge_to_custo_ex_gst <- billing_doc_output$QTY * billing_doc_output$charge_to_custo_exgst


# generate the file path
output_folder <- file.path(getwd(), paste0("output_billing_dates_", predefined_text))
if (!file.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

file_name <- paste0("billing_doc_output_", predefined_text, ".csv")
full_file_path <- file.path(output_folder, file_name)

write.csv(billing_doc_output, file = full_file_path, row.names = FALSE)

#### aggregation block ----
# this will be used to generate the sums and the ap_post_supply will be taken from here
#create international_charge_zone
billing_doc_output$intl_charge_zone <- billing_doc_output$CHARGE.ZONE

#Restructure define the services for the outputs
billing_doc_output$Service[billing_doc_output$DESCRIPTION == "AP International Line Haul Surcharge"] <- "eParcel International Line Haul Surcharge"
billing_doc_output$Service[billing_doc_output$DESCRIPTION == "AP Parcels Domestic Fuel Surcharge" |
                             billing_doc_output$DESCRIPTION == "AP Parcels Domestic Fuel Surchg Tax Free"] <- "eParcel Regular"
billing_doc_output$Service[billing_doc_output$DESCRIPTION == "AP Security Mgt Charge" |
                             billing_doc_output$DESCRIPTION == "AP Security Mgt Charge Tax Free"] <- "eParcel Express"
billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Express Post Parcels (BYO up to 5kg)"] <- "eParcel Express"
billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Local Pickup and Delivery Services" |
                             billing_doc_output$DESCRIPTION == "Lodgement Management Fee" |
                             billing_doc_output$DESCRIPTION == "More to Pay"] <- "eParcel Regular Returns"
billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Over Maximum Limits Fee"] <- "Additional Charges"
billing_doc_output$Service[billing_doc_output$DESCRIPTION == "APGL NZ Express w/Signature"] <- "APGL NZ Express"
billing_doc_output$Service[grep("demand", billing_doc_output$DESCRIPTION, ignore.case = TRUE)] <- "StarTrack OnDemand"

billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Duties and Taxes Admin Fee (DDP)" |
                             billing_doc_output$DESCRIPTION == "Delivered Duty Paid"] <- billing_doc_output$DESCRIPTION[billing_doc_output$DESCRIPTION %in% c("Duties and Taxes Admin Fee (DDP)", "Delivered Duty Paid")]

billing_doc_output$Service[billing_doc_output$DESCRIPTION == "EPARCEL WINE STD"] <- "eParcel Wine"
billing_doc_output$Service[billing_doc_output$DESCRIPTION == "eParcel Call For Return" |
                             billing_doc_output$DESCRIPTION == "eParcel Return To Sender" |
                             billing_doc_output$DESCRIPTION == "eParcel Post Return"] <- "eParcel Regular Returns"

billing_doc_output$Service[billing_doc_output$DESCRIPTION == "STC Parcels Domestic Fuel Surcharge"] <- "OnDemand"

billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Parcel Post with Signature"] <- "eParcel Regular"
billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Express Post with Signature"] <- "eParcel Express"

billing_doc_output$Service[billing_doc_output$DESCRIPTION %in% c("PACK AND TRACK INTERNATIONAL", "Express Courier International (eParcel)", "International Returns AIR")] <- "eParcel International"



# Produce the output for in the right structure this will be the basis for the aggregation and calculation files
desired_order <- c(
  "customer_code", "NAME_1", "MAILING.STATEMENT.NO.", "ASSIGNMENT.NO." , "SERVICE.DATE" , "DESCRIPTION",
  "BILLING.DATE", "CONSIGNMENT.ID", "article_id", "LODGEMENT.DATE", "ACTUAL.WEIGHT", "ACTUAL.UNIT", "ACTUAL.LENGTH",
  "ACTUAL.WIDTH", "ACTUAL.HEIGHT", "ACTUAL.UNIT.TYPE", 	
  "DECLARED.UNIT.TYPE", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH",	"DECLARED.WIDTH",
  "DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "FROM.NAME", 	"FROM.ADDRESS",	"FROM.CITY",	"FROM.STATE",	"FROM.POSTAL.CODE",
 "TO.NAME",	"TO.ADDRESS",	"TO.CITY",	"TO.STATE",	"TO.POSTAL.CODE", "CUST.REF.1",	"CUST.REF.2",	"BILLED.LENGTH", "BILLED.WIDTH",
 "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "CHARGE.CODE", "RECEIVING.COUNTRY", "intl_charge_zone", "CHARGE.ZONE", "Service", "QTY", "AMOUNT.INCL.TAX", 
 "AMOUNT.EXCL.TAX", "base_charge_incgst", "base_charge_exgst", "uplift_figure_exgst", "charge_to_custo_exgst", "fuel_surcharge", "FUEL.SURCHARGE..", 
 "SMC.FEE", "sec_mng_chrg", "over_max_limits_fee", "BILLING.DOC", "is_gst_free_zone"#, "OVER.MAX.LIMITS.FEE"
)
# Reorder the columns in final_output
#billing_doc_output <- billing_doc_output [, desired_order]


#### build ap_post_supply ----

desired_order <- c(
  "customer_code", "NAME_1", "MAILING.STATEMENT.NO.", "ASSIGNMENT.NO.", "SERVICE.DATE", "DESCRIPTION",
  "BILLING.DATE", "CONSIGNMENT.ID", "article_id", "LODGEMENT.DATE", "ACTUAL.WEIGHT", "ACTUAL.UNIT", "ACTUAL.LENGTH",
  "ACTUAL.WIDTH", "ACTUAL.HEIGHT", "ACTUAL.UNIT.TYPE", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH",	"DECLARED.WIDTH",
  "DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "FROM.NAME", 	"FROM.ADDRESS",	"FROM.CITY",	"FROM.STATE",	"FROM.POSTAL.CODE",
  "TO.NAME",	"TO.ADDRESS",	"TO.CITY",	"TO.STATE",	"TO.POSTAL.CODE", "CUST.REF.1",	"CUST.REF.2",	"BILLED.LENGTH", "BILLED.WIDTH",
  "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "CHARGE.CODE", "intl_charge_zone", "RECEIVING.COUNTRY", "CHARGE.ZONE", "Service", "QTY",
  "avg_unit_price_charge_to_custo_ex_gst", "charge_to_custo_exgst")

# Reorder the columns in final_output.
# this is needed for the consoladated 
ap_post_supply <- billing_doc_output [, desired_order]


# Create a new folder in the specified directory
folder_name <- paste0("ap_post_supply_", predefined_text, ".csv")
# Replace invalid characters in folder name
clean_folder_name <- gsub("[^A-Za-z0-9._-]", "_", folder_name)
new_folder_path <- file.path(output_folder, clean_folder_name)
dir.create(new_folder_path, showWarnings = FALSE)



# Get unique values of NAME_1
unique_names <- unique(billing_doc_output$NAME_1)

# Loop through each unique NAME_1 value
for (name in unique_names) {
  # Subset data frame for current NAME_1 value
  subset_data <- billing_doc_output[billing_doc_output$NAME_1 == name, ]
  
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

file_name <- paste0("ap_post_supply_consolidated_", predefined_text, ".csv")
full_file_path <- file.path(output_folder, file_name)

write.csv(ap_post_supply, file = full_file_path, row.names = FALSE)


