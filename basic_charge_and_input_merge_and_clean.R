#### join the columns onto the billing doc in the correct places ####
# rename the article_id
names(bill)[names(bill) == "ARTICLE.ID"] <- "article_id"
names(output_all_services_2)[names(output_all_services_2) == "ARTICLE.ID"] <- "article_id"

# Specify columns to merge from output_all_services_2
merge_cols <- c("customer_code2", "customer_code", "service", "uplift", "DESCRIPTION", "BILLING.DOC", "article_id", 
                "base_charge_incgst", "base_charge_exgst", "base_charge_tax", "charge_value_uplift", "uplift_figure_exgst", 
                "charge_to_custo_exgst", "cubic_weight", "max_weight", "fuel_surcharge", "fuel_gst", "sec_mng_chrg", "sec_mng_gst", 
                "over_max_limits_fee", "weight_category_max", "warnings")

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


write.csv(billing_doc_output , file = "billing_doc_output .csv")

#create international_charge_zone here for the time being at least
billing_doc_output$intl_charge_zone <- billing_doc_output$CHARGE.ZONE



##### final_output_v2 ####

# Produce the output for in the right structure for the output file. This will also be the basis for the aggregation and calculation files so the ends will have to come off
#
desired_order <- c(
  "customer_code", "NAME_1", "MAILING.STATEMENT.NO.", "ASSIGNMENT.NO." , "SERVICE.DATE" , "DESCRIPTION",
  "BILLING.DATE", "CONSIGNMENT.ID", "article_id", "LODGEMENT.DATE", "ACTUAL.WEIGHT", "ACTUAL.UNIT", "ACTUAL.LENGTH",
  "ACTUAL.WIDTH", "ACTUAL.HEIGHT", "ACTUAL.UNIT.TYPE", "DECLARED.WEIGHT", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH", 
  "DECLARED.WIDTH"	, "DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH",	"DECLARED.WIDTH",
  "DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "FROM.NAME", 	"FROM.ADDRESS",	"FROM.CITY",	"FROM.STATE",	"FROM.POSTAL.CODE",
 "TO.NAME",	"TO.ADDRESS",	"TO.CITY",	"TO.STATE",	"TO.POSTAL.CODE", "CUST.REF.1",	"CUST.REF.2",	"BILLED.LENGTH", "BILLED.WIDTH",
 "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "CHARGE.CODE", "RECEIVING.COUNTRY", "intl_charge_zone", "CHARGE.ZONE", "service", "QTY", "AMOUNT.INCL.TAX", 
 "AMOUNT.EXCL.TAX", "base_charge_incgst", "base_charge_exgst", "uplift_figure_exgst", "charge_to_custo_exgst", "fuel_surcharge", "FUEL.SURCHARGE..", 
 "SMC.FEE", "sec_mng_chrg", "over_max_limits_fee"
)
# Reorder the columns in final_output
ap_post_supply <- billing_doc_output [, desired_order]

write.csv(ap_post_supply , file = "ap_post_supply.csv")






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





