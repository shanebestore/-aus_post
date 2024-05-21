install.packages("sqldf")
library(sqldf)

ap_post_supply_1702 <- ap_post_supply
old_uplifted = read.csv("AP supply - old uplifted invoices/ALL - Australia Post Supply this Period Ending 16 03 2024.csv", head=TRUE, sep=",")


new_col_names <- c("Code", "NAME_1", "MAILING STATEMENT NO.", "ASSIGNMENT NO.", "SERVICE DATE", "DESCRIPTION", "BILLING DATE", "CONSIGNMENT ID", 
                   "ARTICLE ID", "LODGEMENT DATE", "ACTUAL WEIGHT", "ACTUAL UNIT", "ACTUAL LENGTH", "ACTUAL WIDTH", "ACTUAL HEIGHT", "ACTUAL UNIT TYPE", 
                   "DECLARED WEIGHT", "DECLARED UNIT", "DECLARED LENGTH", "DECLARED WIDTH", "DECLARED HEIGHT", "DECLARED UNIT TYPE", "FROM NAME", 
                   "FROM ADDRESS", "FROM CITY", "FROM STATE", "FROM POSTAL CODE", "TO NAME", "TO ADDRESS", "TO CITY", "TO STATE", "TO POSTAL CODE", 
                   "CUST REF 1", "CUST REF 2", "BILLED LENGTH", "BILLED WIDTH", "BILLED HEIGHT", "CUBIC WEIGHT", "BILLED WEIGHT", "CHARGE CODE", 
                   "INTL CHARGE ZONE", "RECEIVING COUNTRY", "Charge Zone", "Service", "QTY", "AVG. UNIT PRICE EX GST", "AMOUNT EX GST")
names(old_uplifted) <- new_col_names

# check col titles and data match

# Specify columns to merge from output_all_services_2
merge_cols <- c( "ARTICLE ID", "Code", "ASSIGNMENT NO.", "DESCRIPTION", "AMOUNT EX GST","CONSIGNMENT ID", 'BILLING DATE', "CUST REF 1")

#merge the cols
old_uplifted_subset<- old_uplifted[, merge_cols]
old_uplifted_subset$"BILLING DATE" <- as.Date(as.character(old_uplifted_subset$"BILLING DATE"), format = "%Y.%m.%d")
old_uplifted_subset$"BILLING DATE" <-as.character(old_uplifted_subset$"BILLING DATE")

ap_post_supply_1702$"BILLING DATE" <-as.character(ap_post_supply_1702$"BILLING DATE")


# Sum AMOUNT EX GST and group by ASSIGNMENT NO. and NAME_1 for ap_post_supply_1702
new_summarized <- ap_post_supply_1702 %>%
  group_by(`Code`,`ASSIGNMENT NO.`, `BILLING DATE`) %>%
  summarise(TOTAL_AMOUNT_EX_GST = sum(`AMOUNT EX GST`, na.rm = TRUE))

# Sum AMOUNT EX GST and group by ASSIGNMENT NO. and NAME_1 for old_uplifted_subset
old_summarized <- old_uplifted_subset %>%
  group_by(`Code`,`ASSIGNMENT NO.`,`BILLING DATE`) %>%
  summarise(TOTAL_AMOUNT_EX_GST = sum(`AMOUNT EX GST`, na.rm = TRUE))

# Optionally, you can merge the two datasets if you want to compare the results
merged_summarized <- merge(new_summarized, old_summarized, by = c('ASSIGNMENT NO.', 'Code', 'BILLING DATE'), all = TRUE, suffixes = c("_NEW", "_OLD"))

# Display the summarized datasets
sum(new_summarized$'TOTAL_AMOUNT_EX_GST')
sum(old_summarized$'TOTAL_AMOUNT_EX_GST')
print(merged_summarized)

sum(merged_summarized$TOTAL_AMOUNT_EX_GST_NEW) 
sum(merged_summarized$TOTAL_AMOUNT_EX_GST_OLD) 

# Write merged dataset to a CSV file
write.csv(merged_summarized, file = "old_v_new_summary_by_assignment_pe1603.csv")

















#################### below didn't work

# this works but there are too many joins
#result <- ap_post_supply_1702 %>%
#  left_join(old_uplifted_subset, by = c( "ARTICLE ID", "DESCRIPTION", "BILLING DATE",  "ASSIGNMENT NO."))
#, "ASSIGNMENT NO." "CUST REF 1"

result <- merge(ap_post_supply_1702, old_uplifted_subset, by = c( "ARTICLE ID", "ASSIGNMENT NO.", "CONSIGNMENT ID" ), all =FALSE)
#, "ASSIGNMENT NO." "CUST REF 1" , "DESCRIPTION"

write.csv(result, file = "ap post supply back billing comparison PE 16 02.csv")






variance <- within(result, {
  variance$SUBTRACTED_AMOUNT <- result$`AMOUNT EX GST.y` - result$`AMOUNT EX GST.x`
})


