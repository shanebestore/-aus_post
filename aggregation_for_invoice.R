head(final_output)

final_output_b <- final_output

#final_output_b  <- subset(final_output_b , NAME_1 %in% c('AGN - Ambition Group NZ - MW1'))

# declare a data.frame
invoice_test <- data.frame(
  Name = c("Bill total from Aus Post"),
  value = c(1)
)


#### original from Aus Post using the easy calcs
#total_supply_for_the_period_inc_fee_and_charges_easy
invoice_test$total_supply_for_the_period_inc_fee_and_charges_easy <- sum(final_output_b$AMOUNT.EXCL.TAX, na.rm = TRUE)

#total_supply_for_the_period_easy
final_output_minus_fees_and_charge_easy <- final_output_b[!grepl("charge|surcharge|admin|fuel|more|unmanifest", final_output_b$DESCRIPTION, ignore.case = TRUE), ]
invoice_test$total_supply_for_the_period_easy <-sum(final_output_minus_fees_and_charge_easy$AMOUNT.EXCL.TAX, na.rm = TRUE)

#fees_and_charges_for_the_period_easy 
final_output_just_fees_and_charges_easy <- final_output_b[grepl("charge|surcharge|admin|fuel|more|unmanifest", final_output_b$DESCRIPTION, ignore.case = TRUE), ]
invoice_test$fees_and_charges_for_the_period_easy <-sum(final_output_just_fees_and_charges_easy$AMOUNT.EXCL.TAX, na.rm = TRUE)

invoice_test$gst <-sum(final_output_b$TAX.AMOUNT, na.rm = TRUE)

#### original from Aus Post using the hard calcs
#fees_and_charges_for_the_period_hard

total_FUEL.SURCHARGE.. <- sum(final_output_b$FUEL.SURCHARGE.., na.rm = TRUE)
total_SMC.FEE  <- sum(final_output_b$SMC.FEE, na.rm = TRUE)
total_SMC.GST <- sum(final_output_b$SMC.GST, na.rm = TRUE)
total_FUEL.GST <- sum(final_output_b$FUEL.GST, na.rm = TRUE)
invoice_test$fees_and_charges_for_the_period_hard <- sum(total_FUEL.SURCHARGE.., total_SMC.FEE)

#total_supply_for_the_period_har
invoice_test$total_supply_for_the_period_hard <- sum(invoice_test$total_supply_for_the_period_inc_fee_and_charges_easy 
                                                     - invoice_test$fees_and_charges_for_the_period_hard)

invoice_test$gst_hard <- sum(final_output_minus_fees_and_charge_easy$SMC.GST + final_output_minus_fees_and_charge_easy$FUEL.GST 
                             + final_output_minus_fees_and_charge_easy$TAX.AMOUNT)




#invoice_test$gst_hard <-sum(total_SMC.GST, total_FUEL.GST, total_TAX.AMOUNT)
write.csv(invoice_test, file = "invoice_test.csv")


##### basic charges only
# original from Aus Post using the easy calcs

# declare a data.frame
invoice_test_basic_charge <- data.frame(
  Name = c("Standard and express"),
  value = c(1)
)

final_output2 <- subset(final_output_b, uplift_service %in% c( "Regular.VIC", "Express.VIC", "Regular.NSW", "Express.NSW", "EPP_fivekg", "ep_return_to_sender", "exp_eparcel_returns",
                                                            "reg_eparcel_returns", "reg_ep_call_for_return"))
#total_supply_for_the_period_inc_fee_and_charges_easy
invoice_test_basic_charge$total_supply_for_the_period_ex_fee_and_charges_easy <-sum(final_output2$AMOUNT.EXCL.TAX, na.rm = TRUE)

#total_supply_for_the_period_easy
final_output_minus_fees_and_charge_easy <- final_output2[!grepl("charge|surcharge|admin|fuel|more|unmanifest", final_output2$DESCRIPTION, ignore.case = TRUE), ]
invoice_test_basic_charge$total_supply_for_the_period_easy <-sum(final_output_minus_fees_and_charge_easy$AMOUNT.EXCL.TAX, na.rm = TRUE)

#fees_and_charges_for_the_period_easy 
final_output_just_fees_and_charges_easy <- final_output2[grepl("charge|surcharge|admin|fuel|more|unmanifest", final_output2$DESCRIPTION, ignore.case = TRUE), ]
invoice_test_basic_charge$fees_and_charges_for_the_period_easy <-sum(final_output_just_fees_and_charges_easy$AMOUNT.EXCL.TAX, na.rm = TRUE)

invoice_test_basic_charge$gst <-sum(final_output2$TAX.AMOUNT, na.rm = TRUE)

#### original from Aus Post using the hard calcs
#fees_and_charges_for_the_period_hard

total_FUEL.SURCHARGE..2 <- sum(final_output2$FUEL.SURCHARGE.., na.rm = TRUE)
total_SMC.FEE2  <- sum(final_output2$SMC.FEE, na.rm = TRUE)
total_SMC.GST2 <- sum(final_output2$SMC.GST, na.rm = TRUE)
total_FUEL.GST2 <- sum(final_output2$FUEL.GST, na.rm = TRUE)
invoice_test_basic_charge$fees_and_charges_for_the_period_hard <- sum(total_FUEL.SURCHARGE..2, total_SMC.FEE2)

#total_supply_for_the_period_hard
#invoice_test_basic_charge$total_supply_for_the_period_hard <- sum(invoice_test_basic_charge$total_supply_for_the_period_ex_fee_and_charges_easy)

#invoice_test_basic_charge$gst_hard <-sum(total_SMC.GST, total_FUEL.GST, total_TAX.AMOUNT)

invoice_test$gst_hard <- sum(final_output_minus_fees_and_charge_easy$SMC.GST + final_output_minus_fees_and_charge_easy$FUEL.GST 
                             + final_output_minus_fees_and_charge_easy$TAX.AMOUNT)

write.csv(invoice_test_basic_charge, file = "invoice_test_basic_charge.csv")



##### post feb rates
invoice_test_post_feb <- data.frame(
  Name = c("Post feb rates base"),
  value = c(1)
)


invoice_test_post_feb$new_base_charge_exgst <- sum(final_output_b$base_charge_exgst, na.rm = TRUE)
invoice_test_post_feb$new_charge_to_custo_exgst <- sum(final_output_b$charge_to_custo_exgst, na.rm = TRUE)
invoice_test_post_feb$fuel_surcharge <- sum(final_output_b$fuel_surcharge, na.rm = TRUE)


# fees and surcharges
invoice_test_post_feb$new_fees_and_surcharges <- sum(final_output$fuel_surcharge,na.rm = TRUE)+ sum(final_output$sec_mng_chrg,na.rm = TRUE)+ sum(final_output$over_max_limits_fee, na.rm = TRUE)
write.csv(invoice_test_post_feb, file = "invoice_test_post_feb.csv")

#average uplift
final_output$charge_value_uplift <- as.numeric(gsub("%", "", final_output$charge_value_uplift))
invoice_test_post_feb$average_uplift <- mean(final_output$charge_value_uplift, na.rm = TRUE)


final_output$uplift_figure_exgst <- as.numeric(gsub("%", "", final_output$uplift_figure_exgst))
invoice_test_post_feb$average_uplift_figure_exgst <- mean(final_output$uplift_figure_exgst, na.rm = TRUE)


########################################################################

#### Checking the tool against the charges on the invoice ####
# Get final output minus fees and charges method1

#invoice_test$total_fees_and_charges <-sum(final_output$AMOUNT.EXCL.TAX) - sum(final_output_minus_fees_and_charges$AMOUNT.EXCL.TAX)





#### GST rough work ###
#final_output$SMC.GST, final_output$FUEL.GST, final_output$MHS.GST
#sum(final_output$SMC.GST)
#sum(final_output$SMC.FEE)


#### getting the figures from the NEW method ####


# gst: fuel_gst,  sec_mng_gst,


sum(final_output$fuel_surcharge, na.rm = TRUE)
sum(final_output$sec_mng_chrg, na.rm = TRUE)
sum(final_output$over_max_limits_fee , na.rm = TRUE)

#### grouping rough work. TBC

#base charge by nam_1 custo
# Convert base_charge_exgst to numeric, replacing NA with 0
#final_output$base_charge_exgst <- as.numeric(replace_na(final_output$base_charge_exgst, 0))
#base_charge_totals <- aggregate(base_charge_exgst ~ NAME_1, data = final_output, FUN = sum)
#print(base_charge_totals)

#final_output$AVG..UNIT.PRICE <- as.numeric(replace_na(final_output$AVG..UNIT.PRICE, 0))
#AVG..UNIT.PRICE_totals <- aggregate(AVG..UNIT.PRICE ~ NAME_1, data = final_output, FUN = sum)

#diff <- ifelse(final_output$AMOUNT.INCL.TAX != final_output$base_charge_incgst, "yes", "no")
#count_diff <- table(diff)
#print(count_diff)
