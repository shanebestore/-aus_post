head(final_output)

final_output_b <- final_output
#final_output_b  <- subset(final_output_b , NAME_1 %in% c('AGN - Ambition Group NZ - MW1'))
final_output_b  = read.csv("final_output.csv", head=TRUE, sep=",")
head(final_output_b)

# declare a data.frame
invoice_test <- data.frame(
  Name = c("Bill total from Aus Post")
)

##### Whole invoice test ####
final_output_minus_fees_and_charge_easy <- final_output_b[!grepl("charge|surcharge|admin|fuel|more|unmanifest", final_output_b$DESCRIPTION, ignore.case = TRUE), ]
invoice_test$total_supply_for_the_period <-sum(final_output_minus_fees_and_charge_easy$AMOUNT.EXCL.TAX, na.rm = TRUE)

invoice_test$fees_and_charges_for_the_period_1 <- sum(final_output_b$FUEL.SURCHARGE.., na.rm = TRUE) + sum(final_output_b$SMC.FEE, na.rm = TRUE) +sum(final_output_b$OVER.MAX.LIMITS.FEE, na.rm = TRUE) 

final_output_just_fees_and_charges <- final_output_b[grepl("charge|surcharge|admin|fuel|more|unmanifest", final_output_b$DESCRIPTION, ignore.case = TRUE), ]
invoice_test$fees_and_charges_for_the_period_2 <-sum(final_output_just_fees_and_charges$AMOUNT.EXCL.TAX, na.rm = TRUE)

invoice_test$gst_1 <- sum(final_output_minus_fees_and_charge_easy$SMC.GST + final_output_minus_fees_and_charge_easy$FUEL.GST 
                             + final_output_minus_fees_and_charge_easy$TAX.AMOUNT,  na.rm = TRUE)


#### Just basic charges ####
invoice_test_just_basic <- data.frame(
  Name = c("invoice_test_just_basic")
)

final_output2 <- subset(final_output_b, uplift_service %in% c( "Regular.VIC", "Express.VIC", "Regular.NSW", "Express.NSW", "EPP_fivekg", "ep_return_to_sender", "exp_eparcel_returns",
                                                               "reg_eparcel_returns", "reg_ep_call_for_return"))

final_output_minus_fees_and_charge_easy <- final_output2[!grepl("charge|surcharge|admin|fuel|more|unmanifest", final_output2$DESCRIPTION, ignore.case = TRUE), ]
invoice_test_just_basic$total_supply_for_the_period <-sum(final_output_minus_fees_and_charge_easy$AMOUNT.EXCL.TAX, na.rm = TRUE)

invoice_test_just_basic$fees_and_charges_for_the_period_1 <- sum(final_output2$FUEL.SURCHARGE..) + sum(final_output2$SMC.FEE) +sum(final_output2$OVER.MAX.LIMITS.FEE)

invoice_test_just_basic$gst_1 <- sum(final_output_minus_fees_and_charge_easy$SMC.GST + final_output_minus_fees_and_charge_easy$FUEL.GST 
                          + final_output_minus_fees_and_charge_easy$TAX.AMOUNT)


#### Post feb changes - my figures ####
invoice_test_post_feb <- data.frame(
  Name = c("Post feb rates base")
)

final_output3 <- subset(final_output_b, uplift_service %in% c( "Regular.VIC", "Express.VIC", "Regular.NSW", "Express.NSW", "EPP_fivekg", "ep_return_to_sender", "exp_eparcel_returns",
                                                               "reg_eparcel_returns", "reg_ep_call_for_return"))

invoice_test_post_feb$total_supply_for_the_period <- sum(final_output3$base_charge_exgst, na.rm = TRUE)

invoice_test_post_feb$new_fees_and_surcharges <- sum(final_output3$fuel_surcharge,na.rm = TRUE)+ sum(final_output3$sec_mng_chrg,na.rm = TRUE)+ sum(final_output3$over_max_limits_fee, na.rm = TRUE)

invoice_test_post_feb$gst_1 <- sum(final_output3$fuel_gst, na.rm = TRUE) + sum(final_output3$sec_mng_gst, na.rm = TRUE) + sum (final_output3$base_charge_tax, na.rm = TRUE)


write.csv(invoice_test_post_feb, file = "invoice_test_post_feb.csv")

### additionals. not sure where to put these just yet
final_output$charge_value_uplift <- as.numeric(gsub("%", "", final_output$charge_value_uplift))
invoice_test_post_feb$average_uplift <- mean(final_output$charge_value_uplift, na.rm = TRUE)


final_output$uplift_figure_exgst <- as.numeric(gsub("%", "", final_output$uplift_figure_exgst))
invoice_test_post_feb$average_uplift_figure_exgst <- mean(final_output$uplift_figure_exgst, na.rm = TRUE)








#### break down by customer #### 
# first clean out the rejected 

final_output_c <- final_output_b %>%
  filter(!(weight_category_max %in% c("Above_22kg_for_Wine", "Above_20kg_for_international")))


avg_base_charge_exgst <- final_output_c %>%
  group_by(NAME_1, DESCRIPTION, weight_category_max) %>% 
  summarise(base_charge_exgst = mean(base_charge_exgst, na.rm = TRUE))
print(avg_base_charge_exgst)




per_custo <- final_output_c %>%
  group_by(NAME_1, DESCRIPTION, weight_category_max) %>% 
  summarise(
    base_charge_exgst_count = n(),
    base_charge_exgst_sum = sum(base_charge_exgst, na.rm = TRUE),
    base_charge_exgst_mean = mean(base_charge_exgst, na.rm = TRUE),
    base_charge_tax_sum = mean(base_charge_tax, na.rm = TRUE),
    charge_value_uplift_mean = mean(charge_value_uplift, na.rm = TRUE),
    uplift_figure_exgst_sum = sum(uplift_figure_exgst, na.rm = TRUE),
    uplift_figure_exgst_mean = mean(uplift_figure_exgst, na.rm = TRUE),
    charge_to_custo_exgst_sum = sum(charge_to_custo_exgst, na.rm = TRUE),
    charge_to_custo_exgst_mean = mean(charge_to_custo_exgst, na.rm = TRUE)
    
    
    
    
    
    # You can add more metrics as needed
  )

print(per_custo)
write.csv(per_custo, file = "per_custo.csv")



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
