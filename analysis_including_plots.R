head(bill)

result <- bill %>%
  group_by(SERVICE.DATE) %>%
  summarise(fuel_surcharge_pct = mean(FUEL.SURCHARGE.. / AVG..UNIT.PRICE, na.rm = TRUE))

#####

bill99 <- subset(bill,DESCRIPTION %in% c("Express Post with Signature", "Parcel Post with Signature") )

result <- bill99 %>%
  group_by(LODGEMENT.DATE, BILLING.DOC) %>%
  summarise(fuel_surcharge_pct = mean(FUEL.SURCHARGE.. / AMOUNT.EXCL.TAX, na.rm = TRUE))

write.csv(result, file = "result.csv")

#result$SERVICE.DATE <- as.character(result$SERVICE.DATE)

# Convert to Date format
#result$date <- as.Date(result$SERVICE.DATE, format = "%d%m%Y")

install.packages("ggplot2")
library(ggplot2)

ggplot(result, aes(x = LODGEMENT.DATE, y = fuel_surcharge_pct)) +
  geom_line() +
  labs(x = "LODGEMENT.DATE", y = "Average Division", title = "Average Division over Time")


#### Check for differences between new calc and aus post ####


#"Regular.VIC", "Express.VIC", "Regular.NSW", "Express.NSW", "EPP_fivekg", "ep_return_to_sender", "exp_eparcel_returns",
#"reg_eparcel_returns", "reg_ep_call_for_return"


final_output_d <- subset(final_output, uplift_service %in% c("Regular.VIC"))
final_output_d$rounded_AMT <- round(final_output_d$AMOUNT.INCL.TAX, 2)
final_output_d$rounded_base_charge <- round(final_output_d$base_charge_incgst, 2)
diff <- ifelse(final_output_d$rounded_AMT != final_output_d$rounded_base_charge, "yes", "no")
count_diff <- table(diff)
print(count_diff)

final_output_d <- subset(final_output, uplift_service %in% c("Express.NSW"))
final_output_d$rounded_AMT <- round(final_output_d$AMOUNT.INCL.TAX, 2)
final_output_d$rounded_base_charge <- round(final_output_d$base_charge_incgst, 2)
diff <- ifelse(final_output_d$rounded_AMT != final_output_d$rounded_base_charge, "yes", "no")
count_diff <- table(diff)
print(count_diff)


final_output_d <- subset(final_output, uplift_service %in% c("Express.VIC"))
final_output_d$rounded_AMT <- round(final_output_d$AMOUNT.INCL.TAX, 2)
final_output_d$rounded_base_charge <- round(final_output_d$base_charge_incgst, 2)
diff <- ifelse(final_output_d$rounded_AMT != final_output_d$rounded_base_charge, "yes", "no")
count_diff <- table(diff)
print(count_diff)

final_output_d <- subset(final_output, uplift_service %in% c("Regular.NSW"))
final_output_d$rounded_AMT <- round(final_output_d$AMOUNT.INCL.TAX, 2)
final_output_d$rounded_base_charge <- round(final_output_d$base_charge_incgst, 2)
diff <- ifelse(final_output_d$rounded_AMT != final_output_d$rounded_base_charge, "yes", "no")
count_diff <- table(diff)
print(count_diff)


final_output_d <- subset(final_output, uplift_service %in% c("reg_eparcel_returns"))
final_output_d$rounded_AMT <- round(final_output_d$AMOUNT.INCL.TAX, 2)
final_output_d$rounded_base_charge <- round(final_output_d$base_charge_incgst, 2)
diff <- ifelse(final_output_d$rounded_AMT != final_output_d$rounded_base_charge, "yes", "no")
count_diff <- table(diff)
print(count_diff)

final_output_d <- subset(final_output, uplift_service %in% c("reg_eparcel_returns"))
final_output_d$rounded_AMT <- round(final_output_d$AMOUNT.INCL.TAX, 2)
final_output_d$rounded_base_charge <- round(final_output_d$base_charge_incgst, 2)
diff <- ifelse(final_output_d$rounded_AMT != final_output_d$rounded_base_charge, "yes", "no")
count_diff <- table(diff)
print(count_diff)

final_output_d <- subset(final_output, uplift_service %in% c("Wine.VIC"))
final_output_d$rounded_AMT <- round(final_output_d$AMOUNT.INCL.TAX, 2)
final_output_d$rounded_base_charge <- round(final_output_d$base_charge_incgst, 2)
diff <- ifelse(final_output_d$rounded_AMT != final_output_d$rounded_base_charge, "yes", "no")
count_diff <- table(diff)
print(count_diff)

final_output_d <- subset(final_output, uplift_service %in% c("Wine.NSW"))
final_output_d$rounded_AMT <- round(final_output_d$AMOUNT.INCL.TAX, 2)
final_output_d$rounded_base_charge <- round(final_output_d$base_charge_incgst, 2)
diff <- ifelse(final_output_d$rounded_AMT != final_output_d$rounded_base_charge, "yes", "no")
count_diff <- table(diff)
print(count_diff)

final_output_d <- subset(final_output, DESCRIPTION %in% c("PACK AND TRACK INTERNATIONAL"))
final_output_d$rounded_AMT <- round(final_output_d$AMOUNT.INCL.TAX, 2)
final_output_d$rounded_base_charge <- round(final_output_d$base_charge_incgst, 2)
diff <- ifelse(final_output_d$rounded_AMT != final_output_d$rounded_base_charge, "yes", "no")
count_diff <- table(diff)
print(count_diff)

final_output_d <- subset(final_output, DESCRIPTION %in% c("Express Courier International (eParcel)"))
final_output_d$rounded_AMT <- round(final_output_d$AMOUNT.INCL.TAX, 2)
final_output_d$rounded_base_charge <- round(final_output_d$base_charge_incgst, 2)
diff <- ifelse(final_output_d$rounded_AMT != final_output_d$rounded_base_charge, "yes", "no")
count_diff <- table(diff)
print(count_diff)

