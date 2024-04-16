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




