######## just the custo and description ########
summary_by_description <- ap_post_supply %>%
  filter(!(DESCRIPTION %in% c("AP Parcels Domestic Fuel Surcharge",
                              "AP Security Mgt Charge",
                              "AP Parcels Domestic Fuel Surchg Tax Free",
                              "AP Security Mgt Charge Tax Free",
                              "STC Parcels Domestic Fuel Surcharge",
                              "Over Maximum Limits Fee"))) %>%
  group_by(NAME_1, DESCRIPTION) %>%
  summarize(
    sum_of_AMOUNT.INCL.TAX = sum(AMOUNT.INCL.TAX, na.rm = TRUE),
    sum_of_base_charge_incgst = sum(base_charge_incgst, na.rm = TRUE),
    sum_of_AMOUNT.EXCL.TAX = sum(AMOUNT.EXCL.TAX, na.rm = TRUE),
    sum_of_base_charge_exgst = sum(base_charge_exgst, na.rm = TRUE),
    sum_of_uplift_figure_exgst = sum(uplift_figure_exgst, na.rm = TRUE),
    sum_of_charge_to_custo_exgst = sum(charge_to_custo_exgst, na.rm = TRUE),
    sum_of_FUEL.SURCHARGE.. = sum(FUEL.SURCHARGE.., na.rm = TRUE),
    sum_of_fuel_surcharge = sum(fuel_surcharge, na.rm = TRUE),
    sum_of_SMC.FEE = sum(SMC.FEE, na.rm = TRUE),
    sum_of_sec_mng_chrg = sum(sec_mng_chrg, na.rm = TRUE),
#    sum_of_OVER.MAX.LIMITS.FEE = sum(OVER.MAX.LIMITS.FEE, na.rm = TRUE),
    sum_of_over_max_limits_fee = sum(over_max_limits_fee, na.rm = TRUE)
  )

print(summary_by_description)


write.csv(summary_by_description, file = "summary_by_custo_&_description_1703_to_3103_incl.csv")

######## just the description ########

summary_by_description <- ap_post_supply %>%
  filter(!(DESCRIPTION %in% c("AP Parcels Domestic Fuel Surcharge",
                              "AP Security Mgt Charge",
                              "AP Parcels Domestic Fuel Surchg Tax Free",
                              "AP Security Mgt Charge Tax Free",
                              "STC Parcels Domestic Fuel Surcharge",
                              "Over Maximum Limits Fee"))) %>%
  group_by(DESCRIPTION) %>%
  summarize(
    sum_of_AMOUNT.INCL.TAX = sum(AMOUNT.INCL.TAX, na.rm = TRUE),
    sum_of_base_charge_incgst = sum(base_charge_incgst, na.rm = TRUE),
    sum_of_AMOUNT.EXCL.TAX = sum(AMOUNT.EXCL.TAX, na.rm = TRUE),
    sum_of_base_charge_exgst = sum(base_charge_exgst, na.rm = TRUE),
    sum_of_uplift_figure_exgst = sum(uplift_figure_exgst, na.rm = TRUE),
    sum_of_charge_to_custo_exgst = sum(charge_to_custo_exgst, na.rm = TRUE),
    sum_of_FUEL.SURCHARGE.. = sum(FUEL.SURCHARGE.., na.rm = TRUE),
    sum_of_fuel_surcharge = sum(fuel_surcharge, na.rm = TRUE),
    sum_of_SMC.FEE = sum(SMC.FEE, na.rm = TRUE),
    sum_of_sec_mng_chrg = sum(sec_mng_chrg, na.rm = TRUE),
#    sum_of_OVER.MAX.LIMITS.FEE = sum(OVER.MAX.LIMITS.FEE, na.rm = TRUE),
    sum_of_over_max_limits_fee = sum(over_max_limits_fee, na.rm = TRUE)
  )

print(summary_by_description)


write.csv(summary_by_description, file = "summary_by_description_1703_to_3103_incl.csv")
