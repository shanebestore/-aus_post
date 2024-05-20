###### Section 4.a Summary calculations created ----
# just the custo and description 
summary_by_description <- billing_doc_output %>%
  filter(!(DESCRIPTION %in% c("AP Parcels Domestic Fuel Surcharge",
                              "AP Security Mgt Charge",
                              "AP Parcels Domestic Fuel Surchg Tax Free",
                              "AP Security Mgt Charge Tax Free",
                              "STC Parcels Domestic Fuel Surcharge",
                              "Over Maximum Limits Fee"))) %>%
  group_by(NAME_1, DESCRIPTION) %>%
  summarize(
    count = n(),
    sum_of_AMOUNT.INCL.TAX = sum(AMOUNT.INCL.TAX, na.rm = TRUE),
    sum_of_base_charge_incgst = sum(base_charge_incgst, na.rm = TRUE),
    sum_of_AMOUNT.EXCL.TAX = sum(AMOUNT.EXCL.TAX, na.rm = TRUE),
    sum_of_base_charge_exgst = sum(base_charge_exgst, na.rm = TRUE),
    sum_of_uplift_figure_exgst = sum(uplift_figure_exgst, na.rm = TRUE),
    sum_of_charge_to_custo_exgst = sum(charge_to_custo_exgst, na.rm = TRUE),
    sum_of_FUEL.SURCHARGE.. = sum(FUEL.SURCHARGE.., na.rm = TRUE),
    sum_of_fuel_surcharge = sum(fuel_surcharge, na.rm = TRUE), 
    sum_of_fuel_surcharge_uplifted = sum(fuel_surcharge_uplifted, na.rm = TRUE),
    sum_of_SMC.FEE = sum(SMC.FEE, na.rm = TRUE),
    sum_of_sec_mng_chrg = sum(sec_mng_chrg, na.rm = TRUE),
    sum_of_sec_mng_chrg_uplifted= sum(sec_mng_chrg_uplifted, na.rm = TRUE),
#    sum_of_OVER.MAX.LIMITS.FEE = sum(OVER.MAX.LIMITS.FEE, na.rm = TRUE),
    sum_of_over_max_limits_fee = sum(over_max_limits_fee, na.rm = TRUE)
  )


output_folder <- file.path(getwd(), paste0("output_billing_dates_", predefined_text))
if (!file.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

file_name <- paste0("summary_by_custo_&_description_", predefined_text, ".csv")
full_file_path <- file.path(output_folder, file_name)

write.csv(summary_by_description, file = full_file_path, row.names = FALSE)


#just the description 
summary_by_description <- billing_doc_output %>%
  filter(!(DESCRIPTION %in% c("AP Parcels Domestic Fuel Surcharge",
                              "AP Security Mgt Charge",
                              "AP Parcels Domestic Fuel Surchg Tax Free",
                              "AP Security Mgt Charge Tax Free",
                              "STC Parcels Domestic Fuel Surcharge",
                              "Over Maximum Limits Fee"))) %>%
  group_by(DESCRIPTION) %>%
  summarize(
    count = n(),
    sum_of_AMOUNT.INCL.TAX = sum(AMOUNT.INCL.TAX, na.rm = TRUE),
    sum_of_base_charge_incgst = sum(base_charge_incgst, na.rm = TRUE),
    sum_of_AMOUNT.EXCL.TAX = sum(AMOUNT.EXCL.TAX, na.rm = TRUE),
    sum_of_base_charge_exgst = sum(base_charge_exgst, na.rm = TRUE),
    sum_of_uplift_figure_exgst = sum(uplift_figure_exgst, na.rm = TRUE),
    sum_of_charge_to_custo_exgst = sum(charge_to_custo_exgst, na.rm = TRUE),
    sum_of_FUEL.SURCHARGE.. = sum(FUEL.SURCHARGE.., na.rm = TRUE),
    sum_of_fuel_surcharge = sum(fuel_surcharge, na.rm = TRUE), 
    sum_of_fuel_surcharge_uplifted = sum(fuel_surcharge_uplifted, na.rm = TRUE),
    sum_of_SMC.FEE = sum(SMC.FEE, na.rm = TRUE),
    sum_of_sec_mng_chrg = sum(sec_mng_chrg, na.rm = TRUE),
    sum_of_sec_mng_chrg_uplifted= sum(sec_mng_chrg_uplifted, na.rm = TRUE),
    #    sum_of_OVER.MAX.LIMITS.FEE = sum(OVER.MAX.LIMITS.FEE, na.rm = TRUE),
    sum_of_over_max_limits_fee = sum(over_max_limits_fee, na.rm = TRUE)
  )

output_folder <- file.path(getwd(), paste0("output_billing_dates_", predefined_text))
if (!file.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

file_name <- paste0("summary_by_description_", predefined_text, ".csv")
full_file_path <- file.path(output_folder, file_name)

write.csv(summary_by_description, file = full_file_path, row.names = FALSE)
