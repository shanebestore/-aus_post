####### output format
final_output_post  = read.csv("final_output.csv", head=TRUE, sep=",")

# Check for duplicate column names
if (any(duplicated(names(final_output_post)))) {
  stop("Duplicate column names found in final_output.")
}

print(duplicated(names(final_output_post)))

# Add the Service column
final_output_post  <- final_output_post  %>%
  mutate(Service = case_when(
    DESCRIPTION == "Over Maximum Limits Fee" ~ "Additional Charges",
    DESCRIPTION == "APGL NZ Express w/Signature" ~ "APGL NZ Express",
    DESCRIPTION == "Express Post with Signature" ~ "eParcel Express",
    DESCRIPTION == "AP Security Mgt Charge" ~ "eParcel Express",
    DESCRIPTION == "PACK AND TRACK INTERNATIONAL" ~ "eParcel International",
    DESCRIPTION == "Express Courier International (eParcel)" ~ "eParcel International",
    DESCRIPTION == "Parcel Post with Signature" ~ "eParcel Regular",
    DESCRIPTION == "AP Parcels Domestic Fuel Surcharge" ~ "eParcel Regular",
    DESCRIPTION == "AP Parcels Domestic Fuel Surchg Tax Free" ~ "eParcel Regular",
    DESCRIPTION == "eParcel Return To Sender" ~ "eParcel Regular Returns",
    DESCRIPTION == "eParcel Call For Return" ~ "eParcel Regular Returns",
    DESCRIPTION == "eParcel Post Return" ~ "eParcel Regular Returns",
    DESCRIPTION == "More to Pay" ~ "eParcel Regular Returns",
    DESCRIPTION == "EPARCEL WINE STD" ~ "eParcel Wine",
    DESCRIPTION == "On Demand Tonight" ~ "StarTrack OnDemand",
    DESCRIPTION == "STC Parcels Domestic Fuel Surcharge" ~ "StarTrack OnDemand",
    DESCRIPTION == "On Demand Return to Sender" ~ "StarTrack OnDemand",
    DESCRIPTION == "On Demand Afternoon" ~ "StarTrack OnDemand",
    TRUE ~ NA_character_
  ))

final_output_post <- final_output_post %>%
  mutate(
    New_Column = ifelse(is.na(charge_to_custo_exgst), AMOUNT.EXCL.TAX, charge_to_custo_exgst)
  )


ap_post_supply <- final_output_post [, c("customer_code" , "NAME_1", "MAILING.STATEMENT.NO.", "ASSIGNMENT.NO." , "SERVICE.DATE" , "DESCRIPTION",
                                   "BILLING.DATE", "CONSIGNMENT.ID", "article_id", "LODGEMENT.DATE", "ACTUAL.WEIGHT", "ACTUAL.UNIT", "ACTUAL.LENGTH",
                                   "ACTUAL.WIDTH", "ACTUAL.HEIGHT", "ACTUAL.UNIT.TYPE", "DECLARED.WEIGHT", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH", 
                                   "DECLARED.WIDTH"	, "DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH",	"DECLARED.WIDTH",
                                   "DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "FROM.NAME", 	"FROM.ADDRESS",	"FROM.CITY",	"FROM.STATE",	"FROM.POSTAL.CODE",
                                   "TO.NAME",	"TO.ADDRESS",	"TO.CITY",	"TO.STATE",	"TO.POSTAL.CODE", "CUST.REF.1",	"CUST.REF.2",	"BILLED.LENGTH", "BILLED.WIDTH",
                                   "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "CHARGE.CODE", "RECEIVING.COUNTRY", "CHARGE.ZONE.x", "Service", "QTY", "New_Column"
                                   

                                   

                                   
                                   )]


write.csv(ap_post_supply, file = "ap_post_supply.csv")



INTL CHARGE ZONE				QTY	AVG. UNIT PRICE EX GST	AMOUNT EX GST


# Check for duplicate column names in final_output
if (any(duplicated(names(final_output_post )))) {
  stop("Duplicate column names found in final_output.")
}

# Check for duplicate column names in final_output_post
if (any(duplicated(names(final_output_post)))) {
  stop("Duplicate column names found in final_output_post.")
}

# Add the new column to final_output_post
final_output_post <- final_output_post %>%
  mutate(
    New_Column = ifelse(is.na(charge_to_custo_exgst), AMOUNT.EXCL.TAX, charge_to_custo_exgst)
  )
                   
                   


