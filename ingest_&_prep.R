##### Prep work #####
# bring in the required packages
#install.packages("qpcR")
library(qpcR)

#install.packages("tidyr")
library(tidyr)

#install.packages("plyr")
library(plyr)

#install.packages("dplyr")
library(dplyr)


##### Bring in the required datasets #####

#bill = read.csv("ESTORELOGISTICSPTYLTD_0006794750_20240303_1013048181.csv", head=TRUE, sep=",")
#bill = read.csv("ESTORELOGISTICSPTYLTD_0006794750_20240219_1013016084.csv", head=TRUE, sep=",")
#bill = read.csv("1013111472-5890569129689088.csv", head=TRUE, sep=",")
#1013156007-5729374082957312
bill = read.csv("1013156007-5729374082957312.csv", head=TRUE, sep=",")

# pre feb base rates. Left in for pulling comparison calcs
#cz_pre_feb_eparcel_regular_ex_mel = read.csv("cz_pre_feb_eparcel_regular_ex_mel.csv", head=TRUE, row.names = 1,  sep=",")
#cz_pre_feb_eparcel_express_ex_mel = read.csv("cz_pre_feb_eparcel_express_ex_mel.csv", head=TRUE, row.names = 1,  sep=",")
#cz_pre_feb_eparcel_regular_ex_syd = read.csv("cz_pre_feb_eparcel_regular_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")
#cz_pre_feb_eparcel_express_ex_syd = read.csv("cz_pre_feb_eparcel_express_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")

# post feb 1st base rates. Left in for pulling comparison calcs

cz_post_feb_eparcel_regular_ex_mel = read.csv("cz_post_feb_eparcel_regular_ex_mel.csv", head=TRUE, row.names = 1,  sep=",")
cz_post_feb_eparcel_express_ex_mel = read.csv("cz_post_feb_eparcel_express_ex_mel.csv", head=TRUE, row.names = 1,  sep=",")
cz_post_feb_eparcel_regular_ex_syd = read.csv("cz_post_feb_eparcel_regular_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")
cz_post_feb_eparcel_express_ex_syd = read.csv("cz_post_feb_eparcel_express_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")
cz_post_feb_eparcel_express_ex_syd = read.csv("cz_post_feb_eparcel_express_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")
cz_post_feb_eparcel_wine_ex_mel = read.csv("cz_post_feb_eparcel_wine_ex_mel.csv", head=TRUE, row.names = 1,  sep=",")
cz_post_feb_eparcel_wine_ex_syd = read.csv("cz_post_feb_eparcel_wine_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")
cz_post_feb_eparcel_international_express_merch = read.csv("cz_post_feb_eparcel_international_express_merch.csv", head=TRUE, row.names = 1,  sep=",")
cz_post_feb_eparcel_international_standard = read.csv("cz_post_feb_eparcel_international_standard.csv", head=TRUE, row.names = 1,  sep=",")


# custo mark up
customer_uplift_march_24 = read.csv("customer_uplift_march_24.csv", head=TRUE, row.names = 1,  sep=",")


#colnames(cz_melb_espress) <- sub("^X", "", colnames(cz_melb_espress))

#### remove the summary lines we do not want ####
bill_cut1 <- bill[!grepl("charge|surcharge|admin|fuel", bill$DESCRIPTION, ignore.case = TRUE), ]
#bill_cut1 <- bill

#cutting the dataset down to just the metrics we need for ALL of the basic calculations
bill_cut1 <-  bill_cut1[,  c("REGION", "RECEIVING.COUNTRY", "CUSTOMER", "NAME_1", "NAME_2", "NAME_3", "DESCRIPTION", "BILLING.DOC", "SERVICE.DATE", "TO.ADDRESS", "CONSIGNMENT.ID", "ARTICLE.ID",   
                            "BILLED.LENGTH", "BILLED.WIDTH", "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "ACTUAL.WEIGHT", "CHARGE.ZONE", "FROM.STATE", "AVG..UNIT.PRICE" , "AMOUNT.EXCL.TAX", "DECLARED.WEIGHT")] 

# get the lift service as per uplift card. This covers all thats in the description
bill_cut1$service <- ifelse(bill_cut1$REGION == "VIC" & bill_cut1$DESCRIPTION == "Parcel Post with Signature", "Regular.VIC",
                                    ifelse(bill_cut1$REGION == "VIC" & bill_cut1$DESCRIPTION == "Express Post with Signature", "Express.VIC",
                                           ifelse(bill_cut1$REGION == "NSW" & bill_cut1$DESCRIPTION == "Parcel Post with Signature", "Regular.NSW",
                                                  ifelse(bill_cut1$REGION == "NSW" & bill_cut1$DESCRIPTION == "Express Post with Signature", "Express.NSW",
                                                         ifelse(bill_cut1$DESCRIPTION == "Express Post Parcels (BYO up to 5kg)", "EPP_fivekg",
                                                                ifelse(bill_cut1$DESCRIPTION == "eParcel Return To Sender", "ep_return_to_sender",
                                                                       ifelse(bill_cut1$DESCRIPTION == "Express Post eparcel returns", "exp_eparcel_returns",
                                                                              ifelse(bill_cut1$DESCRIPTION == "eParcel Post Return", "reg_eparcel_returns",
                                                                                     ifelse(bill_cut1$DESCRIPTION == "eParcel Call For Return", "reg_ep_call_for_return",
                                                                                            ifelse(bill_cut1$REGION == "VIC" & bill_cut1$DESCRIPTION == "EPARCEL WINE STD", "Wine.VIC",
                                                                                                   ifelse(bill_cut1$REGION == "NSW" & bill_cut1$DESCRIPTION == "EPARCEL WINE STD", "Wine.NSW",
                                                                                                          ifelse(bill_cut1$DESCRIPTION == "PACK AND TRACK INTERNATIONAL", "International",
                                                                                                                 ifelse(bill_cut1$DESCRIPTION == "Express Courier International (eParcel)", "International",
                                                         NA)))))))))))))

bill_cut1$uplift <- ifelse(bill_cut1$DESCRIPTION == "Express Post Parcels (BYO up to 5kg)", "EPP_fivekg",
                           ifelse(bill_cut1$DESCRIPTION %in% c("eParcel Return To Sender", "eParcel Post Return", "eParcel Call For Return"), 
                                  ifelse(bill_cut1$REGION == "VIC", "Regular.VIC", 
                                         ifelse(bill_cut1$REGION == "NSW", "Regular.NSW", NA)
                                  ),
                                  ifelse(bill_cut1$DESCRIPTION == "Express Post eparcel returns",
                                         ifelse(bill_cut1$REGION == "VIC", "Express.VIC", 
                                                ifelse(bill_cut1$REGION == "NSW", "Express.NSW", NA)
                                         ),
                                         ifelse(bill_cut1$DESCRIPTION == "EPARCEL WINE STD",
                                                ifelse(bill_cut1$REGION == "VIC", "Wine.VIC",
                                                       ifelse(bill_cut1$REGION == "NSW", "Wine.NSW", NA)
                                                ),
                                                ifelse(bill_cut1$DESCRIPTION %in% c("PACK AND TRACK INTERNATIONAL", "Express Courier International (eParcel)", "International Returns Express"), "International", 
                                                       ifelse(bill_cut1$REGION == "VIC", 
                                                              ifelse(bill_cut1$DESCRIPTION == "Parcel Post with Signature", "Regular.VIC",
                                                                     ifelse(bill_cut1$DESCRIPTION == "Express Post with Signature", "Express.VIC", NA)),
                                                              ifelse(bill_cut1$REGION == "NSW", 
                                                                     ifelse(bill_cut1$DESCRIPTION == "Parcel Post with Signature", "Regular.NSW",
                                                                            ifelse(bill_cut1$DESCRIPTION == "Express Post with Signature", "Express.NSW", NA)), NA
                                                              )))))))
       





##### customer code ####
# Function to extract letters before the first "-"
extract_letters <- function(text) {
  split_text <- strsplit(as.character(text), "-")[[1]]
  return(trimws(split_text[1]))  
}
bill_cut1$customer_code <- sapply(bill_cut1$NAME_1, extract_letters)
### create a col to determine if its GST free

# Define a function to apply the logic
is_gst_free <- function(zone) {
  ifelse(zone %in% c("NF", "W4", "AAT", "Z1", "Z2", "Z3", "Z4", "Z5", "Z6", "Z7", "Z8", "Z9" ), "Yes", "No")
}

# Apply the function to create the new column
bill_cut1$is_gst_free_zone <- is_gst_free(bill_cut1$CHARGE.ZONE)


