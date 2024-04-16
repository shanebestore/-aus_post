
##### Basic Charges - Melbourne Express #####
# Basic rate for Melbourne only. Firstly we'll do Parcel Post out of Melbourne
# old calculation. Done as the logic was needed for the main calculation and to compare results against

## Just looking at one billing.doc for testing purposes a
#sub_1 <- subset(bill, bill$BILLING.DOC == 7817519603 & bill$DESCRIPTION == "Express Post with Signature")

#cutting the dataset down again to just what we need for this caculation
bill_cut2 <-  bill_cut1[, c("BILLING.DOC", "DESCRIPTION", "BILLED.WEIGHT", "CHARGE.ZONE", "FROM.STATE")] 


#remove the rows to which description does not apply
bill_cut2 <- subset(bill_cut2, bill_cut2$DESCRIPTION == "Express Post with Signature" | bill_cut2$DESCRIPTION == "Parcel Post with Signature")
bill_cut2 <- subset(bill_cut2, FROM.STATE == "VIC")

nrow(bill_cut2)

## find the charge_zone weight
cz_weight_data <- bill_cut2$BILLED.WEIGHT

# define categorisation function to calssify the weight
categorize_weight <- function(weight_kg) {
  categories <- sapply(weight_kg, function(w) {
    if (is.na(w)) {
      return("NA")
    } else if (w == 0) {
      return("na")
    } else if(w >= 0.1 & w <= 0.5) {
      return("Up_to_500g")
    } else if (w <= 1) {
      return("X501g_to_1kg")
    } else if (w <= 2) {
      return("X1.01kg_to_2kg")
    } else if (w <= 3) {
      return("X2.01kg_to_3kg")
    } else if (w <= 4) {
      return("X3.01kg_to_4kg")
    } else if (w <= 5) {
      return("X4.01kg_to_5kg")
    } else if (w <= 7) {
      return("X5.01kg_to_7kg")
    } else if (w <= 10) {
      return("X7.01kg_to_10kg")
    } else if (w <= 15) {
      return("X10.01kg_to_15kg")
    } else if (w <= 22) {
      return("X15.01kg_to_22kg")
    } else {
      return(">22kg")
    }
  })
  return(categories)
}
weight_category <- categorize_weight(cz_weight_data)
### attach the charge zone and the weight for classifying prices
output <- cbind(bill_cut2,  weight_category)

# remove the rows where the bill.weight is zero -- this was relevant before we removed everything bar bill$DESCRIPTION == "Express Post with Signature" 
#bill$DESCRIPTION == "Parcel Post with Signature". Have to confirm this
#output <- subset(bill_cut2, BILLED.WEIGHT != 0) 

# have to remove the columns where the weight is over 22kg for additional processing
output <- subset(output, weight_category != ">22kg")




#### now that we have the output file we need to determine the indexes to use to query the express charge sheet
# find the row and column number to reference against z_c
col_name <- as.character(weight_category)
col_index <- unlist(lapply(weight_category, function(col_name) {
  which(colnames(cz_melb_espress) == col_name)
}))


row_name <- as.character(output$CHARGE.ZONE)
row_index <- unlist(lapply(output$CHARGE.ZONE, function(row_name) {
  which(rownames(cz_melb_espress) == row_name)
}))


#appened the the results into our dataset
output_2 <-cbind(output, (cbind(row_index, col_index)))




# Function to extract values from charge dataset based on indices provided in billing dataset
#nb this might only give us the first element of the cell we want

extract_charge_value <- function(row_index, col_index) {
  # Extract value from charge dataset
  charge_value <- cz_melb_espress[row_index, col_index]
  return(charge_value)
}


# Apply function to each row of billing dataset to extract charge indices and fed into the output file
#charge_value <- mapply(extract_charge_value, output_2$row_index, output_2$col_index)
output_2$charge_value <- mapply(extract_charge_value, output_2$row_index, output_2$col_index)



write.csv(output_2, file = "output_2.csv")





