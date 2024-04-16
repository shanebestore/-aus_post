bill = read.csv("ESTORELOGISTICSPTYLTD_0006794750_20240303_1013048181.csv", head=TRUE, sep=",")
cz_melb_espress = read.csv("charge_zone_melbourne_express.csv", head=TRUE, row.names = 1,  sep=",")

#colnames(cz_melb_espress) <- sub("^X", "", colnames(cz_melb_espress))


##### Basic Charges #####
# old calculation. Done as the logic was needed for the main calculation and to compare results against

## Creating a test subset for testing purposes and cut it down to the metrics we need for the calc.
sub_1 <- subset(bill, bill$BILLING.DOC == 7817519603 & bill$DESCRIPTION == "Express Post with Signature")
bill_eg <-  sub_1[, c("BILLING.DOC", "BILLED.WEIGHT", "CHARGE.ZONE", "FROM.STATE")] 


## find the charge_zone weight
cz_weight_data <- bill_eg$BILLED.WEIGHT

# define categorisation function to calssify the weight
categorize_weight <- function(weight_kg) {
  categories <- sapply(weight_kg, function(w) {
    if (w <= 0.5) {
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
      return(NA)
    }
  })
  return(categories)
}
weight_category <- categorize_weight(cz_weight_data)
### attach the charge zone and the weight. Now that we have these we can query the reference data for the prices
output <- cbind(bill_eg,  weight_category)

#### now that we have the output file we need to determine the indexes to use to query the express charge sheet
# find the coloumn number to reference against z_c
col_name <- as.character(weight_category)

col_index <- lapply(weight_category, function(col_name) {
  which(colnames(cz_melb_espress) == col_name)
})

#find the row number to reference against the z_c
# have to make 

row_name <- as.character(output$CHARGE.ZONE)

row_index <- lapply(output$CHARGE.ZONE, function(row_name) {
  which(rownames(cz_melb_espress) == row_name)
})


#appened the the results into our dataset
output_2 <-cbind(output, (cbind(row_index, col_index)))
###### happy with the above



# Function to extract values from charge dataset based on indices provided in billing dataset
extract_charge_value <- function(row_index, col_index) {
  # Extract value from charge dataset
  charge_value <- cz_melb_espress[row_index, col_index]
  return(charge_value)
}

# Apply function to each row of billing dataset to extract charge values
output_2$charge_value <- mapply(extract_charge_value, output_2$row_index, output_2$col_index)

# Print updated billing dataset with charge values
print(output_2)





