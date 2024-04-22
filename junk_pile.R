







# calculate the Fuel surcharge tax free 
#output_all_services$fuel_surcharge_tax_free  <-  ifelse(output_all_services$is_gst_free == "Yes",
#                                                ((((output_all_services$fuel_surcharge/110)*100)/110) * 100), 
#                                                NA)



# service
#col_name_uplift<- as.character(output_all_services$uplift_service)
#col_index_uplift <- unlist(lapply(output_all_services$uplift_service, function(col_name_uplift) {
#  which(colnames(customer_uplift_march_24) == col_name_uplift)
#}))

# customer
#row_name_uplift<- as.character(output_all_services$customer_code)
#row_index_uplift <- unlist(lapply(output_all_services$customer_code, function(row_name_uplift) {
#  index <- which(rownames(customer_uplift_march_24) == row_name_uplift)
# if (length(index) == 0) {
#    NA
#  } else {
#    index
#  }
#}))










#factor <- 250  # Change this to your desired factor

# Sum the specified columns and multiply by the factor
#output_a_2 <- output_a_2 %>%
#  mutate(total_billed = BILLED.HEIGHT + BILLED.LENGTH + BILLED.WIDTH,
#         result = total_billed * factor)


# remove the rows where the bill.weight is zero -- this was relevant before we removed everything bar bill$DESCRIPTION == "Express Post with Signature" 
#bill$DESCRIPTION == "Parcel Post with Signature". Have to confirm this
#output <- subset(bill_cut2, BILLED.WEIGHT != 0) 



#clean the 0 values
#output_2$charge_value <- output_2$charge_value[output_2$charge_value != 0]

#output_2$charge_value <- unlist(output_2$charge_value) 


output_2$charge_value <- mapply(extract_charge_value, output_2$row_index, output_2$col_index)



distinct_count_article.id <- bill %>%
  distinct(ARTICLE.ID) %>%
  summarise(distinct_count = n())

print(distinct_count_article.id)


distinct_count_consignment.id <- bill %>%
  distinct(CONSIGNMENT.ID) %>%
  summarise(distinct_count = n())
print(distinct_count_consignment.id)



differential <- bill[bill$CONSIGNMENT.ID == bill$ARTICLE.ID, ]

write.csv(differential, file = "differential.csv")
nrow(bill)
nrow(differential)

bill_cut2 <-  bill_cut1[, c("BILLING.DOC", "BILLED.WEIGHT", "CHARGE.ZONE", "FROM.STATE")] 

## find the charge_zone weight
cz_weight_data <- bill_cut2$BILLED.WEIGHT

# define categorisation function to calssify the weight
categorize_weight <- function(weight_kg) {
  categories <- sapply(weight_kg, function(w) {
    if (w == 0) {
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
### attach the charge zone and the weight. Now that we have these we can query the reference data for the prices
output <- cbind(bill_cut2,  weight_category)

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



bill_cut2 <-  bill_cut1[, c("BILLING.DOC", "BILLED.WEIGHT", "CHARGE.ZONE", "FROM.STATE")] 

## find the charge_zone weight
cz_weight_data <- bill_cut2$BILLED.WEIGHT

# define categorisation function to calssify the weight
categorize_weight <- function(weight_kg) {
  categories <- sapply(weight_kg, function(w) {
    if (w == 0) {
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
### attach the charge zone and the weight. Now that we have these we can query the reference data for the prices
output <- cbind(bill_cut2,  weight_category)

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
bill_cut2 <-  bill_cut1[, c("BILLING.DOC", "BILLED.WEIGHT", "CHARGE.ZONE", "FROM.STATE")] 

## find the charge_zone weight
cz_weight_data <- bill_cut2$BILLED.WEIGHT

# define categorisation function to calssify the weight
categorize_weight <- function(weight_kg) {
  categories <- sapply(weight_kg, function(w) {
    if (w == 0) {
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
### attach the charge zone and the weight. Now that we have these we can query the reference data for the prices
output <- cbind(bill_cut2,  weight_category)

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
bill_cut2 <-  bill_cut1[, c("BILLING.DOC", "BILLED.WEIGHT", "CHARGE.ZONE", "FROM.STATE")] 

## find the charge_zone weight
cz_weight_data <- bill_cut2$BILLED.WEIGHT

# define categorisation function to calssify the weight
categorize_weight <- function(weight_kg) {
  categories <- sapply(weight_kg, function(w) {
    if (w == 0) {
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
### attach the charge zone and the weight. Now that we have these we can query the reference data for the prices
output <- cbind(bill_cut2,  weight_category)

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




CONSIGNMENT ID
ARTICLE ID
count (distinct(bill$ARTICLE.ID))


distinct_count <- bill %>%
  distinct(ARTICLE.ID) %>%
  summarise(distinct_count = n())

print(distinct_count)


distinct_count <- bill %>%
  distinct(CONSIGNMENT.ID) %>%
  summarise(distinct_count = n())
print(distinct_count)




differential <- bill[bill$CONSIGNMENT.ID == bill$ARTICLE.ID, ]

write.csv(differential, file = "differential.csv")
nrow(bill)
nrow(differential)



#### junk pile



# Convert indices to character type
output$CHARGE.ZONE <- as.character(output$CHARGE.ZONE)
output$weight_category <- as.character(output$weight_category)

# Check unique values
print(unique(output$CHARGE.ZONE))
print(unique(output$weight_category))



#row_index <- which(rownames(cz_melb_espress) == row_name)


# Query the values from cz_melb_espress using row and column indices
queried_values <- cz_melb_espress[output$CHARGE.ZONE, output$weight_category]

# Print the queried values
print(queried_values)


## testing the below
#first check coloumn names data type
#class(cz_melb_espress[1,])

#remeber r starts 
#print(cz_melb_espress[0,])

