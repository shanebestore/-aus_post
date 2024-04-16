#### Basic Charges - Melbourne & eParcel Regular/Parcel post with signature #####
# new calculation goes here

#### Ingesting Data and cutting down data set ####
bill_cut_a <-  bill_cut1

#### Cubic factor ####
# leaving this here as it might only relate to basic charges

factor <- 250  # Change this to your desired factor
cubic_size <- bill_cut_a$BILLED.HEIGHT * bill_cut_a$BILLED.LENGTH * bill_cut_a$BILLED.WIDTH
bill_cut_a$cubic_weight <- cubic_size * factor

#### pull the max of cubic_weight vs billed_weight #########
bill_cut_a <- mutate(bill_cut_a, max_weight = pmax(cubic_weight, BILLED.WEIGHT))

bill_cut_a$max_weight <- ceiling(bill_cut_a$max_weight)

#### Classifying weights ####
# leaving this in as we are classfifying the new max weight
cz_max_weight <- bill_cut_a$max_weight

# define categorisation function to calssify the weight
categorize_weight <- function(weight_kg) {
  categories <- sapply(weight_kg, function(w) {
    if (is.na(w)) {
      return("NA")
    } else if (w == 0) {
      return("na")
    } else if(w >= 0.00001 & w <= 0.5) {
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
      return("Basic")  # Changed this line to return "Basic" for weights > 22kg
    }
  })
  return(categories)
}
weight_category_max <- categorize_weight(cz_max_weight)
output_d <- cbind(bill_cut_a,  weight_category_max)

#write.csv(output_d, file = "output_d.csv")

################################# construction zone ##################################
# just putting the dataset cut here so that we can build the CSV for tomorrow

output_d <- subset(output_d, uplift_service %in% c("Express.NSW"))

#############################################################################


#### Get the base charge ####
#Determine the indexes to use to query the new base charge zone sheet 

# find the row and column number to reference against z_c
col_name_max<- as.character(output_d$weight_category_max)
col_index_max <- unlist(lapply(output_d$weight_category_max, function(col_name_max) {
  which(colnames(cz_post_feb_eparcel_express_ex_syd) == col_name_max)
}))

row_name_max<- as.character(output_d$CHARGE.ZONE)
row_index_max <- unlist(lapply(row_name_max, function(row_name_max) {
  index <- which(rownames(cz_post_feb_eparcel_express_ex_syd) == row_name_max)
  if (length(index) == 0) {
    NA
  } else {
    index
  }
}))


output_d_2 <-cbind(output_d, (cbind(row_index_max, col_index_max)))

# query new base charge rate 
# Function to extract values from charge zone dataset based on indices

extract_charge_value_max <- function(row_index_max, col_index_max) {
  charge_value <- cz_post_feb_eparcel_express_ex_syd[row_index_max, col_index_max]
  return(charge_value)
}

output_d_2$charge_value_max <- mapply(extract_charge_value_max, output_d_2$row_index_max, output_d_2$col_index_max)


# Function to calculate charge based on charge_value_max and Per_Kg_#. Also does the calc if  weight_category_max == "Basic"
calculate_final_charge <- function(charge_value_max, weight_category_max, max_weight, row_index_max) {
  if (weight_category_max == "Basic") {
    per_kg_value <- cz_post_feb_eparcel_express_ex_syd[row_index_max, "Per_Kg"]
    return(charge_value_max + (per_kg_value * max_weight))
  } else {
    return(charge_value_max)  
  }
}

output_d_2$final_charge_incgrt22 <- mapply(calculate_final_charge, output_d_2$charge_value_max, output_d_2$weight_category_max, output_d_2$max_weight, output_d_2$row_index_max)


#write.csv(output_d_2, file = "output_d_2.csv")

##### multiply by customer uplift   ####
# first step is to find the indices 

# service
col_name_uplift<- as.character(output_d_2$uplift_service)
col_index_uplift <- unlist(lapply(output_d_2$uplift_service, function(col_name_uplift) {
  which(colnames(customer_uplift_march_24) == col_name_uplift)
}))

# customer
row_name_uplift<- as.character(output_d_2$customer_code)
row_index_uplift <- unlist(lapply(output_d_2$customer_code, function(row_name_uplift) {
  index <- which(rownames(customer_uplift_march_24) == row_name_uplift)
  if (length(index) == 0) {
    NA
  } else {
    index
  }
}))

output_d_2 <-cbind(output_d_2, (cbind(row_index_uplift, col_index_uplift)))

# query uplift sheet to find uplift % ####

# Function to extract values from charge zone dataset based on indices
extract_charge_value_uplift<- function(row_index_uplift, col_index_uplift) {
  charge_value <- customer_uplift_march_24[row_index_uplift, col_index_uplift]
  return(charge_value)
}

output_d_2$charge_value_uplift <- mapply(extract_charge_value_uplift, output_d_2$row_index_uplift, output_d_2$col_index_uplift)

#write.csv(output_d_2, file = "output_d_2.csv")

#### multiply base by uplift ####
# Convert charge_value_uplift to numeric, handling NA values
output_d_2$charge_value_uplift_numeric <- ifelse(is.na(output_d_2$charge_value_uplift) | is.na(output_d_2$final_charge_incgrt22),
                                                 NA,
                                                 as.numeric(sub("%", "", output_d_2$charge_value_uplift)))

# Convert final_charge_incgrt22 to numeric, handling NA values
output_d_2$charge_value_max_numeric <- ifelse(is.na(output_d_2$charge_value_uplift) | is.na(output_d_2$final_charge_incgrt22),
                                              NA,
                                              as.numeric(gsub("[^0-9.]", "", output_d_2$final_charge_incgrt22)))

# Calculate the percentage of final_charge_incgrt22, handling NA values
output_d_2$uplift_figure <- ifelse(is.na(output_d_2$charge_value_uplift_numeric) | is.na(output_d_2$charge_value_max_numeric),
                                   NA,
                                   (output_d_2$charge_value_uplift_numeric / 100) * output_d_2$charge_value_max_numeric)

# Filter out NA and non-numeric values before performing addition
output_d_2$charge_to_custo <- ifelse(is.na(output_d_2$charge_value_max_numeric) | is.na(output_d_2$uplift_figure) | !is.numeric(output_d_2$charge_value_max_numeric) | !is.numeric(output_d_2$uplift_figure),
                                     NA,
                                     output_d_2$charge_value_max_numeric + output_d_2$uplift_figure)


# update table name
output_d_3_express_NSW <- output_d_2

##### write to CSV ####

write.csv(output_d_3_express_NSW, file = "output_d_3_express_NSW.csv")