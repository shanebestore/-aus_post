#### Basic Charges - Melbourne & eParcel Regular/Parcel post with signature #####
# new calculation goes here

#### Ingesting Data and cutting down data set ####
bill_cut_a <-  bill_cut1

#### temp work zone here
#bill_cut_a  <- subset(bill_cut_a , NAME_1 %in% c('AGN - Ambition Group NZ - MW1'))

#### Cubic factor ####
# leaving this here as it might only relate to basic charges

factor <- 250  # Change this to your desired factor
cubic_size <- bill_cut_a$BILLED.HEIGHT * bill_cut_a$BILLED.LENGTH * bill_cut_a$BILLED.WIDTH
bill_cut_a$cubic_size <- cubic_size
bill_cut_a$cubic_weight <- cubic_size * factor
over_max_limits_fee <-100
#ex_pp_byo_up_to_5kg <-'Express Post Parcels (BYO up to 5kg)'

#### pull the max of cubic_weight vs billed_weight #########
bill_cut_a <- mutate(bill_cut_a, max_weight = pmax(cubic_weight, BILLED.WEIGHT))


#### Declare the charges ####
#fuel charge_ex_gst has to be calculated from the exgst charge value
gst <- 0.1
fuel_surcharge_pct <- 0.077
sec_mng_chrg_pct <- 0.0435
ep_return_to_sender_fee <- 12.85 #same for both express and standard
exp_eparcel_returns_fee <- 28.45 
reg_eparcel_returns_fee <- 12.43

#### over max limites fee
bill_cut_a$over_max_limits_fee <- ifelse(bill_cut_a$ACTUAL.WEIGHT > 22 | bill_cut_a$BILLED.LENGTH > 105 | bill_cut_a$cubic_size > 0.25, 100, NA)

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
output_a <- cbind(bill_cut_a,  weight_category_max)




write.csv(bill_cut_a, file = "bill_cut_a.csv")

#### base charge for Regular.VIC ####

# cut the dataset down to correct uplift service
output_a1 <- subset(output_a, uplift_service %in% c("Regular.VIC"))

#Determine the indexes to use to query the new base charge zone sheet 

# find the row and column number to reference against z_c
col_name_max<- as.character(output_a1$weight_category_max)
col_index_max <- unlist(lapply(output_a1$weight_category_max, function(col_name_max) {
  which(colnames(cz_post_feb_eparcel_regular_ex_mel) == col_name_max)
}))

row_name_max<- as.character(output_a1$CHARGE.ZONE)
row_index_max <- unlist(lapply(row_name_max, function(row_name_max) {
  index <- which(rownames(cz_post_feb_eparcel_regular_ex_mel) == row_name_max)
  if (length(index) == 0) {
    NA
  } else {
    index
  }
}))


output_a_2 <-cbind(output_a1, (cbind(row_index_max, col_index_max)))

# query new base charge rate 
# Function to extract values from charge zone dataset based on indices

extract_charge_value_max_incgst <- function(row_index_max, col_index_max) {
  charge_value <- cz_post_feb_eparcel_regular_ex_mel[row_index_max, col_index_max]
  return(charge_value)
}

output_a_2$charge_value_max_incgst <- mapply(extract_charge_value_max_incgst, output_a_2$row_index_max, output_a_2$col_index_max)


# Function to calculate charge based on charge_value_max and Per_Kg_#. Also does the calc if  weight_category_max == "Basic"
calculate_final_charge <- function(charge_value_max_incgst , weight_category_max, max_weight, row_index_max) {
  if (weight_category_max == "Basic") {
    per_kg_value <- cz_post_feb_eparcel_regular_ex_mel[row_index_max, "Per_Kg"]
    return(charge_value_max_incgst  + (per_kg_value * (ceiling(max_weight))))
  } else {
    return(charge_value_max_incgst )  
  }
}

output_a_2$base_charge_incgst <- mapply(calculate_final_charge, output_a_2$charge_value_max_incgst , output_a_2$weight_category_max, output_a_2$max_weight, output_a_2$row_index_max)

#### Base charge for Express.VIC ####
#sel
output_b1 <- subset(output_a, uplift_service %in% c("Express.VIC"))

#Determine the indexes to use to query the new base charge zone sheet 

# find the row and column number to reference against z_c
col_name_max<- as.character(output_b1$weight_category_max)
col_index_max <- unlist(lapply(output_b1$weight_category_max, function(col_name_max) {
  which(colnames(cz_post_feb_eparcel_express_ex_mel) == col_name_max)
}))

row_name_max<- as.character(output_b1$CHARGE.ZONE)
row_index_max <- unlist(lapply(row_name_max, function(row_name_max) {
  index <- which(rownames(cz_post_feb_eparcel_express_ex_mel) == row_name_max)
  if (length(index) == 0) {
    NA
  } else {
    index
  }
}))


output_b_2 <-cbind(output_b1, (cbind(row_index_max, col_index_max)))

# query new base charge rate 
# Function to extract values from charge zone dataset based on indices

extract_charge_value_max_incgst <- function(row_index_max, col_index_max) {
  charge_value <- cz_post_feb_eparcel_express_ex_mel[row_index_max, col_index_max]
  return(charge_value)
}

output_b_2$charge_value_max_incgst <- mapply(extract_charge_value_max_incgst, output_b_2$row_index_max, output_b_2$col_index_max)


# Function to calculate charge based on charge_value_max_incgst and Per_Kg_#. Also does the calc if  weight_category_max == "Basic"
calculate_final_charge <- function(charge_value_max_incgst, weight_category_max, max_weight, row_index_max) {
  if (weight_category_max == "Basic") {
    per_kg_value <- cz_post_feb_eparcel_express_ex_mel[row_index_max, "Per_Kg"]
    return(charge_value_max_incgst + (per_kg_value * (ceiling(max_weight))))
  } else {
    return(charge_value_max_incgst)  
  }
}

output_b_2$base_charge_incgst <- mapply(calculate_final_charge, output_b_2$charge_value_max_incgst, output_b_2$weight_category_max, output_b_2$max_weight, output_b_2$row_index_max)


##### Regular.NSW  ####

output_c1 <- subset(output_a, uplift_service %in% c("Regular.NSW"))

#Determine the indexes to use to query the new base charge zone sheet 

# find the row and column number to reference against z_c
col_name_max<- as.character(output_c1$weight_category_max)
col_index_max <- unlist(lapply(output_c1$weight_category_max, function(col_name_max) {
  which(colnames(cz_post_feb_eparcel_regular_ex_syd ) == col_name_max)
}))

row_name_max<- as.character(output_c1$CHARGE.ZONE)
row_index_max <- unlist(lapply(row_name_max, function(row_name_max) {
  index <- which(rownames(cz_post_feb_eparcel_regular_ex_syd ) == row_name_max)
  if (length(index) == 0) {
    NA
  } else {
    index
  }
}))


output_c_2 <-cbind(output_c1, (cbind(row_index_max, col_index_max)))

# query new base charge rate 
# Function to extract values from charge zone dataset based on indices

extract_charge_value_max_incgst <- function(row_index_max, col_index_max) {
  charge_value <- cz_post_feb_eparcel_regular_ex_syd [row_index_max, col_index_max]
  return(charge_value)
}

output_c_2$charge_value_max_incgst <- mapply(extract_charge_value_max_incgst, output_c_2$row_index_max, output_c_2$col_index_max)


# Function to calculate charge based on charge_value_max_incgst and Per_Kg_#. Also does the calc if  weight_category_max == "Basic"
calculate_final_charge <- function(charge_value_max_incgst, weight_category_max, max_weight, row_index_max) {
  if (weight_category_max == "Basic") {
    per_kg_value <- cz_post_feb_eparcel_regular_ex_syd [row_index_max, "Per_Kg"]
    return(charge_value_max_incgst + (per_kg_value * (ceiling(max_weight))))
  } else {
    return(charge_value_max_incgst)  
  }
}

output_c_2$base_charge_incgst <- mapply(calculate_final_charge, output_c_2$charge_value_max_incgst, output_c_2$weight_category_max, output_c_2$max_weight, output_c_2$row_index_max)


#### Express.VIC ####

output_d1 <- subset(output_a, uplift_service %in% c("Express.NSW"))

#Determine the indexes to use to query the new base charge zone sheet 

# find the row and column number to reference against z_c
col_name_max<- as.character(output_d1$weight_category_max)
col_index_max <- unlist(lapply(output_d1$weight_category_max, function(col_name_max) {
  which(colnames(cz_post_feb_eparcel_express_ex_syd) == col_name_max)
}))

row_name_max<- as.character(output_d1$CHARGE.ZONE)
row_index_max <- unlist(lapply(row_name_max, function(row_name_max) {
  index <- which(rownames(cz_post_feb_eparcel_express_ex_syd) == row_name_max)
  if (length(index) == 0) {
    NA
  } else {
    index
  }
}))


output_d_2 <-cbind(output_d1, (cbind(row_index_max, col_index_max)))

# query new base charge rate 
# Function to extract values from charge zone dataset based on indices

extract_charge_value_max_incgst <- function(row_index_max, col_index_max) {
  charge_value <- cz_post_feb_eparcel_express_ex_syd[row_index_max, col_index_max]
  return(charge_value)
}

output_d_2$charge_value_max_incgst <- mapply(extract_charge_value_max_incgst, output_d_2$row_index_max, output_d_2$col_index_max)


# Function to calculate charge based on charge_value_max_incgst and Per_Kg_#. Also does the calc if  weight_category_max == "Basic"
calculate_final_charge <- function(charge_value_max_incgst, weight_category_max, max_weight, row_index_max) {
  if (weight_category_max == "Basic") {
    per_kg_value <- cz_post_feb_eparcel_express_ex_syd[row_index_max, "Per_Kg"]
    return(charge_value_max_incgst + (per_kg_value * (ceiling(max_weight))))
  } else {
    return(charge_value_max_incgst)  
  }
}

output_d_2$base_charge_incgst <- mapply(calculate_final_charge, output_d_2$charge_value_max_incgst, output_d_2$weight_category_max, output_d_2$max_weight, output_d_2$row_index_max)

#### Express Post Parcels (BYO up to 5kg) ####
# to be tested when I can bring this in
#output_e <- subset(output_a, uplift_service %in% c("EPP_fivekg"))

#output_e$base_charge_incgst <- ifelse(output_e$DESCRIPTION == "Express Post Parcels (BYO up to 5kg)",
#                                                ex_pp_byo_up_to_5kg,
#                                                NA)

#### eparcel return to sender, Express Post eparcel returns, eParcel Post Return (Reg)  ####
# Function to subset data based on uplift_service and perform operations
subset_and_operate <- function(data, service, fee) {
  subset_data <- subset(data, uplift_service %in% service)
  if (nrow(subset_data) > 0) {
    subset_data$row_index_max <- NA
    subset_data$col_index_max <- NA
    subset_data$charge_value_max_incgst <- NA
    subset_data$base_charge_incgst <- fee
    return(subset_data)
  } else {
    return(NULL)
  }
}

# eparcel return to sender
output_f <- subset_and_operate(output_a, "ep_return_to_sender", ep_return_to_sender_fee)

# Express Post eparcel returns
output_g <- subset_and_operate(output_a, "exp_eparcel_returns", exp_eparcel_returns_fee)

# eParcel Post Return (Reg)
output_h <- subset_and_operate(output_a, c("reg_eparcel_returns", "reg_ep_call_for_return"), reg_eparcel_returns_fee)




#### combine all DFs together ######
output_all_services  <- rbind(output_a_2, output_b_2, output_c_2, output_d_2, output_f, output_g, output_h)

write.csv(output_all_services, file = "output_all_services.csv")

#### additional mapping ####
# get charge_value_exgst 

output_all_services$base_charge_exgst <- (output_all_services$base_charge_incgst /110) *100

# calculate fuel surcharge based on ex gst
output_all_services$fuel_surcharge <- output_all_services$base_charge_exgst * fuel_surcharge_pct
output_all_services$fuel_gst <- output_all_services$fuel_surcharge  * gst

# calculate the Fuel surcharge tax free 
#output_all_services$fuel_surcharge_tax_free  <-  ifelse(output_all_services$is_gst_free == "Yes",
#                                                ((((output_all_services$fuel_surcharge/110)*100)/110) * 100), 
#                                                NA)

# calculate security management fee
output_all_services$sec_mng_chrg <- ifelse(output_all_services$DESCRIPTION == "Express Post with Signature",
                                           output_all_services$base_charge_exgst * sec_mng_chrg_pct,
                                           NA)
output_all_services$sec_mng_gst <- output_all_services$sec_mng_chrg * gst



##### multiply by customer uplift   ####
# first step is to find the indices 

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

# Initialize vectors to store results
col_index_uplift <- numeric(nrow(output_all_services))
row_index_uplift <- numeric(nrow(output_all_services))

# Iterate over each row
for (i in 1:nrow(output_all_services)) {
  if (output_all_services$DESCRIPTION[i] %in% c("Parcel Post with Signature", "Express Post with Signature")) {
    # For rows with specified DESCRIPTIONs, find column and row indices
    col_name_uplift <- as.character(output_all_services$uplift_service[i])
    col_index_uplift[i] <- which(colnames(customer_uplift_march_24) == col_name_uplift)
    
    row_name_uplift <- as.character(output_all_services$customer_code[i])
    row_index <- which(rownames(customer_uplift_march_24) == row_name_uplift)
    
    # Check if the row index exists, otherwise assign NA
    if (length(row_index) == 0) {
      row_index_uplift[i] <- NA
    } else {
      row_index_uplift[i] <- row_index
    }
  } else {
    # For other rows, assign NA to indices
    col_index_uplift[i] <- NA
    row_index_uplift[i] <- NA
  }
}



############

output_all_services_2 <-cbind(output_all_services, (cbind(row_index_uplift, col_index_uplift)))

write.csv(output_all_services_2, file = "output_all_services_2.csv")

# query uplift sheet to find uplift % ####

# Function to extract values from charge zone dataset based on indices
#extract_charge_value_uplift<- function(row_index_uplift, col_index_uplift) {
#  charge_value <- customer_uplift_march_24[row_index_uplift, col_index_uplift]
 # return(charge_value)
#}
###########################
extract_charge_value_uplift <- function(row_index_uplift, col_index_uplift) {
  if (is.na(row_index_uplift) || is.na(col_index_uplift)) {
    return(0)
  } else {
    charge_value <- customer_uplift_march_24[row_index_uplift, col_index_uplift]
    return(charge_value)
  }
}
#################################
output_all_services_2$charge_value_uplift <- mapply(extract_charge_value_uplift, output_all_services_2$row_index_uplift, output_all_services_2$col_index_uplift)

#write.csv(output_all_services_2, file = "output_all_services_2.csv")

#### multiply base by uplift ####
# Incgst Convert charge_value_uplift to numeric, handling NA values
output_all_services_2$charge_value_uplift_numeric_incgst <- ifelse(is.na(output_all_services_2$charge_value_uplift) | is.na(output_all_services_2$base_charge_exgst),
                                                 NA,
                                                 as.numeric(sub("%", "", output_all_services_2$charge_value_uplift)))
#exgst
output_all_services_2$charge_value_uplift_numeric_exgst <- ifelse(is.na(output_all_services_2$charge_value_uplift) | is.na(output_all_services_2$base_charge_exgst),
                                                            NA,
                                                            as.numeric(sub("%", "", output_all_services_2$charge_value_uplift)))

# Incgst Convert base_charge_exgst to numeric, handling NA values
#output_all_services_2$charge_value_max_incgst_numeric <- ifelse(is.na(output_all_services_2$charge_value_uplift) | is.na(output_all_services_2$base_charge_exgst),
#                                              NA,
#                                              as.numeric(gsub("[^0-9.]", "", output_all_services_2$base_charge_exgst)))
#exgst
output_all_services_2$charge_value_max_exgst_numeric <- ifelse(is.na(output_all_services_2$charge_value_uplift) | is.na(output_all_services_2$base_charge_exgst),
                                                                NA,
                                                                as.numeric(gsub("[^0-9.]", "", output_all_services_2$base_charge_exgst)))


# Incgst Calculate the percentage of base_charge_exgst, handling NA values
#output_all_services_2$uplift_figure_incgst <- ifelse(is.na(output_all_services_2$charge_value_uplift_numeric_incgst) | is.na(output_all_services_2$charge_value_max_incgst_numeric),
#                                   NA,
#                                   (output_all_services_2$charge_value_uplift_numeric_incgst / 100) * output_all_services_2$charge_value_max_incgst_numeric)
#exgst
output_all_services_2$uplift_figure_exgst <- ifelse(is.na(output_all_services_2$charge_value_uplift_numeric_exgst) | is.na(output_all_services_2$charge_value_max_exgst_numeric),
                                                     NA,
                                                     (output_all_services_2$charge_value_uplift_numeric_exgst / 100) * output_all_services_2$charge_value_max_exgst_numeric)

#  Incgst Filter out NA and non-numeric values before performing addition
#output_all_services_2$charge_to_custo_incgst <- ifelse(is.na(output_all_services_2$charge_value_max_incgst_numeric) | is.na(output_all_services_2$uplift_figure_incgst) | !is.numeric(output_all_services_2$charge_value_max_incgst_numeric) | !is.numeric(output_all_services_2$uplift_figure_incgst),
#                                     NA,
 #                                    output_all_services_2$charge_value_max_incgst_numeric + output_all_services_2$uplift_figure_incgst)
#exgst
output_all_services_2$charge_to_custo_exgst <- ifelse(is.na(output_all_services_2$charge_value_max_exgst_numeric) | is.na(output_all_services_2$uplift_figure_exgs) | !is.numeric(output_all_services_2$charge_value_max_exgst_numeric) | !is.numeric(output_all_services_2$uplift_figure_exgs),
                                                NA,
                                                output_all_services_2$charge_value_max_exgst_numeric + output_all_services_2$uplift_figure_exgs)


# update table name
output_all_services_2 <- output_all_services_2

##### write to CSV ####

write.csv(output_all_services_2, file = "output_all_services_2.csv")
