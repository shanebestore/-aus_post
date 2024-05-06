#### All services are and charges are calculated in this script #####

#### Ingesting Data ####
# Changing name so  cleaniness purposes
bill_cut_a <-  bill_cut1


#### Cubic size ####

factor <- 250  # Change this to your desired factor
cubic_size <- bill_cut_a$BILLED.HEIGHT * bill_cut_a$BILLED.LENGTH * bill_cut_a$BILLED.WIDTH
bill_cut_a$cubic_size <- cubic_size
bill_cut_a$cubic_weight <- cubic_size * factor

#ex_pp_byo_up_to_5kg <-'Express Post Parcels (BYO up to 5kg)'
bill_cut_a <- mutate(bill_cut_a, 
                     max_weight = ifelse(service == 'International', 
                                         ifelse(BILLED.WEIGHT == 0, DECLARED.WEIGHT, BILLED.WEIGHT),
                                         ifelse(cubic_weight == 0 & BILLED.WEIGHT == 0, 
                                                DECLARED.WEIGHT, 
                                                pmax(cubic_weight, BILLED.WEIGHT))))


#### Declare the charges ####
#fuel charge_ex_gst has to be calculated from the exgst charge value
gst <- 0.1
fuel_surcharge_pct <- 0.08
sec_mng_chrg_pct <- 0.0435
ep_return_to_sender_fee <- 12.85 #same for both express and standard
exp_eparcel_returns_fee <- 28.45 
reg_eparcel_returns_fee <- 12.43
over_max_limits_fee <-100


#### over max limites fee
bill_cut_a$over_max_limits_fee <- ifelse(bill_cut_a$ACTUAL.WEIGHT > 22 | bill_cut_a$BILLED.LENGTH > 105 | bill_cut_a$cubic_size > 0.25, 100, NA)

#### Classifying weights ####
cz_max_weight <- bill_cut_a$max_weight

# Define the new categorisation function for "EPARCEL WINE STD" for VIC and NSW
categorize_weight_for_wine <- function(weight_kg) {
  categories <- sapply(weight_kg, function(w) {
    if (is.na(w)) {
      return("na")
    } else if (w == 0) {
      return("na")
    } else if (w >= 0.00001 & w <= 2) {
      return("Up_to_2kg")
    } else if (w <= 3) {
      return("X2.01kg_to_3kg")
    } else if (w <= 5) {
      return("X3.01kg_to_5kg")
    } else if (w <= 9) {
      return("X5.01kg_to_9kg")
    } else if (w <= 16) {
      return("X9.01kg_to_16kg")
    } else if (w <= 22) {
      return("X16.01kg_to_22kg")
    } else {
      return("Above_22kg_for_Wine")
    }
  })
  return(categories)
}

# Define the new categorisation function for "International " for VIC and NSW
categorize_weight_for_international <- function(weight_kg) {
  categories <- sapply(weight_kg, function(w) {
    if (is.na(w)) {
      return("na")
    } else if (w == 0) {
      return("na")
    } else if (w >= 0.00001 & w <= 0.5) {
      return("Up_to_500g")
    } else if (w <= 1) {
      return("X501g_to_1kg")
    } else if (w <= 2) {
      return("X1.01kg_to_2kg")
    } else if (w <= 20) {
      return("X2.01kg_to_20kg")
    } else {
      return("Above_20kg_for_international")
    }
  })
  return(categories)
}

# Define the new categorisation function for "Basic charge" for VIC and NSW
categorize_weight_for_basic <- function(weight_kg) {
  categories <- sapply(weight_kg, function(w) {
    if (is.na(w)) {
      return("na")
    } else if (w == 0) {
      return("na")
    } else if (w >= 0.00001 & w <= 0.5) {
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
      return("Basic")
    }
  })
  return(categories)
}


weight_category_max <- character(nrow(bill_cut_a))

# description here is better as we the weight categories are for the rates card, not the uplift service. 
for (i in 1:nrow(bill_cut_a)) {
  DESCRIPTION  <- bill_cut_a$DESCRIPTION [i]
  weight <- cz_max_weight[i]
  
  if (DESCRIPTION  %in% c("EPARCEL WINE STD")) {
    weight_category_max[i] <- categorize_weight_for_wine(weight)
  } else if (DESCRIPTION  %in% c("Express Courier International (eParcel)", "PACK AND TRACK INTERNATIONAL")) {
    weight_category_max[i] <- categorize_weight_for_international(weight)
  } else {
    weight_category_max[i] <- categorize_weight_for_basic(weight)
  }
}

output_a <- cbind(bill_cut_a, weight_category_max)

#### Calculate the base charge ####
#### Base charge for Regular.VIC ####

# cut the dataset down to correct uplift service
output_a1 <- subset(output_a, service %in% c("Regular.VIC"))

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

output_b1 <- subset(output_a, service %in% c("Express.VIC"))

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

#### Base charge for Regular.NSW  ####

output_c1 <- subset(output_a, service %in% c("Regular.NSW"))

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

#### Base charge for Express.NSW ####
output_d1 <- subset(output_a, service %in% c("Express.NSW"))

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

#### Base charge for Express Post Parcels (BYO up to 5kg) ####
# to be tested when I can bring this in
#output_e <- subset(output_a, service %in% c("EPP_fivekg"))

#output_e$base_charge_incgst <- ifelse(output_e$DESCRIPTION == "Express Post Parcels (BYO up to 5kg)",
#                                                ex_pp_byo_up_to_5kg,
#                                                NA)

#### Base charge for eparcel return to sender, Express Post eparcel returns, eParcel Post Return (Reg)  ####
# Function to subset data based on service and perform operations
subset_and_operate <- function(data, services, fee) {
  subset_data <- subset(data, service %in% services)
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

#### base charge for eparcel_wine.VIC ####

# cut the dataset down to correct uplift service
output_i1 <- subset(output_a, service %in% c("Wine.VIC"))

#Determine the indexes to use to query the new base charge zone sheet 

# find the row and column number to reference against z_c
col_name_max<- as.character(output_i1$weight_category_max)
col_index_max <- unlist(lapply(output_i1$weight_category_max, function(col_name_max) {
  which(colnames(cz_post_feb_eparcel_wine_ex_mel) == col_name_max)
}))

row_name_max<- as.character(output_i1$CHARGE.ZONE)
row_index_max <- unlist(lapply(row_name_max, function(row_name_max) {
  index <- which(rownames(cz_post_feb_eparcel_wine_ex_mel) == row_name_max)
  if (length(index) == 0) {
    NA
  } else {
    index
  }
}))

output_i_2 <-cbind(output_i1, (cbind(row_index_max, col_index_max)))

# query new base charge rate 
# Function to extract values from charge zone dataset based on indices

extract_charge_value_max_incgst <- function(row_index_max, col_index_max) {
  charge_value <- cz_post_feb_eparcel_wine_ex_mel[row_index_max, col_index_max]
  return(charge_value)
}

output_i_2$charge_value_max_incgst <- mapply(extract_charge_value_max_incgst, output_i_2$row_index_max, output_i_2$col_index_max)

output_i_2$base_charge_incgst <- output_i_2$charge_value_max_incgst

#### base charge for eparcel_wine.NSW #####

# cut the dataset down to correct uplift service
output_j1 <- subset(output_a, service %in% c("Wine.NSW"))

#Determine the indexes to use to query the new base charge zone sheet 

# find the row and column number to reference against z_c
col_name_max<- as.character(output_j1$weight_category_max)
col_index_max <- unlist(lapply(output_j1$weight_category_max, function(col_name_max) {
  which(colnames(cz_post_feb_eparcel_wine_ex_syd) == col_name_max)
}))

row_name_max<- as.character(output_j1$CHARGE.ZONE)
row_index_max <- unlist(lapply(row_name_max, function(row_name_max) {
  index <- which(rownames(cz_post_feb_eparcel_wine_ex_syd) == row_name_max)
  if (length(index) == 0) {
    NA
  } else {
    index
  }
}))


output_j_2 <-cbind(output_j1, (cbind(row_index_max, col_index_max)))


# query new base charge rate 
# Function to extract values from charge zone dataset based on indices

extract_charge_value_max_incgst <- function(row_index_max, col_index_max) {
  charge_value <- cz_post_feb_eparcel_wine_ex_syd[row_index_max, col_index_max]
  return(charge_value)
}

output_j_2$charge_value_max_incgst <- mapply(extract_charge_value_max_incgst, output_j_2$row_index_max, output_j_2$col_index_max)

output_j_2$base_charge_incgst <- output_j_2$charge_value_max_incgst

#### Base charge for PACK AND TRACK INTERNATIONAL ####

# using the description here
output_l1 <- subset(output_a, DESCRIPTION  %in% c("PACK AND TRACK INTERNATIONAL"))

#Determine the indexes to use to query the new base charge zone sheet 

# find the row and column number to reference against z_c
col_name_max<- as.character(output_l1$weight_category_max)
col_index_max <- unlist(lapply(output_l1$weight_category_max, function(col_name_max) {
  which(colnames(cz_post_feb_eparcel_international_standard) == col_name_max)
  
}))

row_name_max<- as.character(output_l1$CHARGE.ZONE)
row_index_max <- unlist(lapply(row_name_max, function(row_name_max) {
  index <- which(rownames(cz_post_feb_eparcel_international_standard) == row_name_max)
  if (length(index) == 0) {
    NA
  } else {
    index
  }
}))

output_l_2 <-cbind(output_l1, (cbind(row_index_max, col_index_max)))
sapply(output_l_2, class)

# query new base charge rate 
# Function to extract values from charge zone dataset based on indices

extract_charge_value_max_incgst <- function(row_index_max, col_index_max) {
  charge_value <- cz_post_feb_eparcel_international_standard[row_index_max, col_index_max]
  return(as.numeric(charge_value))
}

output_l_2$charge_value_max_incgst <- mapply(extract_charge_value_max_incgst, output_l_2$row_index_max, output_l_2$col_index_max)

#sapply(output_l_2, class)

# Function to calculate charge based on charge_value_max_incgst and Per_Kg_#. Also does the calc if  weight_category_max == "X2.01kg_to_20kg"
#calculate_final_charge <- function(charge_value_max_incgst, weight_category_max, max_weight, row_index_max) {
#  if (weight_category_max == "X2.01kg_to_20kg") {
#    per_kg_value <- cz_post_feb_eparcel_international_standard[row_index_max, "Per_Kg_2"]
#   return(charge_value_max_incgst + (per_kg_value * (max_weight)))
#  } else {
#    return(charge_value_max_incgst)  
#  }
#}

calculate_final_charge <- function(charge_value_max_incgst, weight_category_max, max_weight, row_index_max) {
  
  if (weight_category_max == "Above_20kg_for_international") {
    return(0)
  } 
  else if (weight_category_max == "X2.01kg_to_20kg") {
    per_kg_value <- cz_post_feb_eparcel_international_standard[row_index_max, "Per_Kg_2"]
    return(charge_value_max_incgst + (per_kg_value * max_weight))
  } 
  else if (weight_category_max %in% c("Up_to_500g", "X501g_to_1kg", "X1.01kg_to_2kg")) {
    per_kg_value <- cz_post_feb_eparcel_international_standard[row_index_max, "Per_Kg_1"]
    return(charge_value_max_incgst + (per_kg_value * max_weight))
  } 
  else {
    return(charge_value_max_incgst) 
  }
}


output_l_2$base_charge_incgst <- mapply(calculate_final_charge, output_l_2$charge_value_max_incgst, output_l_2$weight_category_max, output_l_2$max_weight, output_l_2$row_index_max)
sapply(output_l_2, class)

#### Base charge fo Express Courier International (eParcel) ####
# using the description here
output_k1 <- subset(output_a, DESCRIPTION  %in% c("Express Courier International (eParcel)"))

#Determine the indexes to use to query the new base charge zone sheet 

# find the row and column number to reference against z_c
col_name_max<- as.character(output_k1$weight_category_max)
col_index_max <- unlist(lapply(output_k1$weight_category_max, function(col_name_max) {
  which(colnames(cz_post_feb_eparcel_international_express_merch) == col_name_max)
}))

row_name_max<- as.character(output_k1$CHARGE.ZONE)
row_index_max <- unlist(lapply(row_name_max, function(row_name_max) {
  index <- which(rownames(cz_post_feb_eparcel_international_express_merch) == row_name_max)
  if (length(index) == 0) {
    NA
  } else {
    index
  }
}))

output_k_2 <-cbind(output_k1, (cbind(row_index_max, col_index_max)))
sapply(output_k_2, class)

# query new base charge rate 
# Function to extract values from charge zone dataset based on indices

extract_charge_value_max_incgst <- function(row_index_max, col_index_max) {
  charge_value <- cz_post_feb_eparcel_international_express_merch[row_index_max, col_index_max]
  return(charge_value)
}

output_k_2$charge_value_max_incgst <- mapply(extract_charge_value_max_incgst, output_k_2$row_index_max, output_k_2$col_index_max)

# Function to calculate charge based on charge_value_max_incgst and Per_Kg_#. Also does the calc if  weight_category_max == "X2.01kg_to_20kg"
calculate_final_charge <- function(charge_value_max_incgst, weight_category_max, max_weight, row_index_max) {
  if (weight_category_max == "X2.01kg_to_20kg") {
    per_kg_value <- cz_post_feb_eparcel_international_express_merch[row_index_max, "Per_Kg"]
    return(charge_value_max_incgst + (per_kg_value * (max_weight)))
  } else {
    return(charge_value_max_incgst)  
  }
}

output_k_2$base_charge_incgst <- mapply(calculate_final_charge, output_k_2$charge_value_max_incgst, output_k_2$weight_category_max, output_k_2$max_weight, output_k_2$row_index_max)

#### combine all DFs together ######
output_all_services  <- rbind(output_a_2, output_b_2, output_c_2, output_d_2, output_f, output_g, output_h, output_i_2, output_j_2, output_l_2, output_k_2)

#write.csv(output_all_services, file = "output_all_services.csv")

#### additional mapping ####
# 1.50 mark up for wine tbc

#output_all_services <- output_all_services %>%
#  arrange(CONSIGNMENT.ID, TO.ADDRESS) %>%
# group_by(CONSIGNMENT.ID, TO.ADDRESS) %>%
# mutate(
#    new_base_charge_incgst = base_charge_incgst - 1.50 * (row_number() - 1)
#  ) %>%
#  ungroup()


output_all_services$base_charge_exgst <- ifelse(output_all_services$is_gst_free_zone == 'No', 
                                                (output_all_services$base_charge_incgst/ 110) * 100, 
                                                output_all_services$base_charge_incgst)


# find the tax amount
output_all_services$base_charge_tax <- output_all_services$base_charge_incgst - output_all_services$base_charge_exgst 


# calculate fuel surcharge based on ex gst
# Calculate fuel surcharge only for non-International entries
output_all_services$fuel_surcharge <- ifelse(output_all_services$uplift != "International",
                                             output_all_services$base_charge_exgst * fuel_surcharge_pct,
                                             0)

# Calculate fuel GST based on fuel surcharge
output_all_services$fuel_gst <- ifelse(output_all_services$fuel_surcharge != 0,
                                       output_all_services$fuel_surcharge * gst,
                                       0)


# calculate security management fee
output_all_services$sec_mng_chrg <- ifelse(output_all_services$DESCRIPTION == "Express Post with Signature",
                                           output_all_services$base_charge_exgst * sec_mng_chrg_pct,
                                           NA)
output_all_services$sec_mng_gst <- output_all_services$sec_mng_chrg * gst




##### multiply by customer uplift   ####
# first step is to find the indices 

# Initialize vectors to store results
col_index_uplift <- numeric(nrow(output_all_services))
row_index_uplift <- numeric(nrow(output_all_services))

# Iterate over each row
for (i in 1:nrow(output_all_services)) {
  # For all rows, find column and row indices
  col_name_uplift <- as.character(output_all_services$uplift[i])
  col_index_uplift[i] <- which(colnames(customer_uplift_march_24) == col_name_uplift)
  
  row_name_uplift <- as.character(output_all_services$customer_code2[i])
  row_index <- which(rownames(customer_uplift_march_24) == row_name_uplift)
  
  # Check if the row index exists, otherwise assign NA
  if (length(row_index) == 0) {
    row_index_uplift[i] <- NA
  } else {
    row_index_uplift[i] <- row_index
  }
}

output_all_services_2 <-cbind(output_all_services, (cbind(row_index_uplift, col_index_uplift)))

# query to find uplift
extract_charge_value_uplift <- function(row_index_uplift, col_index_uplift) {
  if (is.na(row_index_uplift) || is.na(col_index_uplift)) {
    return(0)
  } else {
    charge_value <- customer_uplift_march_24[row_index_uplift, col_index_uplift]
    
    # Check if charge_value is blank or NA, if so, return 0
    if (is.na(charge_value) || charge_value == "") {
      return(0)
    } else {
      return(charge_value)
    }
  }
}

output_all_services_2$charge_value_uplift <- mapply(extract_charge_value_uplift, output_all_services_2$row_index_uplift, output_all_services_2$col_index_uplift)

#### warning cols taken here
# Col to highlight if we are missing weight information or custo has no uplift
# Create a new column 'warnings' in output_all_services_2
output_all_services_2$warnings <- NA

# Condition 1: If charge_value_uplift is blank, NA, or 0
output_all_services_2$warnings <- ifelse(is.na(output_all_services_2$charge_value_uplift) | 
                                           output_all_services_2$charge_value_uplift == 0 |
                                           output_all_services_2$charge_value_uplift == "",
                                         "No uplift found. ",
                                         output_all_services_2$warnings)

# Condition 2: If service == 'International' and BILLED.WEIGHT == 0
output_all_services_2$warnings <- ifelse(output_all_services_2$service == 'International' & 
                                           output_all_services_2$BILLED.WEIGHT == 0,
                                         paste(output_all_services_2$warnings, "Declared weight used. "),
                                         output_all_services_2$warnings)

# Condition 3: If service is not one of the specified values and cubic_weight == 0 & BILLED.WEIGHT == 0
output_all_services_2$warnings <- ifelse(!(output_all_services_2$service %in% c('International', 
                                                                                'reg_ep_call_for_return', 
                                                                                'ep_return_to_sender', 
                                                                                'reg_eparcel_returns')) & 
                                           output_all_services_2$cubic_weight == 0 & 
                                           output_all_services_2$BILLED.WEIGHT == 0,
                                         paste(output_all_services_2$warnings, "Declared weight used. "),
                                         output_all_services_2$warnings)

# Condition 4: If weight_category_max is NA or 0
output_all_services_2$warnings <- ifelse(is.na(output_all_services_2$weight_category_max) | 
                                           output_all_services_2$weight_category_max == 0,
                                         paste(output_all_services_2$warnings, "no weight detected so 0 charge applied"),
                                         output_all_services_2$warnings)

# Condition 5: If weight_category_max is "Above_22kg_for_Wine"
output_all_services_2$warnings <- ifelse(output_all_services_2$weight_category_max == "Above_22kg_for_Wine",
                                         paste(output_all_services_2$warnings, "Over 22kg for wine. "),
                                         output_all_services_2$warnings)

# Condition 6: If CHARGE.ZONE is blank or NA
output_all_services_2$warnings <- ifelse(is.na(output_all_services_2$CHARGE.ZONE) | 
                                           output_all_services_2$CHARGE.ZONE == "",
                                         paste(output_all_services_2$warnings, "No charge zone detected. "),
                                         output_all_services_2$warnings)

#output_all_services_2$warnings <- ifelse(is.na(output_all_services_2$corresponding_value) | 
#                                           output_all_services_2$corresponding_value == "custo code not found",
#                                         "custo code not found",
#                                         output_all_services_2$warnings)

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
#exgst
output_all_services_2$charge_value_max_exgst_numeric <- ifelse(is.na(output_all_services_2$charge_value_uplift) | is.na(output_all_services_2$base_charge_exgst),
                                                               NA,
                                                               as.numeric(gsub("[^0-9.]", "", output_all_services_2$base_charge_exgst)))

# Incgst Calculate the percentage of base_charge_exgst, handling NA values
#exgst
output_all_services_2$uplift_figure_exgst <- ifelse(is.na(output_all_services_2$charge_value_uplift_numeric_exgst) | is.na(output_all_services_2$charge_value_max_exgst_numeric),
                                                    NA,
                                                    (output_all_services_2$charge_value_uplift_numeric_exgst / 100) * output_all_services_2$charge_value_max_exgst_numeric)

#  Incgst Filter out NA and non-numeric values before performing addition
#exgst
output_all_services_2$charge_to_custo_exgst <- ifelse(is.na(output_all_services_2$charge_value_max_exgst_numeric) | is.na(output_all_services_2$uplift_figure_exgst) | !is.numeric(output_all_services_2$charge_value_max_exgst_numeric) | !is.numeric(output_all_services_2$uplift_figure_exgst),
                                                      NA,
                                                      output_all_services_2$charge_value_max_exgst_numeric + output_all_services_2$uplift_figure_exgst)

# update table name
output_all_services_2 <- output_all_services_2

#output_all_services_2 <- subset(output_all_services_2 , ARTICLE.ID %in% c('ET236199765AU'))
##### write to CSV ####

#write.csv(output_all_services_2, file = "output_all_services_2.csv")

