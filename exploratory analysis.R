#### additional_processing ####

##### denoter of whats heavier ####
#cubic_weight_is_heavier <- output_a_3$cubic_weight > output_a_3$BILLED.WEIGHT
#output_a_3 <- cbind(output_a_3,  cubic_weight_is_heavier)


#### check which are heavier ####
#output_a_3$charge_value_cubic_is_heavier <- output_a_3$charge_value_cubic > output_a_3$charge_value_billed
#output_a_3 <- cbind(output_a_3,  charge_value_cubic_is_heavier)

##### checking for differences #####
#output_a_3$charge_value_max <- as.numeric(gsub("[^0-9.-]", "", output_a_3$charge_value_max))
#output_a_3$AVG..UNIT.PRICE <- as.numeric(gsub("[^0-9.-]", "", output_a_3$AVG..UNIT.PRICE))

# Step 2: Create a new column indicating difference
#output_a_3$diff <- ifelse(output_a_3$AVG..UNIT.PRICE != output_a_3$charge_value_max, "yes", "no")

# Step 3: Count occurrences of "yes" and "no"
#count_diff <- table(output_a_3$diff)
#print(count_diff)












count_unique <- function(column) {
  length(unique(column))
}

# Apply the function to all columns
result <- sapply(bill , count_unique)

print(result)

####################### for production purposes only
write.csv(output, file = "output.csv")

bill_cut2
write.csv(bill_cut2, file = "bill_cut2.csv")


count_values <- function(column_data) {
  # Using table() to count occurrences of each unique value
  counts <- table(column_data)
  
  # Return the counts
  return(counts)
}

# Call the function with the column data
result <- count_values(bill_cut2$DESCRIPTION)

# Print the result
print(result)

class(output_2$BILLING.DOC)
class(output_2$BILLING.WEIGHT)
class(output_2$CHARGE.ZONE)
class(output$FROM.STATE)
class(output$weight_category)
class(output_2$row_index)
class(output_2$col_index)
class(output_2$charge_value)


class(row_index)



#######################################

0.58*0.36*0.16*250





