start_time <- Sys.time()
# Source the first script
source("C:/Users/shaneb/Desktop/aus_repo/Aus_post_repo/ingest_&_prep.R")

# Run the second script
source("C:/Users/shaneb/Desktop/aus_repo/Aus_post_repo/basic_charges_combined.R")

source("C:/Users/shaneb/Desktop/aus_repo/Aus_post_repo/basic_charge_and_input_merge_and_clean.R")

end_time <- Sys.time()

# Calculate execution time
execution_time <- end_time - start_time
print(paste("Execution Time:", execution_time, "seconds"))

