#clean enviornment
rm(list = ls())
# Source the first script

start_time <- Sys.time()

source("C:/Users/shaneb/Desktop/aus_repo_2/-aus_post/ingest_&_prep.R")


# Run the second script
source("C:/Users/shaneb/Desktop/aus_repo_2/-aus_post/basic_charges_combined.R")

source("C:/Users/shaneb/Desktop/aus_repo_2/-aus_post/basic_charge_and_input_merge_and_clean.R")

source("C:/Users/shaneb/Desktop/aus_repo_2/-aus_post/back billing aggregation.R")

end_time <- Sys.time()

# Calculate execution time
execution_time <- end_time - start_time
print(paste("Execution Time:", execution_time))

