#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#precurser files
#rsconnect::setAccountInfo(name='estorelogistics',
#                          token='DEDD479AA07AC545E656A5C9015EB3BB',
#                          secret='RfE1CzWLqUdudUMr2Te+H9c069453mXDXKxw1KsL')

#library(rsconnect)


#rsconnect::deployApp('C:\\Users\\shaneb\\Desktop\\aus_repo_2\\-aus_post\\app1505')
knitr::opts_chunk$set(echo = TRUE)
#install.packages("dplyr")
#library(dplyr)

#install.packages("zip")
library(zip)

#install.packages("dplyr")
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Input Form"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csv_file", "Upload CSV File", accept = ".csv"),
      textInput("fuel_surcharge", "Insert Fuel Surcharge % for the month (e.g., 7.7) "),
      textInput("force_majeure_fee", "Insert Force Majeure Fee. Insert 0 if there is none"),
      textInput("peak_fee", "Insert Peak Fee. Insert 0 if there is none"),
 #     textInput("folder_path", "Insert Folder Path to Save CSV"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      textOutput("feedback_message"), # Display feedback message
      verbatimTextOutput("custom_output"), # Adding custom output element
      downloadButton("download_result1", "Download billing_doc_output"),
      downloadButton("download_result2", "Download ap_post_supply"),
      downloadButton("download_result3", "Download ap_post_supply per customer"),
      downloadButton("download_result4", "Download summary_by_custo_&_description"),
      downloadButton("download_result5", "Download summary_by_description")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Increase maximum file size limit to 1 GB
  options(shiny.maxRequestSize = 1024*1024*1024) # 1 GB in bytes
  
  # Define a reactive expression to store user inputs
  user_data <- reactive({
    list(
      fuel_surcharge = as.numeric(input$fuel_surcharge),
      force_majeure_fee = as.numeric(input$force_majeure_fee),
      peak_fee = as.numeric(input$peak_fee),
      csv_file = input$csv_file
    )
  })
  
  # Define an observer to trigger custom script after user input and summary rendering
  observeEvent(input$submit, {
    user_inputs <- user_data()
    
    # Check if fields are filled
    if (any(sapply(user_inputs[c("fuel_surcharge", "force_majeure_fee", "peak_fee")], is.null))) {
      output$custom_output <- renderPrint({
        cat("Error: Please fill in all fields.\n")
      })
      return()
    }
    
    # Check if numeric values are entered
    if (any(sapply(user_inputs[c("fuel_surcharge", "force_majeure_fee", "peak_fee")], function(x) !is.numeric(x)))) {
      output$custom_output <- renderPrint({
        cat("Error: Please enter numeric values for the fuel surcharge, force majeure fee, and peak fee.\n")
      })
      return()
    }
    
    # Check if CSV is uploaded
    if (is.null(user_inputs$csv_file)) {
      output$custom_output <- renderPrint({
        cat("Error: Please upload a CSV file.\n")
      })
      return()
    }
    
    # Feedback message
    output$feedback_message <- renderText({
      "Process initiated."
    })
    
    # Check if fuel surcharge is numeric
    if (!is.numeric(user_inputs$fuel_surcharge)) {
      output$custom_output <- renderPrint({
        cat("Error: Fuel surcharge must be a numeric value.\n")
      })
      return()
    }
    
    # Read the uploaded CSV file
    tryCatch({
      bill <- read.csv(user_inputs$csv_file$datapath, head=TRUE, sep=",")
    }, error = function(e) {
      output$custom_output <- renderPrint({
        cat("Error: Unable to read the uploaded CSV file.\n")
      })
      return()
    })
    
    # Check if 'QTY' column exists in the CSV
    if (!("QTY" %in% colnames(bill))) {
      output$custom_output <- renderPrint({
        cat("Error: The uploaded CSV does not contain a 'QTY' column.\n")
      })
      return()
    }
    
    # Check if fuel surcharge is NA
    if (is.na(user_inputs$fuel_surcharge)) {
      output$custom_output <- renderPrint({
        cat("Error: Fuel surcharge must be a numeric value.\n")
      })
      return()
    }
    
########################################
    
    #### 1.c billing date extracted for title generation 
    # Get the min and max dates the bill covers
    bill$BILLING.DATE <- as.Date(as.character(bill$BILLING.DATE), format = "%Y%m%d")
    min_date <- min(bill$BILLING.DATE, na.rm = TRUE)
    max_date <- max(bill$BILLING.DATE, na.rm = TRUE)
    
    predefined_text <- paste( format(min_date, "%Y-%m-%d"), "to", format(max_date, "%Y-%m-%d"))
    
    # pre feb base rates. Left in for pulling comparison calcs
    #cz_pre_feb_eparcel_regular_ex_mel = read.csv("reference_data/cz_pre_feb_eparcel_regular_ex_mel.csv", head=TRUE, row.names = 1,  sep=",")
    #cz_pre_feb_eparcel_express_ex_mel = read.csv("reference_data/cz_pre_feb_eparcel_express_ex_mel.csv", head=TRUE, row.names = 1,  sep=",")
    #cz_pre_feb_eparcel_regular_ex_syd = read.csv("reference_data/cz_pre_feb_eparcel_regular_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")
    #cz_pre_feb_eparcel_express_ex_syd = read.csv("reference_data/cz_pre_feb_eparcel_express_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")
    
    #### 1.d load the reference data
    #this could be changed to extract some a shared drive
    
    cz_post_feb_eparcel_regular_ex_mel = read.csv("reference_data/cz_post_feb_eparcel_regular_ex_mel.csv", head=TRUE, row.names = 1,  sep=",")
    cz_post_feb_eparcel_express_ex_mel = read.csv("reference_data/cz_post_feb_eparcel_express_ex_mel.csv", head=TRUE, row.names = 1,  sep=",")
    cz_post_feb_eparcel_regular_ex_syd = read.csv("reference_data/cz_post_feb_eparcel_regular_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")
    cz_post_feb_eparcel_express_ex_syd = read.csv("reference_data/cz_post_feb_eparcel_express_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")
    cz_post_feb_eparcel_express_ex_syd = read.csv("reference_data/cz_post_feb_eparcel_express_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")
    cz_post_feb_eparcel_wine_ex_mel = read.csv("reference_data/cz_post_feb_eparcel_wine_ex_mel.csv", head=TRUE, row.names = 1,  sep=",")
    cz_post_feb_eparcel_wine_ex_syd = read.csv("reference_data/cz_post_feb_eparcel_wine_ex_syd.csv", head=TRUE, row.names = 1,  sep=",")
    cz_post_feb_eparcel_international_express_merch = read.csv("reference_data/cz_post_feb_eparcel_international_express_merch.csv", head=TRUE, row.names = 1,  sep=",")
    cz_post_feb_eparcel_international_standard = read.csv("reference_data/cz_post_feb_eparcel_international_standard.csv", head=TRUE, row.names = 1,  sep=",")
    
    # custo mark up
    customer_uplift_march_24 = read.csv("reference_data/customer_uplift_march_24.csv", head=TRUE, row.names = 1,  sep=",")
    #custo codes
    estore_custo_codes = read.csv("reference_data/estore_custo_codes.csv", head=TRUE, sep=",")
    
    
    #### 1.e customer code ----
    # Function to extract letters before the first "-"
    extract_letters <- function(text) {
      split_text <- strsplit(as.character(text), " ")[[1]]
      return(trimws(split_text[1]))  
    }
    
    # Create a new column in bill to store the corresponding values from DF2
    bill$customer_code <- NA
    
    # Loop through each row in bill
    for (i in 1:nrow(bill)) {
      # Get the trading name from bill$NAME_1
      trading_name <- bill$NAME_1[i]
      
      # Find the corresponding row index in DF2 where trading_name matches
      match_index <- which(estore_custo_codes$trading_name == trading_name)
      
      # If a match is found, assign the corresponding value from DF2 to bill
      if (length(match_index) > 0) {
        bill$customer_code[i] <- estore_custo_codes$eStore_code[match_index]
      } else {
        # Handle cases where no match is found
        # Extract letters before the "-" in bill$NAME_1
        letters_before_dash <- substr(trading_name, 1, regexpr("-", trading_name) - 1)
        bill$customer_code[i] <- ifelse(letters_before_dash != "", letters_before_dash, "custo code not found")
      }
    }
    
    # Apply the extract_letters function to customer_code where necessary
    bill$customer_code2 <- sapply(bill$customer_code, function(code) {
      if (code == "custo code not found") {
        extract_letters(bill$NAME_1[i])
      } else {
        extract_letters(code)
      }
    })
    
    #### 1.f remove the summary lines and cut the dataset down ----
    bill_cut1 <- bill[!grepl("charge|surcharge|admin|fuel", bill$DESCRIPTION, ignore.case = TRUE), ]
    #bill_cut1 <- bill
    
    #cutting the dataset down to just the metrics we need for ALL of the basic calculations
    bill_cut1 <-  bill_cut1[,  c("REGION", "RECEIVING.COUNTRY", "customer_code", "customer_code2", "CUSTOMER", "NAME_1", "NAME_2", "NAME_3", "DESCRIPTION", "BILLING.DOC", "SERVICE.DATE", "TO.ADDRESS", "CONSIGNMENT.ID", "ARTICLE.ID",   
                                 "BILLED.LENGTH", "BILLED.WIDTH", "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "ACTUAL.WEIGHT", "CHARGE.ZONE", "FROM.STATE", "AVG..UNIT.PRICE" ,"AMOUNT.INCL.TAX", "AMOUNT.EXCL.TAX", "DECLARED.WEIGHT")] 
    
    #### 1.g create the service column from the description to reference against rate cards
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
                                                                                                                    ifelse(bill_cut1$DESCRIPTION == "APGL NZ Express w/Signature", "APGL",
                                                                                                                           ifelse(bill_cut1$DESCRIPTION %in% c("On Demand Tonight", "On Demand Afternoon"), "OnDemand",
                                                                                                                                  NA)))))))))))))))
    
    #### 1.h create the uplift column from the description to reference against uplift card ----
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
    
    # Additional Conditions (no new rates for these)
    bill_cut1$uplift <- ifelse(bill_cut1$DESCRIPTION %in% c("On Demand Tonight", "On Demand Afternoon"), "OnDemand", bill_cut1$uplift)
    bill_cut1$uplift <- ifelse(bill_cut1$DESCRIPTION == "APGL NZ Express w/Signature", "APGL", bill_cut1$uplift)
    
    
    #### 1.i create a col to determine if its GST free ----
    
    # the NZ here represents all og APGL
    is_gst_free <- function(zone) {
      ifelse(zone %in% c("NF", "W4", "AAT", "Z1", "Z2", "Z3", "Z4", "Z5", "Z6", "Z7", "Z8", "Z9", "NZ"), "Yes", "No")
    }
    
    # Apply the function to create the new column
    bill_cut1$is_gst_free_zone <- is_gst_free(bill_cut1$CHARGE.ZONE)
    
    ###### Section 2. Base charges and uplift applied ####
    #All services are and charges are calculated in this script #####
    
    #### 2.a Change variable name ----
    # Changing name so  cleaniness purposes
    bill_cut_a <-  bill_cut1
    
    #### 2.b calculate Cubic size ----
    factor <- 250  # Change this to your desired factor
    cubic_size <- bill_cut_a$BILLED.HEIGHT * bill_cut_a$BILLED.LENGTH * bill_cut_a$BILLED.WIDTH
    bill_cut_a$cubic_size <- cubic_size
    bill_cut_a$cubic_weight <- cubic_size * factor
    
    bill_cut_a <- mutate(bill_cut_a, 
                         max_weight = ifelse(service == 'International', 
                                             ifelse(BILLED.WEIGHT == 0, DECLARED.WEIGHT, BILLED.WEIGHT),
                                             ifelse(cubic_weight == 0 & BILLED.WEIGHT == 0, 
                                                    DECLARED.WEIGHT, 
                                                    pmax(cubic_weight, BILLED.WEIGHT))))
    
    
    #### 2.c declare the charges (user inputs) ----
    #fuel charge_ex_gst has to be calculated from the exgst charge value
    gst <- 0.1
    fuel_surcharge_pct <- (as.numeric(input$fuel_surcharge)/100)  #### 1/1 of uder inputs
    #fuel_surcharge_pct <- 0.097  # this has to be changed manually
    sec_mng_chrg_pct <- 0.0435
    ep_return_to_sender_fee <- 12.85 #same for both express and standard
    exp_eparcel_returns_fee <- 28.45 
    reg_eparcel_returns_fee <- 12.43
    over_max_limits_fee <-100
    
    #### 2.d over max limits fee ----
    bill_cut_a$over_max_limits_fee <- ifelse(bill_cut_a$service != 'International' & (bill_cut_a$ACTUAL.WEIGHT > 22 | bill_cut_a$BILLED.LENGTH > 105 | bill_cut_a$cubic_size > 0.25), 100, NA)
    
    #### 2.e weight classification  ----
    # take just the max weight
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
    
    # apply the functions based on description
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
    
    #### 2.f Calculate the base charges from the rate card ----
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
    
    
    
    # Function to calculate charge based on charge_value_max_incgst and Per_Kg_#. 
    
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
    
    #### the services we are not changing namely "APGL NZ Express w/Signature", "On Demand Tonight", "On Demand Afternoon"----
    
    #, "Express Post Parcels (BYO up to 5kg)"
    subset_and_operate <- function(data) {
      subset_data <- subset(data, DESCRIPTION %in% c("APGL NZ Express w/Signature", "On Demand Tonight", "On Demand Afternoon")) 
      if (nrow(subset_data) > 0) {
        subset_data$row_index_max <- NA
        subset_data$col_index_max <- NA
        subset_data$charge_value_max_incgst <- NA
        subset_data$base_charge_incgst <- subset_data$AMOUNT.INCL.TAX
        return(subset_data)
      } else {
        return(NULL)
      }
    }
    
    # Applying the function to output_a
    output_m <- subset_and_operate(output_a)
    
    
    #### 2.g combine all DFs together ----
    output_all_services  <- rbind(output_a_2, output_b_2, output_c_2, output_d_2, output_f, output_g, output_h, output_i_2, output_j_2, output_l_2, output_k_2, output_m)
    
    # write to ourput fortesting purposes
    #write.csv(output_all_services, file = "output_all_services.csv")
    
    
    #### 2.h wine mark up (WIP) ----
    # 1.50 mark up for wine tbc
    
    #output_all_services <- output_all_services %>%
    #  arrange(CONSIGNMENT.ID, TO.ADDRESS) %>%
    # group_by(CONSIGNMENT.ID, TO.ADDRESS) %>%
    # mutate(
    #    new_base_charge_incgst = base_charge_incgst - 1.50 * (row_number() - 1)
    #  ) %>%
    #  ungroup()
    
    #### 2.i GST removal, fuel surcharge calculation and security mng calculation ----
    output_all_services$base_charge_exgst <- ifelse(output_all_services$is_gst_free_zone == 'No', 
                                                    (output_all_services$base_charge_incgst/ 110) * 100, 
                                                    output_all_services$base_charge_incgst)
    
    # find the tax amount
    output_all_services$base_charge_tax <- output_all_services$base_charge_incgst - output_all_services$base_charge_exgst 
    
    # Calculate fuel surcharge only for non-International entries
    output_all_services$fuel_surcharge <- ifelse(!(output_all_services$uplift %in% c("International", "APGL", "OnDemand")),
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
    
    
    
    
    #### 2.j index and customer uplift ----
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
    
    # left in for testing purposes
    #write.csv(output_all_services_2, file = "output_all_services.csv")
    
    #### 2.k warning column ----
    # Col to highlight if we are missing weight information or custo has no uplift
    
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
    
    #might need to cut this
    #output_all_services_2$warnings <- ifelse(is.na(output_all_services_2$corresponding_value) | 
    #                                           output_all_services_2$corresponding_value == "custo code not found",
    #                                         "custo code not found",
    #                                         output_all_services_2$warnings)
    
    #### 2.l multiply base by the uplift figure ----
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
    
    
    #### 2.m calculate fuel surcharge ----
    # calculate fuel surcharge based on mark up
    # Calculate fuel surcharge only for non-International entries
    output_all_services_2$fuel_surcharge_uplifted <- ifelse(!(output_all_services_2$uplift %in% c("International", "APGL", "OnDemand")),
                                                            output_all_services_2$charge_to_custo_exgst * fuel_surcharge_pct,
                                                            0)
    
    # Calculate fuel GST based on fuel surcharge
    output_all_services_2$fuel_surchrg_uplift_gst <- ifelse(output_all_services_2$fuel_surcharge_uplifted != 0,
                                                            output_all_services_2$fuel_surcharge_uplifted * gst,
                                                            0)
    
    # calculate security management fee
    output_all_services_2$sec_mng_chrg_uplifted <- ifelse(output_all_services_2$DESCRIPTION == "Express Post with Signature",
                                                          output_all_services_2$charge_to_custo_exgst * sec_mng_chrg_pct,
                                                          NA)
    output_all_services_2$sec_mng_uplifted_gst <- output_all_services_2$sec_mng_chrg_uplifted * gst
    
    # update table name
    output_all_services_2 <- output_all_services_2
    
    # left in for testing purposes
    #write.csv(output_all_services_2, file = "output_all_services_2.csv")
    
    #### Section 3. Generate billing doc output file (line by line comparison). Its a merger between AP invoice and the newly calculated cols ####
    # join the selected columns onto the billing doc in the correct places
    # this creates the billing_doc_output which is a just the original output withe the new columns added. To be used to inspect individual lines
    
    #### 3.a Specify which columns to merge with AP invoice and join in correct spot ----
    # Specify columns to merge from output_all_services_2
    merge_cols <- c( "service", "uplift", "DESCRIPTION", "BILLING.DOC", "ARTICLE.ID", 
                     "base_charge_incgst", "base_charge_exgst", "base_charge_tax", "charge_value_uplift", "uplift_figure_exgst", 
                     "charge_to_custo_exgst", "cubic_weight", "max_weight", "fuel_surcharge","fuel_surcharge_uplifted", "fuel_gst","fuel_surchrg_uplift_gst", "sec_mng_chrg", "sec_mng_chrg_uplifted", "sec_mng_gst", "sec_mng_uplifted_gst",
                     "over_max_limits_fee", "weight_category_max", "warnings", "is_gst_free_zone")
    
    selected_output_all_services_2 <- output_all_services_2[, merge_cols]
    
    # this can probably all go now
    # Merge bill and selected columns from output_all_services_2 by the unique identifier 
    billing_doc_output <- merge(bill, selected_output_all_services_2, by = c("ARTICLE.ID", "BILLING.DOC", "DESCRIPTION"), all = TRUE)
    
    # Insert new columns after "AVG..UNIT.PRICE"
    avg_unit_price_index <- which(names(billing_doc_output ) == "AMOUNT.INCL.TAX")
    billing_doc_output  <- cbind(billing_doc_output [, 1:avg_unit_price_index],
                                 billing_doc_output [, c("base_charge_incgst","base_charge_exgst", "base_charge_tax", "charge_value_uplift", "uplift_figure_exgst", "charge_to_custo_exgst", "warnings")],
                                 billing_doc_output [, (avg_unit_price_index + 1):ncol(billing_doc_output )])
    
    # Insert new columns after "FUEL.GST"
    fuel_gst_index <- which(names(billing_doc_output ) == "FUEL.GST")
    billing_doc_output  <- cbind(billing_doc_output [, 1:fuel_gst_index  ],
                                 billing_doc_output [, c("fuel_surcharge" ,"fuel_gst", "sec_mng_chrg", "sec_mng_gst", "over_max_limits_fee")],
                                 billing_doc_output [, (fuel_gst_index  + 1):ncol(billing_doc_output )])
    
    # Insert new columns after "BILLED.WEIGHT"
    billed_weight_index <- which(names(billing_doc_output ) == "BILLED.WEIGHT")
    billing_doc_output  <- cbind(billing_doc_output [, 1:billed_weight_index  ], 
                                 billing_doc_output [, c("cubic_weight", "max_weight", "weight_category_max", "service", "uplift")],
                                 billing_doc_output [, (billed_weight_index  + 1):ncol(billing_doc_output )])
    
    # Quick descrepancy check for testing purposes
    discrepancy <- function(billing_doc_output) {
      # Check if the rounded value of AMOUNT.EXCL.TAX is equal to base_charge_exgst
      billing_doc_output$discrepancy <- ifelse(
        round(billing_doc_output$AMOUNT.INCL.TAX, 2) == round(billing_doc_output$base_charge_incgst, 2),
        "no",
        "yes"
      )
      return(billing_doc_output)
    }
    billing_doc_output <- discrepancy(billing_doc_output)
    
    #### 3.b remove duplicates and re-order columns ----
    desired_order <- c("ARTICLE.ID", "BILLING.DOC", "DESCRIPTION", "CUSTOMER", "NAME_1", "NAME_2", "NAME_3", "STREET", "CITY", "REGION", "POST.CODE", 
                       "TELEPHONE", "FAX.NUMBER", "MAILING.STATEMENT.NO.", "ASSIGNMENT.NO.", "SERVICE.DATE", "WORK.CENTRE", "WORK.CENTRE.NAME", "CUSTOMER.REF", "CUSTOMER.REFDOC", 
                       "ITEM", "MATERIAL", "QTY", "AVG..UNIT.PRICE", "AMOUNT.INCL.TAX", "base_charge_incgst", "base_charge_exgst", "discrepancy", "base_charge_tax", "charge_value_uplift", 
                       "uplift_figure_exgst", "charge_to_custo_exgst", "warnings", "TAX.CODE", "TAX.AMOUNT", "AMOUNT.EXCL.TAX", "INVOICE.TOTAL", "TOTAL.QTY", "BILLING.CURRENCY", 
                       "EXCHANGE.RATE", "LOCAL.CURRENCY", "FUEL.SURCHARGE..", "FUEL.SURCHARGE.DISC", "FUEL.GST", "fuel_surcharge","fuel_surcharge_uplifted", "fuel_gst","fuel_surchrg_uplift_gst", 
                       "sec_mng_chrg", "sec_mng_chrg_uplifted", "sec_mng_gst", "sec_mng_uplifted_gst",
                       "over_max_limits_fee", "MHS.FEE", "MHS.DISCOUNT", "MHS.GST", "SMC.FEE", "SMC.DISCOUNT", "SMC.GST", "INTL.SURCHARGE", "INTL.SURCHARGE.MANIFEST", "INVOICE.NO", 
                       "BILLING.DATE", "SALES.ORDER", "SALES.ORDER.ITEM", "PAYER", "PAYER.NAME", "CONSIGNMENT.ID", "LODGEMENT.DATE", "ACTUAL.WEIGHT", "ACTUAL.UNIT", "ACTUAL.LENGTH", 
                       "ACTUAL.WIDTH", "ACTUAL.HEIGHT", "ACTUAL.UNIT.TYPE", "DECLARED.WEIGHT", "DECLARED.UNIT", "DECLARED.LENGTH", "DECLARED.WIDTH", "DECLARED.HEIGHT", 
                       "DECLARED.UNIT.TYPE", "FROM.NAME", "FROM.ADDRESS", "FROM.CITY", "FROM.STATE", "FROM.POSTAL.CODE", "FROM.EMAIL.ADDRESS", "TO.NAME", "TO.ADDRESS", "TO.CITY", 
                       "TO.STATE", "TO.POSTAL.CODE", "TO.EMAIL.ADDRESS", "RECORD.COUNT", "TOT.AMOUNT.EXCL.TAX", "CUST.REF.1", "CUST.REF.2", "BILLED.LENGTH", "BILLED.WIDTH", 
                       "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "cubic_weight", "max_weight", "weight_category_max", "service", "uplift", "INTERNATIONAL.SURCHARGE.RATE", 
                       "CHARGE.CODE", "CHARGE.ZONE", "ATO.DESPATCH.REFERENCE.NUMBER", "RECEIVING.COUNTRY", "SIGNATURE.ON.DELIVERY", "TRANSIT.COVER", "CAPTURE.ID", 
                       "UNMANIFESTED.ARTICLE", "RETURN.TO.SENDER", "LODGEMENT.ZONE", "DESTINATION.ZONE", "CUST.REF.3", "WINE...ALCOHOL", "PEAK.FEE", "PEAK.FEE.DISCOUNT", 
                       "PEAK.FEE.GST", "OVER.MAX.LIMITS.FEE", "OVER.MAX.LIMITS.FEE.DISCOUNT", "OVER.MAX.LIMITS.FEE.GST", "INTERNATIONAL.UNMANIFESTED.FEE", "customer_code2", 
                       "customer_code", "is_gst_free_zone")
    
    # Reorder the columns in final_output
    billing_doc_output <- billing_doc_output [, desired_order]
    
    #### 3.c sum the aggregation lines ----
    # Calculate the sum of fuel_surcharge_uplifted per BILLING.DOC
    fuel_surcharge_per_billing_doc <- billing_doc_output %>%
      filter(is_gst_free_zone == 'No') %>%
      group_by(BILLING.DOC) %>%
      summarise(total_fuel_surcharge = sum(fuel_surcharge_uplifted, na.rm = TRUE))
    
    # Merge the total_fuel_surcharge back to the original dataframe
    billing_doc_output <- left_join(billing_doc_output, fuel_surcharge_per_billing_doc, by = "BILLING.DOC")
    
    # Update base_charge_exgst with the total fuel surcharge where DESCRIPTION is "AP Parcels Domestic Fuel Surcharge"
    billing_doc_output <- billing_doc_output %>%
      mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Parcels Domestic Fuel Surcharge",
                                        total_fuel_surcharge,
                                        base_charge_exgst)) %>% select(-total_fuel_surcharge)
    
    
    #### sum the fuel_surcharg tax free ---
    fuel_surcharge_per_billing_doc <- billing_doc_output %>%
      filter(is_gst_free_zone == 'Yes') %>%
      group_by(BILLING.DOC) %>%
      summarise(total_fuel_surcharge = sum(fuel_surcharge_uplifted, na.rm = TRUE))
    
    billing_doc_output <- left_join(billing_doc_output, fuel_surcharge_per_billing_doc, by = "BILLING.DOC")
    
    billing_doc_output <- billing_doc_output %>%
      mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Parcels Domestic Fuel Surchg Tax Free",
                                        total_fuel_surcharge,
                                        base_charge_exgst)) %>% select(-total_fuel_surcharge)
    
    #### sum the security management fee ----
    sec_mng_chrg_per_billing_doc <- billing_doc_output %>%
      filter(is_gst_free_zone == 'No') %>%
      group_by(BILLING.DOC) %>%
      summarise(total_sec_mng_chrg = sum(sec_mng_chrg_uplifted, na.rm = TRUE))
    
    billing_doc_output <- left_join(billing_doc_output, sec_mng_chrg_per_billing_doc, by = "BILLING.DOC")
    
    billing_doc_output <- billing_doc_output %>%
      mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Security Mgt Charge",
                                        total_sec_mng_chrg,
                                        base_charge_exgst)) %>% select(-total_sec_mng_chrg)
    
    #### sum the security AP Security Mgt Charge Tax Free ----
    
    sec_mng_chrg_per_billing_doc <- billing_doc_output %>%
      filter(is_gst_free_zone == 'Yes') %>%
      group_by(BILLING.DOC) %>%
      summarise(total_sec_mng_chrg = sum(sec_mng_chrg_uplifted, na.rm = TRUE))
    
    billing_doc_output <- left_join(billing_doc_output, sec_mng_chrg_per_billing_doc, by = "BILLING.DOC")
    
    billing_doc_output <- billing_doc_output %>%
      mutate(base_charge_exgst = ifelse(DESCRIPTION == "AP Security Mgt Charge Tax Free",
                                        total_sec_mng_chrg,
                                        base_charge_exgst)) %>% select(-total_sec_mng_chrg)
    
    #### 3.d bring across the services we are not uplifting ----
    billing_doc_output$base_charge_incgst <- ifelse(billing_doc_output$DESCRIPTION %in% c(
      "More to Pay",
      "On Demand Return to Sender", 
      "STC Parcels Domestic Fuel Surcharge", 
      "Duties and Taxes Admin Fee (DDP)",
      "Delivered Duty Paid",
      "AP International Line Haul Surcharge",
      "International  Returns AIR",
      "Lodgement Management Fee",
      "Unmanifest Article"),
      billing_doc_output$AMOUNT.INCL.TAX,
      billing_doc_output$base_charge_incgst)
    
    billing_doc_output$base_charge_exgst <- ifelse(billing_doc_output$DESCRIPTION %in% c(
      "More to Pay",
      "On Demand Return to Sender", 
      "STC Parcels Domestic Fuel Surcharge", 
      "Duties and Taxes Admin Fee (DDP)",
      "Delivered Duty Paid",
      "AP International Line Haul Surcharge",
      "International  Returns AIR",
      "Lodgement Management Fee",
      "Unmanifest Article"),
      billing_doc_output$AMOUNT.EXCL.TAX,
      billing_doc_output$base_charge_exgst)
    
    
    
    #### 3.e get final charge per customer -----
    #billing_doc_output$avg_unit_price_charge_to_custo_ex_gst <- billing_doc_output$QTY * billing_doc_output$charge_to_custo_exgst
    max_charge <- pmax(billing_doc_output$charge_to_custo_exgst, billing_doc_output$base_charge_exgst, na.rm = TRUE)
    max_charge[is.na(max_charge)] <- 0
    
    billing_doc_output$charge_to_custo_exgst <- max_charge
    billing_doc_output$avg_unit_price_charge_to_custo_ex_gst <- billing_doc_output$QTY * billing_doc_output$charge_to_custo_exgst
    
    
    #### 3.f generate the file path ----
    output_folder <- file.path(getwd(), paste0("output_billing_dates_", predefined_text))
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    file_name <- paste0("billing_doc_output_", predefined_text, ".csv")
    full_file_path <- file.path(output_folder, file_name)
    
    write.csv(billing_doc_output, file = full_file_path, row.names = FALSE)
    
    #### aggregation block ----
    # this will be used to generate the sums and the ap_post_supply will be taken from here
    #create international_charge_zone
    billing_doc_output$intl_charge_zone <- billing_doc_output$CHARGE.ZONE
    
    #### 3.g Redefine the services for the supply file ----
    # Restructuring to define the services for the outputs
    billing_doc_output$Service <- ""
    
    # Restructuring to define the services for the outputs
    billing_doc_output$Service <- ""
    
    # Mapping descriptions to services
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Delivered Duty Paid" |
                                 billing_doc_output$DESCRIPTION == "Over Maximum Limits Fee"]               <- "Additional Charges"
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Duties and Taxes Admin Fee (DDP)"]        <- "Add'l Charges - DDP"
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "APGL NZ Express w/Signature"]             <- "APGL NZ Express"
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Imprint Large Charge Letters Regular" |
                                 billing_doc_output$DESCRIPTION == "Imprint Large Charge Letters Priority" |
                                 billing_doc_output$DESCRIPTION == "Imprint Small Charge Letters Regular"]  <- "ELMS"
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Express Post with Signature"|
                                 billing_doc_output$DESCRIPTION == "AP Security Mgt Charge" |
                                 billing_doc_output$DESCRIPTION == "AP Security Mgt Charge Tax Free"|
                                 billing_doc_output$DESCRIPTION == "Express Post Parcels (BYO up to 5kg)"]  <- "eParcel Express"
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Express Post eparcel returns" ]           <- "eParcel Express Returns"
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "PACK AND TRACK INTERNATIONAL"|
                                 billing_doc_output$DESCRIPTION == "Express Courier International (eParcel)"] <- "eParcel International"
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "AP International Line Haul Surcharge"]    <- "eParcel International Line Haul Surcharge"
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "International Returns Express"|
                                 billing_doc_output$DESCRIPTION == "International Returns AIR"]           <- "eParcel International Returns"  # check here for typo
    
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "Parcel Post with Signature"|
                                 billing_doc_output$DESCRIPTION == "AP Parcels Domestic Fuel Surcharge" |
                                 billing_doc_output$DESCRIPTION == "AP Parcels Domestic Fuel Surchg Tax Free"|
                                 billing_doc_output$DESCRIPTION == "AP Manual Handling Surcharge"|
                                 billing_doc_output$DESCRIPTION == "Weekend & Public Holiday Collections"|
                                 billing_doc_output$DESCRIPTION == "eParcel"|
                                 billing_doc_output$DESCRIPTION == "Underpaid Parcels Charges"|
                                 billing_doc_output$DESCRIPTION == "AP Manual Handling Surcharge Tax Free"] <- "eParcel Regular"
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "eParcel Return To Sender" |
                                 billing_doc_output$DESCRIPTION == "eParcel Post Return" |
                                 billing_doc_output$DESCRIPTION == "Local Pickup and Delivery Services"|
                                 billing_doc_output$DESCRIPTION == "Return Paid Parcels Local"|
                                 billing_doc_output$DESCRIPTION == "eParcel Call For Return"|
                                 billing_doc_output$DESCRIPTION == "Lodgement Management Fee" |
                                 billing_doc_output$DESCRIPTION == "More to Pay" |
                                 billing_doc_output$DESCRIPTION == "Unmanifest Article" |
                                 billing_doc_output$DESCRIPTION == "Return To Sender Parcels" ]             <- "eParcel Regular Returns"
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "EPARCEL WINE STD"] <- "eParcel Wine"
    billing_doc_output$Service[billing_doc_output$DESCRIPTION == "On Demand Return to Sender" |
                                 billing_doc_output$DESCRIPTION == "STC Parcels Domestic Fuel Surcharge" |
                                 billing_doc_output$DESCRIPTION == "On Demand Afternoon" |
                                 billing_doc_output$DESCRIPTION == "On Demand Tonight" |
                                 billing_doc_output$DESCRIPTION == "STC Sundry" |
                                 billing_doc_output$DESCRIPTION == "STC EMS"]                               <- "StarTrack OnDemand"
    
    
    
    #### 3.h Remove the compensation rows ----
    #Remove rows where DESCRIPTION is blank 
    billing_doc_output <- billing_doc_output[!is.na(billing_doc_output$DESCRIPTION) & billing_doc_output$DESCRIPTION != "", ]
    
    
    # Produce the output for in the right structure this will be the basis for the aggregation and calculation files
    # need to determine if we remove the below
    desired_order <- c(
      "customer_code", "NAME_1", "MAILING.STATEMENT.NO.", "ASSIGNMENT.NO." , "SERVICE.DATE" , "DESCRIPTION",
      "BILLING.DATE", "CONSIGNMENT.ID", "ARTICLE.ID", "LODGEMENT.DATE", "ACTUAL.WEIGHT", "ACTUAL.UNIT", "ACTUAL.LENGTH",
      "ACTUAL.WIDTH", "ACTUAL.HEIGHT", "ACTUAL.UNIT.TYPE", 	
      "DECLARED.UNIT.TYPE", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH",	"DECLARED.WIDTH",
      "DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "FROM.NAME", 	"FROM.ADDRESS",	"FROM.CITY",	"FROM.STATE",	"FROM.POSTAL.CODE",
      "TO.NAME",	"TO.ADDRESS",	"TO.CITY",	"TO.STATE",	"TO.POSTAL.CODE", "CUST.REF.1",	"CUST.REF.2",	"BILLED.LENGTH", "BILLED.WIDTH",
      "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "CHARGE.CODE", "RECEIVING.COUNTRY", "intl_charge_zone", "CHARGE.ZONE", "Service", "QTY", "AMOUNT.INCL.TAX", 
      "AMOUNT.EXCL.TAX", "base_charge_incgst", "base_charge_exgst", "uplift_figure_exgst", "charge_to_custo_exgst", "fuel_surcharge", "FUEL.SURCHARGE..", 
      "SMC.FEE", "sec_mng_chrg", "over_max_limits_fee", "BILLING.DOC", "is_gst_free_zone"#, "OVER.MAX.LIMITS.FEE"
    )
    # Reorder the columns in final_output
    #billing_doc_output <- billing_doc_output [, desired_order]
    
    
    #### 3.i reorder and rename columns for ap_post_supply ----
    
    desired_order <- c(
      "customer_code", "NAME_1", "MAILING.STATEMENT.NO.", "ASSIGNMENT.NO.", "SERVICE.DATE", "DESCRIPTION",
      "BILLING.DATE", "CONSIGNMENT.ID", "ARTICLE.ID", "LODGEMENT.DATE", "ACTUAL.WEIGHT", "ACTUAL.UNIT", "ACTUAL.LENGTH",
      "ACTUAL.WIDTH", "ACTUAL.HEIGHT", "ACTUAL.UNIT.TYPE", "DECLARED.WEIGHT",	"DECLARED.UNIT",	"DECLARED.LENGTH",	"DECLARED.WIDTH",
      "DECLARED.HEIGHT",	"DECLARED.UNIT.TYPE", "FROM.NAME", 	"FROM.ADDRESS",	"FROM.CITY",	"FROM.STATE",	"FROM.POSTAL.CODE",
      "TO.NAME",	"TO.ADDRESS",	"TO.CITY",	"TO.STATE",	"TO.POSTAL.CODE", "CUST.REF.1",	"CUST.REF.2",	"BILLED.LENGTH", "BILLED.WIDTH",
      "BILLED.HEIGHT", "CUBIC.WEIGHT", "BILLED.WEIGHT", "CHARGE.CODE", "intl_charge_zone", "RECEIVING.COUNTRY", "CHARGE.ZONE", "Service", "QTY",
      "avg_unit_price_charge_to_custo_ex_gst", "charge_to_custo_exgst")
    
    # Reorder the columns in final_output.
    # this is needed for the consoladated 
    ap_post_supply <- billing_doc_output [, desired_order]
    
    # new column names as per desired output
    new_col_names <- c("Code", "NAME_1", "MAILING STATEMENT NO.", "ASSIGNMENT NO.", "SERVICE DATE", "DESCRIPTION", "BILLING DATE", "CONSIGNMENT ID", 
                       "ARTICLE ID", "LODGEMENT DATE", "ACTUAL WEIGHT", "ACTUAL UNIT", "ACTUAL LENGTH", "ACTUAL WIDTH", "ACTUAL HEIGHT", "ACTUAL UNIT TYPE", 
                       "DECLARED WEIGHT", "DECLARED UNIT", "DECLARED LENGTH", "DECLARED WIDTH", "DECLARED HEIGHT", "DECLARED UNIT TYPE", "FROM NAME", 
                       "FROM ADDRESS", "FROM CITY", "FROM STATE", "FROM POSTAL CODE", "TO NAME", "TO ADDRESS", "TO CITY", "TO STATE", "TO POSTAL CODE", 
                       "CUST REF 1", "CUST REF 2", "BILLED LENGTH", "BILLED WIDTH", "BILLED HEIGHT", "CUBIC WEIGHT", "BILLED WEIGHT", "CHARGE CODE", 
                       "INTL CHARGE ZONE", "RECEIVING COUNTRY", "Charge Zone", "Service", "QTY", "AVG. UNIT PRICE EX GST", "AMOUNT EX GST")
    names(ap_post_supply) <- new_col_names
    
    #### 3.j create a folder to store outputs ----
    folder_name <- paste0("ap_post_supply_", predefined_text, ".csv")
    # Replace invalid characters in folder name
    clean_folder_name <- gsub("[^A-Za-z0-9._-]", "_", folder_name)
    new_folder_path <- file.path(output_folder, clean_folder_name)
    dir.create(new_folder_path, showWarnings = FALSE)
    
    #### 3.k split the ap supply out per customer ----
    # Get unique values of NAME_1
    unique_names <- unique(billing_doc_output$NAME_1)
    
    # Loop through each unique NAME_1 value
    for (name in unique_names) {
      # Subset data frame for current NAME_1 value
      subset_data <- billing_doc_output[billing_doc_output$NAME_1 == name, ]
      
      # Reorder the columns in final_output
      subset_data <- subset_data[, desired_order]
      
      # Replace invalid characters in name
      clean_name <- gsub("[^A-Za-z0-9._-]", "_", name)
      
      # Generate file name with folder path
      file_name <- file.path(new_folder_path, paste0("ap_post_supply_", predefined_text, "_", clean_name, ".csv"))
      
      # Write subset to CSV
      write.csv(subset_data, file = file_name, row.names = FALSE)
    }
    
    #### 3.l create the ap post supply consolidated ----
    output_folder <- file.path(getwd(), paste0("output_billing_dates_", predefined_text))
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    file_name <- paste0("ap_post_supply_consolidated_", predefined_text, ".csv")
    full_file_path <- file.path(output_folder, file_name)
    
    write.csv(ap_post_supply, file = full_file_path, row.names = FALSE)
    
    ###### Section 4.a Summary calculations created ----
    # just the custo and description 
    summary_by_custo_description <- billing_doc_output %>%
      filter(!(DESCRIPTION %in% c("AP Parcels Domestic Fuel Surcharge",
                                  "AP Security Mgt Charge",
                                  "AP Parcels Domestic Fuel Surchg Tax Free",
                                  "AP Security Mgt Charge Tax Free",
                                  "STC Parcels Domestic Fuel Surcharge",
                                  "Over Maximum Limits Fee"))) %>%
      group_by(NAME_1, DESCRIPTION) %>%
      summarize(
        count = n(),
        sum_of_AMOUNT.INCL.TAX = sum(AMOUNT.INCL.TAX, na.rm = TRUE),
        sum_of_base_charge_incgst = sum(base_charge_incgst, na.rm = TRUE),
        sum_of_AMOUNT.EXCL.TAX = sum(AMOUNT.EXCL.TAX, na.rm = TRUE),
        sum_of_base_charge_exgst = sum(base_charge_exgst, na.rm = TRUE),
        sum_of_uplift_figure_exgst = sum(uplift_figure_exgst, na.rm = TRUE),
        sum_of_charge_to_custo_exgst = sum(charge_to_custo_exgst, na.rm = TRUE),
        sum_of_FUEL.SURCHARGE.. = sum(FUEL.SURCHARGE.., na.rm = TRUE),
        sum_of_fuel_surcharge = sum(fuel_surcharge, na.rm = TRUE), 
        sum_of_fuel_surcharge_uplifted = sum(fuel_surcharge_uplifted, na.rm = TRUE),
        sum_of_SMC.FEE = sum(SMC.FEE, na.rm = TRUE),
        sum_of_sec_mng_chrg = sum(sec_mng_chrg, na.rm = TRUE),
        sum_of_sec_mng_chrg_uplifted= sum(sec_mng_chrg_uplifted, na.rm = TRUE),
        #    sum_of_OVER.MAX.LIMITS.FEE = sum(OVER.MAX.LIMITS.FEE, na.rm = TRUE),
        sum_of_over_max_limits_fee = sum(over_max_limits_fee, na.rm = TRUE)
      )
    
    
    output_folder <- file.path(getwd(), paste0("output_billing_dates_", predefined_text))
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    file_name <- paste0("summary_by_custo_description_", predefined_text, ".csv")
    full_file_path <- file.path(output_folder, file_name)
    
    write.csv(summary_by_custo_description, file = full_file_path, row.names = FALSE)
    
    
    #just the description 
    summary_by_description <- billing_doc_output %>%
      filter(!(DESCRIPTION %in% c("AP Parcels Domestic Fuel Surcharge",
                                  "AP Security Mgt Charge",
                                  "AP Parcels Domestic Fuel Surchg Tax Free",
                                  "AP Security Mgt Charge Tax Free",
                                  "STC Parcels Domestic Fuel Surcharge",
                                  "Over Maximum Limits Fee"))) %>%
      group_by(DESCRIPTION) %>%
      summarize(
        count = n(),
        sum_of_AMOUNT.INCL.TAX = sum(AMOUNT.INCL.TAX, na.rm = TRUE),
        sum_of_base_charge_incgst = sum(base_charge_incgst, na.rm = TRUE),
        sum_of_AMOUNT.EXCL.TAX = sum(AMOUNT.EXCL.TAX, na.rm = TRUE),
        sum_of_base_charge_exgst = sum(base_charge_exgst, na.rm = TRUE),
        sum_of_uplift_figure_exgst = sum(uplift_figure_exgst, na.rm = TRUE),
        sum_of_charge_to_custo_exgst = sum(charge_to_custo_exgst, na.rm = TRUE),
        sum_of_FUEL.SURCHARGE.. = sum(FUEL.SURCHARGE.., na.rm = TRUE),
        sum_of_fuel_surcharge = sum(fuel_surcharge, na.rm = TRUE), 
        sum_of_fuel_surcharge_uplifted = sum(fuel_surcharge_uplifted, na.rm = TRUE),
        sum_of_SMC.FEE = sum(SMC.FEE, na.rm = TRUE),
        sum_of_sec_mng_chrg = sum(sec_mng_chrg, na.rm = TRUE),
        sum_of_sec_mng_chrg_uplifted= sum(sec_mng_chrg_uplifted, na.rm = TRUE),
        #    sum_of_OVER.MAX.LIMITS.FEE = sum(OVER.MAX.LIMITS.FEE, na.rm = TRUE),
        sum_of_over_max_limits_fee = sum(over_max_limits_fee, na.rm = TRUE)
      )
    
    output_folder <- file.path(getwd(), paste0("output_billing_dates_", predefined_text))
    if (!file.exists(output_folder)) {
      dir.create(output_folder, recursive = TRUE)
    }
    
    file_name <- paste0("summary_by_description_", predefined_text, ".csv")
    full_file_path <- file.path(output_folder, file_name)
    
    write.csv(summary_by_description, file = full_file_path, row.names = FALSE)
    
    
################################################    
    # Provide a download link for the result CSV files
    output$download_result1 <- downloadHandler(
      filename = function() {
        "billing_doc_output.csv"
      },
      content = function(file) {
        write.csv(billing_doc_output, file)
      }
    )
    
    
    output$download_result2 <- downloadHandler(
      filename = function() {
        "ap_post_supply.csv"
      },
      content = function(file) {
        write.csv(ap_post_supply, file)
      }
    )
    
    output$download_result3 <- downloadHandler(
      filename = function() {
        paste("ap_post_supply_", predefined_text, ".zip", sep = "")
      },
      content = function(file) {
        # Create a temporary directory
        temp_dir <- tempdir()
        
        # Define a folder path within the temp directory
        folder_name <- paste0("ap_post_supply_", predefined_text)
        clean_folder_name <- gsub("[^A-Za-z0-9._-]", "_", folder_name)
        new_folder_path <- file.path(temp_dir, clean_folder_name)
        dir.create(new_folder_path, showWarnings = FALSE)
        
        # Get unique values of NAME_1
        unique_names <- unique(billing_doc_output$NAME_1)
        
        # Loop through each unique NAME_1 value and write CSV files
        for (name in unique_names) {
          # Subset data frame for current NAME_1 value
          subset_data <- billing_doc_output[billing_doc_output$NAME_1 == name, ]
          
          # Reorder the columns in final_output
          subset_data <- subset_data[, desired_order]
          
          # Replace invalid characters in name
          clean_name <- gsub("[^A-Za-z0-9._-]", "_", name)
          
          # Generate file name with folder path
          file_name <- file.path(new_folder_path, paste0("ap_post_supply_", predefined_text, "_", clean_name, ".csv"))
          
          # Write subset to CSV
          write.csv(subset_data, file = file_name, row.names = FALSE)
        }
        
        # Zip the folder
        zip_file <- file.path(temp_dir, paste0("ap_post_supply_", predefined_text, ".zip"))
        zip::zipr(zip_file, new_folder_path)
        
        # Copy the zip file to the file argument
        file.copy(zip_file, file)
      },
      contentType = "application/zip"
    )
  
    
     
    
    
    
    output$download_result4 <- downloadHandler(
      filename = function() {
        "summary_by_custo_description.csv"
      },
      content = function(file) {
        write.csv(summary_by_custo_description, file)
      }
    )
    
    output$download_result5 <- downloadHandler(
      filename = function() {
        "summary_by_description.csv"
      },
      content = function(file) {
        write.csv(summary_by_description, file)
      }
    )
    
    downloadButton("download_result1", "Download billing_doc_output")
    downloadButton("download_result2", "Download ap_post_supply")
    downloadButton("download_result3", "Download ap_post_supply_per_customer")
    downloadButton("download_result4", "Download summary_by_custo_&_description")
    downloadButton("download_result5", "Download summary_by_description")
    
    # Print confirmation message with the result
    output$custom_output <- renderPrint({
      cat("Calculation completed successfully.\n")
#      cat("Total Quantity:", sum_qty, "\n")
      cat("Fuel Surcharge:", user_inputs$fuel_surcharge, "\n")
   #   cat("Result (sum_qty * fuel_surcharge):", result, "\n")
    })
  })
  
  # Render the summary text
  output$summary <- renderPrint({
    user_inputs <- user_data()
    cat("User Inputs:\n")
    cat("Fuel Surcharge:", user_inputs$fuel_surcharge, "\n")
    cat("Force Majeure Fee:", user_inputs$force_majeure_fee, "\n")
    cat("Peak Fee:", user_inputs$peak_fee, "\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
