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
library(dplyr)
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
      textInput("folder_path", "Insert Folder Path to Save CSV"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      verbatimTextOutput("custom_output") # Adding custom output element
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
    
    # Read the uploaded CSV file
    bill <- read.csv(user_inputs$csv_file$datapath, head=TRUE, sep=",")
    
    # Check if 'QTY' column exists in the CSV
    if (!("QTY" %in% colnames(bill))) {
      output$custom_output <- renderPrint({
        cat("Error: The uploaded CSV does not contain a 'QTY' column.\n")
      })
      return()
    }
    
    # Proceed with calculation
    sum_qty <- sum(bill$QTY, na.rm = TRUE)
    result <- sum_qty * user_inputs$fuel_surcharge
    
    # Provide a download link for the result CSV file
    output$download_link <- downloadHandler(
      filename = function() {
        "result.csv"
      },
      content = function(file) {
        write.csv(result, file)
      }
    )
    
    # Print confirmation message with the result
    output$custom_output <- renderPrint({
      cat("Calculation completed successfully.\n")
      cat("Total Quantity:", sum_qty, "\n")
      cat("Fuel Surcharge:", user_inputs$fuel_surcharge, "\n")
      cat("Result (sum_qty * fuel_surcharge):", result, "\n")
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

# Define UI
ui <- fluidPage(
  titlePanel("Input Form"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csv_file", "Upload CSV File", accept = ".csv"),
      textInput("fuel_surcharge", "Insert Fuel Surcharge % for the month (e.g., 7.7) "),
      textInput("force_majeure_fee", "Insert Force Majeure Fee. Insert 0 if there is none"),
      textInput("peak_fee", "Insert Peak Fee. Insert 0 if there is none"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      verbatimTextOutput("custom_output"), # Adding custom output element
      downloadButton("download_link", "Download Result CSV")
    )
  )
)

# Run the application
shinyApp(ui = ui, server = server)

