#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
---
  title: "Untitled"
author: "Shane Brennan"
date: "2024-05-13"
output: html_document
runtime: shiny
---
  

library(dplyr)
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Input Form"),
  sidebarLayout(
    sidebarPanel(
      fileInput("csv_file", "Upload CSV File", accept = ".csv"),
      textInput("fuel_surcharge", "Insert Fuel Surcharge % for the month (eg 7.7) "),
      textInput("force_majeure_fee", "Insert Force Majeure Fee, if any"),
      textInput("peak_fee", "Insert Peak Fee, if any"),
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
  # Define a reactive expression to store user inputs
  options(shiny.maxRequestSize = 1024*1024*1024) # 1 GB in bytes
  user_data <- reactive({
    list(
      fuel_surcharge = as.numeric(gsub("%", "", input$fuel_surcharge)),
      force_majeure_fee = as.numeric(gsub("%", "", input$force_majeure_fee)),
      peak_fee = as.numeric(gsub("%", "", input$peak_fee)),
      csv_file = input$csv_file
    )
  })
  
  # Define an observer to trigger custom script after user input and summary rendering
  observeEvent(input$submit, {
    user_inputs <- user_data()
    
    if (!is.null(user_inputs$fuel_surcharge) && !is.null(user_inputs$force_majeure_fee) && !is.null(user_inputs$peak_fee) && !is.null(user_inputs$csv_file)) {
      # Execute your custom script here
      bill <- read.csv(user_inputs$csv_file$datapath, head=TRUE, sep=",")  # Read uploaded CSV file
      #      bill <- read.csv("billing_docs/1013048181-6514511150317568.csv", head=TRUE, sep=",")  # 17 - 28 feb
      sum_qty <- sum(bill$QTY)
      result <- sum_qty * user_inputs$fuel_surcharge
      print(result)
      write.csv(result, file = "result.csv")
      print("ran")  # Print "ran" after the result
    }
  })
  
  # Render the summary text
  output$summary <- renderPrint({
    user_inputs <- user_data()
    cat("User Inputs:\n")
    cat("Fuel Surcharge:", user_inputs$fuel_surcharge, "\n")
    cat("Force Majeure Fee:", user_inputs$force_majeure_fee, "\n")
    cat("Peak Fee:", user_inputs$peak_fee, "\n")
  })
  
  # Custom output
  output$custom_output <- renderPrint({
    user_inputs <- user_data()
    # "ran" will be printed after the result
  })
}

# Run the application
shinyApp(ui = ui, server = server)

