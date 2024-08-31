library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(DT)
library(ggplot2)
library(shiny)
library(htmltools)

load("Tourism_data.Rdata")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Tables",
             titlePanel("Table and Graph"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("tableType", "Select Table Type:", choices = table_list),
               ),
               mainPanel(
                 DTOutput("table"),
                 plotOutput("plot")
               )
             )),
    tabPanel("Main parts of India",
             titlePanel("Part Wise Tourist Place"),
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("N_S", "Select", choices = table2_list),
                 
                 uiOutput("data_type"),
                 uiOutput("pictures")
               ),
               
               mainPanel(
                 DTOutput("content")
               )
             )
    ),
    tabPanel("States",
             tabPanel("States",
                      titlePanel("State Wise Tourist Place"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("rajya", "State/Union Territory", choices = State_list),
                          
                          uiOutput("data_type3"),
                          uiOutput("pictures3")
                        ),
                        
                        mainPanel(
                          DTOutput("content3")
                        )
                      )
             )),
    tabPanel("Motorbike Tour",
             titlePanel("Bike Tour Table"),
             mainPanel(
               DTOutput("myTable")
             )),
    tabPanel("Travel Monthwise",
             titlePanel("Travel by Month in India"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("Month", "Select Month:", choices = Travel_by_Month_in_India$Months),
               ),
               mainPanel(
                 textOutput("Description")
               )
             ))
  )
)

server <- function(input, output) {
  datasetInput1 <- reactive({
    switch(input$tableType,
           "Foreign tourist arrivals in India (1997–2022)." = table_1,
           "Foreign exchange earnings from tourism in India (1997–2020)." = table_2,
           "Source countries for foreign tourist arrivals in India in 2019." = table_3,
           "Share of top 10 states/UTs of India in number of foreign tourist visits in 2017." = table_4,
           "Share of top 10 states/UTs of India in number of domestic tourist visits in 2017." = table_5,
           "Scrapped_data_table" = final_table
    )
  })
  
  output$table <- renderDT({
    datatable(
      datasetInput1(),
      escape = FALSE
    )
    
  })
  
  output$plot <- renderPlot({
    data <- datasetInput1()
    
    if (identical(data, table_1)) {
      ggplot(data = data) +
        geom_line(mapping = aes(x = Year, y = `Arrivals (millions)`), linetype = 1, linewidth = 1, color = "red")
    } else if (identical(data, table_2)) {
      
      
      
      # Assuming your data is in a tibble named table_2
      data$`Earnings (₹ crores)` <- as.numeric(gsub(",", "", gsub("−", "-", data$`Earnings (₹ crores)`)))
      # Create a bar plot
      barplot(data$`Earnings (₹ crores)`, names.arg = data$Year, col = "blue", main = "Earnings Over the Years",
              xlab = "Year", ylab = "Earnings in ₹ Crores")
      
    } else if (identical(data, table_3)) {
      
      data <- data[-c(12),]
      
      # Assuming your data is in a tibble named table_2
      data$Number <- as.numeric(gsub(",", "", data$Number))
      barplot(data$Number, names.arg = data$Country, col = "blue", main = "Number of People",
              xlab = "Country", ylab = "Number")
      
    } else if (identical(data, table_4)) {
      
      data <- data[c(1:10),]
      
      # Assuming your data is in a tibble named table_2
      data$Number <- as.numeric(gsub(",", "", data$Number))
      barplot(data$Number, names.arg = data$`State/Union Territory`, col = "blue", main = "Share of top 10 states/UTs of India in number of foreign tourist visits in 2017",
              xlab = "State/Union Territory", ylab = "NUmber")
      
    } else if (identical(data, table_5)) {
      
      data <- data[c(1:10),]
      
      # Assuming your data is in a tibble named table_2
      data$Number <- as.numeric(gsub(",", "", data$Number))
      barplot(data$Number, names.arg = data$`State/Union Territory`, col = "blue", main = "Share of top 10 states/UTs of India in number of domestic tourist visits in 2017",
              xlab = "State/Union Territory", ylab = "NUmber")
      
    } 
  })
  
  datasetInput <- reactive({
    switch(input$N_S,
           "North_India" = North_India,
           "South_India" = South_India,
           "Central_India" = Central_India,
           "West_India" = West_India,
           "East_India" = East_India,
           "NorthEast_India" = NorthEast_India
    )
  })
  
  output$data_type <- renderUI({
    data <- datasetInput()
    selectInput("data_type", "Please Select", choices = data$Data_Type)
  })
  
  output$pictures <- renderUI({
    data <- datasetInput()
    pictures_url <- data[data$Data_Type == input$data_type, "Picture_URL"]
    
    if (is.null(pictures_url) || length(pictures_url) == 0) {
      return(NULL)
    }
    
    pictures <- lapply(pictures_url, function(url) {
      tags$img(src = url)
    })
    
    div(pictures)
  })
  
  output$content <- renderDT({
    data <- datasetInput()
    datatable(data)
  })
  
  output$Description <- renderText({
    if (input$Month == "November")
    {
      Travel_by_Month_in_India$Description[1]
    } else if (input$Month == "December")
    {
      Travel_by_Month_in_India$Description[2]
    } else if (input$Month == "January")
    {
      Travel_by_Month_in_India$Description[3]
    } else if (input$Month == "February")
    {
      Travel_by_Month_in_India$Description[4]
    } else if (input$Month == "February")
    {
      Travel_by_Month_in_India$Description[4]
    } else if (input$Month == "March")
    {
      Travel_by_Month_in_India$Description[5]
    } else if (input$Month == "April")
    {
      Travel_by_Month_in_India$Description[6]
    } else if (input$Month == "May")
    {
      Travel_by_Month_in_India$Description[7]
    } else if (input$Month == "June")
    {
      Travel_by_Month_in_India$Description[8]
    } else if (input$Month == "July")
    {
      Travel_by_Month_in_India$Description[9]
    } else if (input$Month == "August")
    {
      Travel_by_Month_in_India$Description[10]
    } else if (input$Month == "September")
    {
      Travel_by_Month_in_India$Description[11]
    } else if (input$Month == "October")
    {
      Travel_by_Month_in_India$Description[12]
    } 
  })
  
  output$myTable <- renderDT({
    datatable(my_tibble)
  })
  
  datasetInput3 <- reactive({
    switch(input$rajya,
           "Andhra_Pradesh" = Andhra_Pradesh,
           "Bihar" = Bihar,
           "chattisgarh" = chattisgarh,
           "Gujarat" = Gujarat,
           "Uttar_pradesh" = Uttar_pradesh,
           "Karnataka" = Karnataka,
           "Jharkhand" = Jharkhand,
           "Goa" = Goa,
           "Madhya_Pradesh" = Madhya_Pradesh,
           "Assam" = Assam,
           "West_bengal"=West_bengal,
           "Delhi"=Delhi,
           "Telangana"=Telangana,
           "Uttrakhand"=Uttrakhand,
           "TamilNadu"=TamilNadu,
           "Kerala"=Kerala,
           "Rajasthan"=Rajasthan,
           "Odisha"=Odisha,
           "Maharastra"=Maharastra,
           "Andaman_and_Nicobar"=Andaman_and_Nicobar
           
           
           
           
    )
  })
  
  output$data_type3 <- renderUI({
    data <- datasetInput3()
    selectInput("data_type3", "Select the tour place", choices = data$Data_Type)
  })
  
  output$pictures3 <- renderUI({
    data <- datasetInput3()
    pictures_url3 <- data[data$Data_Type == input$data_type3, "Picture_URL"]
    
    if (is.null(pictures_url3) || length(pictures_url3) == 0) {
      return(NULL)
    }
    
    pictures3 <- lapply(pictures_url3, function(url) {
      tags$img(src = url)
    })
    
    div(pictures3)
  })
  
  output$content3 <- renderDT({
    data <- datasetInput3()
    datatable(data)
  })
  
  
}

shinyApp(ui, server)




