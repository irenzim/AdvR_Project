library(shiny)
library(shinydashboard)
library(shinythemes)
library(shiny)
library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title="Training Metric Program"),
  dashboardSidebar(
    sidebarMenu(
      ## 01. DATA PREPARATION
      menuItem("Data Preparation", tabName = "preps", icon = icon("database"),
               menuSubItem("Upload Here", tabName="uploadcsv"),
               menuSubItem("Cleaning NA", tabName="cleaning"),
               menuSubItem("Convert Values", tabName="convertVal")),
      
      ## 02. DATA VISUALIZAION
      menuItem("Data Visualization", tabName = "vis", icon = icon("line-chart")),
      ## 03. MODEL METRICS
      menuItem("Model Metrics", tabName = "metrics", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      ## 01. DATA PREPARATION - TAB: UPLOAD CSV
      tabItem(tabName = "uploadcsv",
              sidebarLayout(
                sidebarPanel(
                  h3("Upload File"),
                  fileInput(inputId = "file", label="Choose a CSV file", accept=".csv"),
                  checkboxInput("header", "Header", TRUE),
                  actionButton("UploadButton", "Proceed")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Show Dataframe",DT::dataTableOutput("data")),
                    tabPanel("Summary",verbatimTextOutput("summary")),
                    tabPanel("Na Values",tableOutput("NaData"))
                  )
                )
              )
      ),
      ## 01. DATA PREPARATION - TAB: CLEANING
      tabItem(tabName="cleaning",
              sidebarLayout(
                sidebarPanel(
                  h3("Cleaning NA"),
                  selectInput(inputId = "DfCols",label="choose your column",choices=NULL,selected = NULL),
                  selectInput(inputId = "HowNa",label="How to clean Na",choices=c("Omit", "Replace with Mean", "Replace with Median"),selected = "Omit"),
                  actionButton("CleanButton", "Clean NAs")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Cleared NA", DT::dataTableOutput("ClearedNA")),
                    tabPanel("NA Description", tableOutput("UpdatedNa"))
                  )
                )
                
              )),
      ## 01. DATA PREPARATION - TAB: CONVERT VALUES
      tabItem(tabName="convertVal",
              sidebarLayout(
                sidebarPanel(
                  actionButton("ShowCurrentSummary", "Show Current Summary"),
                  h3("Convert Values"),
                  selectInput(inputId = "DfColsConvert",label="choose your column",choices=NULL,selected = NULL),
                  selectInput(inputId = "HowConvert",label="Convert Values",choices=c("String", "Integer", "Numeric","Factor","Date"),selected = "Integer"),
                  actionButton("ConvertVal", "Convert Value"),
                  br(),
                  br(),
                  br(),
                  h3("Download Updated Dataset"),
                  h4("After cleaning and preaparing your data, you may download the csv"),
                  downloadButton("downloadData", "Download Latest Updated Data")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Summary",verbatimTextOutput("summaryconverted")),
                    tabPanel("Dataframe", DT::dataTableOutput("convertdata"))
                  )
                )
                
              ))
    )
  ))

server <- function(input, output,session) {
  
  
  # 01. DATA PREPARATION - TAB: UPLOAD CSV
  data <- reactive({
    req(input$file)
    # Get the file extension
    ext <- tools::file_ext(input$file$name)
    # Check if the file extension is CSV
    if(ext == "csv") {
      fread(input$file$datapath, header = TRUE, stringsAsFactors = FALSE)
    } else {
      stop("Invalid file format. Please upload a CSV file.")
    }
  })
  observeEvent(input$UploadButton, {
    # TAB 1 : DF.HEAD (5)
    headDf <- reactive({ data() %>% slice(1:5) })
    output$data <- DT::renderDataTable({ headDf() %>% datatable(options=list(scrollX=TRUE))})
    # TAB 2 : DF STR
    summaryDf <- reactive({str(data())})
    output$summary <- renderPrint({summaryDf()})
    # TAB 3 : SHOW NA DATA
    na_counts <- reactive({ colSums(is.na(data())) })
    na_cols <- names(na_counts())[na_counts() > 0]
    na_counts <- na_counts()[na_cols]
    NaDf <- data.frame(ColNames = na_cols, NAs = na_counts)
    output$NaData <- renderTable(NaDf)
  })
  
  # 01. DATA PREPARATION - TAB: CLEANING NA
  # TAB 1 : DF.HEAD (5)
  # Initialize reactive value
  updateDf <- reactiveVal()
  
  observe({
    # Update select input when data is uploaded
    if(!is.null(data())) {
      updateSelectInput(session, "DfCols", label="choose your column", choices=names(data()), selected=names(data())[1])
    }
  })
  
  observeEvent(input$CleanButton, {
    col <- input$DfCols
    how_na <- input$HowNa
    
    # Get the cleaned data to work with
    if (is.null(updateDf())) {
      data_cleaned <- data()
    } else {
      data_cleaned <- updateDf()
    }
    
    # Clean data based on user input
    if (how_na == "Omit"){
      data_cleaned <- data_cleaned %>% drop_na({{col}})
    }
    else if (how_na == "Replace with Mean"){
      data_cleaned <- data_cleaned %>% mutate({{col}} := ifelse(is.na({{col}}), mean({{col}}, na.rm = TRUE), {{col}}))
    }
    else if (how_na == "Replace with Median"){
      data_cleaned <- data_cleaned %>% mutate({{col}} := ifelse(is.na({{col}}), median({{col}}, na.rm = TRUE), {{col}}))
    }
    
    # Update the reactive value with cleaned data
    updateDf(data_cleaned)
    
    # Render cleaned data table
    output$ClearedNA <- DT::renderDataTable({data_cleaned %>% slice(1:5)%>% datatable(options=list(scrollX=TRUE))})
    
    # TAB 2 : SHOW REMAINING NA DATA
    na_counts <- reactive({ colSums(is.na(data_cleaned)) })
    na_cols <- names(na_counts())[na_counts() > 0]
    na_counts <- na_counts()[na_cols]
    NaDf <- data.frame(ColNames = na_cols, NAs = na_counts)
    output$UpdatedNa <- renderTable(NaDf)
    
  })
  
  # 01. DATA PREPARATION - TAB: CONVERT VALUES
  data_cleaned <- reactiveVal()
  cleanedData <- reactiveVal(NULL)
  
  observeEvent(updateDf(), {
    data_cleaned(updateDf())
    cleanedData(data_cleaned())
  })
  
  observeEvent(input$ShowCurrentSummary, {
    # Get the cleaned data to work with
    if (is.null(updateDf())) {
      currentCleanedDf <- data()
    } else {
      currentCleanedDf <- cleanedData()
    }
    output$summaryconverted <- renderPrint({str(currentCleanedDf)})
    output$convertdata <- DT::renderDataTable({currentCleanedDf %>% slice(1:5)%>% datatable(options=list(scrollX=TRUE))})
  })
  
  observe({
    # Update select input when data is uploaded
    if(!is.null(updateDf())) {
      updateSelectInput(session, "DfColsConvert", label="choose your column", choices=names(updateDf()), selected=names(updateDf())[1])
    } else {
      updateSelectInput(session, "DfColsConvert", label="choose your column", choices=names(data()), selected=names(data())[1])
    }
  })
  observeEvent(input$ConvertVal, {
    col_convert <- input$DfColsConvert
    how_convert <- input$HowConvert
    
    if (is.null(updateDf())) {
      currentCleanedDf <- data()
    } else {
      currentCleanedDf <- cleanedData()
    }
    
    if (how_convert=="Integer"){
      currentCleanedDf[[col_convert]] <- as.integer(currentCleanedDf[[col_convert]])
    } else if (how_convert == "String") {
      currentCleanedDf[[col_convert]] <- as.character(currentCleanedDf[[col_convert]])
    } else if (how_convert == "Numeric") {
      currentCleanedDf[[col_convert]] <- as.numeric(currentCleanedDf[[col_convert]])
    } else if (how_convert == "Factor") {
      currentCleanedDf[[col_convert]] <- as.factor(currentCleanedDf[[col_convert]])
    } else if (how_convert == "Date") {
      currentCleanedDf[[col_convert]] <- as.Date(currentCleanedDf[[col_convert]],format = "%Y%m%dT%H%M%S")
    }
    
    cleanedData(currentCleanedDf)
    output$summaryconverted <- renderPrint({str(currentCleanedDf)})
    output$convertdata <- DT::renderDataTable({currentCleanedDf %>% slice(1:5)%>% datatable(options=list(scrollX=TRUE))})
    
    # still not working yet
    output$downloadData <-downloadHandler(
      filename = function(){"updateddata.csv"},
      content = function(file) {
        write.csv(currentCleanedDf, file)
      }
    )
    
  })
}

shinyApp(ui, server)