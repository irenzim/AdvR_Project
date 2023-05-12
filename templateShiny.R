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
      ## 01. DATA CLEANING
      menuItem("Data Cleaning", tabName = "preps", icon = icon("database"),
               menuSubItem("Upload Here", tabName="uploadcsv"),
               menuSubItem("Cleaning NA", tabName="cleaning"),
               menuSubItem("Convert Values", tabName="convertVal")),
      
      ## 02. DATA VISUALIZAION
      menuItem("Data Visualization", tabName = "vis", icon = icon("line-chart")),
      ## 03. MODEL METRICS
      menuItem("Model", tabName = "model", icon = icon("dashboard"),
               menuSubItem("Data Preparation", tabName="modelpreparation"),
               menuSubItem("Select Model", tabName="selectmodel"),
               menuSubItem("Model Metrics", tabName="metrics"))
    )
  ),
  dashboardBody(
    tabItems(
      ## 01. DATA CLEANING - TAB: UPLOAD CSV
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
      ## 01. DATA CLEANING - TAB: CLEANING
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
      ## 01. DATA CLEANING - TAB: CONVERT VALUES
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
                
              )),
      ## 03. MODEL - TAB: DATA PREPARATION
      tabItem(tabName = "modelpreparation",
              sidebarLayout(
                sidebarPanel(
                  fluidRow(
                    column(width = 8,
                           h3("Select Variables"),
                           h4("Variable Y"),
                           selectInput(inputId = "YCol", label = "Choose Your Y Variable", choices = NULL, selected = NULL),
                           h4("Variable X"),
                           selectInput(inputId = "XCols", label = "Choose your X Variables", choices = NULL, multiple = TRUE),
                           actionButton("PickVars", "Select Variables")
                    ),
                  ),
                  fluidRow(
                    column(width = 8,
                           h3("Split Ratio"),
                           selectInput(inputId = "TrainTestSplit", label = "Choose your split ratio", choices = c("70-30", "80-20", "90-10"), selected = "80-20"),
                           actionButton("TrainTestButton", "Split!")
                    )
                  )
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Summary", verbatimTextOutput("formulasummary"),verbatimTextOutput("splitratiosummary"))
                  )
                )
              )
      )
      
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
  # 02. DATA VISUALIZATION
  
  # 03. MODEL - TAB: DATA PREPARATION
  currentCleanedDf <- reactiveVal()
  modelDf <- reactiveVal(NULL)
  
  observeEvent(cleanedData(), {
    currentCleanedDf(cleanedData())
    modelDf(data_cleaned())
  })
  
  observeEvent(cleanedData(), {
    if (!is.null(cleanedData())) {
      modelDf(cleanedData())
    } else if (!is.null(updateDf())) {
      modelDf(updateDf())
    } else {
      modelDf(data())
    }
  })
  
  # Choosing Y Var
  observe({
    if(!is.null(cleanedData())) {
      updateSelectInput(session, "YCol", label="choose your column", choices=names(cleanedData()), selected=names(cleanedData())[1])
    } else if (!is.null(updateDf())){
      updateSelectInput(session, "YCol", label="choose your column", choices=names(updateDf()), selected=names(updateDf())[1])
    } else {
      updateSelectInput(session, "YCol", label="choose your column", choices=names(data()), selected=names(data())[1])
    }
  })
  # Choosing X Var
  observe({
    if(!is.null(cleanedData())) {
      updateSelectInput(session, "XCols", label="choose your column", choices=names(cleanedData()), selected=names(cleanedData())[1])
    } else if (!is.null(updateDf())){
      updateSelectInput(session, "XCols", label="choose your column", choices=names(updateDf()), selected=names(updateDf())[1])
    } else {
      updateSelectInput(session, "XCols", label="choose your column", choices=names(data()), selected=names(data())[1])
    }
  })
  
  formula <- reactiveVal()
  train_data <- reactiveVal()
  test_data <- reactiveVal()
  
  # Making Formula
  observeEvent(input$PickVars, {
    y_var <- input$YCol
    x_vars <- input$XCols
    
    # Create the formula using the selected variables
    formula_str <- paste(y_var, "~", paste(x_vars, collapse = " + "))
    
    # Store the formula
    formula((formula_str))
    print(formula)
    output$formulasummary <- renderPrint({formula_str})
    
    
  })
  
  # Split Data
  observeEvent(input$TrainTestButton, {
    if (!is.null(cleanedData())) {
      modelDf <- cleanedData()
    } else if (!is.null(updateDf())) {
      modelDf <- updateDf()
    } else {
      modelDf <- data()
    }
    split_ratio <- as.numeric(strsplit(input$TrainTestSplit, "-")[[1]]) / 100
    
    
    if (input$TrainTestSplit == "70-30") {
      train <- modelDf[1:round(split_ratio[1] * nrow(modelDf)), ]
      test <- modelDf[(round(split_ratio[1] * nrow(modelDf)) + 1):nrow(modelDf), ]
    } else if (input$TrainTestSplit == "80-20") {
      train <- modelDf[1:round(split_ratio[1] * nrow(modelDf)), ]
      test <- modelDf[(round(split_ratio[1] * nrow(modelDf)) + 1):nrow(modelDf), ]
    } else if (input$TrainTestSplit == "90-10") {
      train <- modelDf[1:round(split_ratio[1] * nrow(modelDf)), ]
      test <- modelDf[(round(split_ratio[1] * nrow(modelDf)) + 1):nrow(modelDf), ]
    }
    train_data(train)
    test_data(test)
    output$splitratiosummary <- renderPrint({
      cat("Train Data Dimensions: ", dim(train)[1], " rows, ", dim(train)[2], " columns\n")
      cat("Test Data Dimensions: ", dim(test)[1], " rows, ", dim(test)[2], " columns\n")
    })
  })
}

shinyApp(ui, server)