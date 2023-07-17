# Programmer: Michael Bradshaw
# Date: July 17, 2023
# Purpose: App.R for kepler Exoplanet Dataset App - ST558 - Project 3

# Packages to include:
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(DT)


# UI Code:
ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "Kepler Exoplanet Analysis App", titleWidth=1000),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Data Exploration", tabName = "exploration"),
      menuItem("Modeling", tabname = "modeling"),
               menuSubItem("Modeling Info", tabName = "modeling_info"),
               menuSubItem("Model Fitting", tabName = "modeling_fitting"),
               menuSubItem("Prediction", tabName = "prediction"),
      menuItem("Data", tabName = "data")
    )),
  
  # Main panel
  dashboardBody(
    # Tabbed layout
    tabItems(
      
      # About page
      tabItem(tabName="about",
              fluidRow(
                column(width = 12,
                       h2("Purpose of the App"),
                       p("The purpose of this app is to analyze and explore the kepler exoplanet dataset."),
                       p("You can explore the data, perform data exploration, fit models, and make predictions.")
                ),
                column(width = 12,
                       h2("Data and Source"),
                       p("The Kepler Space Observatory was launched in 2009. Its primary mission is to explore star systems beyond our own, specifically targeting exoplanets. The objective of the Kepler telescope is to discover potentially habitable planets around other stars. By observing these exoplanets, Kepler aims to expand our understanding of the universe and the possibility of life elsewhere. The dataset used in this app is a cumulative record of all observed Kepler objects of interest, which includes approximately 10,000 exoplanet candidates on which Kepler has collected observations."),
                       p("You can find more information about the dataset used in this app.  ",
                         a(href = "https://www.kaggle.com/datasets/nasa/kepler-exoplanet-search-results?resource=download", "Learn about the dataset here!")),
                       p("For specific details on the data columns, refer to the data dictionary. ",
                         a(href = "https://exoplanetarchive.ipac.caltech.edu/docs/API_kepcandidate_columns.html", "A data dictionary of the all data columns is here!"))
                ),
                column(width = 12,
                       h2("What does this app contain?"),
                       
                       p("The About page introduces this app with the purpose, a description of the data, and app overview."),
                       p("The Data Exploration page provides numerical and graphical summaries of the data. You can choose what variables and plots to review."),
                       p("The Modeling tab consists of three pages: Modeling Info, Model Fitting, and Prediction. The Modeling Info page examines a multiple linear regression model, a boosted tree model, and a random forest model. The Model Fitting page involves dividing the data into training and testing sets (proportion can be chosen), constructing the three models, and comparing their performance. Lastly, the Prediction page allows you to make predictions for the response variable using the model of your choice."),
                       p("The Data page has the dataset used for this app. Here, you can filter, subset, and download the kepler exoplanet data.")
                ),
              ),
              br(),
              fluidRow(
                column(width = 12,
                       img(src = "k2_100planet_image.png", width = "100%")
                )
              )
      ),

# Data Exploration page
#          tabItem("Data Exploration", value = "exploration",
#                   # Display numerical and graphical summaries based on user selections
#          ),
#          
#          # Modeling page
#          tabItem("Modeling", value = "modeling",
#                   
#                   # Model Info tab
#                   tabItem("Modeling Info",
#                            # Provide explanation of different modeling approaches and their benefits/drawbacks
#                            # Include mathJax equations for better understanding
#                   ),
#                   
#                   # Model Fitting tab
#                   tabItem("Model Fitting",
#                            # Split data into training and test sets
#                            # Allow users to choose model settings and variables
#                            # Provide buttons to fit models on training data and display model summaries
#                   ),
#                   # Prediction page
#                   tabItem("Prediction", value = "prediction",
#                            # Allow users to input predictor values and obtain predictions
#                   )
#          ),
          # Data page
          tabItem(tabName="data",
                  fluidRow(
                    column(width=12,
                   # Display the dataset in a scrollable table
                   # Include options to subset rows and columns
                   box(width=12,
                   radioButtons(inputId = "dataDisposition",
                               label = "Would you like to subset the dataset by exoplanet disposition?",
                               choices = c("Do Not Filter" = "No",
                                           "FALSE POSITIVE" = "FALSE POSITIVE",
                                           "CANDIDATE" = "CANDIDATE",
                                           "CONFIRMED" = "CONFIRMED"),
                               selected = "No")
                   ),
                   # Provide checkboxes to select variables
                   box(width = 12, style = "height:350px; overflow-y: scroll;",
                       checkboxGroupInput(inputId = "selectedColumns", "Select variables to include:",
                                          choices = NULL, # Provide all column names as choices
                                          selected = NULL) # Initially select all columns
                       ),
                   # Provide a button to save the data as a CSV file
                   box(width = 12,
                       title = "Download Data Here:",
                       downloadButton("dataDownload", "Download"))
                    ),
                   # Show exoplanet dataset that has been subset or filtered
                   column(width=12,
                          box(width=12,
                              dataTableOutput("data"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                          )
                   )
                  )
          )
        )
      )
    )



server <- shinyServer(function(input, output, session) {
  
  # Reads in data and data cleans slightly
  kepler <- reactive({
    keplerData <- read_csv("keplerExoplanetCumulative.csv")
    return(keplerData)
  })
  
  # Update choices for checkboxGroupInput based on available columns
  observe({
    columns <- names(kepler())
    updateCheckboxGroupInput(session, "selectedColumns", choices = columns, selected = columns)
  })
  
  # Filter and subset the data based on user selection
    filteredData <- reactive({
      kepler <- kepler()
      
      if (input$dataDisposition != "No") {
        kepler <- kepler %>%
          filter(koi_disposition == input$dataDisposition)
      }
      
      # Subsetting columns based on user selection
      kepler <- kepler %>%
        select(input$selectedColumns)
      
      return(kepler)
    })
  
  # Render the data table
  output$data <- renderDataTable({
    datatable(filteredData(), options = list(paging = FALSE))
  })
  
  # Download the data as a CSV file
  output$dataDownload <- downloadHandler(
    filename = function(){
      paste("keplerDataDownload-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file){
      write.csv(filteredData(), file)
    }
  )
  
})

# Run the application 
shinyApp(ui = ui, server = server)
