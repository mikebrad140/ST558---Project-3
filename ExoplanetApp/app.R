# Programmer: Michael Bradshaw
# Date: July 21, 2023
# Purpose: App.R for kepler Exoplanet Dataset App - ST558 - Project 3

# Packages to include:
library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(caret)
library(DT)
library(doParallel)

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
                       p("The Modeling tab consists of three pages: Modeling Info, Model Fitting, and Prediction. The Modeling Info page examines a multiple logistic regression model, a boosted tree model, and a random forest model. The Model Fitting page involves dividing the data into training and testing sets (proportion can be chosen), constructing the three models, and comparing their performance. Lastly, the Prediction page allows you to make predictions for the response variable using the model of your choice."),
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
          tabItem(tabName = "exploration",
                  fluidRow(
                    column(width=12, align="center" , h3("Data Exploration of the Kepler Exoplanet dataset")),
                    br(),
                    br(),
                    br(),
                    # Dropdown to select disposition type
                    column(width=4,
                    selectInput("dispositionType", "Filter Data by Disposition Type:",
                                choices = c("All", "FALSE POSITIVE", "CANDIDATE", "CONFIRMED"),
                                selected = "All"),
                    
                    # Dropdown to select plot type
                    selectInput("plotType", "Select Plot Type:",
                                choices = c("Histogram","Density Plot"),
                                selected = "Histogram"),
                    
                    # Dropdown to select summary type
                    selectInput("summaryType", "Select Summary Type:",
                                choices = c("Summary Statistics", "Summary Statistics by Exoplanet Disposition"),
                                selected = "Summary Statistics"),
                    
                    # Dropdown to select variables
                    selectInput("selectedVariable", "Selected Variables:",
                                choices = c("DispositionScore", "OrbitalPeriodDays",
                                            "TransitEpoch", "ImpactParameter", "TransitDurationHrs",
                                            "TransitDepth_ppm", "PlanetaryRadius_Earthradii",
                                            "StellarEffectiveTemperatureK", "StellarSurfaceGravity"),
                                selected = "DispositionScore"),
                    ),
                    # Plot output
                    column(width=8,
                    plotOutput("explorationPlot")
                    ),
                    # Table output for summary statistics or Contingency table
                    tableOutput("summaryTable")
                  )
          ),
          
           # Modeling page
                    # Model Info tab
                   tabItem("modeling_info",
                           fluidRow(
                          # Add latex
                          withMathJax(),
                          column(# Linear Regression
                                 h3("Generalized Linear Regression: Logistic Regression"),
                                 width=12,
                                     h4("Explanation of method:"),
                                     p("Multiple logistic regression is a statistical method used to model the relationship between a binary dependent/response variable (exoplanet disposition) and two or more independent variables (predictors).  The model applies the logit function to the linear combination of predictors to estimate the probability of the binary outcome.  These models are usually written as: $$P(Y=1)=\\dfrac{1}{1+e^{-(\\beta_0 + \\beta_1 x_{1i} + \\beta_2 x_{2i}+ ... + \\beta_n x_{ni})}}$$"),  
                                     p("$$where~P(Y=1) ~ is~the~probability~of~our~response~variable~being~1.$$"),
                                     p("$$X_1,X_2,X_n~are~the~predictor~variables.$$"),
                                     p("$$\\beta_1 + \\beta_2+ ... + \\beta_n~are~the~coefficients~for~each~predictor.$$"),
                                     br(),
                                     h4("Benefits:"),
                                     p("The benefits of multiple logistic regression include 1) it is simple and relatively easy to interpret, and 2) it can include both categorical and continious predictors, 3) it works when the response variable is binary"),
                                     br(),
                                     h4("Drawbacks:"),
                                     p("The drawbacks of multiple logistic regression include that is assumes a linear relationship between predictors and the log-odds of the response, which is not always the case. Similarly, complex nonlinear relationships may not be captured. Another disadvantage is that multiple logistic regression can be prone to overfitting.  ")),
                                 # Classification Tree
                                 column(
                                     h2("Classification Tree"),
                                     width=12,
                                     h4("Explanation of method:"),
                                     p("A Classification Tree is a predictive model that use decision trees to split up the predictor space into regions. The goal is to predict or categorize group membership for each data point. In our case we want to predict the exoplanet disposition type of each data points based on certain features.  The tree structure consists of nodes and branches, and each node represents a feature or attribute, and each branch represents a decision rule based on that feature.  The main objective of building a classification tree is to partition the data into subsets in a way that maximizes the homogeneity of class labels within each subset. One commonly used measure is the Gini index. For a binary response (0=False Positive, 1=Candidate or Confirmed), within a given node (p=P(correct classification)):"),
                                     p("Gini: 2p(1-p) and Deviance: -2p*log(p) - 2(1-p)*log(1-p)."), 
                                     p("These measures quantify the impurity or uncertainty within the node based on the class proportions."),
                                     br(),
                                     h4("Benefits"),
                                     p("The benefits of using classification trees include easy interpretability. So they are relatively straightforward to understand, and visualize. Additionally, classification trees are non-parametric, therefore there are no assumptions that need to be met before using this method. Therefore, they can capture complex non-linear relationships."),
                                     br(),
                                     h4("Drawbacks:"),
                                     p("A drawback for classification trees is that they are prone to overfitting. Generally, it is a good approach to 'prune' or limit the tree depth to avoid this. Another drawback involves time, as it may take longer to train the model as the calculations can become more complex. Similarly, if the training data is not balanced, the training model could become biased towards certain classifications.")
                          ),
                          
                          column(
                                 # Random Forest
                                 h2("Random Forest"), width=12,
                                     h4("Explanation of method:"),
                                     p("Random forest is an ensemble learning technique. The random forest algorithm uses bagging, to resample from the data (bootstrap sample). These subsets ensure that each decision tree is trained on slightly different data. Next, multiple decision trees are created from these samples to create an uncorrelated forest and the results are then averaged. Unlike bagging, random forest modeling doesn't use all its predictors but uses a random subset of predictors for each bootstrap sample. The number of randomly selected predictors used for the prediction method is calculated by finding one-third of the number of variables. $$m = \\sqrt{p}$$ This random feature selection increases the diversity among the decision trees and prevents one feature from dominating the tree fits. In classification, the final prediction of the Random Forest is determined by a majority vote among the individual decision trees."),
                                     br(),
                                     h4("Benefits"),
                                     p("The benefits of Random Forest are that it is robust to outliers, and it generally provides better predictions compared to a single decision tree. The risk of overfitting a RF model is lower, and RF works with non-linear data."),
                                     br(),
                                     h4("Drawbacks"),
                                     p("Some drawbacks for the Random Forest mothod are that it is computationally slow amd complex. Additionally, the combined predictions of many decision trees make interpretability more difficult compared to a single decision tree. ")
                                 )
                          ) 
                           ),
                
                   # Model Fitting tab
                   tabItem("modeling_fitting",
                            # Split data into training and test sets
                          fluidRow(
                           column(width=4,
                                  box(width=12,
                                      numericInput(inputId = "proportion",
                                                   label = "Proportion of dataset to use for training data:",
                                                   value = 0.7,
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.1)
                                  ),
                          # Provide checkboxes to select variables
                          # Choose what variables you want in the model 
                          box(width=12,
                          checkboxGroupInput(inputId = "SelectModelVars",
                                             label = "Choose the variables you want included in the models:",
                                             choices = c("Orbital Period Days" = "OrbitalPeriodDays",
                                                         "Transit Epoch" = "TransitEpoch", 
                                                         "Impact Parameter" ="ImpactParameter", 
                                                         "Transit Duration (Hours)" = "TransitDurationHrs",
                                                         "Transit Depth (ppm)" = "TransitDepth_ppm", 
                                                         "Insolation Flux (Earth Flux)" = "InsolationFlux_Earthflux",
                                                         "Transit Signal to Noise" = "TransitSignalNoise",
                                                         "Stellar Effective Temperature (Kelvin)" = "StellarEffectiveTemperatureK", 
                                                         "Stellar Surface Gravity" ="StellarSurfaceGravity",
                                                         "Stellar Radius (solar radii)" = "StellarRadius_Solarradii"),
                                             selected = c("Orbital Period Days" = "OrbitalPeriodDays",
                                                          "Transit Epoch" = "TransitEpoch", 
                                                          "Impact Parameter" ="ImpactParameter", 
                                                          "Transit Duration (Hours)" = "TransitDurationHrs",
                                                          "Transit Depth (ppm)" = "TransitDepth_ppm", 
                                                          "Insolation Flux (Earth Flux)" = "InsolationFlux_Earthflux",
                                                          "Transit Signal to Noise" = "TransitSignalNoise",
                                                          "Stellar Effective Temperature (Kelvin)" = "StellarEffectiveTemperatureK", 
                                                          "Stellar Surface Gravity" ="StellarSurfaceGravity",
                                                          "Stellar Radius (solar radii)" = "StellarRadius_Solarradii"))),
                          box(width=12,
                              numericInput(inputId = "num_folds",
                                           label = "Specify the number of cross-validation folds:",
                                           value = 5,
                                           min = 1,
                                           max = 10,
                                           step = 1)
                          ),
                          box(width=12,
                              numericInput(inputId = "num_repeats",
                                           label = "Specify the number of cross-validation repeats:",
                                           value = 3,
                                           min = 1,
                                           max = 10,
                                           step = 1)
                          ),
                          box(width=12,
                              background = "green",
                              
                              # Button to press to run models
                              actionButton("GoGo", "Run Models"),
                              p("Please wait 1-2 minutes for results to display!")
                              
                          )
                   ),
                   column(width=8,
                          box(width=12,
                              # Displaying Accuracy table for each model
                              title = "Accuracy for Each Model Using the Training Dataset",
                              tableOutput("summary_Accuracy")
                          ),
                          box(width=12,
                              # Summary of predictors: Multiple Logisitic Regression 
                              title = "Model Summary: Generalized Linear Regression Model",
                              verbatimTextOutput("summary_Logic")
                          ),
                          box(width=12,
                              # Variable importance plot: Classification Tree
                              title = "Importance Scores of Top 10 Variables: Classification Tree Model Summary",
                              plotOutput("plot_ClassificationTree")
                          ),
                          box(width=12,
                              # Variable importance plot: Random forest
                              title = "Importance Scores of Top 10 Variables: Random Forest Model Summary",
                              plotOutput("plot_RandomForest")
                          ),
                          box(width=12,
                              title = "Comparing the Three Models on the Test Data Set",
                              tableOutput("model_summary"),
                              textOutput("model_summaryPrint")
                          )
                   )

                          
                          )),
# Prediction page
tabItem(tabName = "prediction",
        fluidRow(
          column(width=6,
                 box(width=12,
                     # Checkbox for using OrbitalPeriod choice
                     checkboxInput(inputId = "OrbitalCheck", 
                                   label = "Use Orbital Period for Predicting in our Model",
                                   value = TRUE),
                     
                     conditionalPanel(condition = "input.OrbitalCheck == 1",
                                      numericInput(inputId = "OrbitalChoice",
                                                  label = "Choose an Orbital Period in Days (1-550):",
                                                  min = 1,
                                                  max = 550,
                                                  step = 1,
                                                  value = 20)),
                     
                     # Checkbox for using TransitEpoch
                     checkboxInput(inputId = "transitCheck", 
                                   label = "Use Transit Epoch for Predicting in our model",
                                   value = TRUE),
                     
                     conditionalPanel(condition = "input.transitCheck == 1",
                                      # Choice of number of trees for predictions
                                      numericInput(inputId = "transitChoice",
                                                   label = "Choose Transit Epoch from 120 to 450:",
                                                   min = 120,
                                                   max = 450,
                                                   step = 1,
                                                   value = 137)),
                     
                     # Checkbox for using Impact Parameter
                     checkboxInput(inputId = "impactCheck", 
                                   label = "Use Impact Parameter for Predicting in our models",
                                   value = TRUE),
                     
                     conditionalPanel(condition = "input.impactCheck == 1",
                                      numericInput(inputId = "impactChoice",
                                                   label = "Choose Impact Parameter from 0 to 1.8:",
                                                   min = 0,
                                                   max = 1.8,
                                                   step = 0.01,
                                                   value = 0.54)),
                     
                     # Checkbox for using transit duration
                     checkboxInput(inputId = "transitDurationCheck", 
                                   label = "Use Transit Duration for Predicting in our models",
                                   value = TRUE),
                     
                     conditionalPanel(condition = "input.transitDurationCheck == 1",
                                      # Choose transit duration for predictions
                                      numericInput(inputId = "transitDurationChoice",
                                                   label = "Choose a Transit Duration (hours) from 0.5 to 30:",
                                                   min = 0.5,
                                                   max = 30,
                                                   step = 0.5,
                                                   value = 4)), 
                     
                     # Checkbox for using transit depth
                     checkboxInput(inputId = "transitDepthCheck", 
                                   label = "Use Transit Depth for Predicting in our models",
                                   value = TRUE),
                     
                     conditionalPanel(condition = "input.transitDepthCheck == 1",
                                      # Choose transit depth for predictions
                                      numericInput(inputId = "transitDepthChoice",
                                                   label = "Choose a Transit Depth (ppm) from 0 to 10,000:",
                                                   min = 0,
                                                   max = 10000,
                                                   step = 100,
                                                   value = 500)), 
                     # Checkbox for using InsolationFlux
                     checkboxInput(inputId = "InsolationFluxCheck", 
                                   label = "Use Insolation Flux for Predicting in our models",
                                   value = TRUE),
                     
                     conditionalPanel(condition = "input.InsolationFluxCheck == 1",
                                      # Choose InsolationFlux for predictions
                                      numericInput(inputId = "InsolationFluxChoice",
                                                   label = "Choose a Insolation Flux from 0 to 4,000:",
                                                   min = 0,
                                                   max = 4000,
                                                   step = 50,
                                                   value = 200)), 
                     # Checkbox for using Transit Signal to Noise
                     checkboxInput(inputId = "TransitSignalNoiseCheck", 
                                   label = "Use Transit Signal to Noise for Predicting in our models",
                                   value = TRUE),
                     
                     conditionalPanel(condition = "input.TransitSignalNoiseCheck == 1",
                                      # Choose TransitSignalNoiseCheck for predictions
                                      numericInput(inputId = "TransitSignalNoiseChoice",
                                                   label = "Choose a Transit Signal to Noise value from 0 to 100:",
                                                   min = 0,
                                                   max = 100,
                                                   step = 2,
                                                   value = 24)), 
                     
                     # Checkbox for using Stellar Temperature
                     checkboxInput(inputId = "StellarTemperatureCheck", 
                                   label = "Use Stellar Temperature (K) for Predicting in our models",
                                   value = TRUE),
                     
                     conditionalPanel(condition = "input.StellarTemperatureCheck == 1",
                                      # Choose Stellar Temperature for predictions
                                      numericInput(inputId = "StellarTemperatureChoice",
                                                   label = "Choose a Stellar Temperature (K) value from 3,000 to 15,000:",
                                                   min = 3000,
                                                   max = 15000,
                                                   step = 1000,
                                                   value = 6000)),    
                     # Checkbox for using Stellar Surface Gravity
                     checkboxInput(inputId = "StellarGravityCheck", 
                                   label = "Use Stellar Surface Gravity for Predicting in our models",
                                   value = TRUE),
                     
                     conditionalPanel(condition = "input.StellarGravityCheck == 1",
                                      # Choose Stellar Gravity for predictions
                                      numericInput(inputId = "StellarGravityChoice",
                                                   label = "Choose a Stellar Gravity value from 4.0 to 4.6:",
                                                   min = 4,
                                                   max = 4.6,
                                                   step = 0.1,
                                                   value = 4.4)),
                     # Checkbox for using Stellar Radius
                     checkboxInput(inputId = "StellarRadiusCheck", 
                                   label = "Use Stellar Radius for Predicting in our models",
                                   value = TRUE),
                     
                     conditionalPanel(condition = "input.StellarRadiusCheck == 1",
                                      # Choose Stellar Radius for predictions
                                      numericInput(inputId = "StellarRadiusChoice",
                                                   label = "Choose a Stellar Radius (Solar Radii) value from 0.5 to 2.5:",
                                                   min = 0.5,
                                                   max = 2.5,
                                                   step = 0.1,
                                                   value = 1.0)),
                 )
          ),
          
          
          column(width=6,
                 box(width=12,
                     h4("Choose a model to use for prediction:"),
                     # Choose model type for predictions
                     radioButtons(inputId = "modeltype",
                                  label = "Choose Model Type:",
                                  choices = c("Logistic Regression Model" = 1,
                                              "Classification Tree Model" = 2,
                                              "Random Forest Model" = 3),
                                  selected = 1)
                 ),
                 box(width=12,
                     # Button to press to run models
                     actionButton("Predict", "Run Selected Predictive Model"),
                     p("Please wait 1-2 minutes for results to display!")
                 ),
                 box(width=12,
                     h4(textOutput("PredictionResults")
                 )
                
                 
          )
        )
)),


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
  
  # Reads in data and renames
  kepler <- reactive({
    keplerData <- read_csv("keplerExoplanetCumulative.csv")
    
    keplerData <- keplerData %>% select(-rowid) %>%
      rename(
        "KepID" = kepid,
        "KOI_Name" = kepoi_name,
        "Kepler_Name" = kepler_name,
        "ExoplanetArchiveDisposition" = koi_disposition,
        "DispositionUsingKeplerData" = koi_pdisposition,
        "DispositionScore" = koi_score,
        "NotTransit-LikeFalsePositiveFlag" = koi_fpflag_nt,
        "StellarEclipseFlag" = koi_fpflag_ss,
        "CentroidOffsetFalsePositiveFlag" = koi_fpflag_co,
        "EphemerisMatchIndicatesContaminationFalsePositiveFlag" = koi_fpflag_ec,
        "OrbitalPeriodDays" = koi_period,
        "OrbitalPeriodUpperErrorDays" = koi_period_err1,
        "OrbitalPeriodLowerErrorDays" = koi_period_err2,
        "TransitEpoch" = koi_time0bk,
        "TransitEpochUpperError" = koi_time0bk_err1,
        "TransitEpochLowerError" = koi_time0bk_err2,
        "ImpactParameter" = koi_impact,
        "ImpactParameterUpperError" = koi_impact_err1,
        "ImpactParameterLowerError" = koi_impact_err2,
        "TransitDurationHrs" = koi_duration,
        "TransitDurationUpperErrorHrs" = koi_duration_err1,
        "TransitDurationLowerErrorHrs" = koi_duration_err2,
        "TransitDepth_ppm" = koi_depth,
        "TransitDepthUpperError_ppm" = koi_depth_err1,
        "TransitDepthLowerError_ppm" = koi_depth_err2,
        "PlanetaryRadius_Earthradii" = koi_prad,
        "PlanetaryRadiusUpperError_Earthradii" = koi_prad_err1,
        "PlanetaryRadiusLowerError_Earthradii" = koi_prad_err2,
        "EquilibriumTemperature_K" = koi_teq,
        "EquilibriumTemperatureUpperErrorK" = koi_teq_err1,
        "EquilibriumTemperatureLowerErrorK" = koi_teq_err2,
        "InsolationFlux_Earthflux" = koi_insol,
        "InsolationFluxUpperError_Earthflux" = koi_insol_err1,
        "InsolationFluxLowerError_Earthflux" = koi_insol_err2,
        "TransitSignalNoise" = koi_model_snr,
        "TCEPlanetNumber" = koi_tce_plnt_num,
        "TCEDeliverName" = koi_tce_delivname,
        "StellarEffectiveTemperatureK" = koi_steff,
        "StellarEffectiveTemperatureUpperErrorK" = koi_steff_err1,
        "StellarEffectiveTemperatureLowerErrorK" = koi_steff_err2,
        "StellarSurfaceGravity" = koi_slogg,
        "StellarSurfaceGravityUpperError" = koi_slogg_err1,
        "StellarSurfaceGravityLowerError" = koi_slogg_err2,
        "StellarRadius_Solarradii" = koi_srad,
        "StellarRadiusUpperError_Solarradii" = koi_srad_err1,
        "StellarRadiusLowerError_Solarradii" = koi_srad_err2,
        "RA_decimaldegrees" = ra,
        "Dec_decimaldegrees" = dec,
        "Kepler_bandMag" = koi_kepmag
      )
    
    return(keplerData)
  })
  
  # Modeling Fitting Output
  ExoModel <- eventReactive(input$GoGo, {
    
    # use the selected variable in our model
    ModelVarList <- input$SelectModelVars
    
    #define the response variable
    keplerModel <- kepler() %>% 
      mutate(KeplarExoplanetDisposition_BinaryYN = ifelse(ExoplanetArchiveDisposition == "CANDIDATE" | ExoplanetArchiveDisposition == "CONFIRMED", 1, 0)) %>%
      select(KeplarExoplanetDisposition_BinaryYN, ModelVarList)
    # Convert the outcome variable to a factor with two levels
    keplerModel$KeplarExoplanetDisposition_BinaryYN <- factor(keplerModel$KeplarExoplanetDisposition_BinaryYN, levels = c(0, 1))
    # remove missings
    keplerModel <- na.omit(keplerModel)
    return(keplerModel)
  })
  
  # Creates training Index with the proportion specified
  trainIndex <- eventReactive(input$GoGo, {
    set.seed(717)
    kepler <- ExoModel()
    trainingIndex <- createDataPartition(kepler$KeplarExoplanetDisposition_BinaryYN, p = input$proportion, list = FALSE)
  })
  
  # Create training dataset
  trainData <- eventReactive(input$GoGo, {
    
    kepler <- ExoModel()
    
    KeplarTrain <- kepler[trainIndex(), ]
  })
  
  # Create test dataset
  testData <- eventReactive(input$GoGo, {
    kepler <- ExoModel()
    KeplarTest <- kepler[-trainIndex(), ]
  })
  
  # Fits the GLM Regression Model. Based on CV and user defined folds and repeats
  Logistic <- eventReactive(input$GoGo, {
    
    # Define the formula for the model using the user-selected variables
    formula <- as.formula(paste("KeplarExoplanetDisposition_BinaryYN ~", paste(input$SelectModelVars, collapse = "+")))
    
    Logistic_model <- train(formula, data = trainData(),
                      method = "glm",
                      family = "binomial", 
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", 
                                               number = input$num_folds))
    return(Logistic_model)
            })
  
  # Display the summary of the Generalized Linear Regression model
  output$summary_Logic <- renderPrint({
    summary(Logistic())
  })
  
  #Classification tree model:
  # Fits the Classification Tree Model. Based on CV and user defined folds and repeats
  ClassificationTree <- eventReactive(input$GoGo, {
    
    # Define the formula for the model using the user-selected variables
    formula <- as.formula(paste("KeplarExoplanetDisposition_BinaryYN ~", paste(input$SelectModelVars, collapse = "+")))
    # Define the values of cp to tune
    cp_values <- seq(0, 0.1, by = 0.001) 
    
    ClassificationTree_model <- train( formula, data = trainData(),
      method = "rpart",
      trControl = trainControl(method = "cv", number = input$num_folds,repeats = input$num_repeats),
      tuneGrid = data.frame(cp = cp_values))
    
    return(ClassificationTree_model)
  })
  
  # Creates the variable importance plot for the regression tree model
  output$plot_ClassificationTree <- renderPlot({
    CT_Imp <- varImp(ClassificationTree())
    plot(CT_Imp, top = 10)
  })
  
  
  # Fits the Random Forest Model. Based on CV and user defined folds and repeats
  RandomForestModel <- eventReactive(input$GoGo, {
    
    # Define the formula for the model using the user-selected variables
    formula <- as.formula(paste("KeplarExoplanetDisposition_BinaryYN ~", paste(input$SelectModelVars, collapse = "+")))
    
    # Parallel Processing
    num_cores <- detectCores()-1
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)
    
    RandomForest_model <- train(
      formula, data = trainData(),
      method = "rf",
      trControl = trainControl(method = "cv", number = input$num_folds, 
                               repeats = input$num_repeats),
      tuneGrid = data.frame(mtry = 1:10))
    # Stop parallel processing
    stopCluster(cl)
    registerDoSEQ()
    return(RandomForest_model)
  })
  
  # Creates the variable importance plot for the Random Forest model
  output$plot_RandomForest <- renderPlot({
    RF_Imp <- varImp(RandomForestModel())
    plot(RF_Imp, top = 10)
  })
  
  # Accuracy table for the 3 models
  output$summary_Accuracy <- renderTable({
    
    LogicFit <- Logistic()
    CTFit <- ClassificationTree()
    RFFit <- RandomForestModel()
    
    LogicAcc <- LogicFit$results %>% select(Accuracy)
    CTAcc <- CTFit$results %>% filter(cp == CTFit$bestTune$cp) %>% select(Accuracy)
    RFAcc <- RFFit$results %>% filter(mtry == RFFit$bestTune$mtry) %>% select(Accuracy)
    
    Summary <- cbind(LogicAcc, CTAcc , RFAcc )
    colnames(Summary) <- list("Logistic Regression", "Classification Tree", "Random Forest")
    Summary
  })
  
  ## DETERMINE THE BEST MODEL
  
  FindBestModel <- reactive({
    
    LogicFit <- Logistic()
    CTFit <- ClassificationTree()
    RFFit <- RandomForestModel()
    
    testData <- testData()
    
    # Predict Planet Disposition using the models and test data
    predFitLogic <- predict(LogicFit, newdata = testData)
    predFitCT <- predict(CTFit, newdata = testData)
    predFitRF <- predict(RFFit, newdata = testData)
    
    # Create confusion matrices for each model
    logic_cm <- confusionMatrix(predFitLogic, testData$KeplarExoplanetDisposition_BinaryYN)
    ct_cm <- confusionMatrix(predFitCT, testData$KeplarExoplanetDisposition_BinaryYN)
    rf_cm <- confusionMatrix(predFitRF, testData$KeplarExoplanetDisposition_BinaryYN)
    
    # Extract the accuracy values from the confusion matrices
    accuracy_logic <- logic_cm$overall["Accuracy"]
    accuracy_ct <- ct_cm$overall["Accuracy"]
    accuracy_rf <- rf_cm$overall["Accuracy"]
    
    # Determine the best model based on accuracy
    best_model <- which.max(c(accuracy_logic, accuracy_ct, accuracy_rf))
    
    # Create a data frame to store the model names and accuracy
    model_data <- data.frame(
      Model = c("Logistic Regression Model", "Classification Tree Model", "Random Forest Model"),
      Accuracy = c(accuracy_logic, accuracy_ct, accuracy_rf)
    )
    
    # Get the best model name and accuracy
    best_model_name <- model_data$Model[best_model]
    best_model_accuracy <- model_data$Accuracy[best_model]
    
    # Return the model_data for use in outputs
    model_data
  })
  
  # Reactive expression for model_data
  model_data <- reactive({
    FindBestModel()
  })
  
  # Render the model_data as a table
  output$model_summary <- renderTable({
    model_data()
  })
  
  # Render the best model information as text
  output$model_summaryPrint <- renderText({
    # Get the best model information
    best_model_info <- model_data() %>% filter(Accuracy == max(Accuracy)) %>% select(Model, Accuracy)
    
    # Create a text string to print the best model information
    best_model_text <- paste("The best model based on accuracy is the ", best_model_info$Model, 
                             "with an accuracy of ", round(best_model_info$Accuracy, 3))
    
    # Return the text
    best_model_text
  })

  ## DATA TAB ##
  # Update choices for checkboxGroupInput (Data Tab) based on available columns
  observe({
    columns <- names(kepler())
    updateCheckboxGroupInput(session, "selectedColumns", choices = columns, selected = columns)
  })
  
  # Filter and subset the data based on user selection (Data Tab)
    filteredData <- reactive({
      kepler <- kepler()
      
      if (input$dataDisposition != "No") {
        kepler <- kepler %>%
          filter(ExoplanetArchiveDisposition == input$dataDisposition)
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
    
    # Filter and subset the data based on user selection (Data Exploration Tab)
    filterDataExplore <- reactive({
      kepler <- kepler()
      
      if (input$dispositionType != "All") {
        kepler <- kepler %>%
          filter(ExoplanetArchiveDisposition == input$dispositionType)
      }
      
      # Subsetting columns based on user selection
      kepler <- kepler %>%
        select(ExoplanetArchiveDisposition, input$selectedVariable)
      
      # Remove missing values for the selected variable
      kepler <- kepler %>%
        drop_na(all_of(input$selectedVariable))  # Use all_of()
      
      # Drop the upper and lower 2.5% of data for the selected variable
      selected_var <- input$selectedVariable
      lower_bound <- quantile(kepler[[selected_var]], 0.025, na.rm = TRUE)  # 2.5th percentile
      upper_bound <- quantile(kepler[[selected_var]], 0.975, na.rm = TRUE)  # 97.5th percentile
      
      # Filter the data to exclude values beyond the lower and upper bounds
      kepler <- kepler %>%
        filter(.data[[selected_var]] >= lower_bound & .data[[selected_var]] <= upper_bound)

  return(kepler)
})
    
    # Render the exploration plot
    output$explorationPlot <- renderPlot({
      # Filter data based on selected variable and remove top and bottom 5% values
        keplerData <- filterDataExplore()
        selected_var <- input$selectedVariable
        
        binwidth_mapping <- list(
          DispositionScore = 0.1,
          OrbitalPeriodDays = 2,
          TransitEpoch = 1,
          ImpactParameter = 0.1,
          TransitDurationHrs = 2,
          TransitDepth_ppm = 100,
          PlanetaryRadius_Earthradii = 0.2,
          StellarEffectiveTemperatureK = 100,
          StellarSurfaceGravity = 0.1
        )
        
      # Plotting the histogram or scatter plot based on user input
      if (input$plotType == "Histogram") {
        binwidth <- binwidth_mapping[[selected_var]]
        ggplot(keplerData, aes_string(x = selected_var)) +
          geom_histogram(binwidth = binwidth, fill = "red", color = "black") +
          labs(x = selected_var, y = "Frequency") +
          theme_classic()
      } 
      else if (input$plotType == "Density Plot") {
          ggplot(keplerData, aes_string(x = selected_var)) +
            geom_density(fill = "blue", alpha = 0.5) +
            labs(x = selected_var, y = "Density") +
            theme_classic()
        }
        
        
        
    })
    
    # Render the summary table
    output$summaryTable <- renderTable({
      keplerData <- filterDataExplore()
      
      # Calculate summary statistics or contingency table based on user selection
      if (input$summaryType == "Summary Statistics") {
        # Calculate summary statistics (mean, median, etc.) for the selected variable
        selected_var <- input$selectedVariable
        summary_stats <- keplerData %>% 
          summarise(
            Statistics = paste0("Summary Statistics for: ", selected_var),
            N = n(),
            Mean = mean(.data[[selected_var]], na.rm = TRUE),
            Median = median(.data[[selected_var]], na.rm = TRUE),
            Std.Dev = sd(.data[[selected_var]], na.rm = TRUE),
            Minimum = min(.data[[selected_var]], na.rm = TRUE),
            Maximum = max(.data[[selected_var]], na.rm = TRUE)
          )
        return(summary_stats)
      } else {
        # Calculate a contingency table for the selected variable and disposition type
        selected_var <- input$selectedVariable
       
        contingency_table <- keplerData %>%
          group_by(ExoplanetArchiveDisposition) %>%
          summarise(Statistics = paste0("Summary Statistics for: ", selected_var),
                    N = n(),
                    Mean = mean(.data[[selected_var]], na.rm = TRUE),
                    Median = median(.data[[selected_var]], na.rm = TRUE),
                    Std.Dev = sd(.data[[selected_var]], na.rm = TRUE),
                    Minimum = min(.data[[selected_var]], na.rm = TRUE),
                    Maximum = max(.data[[selected_var]], na.rm = TRUE)
          )
        
        
        return(contingency_table)
      }
    })
    
  
  ## PREDICTION TAB
  SelectedPredictVars <- reactive({
    # Create a character vector to store the selected predictor variables
    selected_vars <- character()
    
    # Append the selected predictor variable names to the vector based on user inputs
    if (input$OrbitalCheck == 1) selected_vars <- c(selected_vars, "OrbitalPeriodDays")
    if (input$transitCheck == 1) selected_vars <- c(selected_vars, "TransitEpoch")
    if (input$impactCheck == 1) selected_vars <- c(selected_vars, "ImpactParameter")
    if (input$transitDurationCheck == 1) selected_vars <- c(selected_vars, "TransitDurationHrs")
    if (input$transitDepthCheck == 1) selected_vars <- c(selected_vars, "TransitDepth_ppm")
    if (input$InsolationFluxCheck == 1) selected_vars <- c(selected_vars, "InsolationFlux_Earthflux")
    if (input$TransitSignalNoiseCheck == 1) selected_vars <- c(selected_vars, "TransitSignalNoise")
    if (input$StellarTemperatureCheck == 1) selected_vars <- c(selected_vars, "StellarEffectiveTemperatureK")
    if (input$StellarGravityCheck == 1) selected_vars <- c(selected_vars, "StellarSurfaceGravity")
    if (input$StellarRadiusCheck == 1) selected_vars <- c(selected_vars, "StellarRadius_Solarradii")
    
    # Return the character vector containing the selected predictor variable names
    return(selected_vars)
  })
  
  # Modeling Fitting Output
  ExoModel2 <- eventReactive(input$Predict, {
    
    # use the selected variable in our model
    ModelVarList <- SelectedPredictVars()
    
    #define the response variable
    keplerModel <- kepler() %>% 
      mutate(KeplarExoplanetDisposition_BinaryYN = ifelse(ExoplanetArchiveDisposition == "CANDIDATE" | ExoplanetArchiveDisposition == "CONFIRMED", 1, 0)) %>%
      select(KeplarExoplanetDisposition_BinaryYN, ModelVarList)
    # Convert the outcome variable to a factor with two levels
    keplerModel$KeplarExoplanetDisposition_BinaryYN <- factor(keplerModel$KeplarExoplanetDisposition_BinaryYN, levels = c(0, 1))
    # remove missings
    keplerModel <- na.omit(keplerModel)
    return(keplerModel)
  })
  
  # Creates training Index with the proportion specified
  trainIndex2 <- eventReactive(input$Predict, {
    set.seed(717)
    kepler <- ExoModel2()
    trainingIndex <- createDataPartition(kepler$KeplarExoplanetDisposition_BinaryYN, p = 0.7, list = FALSE)
  })
  
  # Create training dataset
  trainData2 <- eventReactive(input$Predict, {
    
    kepler <- ExoModel2()
    
    KeplarTrain <- kepler[trainIndex2(), ]
  })
  
  # Create test dataset
  testData2 <- eventReactive(input$Predict, {
    kepler <- ExoModel2()
    KeplarTest <- kepler[-trainIndex2(), ]
  })
  
  
  # Reactive function for creating the prediction data
  predData <- reactive({
    # Create a data frame with variables based on user inputs
    prediction_data <- data.frame(
      OrbitalPeriodDays = ifelse("OrbitalPeriodDays" %in% SelectedPredictVars(), input$OrbitalChoice, NA),
      TransitEpoch = ifelse("TransitEpoch" %in% SelectedPredictVars(), input$transitChoice, NA),
      ImpactParameter = ifelse("ImpactParameter" %in% SelectedPredictVars(), input$impactChoice, NA),
      TransitDurationHrs = ifelse("TransitDurationHrs" %in% SelectedPredictVars(), input$transitDurationChoice, NA),
      TransitDepth_ppm = ifelse("TransitDepth_ppm" %in% SelectedPredictVars(), input$transitDepthChoice, NA),
      InsolationFlux_Earthflux = ifelse("InsolationFlux_Earthflux" %in% SelectedPredictVars(), input$InsolationFluxChoice, NA),
      TransitSignalNoise = ifelse("TransitSignalNoise" %in% SelectedPredictVars(), input$TransitSignalNoiseChoice, NA),
      StellarEffectiveTemperatureK = ifelse("StellarEffectiveTemperatureK" %in% SelectedPredictVars(), input$StellarTemperatureChoice, NA),
      StellarSurfaceGravity = ifelse("StellarSurfaceGravity" %in% SelectedPredictVars(), input$StellarGravityChoice, NA),
      StellarRadius_Solarradii = ifelse("StellarRadius_Solarradii" %in% SelectedPredictVars(), input$StellarRadiusChoice, NA)
    )
    
    return(prediction_data)
  })
    
  # Reactive function for fitting the GLM Regression Model
  Logistic2 <- eventReactive(input$Predict, {
    # Fit the GLM Regression Model
    Logistic_model <- train(KeplarExoplanetDisposition_BinaryYN ~ ., data = trainData2(),
                            method = "glm",
                            preProcess = c("center", "scale"),
                            trControl = trainControl(method = "cv", 
                                                     number = 5))
    return(Logistic_model)
  })
  
  
  ClassificationTree2 <- eventReactive(input$Predict, {
    # Define the values of cp to tune
    cp_values <- seq(0, 0.1, by = 0.001) 
    
    ClassificationTree_model <- train(KeplarExoplanetDisposition_BinaryYN ~ ., 
                                      data = trainData2(),
                                      method = "rpart",
                                      trControl = trainControl(method = "cv", number = 5, 
                                                               repeats = 2),
                                      tuneGrid = data.frame(cp = cp_values))
    return(ClassificationTree_model)
  })
  
  # Fits the Random Forest Model.
  RandomForestModel2 <- eventReactive(input$Predict, {
    # Parallel Processing
    num_cores <- detectCores()-1
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)
    
    RandomForest_model <- train(KeplarExoplanetDisposition_BinaryYN ~ ., 
                                data = trainData2(),
                                method = "rf",
                                trControl = trainControl(method = "cv", number = 5, repeats = 3),
                                tuneGrid = data.frame(mtry = 1:10))
    # Stop parallel processing
    stopCluster(cl)
    registerDoSEQ()
    
    return(RandomForest_model)
  })
  
  
  # Reactive function for making predictions using test data
  prediction_results <- eventReactive(input$Predict, {
    # Get the selected model type
    model_type <- input$modeltype
    
    # Get the prediction data
    prediction_data <- predData()
    LogicFit <- Logistic2()
    CTFit <- ClassificationTree2()
    RFFit <- RandomForestModel2()
    
    # Make predictions based on the selected model type and the prediction data
    if (model_type == 1) {
      pred <- predict(LogicFit, newdata = prediction_data)
    } else if (model_type == 2) {
      pred <- predict(CTFit, newdata = prediction_data)
    } else if (model_type == 3) {
      pred <- predict(RFFit, newdata = prediction_data)
    } else {
      pred <- NULL
    }
    
    return(pred)
  })
  
  output$PredictionResults <- renderPrint({
    # Get the prediction results
    pred <- prediction_results()
    model_type <- input$modeltype
    if (!is.null(pred)) {
      # Convert the predictions to meaningful labels
      result_labels <- ifelse(pred == 0, "False Positive", "Candidate Planet")
      
      # Display the selected model type
      if (model_type == "1") {
        model_type_message <- "Logistic Regression Model"
      } else if (model_type == "2") {
        model_type_message <- "Classification Tree Model"
      } else if (model_type == "3") {
        model_type_message <- "Random Forest Model"
      } else {
        model_type_message <- "Invalid model type selected. Please choose a valid model type (1 for Logistic Regression, 2 for Classification Tree, 3 for Random Forest)."
      }
      
      # Display the prediction result
      paste0("Given these parameters, our prediction using a ", model_type_message, " for exoplanet disposition is: ", result_labels)
    } else {
      # If pred is NULL (invalid model_type selected), display an error message
      "Invalid model type selected. Please choose a valid model type (1 for Logistic Regression, 2 for Classification Tree, 3 for Random Forest)."
    }
  })
  
  
})

# Run the application 
shinyApp(ui = ui, server = server)
