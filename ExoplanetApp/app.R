# Programmer: Michael Bradshaw
# Date: July 17, 2023
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
                                 h3("Multiple Linear Regression"),
                                 width=12,
                                     h4("Explanation of method:"),
                                     p("Multiple linear regression is a statistical method that attempts to model a linear relationship between two variables, the response, and the predictors/explanatory variables. Typically, we use the least-squares method, which minimizes the sum of squared residuals to find the best fitted line between the response and the predictors. These models are usually written as $$Y_i=\\beta_0 + \\beta_1 x_{1i} + ... + \\beta_n x_{ni}$$"),  
                                     p("$$where~Y_i ~ is~our~response~variable.$$"),
                                     p("$$\\beta_0~is~the~intercept~term.$$"),
                                     p("$$\\beta_1 + ... + \\beta_n~are~the~coefficients~for~each~predictor.$$"),
                                     br(),
                                     h4("Benefits:"),
                                     p("The benefits of multiple linear regression include 1) it is simple and relatively easy to interpret, and 2) it can include both categorical and continious predictors"),
                                     br(),
                                     h4("Drawbacks:"),
                                     p("The drawbacks of multiple linear regression include that is assumes a linear relationship between the response and predictors, which is not always the case. Similarly, complex nonlinear relationships may not be captured. Another disadvantage is that multiple linear regression is sensitive to outliers which can significantly affect the model's performance and the model's assumptions. Additionally, leverage points (think extreme value on a predictor variable) and influential points (removing the observation dramatically changes the estimates of the regression coefficient) can also have a strong impact on the regression model.  ")),
                                 
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
                              actionButton("GoGo", "Run Models")
                          )
                   ),
                   column(width=8,
                          box(width=12,
                              # Displaying Accuracy table for each model
                              title = "Accuracy for Each Model Using the Training Dataset",
                              tableOutput("summary_Accuracy")
                          ),
                          box(width=12,
                              # Summary of predictors: Multiple Linear Regression 
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
      trControl = trainControl(method = "cv", number = input$num_folds, repeats = input$num_repeats),
      tuneGrid = data.frame(mtry = 1:10))
    
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)
    
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
    
    # Filter and subset the data based on user selection (Data Tab)
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
