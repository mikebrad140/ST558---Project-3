# ST558---Project-3
Author: Michael Bradshaw

## Purpose: 
This repo contains code for this project in the app.R file. This creates a shiny app that is used to explore Kepler exoplanet data and model it. 

## R packages used:  

* [`shiny`](https://shiny.rstudio.com/): This package is used to build the interactive web app. 
* [`shinydashboard`](https://rstudio.github.io/shinydashboard/): This package is used to create dashboards, specifically for the tabs and different pages. 
* [`TidyVerse`](https://www.tidyverse.org/): This package was loaded to retrieve the packages below.
  * [`ggplot2`](https://ggplot2.tidyverse.org/): This package was used to create our plots for the exploratory data analysis.
  * [`dplyr`](https://dplyr.tidyverse.org/): This package was used to `select`, `filter`, and `summarise` our data.
* [`caret`](https://cran.r-project.org/web/packages/caret/index.html): This package was used for training and plotting regression models.
* [`DT`](https://rstudio.github.io/DT/): This package was used to display the data table.
* [`doParallel`](https://cran.r-project.org/web/packages/doParallel/index.html): This package was used to execute R code in parallel, utilizing multiple cores or processors.

## Code to install packages:
`install.packages(c("shiny", "shinydashboard", "readxl", "lubridate", "tidyverse", "caret", "DT", "doParallel"))`

## Code to create app:
`shiny::runGitHub("ST558---Project-3", "mikebrad140", subdir = "ExoplanetApp")`
