# FinalProject

## App Description
This app can be used to perform data exploration, modeling, and prediction using data about wine from the UCI Machine Learning Repository. The app contains the following four tabs:  

1. An informational **About** tab
2. A **Data Exploration** tab with user configurable plots and summary statistics
3. A **Modeling** tab to compare 3 different predictive models: logistic regression, classification tree, and random forest
4. A **Data** tab that allows the user to subset the dataset and download it

## Required Packages
`shiny`  
`shinydashboard`  
`caret`  
`tidyverse`  
`DT`  
`png`  
`randomForest`  

## Code to Install Packages
install.packages(c("shiny", "shinydashboard", "caret", "DT", "tidyverse", "png", "randomForest"))

## Code for running app
shiny::runGitHub("FinalProject", "nmlevin11")
