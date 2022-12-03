
library(caret)
library(shiny)
library(DT)
library(tidyverse)
library(shinydashboard)
#shinyUI(fluidPage(
#  titlePanel("Wine Quality"),
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "About", icon = icon("info")),
      menuItem("Data Exploration", tabName = "EDA", icon = icon("signal")),
      menuItem("Modeling", tabName = "Model", icon = icon("play")),
      menuItem("Data", tabName = "Data", icon = icon("save"))
    )
  )
  
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "About",
              mainPanel(h2("About this App"), dataTableOutput("tab")
              )
              
      ),
      
      tabItem(tabName = "EDA",
              sidebarLayout(
              sidebarPanel(h1("Data Exploration"),
                           selectInput("filter_choice", "Filter by Wine Type",
                                       choices = c("Include All", 
                                                   "Red Only",
                                                   "White Only")),
                           selectInput("plot_choice", "Select Plot Type", 
                                       choices = c("Histogram", "Scatterplot")),
                           selectInput("x_choice", "Select Plot Variable",
                                       choices = c("alcohol", 
                                                   "volatile_acidity", 
                                                   "type", 
                                                   "sulphates", 
                                                   "residual_sugar",
                                                   "total_SO2"),),
                           selectInput("summary_choice", "Choose summary statistic",
                                       choices = c("mean", "median"))
                           ),
              mainPanel(h1("Text"), plotOutput("testplot"),
                        dataTableOutput("tab"))
              )
              
      ),
      tabItem(tabName = "Model",
              #h2("Modeling")
              mainPanel(
                title = "Modeling",
                tabsetPanel(type = "tabs",
                            tabPanel("Model Info", 
                                     h2("Logistic Regression"),
                                     h4("Insert info about regression model"),
                                     h2("Classification Tree"),
                                     h4("insert info about model"),
                                     h2("Random Forest"),
                                     h4("Insert info about model")
                            ),
                            tabPanel("Modeling", 
                                     sidebarPanel(
                                       h4("Select variables to include in model"),
                                       checkboxGroupInput("pred_choice",
                                                          label = "Variable Options",
                                                          choices = c("alcohol", 
                                                                      "volatile_acidity", 
                                                                      "type", 
                                                                      "sulphates", 
                                                                      "residual_sugar",
                                                                      "total_SO2"), 
                                                          selected = "alcohol"),
                                       h4("Select model tuning parameters"),
                                       sliderInput("p_choice", label = "Proportion for data split",
                                                   min=0.1, max=0.9, step=0.05, value = 0.7),
                                       sliderInput("cp_choice", label = "CP for Classification Tree fit",
                                                   min=0, max = 0.1, step = 0.005, value = 0.05),
                                       sliderInput("m_choice", label = "m for Random Forest fit",
                                                   min = 1, max = 5, step = 1, value = 3)
                                     )),
                            tabPanel("Prediction", 
                                     sidebarPanel(h3("Select model for prediction"),
                                                  selectInput("model_choice", 
                                                              label = "",
                                                              choices = 
                                                                c("Regression", 
                                                                  "Tree", 
                                                                  "Random Forest"))
                                                  #Need to figure out selecting values of predictors.
                                                  )
                                     )
                )
              )
              
      ),
      #Right now this just has test stuff in it
      tabItem(tabName = "Data",
              #h2("Data")
              mainPanel(
                # Output: Tabset w/ plot, summary, and table ----
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", "plotOutput"),
                            tabPanel("Summary", "verbatimTextOutput"),
                            tabPanel("Table", dataTableOutput("tab"), plotOutput("dataPlot"))
                ))
      )
    )
  )
  
  # Put them together into a dashboardPage
  dashboardPage(
    dashboardHeader(title = "Wine App"),
    sidebar,
    body
  )

