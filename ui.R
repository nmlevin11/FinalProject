
library(caret)
library(shiny)
library(shinythemes)
library(DT)
library(shinydashboard)
library(png)
library(randomForest)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "About", icon = icon("info")),
    menuItem("Data Exploration", tabName = "EDA", icon = icon("signal")),
    menuItem("Modeling", tabName = "Model", icon = icon("play")),
    menuItem("Data", tabName = "Data", icon = icon("save"))
  )
)

body <- dashboardBody(width = 600,
  tabItems(
    tabItem(tabName = "About",
            sidebarLayout(sidebarPanel(img(src = "wine glass.png", height = "100%", 
                                           width = "100%", align = "center")),
              mainPanel(h2("About this App"),
                      h4("Have you ever sat down at the end of the day with a nice 
                         glass of wine and wondered if you could make the experience
                         nerdier? If the answer is yes, this may be just the app for 
                         you. The purpose of this app is to model wine quality based
                         on physiochemical tests. The app allows the user to perform
                         exploratory data analysis, compare three different models,
                         and make predictions for the quality result of future data
                         points based on user-inputted values."),
                      h4("The data used in this app comes from two datasets, one for
                         red wine and one for white, from the UCI Machine Learning
                         Repository. The full datasets includes 11 physiochemical values,
                         like sulphates and percent alcohol, and a sensory quality
                         score for each wine. For the purposes of this analysis, quality 
                         has been dichotomized such that a score greater than 5 is 
                         considered 'good' and a score of 5 or below is considered 'bad.'"),
                      h4("The full dataset can be accessed", a(href = 
                      "https://archive.ics.uci.edu/ml/datasets/Wine+Quality", "here")),
                      h4("The app contains 4 tabs:"), 
                      h4("1. this About tab"),
                      h4("2. a Data Exploration tab with user configurable plots and
                         summary statistics"),
                      h4("3. A Modeling tab to compare 3 different predictive models,
                         logistic regression, classification tree, and random forest."),
                      h4("4. a Data tab that allows the user to subset the dataset and
                         download it.")
                      )
            )
    ),
    
    tabItem(tabName = "EDA", 
            sidebarLayout(
              sidebarPanel(h1("Data Exploration"),
                           selectInput("summary_choice", "Choose summary statistic for table",
                                       choices = c("mean", "median")),
                           selectInput("x_choice", "Select variable to summarize",
                                       choices = c("alcohol", 
                                                   "volatile_acidity", 
                                                   "sulphates", 
                                                   "residual_sugar",
                                                   "total_SO2")),
                           selectInput("plot_choice", "Select Plot Type", 
                                       choices = c("Histogram", "Boxplot")),
                           selectInput("filter_choice", "Filter Plot by Wine Type",
                                       choices = c("Include All", 
                                                   "Red Only",
                                                   "White Only"))
              ),
              mainPanel(h1(textOutput("header")),
                #h1("Data Summaries"), 
                        dataTableOutput("tab"),
                        plotOutput("dataPlot")
              )
            )
    ),
    tabItem(tabName = "Model", withMathJax(),
            
            mainPanel(
              title = "Modeling",
              tabsetPanel(type = "tabs",
                          tabPanel("Model Info",
                                   h3("Logistic Regression"),
                                   h5("In the logistic regression model the goal
                                      is to model a binary outcome using a linear 
                                      equation where the logit function is used as a 
                                      link between the mean and the linear form of
                                      the model. The logit function is the following:"),
                                   helpText('$$ln(\\frac{p}{1-p})$$'),
                                   h5("One benefit of using a regression model is that
                                      it is typically the simplest model. The downside 
                                      is that it is typically not the best-performing
                                      for prediction."),
                                   h3("Classification Tree"),
                                   h5("In a tree based model, the predictor space is
                                      split into regions with different predictions
                                      for each region. For a classification tree,
                                      usually the most prevalent class in the region
                                      is used as the prediction. Often for 
                                      classification trees, the splits are selected 
                                      by minimizing the Gini index:"),
                                   helpText('$$2p(1-p)$$'),
                                   h5("One benefit of trees is that they are easy to
                                      understand and interpret output. One con is that
                                      small changes in data can vastly change the tree."),
                                   h3("Random Forest"),
                                   h5("A random forest model is an extension of a
                                      tree based model where multiple trees are created
                                      from bootstrap samples and the results are 
                                      averaged. Each individual tree uses a subset (m) 
                                      of the predictiors (p).for classification trees,
                                      usually m is \\(\\sqrt{p}\\)"),
                                   h5("One pro of using a random forest is that it 
                                      will often perform better for prediction over the
                                      single tree and regression models. One con is that 
                                      you lose interprettability.")
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
                                                        selected = c("alcohol",
                                                                   "volatile_acidity", 
                                                                    "type", 
                                                                    "sulphates", 
                                                                    "residual_sugar",
                                                                    "total_SO2")),
                                     h4("Select model tuning parameters"),
                                     sliderInput("p_choice", label = "Proportion for data split",
                                                 min=0.1, max=0.9, step=0.05, value = 0.7),
                                     sliderInput("cp_choice", label = "CP for Classification Tree fit",
                                                 min=0, max = 0.1, step = 0.005, value = 0.05),
                                     sliderInput("m_choice", label = "m for Random Forest fit",
                                                 min = 1, max = 5, step = 1, value = 3),
                                     actionButton("go", "Run Models")
                                   ),
                                   mainPanel(tabsetPanel(type = "tabs",
                                     tabPanel("Logistic Regression", box(width = 200,
                                                 h3("Logistic Regression Model"),
                                                 h4("Fit Statistics"),
                                                 tableOutput("reg_table"),
                                                 h4("Model Summary"),
                                                 box(width = 200, verbatimTextOutput("reg_summary")),
                                                 h4("Accuracy Predicting Test Data"),
                                                 box(width = 200, verbatimTextOutput("reg_conf")))),
                                     tabPanel("Classification Tree", box(width = 200,
                                              h3("Classification Tree Model"),
                                               h4("Fit Statistics"),
                                                        tableOutput("tree_table"),
                                                        h4("Model Summary"),
                                                        plotOutput("tree_summary"),
                                                        h4("Accuracy Predicting Test Data"),
                                                        box(width = 200, 
                                                            verbatimTextOutput("tree_conf")))),
                                     tabPanel("Random Forest", box(width = 200, 
                                              h3("Random Forest Model"),
                                                        h4("Fit Statistics"),
                                                        tableOutput("rf_table"),
                                                        h4("Model Summary"),
                                                        plotOutput("rf_summary"),
                                                        h4("Accuracy Predicting Test Data"),
                                                        box(width = 100, verbatimTextOutput("rf_conf")))
                                     ) 
                                     ))
                                   ),
                          tabPanel("Prediction", sidebarLayout(
                                   sidebarPanel(h4("Select values of predictors"),
                                                numericInput("num_alcohol",
                                                             "Percent alcohol",
                                                             value = 0,
                                                             min = 0,
                                                             max = 20,
                                                             step = 1),
                                                numericInput("num_acid",
                                                             "Volatile Acidity",
                                                             value = 0,
                                                             min = 0,
                                                             max = 2,
                                                             step = 0.05),
                                                numericInput("num_sulphate",
                                                             "Sulphates",
                                                             value = 0,
                                                             min = 0,
                                                             max = 2,
                                                             step = 0.05),
                                                numericInput("num_sugar",
                                                             "Residual Sugar",
                                                             value = 0,
                                                             min = 0,
                                                             max = 10,
                                                             step = 0.1),
                                                numericInput("num_SO2",
                                                             "Total SO2",
                                                             value = 0,
                                                             min = 0,
                                                             max = 200,
                                                             step = 5),
                                                selectInput("num_type", 
                                                            "Type",
                                                            choices = c("red", 
                                                                        "white"))
                                   ),
                                   mainPanel(h2("Prediction"),
                                             h5("Based on the selections made on the left,
                                                our prediction using a logistic regression
                                                model with all variables included is the 
                                                following:"),
                                     textOutput("pred_text"))
                          )
                          )
              )
            )
            
    ),
    #Data tab. Still need to add row filtering options.
    tabItem(tabName = "Data",
            sidebarLayout(
              
              sidebarPanel(checkboxGroupInput("data_choice",
                                              label = "Variable Options",
                                              choices = c("alcohol", 
                                                          "volatile_acidity", 
                                                          "type", 
                                                          "sulphates", 
                                                          "residual_sugar",
                                                          "total_SO2",
                                                          "quality"), 
                                              selected = "alcohol"),
                           selectInput("row_choice", "Filter by Wine Type",
                                       choices = c("Include All", 
                                                   "Red Only",
                                                   "White Only")),
                downloadButton('downloadData', 'Download')
              ),
                mainPanel(h1("Table of Selected Data"), dataTableOutput("data_table")
                          )
              
            )
            
    )
  )
)

dashboardPage(skin = "red",
  dashboardHeader(title = "A Sip of Stats"),
  sidebar,
  body
)



