
library(caret)
data("GermanCredit")
library(shiny)
library(DT)
shinyUI(fluidPage(
  titlePanel("Summaries for German Credit Data"),
  sidebarLayout(
    sidebarPanel(
      h3("This data set comes from the", 
      tags$a(href="https://topepo.github.io/caret/", "caret package"), 
      "originally from the UCI machine learning repository"),
      h4("You can create a few bar plots using the radio buttons below."),
      radioButtons("selectPlot", "Select the Plot Type", 
                   choices = c("Just Classification", 
                               "Classification and Unemployed",
                               "Classification and Foreign")),
      br(),
      h4("You can find the ",
      tags$b("sample mean"), 
      "for a few variables below"),
      selectInput("var", label="Variables to Summarize", 
                  choices = c("Duration", "Amount", "Age")),
      numericInput("round", label="Select the number of digits for rounding", value=2)
    ),
    mainPanel(plotOutput("dataPlot"), dataTableOutput("tab"))
    ) )
)

