
library(shiny)
library(caret)
library(tidyverse)
library(DT)
data("GermanCredit")
shinyServer(function(input, output) {
  #Table Setup
  output$tab <- renderDataTable(GermanCredit %>%
    select("Class", "InstallmentRatePercentage", input$var) %>% 
    group_by(Class, InstallmentRatePercentage) %>%
    summarize(mean = round(mean(get(input$var)), input$round))
  )
  
  #Plot setup. If statements used to decide which plot to make.
  output$dataPlot <- renderPlot(
  if(input$selectPlot == "Just Classification"){
    ggplot(data=GermanCredit, aes(x=Class)) + 
      geom_bar()
    } else if (input$selectPlot == "Classification and Unemployed"){
      GermanCredit$EmploymentDuration.Unemployed <- factor(GermanCredit$EmploymentDuration.Unemployed, levels=c(0,1), labels = c("Employed", "Unemployed"))
    ggplot(data=GermanCredit, aes(x=Class)) +
      geom_bar(aes(fill=EmploymentDuration.Unemployed), position="dodge") +
      scale_fill_discrete(name = "Unemployment Status")
  } else {GermanCredit$ForeignWorker <- factor(GermanCredit$ForeignWorker, levels=c(0,1), labels = c("German", "Foreign"))
    ggplot(data=GermanCredit, aes(x=Class)) +
      geom_bar(aes(fill=ForeignWorker), position="dodge") +
      scale_fill_discrete(name = "Status")
    }
  
  )
})
