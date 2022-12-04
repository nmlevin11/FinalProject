
library(shiny)
library(caret)
library(tidyverse)
library(DT)
#data("GermanCredit")

#Import the data
red_data <- read_delim("winequality-red.csv", delim = ";")
white_data <- read_delim("winequality-white.csv", delim = ";")

#Add variables for type (red or white)
red_data2 <- cbind(red_data, type = "red")
white_data2 <- cbind(white_data, type = "white")

#Conbine datasets
wine_data <- rbind(red_data2, white_data2)
wine_data <- wine_data %>% rename(fixed_acidity = 'fixed acidity', volatile_acidity = 'volatile acidity', citric_acid = 'citric acid', residual_sugar = 'residual sugar', free_SO2 = 'free sulfur dioxide', total_SO2 = 'total sulfur dioxide', qual_orig = 'quality')

#Add variable for quality with >5 = good and otherwise bad
wine_data <- wine_data %>% mutate(quality = if_else(qual_orig >5, "good", "bad") )
wine_data$quality <- factor(wine_data$quality)
wine_data <- wine_data[,-12]
#wine_data <- wine_data %>% select(alcohol, volatile_acidity, type, sulphates, residual_sugar, total_SO2, quality)

shinyServer(function(input, output, session){
  #Table Setup
  output$tab <- renderDataTable({
    variable_choice <- input$x_choice
    if(input$summary_choice == "mean"){
      wine_data %>%
        select("type", variable_choice) %>% 
        group_by(type) %>%
        summarize("Mean" = round(mean(get(input$x_choice)), 2))
    } else{
      wine_data %>%
        select("type", variable_choice) %>% 
        group_by(type) %>%
        summarize("Median" = round(median(get(input$x_choice)), 2))
    }
  })
  #Plot setup. If statements used to decide which plot to make. For now just put a plot in.
  output$dataPlot <- renderPlot({
    if(input$plot_choice=="Histogram"){
      ggplot(data=wine_data, aes(x=get(input$x_choice))) + 
        geom_histogram()
    } else {
      ggplot(data=wine_data, aes(x=get(input$x_choice))) + 
        geom_boxplot()
    }
  })
  
  #Table of data for data tab
  data_selected <- reactive({
    wine_data %>%
      select(input$data_choice)
  })
  output$data_table <- renderDataTable({
    data_selected()
  })
    
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- downloadHandler(
    filename = "wine_export.csv",
    
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      
      # Write to a file specified by the 'file' argument. Right now this just uses my whole dataset.
      write.table(data_selected(), file, sep = ",", row.names = FALSE)
    }
  )
  
})
