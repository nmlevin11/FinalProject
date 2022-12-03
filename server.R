
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

output$testplot <- ggplot(data=wine_data, aes(x=alcohol, y=sulphates)) + 
  geom_point()


shinyServer(function(input, output){
  #Table Setup
  output$tab <- renderDataTable({wine_data %>%
    select("type", "alcohol") %>% 
    group_by(type) %>%
    summarize("Mean Alcohol" = round(mean(alcohol), 2))
  })
  #Plot setup. If statements used to decide which plot to make. For now just put a plot in.
  output$dataPlot <- reactive(function(){
    ggplot(data=wine_data, aes(x=alcohol)) + 
      geom_bar()
  })
})
