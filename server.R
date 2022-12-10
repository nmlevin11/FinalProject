
library(shiny)
library(caret)
library(tidyverse)
library(DT)


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
wine_data <- wine_data[,-12]
wine_data$quality <- factor(wine_data$quality)
wine_data <- wine_data %>% select(alcohol, volatile_acidity, type, sulphates, residual_sugar, total_SO2, quality)

#Run logistic regression with all variables to use with prediction tab.
pred_model <- glm(quality ~ ., data = wine_data, family = "binomial")

shinyServer(function(input, output, session){
  #Data Tab Header Text
  output$header <- renderText(paste0("Data summaries for ", input$x_choice))
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
    #Filter Data
    if(input$filter_choice == "Red Only"){plot_data <- wine_data %>% filter(type == "red")
    }else if(input$filter_choice == "White Only"){plot_data <- wine_data %>% filter(type == "white")
    } else {plot_data <- wine_data}
    
    #Create plot chosen
    if(input$plot_choice=="Histogram"){
      ggplot(data=plot_data, aes(x=get(input$x_choice))) + 
        geom_histogram(aes(fill=quality)) +
        labs(x = input$x_choice)
    } else {
      ggplot(data=plot_data, aes(x=get(input$x_choice), y=quality)) + 
        geom_boxplot()+
        labs(x = input$x_choice)
    }
  })
  
  #Table of data for data tab
  data_selected <- reactive({
    if(input$row_choice == "Include All"){wine_data %>% select(input$data_choice)
    } else if(input$row_choice == "Red Only"){wine_data %>% select(input$data_choice) %>% filter(type == "red")
      }else {wine_data %>% filter(type == "white") %>% select(input$data_choice) 
      }
  })
  output$data_table <- renderDataTable({
    data_selected()
  })
  
  #Modeling section. Make this work with a run button
  #model_string <- eventReactive(input$go, {paste(input$pred_choice, collapse = "+")
  #})
  
  set.seed(371)
  
  #Allow this to be user-defined
  #prop_choice <- 0.7
  #m_choice <- 3
  
  #eventReactive(input$go, {
    train_index <- createDataPartition(wine_data$quality, p=0.7, list=FALSE)
    data_train <- wine_data[train_index, ]
    data_test <- wine_data[-train_index, ]
  #})
    
    #regression_fit <- eventReactive(input$go, {train(quality ~ alcohol, 
                                                  #  data = data_train, method= "glm", 
                                                   # family = "binomial", 
                                                  #  preProcess = c("center", "scale"), 
                                                   # trControl = trainControl(method = "cv", 
                                                    #                         number = 10))
   # })
    #output$reg_summary <- renderTable({summary(regression_fit$finalModel)})
    output$reg_results <- renderDataTable({
      if(input$go){
        model_string <- paste(input$pred_choice, collapse = "+")
        regression_fit <- train(quality ~ model_string, data = data_train, method= "glm", 
                                family = "binomial", 
                                preProcess = c("center", "scale"), 
                                trControl = trainControl(method = "cv", 
                                                         number = 10))
        data.frame(regression_fit$results)}
    })
    
    
  #Prediction. Need to figure out getting an output here.
  output$pred_text <- renderText({
    prediction_values <- data.frame(alcohol = input$num_alcohol, 
                                    volatile_acidity = input$num_acid, 
                                    type = input$num_type, 
                                    sulphates = input$num_sulphate,
                                    residual_sugar = input$num_sugar, 
                                    total_SO2 = input$num_SO2)
    predict_qual <- predict(pred_model, newdata = prediction_values, type = "response", se.fit=TRUE)
    predict_qual_round <- round(predict_qual[[1]], 3)
    predict_se_round <- round(predict_qual[[2]], 3)
    result <- (paste0("The predicted probability of good quality is ", predict_qual_round," with a
                  standard error of ", predict_se_round))
    result
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
