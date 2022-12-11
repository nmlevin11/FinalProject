
#Nicole Levin
#ST558 Final Project
#12-11-2022

#Load packages
library(shiny)
library(caret)
library(tidyverse)
library(DT)
library(randomForest)

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

#Functions to use for modeling
#Function to split data into training and test sets
split_fun <- function(selection, p_choice){
  model_data <- wine_data %>% select(selection, quality)
  train_index <- createDataPartition(model_data$quality, p=p_choice, list=FALSE)
  data_train <- model_data[train_index, ]
  data_test <- model_data[-train_index, ]
  return(list(data_train, data_test))
}

#Function to train and test regression model.
reg_fun <- function(datalist){
  regression_fit <- train(quality ~ ., data = datalist[[1]], method= "glm", 
                          family = "binomial", 
                          preProcess = c("center", "scale"), 
                          trControl = trainControl(method = "cv", 
                                                   number = 10))
  out1 <- data.frame(regression_fit$results)
  out2 <- summary(regression_fit)
  out3 <- confusionMatrix(regression_fit, newdata = datalist[[2]])
  return(list(out1, out2, out3))
}

#Function to train and test classification tree
tree_fun <- function(datalist, cp_choice){
  tree_fit <-train(quality ~ ., data = datalist[[1]], 
                   method= "rpart", preProcess = c("center", "scale"), 
                   tuneGrid = data.frame(cp = cp_choice), 
                   trControl = trainControl(method = "cv", number = 10))
  out1 <- data.frame(tree_fit$results)
  out2 <- tree_fit$finalModel
  out3 <- confusionMatrix(tree_fit, newdata = datalist[[2]])
  return(list(out1, out2, out3))
}

#Function to train and test random forest.
rf_fun <- function(datalist, m_choice){
  rf_fit <- train(quality ~ ., data = datalist[[1]], 
                  method= "rf", preProcess = c("center", "scale"), 
                  tuneGrid = data.frame(mtry = m_choice), 
                  importance = TRUE, 
                  trControl = trainControl(method = "cv", number = 10))
  out1 <- data.frame(rf_fit$results)
  out2 <- rf_fit$finalModel
  out3 <- confusionMatrix(rf_fit, newdata = datalist[[2]])
  return(list(out1, out2, out3))
}
 
#Interactive code section
shinyServer(function(input, output, session){
  #Data Tab Header Text
  output$header <- renderText(paste0("Data summaries for ", input$x_choice))
  
  #Table Setup allowing for user selections
  output$tab <- renderDataTable({
    variable_choice <- input$x_choice
    if(input$filter_choice == "Red Only"){
      if(input$summary_choice == "mean"){
        wine_data %>%
          select("type", "quality", variable_choice) %>% 
          filter(type == "red") %>%
          group_by(quality) %>%
          summarize("Mean" = round(mean(get(input$x_choice)), 2))
      } else{
        wine_data %>%
          select("type", "quality", variable_choice) %>% 
          filter(type == "red") %>%
          group_by(quality) %>%
          summarize("Median" = round(median(get(input$x_choice)), 2))
      }
    }else if(input$filter_choice == "White Only"){
      if(input$summary_choice == "mean"){
        wine_data %>%
          select("type", "quality", variable_choice) %>% 
          filter(type == "white") %>%
        group_by(quality) %>%
          summarize("Mean" = round(mean(get(input$x_choice)), 2))
      } else{
        wine_data %>%
          select("type", "quality", variable_choice) %>% 
          filter(type == "white") %>%
        group_by(quality) %>%
          summarize("Median" = round(median(get(input$x_choice)), 2))
      }
    } else{
      if(input$summary_choice == "mean"){
        wine_data %>%
          select("quality", variable_choice) %>% 
        group_by(quality) %>%
          summarize("Mean" = round(mean(get(input$x_choice)), 2))
      } else{
        wine_data %>%
          select("quality", variable_choice) %>% 
        group_by(quality) %>%
          summarize("Median" = round(median(get(input$x_choice)), 2))
      }
    }
  })
  
  #Plot setup. If statements used to decide which plot to make.
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
  
  #Modeling Section
  #Run data splitting function and modeling function after button is pushed
  datasplit <- eventReactive(input$go,
                             {split_fun(selection = input$pred_choice, p_choice = input$p_choice)})
  
  result_reg <- eventReactive(input$go, {reg_fun(datalist = datasplit())})
  result_tree <- eventReactive(input$go, {
    tree_fun(datalist = datasplit(), cp_choice = input$cp_choice)})
  result_rf <- eventReactive(input$go, {rf_fun(datalist = datasplit(), m_choice = input$m_choice)})
  
  #Create outputs from regression model
  output$reg_table <- renderTable({if(input$go)
      result_reg()[[1]]
    })
  output$reg_summary <- renderPrint({if(input$go)
    result_reg()[[2]]
    })
  output$reg_conf <- renderPrint({if(input$go)
    result_reg()[[3]]
    })
  
  #Create outputs from tree model
  output$tree_table <- renderTable({if(input$go)
      result_tree()[[1]]
    })
  output$tree_summary <- renderPlot({if(input$go)
    plot(result_tree()[[2]])
    text(result_tree()[[2]])
    })
  output$tree_conf <- renderPrint({if(input$go)
    result_tree()[[3]]
    })
  
  #Create outputs from rf model
  output$rf_table <- renderTable({if(input$go)
      result_rf()[[1]]
    })
  output$rf_summary <- renderPlot({if(input$go)
    varImpPlot(result_rf()[[2]])
    })
  output$rf_conf <- renderPrint({if(input$go)
    result_rf()[[3]]
    })
    
  #Prediction Section. Predict probability of a good wine based on user inputs
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

  #Data Section 
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
  
  # Allow for downloading data
  output$downloadData <- downloadHandler(
    filename = "wine_export.csv",
    
    # Function to right data to file.
    content = function(file) {
      write.table(data_selected(), file, sep = ",", row.names = FALSE)
    }
  )
  
})
