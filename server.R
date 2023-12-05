library(shiny)
library(tidyverse)
library(caret)
library(recipes)

# Data preparation --------------------------------------------------------

rawdata <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", sep = ',', fill = F, strip.white = T)

colnames(rawdata) <- c('age', 'workclass', 'fnlwgt', 'education',
                       'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex',
                       'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')
#replace ? with Unknown
rawdata[rawdata == "?"] <- "Unknown"

#Create Native var
adult <- rawdata %>% mutate(Native = if_else(native_country=="United-States","Y","N"))

#select vars
adult <- adult %>% select(-fnlwgt,-education,-relationship, -native_country)

shinyServer(function(input, output, session) {

# data exploration --------------------------------------------------------

  getData <- reactive({
    newData <- adult %>% filter(income == input$hist_cat_income)
    })
  
  output$datainfo <- renderUI({
    if(input$plottype == "hist"){
      #get filtered data
      newData <- getData()
      if(input$center){
        paste("For the income level of ", input$hist_cat_income, ", the median education years is ", round(median(newData$education_num, na.rm = TRUE), 1), ", the median age is ", round(median(newData$age, na.rm = TRUE), 1), ", and the median hours per week is", round(median(newData$hours_per_week, na.rm = TRUE), 1), sep = " ")
      }
    else{
    paste("For the income level of ", input$hist_cat_income, ", the average education years is ", round(mean(newData$education_num, na.rm = TRUE), 1), ", the average age is ", round(mean(newData$age, na.rm = TRUE), 1), ", and the average hours per week is", round(mean(newData$hours_per_week, na.rm = TRUE), 1), sep = " ")}}
  })

  output$dataplot <- renderPlot({
      # histogram
      if(input$plottype == 'hist'){
        newData <- getData()
        g <- ggplot(newData, aes_string(x = input$hist_num_var, fill=input$hist_cat_var))
        g + geom_histogram(bins = input$breakcount, color='black')
      }
      # # bar plot
      else
        if(input$plottype == 'bar'){
        g <- ggplot(adult, aes_string(x = input$bar_cat_var_1, fill=input$bar_cat_var_2))
        g + geom_bar()
      }
      # scatter plot
      else if(input$plottype == 'scatter'){
        g <- ggplot(adult, aes_string(x = input$scatter_num_var_1, y = input$scatter_num_var_2, color=input$scatter_cat_var))
        g + geom_point()
      }
      # box plot
      else if(input$plottype == 'box'){
        g <- ggplot(adult, aes_string(x = input$box_cat_var, y = input$box_num_var, fill = input$box_cat_var))
        g + stat_boxplot(geom = "errorbar",
                          width = 0.5) + geom_boxplot()
      }
    })
  

# Modeling ----------------------------------------------------------------


  observeEvent(input$submit,{

  ## train/test splitting -----------------------------------------------
    train_id <- sample(1:nrow(adult), size = nrow(adult)*as.numeric(input$split))
    test_id <- dplyr::setdiff(1:nrow(adult), train_id)
    adult_train <- adult[train_id,]
    adult_test <- adult[test_id,]
    
  ## logistic regression model -----------------------------------------------
    
      var <- paste(input$vars, collapse = " + ")
      form <- as.formula(paste0("income ~", var))
      logistic_model <- train(form,
                              data = adult_train, 
                              method = "glm",
                              family = "binomial",
                              preProcess = c("center","scale"),
                              trControl = trainControl(method = "cv", 
                                                       number = input$cv_num),
                              metric = "Accuracy"
                        )
      
      output$logistic <- renderPrint({
        summary(logistic_model)
      })
    
  ## random forest model -----------------------------------------------
   
      rf_model <- train(form,
                        data = adult_train,
                        method = "rf",
                        trControl = trainControl(method = "cv",
                                                 number = input$cv_num,
                                                 preProcOptions = c("center", "scale")),
                        tuneGrid = data.frame(mtry = seq(input$tuning[1],input$tuning[2])),
                        metric = "Accuracy"
      )
      
      output$rf <- renderPrint({
        print(rf_model)
      })

  ## prediction on logistic --------------------------------------------------

      log_pred <- predict (logistic_model, newdata = adult_test)
      cm_log <- confusionMatrix(data = log_pred, reference =  as.factor(adult_test$income))
      output$cm_log <-renderText({
        cm_log$overall[1]
      })  
      
  ## prediction on random forest ---------------------------------------------

      rf_pred <- predict (rf_model, newdata = adult_test)
      cm_rf <- confusionMatrix(data = rf_pred, reference =  as.factor(adult_test$income))
      output$cm_rf <-renderText({
        cm_rf$overall[1]
      }) 
      
    output$rf_plot <- renderPlot({
      plot(rf_model)
    })
    
    observeEvent(input$predict,{
      pred_data <- data.frame("age"=input$age,
                              "workclass"=input$workclass,
                              "education_num"=input$education_num,
                              "marital_status"=input$marital_status,
                              "occupation"=input$occupation,
                              "race"=input$race,
                              "sex"=input$sex,
                              "capital_gain"=input$capital_gain,
                              "capital_loss"=input$capital_loss,
                              "hours_per_week"=input$hours_per_week,
                              "Native"=input$native)
      
      log_result <- predict(logistic_model, newdata = pred_data)
      #log_result <- if_else(log_re == 1, "<=50K", ">50K")
      
      rf_result <- predict(rf_model, newdata = pred_data)
      #rf_result <- if_else(rf_re == 1, "<=50K", ">50K")
      
      output$log_result <- renderText({log_result})
      
      output$rf_result <- renderText({rf_result})
      
    })
    
    })
  
})


            
