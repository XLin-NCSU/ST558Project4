library(shiny)
library(tidyverse)

# Data preparation --------------------------------------------------------

adult <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 
                    sep = ',', fill = F, strip.white = T)

colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 
                     'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')
#replace ? with Unknown
adult[adult == "?"] <- "Unknown"

#Create Native var
adult <- adult %>% mutate(Native = if_else(native_country=="United-States","Y","N"))

#select vars
adult <- adult %>% select(-fnlwgt,-education,-relationship, -native_country)


# Data exploration --------------------------------------------------------

shinyServer(function(input, output, session) {

  getData <- reactive({
    newData <- adult %>% filter(income == input$hist_cat_income)
  })
  
  output$datainfo <- renderText({
    if(input$plottype == "hist"){
    #get filtered data
    newData <- getData()
    paste("For the income level of ", input$hist_cat_income, ", the average education years is ", round(mean(newData$education_num, na.rm = TRUE), 1), ", the average age is ", round(mean(newData$age, na.rm = TRUE), 1), ", and the average hours per week is", round(mean(newData$hours_per_week, na.rm = TRUE), 1), sep = " ")}
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
  
  
  output$bar <- renderPlot({
    g <- ggplot(adult, aes(x = input$bar_cat_var1, group=input$bar_cat_var_2, fill=input$bar_cat_var_2))
    g + geom_bar()
  })
  
  # scatter plot
  output$bar <- renderPlot({
    g <- ggplot(adult, aes(x = input$scatter_num_var_1, y = input$scatter_num_var_2, group=input$scatter_cat_var))
    g + geom_point()
  })
  
  # box plot
  output$bar <- renderPlot({

  })
  

# Modeling ----------------------------------------------------------------

  
  
})