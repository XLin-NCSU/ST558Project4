library(shiny)
library(tidyverse)

adult <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 
                    sep = ',', fill = F, strip.white = T)
colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 
                     'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')

adult[adult == "?"] <- "Unknown"

adult <- adult %>% mutate(Native = if_else(native_country=="United-States","Y","N"))

adult <- adult %>% select(-fnlwgt,-education,-relationship, -native_country)

shinyServer(function(input, output, session) {
  

# Data exploration --------------------------------------------------------

  getData <- reactive({
    newData <- adult %>% filter(income == input$hist_cat_income)
  })
  
  output$info <- renderText({
    #get filtered data
    newData <- getData()
    paste("The average education years of ", input$hist_cat_income, " income level is", round(mean(newData$education_num, na.rm = TRUE), 1), "and the average hours per week is", round(mean(newData$hours_per_week, na.rm = TRUE), 1), sep = " ")
  })
  
  # histogram
  output$hist <- renderPlot({
    newData <- getData()
    g <- ggplot(newData, aes(x = as.numeric(input$hist_num_var), group=input$hist_cat_var, fill=input$hist_cat_var))
    g + geom_histogram(binwidth=1, color='black')
  })
  
  # bar plot
  output$bar <- renderPlot({
    g <- ggplot(adult, aes(x = input$bar_cat_var1, group=input$bar_cat_var_2, fill=input$bar_cat_var_2))
    g + geom_bar()
  })
  
  # scatter plot
  output$bar <- renderPlot({
    g <- ggplot(adult, aes(x = as.numeric(input$scatter_num_var_1), y = as.numeric(input$scatter_num_var_2), group=input$scatter_cat_var))
    g + point()
  })
  
  # box plot
  output$bar <- renderPlot({
    g <- ggplot(adult, aes(x = as.factor(input$box_cat_var), y = as.numeric(input$box_num_var)))
    g + boxplot()
  })
  

# Modeling ----------------------------------------------------------------

  
  
})