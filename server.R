library(shiny)
library(tidyverse)

adult <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 
                    sep = ',', fill = F, strip.white = T)
colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'educatoin', 
                     'educatoin_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')

adult[adult == "?"] <- "Unknown"

adult <- adult %>% mutate(Native = if_else(native_country=="United-States","Y","N"))

adult <- adult %>% select(-fnlwgt,-educatoin,-relationship, -native_country)

shinyServer(function(input, output, session) {
  
  getData <- reactive({
    newData <- adult %>% filter(sex == input$sex)
  })
  
  output$info <- renderText({
    #get filtered data
    newData <- getData()
    paste("The average education years of", input$sex, "is", round(mean(newData$education_num, na.rm = TRUE), 1), "and the average hours per week is", round(mean(newData$hours_per_week, na.rm = TRUE), 1), sep = " ")
  })
  
  output$hist <- renderPlot({
    newData <- getData()
    g <- ggplot(newData, aes(x = as.numeric(input$numchoice), group=income, fill=income))
    g + geom_histogram(binwidth=1, color='black')
  })
  
  output$bar <- renderPlot({
    newData <- getData()
    g <- ggplot(newData, aes(x = as.numeric(input$catchoice), group=income, fill=income))
    g + geom_bar(stat = "identity")
  })
  
})