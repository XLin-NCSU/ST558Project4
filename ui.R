
library(shiny)

# page setting
fluidPage(
  
# Application title
titlePanel("Census Adult Income Data"),
  
# MathJax
withMathJax(),
  
# Top level tabs
tabsetPanel(
    type = "tabs",
# About Tab ---------------------------------------------------------------

    tabPanel(
      "About",

## Intro -------------------------------------------------------------------
      tags$br(),
      tags$p("The purpose of this app is to predict whether income exceeds $50K/yr based on census data. The data set comes from ",
             tags$a("UCI Machine Learning Repository", href = "https://archive.ics.uci.edu/dataset/2/adult")),
      tags$p(img(src = 'https://upload.wikimedia.org/wikipedia/commons/thumb/c/c1/United_States_Census_Bureau_Wordmark.svg/315px-United_States_Census_Bureau_Wordmark.svg.png', align = "center")),
      tags$p("Extraction was done by Barry Becker from the 1994 Census database.  A set of reasonably clean records was extracted using the following conditions: ((AAGE>16) && (AGI>100) && (AFNLWGT>1)&& (HRSWK>0))"),
      tags$p("You can find some numerical and graphical summaries in the ",
             tags$b("Data Exploration"),
             " tab and two predivtive models in the ",
             tags$b("Modeling"),
             " tab."),
      tags$p("The original data set contains the following information:"),

## Table -------------------------------------------------------------------

      tags$table(
        style = "width:100%",
        style = "text-align: center",
        tags$tr(
          tags$th("Variable Name"),
          tags$th("Type"),
          tags$th("Demographic"),
          tags$th("Description")),
        tags$tr(
          tags$td("age"),
          tags$td("Integer"),
          tags$td("Age"),
          tags$td("N/A")),
        tags$tr(
          tags$td("work class"),
          tags$td("Categorical"),
          tags$td("Income"),
          tags$td("Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.")),
        tags$tr(
          tags$td("education"),
          tags$td("Categorical"),
          tags$td("Education Level"),
          tags$td("Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.")),
        tags$tr(
          tags$td("education_num"),
          tags$td("Integer"),
          tags$td("Education Level"),
          tags$td("")),
        tags$tr(
          tags$td("marital-status"),
          tags$td("Categorical"),
          tags$td("Other"),
          tags$td("Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.")),
        tags$tr(
          tags$td("occupation"),
          tags$td("Categorical"),
          tags$td("Other"),
          tags$td("Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.")),
        tags$tr(
          tags$td("relationship"),
          tags$td("Categorical"),
          tags$td("Other"),
          tags$td("Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.")),
        tags$tr(
          tags$td("race"),
          tags$td("Categorical"),
          tags$td("Race"),
          tags$td("White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.")),
        tags$tr(
          tags$td("sex"),
          tags$td("Binary"),
          tags$td("Sex"),
          tags$td("Female, Male.")),
        tags$tr(
          tags$td("capital-gain"),
          tags$td("Integer"),
          tags$td(""),
          tags$td("")),
        tags$tr(
          tags$td("capital-loss"),
          tags$td("Integer"),
          tags$td(""),
          tags$td("")),
        tags$tr(
          tags$td("hours-per-week"),
          tags$td("Integer"),
          tags$td(""),
          tags$td("")),
        tags$tr(
          tags$td("native-country"),
          tags$td("Categorical"),
          tags$td("Other"),
          tags$td("United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.")),
        tags$tr(
          tags$td("income"),
          tags$td("Binary"),
          tags$td("Income"),
          tags$td(">50K, <=50K."))
        ), 

## closing table and summary ------------------------------------------------

      tags$br(),
      tags$p("Since fnlwgt is not relevent to this study, while education and relationship have similar infomation with education_num and marital_status, I removed these three variables from the dataset."),
      tags$p("Moreover, I created a variable 'Native' indicating whether the worder's native country was United States or not to replace the native_country variable.")
    ), 
    #closing tab panel

# Data Exploration Tab ---------------------------------------------------------------
    tabPanel(
      "Data Exploration",
      sidebarLayout(

## sidebar panel -----------------------------------------------------------
        sidebarPanel(
          
          # Four type of plots
          selectInput(inputId = "plottype",
                      "Choose your plot", 
                      c("Histogram" = "hist",
                        "Bar Graph" = "bar",
                        "Scatter Plot" = "scatter",
                        "Box Plot" = "box")
            ),
          
          # histogram - numerical var
          conditionalPanel( 
            condition = "input.plottype == 'hist'",
            selectInput(inputId = "hist_num_var",
                        "Choose the numerical variable that you want to investigate:",
                        c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week"))
          ),
          
          # histogram - categorical var comparison
          conditionalPanel( 
            condition =  "input.plottype == 'hist'",
            selectInput(inputId = "hist_cat_var",
                        label = "Choose the categorical variable that you want to compare:",
                        c("Work class" = "workclass",
                          "Marital status" = "marital_status",
                          "Occupation" = "occupation",
                          "Race" = "race",
                          "Gender" = "sex",
                          "Native or International" =  "Native"))
          ),
          
          # histogram - row filter on income
          conditionalPanel( 
            condition =  "input.plottype == 'hist'",
            selectInput(inputId = "hist_cat_income", 
                        label = "filter the income level", 
                        selected = "<= 50K", 
                        choices = c("<=50K",
                                       ">50K"))
          ),
          
          conditionalPanel(
            condition = "input.plottype == 'hist'",
            sliderInput(inputId = "breakcount", 
                        label = "Break Count", 
                        min = 1, 
                        max = 50, 
                        value = 10)
          ),
          
          conditionalPanel(
            condition = "input.plottype == 'hist'",
            checkboxInput(inputId = "center",
                          label = "Use median instead of mean as center")
          ),
          
          # bar plot - categorical var
          conditionalPanel( 
            condition =  "input.plottype == 'bar'",
            selectInput(inputId = "bar_cat_var_1",
                        label = "Choose the categorical variable that you want to investigate:",
                        choices = c("Work class" = "workclass",
                          "Marital status" = "marital_status",
                          "Occupation" = "occupation",
                          "Race" = "race",
                          "Gender" = "sex",
                          "Native or International" =  "Native",
                          "Income Category" = "income"))
          ),
          
          conditionalPanel(
            condition =  "input.plottype == 'bar'",
            selectInput(inputId = "bar_cat_var_2",
                        label = "Choose the categorical variable that you want to compare:",
                        choices = c("Work class" = "workclass",
                          "Marital status" = "marital_status",
                          "Occupation" = "occupation",
                          "Race" = "race",
                          "Gender" = "sex",
                          "Native or International" =  "Native",
                          "Income Category" = "income"),
                        selected = "marital_status")
          ),
          
          # scatter plot - numerical & numerical
          conditionalPanel(
            condition = "input.plottype == 'scatter'",
            selectInput(inputId = "scatter_num_var_1",
                        label = "Choose the first numerical variable that you want to investigate:",
                        choices = c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week"),
                        selected = "age")
          ),
          
          # scatter plot - numerical & numerical
          conditionalPanel(
            condition = "input.plottype == 'scatter'",
            selectInput(inputId = "scatter_num_var_2",
                        label = "Choose the second numerical variable that you want to investigate:",
                        choices = c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week"),
                        selected = "education_num")
          ),
          
          # scatter plot - numerical & numerical
          conditionalPanel( 
            condition = "input.plottype == 'scatter'",
            selectInput(inputId = "scatter_cat_var",
                        label = "Choose the categirical variable that you want to compare:",
                        choices = c("Work class" = "workclass",
                          "Marital status" = "marital_status",
                          "Occupation" = "occupation",
                          "Race" = "race",
                          "Gender" = "sex",
                          "Native or International" =  "Native",
                          "Income Category" = "income"))
          ),
          
          # box plot - numerical & categorical
          conditionalPanel(
            condition = "input.plottype == 'box'",
            selectInput(inputId = "box_num_var",
                        label = "Choose the numerical variable that you want to investigate:",
                        choices = c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week"))
          ),
          
          # box plot - numerical & categorical
          conditionalPanel( 
            condition = "input.plottype == 'box'",
            selectInput(inputId = "box_cat_var",
                        label = "Choose the categorical variable that you want to compare:",
                        choices = c("Work class" = "workclass",
                          "Marital status" = "marital_status",
                          "Occupation" = "occupation",
                          "Race" = "race",
                          "Gender" = "sex",
                          "Native or International" =  "Native",
                          "Income Category" = "income"))
          )
        ), 
        #closing data exploration sidebarpanel

## main panel --------------------------------------------------------------

        # Show a plot of the generated distribution
        mainPanel(
          tags$br(),
          uiOutput("datainfo"),
          tags$br(),
          plotOutput("dataplot")
          
        ) 
        # closing data exploration mainpanel

## closing this tab --------------------------------------------------------

      )
      # closing data exploration sidebarlayout
  ), 
  # closing data exploration tabpanel
  

# Modeling Tab ------------------------------------------------------------

    tabPanel(
      "Modeling",
      # Sub tabs
      tabsetPanel(

## Modeling subpanel 1: info -----------------------------------------------

        tabPanel(
          "Modeling Info",
          tags$h3("Logistic Regression"),
          tags$p("Logistic regression is a statistical method used for binary classification. It is a type of regression analysis that models the probability of a binary outcome (1/0, Yes/No, True/False) as a function of one or more predictor variables. The logistic regression model is named after the logistic function, also known as the sigmoid function, which is used to transform the linear combination of predictor variables into probabilities."),
          tags$p("Here's a basic overview of how logistic regression works:"),
          tags$ol(
            tags$li(tags$b("Model Representation:"),
                    tags$ul(tags$li("The logistic regression model is represented as follows:",
                            tags$br(),
                            '$$P(Y=1)=\\frac{1}{1+e^{-(\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\dots + \\beta_n X_n)}}$$',
                            tags$br(),
                            "where:"),
                            tags$li(
                              '\\(P(Y=1)\\) is the probability of the event occurring (the dependent variable being 1),'
                              ),
                            tags$li(
                              '\\(e\\) is the base of the natural logarithm,'
                            ),
                            tags$li(
                              '\\(\\beta_0,\\beta_1,\\dots,\\beta_n\\) are the coefficients,'
                            ),
                            tags$li(
                              '\\(X_1,X_2,\\dots,X_n\\) are the predictor variables.'
                            )
                            )
                    ),        
            tags$li(tags$b("Sigmoid Function:"),
                    tags$ul(tags$li(
                      'The sigmoid function \\((\\frac{1}{1+e^{-z}})\\)  is used to transform the linear combination \\(z\\)  into a value between 0 and 1. This is crucial for interpreting the result as a probability.'
                    ))),
            tags$li(tags$b("Training:"),
                    tags$ul(tags$li(
                      'The coefficients \\(\\beta\\) are estimated through a process called maximum likelihood estimation. The goal is to find the values of the coefficients that maximize the likelihood of observing the given set of outcomes.'
                    ))),
            tags$li(tags$b("Decision Boundary:"),
                    tags$ul(tags$li(
                      'The logistic regression model predicts the probability that an instance belongs to a particular class. A decision boundary is set (usually 0.5) such that if the predicted probability is greater than or equal to the decision boundary, the instance is classified as class 1; otherwise, it is classified as class 0.'
                    )))),
          tags$p("Logistic regression is widely used in various fields, such as finance, healthcare, and social sciences, for tasks like predicting whether a customer will churn, whether a patient has a particular disease, or whether a student will pass an exam. It's important to note that logistic regression is specifically designed for binary classification problems; for multi-class problems, extensions like multinomial logistic regression are used."),
          tags$h4("Benefits of Logistic Regression:"),
          tags$ol(
            tags$li("Simple to Implement and Interpret"),
            tags$li("Efficient for Small Datasets"),
            tags$li("Probabilistic Output"),
            tags$li("No Assumption of Linearity"),
            tags$li("Works Well with Feature Scaling"),
            tags$li("Less Susceptible to Overfitting")
          ),
          tags$h4("Drawbacks of Logistic Regression:"),
          tags$ol(
            tags$li("Assumption of Linearity"),
            tags$li("Limited Expressiveness"),
            tags$li("Not Suitable for Complex Relationships"),
            tags$li("Sensitive to Outliers"),
            tags$li("Independence of Errors"),
            tags$li("Requires a Large Sample Size for Stable Estimates")
          ),
          tags$br(),
          tags$h3("Random Forest"),
          tags$p("Random Forest is an ensemble learning method used for both classification and regression tasks. It operates by constructing a multitude of decision trees during training and outputs the class that is the mode of the classes (classification) or the mean prediction (regression) of the individual trees."),
          tags$p(tags$b("Key features of random forest:")),
          tags$ol(
            tags$li(tags$b("High accuracy:"),"Random forest is one of the most accurate machine learning algorithms available."),
            tags$li(tags$b("Robustness to overfitting:"),"Random forest is relatively robust to overfitting, which means that it is less likely to make poor predictions on new data."),
            tags$li(tags$b("Interpretability:"),"Random forest is relatively interpretable, which means that it is possible to understand how the algorithm is making its predictions.")
          ),
          tags$h4("Benefits of Random Forest:"),
          tags$ol(
            tags$li("High accuracy"),
            tags$li("Robust to Overfitting"),
            tags$li("Handles Non-Linearity and Interactions"),
            tags$li("Works Well with Large Datasets"),
            tags$li("Implicit Feature Selection"),
            tags$li("Deals with Imbalanced Datasets")
          ),
          tags$h4("Drawbacks of Random Forest:"),
          tags$ol(
            tags$li("Lack of Interpretability"),
            tags$li("Computational Complexity"),
            tags$li("Sensitive to Noisy Data"),
            tags$li("Parameter Tuning Complexity"),
            tags$li("Not Suitable for Very Sparse Datasets")
          )
          
        ),

## Modeling subpanel 2: fitting model --------------------------------------

        tabPanel(
          "Model Fitting",
          sidebarLayout(
            # sidebar panel
            sidebarPanel(
              
              selectInput(inputId = "split",
                          label = "Train/test data split:",
                          choices = c("90/10" = 0.9,
                                      "80/20" = 0.8,
                                      "70/30" = 0.7),
                          selected = 0.8),
              
              selectInput(inputId = "vars",
                          label = "Choose the predictor variables that you want to put in the model:",
                          choices = c("age",
                                      "workclass",
                                      "education_num",
                                      "marital_status",
                                      "occupation",
                                      "race",
                                      "sex",
                                      "capital_gain",
                                      "capital_loss",
                                      "hours_per_week",
                                      "Native"),
                          multiple = TRUE,
                          selected = c("age",
                                       "workclass",
                                       "education_num",
                                       "marital_status",
                                       "occupation",
                                       "race",
                                       "sex",
                                       "capital_gain",
                                       "capital_loss",
                                       "hours_per_week",
                                       "Native")
                          ),
              
              sliderInput(inputId = "tuning",
                            label = "Number of variables randomly sampled as candidates at each split:",
                            min = 1,
                            max = 11,
                            value = c(1,8)),

              numericInput(inputId = "cv_num",
                             label = "Number of folds in cross validation",
                             value = 5),
            
              actionButton(inputId = "submit",
                             label = "Build the model")
              
            ),
            
            # main panel
            mainPanel(
              tags$br(),
              tags$p("Notice: It would take a couple of minutes to run the models, please be patient."),
              tags$br(),
              tags$p("The result of the logistic regression model is:"),
              verbatimTextOutput("logistic"),
              tags$br(),
              tags$p("The result of the random forest model is:"),
              verbatimTextOutput("rf"),
              tags$p("The accuracy over selected predictors of random forest regression model is:"),
              plotOutput("rf_plot"),
              # verbatimTextOutput("randomforest"),
              tags$br(),
              tags$p("Accuracy is used as matric to measure the performance of the model."),
              tags$p("The accuracy of logistic regression model on the test dataset is "),
              textOutput("cm_log"),
              tags$p("The accuracy of random forest model on the test dataset is "),
              textOutput("cm_rf")
            )
            
          ) # closing sidebarlayout
        ), # closing model fitting tabpanel


## Modeling subpanel 3: prediction -----------------------------------------

        tabPanel(
          "Prediction",
          sidebarLayout(
            # sidebar panel
            sidebarPanel(
              sliderInput(inputId = "age",
                          label = "age",
                          min = 17,
                          max = 90,
                          value = 34),
              
              sliderInput(inputId = "education_num",
                          label = "years of education",
                          min = 1,
                          max = 16,
                          value = 12),
              
              sliderInput(inputId = "capital_gain",
                          label = "capital gain",
                          min = 0,
                          max = 99999,
                          value = 0),
              
              sliderInput(inputId = "capital_loss",
                          label = "capital loss",
                          min = 0,
                          max = 4400,
                          value = 0),
              
              sliderInput(inputId = "hours_per_week",
                          label = "Working hours per week",
                          min = 1,
                          max = 99,
                          value = 40),
              
              radioButtons(inputId = "workclass",
                           label = "work class",
                           choices = c("Federal-gov",
                                       "Local-gov",
                                       "Never-worked",
                                       "Private",
                                       "Self-emp-inc",
                                       "Self-emp-not-inc",
                                       "State-gov",
                                       "Unknown",
                                       "Without-pay"),
                           selected = "Local-gov"
                           ),
              
              radioButtons(inputId = "marital_status",
                           label = "marital status",
                           choices = c("Divorced",
                                       "Married-AF-spouse",
                                       "Married-civ-spouse",
                                       "Married-spouse-absent",
                                       "Never-married",
                                       "Separated",
                                       "Widowed" ),
                           selected = "Married-AF-spouse"
                           ),
              
              radioButtons(inputId = "occupation",
                           label = "occupation",
                           choices = c("Adm-clerical",      
                                       "Armed-Forces",      
                                       "Craft-repair",      
                                       "Exec-managerial",   
                                       "Farming-fishing",   
                                       "Handlers-cleaners", 
                                       "Machine-op-inspct",
                                       "Other-service",     
                                       "Priv-house-serv",   
                                       "Prof-specialty",    
                                       "Protective-serv",   
                                       "Sales",            
                                       "Tech-support",      
                                       "Transport-moving",
                                       "Unknown"),
                           selected = "Sales"
                
              ),
              
              radioButtons(inputId = "race",
                           label = "race",
                           choices = c("Amer-Indian-Eskimo", 
                                       "Asian-Pac-Islander", 
                                       "Black",              
                                       "Other",              
                                       "White"),
                           selected = "White"
                           
              ),
              
              radioButtons(inputId = "sex",
                           label = "sex",
                           choices = c("Female", 
                                       "Male"),
                           selected = "Male"
                           
              ),
              
              radioButtons(inputId = "native",
                           label = "native American worker",
                           choices = c("Yes" = "Y", 
                                       "No" = "N"),
                           selected = "Y"
                           
              ),
              
              actionButton(inputId = "predict",
                           label = "Predict it")
              
            ),
            
            mainPanel(
              
              tags$p("The predicted category from logistic regression model is:"),
              verbatimTextOutput("log_result"),
              tags$br(),
              tags$p("The predicted category from random forest model is:"),
              verbatimTextOutput("rf_result"),
              tags$br(),
              tags$p("1 means <=50K and 2 means >50K")
            )
          )
        ) # closing prediction tabpanel

## Closing sub panels and modeling panel----------------------------------------------------------------

      ) # closing modeling tabsetpanel

    ) # closing modeling panel 

# Ends --------------------------------------------------------------------

  ) # closing top level tabsetpanel
) # closing page setting