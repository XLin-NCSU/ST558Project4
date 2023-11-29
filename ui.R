#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # MathJax
  withMathJax(),
  
  # Top level tabs
  tabsetPanel(
    type = "tabs",
    
    # About Tab
    tabPanel(
      "About",
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
      #closing table
      tags$br(),
      tags$p("Since fnlwgt is not relevent to this study, while education and relationship have similar infomation with education_num and marital_status, I removed these three variables from the dataset."),
      tags$p("Moreover, I created a variable 'Native' indicating whether the worder's native country was United States or not to replace the native_country variable.")
    ), 
    #closing tab panel
    
    # Data Exploration Tab
    tabPanel(
      "Data Exploration",
      sidebarLayout(
        sidebarPanel(
          
          # Four type of plots
          selectInput("plottype",
                      "Choose your plot", 
                      c("Histogram" = "hist",
                        "Bar Graph" = "bar",
                        "Scatter Plot" = "scatter",
                        "Box Plot" = "box")
            ),
          
          # histogram - numerical var
          conditionalPanel( 
            condition = "input.plottype == 'hist'",
            selectInput("hist_num_var",
                        "Choose the numerical variable that you want to investigate:",
                        c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week"))
          ),
          
          # histogram - categorical var comparison
          conditionalPanel( 
            condition =  "input.plottype == 'hist'",
            selectInput("hist_cat_var",
                        "Choose the categorical variable that you want to compare:",
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
            selectInput("hist_cat_income",
                        "filter the income level",
                        c("<= 50K" = "<= 50K",
                          ">50K" = ">50K"))
          ),
          
          # bar plot - categorical var
          conditionalPanel( 
            condition =  "input.plottype == 'bar'",
            selectInput("bar_cat_var_1",
                        "Choose the categorical variable that you want to investigate:",
                        c("Work class" = "workclass",
                          "Marital status" = "marital_status",
                          "Occupation" = "occupation",
                          "Race" = "race",
                          "Gender" = "sex",
                          "Native or International" =  "Native",
                          "Income Category" = "income"))
          ),
          
          conditionalPanel(
            condition =  "input.plottype == 'bar'",
            selectInput("bar_cat_var_2",
                        "Choose the categorical variable that you want to compare:",
                        c("Work class" = "workclass",
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
            selectInput("scatter_num_var_1",
                        "Choose the first numerical variable that you want to investigate:",
                        c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week"),
                        selected = "age")
          ),
          
          # scatter plot - numerical & numerical
          conditionalPanel(
            condition = "input.plottype == 'scatter'",
            selectInput("scatter_num_var_2",
                        "Choose the second numerical variable that you want to investigate:",
                        c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week"),
                        selected = "education_num")
          ),
          
          # scatter plot - numerical & numerical
          conditionalPanel( 
            condition = "input.plottype == 'scatter'",
            selectInput("scatter_cat_var",
                        "Choose the categirical variable that you want to compare:",
                        c("Work class" = "workclass",
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
            selectInput("box_num_var",
                        "Choose the numerical variable that you want to investigate:",
                        c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week"))
          ),
          
          # box plot - numerical & categorical
          conditionalPanel( 
            condition = "input.plottype == 'box'",
            selectInput("box_cat_var",
                        "Choose the categorical variable that you want to compare:",
                        c("Work class" = "workclass",
                          "Marital status" = "marital_status",
                          "Occupation" = "occupation",
                          "Race" = "race",
                          "Gender" = "sex",
                          "Native or International" =  "Native",
                          "Income Category" = "income"))
          ),
          
        ), 
        #closing data exploration sidebarpanel
        
        # Show a plot of the generated distribution
        mainPanel(
          
        ) 
        # closing data exploration mainpanel
        
      ) 
      # closing data exploration sidebarlayout
      
  ), 
  # closing data exploration tabpanel
  
  
    # Modeling Tab
    tabPanel(
      "Modeling",
      # Sub tabs
      tabsetPanel(
        
        # sub tab 1: Modeling Info
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
        # sub tab 2: Model Fitting
        tabPanel(
          "Model Fitting"
        ),
        
        # sub tab 3: Prediction
        tabPanel(
          "Prediction"
        )
      )
      # sidebarLayout(
      #   sidebarPanel(
      #     select
      #     
      #   ), #closing modeling sidebarpanel
      #   
      #   mainPanel(
      #     
      #   ) # closing modeling mainpanel
      # ) # closing modeling sidebarlayout
    ) # closing modeling tabpanel
    
)#closing tabsetpanel

)  


