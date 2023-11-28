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
  
  tabsetPanel(
    type = "tabs",
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
        ), #closing table
      tags$br(),
      tags$p("Since fnlwgt is not relevent to this study, while education and relationship have similar infomation with education_num and marital_status, I removed these three variables from the dataset."),
      tags$p("Moreover, I created a variable 'Native' indicating whether the worder's native country was United States or not to replace the native_country variable.")
    ), #closing tab panel
    
    tabPanel(
      "Data Exploration",
      sidebarLayout(
        sidebarPanel(
          selectInput("plottype",
                      "Choose your plot", # Four type of plots
                      c("Histogram" = "hist",
                        "Bar Graph" = "bar",
                        "Scatter Plot" = "scatter",
                        "Box Plot" = "box")
            ),
          
          conditionalPanel( # histogram - numerical var
            condition = "input.plottype == 'hist'",
            selectInput("hist_num_var",
                        "Choose the numerical variable that you want to investigate:",
                        c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week",
                          "Capital gain" = "capital_gain",
                          "Capital loss" = "capital_loss"))
          ),
          
          conditionalPanel( # histogram - categorical var comparison
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
          
          conditionalPanel( # histogram - row filter on income
            condition =  "input.plottype == 'hist'",
            selectInput("hist_cat_income",
                        "filter the income level",
                        c("<= 50K" = "<= 50K",
                          ">50K" = ">50K"))
          ),
          
          conditionalPanel( # bar plot - categorical var
            condition =  "input.plottype == 'bar'",
            selectInput("bar_cat_var",
                        "Choose the categorical variable that you want to investigate:",
                        c("Work class" = "workclass",
                          "Marital status" = "marital_status",
                          "Occupation" = "occupation",
                          "Race" = "race",
                          "Gender" = "sex",
                          "Native or International" =  "Native",
                          "Income Category" = "income"))
          ),
          
          conditionalPanel( # scatter plot - numerical & numerical
            condition = "input.plottype == 'scatter'",
            selectInput("scatter_num_var_1",
                        "Choose the first numerical variable that you want to investigate:",
                        c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week",
                          "Capital gain" = "capital_gain",
                          "Capital loss" = "capital_loss"),
                        selected = "age")
          ),
          
          conditionalPanel( # scatter plot - numerical & numerical
            condition = "input.plottype == 'scatter'",
            selectInput("scatter_num_var_2",
                        "Choose the second numerical variable that you want to investigate:",
                        c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week",
                          "Capital gain" = "capital_gain",
                          "Capital loss" = "capital_loss"),
                        selected = "education_num")
          ),
          
          conditionalPanel( # scatter plot - numerical & numerical
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
          
          conditionalPanel( # box plot - numerical & categorical
            condition = "input.plottype == 'box'",
            selectInput("box_num_var",
                        "Choose the numerical variable that you want to investigate:",
                        c("Age" = "age",
                          "Years of education" = "education_num",
                          "Working hours per week" = "hours_per_week",
                          "Capital gain" = "capital_gain",
                          "Capital loss" = "capital_loss"))
          ),
          
          conditionalPanel( # box plot - numerical & categorical
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
          
          
          
        ), #closing data exploration sidebarpanel
        
        # Show a plot of the generated distribution
        mainPanel(
          
        ) # closing data exploration mainpanel
      ) # closing data exploration sidebarlayout
  ), # closing data exploration tabpanel
  
  tabPanel(
    "Modeling",
    
    tabsetPanel(
      tabPanel(
        "Modeling Info"
      ),
      
      tabPanel(
        "Model Fitting"
      ),
      
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


