# ST558 Final Project
- Introduction
  - This app is to predict whether income exceeds $50K/yr based on 1994 Census data. This dataset has more than 30,000 records with 15 variables:
    - age
    - workclass
    - final weight
    - education level
    - years of education
    - marital status
    - occupation
    - relationship
    - race
    - sex
    - capital gain
    - captal loss
    - working hours per week
    - native country
    - income category
  
- packages required to run the app:
  - `r shiny`
  - `r tidyverse`
  - `r caret`

- Install packages
  ```r
  install.packages("shiny")
  install.packages("tidyverse")
  install.packages("caret")
  ```
- Run the app
  ```r
  shiny::runGitHub("ST558Project4", "XLin-NCSU")
  ```
