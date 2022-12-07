# ST558_Project4

---
## Brief description
This application was created as a part of course project for ST 558 in Fall 2022. The goal of this project is to create a shiny app that can be used to explore data and model it.

Shiny is an R package that makes it easy to build interactive web apps straight from R. You can host standalone apps on a webpage or embed them in R Markdown documents or build dashboards. This is one such application where I have used shiny to create dashboard to performing predictive modelling on a dataset.
This [Heart Disease Dataset](https://www.kaggle.com/datasets/johnsmith88/heart-disease-dataset) dates from 1988 and consists of four databases: Cleveland, Hungary, Switzerland, and Long Beach V combined. The original dataset contained 76 attributes, including the predicted attribute, but all published experiments refer to using a subset of 14 of them. So for this application I have considered these 14 attributes only. The "target" field refers to the presence of heart disease in the patient. It is integer valued 0 = no disease and 1 = disease.

This application is created specifically using [shinydashboard](https://rstudio.github.io/shinydashboard/index.html) which allows you to customize the header, sidebar and body of the app.

---
## Packages needed to run the app

1. shiny
2. shinydashboard
3. tidyverse
4. DT
5. corrplot
6. caret

### R code to install and use these packages
Just run this line of code in your R terminal
```
install.packages(c("shiny","shinydashboard","tidyverse","DT","corrplot","caret"))
```

### Run the app
You can run this app without having to clone this repository. Just install the packages given above and run the following command in your R terminal
```
shiny::runGitHub("ST558_Project4","rhnprabhune")
```

I have also hosted this app. Do check it out - https://rohansrprojects.shinyapps.io/st558_project4/
