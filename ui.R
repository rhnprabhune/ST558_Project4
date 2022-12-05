library(shiny)
library(shinydashboard)

dashboardPage(skin = "red",
  dashboardHeader(title = "ST 558- Project 4"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About",tabName="about",icon = icon("home", lib = "glyphicon")),
      menuItem("Data Exploration",tabName="data_exp",
               icon=icon("signal",lib="glyphicon")),
      menuItem("Modeling",tabName = "model",icon=icon("play",lib="glyphicon")),
      menuItem("Data",tabName="data",icon=icon("th-list",lib="glyphicon"))
    )
  ),
  dashboardBody(
    tabItems(
     tabItem(tabName = "about",
      fluidRow(
        column(12,align="center",div(style="margin:0%"),
            imageOutput("img"),
            h4("Created by:",a(href="https://www.linkedin.com/in/rohan-prabhune-57b083174/",
                               "Rohan Prabhune",style="font-size:20px;"))
        )
      ),
      fluidRow(
        column(3),
        column(6,align = "center",
           box(width=NULL,height=45,background="red",
              title="About this app"
           )
        ),
        column(3)
      ),
      fluidRow(
        column(4,
          box(width=NULL,title="Purpose of the App",status="danger",solidHeader = TRUE,
            h4("This application was created as a part of course project for ST 558 in Fall 2022.
               The goal of this project is to create a shiny app that can be used to explore data and model it."),
            h4("Shiny is an R package that makes it easy to build interactive web apps straight from R.
               You can host standalone apps on a webpage or embed them in R Markdown documents or build dashboards.
               This is one such application where I have used shiny to create dashboard to performing predictive
               modelling on a dataset taken from ",a(href="https://www.kaggle.com/","Kaggle",style="font-size:20px;")),
            h4("This application is created specifically using ",
               a(href="https://rstudio.github.io/shinydashboard/index.html","shinydashboard",style="font-size:20px;"),
               "which allows you to customize the header, sidebar and body of the app.")
          )
        ),
        column(4,
          box(width=NULL,title="Data",status="danger",solidHeader = TRUE,
            h4("This Heart Disease Dataset dates from 1988 and consists of four databases: 
               Cleveland, Hungary, Switzerland, and Long Beach V combined. 
               The original dataset contained 76 attributes, including the predicted attribute, 
               but all published experiments refer to using a subset of 14 of them. So for this application I 
               have considered these 14 attributes only. 
               The \"target\" field refers to the presence of heart disease in the patient. 
               It is integer valued 0 = no disease and 1 = disease."),
            h4("The dataset can be found ",a(href="https://www.kaggle.com/datasets/johnsmith88/heart-disease-dataset",
                                             "here",style="font-size:18px;")),
            h4("You can find more information on the 14 attributes used in the dataset and the relevant research papers
               that cite this data set ",
               a(href="https://archive.ics.uci.edu/ml/datasets/heart+disease","here",style="font-size:18px;"))
          )
        ),
        column(4,
          box(width=NULL,title="Navigating the application",status="danger",solidHeader = TRUE,
            h4("The application consists of 4 tabs in the sidebar"),
            h4("1. About: (You are here!)"),
            h4("2. Data Exploration:"),
            h4("This page allows you to do Exploratory Data Analysis on the dataset
               which includes Quantitaive data graphs, Categorical data graphs,
                Graphical analysis with target variable and Correlation plots."),
            h4("3. Modeling:"),
            h4("This tab allows the user to fit 3 classificaiton models to the data: Generalized linear model,
               Classification tree and a Random Forest model. Here you can find the infomartion on each of these models,
               tune the hyperparameters of the model, check the fit statistics and perform predictive analysis."),
            h4("4. Data:"),
            h4("Here you can check out the data used for this application, subset the dataset and 
               obtain a .csv file.")
          )
        )
      )
     ),  #Tab item
     tabItem(tabName = "data_exp",
      fluidRow(
       column(3,
        box(width=NULL,height=405,title="Numerical Summaries of variables",
            checkboxGroupInput("num_summary",
                               "Select the variables for which you want the numerical summaries",
                                c("Age"="age","Resting Blood Pressure(trestbps)"="trestbps",
                                  "Cholestrol(chol)"="chol","Max. heart rate(thalach)"="thalach",
                                  "Old peak"="oldpeak"),
                               selected=c("age","trestbps","chol","thalach","oldpeak"))
        )
       ),
       column(9,
        box(width=NULL,title="Numerical summaries",
            dataTableOutput("numerical_summaries")
        )
       )
      ),
      fluidRow(
        column(3,
          box(width=NULL,title="Variable selection for Contingency tables",
              h5("Here you can select the categorical variables to find their two way contingency tables"),
              selectInput("cont_var1","Select variable 1",
                          c("Gender"="sex",
                            "Chest Pain type"="cp",
                            "Fasting blood sugar"="fbs",
                            "Resting ECG"="restecg",
                            "Blood disorder(thalassemia)"="thal",
                            "Execrcise induced Angina"="exang",
                            "Major vessels colored by flourosopy"="ca"),selected="sex"),
              selectInput("cont_var2","Select variable 2",
                          c("Gender"="sex",
                            "Chest Pain type"="cp",
                            "Fasting blood sugar"="fbs",
                            "Resting ECG"="restecg",
                            "Blood disorder(thalassemia)"="thal",
                            "Execrcise induced Angina"="exang",
                            "Major vessels colored by flourosopy"="ca"),selected="cp")
          )       
        ),
        column(9,
          box(width=NULL,title="Contingency Tables",
              dataTableOutput("contingency_table"))
        )
      ),
      fluidRow(
        column(1
        ),
        column(10,align = "center",
               box(width=NULL,height=45,title="Quantitative Data Graphs",background="red")
        ),
        column(1
        )
      ),
      fluidRow(
        column(2,
          box(width=NULL,title="Plots for numerical variables",
            selectInput("box_or_hist","Select the kinds of plots you want to see",
                        c("Only Boxplots"="box_only","Only Histograms"="hist_only",
                          "Both Box plots and Histograms"="both_box_hist"),selected="both_box_hist")
          )       
        ),
        column(5,
          box(width=NULL,
            plotOutput('bh_plot1')
          ),
          box(width=NULL,
            plotOutput('bh_plot3')
          ),
          conditionalPanel(
            condition = "input.box_or_hist == 'both_box_hist'",
            box(width=NULL,
              plotOutput("bh_plot5")
            ),
            box(width=NULL,
              plotOutput("bh_plot7")
            )
          ) 
        ),
        column(5,
          box(width=NULL,
            plotOutput('bh_plot2')
          ),
          box(width=NULL,
            plotOutput('bh_plot4')
          ),
          conditionalPanel(
            condition = "input.box_or_hist == 'both_box_hist'",
            box(width=NULL,
                plotOutput("bh_plot6")
            ),
            box(width=NULL,
                plotOutput("bh_plot8")
            )
          )
        )
      ),
      fluidRow(
        column(1
        ),
        column(10,align = "center",
               box(width=NULL,height=45,title="Categorical Data Graphs",background="red")
        ),
        column(1
        )
      ),
      fluidRow(
        column(2,
          box(width=NULL,title="Plots for categorical variables",
            checkboxGroupInput("cat_plots",
                               "Select the variables for which you want to see bar plots",
                               c("Chest Pain type"="cp",
                                 "Fasting blood sugar"="fbs",
                                 "Resting ECG"="restecg",
                                 "Blood disorder(thalassemia)"="thal",
                                 "Execrcise induced Angina"="exang",
                                 "Major vessels colored by flourosopy"="ca"),
                               selected=c("cp","fbs","restecg","thal","exang","ca"))
          )
        ),
        column(10,
          fluidRow(
            column(12,
              box(width=NULL,
                plotOutput("cat_gender")
              ),
              conditionalPanel(
                condition = "input.cat_plots.includes('cp')",
                box(width=NULL,
                    plotOutput("cat_cp"))
              ),
              conditionalPanel(
                condition = "input.cat_plots.includes('fbs')",
                box(width=NULL,
                    plotOutput("cat_fbs"))
              ),
              conditionalPanel(
                condition = "input.cat_plots.includes('restecg')",
                box(width=NULL,
                    plotOutput("cat_restecg"))
              ),
              conditionalPanel(
                condition = "input.cat_plots.includes('thal')",
                box(width=NULL,
                    plotOutput("cat_thal"))
              ),
              conditionalPanel(
                condition = "input.cat_plots.includes('exang')",
                box(width=NULL,
                    plotOutput("cat_exang"))
              ),
              conditionalPanel(
                condition = "input.cat_plots.includes('ca')",
                box(width=NULL,
                    plotOutput("cat_ca"))
              )
            )
          )
        )
      ),
      fluidRow(
        column(1
        ),
        column(10,align = "center",
               box(width=NULL,height=45,title="Exploratory Data Analysis with target variable",background="red")
        ),
        column(1
        )
      ),
      fluidRow(
        column(3,
          box(width=NULL,title="Scatter plot",
              selectInput("scatter_var1","Select the numerical variable on x-axis",
                          c("Age"="age","Resting Blood Pressure"="trestbps",
                            "Cholestrol"="chol","Max. Heart Rate"="thalach",
                            "Old peak"="oldpeak"),selected="age"),
              selectInput("scatter_var2","Select the numerical variable on y-axis",
                          c("Age"="age","Resting Blood Pressure"="trestbps",
                            "Cholestrol"="chol","Max. Heart Rate"="thalach",
                            "Old peak"="oldpeak"),selected="trestbps"),
              selectInput("scatter_var3","Select by categorical variable for color",
                          c("Gender"="sex",
                            "Chest Pain type"="cp",
                            "Fasting blood sugar"="fbs","Resting ECG"="restecg",
                            "Blood disorder(thalassemia)"="thal",
                            "Execrcise induced Angina"="exang",
                            "Major vessels colored by flourosopy"="ca"),selected="sex"),
              sliderInput("slider","Size of Points on Graph",
                          min=2,max=5,value =3,step=0.5)
          )
        ),
        column(9,
          plotOutput("scatter")
        )
      ),
      fluidRow(
        column(3,
          box(width=NULL,title="Analysis of Categorical vairables for target",
              selectInput("bar_var","Select the categorical variable",
                          c("Gender"="sex",
                            "Chest Pain type"="cp",
                            "Fasting blood sugar"="fbs",
                            "Resting ECG"="restecg",
                            "Blood disorder(thalassemia)"="thal",
                            "Execrcise induced Angina"="exang",
                            "Major vessels colored by flourosopy"="ca"),selected="cp")
          )
        ),
        column(9,
          plotOutput("Bar_taget")
        )
      ),
      fluidRow(
        column(1
        ),
        column(10,align = "center",
               box(width=NULL,height=45,title="Correlation plots",background="red")
        ),
        column(1
        )
      ),
      fluidRow(
        column(1
        ),
        column(10,align = "center",
               plotOutput("corrplot")
        ),
        column(1
        )
      )
    ), #tabItem
    tabItem(tabName = "model",
      tabsetPanel(
        tabPanel("Modeling Info",
          fluidRow(
            column(12,
              box(width=NULL,title="Generalized Linear Model: Binary Logistic Regression",
                  status="danger",solidHeader = TRUE,
                h4("This type of statistical model (also known as logit model) is
                often used for classification.Logistic regression estimates the 
                probability of an event occurring based on a given dataset of 
                   independent variables. 
                   The dependent variable is bounded between 0 and 1."),
                h4("In logistic regression, a logit transformation is applied on 
                the odds—that is, the probability of success divided by the 
                probability of failure. This is also commonly known as the log 
                odds, or the natural logarithm of odds, and this logistic function
                   is represented by the following formulas:"),
                withMathJax(),
                helpText('$$logit(p) = log(\\frac{p}{1-p}) = \\beta_0 + \\beta_1 \\cdot x_1 + 
                         \\beta_2 \\cdot x_2 + ... + \\beta_k \\cdot x_k$$'),
                h5("Here the ",tags$b("logit"),
                " function is called the ",
                tags$b("link"),
                " function and ",
                tags$b("p = average number of successes at a given x")),
                h4("This method tests different values of beta through multiple 
                   iterations to optimize for the best fit of log odds. 
                   For binary classification, a probability less than .5 will 
                   predict 0 while a probability greater than 0 will predict 1"),
                h4(tags$b("Advantages:"),
                   tags$br(),
                   "Easy to implement, interpret, and very efficient to train.",
                   tags$br(),
                   "Good accuracy for many simple data sets and it performs well when 
                   the dataset is linearly separable.",
                   tags$br(),
                   "It can easily extend to multiple classes(multinomial regression) and 
                   a natural probabilistic view of class predictions."),
                h4(tags$b("Drawbacks:"),
                   tags$br(),
                   "If the number of observations is lesser than the number 
                   of features, it may lead to overfitting",
                   tags$br(),
                   "Non-linear problems can’t be solved with logistic regression 
                   because it has a linear decision surface",
                   tags$br(),
                   "It is tough to obtain complex relationships using logistic 
                   regression")
              )
            )
          ),
          fluidRow(
            column(12,
              box(width=NULL,title="Classification Tree",
                  status="danger",solidHeader = TRUE
              )
            )
          ),
          fluidRow(
            column(12,
              box(width=NULL,title="Random Forest Model",
                  status="danger",solidHeader = TRUE
              )
            )
          )
        ),
        tabPanel("Model Fitting"),
        tabPanel("Prediction")
      )
    ), #tabItem
    tabItem(tabName = "data"
    )
   ) #tabItems
  ) #dashboardBody
) #dashboardPage