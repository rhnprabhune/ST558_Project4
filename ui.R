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
        column(1,
        ),
        column(10,align = "center",
               box(width=NULL,height=45,title="Quantitative Data Graphs",background="red")
        ),
        column(1,
        )
      ),
      fluidRow(
        column(2,
          box(width=NULL,title="Plots for numerical variables",
            selectInput("box_or_hist","Select the kinds of plots you want to see",
                        c("Only Boxplots"="box_only","Only Histograms"="hist_only",
                          "Both Box plots and Histograms"="both_box_hist"),selected="box_only")
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
        column(1,
        ),
        column(10,align = "center",
               box(width=NULL,height=45,title="Categorical Data Graphs",background="red")
        ),
        column(1,
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
                                 "Major vessels colored by flourosopy"="ca"))
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
      )
      
      
      )
    )
  )
)