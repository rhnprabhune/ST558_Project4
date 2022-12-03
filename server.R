library(shiny)
library(shinydashboard)
library(tidyverse)
library(kableExtra)
library(DT)

data <- read_csv("data/heart.csv")
data$ca <- ordered(as.factor(data$ca),
                   levels = c("0","1","2","3"))
#Tibble for printing ease
og <- c("sex","cp","fbs","restecg","thal","exang","ca","age","trestbps","chol",
        "thalach","oldpeak")
new <- c("gender","cp_type","fbs_type","restecg_type","thal_type",
         "exang_type","ca","age","trestbps","chol","thalach","oldpeak")
print_string <- c("Sex","Chest Pain Type","Fasting blood sugar",
                  "Resting ECG result","Blood disorder(thalassemia)",
                  "Exercise induced angina",
                  "Number of major vessels colored by flourosopy","Age",
                  "Resting Blood Pressure","Serum cholestoral in mg/dl",
                  "Maximum heart rate",
                  "ST depression induced by exercise relative to rest")
ct_tibble <- tibble(og,new,print_string)

#---------------------------------------------------------------------
#Split data
set.seed(52)
split=0.7
# Get the indexes for training data
train_size <- sample(nrow(data), nrow(data)*split)
# Get training data
train_df <- data[train_size,]
# Get test data
test_df <- data[-train_size,] 
#---------------------------------------------------------------------
#Data manipulation
train_df$gender <- if_else(train_df$sex==1,"Male","Female")

train_df$cp_type <- ifelse(train_df$cp==0,"Typical Angina",
                           if_else(train_df$cp==1,"Atypical Angina",
                                   if_else(train_df$cp==2,"Non-Anginal",
                                           "Aysmptomatic")))
train_df$cp_type <- ordered(as.factor(train_df$cp_type),
                            levels = c("Typical Angina","Atypical Angina",
                                       "Non-Anginal","Aysmptomatic"))

train_df$fbs_type <- ifelse(train_df$fbs==1,"Greater than 120 mg/dl",
                            "Less than 120 mg/dl")
train_df$fbs_type <- ordered(as.factor(train_df$fbs_type),
                             levels = c("Less than 120 mg/dl",
                                        "Greater than 120 mg/dl"))

train_df$restecg_type <- ifelse(train_df$restecg==0,"Normal",
                                ifelse(train_df$restecg==1,
                                       "Having ST-T wave abnormality",
                                       "Showing left ventricular hypertrophy"))
train_df$restecg_type <- ordered(as.factor(train_df$restecg_type),
                                 levels = c("Normal",
                                            "Having ST-T wave abnormality",
                                            "Showing left ventricular hypertrophy"))

train_df$thal_type <- ifelse(train_df$thal==1,"Normal",
                             if_else(train_df$thal==2,"Fixed defect",
                                     if_else(train_df$thal==3,"Reversable defect",
                                             "Not captured")))
train_df$thal_type <- ordered(as.factor(train_df$thal_type),
                              levels = c("Normal","Fixed defect",
                                         "Reversable defect","Not captured"))

train_df$exang_type <- ifelse(train_df$exang==1,"Yes","No")
train_df$exang_type <- ordered(as.factor(train_df$exang_type),
                               levels = c("No","Yes"))

train_df$target_type <- ifelse(train_df$target==0,"No disease","Disease")
train_df$target_type <- ordered(as.factor(train_df$target_type),
                                levels = c("No disease","Disease"))

#----------------------------------------------------------------------

function(input, output, session) { 
  
  #1. Numerical Summaries data table
  output$numerical_summaries <- DT::renderDataTable({
    variables_selected = input$num_summary
    df_summary <- train_df %>%
      select(all_of(variables_selected))
    predictor_table <- apply(df_summary, MARGIN = 2,FUN = summary, na.rm = TRUE)
  })
  
  #2. Contingency tables
  output$contingency_table <- DT::renderDataTable({
    input_eda1 = input$cont_var1
    input_eda2 = input$cont_var2
    
    input_eda1_type <- ct_tibble[og==input_eda1,]$new
    input_eda1_str <- ct_tibble[og==input_eda1,]$print_string
    input_eda2_type <- ct_tibble[og==input_eda2,]$new
    input_eda2_str <- ct_tibble[og==input_eda2,]$print_string
    
    temp_df <- tibble(a=train_df[input_eda1_type][[1]],
                      b=train_df[input_eda2_type][[1]])
    
    tab <- as.data.frame.matrix(table(train_df[input_eda1_type][[1]],train_df[input_eda2_type][[1]]))
  })
  
  

  
  
}
