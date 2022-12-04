library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(corrplot)
library(caret)

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
gg_box1 <- ggplot(train_df,aes(age)) +
  geom_boxplot(fill="steelblue") +
  labs(x="Age",title="Box plot for Age") + 
  theme(text=element_text(size=15))
gg_hist1 <- ggplot(train_df,aes(x=age)) + 
  geom_histogram(aes(y = ..density..),fill="steelblue") +
  geom_density() +
  labs(x="Age",title="Histogram for Age") + 
  theme(text=element_text(size=15))
gg_box2 <- ggplot(train_df,aes(trestbps)) +
  geom_boxplot(fill="steelblue") +
  labs(x="Resting Blood Pressure",title="Box plot for Resting Blood Pressure (trestbps)") + 
  theme(text=element_text(size=15))
gg_hist2 <- ggplot(train_df,aes(x=trestbps)) + 
  geom_histogram(aes(y = ..density..),fill="steelblue") +
  geom_density() +
  labs(x="Resting Blood Pressure",title="Histogram for Resting Blood Pressure (trestbps)") + 
  theme(text=element_text(size=15))
gg_box3 <- ggplot(train_df,aes(chol)) +
  geom_boxplot(fill="steelblue") +
  labs(x="Cholestrol",title="Box plot for Cholestrol (chol)") + 
  theme(text=element_text(size=15))
gg_hist3 <- ggplot(train_df,aes(x=chol)) + 
  geom_histogram(aes(y = ..density..),fill="steelblue") +
  geom_density() +
  labs(x="Cholestrol",title="Histogram for Cholestrol (chol)") + 
  theme(text=element_text(size=15))
gg_box4 <- ggplot(train_df,aes(thalach)) +
  geom_boxplot(fill="steelblue") +
  labs(x="Max. Heart Rate",title="Box plot for Max. Heart Rate (thalach)") + 
  theme(text=element_text(size=15))
gg_hist4 <- ggplot(train_df,aes(x=thalach)) + 
  geom_histogram(aes(y = ..density..),fill="steelblue") +
  geom_density() +
  labs(x="Max. Heart Rate",title="Histogram for Max. Heart Rate (thalach)") + 
  theme(text=element_text(size=15))
#----------------------------------------------------------------------
#---------------------------------------------------------------------
# Visualizations for categorical variables

#Sex
cat_gender <- ggplot(train_df,aes(x=gender)) + geom_bar(width=0.5,aes(fill=gender)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Sex") + 
  labs(x ="Gender",y="Count",title="Bar plot for Gender") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))

#Chest pain type
cat_cp <- ggplot(train_df,aes(x=cp_type)) + geom_bar(width=0.5,aes(fill=cp_type)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Chest Pain Type") + 
  labs(x ="Chest pain type",y="Count",title="Bar plot for Chest Pain Type") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))

#Fasting blood sugar
cat_fbs <- ggplot(train_df,aes(x=fbs_type)) + geom_bar(width=0.5,aes(fill=fbs_type)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Fasting Blood Sugar") + 
  labs(x ="Fasting Blood Sugar",y="Count",
       title="Bar plot for Fasting Blood Sugar") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))

#Exercise Induced Angina
cat_exang <- ggplot(train_df,aes(x=exang_type)) + geom_bar(width=0.5,aes(fill=exang_type)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Exercise Induced Angina") + 
  labs(x ="Exercise Induced Angina",y="Count",
       title="Bar plot for Exercise Induced Angina") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))

#Resting ECG
cat_restecg <- ggplot(train_df,aes(x=restecg_type)) + geom_bar(width=0.5,aes(fill=restecg_type)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Resting ECG") + 
  labs(x ="Resting ECG",y="Count",title="Bar plot for Resting ECG") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))

#Colored blood vessels
cat_ca <- ggplot(train_df,aes(x=ca)) + geom_bar(width=0.5,aes(fill=ca)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Number of major vessels colored by flourosopy") + 
  labs(x ="Number of major vessels colored by flourosopy",
       y="Count",
       title="Bar plot for Number of major vessels colored by flourosopy") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))

#Thal
cat_thal <- ggplot(train_df,aes(x=thal_type)) + geom_bar(width=0.5,aes(fill=thal_type)) + 
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5) +
  scale_fill_discrete(name = "Blood disorder(thalassemia)") + 
  labs(x ="Blood disorder(thalassemia)",y="Count",
       title="Bar plot for Blood disorder(thalassemia)") +
  theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))
#--------------------------------------------------------------------------

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
  
  #3. Box or histograms
  output$bh_plot1 <- renderPlot({
    if (input$box_or_hist=="box_only" | input$box_or_hist =="both_box_hist"){
      gg_box1
    }
    else if (input$box_or_hist=="hist_only"){
      gg_hist1
    }
  })
  
  output$bh_plot2 <- renderPlot({
    if (input$box_or_hist=="box_only"){
      gg_box2
    }
    else if (input$box_or_hist=="hist_only"){
      gg_hist2
    }
    else if (input$box_or_hist=="both_box_hist"){
      gg_hist1
    }
  })
  
  output$bh_plot3 <- renderPlot({
    if (input$box_or_hist=="box_only"){
      gg_box3
    }
    else if (input$box_or_hist=="hist_only"){
      gg_hist3
    }
    else if (input$box_or_hist=="both_box_hist"){
      gg_box2
    }
  })
  
  output$bh_plot4 <- renderPlot({
    if (input$box_or_hist=="box_only"){
      gg_box4
    }
    else if (input$box_or_hist=="hist_only"){
      gg_hist4
    }
    else if (input$box_or_hist=="both_box_hist"){
      gg_hist2
    }
  })
  output$bh_plot5 <- renderPlot({gg_box3})
  output$bh_plot6 <- renderPlot({gg_hist3})
  output$bh_plot7 <- renderPlot({gg_box4})
  output$bh_plot8 <- renderPlot({gg_hist4})
  
  #3. Bar plots
  output$cat_gender <- renderPlot({cat_gender})
  output$cat_cp <- renderPlot({cat_cp})
  output$cat_restecg <- renderPlot({cat_restecg})
  output$cat_ca <- renderPlot({cat_ca})
  output$cat_thal <- renderPlot({cat_thal})
  output$cat_fbs <- renderPlot({cat_fbs})
  output$cat_exang <- renderPlot({cat_exang})
  
  #4. Scatter plot
  #EDA with Target variable (Numerical variables)
  
  output$scatter <- renderPlot({
    target_type_str = "target_type"
    input_target_num_str1 <- ct_tibble[og==input$scatter_var1,]$print_string
    input_target_num_str2 <- ct_tibble[og==input$scatter_var2,]$print_string
    input_target_num3_type <- ct_tibble[og==input$scatter_var3,]$new
    input_target_num_str3 <- ct_tibble[og==input$scatter_var3,]$print_string
    
    ggplot(train_df,aes_string(x=input$scatter_var1,y=input$scatter_var2)) + 
      geom_point(aes_string(color=input_target_num3_type,shape=target_type_str),
                 size=input$slider) + geom_smooth(method = "lm") + 
      scale_color_discrete(name = input_target_num_str3) + 
      scale_shape_discrete(name = "Heart Disease") +
      labs(x =input_target_num_str1,y=input_target_num_str2,
           title=paste0("Scatter plot for ",input_target_num_str2," vs ",
                        input_target_num_str1)) +
      theme(text=element_text(size=15))
  })
  
  #5. EDA with Target variable (Categorical variables)
  output$Bar_taget <- renderPlot({
    input_target_cat = input$bar_var
    target_variable = "target_type"
    input_target_type <- ct_tibble[og==input_target_cat,]$new
    input_target_str <- ct_tibble[og==input_target_cat,]$print_string
    
    ggplot(train_df,aes_string(x=input_target_type,fill=target_variable)) + 
      geom_bar(position = "dodge",width=0.5) +
      scale_fill_discrete(name = "Heart Disease") +
      labs(x =input_target_str,y="Count",
           title=paste0("Heart disease based on ",input_target_str)) +
      theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))+
      geom_text(aes(label = ..count..), stat = "count", vjust = -0.5,
                position = position_dodge(width = 0.5))
  })
  
  #6. Correlation plot
  output$corrplot <- renderPlot({
    corr_data1 <- train_df %>% select(age,trestbps,chol,thalach,oldpeak)
    corr1 = cor(corr_data1,method = c("spearman"))
    corrplot(corr1,diag=FALSE)
    
  })
  
  #7. About - Image
  output$img <- renderImage({
    filename <- normalizePath(file.path('./images',"ecg2.png"))
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = "ECG image",width=1000,height=300)
  }, deleteFile = FALSE)

}
