library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(corrplot)
library(caret)

data <- read_csv("data/heart.csv")
data$ca <- ordered(as.factor(data$ca),
                   levels = c("0","1","2","3"))
data$sex <- as_factor(data$sex)
data$cp <- as_factor(data$cp)
data$fbs <- as_factor(data$fbs)
data$restecg <- as_factor(data$restecg)
data$exang <- as_factor(data$exang)
data$slope <- as_factor(data$slope)
data$thal <- as_factor(data$thal)
data$target <- as_factor(data$target)
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
function(input, output, session) { 
  
  #0. Split Data
  set_up_vals <- eventReactive(input$split, {
    #Split data
    set.seed(52)
    split=input$split_number
    train_size <- sample(nrow(data), nrow(data)*split)
    train_df <- data[train_size,] 
    test_df <- data[-train_size,] 
    
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
    #Box and histograms
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
    
    #RETURN
    list(train_df=train_df,test_df=test_df,
         gg_box1=gg_box1,gg_box2=gg_box2,gg_box3=gg_box3,gg_box4=gg_box4,
         gg_hist1=gg_hist1,gg_hist2=gg_hist2,gg_hist3=gg_hist3,gg_hist4=gg_hist4,
         cat_gender=cat_gender,cat_cp=cat_cp,cat_fbs=cat_fbs,cat_exang=cat_exang,
         cat_restecg=cat_restecg,cat_ca=cat_ca,cat_thal=cat_thal)
    
  },ignoreNULL = FALSE)
  #-----------------------------------------------------------------------------
  
  #1. Numerical Summaries data table
  output$numerical_summaries <- DT::renderDataTable({
    
    variables_selected = input$num_summary
    df_summary <- set_up_vals()$train_df %>%
      select(all_of(variables_selected))
    predictor_table <- apply(df_summary, MARGIN = 2,FUN = summary, na.rm = TRUE)
    tab <- as.data.frame.matrix(predictor_table)
    #df <- as_tibble((predictor_table))
    #df$stats <- rownames(predictor_table) 
    #df <- df %>% select(stats,everything())
    #df
  })
  #-----------------------------------------------------------------------------
  #2. Contingency tables
  output$contingency_table <- DT::renderDataTable({
    input_eda1 = input$cont_var1
    input_eda2 = input$cont_var2
    
    input_eda1_type <- ct_tibble[og==input_eda1,]$new
    input_eda1_str <- ct_tibble[og==input_eda1,]$print_string
    input_eda2_type <- ct_tibble[og==input_eda2,]$new
    input_eda2_str <- ct_tibble[og==input_eda2,]$print_string
    
    tab <- as.data.frame.matrix(table(set_up_vals()$train_df[input_eda1_type][[1]],
                                      set_up_vals()$train_df[input_eda2_type][[1]]))
  })
  #-----------------------------------------------------------------------------
  #3. Box or histograms
  output$bh_plot1 <- renderPlot({
    if (input$box_or_hist=="box_only" | input$box_or_hist =="both_box_hist"){
      set_up_vals()$gg_box1
    }
    else if (input$box_or_hist=="hist_only"){
      set_up_vals()$gg_hist1
    }
  })
  
  output$bh_plot2 <- renderPlot({
    if (input$box_or_hist=="box_only"){
      set_up_vals()$gg_box2
    }
    else if (input$box_or_hist=="hist_only"){
      set_up_vals()$gg_hist2
    }
    else if (input$box_or_hist=="both_box_hist"){
      set_up_vals()$gg_hist1
    }
  })
  
  output$bh_plot3 <- renderPlot({
    if (input$box_or_hist=="box_only"){
      set_up_vals()$gg_box3
    }
    else if (input$box_or_hist=="hist_only"){
      set_up_vals()$gg_hist3
    }
    else if (input$box_or_hist=="both_box_hist"){
      set_up_vals()$gg_box2
    }
  })
  
  output$bh_plot4 <- renderPlot({
    if (input$box_or_hist=="box_only"){
      set_up_vals()$gg_box4
    }
    else if (input$box_or_hist=="hist_only"){
      set_up_vals()$gg_hist4
    }
    else if (input$box_or_hist=="both_box_hist"){
      set_up_vals()$gg_hist2
    }
  })
  output$bh_plot5 <- renderPlot({set_up_vals()$gg_box3})
  output$bh_plot6 <- renderPlot({set_up_vals()$gg_hist3})
  output$bh_plot7 <- renderPlot({set_up_vals()$gg_box4})
  output$bh_plot8 <- renderPlot({set_up_vals()$gg_hist4})
  #-----------------------------------------------------------------------------
  #3. Bar plots
  output$cat_gender <- renderPlot({set_up_vals()$cat_gender})
  output$cat_cp <- renderPlot({set_up_vals()$cat_cp})
  output$cat_restecg <- renderPlot({set_up_vals()$cat_restecg})
  output$cat_ca <- renderPlot({set_up_vals()$cat_ca})
  output$cat_thal <- renderPlot({set_up_vals()$cat_thal})
  output$cat_fbs <- renderPlot({set_up_vals()$cat_fbs})
  output$cat_exang <- renderPlot({set_up_vals()$cat_exang})
  #-----------------------------------------------------------------------------
  #4. Scatter plot
  #EDA with Target variable (Numerical variables)
  
  output$scatter <- renderPlot({
    target_type_str = "target_type"
    input_target_num_str1 <- ct_tibble[og==input$scatter_var1,]$print_string
    input_target_num_str2 <- ct_tibble[og==input$scatter_var2,]$print_string
    input_target_num3_type <- ct_tibble[og==input$scatter_var3,]$new
    input_target_num_str3 <- ct_tibble[og==input$scatter_var3,]$print_string
    
    ggplot(set_up_vals()$train_df,aes_string(x=input$scatter_var1,y=input$scatter_var2)) + 
      geom_point(aes_string(color=input_target_num3_type,shape=target_type_str),
                 size=input$slider) + geom_smooth(method = "lm") + 
      scale_color_discrete(name = input_target_num_str3) + 
      scale_shape_discrete(name = "Heart Disease") +
      labs(x =input_target_num_str1,y=input_target_num_str2,
           title=paste0("Scatter plot for ",input_target_num_str2," vs ",
                        input_target_num_str1)) +
      theme(text=element_text(size=15))
  })
  #-----------------------------------------------------------------------------
  #5. EDA with Target variable (Categorical variables)
  output$Bar_taget <- renderPlot({
    input_target_cat = input$bar_var
    target_variable = "target_type"
    input_target_type <- ct_tibble[og==input_target_cat,]$new
    input_target_str <- ct_tibble[og==input_target_cat,]$print_string
    
    ggplot(set_up_vals()$train_df,aes_string(x=input_target_type,fill=target_variable)) + 
      geom_bar(position = "dodge",width=0.5) +
      scale_fill_discrete(name = "Heart Disease") +
      labs(x =input_target_str,y="Count",
           title=paste0("Heart disease based on ",input_target_str)) +
      theme(text=element_text(size=13), plot.title = element_text(hjust = 0.5))+
      geom_text(aes(label = ..count..), stat = "count", vjust = -0.5,
                position = position_dodge(width = 0.5))
  })
  #-----------------------------------------------------------------------------
  #6. Correlation plot
  output$corrplot <- renderPlot({
    corr_data1 <- set_up_vals()$train_df %>% select(age,trestbps,chol,thalach,oldpeak)
    corr1 = cor(corr_data1,method = c("spearman"))
    corrplot(corr1,diag=FALSE)
    
  })
  #-----------------------------------------------------------------------------
  #7. About - Image
  output$img <- renderImage({
    filename <- normalizePath(file.path('./images',"ecg2.png"))
    list(src = filename, alt = "ECG image",width=1000,height=300)
  }, deleteFile = FALSE)
  
  #8. Classification Tree - Image
  output$tree <- renderImage({
    filename <- normalizePath(file.path('./images',"tree.png"))
    list(src = filename, alt = "Tree image",width=500,height=300)
  }, deleteFile = FALSE)
  
  #9. Classification Tree - Image
  output$rf <- renderImage({
    filename <- normalizePath(file.path('./images',"rf.png"))
    list(src = filename,alt = "RF image",width=400,height=300)
  }, deleteFile = FALSE)
  
  #-----------------------------------------------------------------------------
  #10. TRAINING
  model_fits <- reactiveValues(test_df=NULL,train_df=NULL,fit_lg=NULL,
                               fit_tree=NULL,fit_rf=NULL,text_out=NULL)
  
  observeEvent(input$model_train,{
    if (input$model_train) {
      train_df <- set_up_vals()$train_df %>%
        select(c(-gender,-cp_type,-fbs_type,-restecg_type,-thal_type,-exang_type,-target_type))
      model_fits$train_df <- train_df %>% drop_na()
      model_fits$test_df <- set_up_vals()$test_df %>% drop_na()
      
      #Logistic Regression
      train_df_lg <- train_df %>% 
        select(target,all_of(input$train_var1))
      
      model_fits$fit_lg <- train(target ~ .,
                      data = train_df_lg,
                      method = "glm",
                      preProcess = c("center", "scale"),
                      trControl = trainControl(method = "cv", number = 10),
                      family="binomial")
      #Classification Tree
      train_df_tree <- train_df %>% 
        select(target,all_of(input$train_var2))
      
      model_fits$fit_tree = train(target ~ ., 
                       data=train_df_tree, 
                       method="rpart2",
                       preProcess = c("center", "scale"),
                       trControl = trainControl(method = "cv",number = 10),
                       tuneGrid = data.frame(maxdepth = input$max_depth))
      #Random Forest
      train_df_rf <- train_df %>% 
        select(target,all_of(input$train_var3))
      model_fits$fit_rf = train(target ~ ., 
                     data=train_df_rf, 
                     method="rf",
                     preProcess = c("center", "scale"),
                     trControl = trainControl(method = "cv",number = 5),
                     tuneGrid = data.frame(mtry = input$mtry)) 
      print("**************************************************")
      model_fits$text_out <- "...Training Complete..."
    }
    if (input$model_train==0) {
      model_fits$text_out <- ""
    }
  })
  
  output$model_fits <- renderText({
    model_fits$text_out
  })
  #-----------------------------------------------------------------------------  
  #11. Training Stats
  output$train_stats_lg <- renderPrint({
    if (is.null(model_fits$train_df)){
      train_accuracy_lg <- "Model not trained yet"
    } else {
      train_df <- model_fits$train_df
      train_pred_lg <- predict(model_fits$fit_lg,newdata = select(train_df,-target))
      train_accuracy_lg <- paste0(as.character(round((sum(train_pred_lg == train_df$target)
                                                      /nrow(train_df))*100,2)),"%")
      train_accuracy_lg  
    }
  })
  output$train_stats_lg_summary <- renderPrint({
      if (is.null(model_fits$fit_lg)){
        train_summary_lg <- "Model not trained yet"
      } else {
        train_summary_lg <- summary(model_fits$fit_lg)  
      }
    train_summary_lg
    })
  
  
  
  output$train_stats_tree <- renderPrint({
    if (is.null(model_fits$train_df)){
      train_accuracy_tree <- "Model not trained yet"
    } else {
      train_df <- model_fits$train_df
      train_pred_tree <- predict(model_fits$fit_tree,newdata = select(train_df,-target))
      train_accuracy_tree <- paste0(as.character(round((sum(train_pred_tree == train_df$target)
                                                      /nrow(train_df))*100,2)),"%")
      train_accuracy_tree  
    }
  })
  output$train_stats_tree_summary <- renderPrint({
    if (is.null(model_fits$fit_tree)){
      train_summary_tree <- "Model not trained yet"
    } else {
      train_summary_tree <- model_fits$fit_tree
    }
    train_summary_tree
  })
  
  
  
  output$train_stats_rf <- renderPrint({
    if (is.null(model_fits$train_df)){
      train_accuracy_rf <- "Model not trained yet"
    } else {
      train_df <- model_fits$train_df
      train_pred_rf <- predict(model_fits$fit_rf,newdata = select(train_df,-target))
      train_accuracy_rf <- paste0(as.character(round((sum(train_pred_rf == train_df$target)
                                                        /nrow(train_df))*100,2)),"%")
      train_accuracy_rf  
    }
  })
  output$train_stats_rf_summary <- renderPrint({
    if (is.null(model_fits$fit_rf)){
      train_summary_rf <- "Model not trained yet"
    } else {
      train_summary_rf <- model_fits$fit_rf
    }
    train_summary_rf
  })
  
  #-----------------------------------------------------------------------------
  #12.Testing Stats
  output$test_stats_lg <- renderPrint({
    if (is.null(model_fits$test_df)){
      test_accuracy_lg <- "Model not trained yet"
    } else {
      test_df <- model_fits$test_df
      test_pred_lg <- predict(model_fits$fit_lg,newdata = select(test_df,-target))
      test_accuracy_lg <- paste0(as.character(round((sum(test_pred_lg == test_df$target)
                                                      /nrow(test_df))*100,2)),"%")
      test_accuracy_lg  
    }
  })
  output$test_cf_lg <- renderPrint({
    if (is.null(model_fits$test_df)){
      test_cf_lg <- "Model not trained yet"
    } else {
      test_df <- model_fits$test_df
      test_pred_lg <- predict(model_fits$fit_lg,newdata = select(test_df,-target))
      test_cf_lg <- confusionMatrix(data = test_pred_lg, reference = test_df$target)
    }
    test_cf_lg
  })
  
  
  
  output$test_stats_tree <- renderPrint({
    if (is.null(model_fits$test_df)){
      test_accuracy_tree <- "Model not trained yet"
    } else {
      test_df <- model_fits$test_df
      test_pred_tree <- predict(model_fits$fit_tree,newdata = select(test_df,-target))
      test_accuracy_tree <- paste0(as.character(round((sum(test_pred_tree == test_df$target)
                                                        /nrow(test_df))*100,2)),"%")
      test_accuracy_tree  
    }
  })
  output$test_cf_tree <- renderPrint({
    if (is.null(model_fits$test_df)){
      test_cf_tree <- "Model not trained yet"
    } else {
      test_df <- model_fits$test_df
      test_pred_tree <- predict(model_fits$fit_tree,newdata = select(test_df,-target))
      test_cf_tree <- confusionMatrix(data = test_pred_tree, reference = test_df$target)
    }
    test_cf_tree
  })
  
  
  
  output$test_stats_rf <- renderPrint({
    if (is.null(model_fits$test_df)){
      test_accuracy_rf <- "Model not trained yet"
    } else {
      test_df <- model_fits$test_df
      test_pred_rf <- predict(model_fits$fit_rf,newdata = select(test_df,-target))
      test_accuracy_rf <- paste0(as.character(round((sum(test_pred_rf == test_df$target)
                                                      /nrow(test_df))*100,2)),"%")
      test_accuracy_rf  
    }
  })
  output$test_cf_rf <- renderPrint({
    if (is.null(model_fits$test_df)){
      test_cf_rf <- "Model not trained yet"
    } else {
      test_df <- model_fits$test_df
      test_pred_rf <- predict(model_fits$fit_rf,newdata = select(test_df,-target))
      test_cf_rf <- confusionMatrix(data = test_pred_rf, reference = test_df$target)
    }
    test_cf_rf
  })
  #-----------------------------------------------------------------------------
  #13. Prediction
  output$final_prediction <- renderText({
    if (is.null(model_fits$fit_lg)){
      out_str <- "Model not trained yet. Please train the model before predicting"
    } else {
      if (input$model_input == "lg"){fit <- model_fits$fit_lg}
      else if (input$model_input == "tree"){fit <- model_fits$fit_tree}
      else if (input$model_input == "rf"){fit <- model_fits$fit_rf}
      
      age <- input$p_age
      sex <- input$p_sex
      cp <- input$p_cp
      trestbps <- input$p_trestbps
      chol <- input$p_chol
      fbs <- input$p_fbs
      restecg <- input$p_restecg
      thalach <- input$p_thalach
      exang <- input$p_exang
      oldpeak <- input$p_oldpeak
      slope <- input$p_slope
      thal <- input$p_thal
      
      test_df <- tibble(age,sex,cp,trestbps,chol,fbs,restecg,thalach,exang,oldpeak,slope,thal)
      prediction <- predict(fit,newdata = test_df)
      if (prediction==1){out_str = "Heart Disease"}
      else if (prediction==0){out_str = "No Heart Disease"}
    }
    out_str
  })
  
  #----------------------------------------------------------------------------
  #14. Download CSV
  
  # Creating a reactive context and generating "thedata" from it
  thedata <- reactive({
    start <- as.numeric(input$offset)
    end <- as.numeric(input$offset) + as.numeric(input$count)
    data_out <- data %>%
      select(all_of(input$get_data)) %>%
      slice(start+1:end)
    data_out
  })
  
  output$data_csv <- DT::renderDataTable({thedata()})
  
  # Download handler
  output$download <- downloadHandler(
    filename = function(){"thename.csv"}, 
    content = function(fname){
      write.csv(thedata(), fname)
    }
  )
  #----------------------------------------------------------------------------
}
