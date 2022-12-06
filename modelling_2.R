library(caret)

train_df <- train_df %>%
  select(c(-gender,-cp_type,-fbs_type,-restecg_type,-thal_type,-exang_type,-target_type))
train_df <- train_df %>% drop_na()
test_df <- test_df %>% drop_na()
#-------------------------------------------------------------------------
#Logistic Regression

fit_lg <- train(target ~ .,
                    data = train_df,
                    method = "glm",
                    preProcess = c("center", "scale"),
                    trControl = trainControl(method = "cv", number = 10),
                    family="binomial")
#Train Data
summary(fit_lg)
train_pred_lg <- predict(fit_lg,newdata = select(train_df,-target))
train_accuracy <- paste0(as.character(round((sum(train_pred_lg == train_df$target)/nrow(train_df))*100,2)),"%")
train_accuracy

#Testing Data
test_pred_lg <- predict(fit_lg,newdata = select(test_df,-target))
postResample(test_pred_lg,obs = test_df$target)
test_accuracy <- paste0(as.character(round((sum(test_pred_lg == test_df$target)/nrow(test_df))*100,2)),"%")
test_accuracy
confusionMatrix(data = test_pred_lg, reference = test_df$target)
#-------------------------------------------------------------------------