library(caret)
library(randomForest)

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
train_accuracy_lg <- paste0(as.character(round((sum(train_pred_lg == train_df$target)/nrow(train_df))*100,2)),"%")
train_accuracy_lg

#Testing Data
test_pred_lg <- predict(fit_lg,newdata = select(test_df,-target))
postResample(test_pred_lg,obs = test_df$target)
test_accuracy_lg <- paste0(as.character(round((sum(test_pred_lg == test_df$target)/nrow(test_df))*100,2)),"%")
test_accuracy_lg
confusionMatrix(data = test_pred_lg, reference = test_df$target)
#-------------------------------------------------------------------------
#Classification Tree
input_max_depth = 6 #2 to 10
fit_tree = train(target ~ ., 
                  data=select(train_df,-ca), 
                  method="rpart2",
                  preProcess = c("center", "scale"),
                  trControl = trainControl(method = "cv",number = 10),
                  tuneGrid = data.frame(maxdepth = input_max_depth))

#Train Data
fit_tree
#plot(fit_tree)
train_pred_tree <- predict(fit_tree,newdata = select(train_df,-target))
train_accuracy_tree <- paste0(as.character(round((sum(train_pred_tree == train_df$target)/nrow(train_df))*100,2)),"%")
train_accuracy_tree

#Testing Data
test_pred_tree <- predict(fit_tree,newdata = select(test_df,-target))
postResample(test_pred_tree,obs = test_df$target)
test_accuracy_tree <- paste0(as.character(round((sum(test_pred_tree == test_df$target)/nrow(test_df))*100,2)),"%")
test_accuracy_tree
confusionMatrix(data = test_pred_tree, reference = test_df$target)
#-------------------------------------------------------------------------
# Random Forest
input_mtry = 5 #2 to 10
fit_rf = train(target ~ ., 
                 data=train_df, 
                 method="rf",
                 preProcess = c("center", "scale"),
                 trControl = trainControl(method = "cv",number = 5),
                 tuneGrid = data.frame(mtry = input_mtry))

#Train Data
fit_rf
#plot(fit_rf)
train_pred_rf <- predict(fit_rf,newdata = select(train_df,-target))
train_accuracy_rf <- paste0(as.character(round((sum(train_pred_rf == train_df$target)/nrow(train_df))*100,2)),"%")
train_accuracy_rf

#Testing Data
test_pred_rf <- predict(fit_rf,newdata = select(test_df,-target))
postResample(test_pred_rf,obs = test_df$target)
test_accuracy_rf <- paste0(as.character(round((sum(test_pred_rf == test_df$target)/nrow(test_df))*100,2)),"%")
test_accuracy_rf
confusionMatrix(data = test_pred_rf, reference = test_df$target)
#-------------------------------------------------------------------------
rfFit <- randomForest(target ~ ., data = train_df, mtry = ncol(train_df)/3,
                     ntree = 200, importance = TRUE)

