

library(MLmetrics)
library(e1071)
library(caret)

folds <- createFolds(finaldata$Score, k=10) #split the data set into folds

# regression tree with cross validation------
rtresults <- lapply(folds, function(x) {
  train <- finaldata[-x, ]
  test <- finaldata[x, ]
  model <- rpart(Score ~ Access1 + Comp1 + Comp2 + Vali1 + Vali2 + Inte1 + Inte2 + Accur1 + Accur2, data = train)
  pred <- predict(model, test)
  actual <- test$Score
  mae <- MAE(pred, actual)
  mse <- MSE(pred, actual)
  R2 <- R2_Score(pred, actual)
  return(list(mse))
})

# linear regression with cross validation
lmresults <- lapply(folds, function(x) {
  train <- finaldata[-x, ]
  test <- finaldata[x, ]
  model <- lm(Score ~ Access1 + Comp1 + Comp2 + Vali1 + Vali2 + Inte1 + Inte2 + Accur1 + Accur2, data = train)
  modelAIC <- step(model)
  pred <- predict(modelAIC, test)
  actual <- test$Score
  mae <- MAE(pred, actual)
  mse <- MSE(pred, actual)
  R2 <- R2_Score(pred, actual)
  return(list(mse))
})

# random forest with cross validation
rfresults <- lapply(folds, function(x) {
  train <- finaldata[-x, ]
  test <- finaldata[x, ]
  model <- randomForest(Score ~  Access1 + Comp1 + Comp2 + Vali1 + Vali2 + Inte1 + Inte2 + Accur1 + Accur2, data = train, 
                        importance = TRUE, na.action = na.omit)
  pred <- predict(modelAIC, test)
  actual <- test$Score
  mae <- MAE(pred, actual)
  mse <- MSE(pred, actual)
  R2 <- R2_Score(pred, actual)
  return(list(mae))
})