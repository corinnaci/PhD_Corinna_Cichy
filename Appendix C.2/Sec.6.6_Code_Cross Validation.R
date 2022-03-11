# This is code to replicate the model evaluation using 10-fold cross validation
# and refers to Section 6.6

#load required packages

library(MLmetrics)
library(e1071)
library(caret)
library(rpart)
library(randomForest)
library(sets)

# read input data
DQ_data <- read_excel(Data_C.2.xlsx)

#split the data set into folds for cross validation
set.seed(222)
folds <- createFolds(DQ_data$Quality, k=10) 

# regression tree with cross validation
# regression tree is fitted for each set of training data and statistical measures
# mse, mae and R2 are computed on each set of test data
rtresults <- lapply(folds, function(x) {
  train <- DQ_data[-x, ]
  test <- DQ_data[x, ]
  model <- rpart(Quality ~ Access1 + Comp1 + Comp2 + Vali1 + Vali2 + Inte1 + Inte2 + Accur1 + Accur2, data = train)
  pred <- predict(model, test)
  actual <- test$Quality
  mae <- MAE(pred, actual)
  mse <- MSE(pred, actual)
  R2 <- R2_Score(pred, actual)
  return(list(mse)) # similarly, use return(list(mae)) and return(list(R2))
})

mean(unlist(rtresults)) # to obtain mean values

# linear regression with cross validation
# linear regression model is fitted for each set of training data and statistical measures
# mse, mae and R2 are computed on each set of test data
lmresults <- lapply(folds, function(x) {
  train <- DQ_data[-x, ]
  test <- DQ_data[x, ]
  model <- lm(Quality ~ Access1 + Comp1 + Comp2 + Vali1 + Vali2 + Inte1 + Inte2 + Accur1 + Accur2, data = train)
  modelAIC <- step(model)
  pred <- predict(modelAIC, test)
  actual <- test$Quality
  mae <- MAE(pred, actual)
  mse <- MSE(pred, actual)
  R2 <- R2_Score(pred, actual)
  return(list(mse)) # similarly, use return(list(mae)) and return(list(R2))
})

mean(unlist(lmresults)) # to obtain mean values

# random forest with cross validation
# random forest is fitted for each set of training data and statistical measures
# mse, mae and R2 are computed on each set of test data
rfresults <- lapply(folds, function(x) {
  train <- DQ_data[-x, ]
  test <- DQ_data[x, ]
  model <- randomForest(Quality ~  Access1 + Comp1 + Comp2 + Vali1 + Vali2 + Inte1 + Inte2 + Accur1 + Accur2, data = train, 
                        importance = TRUE, na.action = na.omit)
  pred <- predict(model, test)
  actual <- test$Quality
  mae <- MAE(pred, actual)
  mse <- MSE(pred, actual)
  R2 <- R2_Score(pred, actual)
  return(list(R2)) # similarly, use return(list(mae)) and return(list(R2))
})

mean(unlist(rfresults)) # to obtain mean values

# proposed approximation by fuzzy rules
train <- DQ_data[-folds$Fold01, ] # repeat for fold02 - fold10
test <- DQ_data[folds$Fold01, ] # repeat for fold02 - fold10

# apply basis functions to training data
attach(train)
gf1 <- sapply(round(Inte1, 2), g1) 
gf2 <- sapply(round(Inte2, 2), g2)
gf3 <- sapply(round(Vali1, 2), g3)
gf4 <- sapply(round(Comp1, 2), g4)
gf5 <- sapply(round(Comp2, 2), g5)
gf6 <- sapply(round(Vali2, 2), g6)
gf7 <- sapply(round(Accur2, 2), g7)
gf8 <- sapply(round(Access1, 2), g8)

# least squares estimation using fuzzy basis for training data
lmDQ <- lm(Quality ~ gf1 + gf2 + gf3 + gf4 +gf5 + gf6 + gf7+ gf8)
lmDQ_AIC <- step(lmDQ)

# apply basis functions to test data
attach(test)
newgf1 <- sapply(round(Inte1, 2), g1)
newgf2 <- sapply(round(Inte2, 2), g2)
newgf3 <- sapply(round(Vali1, 2), g3)
newgf4 <- sapply(round(Comp1, 2), g4)
newgf5 <- sapply(round(Comp2, 2), g5)
newgf6 <- sapply(round(Vali2, 2), g6)
newgf7 <- sapply(round(Accur2, 2), g7)
newgf8 <- sapply(round(Access1, 2), g8)

# evaluate predictions
newdata <- data.frame(gf1 = newgf1, gf2 = newgf2, gf3 = newgf3, gf4 = newgf4, 
                gf5 = newgf5, gf6 = newgf6, gf7=newgf7, gf8=newgf8, data = test)
fuzzypred <- predict(lmDQ_AIC, newdata) # make predictions
actual <- test$Quality
#compute comparative metrics
MAE(fuzzypred, actual) # compute mean absolute error
MSE(fuzzypred, actual) # compute mean squared error
R2_Score(fuzzypred, actual) # compute R^2

# repeat for each fold and compute mean values of comparative metrics
