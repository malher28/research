# clear memory
rm(list=ls())

#install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("rsample")
install.packages("data.table")
install.packages("reshape2")
install.packages("pROC")
install.packages("ranger")
install.packages("caret")

# packages
library(dplyr)
library(ggplot2)
library(ranger)
library(rsample)
library(pROC)
library(caret)

set.seed(42)

# load train, test dataframes from equity bootstrapping
equity_train <- read.csv("/home/rstudio/Research/boot_train.csv")
val <- read.csv("/home/rstudio/Research/val.csv") # not sure if i need this yet
test <- read.csv("/home/rstudio/Research/test.csv")


# change target variable to work with caret
equity_train$target <- factor(
  equity_train$target,
  levels = c(0, 1),
  labels = c("negative", "positive")
)

# also test if going with caret
test$target <- factor(
  test$target,
  levels = c(1, 0),
  labels = c("positive", "negative")
)


# tuning hyperparamters using kfold cv, then train model

# making small grid to test run kfold cv
# define hyperparameter grid
tuning_grid <- expand.grid(
  mtry = 2,                   # number of variables to sample
  splitrule = "gini",        # splitting rule
  min.node.size = c(1, 5)        # minimum node size
)



# define cross-validation settings
train_control <- trainControl(
  method = "cv",       # k-fold cv
  number = 3, # number of folds
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  search = "grid"      # use grid search for hyperparameter tuning
)


# train rf model
rf_model <- train(
  target ~ .,    # formula
  data = equity_train,   # training data
  method = "ranger",
  trControl = train_control,
  tuneGrid = tuning_grid,
  metric = "ROC",    # metric to optimize, AUC
  num.trees = 500,
  sample.fraction = 1,
  replace = TRUE,
  importance = "none"
)

# results
print(rf_model)
plot(rf_model)
best_hyperparameters <- rf_model$bestTune
print(best_hyperparameters)
# ITS NOT WORKING YET IDK!!!!!
# maybe too much computation?







# Helper to compute confusion metrics at a threshold chosen to give a target specificity
metrics_at_spec = function(fit, data, target_spec = 0.90) {
  probs = predict(fit, data=data)$predictions
  actual = as.numeric(as.character(data$target))  # 0/1 numeric
  
  # get predicted probabilities for negatives only
  probs_neg = probs[actual == 0]
  
  # threshold that yields the desired specificity:
  # specificity = proportion of negatives with prob <= thresh
  thresh = as.numeric(quantile(probs_neg, probs = target_spec, type = 8))
  
  pred = ifelse(probs > thresh, 1, 0)
  TP = sum(pred == 1 & actual == 1)
  TN = sum(pred == 0 & actual == 0)
  FP = sum(pred == 1 & actual == 0)
  FN = sum(pred == 0 & actual == 1)
  
  specificity = TN / (TN + FP)
  sensitivity = TP / (TP + FN)
  accuracy = (TP + TN) / length(actual)
  
  # compute AUC
  model_auc <- auc(actual, pred)
  
  list(threshold = thresh,
       confusion = matrix(c(TN, FP, FN, TP), nrow = 2,
                          dimnames = list(Pred0_1 = c("Pred0","Pred1"),
                                          Actual0_1 = c("Act0","Act1"))),
       specificity = specificity,
       sensitivity = sensitivity,
       accuracy = accuracy,
       auc = model_auc)
}

# print all the metrics at a target specificity
target_spec = 0.8
cat("\nEquity Random Forest Training Set\n")
print(metrics_at_spec(equity_rf_fit, equity_train, target_spec))

cat("\nEquity Random Forest Test Set\n")
print(metrics_at_spec(equity_rf_fit, test, target_spec))



