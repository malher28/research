# clear all memory
rm(list = ls())

# packages
library(dplyr)
library(ggplot2)
library(ranger)
library(rsample)
library(data.table)
library(reshape2)
library(pROC)

# training data
train <- read.csv("/home/rstudio/Research/train.csv") # original training, not EBS

# set seed
set.seed(42)


# define rf parameters
p <- ncol(training) - 1
numtrees_val <- 100
mtry_val <- 10
min_node <-  10



# ebs-embedded fkold cross validation

# make list to store folds
folds <- vfold_cv(train, # original training
                  v = 5, # number of folds
                  strata = target # preserves class proportion
                  )

# make list to store fold auc
fold_auc <- numeric(length = nrow(folds))
fold_acc  <- numeric(length = nrow(folds))
fold_spec <- numeric(length = nrow(folds))
fold_sens <- numeric(length = nrow(folds))
fold_f1   <- numeric(length = nrow(folds))


# implementing ebs into the kfold cv
for (i in seq_len(nrow(folds))) {
  
  # first fold
  split_i <- folds$splits[[i]]
  
  # save the two splits, one for training and the other for validation
  fold_train <- analysis(split_i) # training set
  fold_valid <- assessment(split_i) # validating set
  
  
  
  
  # ebs algorithm on training set

  # make groups -> A1, A0, B1, B0, C1, C0
  groups <- split(fold_train, # only training set
                  list(fold_train$planet, fold_train$target)) # make 6 lists by planet and class
  
  # make list
  boot_groups <- {} 
  
  # updated bootstrap -> if bucket < N, sample replace otherwise, no replacement
  N = 15000 # biggest bucket size
  
  for (name in names(groups)){
    if (nrow(groups[[name]]) < N) {
      boot_groups[[name]] <- groups[[name]] %>% 
        slice_sample(
          n = N,      # number of obs in each group
          replace = TRUE # sample with replacement
        )
    }
    else {
      boot_groups[[name]] <- groups[[name]] %>% 
        slice_sample( 
          n = N,      # number of obs in each group
          replace = FALSE # sample without replacement
        )
    }
  }
  
  # combine lists to make big dataframe
  comb_train <- bind_rows(boot_groups)
  
  # shuffle dataframe for final training set
  ebs_train <- comb_train[sample(1:nrow(comb_train)), ]
  
  
  
  # define model with chosen parameters and train on ebs data
  rf_fit <- ranger(target ~ .,  
                   data = ebs_train,  # ebs training
                   num.trees = numtrees_val,
                   mtry = mtry_val,
                   min.node.size = min_node,
                   splitrule = "gini",
                   probability = TRUE
  )
  
  # make prediction
  probs <- predict(
    rf_fit,  # rf model trained on ebs data
    data = fold_valid, # predict with validation fold
  )$predictions[, 2] 
  
  
  actual = as.numeric(as.character(fold_valid$target))  # 0/1 numeric
  # get predicted probabilities for negatives only
  probs_neg = probs[actual == 0]
  
  # threshold that yields the desired specificity:
  # specificity = proportion of negatives with prob <= thresh
  thresh = as.numeric(quantile(probs_neg, probs = .7, type = 8)) # .7 for threshold placeholder right now
  
  # confusion matrix
  pred = ifelse(probs > thresh, 1, 0)
  TP = sum(pred == 1 & actual == 1)
  TN = sum(pred == 0 & actual == 0)
  FP = sum(pred == 1 & actual == 0)
  FN = sum(pred == 0 & actual == 1)
  
  # compute eval metrics
  fold_spec[i] = TN / (TN + FP)
  fold_sens[i] = TP / (TP + FN)
  fold_acc[i] = (TP + TN) / length(actual)
  
  # f1 score
  fold_f1[i] <- (2 * TP) / (2 * TP + FP + FN)
  
  
  # save auc
  fold_auc[i] <- as.numeric(
    auc(
      response = fold_valid$target,
      predictor = probs
    )
  )  
  
}

# take mean of all metrics
mean_auc <- mean(fold_auc)
mean_spec <- mean(fold_spec)
mean_sens <- mean(fold_sens)
mean_acc <- mean(fold_acc)
mean_f1 <- mean(fold_f1)



# verify outputs
for (i in seq_len(nrow(folds))) {
  fold_valid <- assessment(folds$splits[[i]])
  print(table(fold_valid$target))
}

print(mean_auc)
print(mean_acc)
print(mean_spec)
print(mean_f1)
print(mean_sens)







