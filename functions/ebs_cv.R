# packages
library(dplyr)
library(rsample)
library(data.table)
library(pROC)

# set seed
set.seed(42)

# ebs embedded kfold cv function
# note: default ranger rf for now
ebs_cv <- function(
    data,
    num_fold,
    target_spec = .8, # threshold by spec default
    group_var, # ebs argument
    target_var,
    bucket_size = NULL){
  
  # make list to store folds
  folds <- vfold_cv(data, # original training
                    v = num_fold, # number of folds
                    strata = target_var # preserves class proportion
                    )
  
  # make list to store metrics
  fold_auc <- numeric(length = nrow(folds))
  #fold_acc  <- numeric(length = nrow(folds))
  fold_spec <- numeric(length = nrow(folds))
  fold_sens <- numeric(length = nrow(folds))
  #fold_f1   <- numeric(length = nrow(folds))
  
  
  # implementing ebs into the kfold cv
  for (i in seq_len(nrow(folds))) {
    
    # first fold
    split_i <- folds$splits[[i]]
    
    # save the two splits, one for training and the other for validation
    fold_train <- analysis(split_i) # training set
    fold_valid <- assessment(split_i) # validating set
    
    
    # ebs each fold
    ebs_train <- equity_bs(data = fold_train,
                           group_var = group_var,
                           target_var = target_var,
                           bucket_size = bucket_size)
    
    # fit rf eith ebs data
    model_formula <- as.formula(
      paste(target_var, "~ .")
    )
    
    rf_fit <- ranger(
      model_formula,
      data = ebs_train,
      probability = TRUE
    )
    
    metrics <- metrics_at_spec(
      fit = rf_fit,
      data = fold_valid,
      target_var = target_var,
      target_spec = target_spec
    )
    
    # compute eval metrics
    fold_spec[i] = metrics$spec
    fold_sens[i] = metrics$sens
    #fold_acc[i] = metrics$acc
    # f1 score
    #fold_f1[i] <- metrics$f1
    
    
    # save auc
    fold_auc[i] <- metrics$auc
  }
  
  # take mean of metrics across k folds
  # take mean of all metrics
  mean_auc <- mean(fold_auc)
  mean_spec <- mean(fold_spec)
  mean_sens <- mean(fold_sens)
  #mean_acc <- mean(fold_acc)
  #mean_f1 <- mean(fold_f1)
  
  return(
    list(
      fold_auc = fold_auc,
      fold_spec = fold_spec,
      fold_sens = fold_sens,
      #fold_acc = fold_acc,
      #fold_f1 = fold_foldf1,
      mean_auc = mean_auc,
      mean_spec = mean_spec,
      mean_sens = mean_sens
      #mean_acc = mean_acc,
      #mean_f1 = mean_f1
    )
  )
  
}
  
  
  