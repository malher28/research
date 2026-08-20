# packages
library(dplyr)
library(rsample)
library(data.table)

# set seed
set.seed(42)

# Helper to compute confusion metrics at a threshold chosen to give a target specificity
metrics_at_spec = function(fit, data, target_var, target_spec = 0.80) {
  
  # make prediction
  probs <- predict(
    fit,
    data = data
  )$predictions[, 2] 
  
  # make confusion matrix for eval metrics
  
  actual <- as.numeric(as.character(data[[target_var]]))  # 0/1 numeric
  # get predicted probabilities for negatives only
  probs_neg = probs[actual == 0]
  
  # threshold that yields the desired specificity:
  # specificity = proportion of negatives with prob <= thresh
  thresh = as.numeric(quantile(probs_neg, probs = target_spec, type = 8))
  
  # confusion matrix
  pred = ifelse(probs > thresh, 1, 0)
  TP = sum(pred == 1 & actual == 1)
  TN = sum(pred == 0 & actual == 0)
  FP = sum(pred == 1 & actual == 0)
  FN = sum(pred == 0 & actual == 1)
  
  # compute eval metrics
  spec = TN / (TN + FP)
  sens = TP / (TP + FN)
  acc = (TP + TN) / length(actual)
  # f1 score
  f1 <- (2 * TP) / (2 * TP + FP + FN)
  
  
  # save auc
  model_auc <- as.numeric(
    auc(
      response = data[[target_var]],
      predictor = probs
    )
  )
  
  return(
    list(
      auc = model_auc,
      spec = spec,
      sens = sens,
      acc = acc,
      f1 = f1,
      threshold = thresh
    )
  )
}
