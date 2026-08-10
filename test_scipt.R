# packages
library(dplyr)
library(rsample)
library(data.table)
library(ranger)

# set seed
set.seed(42)

# load in function
source("/home/rstudio/Research/ebs_research/equity_bs.R")
source("/home/rstudio/Research/ebs_research/ebs_cv.R")

# load in data
train <- read.csv("/home/rstudio/Research/train.csv") # original training, not EBS

ebs_train <- equity_bs(
  data = train,
  group_var = "planets",
  target_var = "target",
  bucket_size = 15000
)

results <- ebs_cv(
  data = train, # using original since ebs is embedd
  num_fold = 5,
  target_spec = 0.8, # threshold by spec
  group_var = "planets", # ebs argument
  target_var = "target",
  bucket_size = 15000
)

# check ebs_cv
results$fold_auc
results$fold_spec
results$fold_sens

results$mean_auc
results$mean_spec
results$mean_sens
