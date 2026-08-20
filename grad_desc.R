# clear all memory
rm(list = ls())

# packages
library(dplyr)
library(rsample)
library(data.table)
library(ranger)

# set seed
set.seed(42)

# load in data
train <- read.csv("/home/rstudio/Research/train.csv") # original training, not EBS


# define feature matrix, coeff vector, actual vector
features = train[, 1:10] # extract features from dataframe, first 10 columns

X <- cbind(1, as.matrix(features)) # make feature matrix (1s + features)

beta <- rep(0, ncol(X)) # initialize and guess coeff vector

y = train$target # actual vector



# define functions

# sigmoid function
sigmoid <- function(x) {
  1 / (1 + exp(-x))
  }


# loss function
log_loss <- function(y, prob) {
  loss <- - sum( y * log(prob) + (1 - y) * log(1 - prob))
  
  return(loss)
  }





# iterated gradient descent
max_its <- 100
tol <- 1e-6
alpha <- 0.001

# bookkeeping
loss_history <- numeric(max_its)

for (i in 1:max_its) {
  
  # prediction
  pred <- X %*% beta
  prob <- sigmoid(pred)
  
  # compute loss
  loss <- log_loss(y, prob)
  loss_history[i] <- loss
  
  # gradient
  grad <- t(X) %*% (prob - y) / nrow(X)
  
  # update
  beta_new <- beta - alpha * grad
  
  # compute error
  error <- sqrt(sum((beta_new - beta)^2))
  
  # print every 10th iteration
  if (i %% 10 == 0) {
    cat(
      "Iteration:", i,
      "| Loss:", loss,
      "| Error:", error, "\n"
    )
  }
  
  # check convergence to break
  if (error < tol) {
    beta <- beta_new
    cat("Converged at iteration", i, "\n")
    break
  }
  
  # keep updating if no convergence
  beta <- beta_new
}










