# packages
library(dplyr)
library(rsample)
library(data.table)
library(matlib)



# Mini-batch IRLS
#
# Fits logistic regression using mini-batch IRLS.
#
# Inputs:
# X           - feature matrix [1's + features]
# y           - target vector
# batch_size  - number of observations per batch
# max_epochs  - maximum number of epochs
# alpha       - damping parameter
# tol         - convergence tolerance
# beta_init   - initial beta value, takes in numeric and convert into vector 
# verbose     - prints loss/error result at each epoch iteration and/if convergence
#
# Returns: best beta vector



lr_irls <- function(X, y, batch_size, max_epochs, alpha, tol, beta_init, verbose = FALSE){
  
  # define sigmoid, log loss functions
  # sigmoid function
  sigmoid <- function(x) {
    1 / (1 + exp(-x))
  }
  
  # loss function
  log_loss <- function(y, prob) {
    loss <- - sum( y * log(prob) + (1 - y) * log(1 - prob))
    return(loss)
  }
  
  # initialize minibatch beta
  beta_mb <- rep(beta_init, ncol(X))
  
  # bookkeeping
  mb_loss_history <- numeric(max_epochs)
  best_loss <- Inf 
  best_beta <- beta_mb
  
  # epoch loop
  for (epoch in 1:max_epochs){
    
    ini_ep_beta <- beta_mb # initial epoch beta (final batch beta)
    
    # create shuffle index to align feature matrix and actual y
    shuffle_index <- sample(seq_len(nrow(X))) # randomize per epoch
    
    # shuffle X and y using index
    X_shuffle <- X[shuffle_index, , drop = FALSE] # make matrix
    y_shuffle <- y[shuffle_index] # vector
    
    # define where batch starts
    batch_starts <- seq(
      1, # starts`  `
      nrow(X), # total observations
      by = batch_size # per 1000
    )
    
    # minibatch loop
    for (start in batch_starts){
      
      # define where where mini batch ends
      end <- min(
        start + batch_size - 1,
        nrow(X) # caps the end at the dataset
      )
      
      # extract the batch
      X_batch <- X_shuffle[start:end, , drop = FALSE]
      y_batch <- y_shuffle[start:end]
      
      
      # newton's/irls method for each batch
      # make predictions
      pred <- X_batch %*% beta_mb # use X batch and minibatch beta
      prob <- sigmoid(pred)
      
      # gradient
      grad <- t(X_batch) %*% (prob - y_batch)
      
      # hessian matrix 
      diag <- as.vector(prob * (1 - prob)) # define diagonal
      H <- t(X_batch) %*% (X_batch * diag) # hessian 
      
      # update beta 
      dir <- solve(H, grad)
      beta_mb <- beta_mb - alpha * dir # update same beta
    }
    
    # predict using recent beta of each epoch
    pred_epoch <- X %*% beta_mb # original feature matrix X
    prob_epoch <- sigmoid(pred_epoch)
    
    # compute epoch loss
    loss <- log_loss(y, prob_epoch) # original y vector
    mb_loss_history[epoch] <- loss
    
    # compute error in beta over one epoch
    epoch_error <- sqrt(
      sum((beta_mb - ini_ep_beta)^2) # final epoch beta vs initial epoch beta
    )
    
    # print progress for each epoch
    if (verbose) {cat(
      "Epoch:", epoch,
      "| Loss:", loss,
      "| Error:", epoch_error,
      "\n"
    )
    }
    
    # keep best beta
    if (loss < best_loss) {
      best_loss <- loss
      best_beta <- beta_mb
    }
    
     # check convergence
     if (verbose) {if (epoch_error < tol) {
      cat("Converged at epoch", epoch, "\n")
      break
        
      }
     }
  }
  
  return(best_beta)
}





