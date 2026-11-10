
# clear memory
rm(list=ls())

library(rsample)
library(data.table)
library(reshape2)

# For decision tree model
library(rpart)
# For data visualization
library(rpart.plot)

# set seed
set.seed(42)






# make self-contained data

# number of observation
n <- 20000

#80/20 split
n_train <- 16000
n_test <- 4000

# define predictors
x1 <- rnorm(n)
x2 <- rnorm(n)

# noise
epsilon <- rnorm(n, mean = 0, sd = 0)

# regression output
# define relation
y <- x1 + x2 + epsilon

# make dataframe
data <- data.frame(x1, x2, y)

# train, test set
train <- data[sample(nrow(data), n_train), ]
test <- data[sample(nrow(data), n_test), ]



# indicator function for observation node assignment
ind_func <- function(node_assign, region) {
  ind_vec <- rep(0, length(node_assign))
  for (i in seq_along(node_assign)) {
    if (node_assign[i] == region) {
      ind_vec[i] <- 1
    }
  }
  return(ind_vec)
}



# gradient tree boosting with mean squared error

# step 1: initialize f0 -> first model
f0 <- mean(train$y)
f_current <- rep(f0, nrow(train))


  
# step 2: M loop and book keeping
M = 20 # set it to 100 for now, will change later
mse_history <- numeric(M)


for (m in 1:M){
  # step 2a
  # compute the gradient
  grad <- train$y - f_current
  
  # step 2b
  # fit regression tree for grad
  fit.tree = rpart(grad ~ x1 +  x2, 
                   data = train, 
                   weights = NULL, # initial weight # no weights added yet
                   method = "anova", # regression/continuous response
                   cp= 0.01
  )
  
  # identify terminal nodes for current tree
  leaf_id <- which(fit.tree$frame$var == "<leaf>")
  J <- length(leaf_id)
  
  # initialize gamma for current tree
  gamma <- numeric(J)
  
  # step 2c: loop over the number of nodes
  for (j in 1:J) {
    
    # Group residuals belonging to terminal node j
    leaf_grad <- grad[fit.tree$where == leaf_id[j]]
    
    # Number of observations in that node
    njm <- length(leaf_grad)
    
    # Sum residuals and divide by njm
    gamma[j] <- sum(leaf_grad) / njm
    
    
    
    # step 2d since we are also looping over j
    ind_vec <- ind_func(fit.tree$where, leaf_id[j]) # indicator vector for region j
    
    # update model for observations
    f_current <- f_current + gamma[j] * ind_vec
  }
  # store current mse
  mse_history[m] <- mean((train$y - f_current)^2)
}

# step 3: output the final model
f_final <- f_current


# plot training mse against boosting iterations
plot(
  1:M, mse_history,
  type = "l",
  lwd = 2.5,
  xlab = "boosting iteration",
  ylab = "training mse",
  main = "gradient boosting: training mse"
)








