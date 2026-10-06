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

# define relation
y <- ifelse(x1 + x2 + epsilon > 0, 1, -1)

# make dataframe
data <- data.frame(x1, x2, y)

# train, test set
train <- data[sample(nrow(data), n_train), ]
test <- data[sample(nrow(data), n_test), ]



# ada boost algorithm

# indicator function
ind_func = function(
    pred, true){ # take in vector argument
  
    ind_vec = rep(0, length(pred)) #define indicator vector 
    
    for (i in seq_along(pred)){
      if (pred[i] != true[i]){
        
        ind_vec[i] = 1
      }
    }
    return(ind_vec)
}


# number of iterations
max_its <-  100

# initialize observation weights 
w <-  rep(1/n_train, n_train)

# making containers for model and model weights
trees <- vector("list", max_its)
alphas <- numeric(max_its)

for (m in 1:max_its){
  
  # fit base tree with training set
  fit.tree = rpart(y ~ ., 
                   data = train, 
                   weights = w, # initial weight
                   method = "class", 
                   cp= 0.01
                   )
  # make prediction on test set
  pred <- predict(fit.tree, newdata = train, type = "class")
  
  # conver to numeric
  pred_num <- as.numeric(as.character(pred))
  
  # make indictor vector 
  ind_vec <- ind_func(pred, train$y)
  
  # compute error
  err = sum(w * ind_vec) / sum(w)
  
  # break if no more learning
  if (err == 0.5){
    break
  }

  # compute alpha, model weight 
  alpha = log( (1 - err) / err)
  
  # save tree models and model weights
  trees[[m]] <- fit.tree
  alphas[m] <- alpha
  
  # adjust and reset observation weight vector
  for (i in seq_along(w)){
    w[i] <- (w[i] * exp(alpha * ind_vec[i]))
  }
  
  # scale observation weights
  w <- w / sum(w)
  
  # debugging
  cat(
    "iteration:", m,
    "error =", err,
    "alpha =", alpha,
    "\n"
  )
  
  # trim containers to match the number of iterations
  trees <- trees[!sapply(trees, is.null)]
  alphas <- alphas[seq_along(trees)]
}


# final G
final_model <- function(design_matrix, alphas, trees){
  
  # make empty list for predictions
  pred_list = vector("list", length(alphas))
  
  # make predictions using alphas and trees from #2
  for (i in seq_along(alphas)){
    pred_list[[i]] = alphas[i] * as.numeric(as.character(predict(trees[[i]], newdata = design_matrix, type = "class")))
  }
  
  
  # return the sign of element-wise 
  return (sign(Reduce("+", pred_list)))
}

# boosted results
pred_boost <- final_model(test[c(1,2)], alphas, trees)








# single tree results for baseline
one_tree = rpart(y ~ ., 
                 data = train, 
                 weights = rep(1, n_train), # default weight = 1
                 method = "class", 
                 cp= 0.01
)

pred_single <-  predict(one_tree, newdata = test, type = "class")



# results

# boosted
table(
  Actual = test$y,
  Predicted = pred_boost
)

#single tree
table(
  Actual = test$y,
  Predicted = pred_single
)


