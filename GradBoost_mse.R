
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




# gradient tree boosting with absolute value

# step 1: initialize f0 -> first model
f0 <- rep(mean(train$y), nrow(train))




  



# test grad and tree for 2c
gradtest <- train$y - f0

# step 2b
# fit regression tree for grad
fit.tree = rpart(gradtest ~ x1 + x2, 
                 data = train, 
                 weights = NULL, # initial weight # no weights added yet
                 method = "anova", # regression/continuous response
                 cp= 0.01
)







# work in progress


  
# step 2: M loop
M = 100 # set it to 100 for now, will change later

for (m in 1:M){
  # step 2a
  # compute the gradient
  grad <- fm - f0
  
  # step 2b
  # fit regression tree for grad
  fit.tree = rpart(grad ~ x1,  x2, 
                   data = train, 
                   weights = NULL, # initial weight # no weights added yet
                   method = "anova", # regression/continuous response
                   cp= 0.01
  )
  
  # step 2c: loop over the number of nodes
  
  
  # step 2d
  # now we update the additive 
  
}
















