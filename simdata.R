#install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("rsample")
install.packages("data.table")
install.packages("reshape2")
install.packages("pROC")

# packages
library(dplyr)
library(ggplot2)
library(rsample)
library(data.table)
library(reshape2)
library(pROC)

# set seed
set.seed(42)

# number of observations
n = 100000

# predictors
# the planets -> {0, 1, 2}, also imbalanced
planets <- sample(
  0:2,
  size = n,
  replace = TRUE,
  prob = c(0.60, 0.30, 0.10)
)

# age [18,90]
age <- round(pmin(pmax(rnorm(n, 50, 10), 18), 90))

# remaining 8 predictors, start with Unif[0,1] random samples
Xrand = matrix(runif(8*n), nrow = n, ncol = 8)

# now apply a random linear transformation to each column
for (j in c(1:8))
{
  b1 = (1 + rnorm(n=1)**2)**2
  b0 = rnorm(n=1, sd=5)
  Xrand[,j] = b1 * Xrand[,j] + b0
}
Xmat = cbind(1, planets, age, Xrand)

# create random logistic regression coefficients
beta_true = rnorm(ncol(Xmat), mean = -0.02, sd = 0.2)

# these are the logits or the log odds
logit_true = Xmat %*% beta_true

# this transformation converts logits to probabilities
p_true = 1/(1 + exp(-logit_true))
            
# make target variable using defined relation
y = rbinom(n, 1, p_true)

# make training dataframe
train = data.frame(planets = planets,
                   age = age,
                   Xrand,
                   target = factor(y, levels = c(0,1)))

# save csv
write.csv(train, "/home/rstudio/Research/simdata.csv", row.names = FALSE)






