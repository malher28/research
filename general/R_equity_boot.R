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

# set seed
set.seed(42)

# load data
simdata <- read.csv("/home/rstudio/Research/simdata.csv")

split = initial_validation_split(
  simdata,
  prop = c(0.6, 0.2), # 60% train, 20% val, 20% test
  strata = target
)

train = training(split)
val   = validation(split)
test  = testing(split)

# make groups -> A1, A0, B1, B0, C1, C0
groups <- split(train, # only training set
                list(train$planet, train$target)) # make 6 lists by planet and class


# equity bootstrapping
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


# check groups and boot_groups
print("Original Groups")
for (name in names(groups)){
  cat("\n", name, "\n")
  print(table(groups[[name]]$target))
} 

print("Bootstrapped Groups")
for (name in names(boot_groups)){
  cat("\n", name, "\n")
  print(table(boot_groups[[name]]$target))
}  
# looks good


# combine lists to make big dataframe
comb_train <- bind_rows(boot_groups)

# shuffle dataframe for final training set
boot_train <- comb_train[sample(1:nrow(comb_train)), ]

# check
print(head(boot_train))
# yay it worked!


# save csv files for modeling
write.csv(train, "/home/rstudio/Research/train.csv", row.names = FALSE) # original training
write.csv(boot_train, "/home/rstudio/Research/boot_train.csv", row.names = FALSE) # boot training
write.csv(val, "/home/rstudio/Research/val.csv", row.names = FALSE) 
write.csv(test, "/home/rstudio/Research/test.csv", row.names = FALSE)







