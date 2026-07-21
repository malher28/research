# packages
library(dplyr)
library(ggplot2)
library(rsample)
library(data.table)

# set seed
set.seed(42)

# load data
sim_data <- read.csv("/home/rstudio/Research/simdata.csv")
head(sim_data)

# train/test split
split <- initial_split(
  sim_data,
  prop = 0.8, #80/20 -> train/test
  strata = target
)

# assign split
train <- training(split)
test  <- testing(split)


# make groups -> A1, A0, B1, B0, C1, C0
groups <- split(train, # only training set
                list(train$planet, train$target)) # make 6 lists by planet and class


# equity bootstrapping
# make list
boot_groups <- {} 

# loop to bootstrap
for (name in names(groups)){
  boot_groups[[name]] <- groups[[name]] %>% 
    slice_sample(    # df.sample equivalent
      n = 1000,      # number of obs in each group, placeholder
      replace = TRUE
    )
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
write.csv(train, "/home/rstudio/Research/train.csv", row.names = FALSE)
write.csv(boot_train, "/home/rstudio/Research/boot_train.csv", row.names = FALSE)
write.csv(test, "/home/rstudio/Research/test.csv", row.names = FALSE)







