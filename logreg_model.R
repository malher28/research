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

# load train, test dataframes from equity bootstrapping
original_train <- read.csv("/home/rstudio/Research/train.csv")
equity_train <- read.csv("/home/rstudio/Research/boot_train.csv")
test <- read.csv("/home/rstudio/Research/test.csv")




# make blind training set -> two groups for each class, bootstrapped
# make two class groups
class_groups <- split(original_train,
                      list(original_train$target) #group 1 and group 0
)

blind_groups <- {}

# loop for blind bootstrapping
for (name in names(class_groups)){
  blind_groups[[name]] <- class_groups[[name]] %>% 
    slice_sample(    # df.sample equivalent
      n = 3000,      # number of obs in each group, n=3000 for consistency
      replace = TRUE
    )
}

# combine and shuffle for blind train
comb_blind_train <- bind_rows(blind_groups)
blind_train <- comb_blind_train[sample(1:nrow(comb_blind_train)), ]

#check
print(head(blind_train))








## train blind log reg model
#define blind model
blind_log_fit <- glm(target ~ ., # formula -> target is response, . -> every other variable for prediction
                            family = "binomial", # placeholder
                            data = blind_train)
blind_log_fit

summary(blind_log_fit)





# predict using log reg models
blind_pred <- predict(blind_log_fit, # training model
                       test, type = "response") # test data

# check 
print(head(blind_pred)) # currently in percent, will need to convert to class with threshold for confusion marix

# convert to class 0/1 with threshold at .50
blind_pred[blind_pred<0.5] = 0
blind_pred[blind_pred>=0.5] = 1

# check again
print(head(blind_pred)) # yay!



# confusion matrix for blind model
blind_matrix <- table(test$target, blind_pred) #comparing actual target from test to blind prediction


# make blind heatmap
blind_matrix_df <- as.data.frame(blind_matrix)
colnames(blind_matrix_df) <- c("Actual", "Predicted", "Count")

ggplot(blind_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "black", size = 6) +  # Add text labels
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Blind Model Confusion Matri", x = "Actual", y = "Predicted") +
  theme_minimal()






# train equity log reg model
# define equity model
equity_log_fit <- glm(target ~ ., # formula -> target is response, . -> every other variable for prediction
                      family = "binomial", # placeholder
                      data = equity_train)
equity_log_fit

summary(equity_log_fit)


# predict using log reg models
equity_pred <- predict(equity_log_fit, # training model
                      test, type = "response") # test data

# check 
print(head(equity_pred)) # currently in odds percent, will need to convert to class with threshold

# convert to class 0/1 with threshold at .50
equity_pred[equity_pred<0.5] = 0
equity_pred[equity_pred>=0.5] = 1

# check again
print(head(equity_pred)) # yay!



# confusion matrix for blind model
equity_matrix <- table(test$target, equity_pred) #comparing actual target from test to blind prediction


# make equity heatmap
equity_matrix_df <- as.data.frame(equity_matrix)
colnames(equity_matrix_df) <- c("Actual", "Predicted", "Count")

ggplot(equity_matrix_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), color = "black", size = 6) +  # Add text labels
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Equity Model Confusion Matrix", x = "Actual", y = "Predicted") +
  theme_minimal()


# metrics function
comp_metrics <- function(conf_mat){
  
  TN <- conf_mat[1,1]
  FP <- conf_mat[1,2]
  FN <- conf_mat[2,1]
  TP <- conf_mat[2,2]
  
  accuracy <- (TP + TN) / sum(conf_mat)
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  return(data.frame(
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Specificity = specificity
  ))
}


# compute AUC
blind_auc <- auc(test$target, blind_pred)
equity_auc <- auc(test$target, equity_pred)




# print all the metrics
cat("\nBlind Logistic Regression\n")
print(comp_metrics(blind_matrix))

cat("\nEquity Logistic Regression\n")
print(comp_metrics(equity_matrix))

cat("\nBlind AUC:", round(blind_auc, 4), "\n")
cat("Equity AUC:", round(equity_auc, 4), "\n")



