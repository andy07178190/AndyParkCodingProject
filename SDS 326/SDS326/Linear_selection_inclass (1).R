#### This file is for an in-class exercise with linear model selection and regularization 
#### We try Problem 9 in Section 6 in the book "An Introduction to Statistical Learning"

library(ggplot2)
library(tidyr)
library(dplyr)
library(leaps) 
library(glmnet)
library(MLmetrics)

############################ Part (a)
n = 100
X_predictor <- rnorm(n)
noise <- rnorm(n)

############################ Part (b)
beta_0 = 1
beta_1 = 2
beta_2 = 3
beta_3 = 4
Y_response <- beta_0 + beta_1 * X_predictor + beta_2 * X_predictor^2 + beta_3 * X_predictor^3 + noise

######################### Part (c)
### We need to choose the best polynomial regression of order 10 
experiment_data <- data.frame(X_predictor, X_predictor^2, X_predictor^3, X_predictor^4, X_predictor^5, X_predictor^6, X_predictor^7, X_predictor^8, X_predictor^9, X_predictor^10, Y_response)

### We use "regsubsets" operator to perform best subset selection. It chooses the best set of variables for each model size
my_regsubset <- regsubsets(Y_response~., experiment_data, nvmax = 10)
my_summary <- summary(my_regsubset)

### Create plots with these values

# With C_p, AIC
ggplot(as.data.frame(my_summary$cp), aes(x = seq(1:10), y = my_summary$cp)) + geom_line(color = "red") + geom_point(size = 1) + xlab("Number of predictors") + ylab("C_p")

# With BIC
ggplot(as.data.frame(my_summary$bic), aes(x = seq(1:10), y = my_summary$cp)) + geom_line(color = "red") + geom_point(size = 1) + xlab("Number of predictors") + ylab("BIC")

# With adjusted R^2
ggplot(as.data.frame(my_summary$adjr2), aes(x = seq(1:10), y = my_summary$adjr2)) + geom_line(color = "pink") + geom_point(size = 1) + xlab("Number of predictors") + ylab("Adjusted R2")

# From the plots of C_p, BIC, and adjusted R^2, we see that the best model consists of three predictors X, X^2, and X^3
# We now perform linear model on Y given these predictors
my_best_lm <- lm(Y_response~ X_predictor + X_predictor.2 + X_predictor.3, data = experiment_data)
summary(my_best_lm)

############ Part (d)
## We now perform forward and backward stepwise selections

## Forward stepwise selection
forward_selec <- regsubsets(Y_response~., experiment_data, method = "forward", nvmax = 10)
my_summary <- summary(forward_selec)

ggplot(as.data.frame(my_summary$cp), aes(x = seq(1:10), y = my_summary$cp)) + geom_line(color = "red") + geom_point(size = 1) + xlab("Number of predictors") + ylab("C_p")

## Backward stepwise selection
backward_selec <- regsubsets(Y_response~., experiment_data, method = "backward", nvmax = 10)
my_summary  <- summary(backward_selec)

ggplot(as.data.frame(my_summary$cp), aes(x = seq(1:10), y = my_summary$cp)) + geom_line(color = "red") + geom_point(size = 1) + xlab("Number of predictors") + ylab("C_p")

############################################################ Part (e)
### We now use the Lasso with the experiment_data
### We consider 100 values of lambda from 0 to 10
lambda_val <- seq(10,0, length = 100)
predictors_val <- model.matrix(Y_response~.,experiment_data)[,-1]
my_lasso <- glmnet(predictors_val,Y_response, alpha = 1, lambda = lambda_val)


### We observe the coefficients from Lasso fit
coef(my_lasso)

### We perform cross-validation using 10 training/ test sets
set.seed(1000)
N = 10 # number of training / test sets
M = 100 # number of values of lambda
lambda_val <- seq(0,10, length = M) # sequence of values of lambda
rmse_out_val <- matrix(0, nrow = M, ncol = N)

### 75% of the sample size for the training set
sample_size <- floor(0.75 * nrow(experiment_data))

for (i in 1:N)
{
  train_index <- sample(seq_len(nrow(experiment_data)), size = sample_size)
  experiment_data_train <- experiment_data[train_index,]
  experiment_data_test <- experiment_data[-train_index,]
  
  response_train_val <- experiment_data_train$Y_response
  predictors_train_val <- model.matrix(Y_response~.,experiment_data_train)[,-1]
  response_test_val <- experiment_data_test$Y_response
  predictors_test_val <- model.matrix(Y_response~.,experiment_data_test)[,-1]
  
  for (j in 1:M)
  {
    my_lasso <- glmnet(predictors_train_val, response_train_val, alpha = 1, lambda = lambda_val[j])
    my_lasso_pred <- predict(my_lasso, s = lambda_val[j], predictors_test_val)
    rmse_out_val[j,i] = RMSE(my_lasso_pred, response_test_val)
  }
}

### Plot the RMSE for 10 training/ test sets at different lambda
new_rmse_out_val <- as.data.frame(rmse_out_val)
plot <- ggplot(data = new_rmse_out_val, aes(x = lambda_val))
for (i in 1:N)
{
  plot <- plot + geom_line( aes_string( y = new_rmse_out_val[,i]), col = i+2)
}
plot + ylab('rmse_out')

#### Take the average of error rates over N training/ test sets
average_rmse_out_val <- rowMeans(rmse_out_val) #We take the average of each row in the matrix rmse_out_val
lambda_best <- lambda_val[which.min(average_rmse_out_val)] ### Best lambda
average_rmse_out_val <- as.data.frame(average_rmse_out_val)
ggplot(average_rmse_out_val, aes(x = lambda_val, y = average_rmse_out_val)) + geom_line(col = "red")

#### Fitting coefficients using the best lambda 
my_best_lasso <- glmnet(predictors_val,Y_response, alpha = 1, lambda = lambda_best)
coef(my_best_lasso)

##################################################### Part (f)
## Generate new Y response
beta_0 = 1
beta_7 = 8
Y_new_response <- beta_0 + beta_7 * X_predictor^7 + noise

## Create a new data frame
new_experiment_data <- data.frame(X_predictor, X_predictor^2, X_predictor^3, X_predictor^4, X_predictor^5, X_predictor^6, X_predictor^7, X_predictor^8, X_predictor^9, X_predictor^10, Y_new_response)

################# We use "regsubsets" operator to perform best subset selection. It chooses the best set of variables for each model size
my_regsubset <- regsubsets(Y_new_response~., new_experiment_data, nvmax = 10)
my_summary <- summary(my_regsubset)

### Create plots with these values

# With C_p
ggplot(as.data.frame(my_summary$cp), aes(x = seq(1:10), y = my_summary$cp)) + geom_line(color = "red") + geom_point(size = 1) + xlab("Number of predictors") + ylab("C_p")

summary(lm(Y_new_response~ X_predictor.2 + X_predictor.4 + X_predictor.6 + X_predictor.7, data = new_experiment_data))
# With BIC
ggplot(as.data.frame(my_summary$bic), aes(x = seq(1:10), y = my_summary$cp)) + geom_line(color = "red") + geom_point(size = 1) + xlab("Number of predictors") + ylab("BIC")

# With adjusted R^2
ggplot(as.data.frame(my_summary$adjr2), aes(x = seq(1:10), y = my_summary$adjr2)) + geom_line(color = "pink") + geom_point(size = 1) + xlab("Number of predictors") + ylab("Adjusted R2")

################################################# Now, we try the lasso
lambda_val <- seq(10,0, length = 100)
predictors_val <- model.matrix(Y_new_response~.,new_experiment_data)[,-1]
my_lasso <- glmnet(predictors_val,Y_new_response, alpha = 1, lambda = lambda_val)

### We observe the coefficients from Lasso fit
coef(my_lasso)

### We perform cross-validation using 10 training/ test sets
set.seed(1000)
N = 10 # number of training / test sets
M = 100 # number of values of lambda
lambda_val <- seq(0,10, length = M) # sequence of values of lambda
rmse_out_val <- matrix(0, nrow = M, ncol = N)

### 75% of the sample size for the training set
#sample_size <- floor(0.75 * nrow(new_experiment_data))
sample_size = 15

for (i in 1:N)
{
  train_index <- sample(seq_len(nrow(new_experiment_data)), size = sample_size)
  experiment_data_train <- new_experiment_data[train_index,]
  experiment_data_test <- new_experiment_data[-train_index,]
  
  response_train_val <- experiment_data_train$Y_new_response
  predictors_train_val <- model.matrix(Y_new_response~.,experiment_data_train)[,-1]
  response_test_val <- experiment_data_test$Y_new_response
  predictors_test_val <- model.matrix(Y_new_response~.,experiment_data_test)[,-1]
  
  for (j in 1:M)
  {
    my_lasso <- glmnet(predictors_train_val, response_train_val, alpha = 1, lambda = lambda_val[j])
    my_lasso_pred <- predict(my_lasso, s = lambda_val[j], predictors_test_val)
    rmse_out_val[j,i] = RMSE(my_lasso_pred, response_test_val)
  }
}

### Plot the RMSE for 10 training/ test sets at different lambda
new_rmse_out_val <- as.data.frame(rmse_out_val)
plot <- ggplot(data = new_rmse_out_val, aes(x = lambda_val))
for (i in 1:N)
{
  plot <- plot + geom_line( aes_string( y = new_rmse_out_val[,i]), col = i+2)
}
plot + ylab('rmse_out')

#### Take the average of error rates over N training/ test sets
average_rmse_out_val <- rowMeans(rmse_out_val) #We take the average of each row in the matrix rmse_out_val
average_rmse_out_val <- as.data.frame(average_rmse_out_val)
ggplot(average_rmse_out_val, aes(x = lambda_val, y = average_rmse_out_val)) + geom_line(col = "red")

#### Fitting coefficients using the best lambda 
average_rmse_out_val <- rowMeans(rmse_out_val)
lambda_best <- lambda_val[which.min(average_rmse_out_val)]

my_best_lasso <- glmnet(predictors_val,Y_new_response, alpha = 1, lambda = lambda_best)
coef(my_best_lasso)



