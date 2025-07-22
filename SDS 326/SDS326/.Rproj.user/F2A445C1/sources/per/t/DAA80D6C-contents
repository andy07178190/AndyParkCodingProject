# a)
install.packages("ISLR")
library(ISLR)
data(Hitters)
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
Hitters$logsalary <- log(Hitters$Salary)
names(Hitters) <- tolower(names(Hitters))

# b)
train200 <- 1:200
test200 <- -(1:200)
trainset <- Hitters[train200, ]
testset <- Hitters[test200, ]

# c)
install.packages("gbm")
library(gbm)
set.seed(1)
vals <- c(0.001, 0.01, 0.05, 0.1, 0.2)
ntrees <- 1000
mse <- rep(0, length(vals))
for (i in seq_along(vals)) {
  boostmod <- gbm(
    formula = logsalary ~ . - salary,
    data = trainset,
    distribution = "gaussian",
    n.trees = ntrees,
    shrinkage = vals[i],
    interaction.depth = 4
  )
  pred <- predict(boostmod, trainset, n.trees = ntrees)
  mse[i] <- mean((trainset$logsalary - pred)^2)
}

plot(vals, mse, type = "b", xlab = "shrinkage parameter", ylab = "training mse")

# d)
testmse <- rep(0, length(vals))
for (i in seq_along(vals)) {
  boostmod <- gbm(
    formula = logsalary ~ . - salary,
    data = trainset,
    distribution = "gaussian",
    n.trees = ntrees,
    shrinkage = vals[i],
    interaction.depth = 4
  )
  pred <- predict(boostmod, testset, n.trees = ntrees)
  testmse[i] <- mean((testset$logsalary - pred)^2)
}
plot(vals, testmse, type = "b", xlab = "shrinkage parameter", ylab = "test mse", 
     main = "boosting: test mse vs. shrinkage")

# e)
lmfit <- lm(logsalary ~ . - salary, data = trainset)
lmpred <- predict(lmfit, newdata = testset)
lmtestmse <- mean((testset$logsalary - lmpred)^2)
lmtestmse

library(glmnet)
x <- model.matrix(logsalary ~ . - salary, data = Hitters)[, -1]
y <- Hitters$logsalary
xtrain <- x[train200, ]
ytrain <- y[train200]
xtest <- x[test200, ]
ytest <- y[test200]
grid <- 10^seq(10, -2, length = 100)
ridgemod <- glmnet(xtrain, ytrain, alpha = 0, lambda = grid)
cvridge <- cv.glmnet(xtrain, ytrain, alpha = 0)
bestlambdaridge <- cvridge$lambda.min
ridgepred <- predict(ridgemod, s = bestlambdaridge, newx = xtest)
ridgemestmse <- mean((ytest - ridgepred)^2)
lassomod <- glmnet(xtrain, ytrain, alpha = 1, lambda = grid)
cvlasso <- cv.glmnet(xtrain, ytrain, alpha = 1)
bestlambdalasso <- cvlasso$lambda.min
lassopred <- predict(lassomod, s = bestlambdalasso, newx = xtest)
lassomestmse <- mean((ytest - lassopred)^2)
lassomestmse
# The linear regression model produced a test MSE about 0.4918, while the boosting models show improved performance. Ridge and lasso regression models tend to lower the test error relative to simple Linear Regression. Overall, boosting yield lower test MSEs, demonstrating their effectiveness in capturing nonlinear relationships.

# f)
boostmodbest <- gbm(
  formula = logsalary ~ . - salary,
  data = trainset,
  distribution = "gaussian",
  n.trees = ntrees,
  shrinkage = 0.1,
  interaction.depth = 4
)
summary(boostmodbest)
# Based on the variable importnace summary, the most influential predictor is Catbat, which accounts for about 22.9% of the model's relative influence. Other predictors that looks significant can be Chits, Cwalks, followed by years and outputs.

# g)
library(randomForest)
set.seed(1)
p <- ncol(trainset) - 2
bagmod <- randomForest(
  logsalary ~ . - salary,
  data = trainset,
  mtry = p,
  ntree = 500
)
bagpred <- predict(bagmod, newdata = testset)
bagtestmse <- mean((testset$logsalary - bagpred)^2)
bagtestmse
# When applying bagging using a random forest approach, the testMSE is about 0.2299. This is lower than the MSE from simple linear regression and competitive with the boosting approach.

# a)
install.packages("ISLR")
library(ISLR)
data(Caravan)
Caravan$Purchase <- tolower(as.character(Caravan$Purchase))

trainidx <- 1:1000
trainset <- Caravan[trainidx, ]
testset <- Caravan[-trainidx, ]
trainset$purchasenum <- ifelse(trainset$Purchase == "yes", 1, 0)
testset$purchasenum <- ifelse(testset$Purchase == "yes", 1, 0)

# b) & c)
install.packages("gbm")
library(gbm)
set.seed(2)
boostcaravan <- gbm(
  purchasenum ~ . - Purchase - purchasenum,
  data = trainset,
  distribution = "bernoulli",
  n.trees = 1000,
  shrinkage = 0.01,
  interaction.depth = 4
)
summary(boostcaravan)

boostprobs <- predict(boostcaravan, newdata = testset, n.trees = 1000, type = "response")
boostpreds <- ifelse(boostprobs > 0.5, "yes", "no")
cm <- table(predicted = boostpreds, actual = testset$Purchase)
precision <- cm["yes", "yes"] / sum(cm["yes", ])
precision

# The summary of the model shows that the top predictors are PPERSAUT, MOSTYPE, and PBRAND with relative influences of about 7.08%, 5.28%, and 4.63%.
# From the result of the matrix, 25.6% indicates that about one in four of people predicted to purchase actually did so.
