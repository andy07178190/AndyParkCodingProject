# 6.6 10
# Question A
set.seed(1)
n <- 1000
p <- 20
Data <- matrix(rnorm(n * p), n, p)
colnames(Data) <- paste0("X", 1:p)
TrueB <- c(3, 2, -1.5, 0.7, -2, rep(0, p - 5))
Err <- rnorm(n)
Resp <- rowSums(Data * matrix(TrueB, n, p, byrow = TRUE)) + Err

# Question B
set.seed(2)
trainIdx <- sample(1:n, 100)
testIdx <- setdiff(1:n, trainIdx)
XTrain <- Data[trainIdx, , drop = FALSE]
YTrain <- Resp[trainIdx]
XTest <- Data[testIdx, , drop = FALSE]
YTest <- Resp[testIdx]

# Question C
library(leaps)
TrainDF <- data.frame(y = YTrain, XTrain)
SubsetMod <- regsubsets(y ~ ., data = TrainDF, nvmax = 10)

PredSubset <- function(mod, newdata, id) {
  coefs <- coef(mod, id = id)
  # Get the predictor names from the coefficients (excluding intercept)
  predNames <- names(coefs)[-1]
  if (length(predNames) == 0) {
    mm <- model.matrix(~1, newdata)
  } else {
    # Build a formula using only the selected predictors
    f <- as.formula(paste("~", paste(predNames, collapse = " + ")))
    mm <- model.matrix(f, newdata)
  }
  mm[, names(coefs), drop = FALSE] %*% coefs
}

# Question D
maxSize <- 10
Sizes <- 0:maxSize
TrainMSE <- numeric(maxSize + 1)
TestMSE <- numeric(maxSize + 1)
CoefErr <- numeric(maxSize + 1)

Mod0 <- lm(y ~ 1, data = data.frame(y = YTrain))
TrainMSE[1] <- mean(residuals(Mod0)^2)
TestMSE[1] <- mean((YTest - coef(Mod0)[1])^2)
Bhat <- rep(0, p)
CoefErr[1] <- sum((Bhat - TrueB)^2)

for (r in 1:maxSize) {
  trainPred <- PredSubset(SubsetMod, newdata = TrainDF, id = r)
  TrainMSE[r + 1] <- mean((YTrain - trainPred)^2)
  
  TestDF <- as.data.frame(XTest)
  colnames(TestDF) <- colnames(XTrain)
  testPred <- PredSubset(SubsetMod, newdata = TestDF, id = r)
  TestMSE[r + 1] <- mean((YTest - testPred)^2)
  
  Bhat <- rep(0, p)
  coefs <- coef(SubsetMod, id = r)
  if (length(coefs) > 1) {
    for (nm in names(coefs)[-1]) {
      idx <- as.numeric(sub("X", "", nm))
      Bhat[idx] <- coefs[nm]
    }
  }
  CoefErr[r + 1] <- sum((Bhat - TrueB)^2)
}

OptSize <- which.min(TestMSE) - 1

par(mfrow = c(1, 3))
plot(Sizes, TrainMSE, type = "b", xlab = "Model Size", ylab = "Train MSE", main = "Train MSE")
grid()
plot(Sizes, TestMSE, type = "b", xlab = "Model Size", ylab = "Test MSE", main = "Test MSE")
grid()
plot(Sizes, CoefErr, type = "b", xlab = "Model Size", ylab = "Coef Error", main = "Coef Error")
grid()
par(mfrow = c(1, 1))

print(OptSize)
print(coef(SubsetMod, id = OptSize))
print(TrueB)

# Question E
# Because the true data-generating process only involved 5 relevant predictors, using exactly those in the model achieves the best possible prediction accuracy without overfitting.
# The optimal model uses the true number of relevant predictors, which is exactly what subset selection found. 
# This confirms that subset selection can recover the true model when data are generated in this controlled way and that test MSE is minimized at the right complexity.

# Question F
# The model selected at size 5 includes exactly the correct variables. The estimated coefficients are very close to the true coefficients (with small random error).
# The best subset selection method correctly identified the structure of the true model. This supports its effectiveness in sparse settings, especially when training data is informative enough.

# QuestionG
# Coefficient error is lowest at model size 5, this same as where test MSE is minimized.
# As model size increases beyond 5, coefficient error does not improve, because extra variables are irrelevant and just add noise to the estimates.
# This shows that including only the true predictors leads to the most accurate coefficient estimates, and the larger models may not help and might hurt, even if they reduce training error.


# 6.6 11
# Question A
library(MASS)
library(leaps)
library(glmnet)
install.packages("pls")
library(pls)

data(Boston)
set.seed(5)
trainidx <- sample(1:nrow(Boston), size = round(0.7 * nrow(Boston)))
Bostontrain <- Boston[trainidx, ]
Bostontest  <- Boston[-trainidx, ]

bestsub <- regsubsets(crim ~ ., data = Bostontrain, nvmax = 13)
trainmat <- model.matrix(crim ~ ., data = Bostontrain)
testmat  <- model.matrix(crim ~ ., data = Bostontest)
valerrors <- rep(NA, 13)
for (i in 1:13) {
  coefi <- coef(bestsub, id = i)
  pred  <- testmat[, names(coefi)] %*% coefi
  valerrors[i] <- mean((Bostontest$crim - pred)^2)
}
bestsize <- which.min(valerrors)
bestsubmse <- min(valerrors)

xtrain <- model.matrix(crim ~ ., data = Bostontrain)[, -1]
ytrain <- Bostontrain$crim
xtest  <- model.matrix(crim ~ ., data = Bostontest)[, -1]
ytest  <- Bostontest$crim

set.seed(5)
cvridge <- cv.glmnet(xtrain, ytrain, alpha = 0)
lambdaridge <- cvridge$lambda.min
ridgefit <- glmnet(xtrain, ytrain, alpha = 0, lambda = lambdaridge)
ridgepred <- predict(ridgefit, newx = xtest)
ridgemse <- mean((ytest - ridgepred)^2)

set.seed(5)
cvlasso <- cv.glmnet(xtrain, ytrain, alpha = 1)
lambdalasso <- cvlasso$lambda.min
lassofit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambdalasso)
lassopred <- predict(lassofit, newx = xtest)
lassomse <- mean((ytest - lassopred)^2)

set.seed(5)
pcrfit <- pcr(crim ~ ., data = Bostontrain, scale = TRUE, validation = "CV")
validationplot(pcrfit, val.type = "MSEP", main = "pcr: cv error")
rmsep <- RMSEP(pcrfit)
optncomp <- which.min(rmsep$val[1, 1, -1])
pcrpred <- predict(pcrfit, Bostontest, ncomp = optncomp)
pcrmse <- mean((Bostontest$crim - pcrpred)^2)

# QuestionB
print(bestsize)
print(round(bestsubmse, 4))
print(round(ridgemse, 4))
print(round(lassomse, 4))
print(round(pcrmse, 4))

# Question C
lassocoefs <- coef(lassofit)
print(lassocoefs)



# 6.6 7
# Question A
set.seed(10)
n <- 100
p <- 5
x <- matrix(rnorm(n * p), n, p)
truebeta <- c(3, -2, 0, 1, 0)
sigma2 <- 1
epsilon <- rnorm(n, mean = 0, sd = sqrt(sigma2))
y <- x %*% truebeta + epsilon

negloglik <- function(beta, x, y, sigma2) {
  sum((y - x %*% beta)^2) / (2 * sigma2)
}

#Question B

neglogpostridge <- function(beta, x, y, sigma2, cval) {
  negloglik(beta, x, y, sigma2) + sum(beta^2) / (2 * cval)
}
neglogpostlasso <- function(beta, x, y, sigma2, bval) {
  negloglik(beta, x, y, sigma2) + sum(abs(beta)) / bval
}

cval <- 1
bval <- 1

# Question C
beta0 <- rep(0, p)
optridge <- optim(beta0, neglogpostridge, x = x, y = y, sigma2 = sigma2, cval = cval)
optlasso <- optim(beta0, neglogpostlasso, x = x, y = y, sigma2 = sigma2, bval = bval, method = "BFGS")

print(optridge$par)
print(optlasso$par)

# QUestion D
library(glmnet)
ridgefit <- glmnet(x, y, alpha = 0, lambda = 1, standardize = FALSE, intercept = FALSE)
lassofit <- glmnet(x, y, alpha = 1, lambda = 2, standardize = FALSE, intercept = FALSE)

print(as.vector(coef(ridgefit))[-1])
print(as.vector(coef(lassofit))[-1])

