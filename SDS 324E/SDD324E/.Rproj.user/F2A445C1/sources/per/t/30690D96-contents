### Review problems for final exam ###

## Chapter 12 Interaction Effects
# Ex 12.8.1
Boston <- MASS::Boston
?Boston
# 12.8.1(a)
lm1 <- lm(medv ~ nox * dis + lstat, data=Boston)
summary(lm1)
# Describe the model
# medv = beta0 + beta1*nox + beta2*dis + beta3*nox*dis + beta4*lstat
# medv = beta0 + + beta2*dis + (beta1 + beta3*dis)*nox + beta4*lstat
# medv = beta0 + (beta2 + beta3*nox)*dis + beta1*nox + beta4*lstat
# Given a research question, what is the appropriate model?
# 12.8.1(b)
lm2 <- lm(medv ~ nox * dis + nox * lstat, data=Boston)
summary(lm2)
# medv = beta0 +  beta1*nox + beta2*dis + beta3*nox*dis + beta4*nox*lstat + beta5*lstat

# 12.8.1(c) Based on the output of summary on part (b), 
# does the evidence suggest that all of the main effects (nox, dis, and lstat) 
# are needed for predicting medv?
## Based on the summary table, lstat and nox:dis are pretty significant, since both of them are lower than 0.05. However, since nox, dis, and nox:lstat are higher than 0.05, we cannot say all data are needed for prediciton.
# 12.8.1(d) Interpret the interaction between nox and dis in part (a), 
# in terms of how changing nox causes the effect of dis on medv to change.
## As nox increase, the medv get decrease, and finally get to negative value. That means as nox increase, the positive effect of dis getting decrease, and finally reach to negative.
# 12.8.1(e) Interpret the interaction between nox and dis in part (a), 
# in terms of how changing dis causes the effect of nox on medv to change.
## as distance increase, the effect of nox on medv getting decrease. But as the distance getting larger enought to reach medv to negative value, it affect negative,
# Ex 12.8.3
# run install.packages('ISLR') if you don't have the package
library(ISLR)
wage <- ISLR::Wage
?Wage
# 12.8.3(a)
lm3 <- lm(wage ~ age * education, data = wage)
summary(lm3)
# how do we interpret the coefficient for age:education5. Advanced Degree?
# what is the model predicted wage for 38 year old person with advanced degree? 
y = 68.09017 + (0.09876 * 61.13781)*34
## Chapter 13 Model Evaluation and Selection
# Ex 13.7.2
mcycle <- MASS::mcycle
?mcycle
# 13.7.2(a)
plot(x=mcycle$times, y=mcycle$accel, xlab='time in milliseconds after impact',
     ylab='acceleration' )
# 13.7.2(c)
# we will fit polynomial regression with degree from 1 to 10 as illustration
# what is polynomial regression with degree 1? 
# (1) 명시적으로 times 변수만 사용하는 선형 회귀
fit1 <- lm(accel ~ times, data = mcycle)
fit1
# (2) poly()를 써서 1차 다항이라고 지정
fit1 <- lm(accel ~ poly(times, 1), data = mcycle)
fit1
fit1_raw <- lm(accel ~ poly(times, 5, raw=TRUE), data=mcycle)
summary(fit1_raw)

# user-defined function to calculate LOO predictions for lm object
loo_predictions <- function(fit) {
  e <- residuals(fit)
  h <- hatvalues(fit)
  Y <- fitted(fit) + e
  return(Y - e / (1 - h))
}
# degrees to try
degrees = 1:10
# place holder to save LOORsq for different polynomial regression
LOORsq = rep(0, 10)
# a loop to fit all polynomial regression and calculate LOORsq
for (i in degrees) {
  poly_lm <- lm(accel ~ poly(times, degree=i), data=mcycle)
  # calculate LOO prediction y_hat_{-i}
  loo_poly_lm <- loo_predictions(poly_lm)
  # LOORsq = cor(y_hat_{-i}, y)^2
  LOORsq[i] <- cor(loo_poly_lm, mcycle$accel)^2
}
# make a plot to visualize
plot(x=degrees, y=LOORsq) 
# what is the best model based on LOORsq measure? 
# what if we use other model selection criteria: AIC, BIC, AdjRsq, PRESS...
## Based on the LOORsq measure, the model with degree 9 is the best model based on LOORsq measure, since it has more biggest value for LOORsq among 1 to 10.   
## 
## Chapter 15 Logistic Regression
# Ex 15.10.2
default_file <- paste0("https://raw.githubusercontent.com/dsnair/", "ISLR/master/data/csv/Default.csv")
Default <- read.csv(default_file)
# take a look at the data
head(Default)
# recode the response to be 0 and 1
Default$default <- ifelse(Default$default=="Yes", 1, 0)
# fit the logistic regression model 
# to predict default probability using student and balance
log1 <- glm(default ~ student + balance, data = Default, family = 'binomial')
summary(log1)
# how do we interpret all the coefficients???
# slope for balance:
# log odds of default increase by 5.738e-03 when balance increase by 1 unit,controlling for student status
# odds of default multiply by exp(5.738e-03) when balance increase by 1 unit,controlling for student status
# log odds of default decrease by 7.149e-01 comparing student vs. not student, controlling for balance
# odds of default multiply by exp(-7.149e-01) comparing student vs. not student, controlling for balance

# how to make prediction???
# a student with balance of 2000
# calculate log odds (the linear predictor)
# log_odds = -1.075e+01 + -7.149e-01 (1) + 5.738e-03*2000

# calculate odds 
odds = exp(log_odds)


# calculate probability
prob = odds / (1 + odds)
log_odds
prob
# use R predict function: the default is type="link" which returns log odds
predict(log1, newdata=data.frame(balance=2000, student="Yes"))
# use type="response" to return actual probability
predict(log1, newdata=data.frame(balance=2000, student="Yes"), type="response")

# how do we interpret confidence intervals for the coefficients???
confint(log1)

## Based on the confint(log1), for 95% significant confidence interval, when the log1 value should be between [-11.498,-10.05]. There is a student present and other situation are controlled, log1 value get increase between [-1.0077,-0.42]. Otherwise, when balance increase by 1, log1 increase [0.00529,0.00620], if other situations are same.  
# log odds of default increase by a range of [0.005296591, 0.006206095] when balance increase by 1 unit,controlling for student status
# odds of default multiply by a range of exp(0.005296591) to exp(0.006206095) when balance increase by 1 unit,controlling for student status


# fit the logistic regression model 
# to predict default probability using student, balance and an interaction
log2 <- glm(default ~ student*balance, data = Default, family = 'binomial')
summary(log2)
# how do we interpret the interaction???
# for non-student, slope of balance is 5.819e-03
# for student, slope of balance is 5.819e-03-2.196e-04 (smaller than slope of balance for non-student)
## Based on the summary(log2), only intercept and balance are significant. Even there is a interaction between studentYes and balance, it does not show any significant. This means there are no signficant correlation for studentYes:balance. Also, the difference of Null and Residual are big, that means the overall plot is not significant too. Big size of AIC supports that it's not signficant too.

# what is the log odds
log_odds = -1.087e+01-3.512e-01+(5.819e-03-2.196e-04)*2000
# what is the odds of default for a student with a balance of 2000?
odds = exp(log_odds)
prob = odds / (1+odds)
prob
# what is the prob of default for a non-student with a balance of 2000?
exp(-1.087e+01+5.819e-03*2000) / (exp(-1.087e+01+5.819e-03*2000)+1)

