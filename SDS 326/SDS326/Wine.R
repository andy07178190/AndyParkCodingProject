library(tidyverse)
library(corrplot)
library(glmnet)
library(caret)
library(randomForest)
library(xgboost)
library(mgcv)
library(pROC)

redWines <- read_csv("winequality_red.csv") %>%
  rename_with(~ gsub("[ .]", "_", .)) %>%
  mutate(type = "Red")
whiteWines <- read_csv("winequality_white.csv") %>%
  rename_with(~ gsub("[ .]", "_", .)) %>%
  mutate(type = "White")

wines <- bind_rows(whiteWines, redWines) %>%
  mutate(
    type  = factor(type, levels = c("White", "Red")),
    isRed = as.integer(type == "Red")
  )

white <- filter(wines, type == "White")

# OLS vs. Lasso for White-Wine Quality Prediction
modelDiag <- lm(quality ~ alcohol + volatile_acidity + sulphates + residual_sugar, data = white)
oldMfrow <- par("mfrow"); oldMar <- par("mar"); oldOma <- par("oma")
par(mfrow = c(2, 2), mar = c(3.5, 3.5, 2.5, 1), oma = c(0, 0, 1.5, 0))
plot(modelDiag)
par(mfrow = oldMfrow, mar = oldMar, oma = oldOma)

residBase <- resid(modelDiag)
rmseBase <- sqrt(mean(residBase^2))
adjR2Base <- summary(modelDiag)$adj.r.squared
inflPoints <- order(cooks.distance(modelDiag), decreasing = T)[1:5]
trimmedData <- white[-inflPoints, ]

refitModel <- lm(quality ~ alcohol + volatile_acidity + sulphates + residual_sugar, data = trimmedData)
summary(refitModel)

residRefit <- resid(refitModel)
rmseRefit <- sqrt(mean(residRefit^2))
adjR2Refit <- summary(refitModel)$adj.r.squared

# Exploring Nonlinearity in the Alcohol–Quality Relationship
gamModel <- gam(quality ~ s(alcohol) + volatile_acidity + sulphates + residual_sugar, data = white)
summary(gamModel)
oldMar2 <- par("mar")
par(mar = c(3, 3, 2, 1))
plot(gamModel, rug = T, pages = 1)
par(mar = oldMar2)

# Secondary modeling to capture curvature
polyModel <- lm(quality ~ alcohol + I(alcohol^2) + volatile_acidity + sulphates + residual_sugar, data = white)
summary(polyModel)
residPoly <- resid(polyModel)
rmsePoly <- sqrt(mean(residPoly^2))
adjR2Poly <- summary(polyModel)$adj.r.squared

# Model Generalization 10-cross Performance Across Resamples
cvCtrl<- trainControl(method = "cv", number = 10)
cvOls <- train(quality ~ alcohol + volatile_acidity + sulphates + residual_sugar, data = white, method = "lm", trControl = cvCtrl)
cvPoly <- train(quality ~ alcohol + I(alcohol^2) + volatile_acidity + sulphates + residual_sugar, data = white, method = "lm", trControl = cvCtrl)
cvGam <- train(quality ~ alcohol + volatile_acidity + sulphates + residual_sugar, data = white, method = "gamLoess", trControl = cvCtrl)

resamps <- resamples(list(OLS  = cvOls, Poly = cvPoly, GAM  = cvGam))
summary(resamps)

clf <- wines %>%
  dplyr::mutate(lbl = factor(ifelse(quality >= 6, "High", "Low"), levels = c("Low","High"))) %>%
  dplyr::select(alcohol, volatile_acidity, sulphates, residual_sugar, lbl)

set.seed(5)
idx <- caret::createDataPartition(clf$lbl, p = 0.8, list = F)
train <- clf[idx, ]
test <- clf[-idx, ]

# Random Forest Classification
rf_model <- randomForest(lbl ~ ., data = train, ntree = 500)
rf_pred <- predict(rf_model, test)
print(caret::confusionMatrix(rf_pred, test$lbl))

# Boost Interpretation
train_mat <- as.matrix(train %>% dplyr::select(-lbl))
train_lab <- as.numeric(train$lbl == "High")
test_mat <- as.matrix(test  %>% dplyr::select(-lbl))
test_lab <- as.numeric(test$lbl == "High")

dtrain <- xgb.DMatrix(data = train_mat, label = train_lab)
dtest <- xgb.DMatrix(data = test_mat, label = test_lab)

params <- list(objective = "binary:logistic", eval_metric = "auc")
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, val = dtest),
  early_stopping_rounds = 10,
  verbose = 0
)
xgb_scores <- predict(xgb_model, dtest)
xgb_pred <- factor(ifelse(xgb_scores > 0.5, "High", "Low"),
                     levels = c("Low","High"))
print(caret::confusionMatrix(xgb_pred, test$lbl))

# ROC comparison
roc_rf <- pROC::roc(test$lbl, predict(rf_model, test, type = "prob")[,2])
roc_xgb <- pROC::roc(test$lbl, xgb_scores)


plot(roc_rf,  col = "blue", main = "ROC Curves")
lines(roc_xgb, col = "red")
legend("bottomright", legend = c(paste("RF AUC =",  round(pROC::auc(roc_rf), 3)), 
                                 paste("XGB AUC =", round(pROC::auc(roc_xgb),3))),
       col = c("blue","red"), lwd = 2)

