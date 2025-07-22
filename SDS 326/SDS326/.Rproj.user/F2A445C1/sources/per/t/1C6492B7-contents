# 1. 라이브러리 불러오기
library(tidyverse)      
library(lmtest)         # Breusch-Pagan test
library(MASS)           # robust regression
library(caret)          # 데이터 분할
library(car)            # 영향도 진단

# 2. 데이터 불러오기 및 전처리
red <- read.csv("/Users/baghuijae/Downloads/winequality_red.csv", header = TRUE, stringsAsFactors = FALSE)
white <- read.csv("/Users/baghuijae/Downloads/winequality_white.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(red) <- gsub("[ .]", "_", colnames(red))
colnames(white) <- gsub("[ .]", "_", colnames(white))
red$Color <- "Red"
white$Color <- "White"
wine <- bind_rows(red, white)
wine$Color <- as.factor(wine$Color)
wine$Color_Red <- ifelse(wine$Color == "Red", 1, 0)

# 3. 데이터 분할 (학습/테스트)
set.seed(4)
train_index <- createDataPartition(wine$quality, p = 0.8, list = FALSE)
train_data <- wine[train_index, ]
test_data <- wine[-train_index, ]

# 4. 학습 데이터 기반 선형 회귀 모델 구축 및 평가
model_train <- lm(quality ~ total_sulfur_dioxide + free_sulfur_dioxide + residual_sugar +
                    fixed_acidity + volatile_acidity + alcohol + sulphates + pH + density + Color_Red,
                  data = train_data)
summary(model_train)
pred_test <- predict(model_train, test_data)
mse_test <- mean((test_data$quality - pred_test)^2)
cat("테스트 데이터 MSE:", mse_test, "\n")

# 5. 잔차 분석 및 이분산성 검정
test_residuals <- test_data$quality - pred_test
bp_test <- bptest(model_train)
print(bp_test)

# 6. 영향도 및 이상치 진단
influencePlot(model_train, main = "영향도 플롯", sub = "원 크기가 Cook's Distance에 비례")

# 7. Robust 회귀 모델 구축 및 평가
robust_model <- rlm(quality ~ total_sulfur_dioxide + free_sulfur_dioxide + residual_sugar +
                      fixed_acidity + volatile_acidity + alcohol + sulphates + pH + density + Color_Red,
                    data = train_data)
summary(robust_model)
robust_pred_test <- predict(robust_model, newdata = test_data)
robust_mse_test <- mean((test_data$quality - robust_pred_test)^2)
cat("Robust 모델 테스트 MSE:", robust_mse_test, "\n")

# 8. 변수 스케일링 및 다항 회귀 (비선형성 확인)
wine_scaled <- wine %>%
  mutate(across(c(total_sulfur_dioxide, free_sulfur_dioxide, residual_sugar, fixed_acidity,
                  volatile_acidity, alcohol, sulphates, pH, density), scale))
model_poly <- lm(quality ~ poly(alcohol, 2) + volatile_acidity + sulphates +
                   residual_sugar + fixed_acidity + pH + density + Color_Red +
                   total_sulfur_dioxide + free_sulfur_dioxide, data = train_data)
summary(model_poly)
vif(model_train)

ggplot(wine, aes(x = alcohol, y = quality)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Alcohol vs Quality")
model_poly <- lm(quality ~ poly(alcohol, 2) + volatile_acidity + sulphates +
                   residual_sugar + fixed_acidity + pH + density + Color_Red +
                   total_sulfur_dioxide + free_sulfur_dioxide, data = train_data)
summary(model_poly)

