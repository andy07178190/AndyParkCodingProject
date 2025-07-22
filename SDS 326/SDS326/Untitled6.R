# 1. 라이브러리 불러오기
library(tidyverse)      # 데이터 처리 및 시각화
library(corrplot)       # 상관관계 히트맵
library(lmtest)         # Breusch-Pagan test (이분산성 검정)
library(MASS)           # robust regression (필요시)
library(caret)          # 데이터 분할
library(car)            # 영향도 진단 (influencePlot)

# 2. 데이터 불러오기 및 전처리
red <- read.csv("/Users/baghuijae/Downloads/winequality_red.csv", header = TRUE, stringsAsFactors = FALSE)
white <- read.csv("/Users/baghuijae/Downloads/winequality_white.csv", header = TRUE, stringsAsFactors = FALSE)

# 컬럼명에 포함된 공백 및 특수문자(점)를 모두 밑줄(_)로 변경
colnames(red) <- gsub("[ .]", "_", colnames(red))
colnames(white) <- gsub("[ .]", "_", colnames(white))

# 와인 색상 변수 추가
red$Color <- "Red"
white$Color <- "White"

# 두 데이터셋 결합
wine <- bind_rows(red, white)

# Color 변수를 팩터형으로 변환 및 수치형 변수(Color_Red) 생성
wine$Color <- as.factor(wine$Color)
wine$Color_Red <- ifelse(wine$Color == "Red", 1, 0)

# 3. 데이터 품질 확인 및 탐색적 데이터 분석 (EDA)
summary(wine)
cat("총 결측치 수:", sum(is.na(wine)), "\n")

ggplot(wine, aes(x = quality, fill = Color)) +
  geom_bar(position = "dodge") +
  labs(title = "와인 품질 분포 (Red vs White)", x = "품질", y = "빈도")

wine %>%
  group_by(Color) %>%
  summarise(MeanQuality = mean(quality)) %>%
  ggplot(aes(x = Color, y = MeanQuality, fill = Color)) +
  geom_bar(stat = "identity") +
  labs(title = "색상별 평균 품질", y = "평균 품질")

# 4. 상관관계 분석
red_numeric <- red %>% select_if(is.numeric)
red_corr <- cor(red_numeric)
corrplot(red_corr, method = "color", title = "Red Wine 상관관계", mar = c(0,0,1,0))

white_numeric <- white %>% select_if(is.numeric)
white_corr <- cor(white_numeric)
corrplot(white_corr, method = "color", title = "White Wine 상관관계", mar = c(0,0,1,0))

common_vars <- intersect(colnames(red_numeric), colnames(white_numeric))
diff_corr <- red_corr[common_vars, common_vars] - white_corr[common_vars, common_vars]
corrplot(diff_corr, method = "color", title = "Red와 White 상관관계 차이", mar = c(0,0,1,0))

# 5. 선형 회귀모델 구축
model_all <- lm(quality ~ total_sulfur_dioxide + free_sulfur_dioxide + residual_sugar + 
                  fixed_acidity + volatile_acidity + alcohol + sulphates + pH + density + Color_Red,
                data = wine)
summary(model_all)

par(mfrow = c(2, 2))
plot(model_all)
par(mfrow = c(1, 1))

mse_in <- mean(model_all$residuals^2)
cat("전체 데이터 인-샘플 MSE:", mse_in, "\n")

# 6. 학습/테스트 데이터 분할 및 평가
set.seed(4)
train_index <- createDataPartition(wine$quality, p = 0.8, list = FALSE)
train_data <- wine[train_index, ]
test_data <- wine[-train_index, ]

model_train <- lm(quality ~ total_sulfur_dioxide + free_sulfur_dioxide + residual_sugar + 
                    fixed_acidity + volatile_acidity + alcohol + sulphates + pH + density + Color_Red,
                  data = train_data)
summary(model_train)

pred_train <- predict(model_train, train_data)
pred_test <- predict(model_train, test_data)

mse_train <- mean((train_data$quality - pred_train)^2)
mse_test <- mean((test_data$quality - pred_test)^2)
mse_train
mse_test

# 7. 잔차 분석
test_residuals <- test_data$quality - pred_test

ggplot(data.frame(residuals = test_residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 0.2, fill = "lightblue", color = "black") +
  labs(title = "테스트 데이터 잔차 분포", x = "잔차", y = "빈도")

qqnorm(test_residuals)
qqline(test_residuals, col = "red")

# 8. 이분산성 및 정규성 검정
bp_test <- bptest(model_train)
cat("Breusch-Pagan Test 결과:\n")
print(bp_test)



# 9. 영향도 및 이상치 진단
plot(model_train, which = 4, main = "Cook's Distance 플롯")
influencePlot(model_train, main = "영향도 플롯", sub = "원 크기가 Cook's Distance에 비례")


## -------------------------
  
robust_model <- rlm(quality ~ total_sulfur_dioxide + free_sulfur_dioxide + residual_sugar + 
                        fixed_acidity + volatile_acidity + alcohol + sulphates + pH + density + Color_Red,
                      data = train_data)

summary(robust_model)

robust_pred_test <- predict(robust_model, newdata = test_data)
robust_mse_test <- mean((test_data$quality - robust_pred_test)^2)
robust_mse_test

wine_scaled <- wine %>%
  mutate(across(c(total_sulfur_dioxide, free_sulfur_dioxide, residual_sugar, fixed_acidity, 
                  volatile_acidity, alcohol, sulphates, pH, density), scale))

model_scaled <- lm(quality ~ total_sulfur_dioxide + free_sulfur_dioxide + residual_sugar + 
                     fixed_acidity + volatile_acidity + alcohol + sulphates + pH + density + Color_Red,
                   data = wine_scaled)

summary(model_scaled)

# Check nonlinearity
ggplot(wine, aes(x = alcohol, y = quality)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(title = "Alcohol vs Quality")

# Optional: try log transformation or polynomial
model_poly <- lm(quality ~ poly(alcohol, 2) + volatile_acidity + sulphates + 
                   residual_sugar + fixed_acidity + pH + density + Color_Red +
                   total_sulfur_dioxide + free_sulfur_dioxide, data = train_data)

summary(model_poly)

vif(model_all)

