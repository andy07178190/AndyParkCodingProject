# alternate_knn.R

# 0. Load packages (install first if needed)
if (!requireNamespace("tidymodels", quietly = TRUE)) install.packages("tidymodels")
if (!requireNamespace("kknn",      quietly = TRUE)) install.packages("kknn")

library(tidyverse)
library(kknn)
library(recipes)
library(rsample)
library(purrr)

# 1. Read and preprocess data
crash <- read_csv("Austin_Crash_Report_Data_-_Crash_Level_Records_20250407.csv.gz") %>%
  mutate(
    fatal = factor(crash_fatal_fl,
                   levels = c(FALSE, TRUE),
                   labels = c("NonFatal", "Fatal"))
  )

# 2. Generate 10-fold CV splits
set.seed(1)
splits_vfold <- vfold_cv(crash, v = 10, strata = fatal)

# 3. Define k values to try
k_vals <- 1:20

# 4. Manual CV loop: compute mean fold accuracy for each k
cv_results <- map_dfr(k_vals, function(k) {
  accs <- map_dbl(splits_vfold$splits, function(split) {
    # split into train/test
    train_df <- analysis(split)
    test_df  <- assessment(split)

    # preprocessing: impute + dummy variables
    rec <- recipe(fatal ~ crash_speed_limit + road_constr_zone_fl + onsys_fl, data = train_df) %>%
      step_impute_mode(all_nominal_predictors()) %>%
      step_impute_median(all_numeric_predictors()) %>%
      step_dummy(all_nominal_predictors()) %>%
      prep(training = train_df, retain = TRUE)

    train_mat <- bake(rec,    new_data = train_df)
    test_mat  <- bake(rec,    new_data = test_df)

    # train kNN and predict
    knn_fit <- train.kknn(fatal ~ ., data = train_mat, kmax = k, kernel = "rectangular")
    preds   <- knn_fit$fitted.values

    # fold accuracy
    mean(preds == test_mat$fatal)
  })

  # return average CV accuracy for this k
  tibble(neighbors = k, cv_accuracy = mean(accs))
})

print(cv_results)

# 5. Select best k
best_k <- cv_results %>% slice_max(cv_accuracy, n = 1) %>% pull(neighbors)
cat("Best k =", best_k, "\n\n")

# 6. Final model on 75% train / 25% test
set.seed(1)
split_full <- initial_split(crash, prop = 0.75, strata = fatal)
ctrain <- training(split_full)
ctest  <- testing(split_full)

# full-train preprocessing
rec_full <- recipe(fatal ~ crash_speed_limit + road_constr_zone_fl + onsys_fl, data = ctrain) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  prep(training = ctrain, retain = TRUE)

train_mat <- bake(rec_full, new_data = ctrain)
test_mat  <- bake(rec_full, new_data = ctest)

# train final kNN and evaluate
final_knn  <- train.kknn(fatal ~ ., data = train_mat, kmax = best_k, kernel = "rectangular")
final_preds <- final_knn$fitted.values
test_acc    <- mean(final_preds == test_mat$fatal)

cat("Test-set accuracy:", round(test_acc, 4), "\n")
