---
  title: "Project 2 Report"
output: html_document
---
  
  ## Group Members
  
  Specify the names and EIDs of all group members (if not working in a group just put your own name and EID): Andy Park, AP59864



## Question

State question being addressed and describe the outcome variable to be predicted. Explain why it is useful to answer this question.

*Can we predict whether a crash will be fatal?*
  
  By pinpointing conditions, such aslighting, road surface quality, and time of day that most often lead to fatal crashes, I can give Austin PD and city planners the insights they need to target their safety interventions more effectively.**
  
  
  ## Data and Setup
  
  Load the dataset into R below. In addition, load any packages that you will need using the `library()` function. For example, you will need to load the `tidyverse` and `tidymodels` packages.


```{r}
## Load package and compare outcome variable 
library(tidyverse)
library(tidymodels)
library(readr)

crash <- read_csv("Austin_Crash_Report_Data_-_Crash_Level_Records_20250407.csv")

crash <- crash %>%
  mutate(fatal = fct_recode(as_factor(crash_fatal_fl), "NonFatal" = "FALSE", "Fatal" = "TRUE"))
```


## Base Prediction Model

Use either linear regression (for continuous outcome) or logistic regression (binary outcome) to build a base prediction model. Use the tidymodels framework to 

1. Split the dataset into a training and testing dataset using 75% of observations for training and 25% for testing; leave the test dataset aside for now.

2. Select variables to include in your model recipe. You may need to experiment with different variable combinations to find the best predicting model.

Use 10-fold cross-validation on the training data to estimate either 

* the root mean squared error (RMSE) if your outcome is continuous; or 
* the accuracy if your outcome is binary.


```{r}
# Split the data, define a preprocessing recipe, and perform 10 fold cross validation to evaluate logistic regression model accuracy.
set.seed(1)
csplit <- initial_split(crash, prop = 0.75, strata = fatal)
ctrain <- training(csplit)
ctest  <- testing(csplit)
cfolds <- vfold_cv(ctrain, v = 10)

crash_recipe <- recipe(fatal ~ crash_speed_limit + road_constr_zone_fl + onsys_fl, data = ctrain) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors())

logm <- logistic_reg() %>%
  set_engine("glm")

logw <- workflow() %>%
  add_recipe(crash_recipe) %>%
  add_model(logm)

set.seed(1)
logr <- fit_resamples(logw, resamples = cfolds, metrics = metric_set(accuracy))

collect_metrics(logr)
```

Which configuration of variables in your model seems to produce the best performing model on the training data?
  
  **The configuration using crash_speed_limit, road_constr_zone_fl, and onsyls_fl produced 99.45% cross validated accuracy. That means higher speed limits, the presence of construction zones, and the raod type are the key environmental factors that may significnatly influence the likelihood of fatal crash.**
  
  ## Alternate Prediction Model
  
  For this part of the analysis, choose one machine learning approach to build an alternate prediction model. The ones we have seen in class are K-Nearest Neighbors (`nearest_neighbor()`), decision trees (`decision_tree()`), and random forests (`rand_forest()`). 

1. Identify the tuning parameters for your chosen model.

2. Explore which combination of variables and tuning parameters produces good model performance.

3. Create a workflow that assembles your recipe and your model specification.

4. Use 10-fold cross-validation to estimate either the RMSE (continous outcome) or the accuracy (binary outcome) for your model in the training data.

5. Tune your model using a range of possible tuning parameters to identify the best-predicting model

NOTE: Doing the cross-validation and model tuning here may take a *significant* amount of time depending on your choice of tuning parameters and the speed of your computer. You may need to be patient with the process here.


```{r}
library(vroom)
library(dplyr)
library(rsample)
library(purrr)
library(kknn)

# 1. Fast I/O + minimal preprocessing
cols_needed <- c("crash_speed_limit","road_constr_zone_fl","onsys_fl","crash_fatal_fl")
crash <- vroom("Austin_Crash_Report_Data_-_Crash_Level_Records_20250407.csv.gz",
               col_select = all_of(cols_needed)) %>%
  mutate(
    # outcome as factor
    fatal = factor(crash_fatal_fl, levels = c(FALSE,TRUE),
                   labels = c("NonFatal","Fatal")),
    # predictors as integers 0/1
    road_constr = as.integer(road_constr_zone_fl),
    onsys       = as.integer(onsys_fl)
  ) %>%
  select(crash_speed_limit, road_constr, onsys, fatal)

# 2. 5-fold CV on 10% to choose k
set.seed(123)
crash_small <- crash %>% slice_sample(prop = 0.1)
splits_sm   <- vfold_cv(crash_small, v = 5, strata = fatal)
k_vals      <- 1:10

cv_sm <- map_dfr(k_vals, function(k) {
  accs <- map_dbl(splits_sm$splits, function(sp) {
    tr <- analysis(sp); te <- assessment(sp)
    fit_k <- kknn(fatal ~ ., train = tr, test = te,
                  k = k, kernel = "rectangular")
    mean(fitted(fit_k) == te$fatal)
  })
  tibble(neighbors = k, cv_accuracy = mean(accs))
})

print(cv_sm)
# … after computing cv_sm …

best_k_vec <- cv_sm %>% filter(cv_accuracy == max(cv_accuracy)) %>% pull(neighbors)
best_k     <- min(best_k_vec)           # choose the smallest of the tied k’s
cat("Chosen k =", best_k, "\n\n")

set.seed(1)
split_full <- initial_split(crash, prop = 0.75, strata = fatal)
ctrain     <- training(split_full)
ctest      <- testing(split_full)

final_fit <- kknn(fatal ~ ., train = ctrain, test = ctest,
                  k = best_k, kernel = "rectangular")

test_acc  <- mean(fitted(final_fit) == ctest$fatal)
cat("Test-set accuracy:", round(test_acc, 4), "\n")
```

Which one of your models (i.e. which combination of tuning parameters and variables) produces the best prediction performance on the training dataset?
  
  **The logistic regression model using crash_speed_limit, road_constr_zone_fl, and onsys_fl produced the best performance with a cross-validated accuracy of 99.45%. The K-Nearest Neighbors model, tuned over k = 1 to 10, achieved a maximum accuracy of 99.36% using k = 1. Therefore, the logistic regression model slightly outperformed KNN in this dataset.**
  
  
  ## Final Model Fit
  
  Choose the model that performs the best on your training dataset (either the linear/logistic regression or the machine learning model). Make a final assessment of your model using the testing dataset.


```{r}
## Add your code here
# Final model fit and evaluation on test set
final_fit <- fit(logw, data = ctrain)

# Predict on test set
test_preds <- predict(final_fit, new_data = ctest) %>%
  bind_cols(ctest)

# Accuracy on test set
test_acc <- accuracy(test_preds, truth = fatal, estimate = .pred_class)

# Confusion matrix
test_cm <- conf_mat(test_preds, truth = fatal, estimate = .pred_class)

test_acc
test_cm

```


What are the final performance metrics for your best model?
  
  **Write your answer here.**
  
  
  
  ## Model Improvement
  
  
  Suppose you had the ability to improve the fit of the model by collecting data on a new variable. What data would you most like to collect to improve the prediction performance of the model? Explain why you think collecting this new variable would improve the prediction performance. You may optionally include some code/analysis to support your explanation.

```{r}
## Optional: Add any supporting code here

```

**Write your answer here.**
  
  
  
  
  ## Discussion
  
  Reflect on the process of conducting this project. What was challenging, what have you learned from the process itself? Was there anything that was unexpected that you found during this analysis? If so, what was your expectation and how did the experience deviate from your expectation?
  
  **Write your answer here.**
  
  
  
  
  
  ## Submission to Gradescope
  
  **Make sure to add your group members to the Gradescope submission!**