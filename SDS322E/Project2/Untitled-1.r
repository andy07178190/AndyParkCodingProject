
## Load package and compare outcome variable 
library(tidyverse)
library(tidymodels)
library(readr)

crash <- read_csv("Austin_Crash_Report_Data_-_Crash_Level_Records_20250407.csv.gz")

crash <- crash %>%
  mutate(fatal = fct_recode(as_factor(crash_fatal_fl), "NonFatal" = "FALSE", "Fatal" = "TRUE"))
```


## Base Prediction Model
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

## Alternate Prediction Model
rf_recipe <- recipe(fatal ~ crash_speed_limit + road_constr_zone_fl + onsys_fl, 
                    data = ctrain) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())    # convert factors to dummies
rf_spec <- rand_forest(
    mode = "classification",
    trees = 1000,
    mtry  = tune(),
    min_n = tune()
  ) %>%
  set_engine("ranger", importance = "impurity")
rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_spec)
rf_grid <- grid_regular(
  mtry(range = c(1, 3)),       # if you have only 3 predictors
  min_n(range = c(2, 20)),
  levels = 5
)
set.seed(2)
rf_tune <- tune_grid(
  rf_workflow,
  resamples = cfolds,
  grid      = rf_grid,
  metrics   = metric_set(accuracy),
  control   = control_grid(save_pred = TRUE)
)

# View top results
rf_tune %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  arrange(desc(mean)) %>%
  slice_head(n = 5)
best_params <- rf_tune %>%
  select_best(metric = "accuracy")

final_rf <- rf_workflow %>%
  finalize_workflow(best_params) %>%
  fit(data = ctrain)
rf_preds <- predict(final_rf, ctest) %>%
  bind_cols(ctest %>% select(fatal))

rf_acc <- rf_preds %>%
  metrics(truth = fatal, estimate = .pred_class) %>%
  filter(.metric == "accuracy")

rf_acc
`