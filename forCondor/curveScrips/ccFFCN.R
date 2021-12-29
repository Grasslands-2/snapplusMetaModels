#load libraries
library(tidyverse)
library(randomForest)
library(tidymodels)

dat <- read_csv("forCondor/curveData/ccCurve.csv")

dat <- dat %>% 
  mutate_if(is.character, factor) 

#partition data
set.seed(123)
split <- initial_split(dat, strata = ffCN)
train <- training(split)
test <- testing(split)

rec <- recipe(ffCN ~ ., data = train)

prepped <- prep(rec)
juiced <- juice(prepped)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 50,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

tune_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train)

rf_grid <- grid_regular(
  mtry(range = c(3,6)),
  min_n(range = c(3,5)),
  levels = 4
)

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)

metrics <- regular_res %>%
  collect_metrics()
write.csv(metrics, "ContCornFFCNMetrics.csv", row.names = FALSE, quote = FALSE)

#choose best model
best_rmse <- select_best(regular_res, "rmse")

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

mod <- final_rf %>%
  set_engine("randomForest") %>%
  fit(ffCN ~ .,
      data = train
  )

saveRDS(mod, "ContCornFFCN.rds")
