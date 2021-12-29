#load libraries
library(tidyverse)
library(randomForest)
library(tidymodels)

#load data
dat <- read_csv("dairyRotationNoCoverErosion.csv")

dat <- dat %>% 
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour))

#partition data
set.seed(123)
split <- initial_split(dat, strata = Erosion)
train <- training(split)
test <- testing(split)

dat_rec <- recipe(Erosion ~ ., data = train)

dat_prep <- prep(dat_rec)
juiced <- juice(dat_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 60,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

tune_wf <- workflow() %>%
  add_recipe(dat_rec) %>%
  add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train)

rf_grid <- grid_regular(
  mtry(range = c(3, 7)),
  min_n(range = c(5, 8)),
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
write.csv(metrics, "dairyRotationNoCoverErosionMetrics.csv", row.names = FALSE, quote = FALSE)

#choose best model
best_rmse <- select_best(regular_res, "rmse")

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

mod <- final_rf %>%
  set_engine("randomForest") %>%
  fit(Erosion ~ .,
      data = train
  )

saveRDS(mod, "dairyRotationNoCoverErosion.rds")

