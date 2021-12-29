#load libraries
library(tidyverse)
library(randomForest)
library(tidymodels)

#load data
dr <- read_csv("dairyRotationCoverCropErosion.csv")

dr <- dr %>% 
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour))

#partition data
set.seed(123)
split <- initial_split(dr, strata = Erosion)
train <- training(split)
test <- testing(split)

dr_rec <- recipe(Erosion ~ ., data = train)

dr_prep <- prep(dr_rec)
juiced <- juice(dr_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 60,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

tune_wf <- workflow() %>%
  add_recipe(dr_rec) %>%
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
write.csv(metrics, "drccErosionMetrics.csv", row.names = FALSE, quote = FALSE)

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

saveRDS(mod, "drccErosion.rds")

