#load libraries
library(tidyverse)
library(randomForest)
library(tidymodels)

#load data
clark <- read_csv("../../clarkJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
#head(clark)
mara <- read_csv("../../maraJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
#head(mara)
taylor <- read_csv("../../taylorJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
# 
full <- bind_rows(clark, mara, taylor)
#head(full)

# with R factor -----------------------------------------------------------


# ptR <- full %>%
#   filter(crop == "pt") %>%
#   mutate_if(is.character, as.factor) %>%
#   select(c(ffCN, rotational, density, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
#   distinct() %>%
#   droplevels()
# 
# summary(ptR)
# 
# #partition data
# set.seed(123)
# splitR <- initial_split(ptR, strata = ffCN)
# trainR <- training(splitR)
# testR <- testing(splitR)
# 
# pt_recR <- recipe(ffCN ~ ., data = trainR)
# 
# pt_prepR <- prep(pt_recR)
# juicedR <- juice(pt_prepR)
# 
# tune_spec <- rand_forest(
#   mtry = tune(),
#   trees = 60,
#   min_n = tune()
# ) %>%
#   set_mode("regression") %>%
#   set_engine("randomForest")
# 
# tune_wf <- workflow() %>%
#   add_recipe(pt_recR) %>%
#   add_model(tune_spec)
# 
# # train hyperparameters
# set.seed(234)
# folds <- vfold_cv(trainR)
# 
# rf_grid <- grid_regular(
#   mtry(range = c(3, 6)),
#   min_n(range = c(3,5)),
#   levels = 4
# )
# 
# set.seed(456)
# regular_resR <- tune_grid(
#   tune_wf,
#   resamples = folds,
#   grid = rf_grid
# )
# 
# metricsR <- regular_resR %>%
#   collect_metrics()
# write.csv(metricsR, "ptFFCN_MetricsCloverBelt_withR.csv", row.names = FALSE, quote = FALSE)
# 
# #choose best model
# best_rmseR <- select_best(regular_resR, "rmse")
# 
# final_rfR <- finalize_model(
#   tune_spec,
#   best_rmseR
# )
# 
# modR <- final_rfR %>%
#   set_engine("randomForest", importance = TRUE) %>%
#   fit(ffCN ~ .,
#       data = trainR
#   )
# 
# 
# saveRDS(modR, "ptFFCN_CloverBelt_withR.rds")
# 
# testRMSE_r <- modR %>%
#   predict(testR) %>%
#   bind_cols(testR) 
# 
# RMSE_r <- round(sqrt(mean((testRMSE_r$.pred - testRMSE_r$ffCN)^2)),5)
# 
# print(paste("RMSE with R factor is", RMSE_r))

# without R ---------------------------------------------------------------


pt <- full %>%
  filter(crop == "pt", 
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN,rotational, density, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
  droplevels()

summary(pt)

#partition data
set.seed(123)
split <- initial_split(pt, strata = ffCN)
train <- training(split)
test <- testing(split)

pt_rec <- recipe(ffCN ~ ., data = train)

pt_prep <- prep(pt_rec)
juiced <- juice(pt_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 60,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

tune_wf <- workflow() %>%
  add_recipe(pt_rec) %>%
  add_model(tune_spec)

# train hyperparameters
set.seed(234)
folds <- vfold_cv(train)

rf_grid <- grid_regular(
  mtry(range = c(3, 6)),
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
write.csv(metrics, "ptFFCN_MetricsCloverBelt.csv", row.names = FALSE, quote = FALSE)

#choose best model
best_rmse <- select_best(regular_res, "rmse")

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

mod <- final_rf %>%
  set_engine("randomForest", importance = TRUE) %>%
  fit(ffCN ~ .,
      data = train
  )

saveRDS(mod, "ptFFCN_CloverBelt.rds")

testRMSE <- mod %>%
  predict(test) %>%
  bind_cols(test) 

RMSE <- round(sqrt(mean((testRMSE$.pred - testRMSE$ffCN)^2)),5)

print(paste("RMSE without R factor is", RMSE))