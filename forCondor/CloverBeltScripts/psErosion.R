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


# psR <- full %>%
#   filter(crop == "ps") %>%
#   mutate_if(is.character, as.factor) %>%
#   select(c(Erosion, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k, R_factor)) %>%
#   mutate(Contour = as.factor(Contour)) %>%
#   distinct() %>%
#   droplevels()
# 
# summary(psR)
# 
# #partition data
# set.seed(123)
# splitR <- initial_split(psR, strata = Erosion)
# trainR <- training(splitR)
# testR <- testing(splitR)
# 
# ps_recR <- recipe(Erosion ~ ., data = trainR)
# 
# ps_prepR <- prep(ps_recR)
# juicedR <- juice(ps_prepR)
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
#   add_recipe(ps_recR) %>%
#   add_model(tune_spec)
# 
# # train hyperparameters
# set.seed(234)
# folds <- vfold_cv(trainR)
# 
# rf_grid <- grid_regular(
#   mtry(range = c(3, 7)),
#   min_n(range = c(5, 8)),
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
# write.csv(metricsR, "psErosionMetricsCloverBelt_withR.csv", row.names = FALSE, quote = FALSE)
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
#   fit(Erosion ~ .,
#       data = trainR
#   )
#   
# 
# saveRDS(modR, "psErosionCloverBelt_withR.rds")
# 
# testRMSE_r <- modR %>%
#   predict(testR) %>%
#   bind_cols(testR) 
# 
# RMSE_r <- round(sqrt(mean((testRMSE_r$.pred - testRMSE_r$Erosion)^2)),5)
# 
# print(paste("RMSE with R factor is", RMSE_r))

# without R ---------------------------------------------------------------


ps <- full %>%
  filter(crop == "ps") %>%
  filter(OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

summary(ps)

#partition data
set.seed(123)
split <- initial_split(ps, strata = Erosion)
train <- training(split)
test <- testing(split)

ps_rec <- recipe(Erosion ~ ., data = train)

ps_prep <- prep(ps_rec)
juiced <- juice(ps_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 60,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

tune_wf <- workflow() %>%
  add_recipe(ps_rec) %>%
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
write.csv(metrics, "psErosionMetricsCloverBelt.csv", row.names = FALSE, quote = FALSE)

#choose best model
best_rmse <- select_best(regular_res, "rmse")

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

mod <- final_rf %>%
  set_engine("randomForest", importance = TRUE) %>%
  fit(Erosion ~ .,
      data = train
  )

saveRDS(mod, "psErosionCloverBelt.rds")

testRMSE <- mod %>%
  predict(test) %>%
  bind_cols(test) 

RMSE <- round(sqrt(mean((testRMSE$.pred - testRMSE$Erosion)^2)),5)

print(paste("RMSE without R factor is", RMSE))

