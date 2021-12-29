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


# drR <- full %>%
#   filter(crop == "dr") %>%
#   mutate_if(is.character, as.factor) %>%
#   select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k, R_factor)) %>%
#   mutate(Contour = as.factor(Contour),
#          tillage = recode(tillage, 
#                           "sm" = "sv")) %>%
#   distinct() %>%
#   droplevels()
# 
# summary(drR)
# 
# #partition data
# set.seed(123)
# splitR <- initial_split(drR, strata = Erosion)
# trainR <- training(splitR)
# testR <- testing(splitR)
# 
# dr_recR <- recipe(Erosion ~ ., data = trainR)
# 
# dr_prepR <- prep(dr_recR)
# juicedR <- juice(dr_prepR)
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
#   add_recipe(dr_recR) %>%
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
# write.csv(metricsR, "drErosionMetricsCloverBelt_withR.csv", row.names = FALSE, quote = FALSE)
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
# saveRDS(modR, "drErosionCloverBelt_withR.rds")
# 
# testRMSE_r <- modR %>%
#   predict(testR) %>%
#   bind_cols(testR) 
# 
# RMSE_r <- round(sqrt(mean((testRMSE_r$.pred - testRMSE_r$Erosion)^2)),5)
# 
# print(paste("RMSE with R factor is", RMSE_r))

# without R ---------------------------------------------------------------


dr <- full %>%
  filter(crop == "dr") %>%
  filter(OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour),
         tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  distinct() %>%
  droplevels()

summary(dr)

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
write.csv(metrics, "drErosionMetricsCloverBelt.csv", row.names = FALSE, quote = FALSE)

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

saveRDS(mod, "drErosionCloverBelt.rds")

testRMSE <- mod %>%
  predict(test) %>%
  bind_cols(test) 

RMSE <- round(sqrt(mean((testRMSE$.pred - testRMSE$Erosion)^2)),5)

print(paste("RMSE without R factor is", RMSE))

