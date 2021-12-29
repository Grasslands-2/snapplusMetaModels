#load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(MASS)
library(gt)

#load data
ps <- read_csv("/Users/elissachasen/Desktop/forCondor/PIdata/ptPI.csv") %>%
  mutate_if(is.character, factor) %>%
  droplevels() %>%
  distinct()


# global settings
train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times


# full model --------------------------------------------------------------

# null hypothesis
best.guess <- round(mean(ps$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = ps$PI, p = 0.7, list = FALSE)
train <- ps[inTrain,]
test <- ps[-inTrain,]

# Evaluate RMSE
RMSE.baseline <- round(sqrt(mean((best.guess-test$PI)^2)),2)

#load models
stepForward <- readRDS("stepAICmodels/ptPI_stepForward.rds")
stepForward$anova

stepForward.model <- train(PI ~ Erosion + density + rotational + slope + initialP + OM + 
                             totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + k + 
                             total.depth + LSsurgo + Erosion:OM + Erosion:initialP + Erosion:totalP2O5_lbs + 
                             Erosion:silt + OM:k + density:slope + OM:slopelenusle.r + 
                             Erosion:total_DM_lbs + density:initialP + initialP:silt + 
                             Erosion:slopelenusle.r + density:k + silt:k + slopelenusle.r:total.depth + 
                             density:slopelenusle.r + initialP:total.depth + OM:silt + 
                             silt:total.depth + initialP:LSsurgo + Erosion:slope + totalP2O5_lbs:silt + 
                             Erosion:LSsurgo + density:LSsurgo + density:totalP2O5_lbs + 
                             OM:total.depth + totalP2O5_lbs:total_DM_lbs + initialP:slopelenusle.r + 
                             OM:LSsurgo + Erosion:density + slope:k + slope:total.depth + 
                             slopelenusle.r:silt + slope:slopelenusle.r + Erosion:k + 
                             Erosion:total.depth + total_DM_lbs:k + OM:total_DM_lbs + 
                             slope:LSsurgo + total.depth:LSsurgo + slope:OM + rotational:OM + 
                             density:silt + density:OM + slope:silt + silt:LSsurgo + initialP:k + 
                             k:total.depth + density:total.depth + total_DM_lbs:total.depth + 
                             slopelenusle.r:LSsurgo + initialP:OM + totalP2O5_lbs:k + 
                             OM:totalP2O5_lbs,
                           data = train, method = "lm", trControl = train.control)
stepForward.model
forward.pred <- predict(stepForward.model, test)
RMSE.forward <- round(sqrt(mean((forward.pred - test$PI)^2)),3)
adj.rsquared.forward <- round((summary(stepForward.model)$adj.r.squared),3)
rsquared.forward <- round((summary(stepForward.model)$r.squared),3)
stepForwardLength <- length(coef(stepForward.model$finalModel))
models <- data.frame(Model = "Step Forward", RMSE = RMSE.forward, Rsquared = rsquared.forward, adj.Rsquared = adj.rsquared.forward, no.coef = stepForwardLength)
models
plot(test$PI ~ forward.pred)


stepBack <- readRDS("stepAICmodels/ptPI_stepBack.rds")
stepBack$anova
stepBack.mod <- train(PI ~Erosion + density + slope + initialP + OM + totalP2O5_lbs + 
                        total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                        LSsurgo + Erosion:density + Erosion:slope + Erosion:initialP + 
                        Erosion:OM + Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + 
                        Erosion:slopelenusle.r + Erosion:silt + Erosion:total.depth + 
                        Erosion:LSsurgo + density:slope + density:initialP + density:OM + 
                        density:total_DM_lbs + density:slopelenusle.r + density:silt + 
                        density:k + density:total.depth + density:LSsurgo + slope:OM + 
                        slope:silt + slope:total.depth + slope:LSsurgo + initialP:OM + 
                        initialP:slopelenusle.r + initialP:silt + initialP:k + initialP:total.depth + 
                        initialP:LSsurgo + OM:totalP2O5_lbs + OM:slopelenusle.r + 
                        OM:silt + OM:k + OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
                        totalP2O5_lbs:k + total_DM_lbs:k + total_DM_lbs:total.depth + 
                        slopelenusle.r:silt + slopelenusle.r:k + slopelenusle.r:total.depth + 
                        slopelenusle.r:LSsurgo + silt:k + silt:total.depth + silt:LSsurgo + 
                        k:total.depth + k:LSsurgo + total.depth:LSsurgo,
                      data = train, method = "lm", trControl = train.control)
stepBack.mod
back.pred <- predict(stepBack.mod, test)
RMSE.back <- round(sqrt(mean((back.pred - test$PI)^2)),3)
adj.rsquared.back <- round((summary(stepBack.mod)$adj.r.squared),3)
rsquared.back <- round((summary(stepBack.mod)$r.squared),3)
stepBackLength <- length(coef(stepBack.mod$finalModel))
models <- add_row(models, Model = "Cover Step Backward", RMSE = RMSE.back, Rsquared = rsquared.back, adj.Rsquared = adj.rsquared.back, 
                  no.coef = stepBackLength)
models
plot(test$PI ~ back.pred) 

saveRDS(stepBack.mod, "models/pasturePI.rds")


# compare to tidymodels ---------------------------------------------------

library(tidymodels)

#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

#estimate or train the model
stepBackTidy <- 
  lm_mod %>% 
  fit(PI ~Erosion + density + slope + initialP + OM + totalP2O5_lbs + 
        total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
        LSsurgo + Erosion:density + Erosion:slope + Erosion:initialP + 
        Erosion:OM + Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + 
        Erosion:slopelenusle.r + Erosion:silt + Erosion:total.depth + 
        Erosion:LSsurgo + density:slope + density:initialP + density:OM + 
        density:total_DM_lbs + density:slopelenusle.r + density:silt + 
        density:k + density:total.depth + density:LSsurgo + slope:OM + 
        slope:silt + slope:total.depth + slope:LSsurgo + initialP:OM + 
        initialP:slopelenusle.r + initialP:silt + initialP:k + initialP:total.depth + 
        initialP:LSsurgo + OM:totalP2O5_lbs + OM:slopelenusle.r + 
        OM:silt + OM:k + OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
        totalP2O5_lbs:k + total_DM_lbs:k + total_DM_lbs:total.depth + 
        slopelenusle.r:silt + slopelenusle.r:k + slopelenusle.r:total.depth + 
        slopelenusle.r:LSsurgo + silt:k + silt:total.depth + silt:LSsurgo + 
        k:total.depth + k:LSsurgo + total.depth:LSsurgo,
      data = train)

tidy(stepBackTidy) %>%
  bind_cols(stepBack.mod$finalModel$coefficients) %>%
  select(-c(std.error:p.value)) %>%
  gt()

saveRDS(stepBackTidy, "tidyLM_PI_pasture.rds")

# random forest without initial P -----------------------------------------
library(tidymodels)

#load data
ps <- read_csv("/Users/elissachasen/Desktop/forCondor/PIdata/ptPI.csv") %>%
  mutate_if(is.character, factor) %>%
  filter(initialP == 25) %>%
  dplyr::select(-initialP) %>%
  droplevels() %>%
  distinct()

#partition data
set.seed(123)
split <- initial_split(ps, strata = PI)
train <- training(split)
test <- testing(split)

dat_rec <- recipe(PI ~ ., data = train)

dat_prep <- prep(dat_rec)
juiced <- juice(dat_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 50,
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
  mtry(range = c(4, 10)),
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
write.csv(metrics, "pastureErosionMetrics.csv", row.names = FALSE, quote = FALSE)

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

mod
pred <- predict(mod, test)
plot(pred$.pred ~ test$PI)

library(vip)

final_rf %>%
  set_engine("randomForest") %>%
  fit(PI ~ .,
      data = train
  ) %>%
  vip(geom = "point")
