# this script created the tidy PI model for pasture seeding in the tainter creek
# it first compares the step back and step forward model outputs
# it also examines the possibility of making a random forest PI model by filtering the data to only 1 initial P value

#load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(MASS)
library(tidymodels)
library(gt)

#load data
ps <- read_csv("/Users/elissachasen/Desktop/forCondor/PIdata/psPI.csv") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour)) %>%
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
stepForward <- readRDS("stepAICmodels/psPI_stepForward.rds")
stepForward$anova

stepForward.model <- train(PI ~ Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
                             total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                             LSsurgo + Erosion:OM + Erosion:initialP + Erosion:totalP2O5_lbs + 
                             tillage:slope + tillage:initialP + Erosion:silt + OM:total.depth + 
                             Erosion:tillage + initialP:LSsurgo + initialP:silt + k:total.depth + 
                             Contour:initialP + Erosion:total_DM_lbs + slope:LSsurgo + 
                             slope:Contour + OM:silt + slopelenusle.r:total.depth + Erosion:total.depth + 
                             initialP:total.depth + silt:k + silt:total.depth + slope:silt + 
                             totalP2O5_lbs:silt + tillage:silt + OM:k + tillage:Contour + 
                             initialP:k + totalP2O5_lbs:LSsurgo + initialP:OM + Contour:k + 
                             Erosion:Contour + slope:initialP + k:LSsurgo + slopelenusle.r:LSsurgo + 
                             slope:slopelenusle.r + Erosion:slopelenusle.r + slope:OM + 
                             tillage:LSsurgo + slopelenusle.r:k + Erosion:slope + silt:LSsurgo + 
                             Erosion:LSsurgo + OM:LSsurgo + initialP:slopelenusle.r + 
                             slope:k + slope:total.depth + initialP:total_DM_lbs + Contour:silt + 
                             Contour:OM + tillage:total_DM_lbs + tillage:OM + tillage:k + 
                             Erosion:k + total.depth:LSsurgo + Contour:LSsurgo + total_DM_lbs:k + 
                             totalP2O5_lbs:total_DM_lbs + slopelenusle.r:silt + Contour:totalP2O5_lbs + 
                             totalP2O5_lbs:slopelenusle.r + OM:total_DM_lbs + totalP2O5_lbs:k + 
                             tillage:total.depth + OM:totalP2O5_lbs + OM:slopelenusle.r,
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


stepBack <- readRDS("stepAICmodels/psPI_stepBack.rds")
stepBack$anova
stepBack.mod <- train(PI ~Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
                        total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                        LSsurgo + Erosion:tillage + Erosion:slope + Erosion:Contour + 
                        Erosion:initialP + Erosion:OM + Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + 
                        Erosion:slopelenusle.r + Erosion:silt + Erosion:k + Erosion:total.depth + 
                        Erosion:LSsurgo + tillage:Contour + tillage:initialP + tillage:OM + 
                        tillage:total_DM_lbs + tillage:slopelenusle.r + tillage:silt + 
                        tillage:k + tillage:total.depth + tillage:LSsurgo + slope:Contour + 
                        slope:initialP + slope:OM + slope:slopelenusle.r + slope:silt + 
                        slope:k + slope:total.depth + slope:LSsurgo + Contour:initialP + 
                        Contour:OM + Contour:totalP2O5_lbs + Contour:silt + Contour:k + 
                        Contour:LSsurgo + initialP:OM + initialP:total_DM_lbs + initialP:slopelenusle.r + 
                        initialP:silt + initialP:k + initialP:total.depth + initialP:LSsurgo + 
                        OM:totalP2O5_lbs + OM:slopelenusle.r + OM:silt + OM:k + OM:total.depth + 
                        OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + totalP2O5_lbs:slopelenusle.r + 
                        totalP2O5_lbs:silt + totalP2O5_lbs:k + totalP2O5_lbs:LSsurgo + 
                        total_DM_lbs:k + slopelenusle.r:silt + slopelenusle.r:k + 
                        slopelenusle.r:total.depth + slopelenusle.r:LSsurgo + silt:k + 
                        silt:total.depth + silt:LSsurgo + k:total.depth + k:LSsurgo + 
                        total.depth:LSsurgo,
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

saveRDS(stepBack.mod, "models/pastureSeedingPI.rds")


# create tidy model -------------------------------------------------------

library(tidymodels)

#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

#estimate or train the model
stepBackTidy <- 
  lm_mod %>% 
  fit(PI ~Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
        total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
        LSsurgo + Erosion:tillage + Erosion:slope + Erosion:Contour + 
        Erosion:initialP + Erosion:OM + Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + 
        Erosion:slopelenusle.r + Erosion:silt + Erosion:k + Erosion:total.depth + 
        Erosion:LSsurgo + tillage:Contour + tillage:initialP + tillage:OM + 
        tillage:total_DM_lbs + tillage:slopelenusle.r + tillage:silt + 
        tillage:k + tillage:total.depth + tillage:LSsurgo + slope:Contour + 
        slope:initialP + slope:OM + slope:slopelenusle.r + slope:silt + 
        slope:k + slope:total.depth + slope:LSsurgo + Contour:initialP + 
        Contour:OM + Contour:totalP2O5_lbs + Contour:silt + Contour:k + 
        Contour:LSsurgo + initialP:OM + initialP:total_DM_lbs + initialP:slopelenusle.r + 
        initialP:silt + initialP:k + initialP:total.depth + initialP:LSsurgo + 
        OM:totalP2O5_lbs + OM:slopelenusle.r + OM:silt + OM:k + OM:total.depth + 
        OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + totalP2O5_lbs:slopelenusle.r + 
        totalP2O5_lbs:silt + totalP2O5_lbs:k + totalP2O5_lbs:LSsurgo + 
        total_DM_lbs:k + slopelenusle.r:silt + slopelenusle.r:k + 
        slopelenusle.r:total.depth + slopelenusle.r:LSsurgo + silt:k + 
        silt:total.depth + silt:LSsurgo + k:total.depth + k:LSsurgo + 
        total.depth:LSsurgo,
      data = train)

tidy(stepBackTidy) %>%
  bind_cols(stepBack.mod$finalModel$coefficients) %>%
  dplyr::select(-c(std.error:p.value)) %>%
  gt()

saveRDS(stepBackTidy, "models/pastureSeedingTidyPI.rds")

# random forest without initial P -----------------------------------------
library(tidymodels)

#load data
ps <- read_csv("/Users/elissachasen/Desktop/forCondor/PIdata/psPI.csv") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour)) %>%
  filter(initialP == 25) %>%
  dplyr::select(-initialP) %>%
  droplevels() %>%
  distinct()

#partition data
set.seed(123)
split <- initial_split(ps, strata = PI)
train <- training(split)
test <- testing(split)

dat_rec <- recipe(PI ~ ., data = train) %>% step_dummy(tillage, Contour)
  
#dummies <- dat_rec %>% step_dummy(tillage, Contour)

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
  fit(PI ~ .,
      data = train
  )

mod
pred <- predict(mod, test)
plot(pred$.pred ~ test$PI)

library(vip)

final_rf %>%
  set_engine("randomForest") %>%
  fit(PI ~ .,
      data = juice(dat_prep)
  ) %>%
  vip(geom = "point")

psWithPred <- mod %>%
  predict(ps) %>%
  bind_cols(ps)

psFull <- read_csv("/Users/elissachasen/Desktop/forCondor/PIdata/psPI.csv") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels() %>%
  distinct()

psFullWithPred <- mod %>%
  predict(psFull) %>%
  bind_cols(psFull) %>%
  mutate(PI25 = .pred) %>%
  dplyr::select(-.pred)


mod1 <- lm(PI ~ Erosion * initialP * PI25, data = psFullWithPred)
summary(mod1)

# null hypothesis
best.guess <- round(mean(psFullWithPred$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = psFullWithPred$PI, p = 0.7, list = FALSE)
train <- psFullWithPred[inTrain,]
test <- psFullWithPred[-inTrain,]

# Evaluate RMSE
RMSE.baseline <- round(sqrt(mean((best.guess-test$PI)^2)),2)

#load models
model <- train(PI ~ Erosion * initialP * PI25 * tillage,
                           data = train, method = "lm", trControl = train.control)
model
pred <- predict(model, test)
RMSE <- round(sqrt(mean((pred - test$PI)^2)),3)
adj.rsquared.forward <- round((summary(stepForward.model)$adj.r.squared),3)
rsquared.forward <- round((summary(model)$r.squared),3)
plot(test$PI ~ pred)

test$pred <- pred
 
lowPI <- subset(test, PI < 10)

ggplot(lowPI, aes(x = PI, y = pred, color = as.factor(initialP), shape = tillage)) + 
  geom_point()

ggplot(psFullWithPred, aes(x = Erosion, y = PI, color = as.factor(initialP), shape = tillage)) + 
  geom_point()
