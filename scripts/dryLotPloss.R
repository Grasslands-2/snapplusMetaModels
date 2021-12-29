# this script creates the tidy model for PI dry lot in the tainter creek
# after comparing the step forward and step backward full models with the models

#load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(MASS)

#load data
dl <- read_csv("/Users/elissachasen/Desktop/forCondor/PIdata/dlPI.csv") %>%
  mutate_if(is.character, factor) %>%
  droplevels() %>%
  distinct()


# global settings
train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times


# full model --------------------------------------------------------------

# null hypothesis
best.guess <- round(mean(dl$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = dl$PI, p = 0.7, list = FALSE)
train <- dl[inTrain,]
test <- dl[-inTrain,]

# Evaluate RMSE
RMSE.baseline <- round(sqrt(mean((best.guess-test$PI)^2)),2)

#load models
stepForward <- readRDS("stepAICmodels/dlPI_stepForward.rds")
stepForward$anova

stepForward.model <- train(PI ~ Erosion + density + slope + initialP + OM + totalP2O5_lbs + 
                             total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                             LSsurgo + Erosion:density + Erosion:OM + Erosion:initialP + 
                             OM:k + slope:silt + slopelenusle.r:LSsurgo + silt:k + density:initialP + 
                             slope:k + initialP:LSsurgo + initialP:silt + silt:total.depth + 
                             slopelenusle.r:total.depth + OM:silt + OM:LSsurgo + density:slope + 
                             slopelenusle.r:silt + Erosion:total.depth + Erosion:slope + 
                             Erosion:LSsurgo + k:LSsurgo + slope:OM + OM:totalP2O5_lbs + 
                             density:total.depth + OM:total.depth + slope:slopelenusle.r + 
                             silt:LSsurgo + Erosion:silt + Erosion:k,
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


stepBack <- readRDS("stepAICmodels/dlPI_stepBack.rds")
stepBack$anova
stepBack.mod <- train(PI ~Erosion + density + slope + initialP + OM + slopelenusle.r + 
                        silt + k + total.depth + LSsurgo + Erosion:density + Erosion:slope + 
                        Erosion:initialP + Erosion:OM + Erosion:silt + Erosion:k + 
                        Erosion:total.depth + Erosion:LSsurgo + density:slope + density:initialP + 
                        density:OM + slope:OM + slope:silt + slope:total.depth + 
                        initialP:silt + initialP:LSsurgo + OM:silt + OM:k + OM:total.depth + 
                        OM:LSsurgo + slopelenusle.r:silt + slopelenusle.r:total.depth + 
                        slopelenusle.r:LSsurgo + silt:k + silt:total.depth + silt:LSsurgo + 
                        total.depth:LSsurgo,
                      data = train, method = "lm", trControl = train.control)
stepBack.mod
back.pred <- predict(stepBack.mod, test)
RMSE.back <- round(sqrt(mean((back.pred - test$PI)^2)),3)
adj.rsquared.back <- round((summary(stepBack.mod)$adj.r.squared),3)
rsquared.back <- round((summary(stepBack.mod)$r.squared),3)
stepBackLength <- length(coef(stepBack.mod$finalModel))
models <- add_row(models, Model = "Step Backward", RMSE = RMSE.back, Rsquared = rsquared.back, adj.Rsquared = adj.rsquared.back, 
                  no.coef = stepBackLength)
models
plot(test$PI ~ back.pred) 

saveRDS(stepBack.mod, "models/dryLotPI.rds")

# create tidy model -------------------------------------------------------

library(tidymodels)

#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

#estimate or train the model
stepBackTidy <- 
  lm_mod %>% 
  fit(PI ~ Erosion + density + slope + initialP + OM + slopelenusle.r + 
        silt + k + total.depth + LSsurgo + Erosion:density + Erosion:slope + 
        Erosion:initialP + Erosion:OM + Erosion:silt + Erosion:k + 
        Erosion:total.depth + Erosion:LSsurgo + density:slope + density:initialP + 
        density:OM + slope:OM + slope:silt + slope:total.depth + 
        initialP:silt + initialP:LSsurgo + OM:silt + OM:k + OM:total.depth + 
        OM:LSsurgo + slopelenusle.r:silt + slopelenusle.r:total.depth + 
        slopelenusle.r:LSsurgo + silt:k + silt:total.depth + silt:LSsurgo + 
        total.depth:LSsurgo,
      data = train)

tidy(stepBackTidy) %>%
  bind_cols(stepBack.mod$finalModel$coefficients) %>%
  dplyr::select(-c(std.error:p.value)) %>%
  gt()

saveRDS(stepBackTidy, "models/DryLot_tidyPI.rds")

