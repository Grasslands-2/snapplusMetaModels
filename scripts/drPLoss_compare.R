# this script creates the tidy model for PI dairy rotation in the tainter creek
# after comparing the step forward and step backward full models with the models subsetted by cover

#load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(MASS)
library(plotly)

#load data
#load data
drcc <- read_csv("forCondor/PIdata/DairyRotCoverPI.csv") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct()
drnc <- read_csv("forCondor/PIdata/DairyRotNoCoverPI.csv") %>%
  mutate(cover = "nc") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct()
dr <- bind_rows(drnc, drcc) %>%
  mutate(tillage = recode(tillage, 
                          "sm" = "sv"))
  


# global settings
train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times


# full model --------------------------------------------------------------

# null hypothesis
dr.best.guess <- round(mean(dr$PI),2)

#partition data
set.seed(123)
dr.inTrain <- createDataPartition(y = dr$PI, p = 0.7, list = FALSE)
dr.train <- dr[dr.inTrain,]
dr.test <- dr[-dr.inTrain,]

# Evaluate RMSE
dr.RMSE.baseline <- round(sqrt(mean((dr.best.guess-dr.test$PI)^2)),2)

#load models
drMod <- readRDS("models/dairyRotationPI.rds")


# with cover --------------------------------------------------------------

# null hypothesis
drcc.best.guess <- round(mean(drcc$PI),2)

#partition data
set.seed(123)
drcc.inTrain <- createDataPartition(y = drcc$PI, p = 0.7, list = FALSE)
drcc.train <- drcc[drcc.inTrain,]
drcc.test <- drcc[-drcc.inTrain,]

# Evaluate RMSE
drcc.RMSE.baseline <- round(sqrt(mean((drcc.best.guess-drcc.test$PI)^2)),2)

#load models
drcc.stepForward <- readRDS("stepAICmodels/dairyRotCoverPI_stepForward.rds")
drcc.stepForward$anova

drcc.stepForward.model <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                                  OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                                  k + total.depth + LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + 
                                  Erosion:initialP + Erosion:silt + OM:k + Erosion:total_DM_lbs + 
                                  slope:totalP2O5_lbs + initialP:silt + OM:slopelenusle.r + 
                                  tillage:slope + totalP2O5_lbs:silt + silt:k + OM:silt + initialP:total.depth + 
                                  initialP:LSsurgo + tillage:initialP + slopelenusle.r:total.depth + 
                                  slopelenusle.r:LSsurgo + tillage:total_DM_lbs + Erosion:total.depth + 
                                  initialP:slopelenusle.r + Erosion:Contour + slope:k + cover:initialP + 
                                  initialP:total_DM_lbs + slope:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
                                  cover:totalP2O5_lbs + Contour:initialP + OM:LSsurgo + slope:OM + 
                                  OM:totalP2O5_lbs + total_DM_lbs:k + total_DM_lbs:total.depth + 
                                  slope:total.depth + slopelenusle.r:k + cover:slope + Erosion:tillage + 
                                  tillage:OM + tillage:slopelenusle.r + Erosion:slopelenusle.r + 
                                  Erosion:slope + Erosion:LSsurgo + tillage:LSsurgo + Contour:slopelenusle.r + 
                                  Contour:LSsurgo + Contour:total_DM_lbs + totalP2O5_lbs:k + 
                                  cover:tillage + total.depth:LSsurgo + k:LSsurgo + Contour:OM + 
                                  tillage:totalP2O5_lbs + initialP:OM + initialP:k + tillage:Contour + 
                                  Erosion:cover + OM:total.depth + k:total.depth + cover:slopelenusle.r + 
                                  silt:total.depth + Erosion:k + OM:total_DM_lbs + initialP:totalP2O5_lbs + 
                                  totalP2O5_lbs:LSsurgo + totalP2O5_lbs:slopelenusle.r + cover:k + 
                                  tillage:total.depth + tillage:silt + totalP2O5_lbs:total.depth + 
                                  slope:total_DM_lbs + cover:total_DM_lbs + slope:slopelenusle.r + 
                                  cover:OM + Contour:k + cover:Contour,
                                 data = drcc.train, method = "lm", trControl = train.control)
drcc.stepForward.model
drcc.forward.pred <- predict(drcc.stepForward.model, drcc.test)
drcc.RMSE.forward <- round(sqrt(mean((drcc.forward.pred - drcc.test$PI)^2)),3)
drcc.adj.rsquared.forward <- round((summary(drcc.stepForward.model)$adj.r.squared),3)
drcc.rsquared.forward <- round((summary(drcc.stepForward.model)$r.squared),3)
drcc.stepForwardLength <- length(coef(drcc.stepForward.model$finalModel))
models <- data.frame(Model = "Cover Step Forward", RMSE = drcc.RMSE.forward, Rsquared = drcc.rsquared.forward, adj.Rsquared = drcc.adj.rsquared.forward, no.coef = drcc.stepForwardLength)
models
plot(drcc.test$PI ~ drcc.forward.pred)


drcc.stepBack <- readRDS("stepAICmodels/dairyRotCoverPI_stepBack.rds")
drcc.stepBack$anova
drcc.stepBack.mod <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                             OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                             k + total.depth + LSsurgo + Erosion:cover + Erosion:tillage + 
                             Erosion:slope + Erosion:Contour + Erosion:initialP + Erosion:OM + 
                             Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + Erosion:slopelenusle.r + 
                             Erosion:silt + Erosion:total.depth + Erosion:LSsurgo + cover:tillage + 
                             cover:Contour + cover:initialP + cover:OM + cover:totalP2O5_lbs + 
                             cover:total_DM_lbs + cover:slopelenusle.r + cover:k + cover:total.depth + 
                             cover:LSsurgo + tillage:slope + tillage:Contour + tillage:initialP + 
                             tillage:OM + tillage:totalP2O5_lbs + tillage:total_DM_lbs + 
                             tillage:slopelenusle.r + tillage:k + tillage:total.depth + 
                             tillage:LSsurgo + slope:OM + slope:total_DM_lbs + slope:silt + 
                             slope:total.depth + slope:LSsurgo + Contour:initialP + Contour:OM + 
                             Contour:total_DM_lbs + Contour:slopelenusle.r + Contour:k + 
                             Contour:total.depth + Contour:LSsurgo + initialP:OM + initialP:totalP2O5_lbs + 
                             initialP:total_DM_lbs + initialP:slopelenusle.r + initialP:silt + 
                             initialP:k + initialP:total.depth + initialP:LSsurgo + OM:totalP2O5_lbs + 
                             OM:total_DM_lbs + OM:slopelenusle.r + OM:silt + OM:k + OM:total.depth + 
                             OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + totalP2O5_lbs:slopelenusle.r + 
                             totalP2O5_lbs:silt + totalP2O5_lbs:k + totalP2O5_lbs:total.depth + 
                             totalP2O5_lbs:LSsurgo + total_DM_lbs:slopelenusle.r + total_DM_lbs:silt + 
                             total_DM_lbs:k + total_DM_lbs:total.depth + total_DM_lbs:LSsurgo + 
                             slopelenusle.r:silt + slopelenusle.r:total.depth + slopelenusle.r:LSsurgo + 
                             silt:k + silt:total.depth + silt:LSsurgo + k:total.depth + 
                             k:LSsurgo + total.depth:LSsurgo,
                            data = drcc.train, method = "lm", trControl = train.control)
drcc.stepBack.mod
drcc.back.pred <- predict(drcc.stepBack.mod, drcc.test)
drcc.RMSE.back <- round(sqrt(mean((drcc.back.pred - drcc.test$PI)^2)),3)
drcc.adj.rsquared.back <- round((summary(drcc.stepBack.mod)$adj.r.squared),3)
drcc.rsquared.back <- round((summary(drcc.stepBack.mod)$r.squared),3)
drcc.stepBackLength <- length(coef(drcc.stepBack.mod$finalModel))
models <- add_row(models, Model = "Cover Step Backward", RMSE = drcc.RMSE.back, Rsquared = drcc.rsquared.back, adj.Rsquared = drcc.adj.rsquared.back, 
                  no.coef = drcc.stepBackLength)
models
plot(drcc.test$PI ~ drcc.back.pred) 


# no cover ----------------------------------------------------------------

# null hypothesis
drnc.best.guess <- round(mean(drnc$PI),2)

#partition data
set.seed(123)
drnc.inTrain <- createDataPartition(y = drnc$PI, p = 0.7, list = FALSE)
drnc.train <- drnc[drnc.inTrain,]
drnc.test <- drnc[-drnc.inTrain,]

# Evaluate RMSE
drnc.RMSE.baseline <- round(sqrt(mean((drnc.best.guess-drnc.test$PI)^2)),2)

#load models
drnc.stepForward <- readRDS("stepAICmodels/dairyRotNoCoverPI_stepForward.rds")
drnc.stepForward$anova

drnc.stepForward.model <- train(PI ~ Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
                                  total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                                  LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + Erosion:initialP + 
                                  Erosion:silt + Erosion:tillage + OM:k + Erosion:total_DM_lbs + 
                                  tillage:initialP + initialP:LSsurgo + initialP:silt + totalP2O5_lbs:silt + 
                                  OM:silt + tillage:k + slopelenusle.r:LSsurgo + tillage:LSsurgo + 
                                  initialP:total.depth + totalP2O5_lbs:LSsurgo + tillage:total.depth + 
                                  Erosion:Contour + Contour:initialP + initialP:total_DM_lbs + 
                                  k:LSsurgo + silt:k + slopelenusle.r:total.depth + slope:total.depth + 
                                  tillage:total_DM_lbs + tillage:silt + tillage:slopelenusle.r + 
                                  Erosion:slope + slope:initialP + Erosion:total.depth + OM:total.depth + 
                                  totalP2O5_lbs:total_DM_lbs + tillage:Contour + total_DM_lbs:k + 
                                  OM:total_DM_lbs + initialP:slopelenusle.r + initialP:k + 
                                  initialP:OM + slopelenusle.r:k + slope:k + k:total.depth + 
                                  OM:LSsurgo + slope:OM + slope:LSsurgo + total.depth:LSsurgo + 
                                  silt:total.depth + tillage:totalP2O5_lbs + Contour:slopelenusle.r + 
                                  Contour:LSsurgo + Contour:k + total_DM_lbs:silt + totalP2O5_lbs:slopelenusle.r + 
                                  total_DM_lbs:total.depth + slope:totalP2O5_lbs + slope:slopelenusle.r + 
                                  Contour:OM + OM:totalP2O5_lbs + totalP2O5_lbs:k + silt:LSsurgo + 
                                  slope:silt + slopelenusle.r:silt + Erosion:slopelenusle.r + 
                                  Erosion:LSsurgo + tillage:slope + OM:slopelenusle.r + slope:total_DM_lbs + 
                                  tillage:OM + Erosion:k + Contour:silt + slope:Contour + initialP:totalP2O5_lbs,
                                 data = drnc.train, method = "lm", trControl = train.control)
drnc.stepForward.model
drnc.forward.pred <- predict(drnc.stepForward.model, drnc.test)
drnc.RMSE.forward <- round(sqrt(mean((drnc.forward.pred - drnc.test$PI)^2)),3)
drnc.adj.rsquared.forward <- round((summary(drnc.stepForward.model)$adj.r.squared),3)
drnc.rsquared.forward <- round((summary(drnc.stepForward.model)$r.squared),3)
drnc.stepForwardLength <- length(coef(drnc.stepForward.model$finalModel))
models <- add_row(models, Model = "No Cover Step Forward", RMSE = drnc.RMSE.forward, Rsquared = drnc.rsquared.forward, adj.Rsquared = drnc.adj.rsquared.forward, no.coef = drnc.stepForwardLength)
models
plot(drnc.test$PI ~ drnc.forward.pred)


drnc.stepBack <- readRDS("stepAICmodels/dairyRotNoCoverPI_stepBack.rds")
drnc.stepBack$anova
drnc.stepBack.mod <- train(PI ~ Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
                             total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                             LSsurgo + Erosion:tillage + Erosion:slope + Erosion:Contour + 
                             Erosion:initialP + Erosion:OM + Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + 
                             Erosion:slopelenusle.r + Erosion:silt + Erosion:k + Erosion:total.depth + 
                             Erosion:LSsurgo + tillage:slope + tillage:Contour + tillage:initialP + 
                             tillage:OM + tillage:totalP2O5_lbs + tillage:total_DM_lbs + 
                             tillage:slopelenusle.r + tillage:silt + tillage:total.depth + 
                             tillage:LSsurgo + slope:Contour + slope:initialP + slope:OM + 
                             slope:totalP2O5_lbs + slope:total_DM_lbs + slope:slopelenusle.r + 
                             slope:silt + slope:k + slope:total.depth + slope:LSsurgo + 
                             Contour:initialP + Contour:OM + Contour:slopelenusle.r + 
                             Contour:silt + Contour:LSsurgo + initialP:OM + initialP:totalP2O5_lbs + 
                             initialP:total_DM_lbs + initialP:slopelenusle.r + initialP:silt + 
                             initialP:k + initialP:total.depth + initialP:LSsurgo + OM:totalP2O5_lbs + 
                             OM:total_DM_lbs + OM:slopelenusle.r + OM:silt + OM:k + OM:total.depth + 
                             OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + totalP2O5_lbs:slopelenusle.r + 
                             totalP2O5_lbs:silt + totalP2O5_lbs:k + totalP2O5_lbs:LSsurgo + 
                             total_DM_lbs:k + total_DM_lbs:total.depth + slopelenusle.r:silt + 
                             slopelenusle.r:k + slopelenusle.r:total.depth + slopelenusle.r:LSsurgo + 
                             silt:k + silt:total.depth + silt:LSsurgo + k:total.depth + 
                             total.depth:LSsurgo,
                            data = drnc.train, method = "lm", trControl = train.control)
drnc.stepBack.mod
drnc.back.pred <- predict(drnc.stepBack.mod, drnc.test)
drnc.RMSE.back <- round(sqrt(mean((drnc.back.pred - drnc.test$PI)^2)),3)
drnc.adj.rsquared.back <- round((summary(drnc.stepBack.mod)$adj.r.squared),3)
drnc.rsquared.back <- round((summary(drnc.stepBack.mod)$r.squared),3)
drnc.stepBackLength <- length(coef(drnc.stepBack.mod$finalModel))
models <- add_row(models, Model = "Cover Step Backward", RMSE = drnc.RMSE.back, Rsquared = drnc.rsquared.back, adj.Rsquared = drnc.adj.rsquared.back, 
                  no.coef = drnc.stepBackLength)
models
plot(drnc.test$PI ~ drnc.back.pred) 



# predictions -------------------------------------------------------------

drPred <- predict(drMod, dr)
dr$pred <- drPred
drccPred <- predict(drcc.stepBack.mod, drcc)
drcc$CoverPred <- drccPred
drncPred <- predict(drnc.stepBack.mod, drnc)
drnc$CoverPred <- drncPred

# bind cover data sets
coverSet <- bind_rows(drcc, drnc)

# join cc and coverSet
drWithPred <- left_join(dr, coverSet)

ggplot(drWithPred, aes(x = pred, y = CoverPred, color = cover)) +
  geom_point()

lowPI <- drWithPred %>% filter(PI < 5)

ggplot(lowPI, aes(x = pred, y = CoverPred, color = cover)) +
  geom_point()

fig <- plot_ly(drWithPred, x = ~pred, y = ~CoverPred, z = ~PI, color = ~cover, hoverinfo = "text",
               text = ~paste('full pred:', pred, '<br>subset pred:', CoverPred, '<br>PI:', PI))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Full data prediction'),
                                   yaxis = list(title = 'Subset data prediction'),
                                   zaxis = list(title = 'SnapPlus PI')))
fig

fig2 <- plot_ly(lowPI, x = ~pred, y = ~CoverPred, z = ~PI, color = ~cover, hoverinfo = "text",
                text = ~paste('full pred:', pred, '<br>subset pred:', CoverPred, '<br>PI:', PI))
fig2 <- fig2 %>% add_markers()
fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = 'Full data prediction'),
                                     yaxis = list(title = 'Subset data prediction'),
                                     zaxis = list(title = 'SnapPlus PI')))
fig2



# create tidy model -------------------------------------------------------

library(tidymodels)

# this model froms from the script: dairyRotation_PI.R and uses the step forward AIC model which
# was saved as the dairyRotationPI.rds
stepForward.model <- train(PI ~ Erosion + cover + tillage + slope + Contour + total_DM_lbs + 
                             slopelenusle.r + silt + k + OM + initialP + totalP2O5_lbs + 
                             total.depth + LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + 
                             Erosion:initialP + Erosion:silt + k:OM + Erosion:tillage + 
                             Erosion:total_DM_lbs + tillage:initialP + silt:totalP2O5_lbs + 
                             silt:OM + initialP:LSsurgo + silt:initialP + silt:k + slope:totalP2O5_lbs + 
                             slope:silt + initialP:total.depth + slopelenusle.r:LSsurgo + 
                             tillage:slope + slopelenusle.r:OM + slopelenusle.r:total.depth + 
                             Erosion:total.depth + cover:initialP + tillage:total_DM_lbs + 
                             Erosion:Contour + Contour:initialP + total_DM_lbs:initialP + 
                             Erosion:slope + tillage:slopelenusle.r + total_DM_lbs:totalP2O5_lbs + 
                             silt:total.depth + Erosion:slopelenusle.r + tillage:silt + 
                             tillage:OM + tillage:Contour + Erosion:LSsurgo + silt:LSsurgo + 
                             slopelenusle.r:silt + OM:LSsurgo + slope:OM + cover:slopelenusle.r + 
                             cover:totalP2O5_lbs + totalP2O5_lbs:total.depth + total_DM_lbs:k + 
                             total_DM_lbs:OM + tillage:total.depth + tillage:LSsurgo + 
                             slope:LSsurgo + k:LSsurgo + tillage:totalP2O5_lbs + Erosion:cover + 
                             cover:tillage + Erosion:k + OM:total.depth + k:total.depth + 
                             Contour:slopelenusle.r + Contour:LSsurgo + Contour:OM + k:initialP + 
                             OM:initialP + slope:total.depth + total.depth:LSsurgo + cover:LSsurgo + 
                             total_DM_lbs:silt + Contour:silt + totalP2O5_lbs:LSsurgo + 
                             slopelenusle.r:totalP2O5_lbs + OM:totalP2O5_lbs + k:totalP2O5_lbs + 
                             Contour:total_DM_lbs + slopelenusle.r:k + slopelenusle.r:initialP + 
                             slope:initialP + total_DM_lbs:total.depth + initialP:totalP2O5_lbs + 
                             slope:k + cover:Contour + slope:slopelenusle.r + cover:total.depth,
                           data = dr.train, method = "lm", trControl = train.control)

#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

#estimate or train the model
stepForwardTidy <- 
  lm_mod %>% 
  fit(PI ~ Erosion + cover + tillage + slope + Contour + total_DM_lbs + 
        slopelenusle.r + silt + k + OM + initialP + totalP2O5_lbs + 
        total.depth + LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + 
        Erosion:initialP + Erosion:silt + k:OM + Erosion:tillage + 
        Erosion:total_DM_lbs + tillage:initialP + silt:totalP2O5_lbs + 
        silt:OM + initialP:LSsurgo + silt:initialP + silt:k + slope:totalP2O5_lbs + 
        slope:silt + initialP:total.depth + slopelenusle.r:LSsurgo + 
        tillage:slope + slopelenusle.r:OM + slopelenusle.r:total.depth + 
        Erosion:total.depth + cover:initialP + tillage:total_DM_lbs + 
        Erosion:Contour + Contour:initialP + total_DM_lbs:initialP + 
        Erosion:slope + tillage:slopelenusle.r + total_DM_lbs:totalP2O5_lbs + 
        silt:total.depth + Erosion:slopelenusle.r + tillage:silt + 
        tillage:OM + tillage:Contour + Erosion:LSsurgo + silt:LSsurgo + 
        slopelenusle.r:silt + OM:LSsurgo + slope:OM + cover:slopelenusle.r + 
        cover:totalP2O5_lbs + totalP2O5_lbs:total.depth + total_DM_lbs:k + 
        total_DM_lbs:OM + tillage:total.depth + tillage:LSsurgo + 
        slope:LSsurgo + k:LSsurgo + tillage:totalP2O5_lbs + Erosion:cover + 
        cover:tillage + Erosion:k + OM:total.depth + k:total.depth + 
        Contour:slopelenusle.r + Contour:LSsurgo + Contour:OM + k:initialP + 
        OM:initialP + slope:total.depth + total.depth:LSsurgo + cover:LSsurgo + 
        total_DM_lbs:silt + Contour:silt + totalP2O5_lbs:LSsurgo + 
        slopelenusle.r:totalP2O5_lbs + OM:totalP2O5_lbs + k:totalP2O5_lbs + 
        Contour:total_DM_lbs + slopelenusle.r:k + slopelenusle.r:initialP + 
        slope:initialP + total_DM_lbs:total.depth + initialP:totalP2O5_lbs + 
        slope:k + cover:Contour + slope:slopelenusle.r + cover:total.depth,
      data = dr.train)

library(gt)

tidy(stepForwardTidy) %>%
  bind_cols(stepForward.model$finalModel$coefficients) %>%
  dplyr::select(-c(std.error:p.value)) %>%
  gt()

saveRDS(stepForwardTidy, "models/dairyRot_tidyPI.rds")
