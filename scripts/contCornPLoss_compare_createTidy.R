# this script creates the tidy model for PI continuous corn in the tainter creek
# after comparing the step forward and step backward full models with the models subsetted by cover

#load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(MASS)
library(plotly)

#load data
#load data
cc <- read_csv("forCondor/PIdata/ccPI.csv") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct()
ccNc <- cc %>%
  filter(cover == "nc")
ccCc <- cc %>%
  filter(cover == "cc" | cover == "gcis" | cover == "gcds")

# global settings
train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times


# full model --------------------------------------------------------------

# null hypothesis
cc.best.guess <- round(mean(cc$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cc$PI, p = 0.7, list = FALSE)
train <- cc[inTrain,]
test <- cc[-inTrain,]

# Evaluate RMSE
cc.RMSE.baseline <- round(sqrt(mean((cc.best.guess-test$PI)^2)),2)

#load models
stepForward <- readRDS("stepAICmodels/ccPI_stepForward.rds")
stepForward$anova

stepForward.model <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                             OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                             k + total.depth + LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + 
                             Erosion:initialP + Erosion:silt + Erosion:tillage + tillage:initialP + 
                             Erosion:total_DM_lbs + OM:k + totalP2O5_lbs:silt + OM:silt + 
                             initialP:LSsurgo + initialP:silt + cover:initialP + tillage:total_DM_lbs + 
                             silt:k + slope:slopelenusle.r + slopelenusle.r:total.depth + 
                             initialP:total.depth + Contour:initialP + cover:tillage + 
                             Erosion:total.depth + OM:slopelenusle.r + tillage:LSsurgo + 
                             cover:slope + totalP2O5_lbs:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
                             tillage:silt + silt:total.depth + slope:LSsurgo + OM:LSsurgo + 
                             Contour:total_DM_lbs + initialP:total_DM_lbs + slope:OM + 
                             tillage:totalP2O5_lbs + total_DM_lbs:k + OM:total_DM_lbs + 
                             cover:total_DM_lbs + tillage:slopelenusle.r + Erosion:slopelenusle.r + 
                             Erosion:Contour + Erosion:cover + slope:k + tillage:total.depth + 
                             total_DM_lbs:total.depth + tillage:Contour + initialP:OM + 
                             initialP:k + slope:total.depth + slopelenusle.r:k + k:LSsurgo + 
                             total.depth:LSsurgo + Contour:slopelenusle.r + Contour:OM + 
                             Contour:LSsurgo + silt:LSsurgo + cover:Contour + OM:total.depth + 
                             k:total.depth + OM:totalP2O5_lbs + totalP2O5_lbs:k + tillage:OM + 
                             initialP:slopelenusle.r + slope:initialP + slope:silt + slopelenusle.r:silt + 
                             slopelenusle.r:LSsurgo + cover:slopelenusle.r + Erosion:slope + 
                             Erosion:LSsurgo + tillage:slope + Erosion:k + cover:LSsurgo + 
                             cover:OM + Contour:k + cover:silt + tillage:k + cover:totalP2O5_lbs + 
                             totalP2O5_lbs:total.depth + slope:totalP2O5_lbs + totalP2O5_lbs:slopelenusle.r + 
                             initialP:totalP2O5_lbs + Contour:total.depth,
                           data = train, method = "lm", trControl = train.control)
stepForward.model
forward.pred <- predict(stepForward.model, cc.test)
RMSE.forward <- round(sqrt(mean((forward.pred - cc.test$PI)^2)),3)
adj.rsquared.forward <- round((summary(stepForward.model)$adj.r.squared),3)
rsquared.forward <- round((summary(stepForward.model)$r.squared),3)
stepForwardLength <- length(coef(stepForward.model$finalModel))
models <- data.frame(Model = "Step Forward", RMSE = RMSE.forward, Rsquared = rsquared.forward, adj.Rsquared = adj.rsquared.forward, no.coef = stepForwardLength)
models
plot(cc.test$PI ~ forward.pred)


stepBack <- readRDS("stepAICmodels/ccPI_stepBack.rds")
stepBack$anova
stepBack.mod <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                        OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                        k + total.depth + LSsurgo + Erosion:cover + Erosion:tillage + 
                        Erosion:slope + Erosion:Contour + Erosion:initialP + Erosion:OM + 
                        Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + Erosion:slopelenusle.r + 
                        Erosion:silt + Erosion:k + Erosion:total.depth + Erosion:LSsurgo + 
                        cover:tillage + cover:slope + cover:Contour + cover:initialP + 
                        cover:OM + cover:totalP2O5_lbs + cover:total_DM_lbs + cover:slopelenusle.r + 
                        cover:silt + cover:LSsurgo + tillage:slope + tillage:Contour + 
                        tillage:initialP + tillage:OM + tillage:totalP2O5_lbs + tillage:total_DM_lbs + 
                        tillage:slopelenusle.r + tillage:silt + tillage:k + tillage:total.depth + 
                        tillage:LSsurgo + slope:Contour + slope:initialP + slope:OM + 
                        slope:totalP2O5_lbs + slope:slopelenusle.r + slope:silt + 
                        slope:k + slope:total.depth + slope:LSsurgo + Contour:initialP + 
                        Contour:OM + Contour:total_DM_lbs + Contour:slopelenusle.r + 
                        Contour:k + Contour:total.depth + initialP:OM + initialP:totalP2O5_lbs + 
                        initialP:total_DM_lbs + initialP:slopelenusle.r + initialP:silt + 
                        initialP:k + initialP:total.depth + initialP:LSsurgo + OM:totalP2O5_lbs + 
                        OM:total_DM_lbs + OM:slopelenusle.r + OM:silt + OM:k + OM:total.depth + 
                        OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + totalP2O5_lbs:slopelenusle.r + 
                        totalP2O5_lbs:silt + totalP2O5_lbs:k + totalP2O5_lbs:total.depth + 
                        totalP2O5_lbs:LSsurgo + total_DM_lbs:k + total_DM_lbs:total.depth + 
                        slopelenusle.r:silt + slopelenusle.r:k + slopelenusle.r:total.depth + 
                        slopelenusle.r:LSsurgo + silt:k + silt:total.depth + silt:LSsurgo + 
                        k:total.depth + total.depth:LSsurgo,
                      data = train, method = "lm", trControl = train.control)
stepBack.mod
back.pred <- predict(stepBack.mod, cc.test)
RMSE.back <- round(sqrt(mean((back.pred - cc.test$PI)^2)),3)
adj.rsquared.back <- round((summary(stepBack.mod)$adj.r.squared),3)
rsquared.back <- round((summary(stepBack.mod)$r.squared),3)
stepBackLength <- length(coef(stepBack.mod$finalModel))
models <- add_row(models, Model = "Step Backward", RMSE = RMSE.back, Rsquared = rsquared.back, adj.Rsquared = adj.rsquared.back, 
                  no.coef = stepBackLength)
models
plot(cc.test$PI ~ back.pred)


# with cover --------------------------------------------------------------

modCcCc <- readRDS("models/ContCorn_withCoverPI.rds")  


# no cover ----------------------------------------------------------------

modCcNc <- readRDS("models/ContCorn_NoCoverPI.rds")


# predictions -------------------------------------------------------------

ccPred <- predict(stepBack.mod, cc)
cc$pred <- ccPred
ccCcPred <- predict(modCcCc, ccCc)
ccCc$CoverPred <- ccCcPred
ccNcPred <- predict(modCcNc, ccNc)
ccNc$CoverPred <- ccNcPred

# bind cover data sets
coverSet <- bind_rows(ccCc, ccNc)

# join cc and coverSet
ccWithPred <- left_join(cc, coverSet)

ggplot(ccWithPred, aes(x = pred, y = CoverPred, color = cover)) +
  geom_point()


fig <- plot_ly(ccWithPred, x = ~pred, y = ~CoverPred, z = ~PI, color = ~cover,
               text = ~paste('full pred:', pred, '<br>subset pred:', CoverPred, '<br>PI:', PI))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Full data prediction'),
                                   yaxis = list(title = 'Subset data prediction'),
                                   zaxis = list(title = 'SnapPlus PI')))
fig

## USE full model = stepBack.mod

saveRDS(stepBack.mod, "models/ContinuousCornPI.rds")

# create tidy model -------------------------------------------------------

library(tidymodels)

#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

#estimate or train the model
stepBackTidy <- 
  lm_mod %>% 
  fit(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
        OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
        k + total.depth + LSsurgo + Erosion:cover + Erosion:tillage + 
        Erosion:slope + Erosion:Contour + Erosion:initialP + Erosion:OM + 
        Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + Erosion:slopelenusle.r + 
        Erosion:silt + Erosion:k + Erosion:total.depth + Erosion:LSsurgo + 
        cover:tillage + cover:slope + cover:Contour + cover:initialP + 
        cover:OM + cover:totalP2O5_lbs + cover:total_DM_lbs + cover:slopelenusle.r + 
        cover:silt + cover:LSsurgo + tillage:slope + tillage:Contour + 
        tillage:initialP + tillage:OM + tillage:totalP2O5_lbs + tillage:total_DM_lbs + 
        tillage:slopelenusle.r + tillage:silt + tillage:k + tillage:total.depth + 
        tillage:LSsurgo + slope:Contour + slope:initialP + slope:OM + 
        slope:totalP2O5_lbs + slope:slopelenusle.r + slope:silt + 
        slope:k + slope:total.depth + slope:LSsurgo + Contour:initialP + 
        Contour:OM + Contour:total_DM_lbs + Contour:slopelenusle.r + 
        Contour:k + Contour:total.depth + initialP:OM + initialP:totalP2O5_lbs + 
        initialP:total_DM_lbs + initialP:slopelenusle.r + initialP:silt + 
        initialP:k + initialP:total.depth + initialP:LSsurgo + OM:totalP2O5_lbs + 
        OM:total_DM_lbs + OM:slopelenusle.r + OM:silt + OM:k + OM:total.depth + 
        OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + totalP2O5_lbs:slopelenusle.r + 
        totalP2O5_lbs:silt + totalP2O5_lbs:k + totalP2O5_lbs:total.depth + 
        totalP2O5_lbs:LSsurgo + total_DM_lbs:k + total_DM_lbs:total.depth + 
        slopelenusle.r:silt + slopelenusle.r:k + slopelenusle.r:total.depth + 
        slopelenusle.r:LSsurgo + silt:k + silt:total.depth + silt:LSsurgo + 
        k:total.depth + total.depth:LSsurgo,
      data = train)

tidy(stepBackTidy) %>%
  bind_cols(stepBackTidy$finalModel$coefficients) %>%
  dplyr::select(-c(std.error:p.value)) %>%
  gt()

stepBackTidy

cc_pred <- stepBackTidy %>%
  predict(cc) %>%
  bind_cols(cc)

ggplot(cc_pred, aes(x = PI, y = .pred)) +
  geom_point()

saveRDS(stepBackTidy, "models/ContCornTidyPI.rds")
