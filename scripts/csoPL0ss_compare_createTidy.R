# this script creates the tidy model for PI corn soy oats in the tainter creek
# after comparing the step forward and step backward full models with the models subsetted by cover

#load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(MASS)
library(plotly)

#load data
#load data
cso <- read_csv("/Users/elissachasen/Desktop/forCondor/PIdata/csoPI.csv") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct()
csonc <- cso %>%
  filter(cover == "nc") %>%
  droplevels()
csocc <- cso %>%
  filter(cover == "cc" | cover == "gcis" | cover == "gcds")

# global settings
train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times


# full model --------------------------------------------------------------

# null hypothesis
cso.best.guess <- round(mean(cso$PI),2)

#partition data
set.seed(123)
cso.inTrain <- createDataPartition(y = cso$PI, p = 0.7, list = FALSE)
cso.train <- cso[cso.inTrain,]
cso.test <- cso[-cso.inTrain,]

# Evaluate RMSE
cso.RMSE.baseline <- round(sqrt(mean((cso.best.guess-cso.test$PI)^2)),2)

#load models
cso.stepForward <- readRDS("stepAICmodels/csoPI_stepForward.rds")
cso.stepForward$anova

cso.stepForward.model <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                                 OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                                 k + total.depth + LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + 
                                 Erosion:initialP + Erosion:silt + tillage:initialP + Erosion:tillage + 
                                 OM:k + Erosion:total_DM_lbs + initialP:LSsurgo + initialP:silt + 
                                 OM:silt + totalP2O5_lbs:silt + tillage:totalP2O5_lbs + Contour:initialP + 
                                 tillage:total.depth + silt:k + slope:slopelenusle.r + cover:initialP + 
                                 tillage:LSsurgo + initialP:total.depth + Contour:slopelenusle.r + 
                                 cover:slope + slopelenusle.r:total.depth + totalP2O5_lbs:LSsurgo + 
                                 OM:slopelenusle.r + tillage:silt + slope:LSsurgo + initialP:total_DM_lbs + 
                                 tillage:total_DM_lbs + Erosion:Contour + OM:LSsurgo + Erosion:total.depth + 
                                 tillage:Contour + slope:OM + OM:total.depth + totalP2O5_lbs:total_DM_lbs + 
                                 Erosion:cover + Contour:LSsurgo + tillage:slopelenusle.r + 
                                 slope:k + Erosion:slopelenusle.r + tillage:OM + Erosion:slope + 
                                 Contour:OM + Erosion:LSsurgo + tillage:slope + initialP:OM + 
                                 cover:totalP2O5_lbs + cover:tillage + total_DM_lbs:k + OM:total_DM_lbs + 
                                 Erosion:k + silt:LSsurgo + slopelenusle.r:k + slope:silt + 
                                 k:total.depth + slopelenusle.r:silt + initialP:k + initialP:slopelenusle.r + 
                                 total_DM_lbs:total.depth + silt:total.depth + slope:initialP + 
                                 Contour:totalP2O5_lbs + OM:totalP2O5_lbs + totalP2O5_lbs:k + 
                                 slopelenusle.r:LSsurgo + cover:slopelenusle.r + cover:total.depth + 
                                 Contour:silt + tillage:k + slope:total.depth + total.depth:LSsurgo + 
                                 k:LSsurgo + Contour:total_DM_lbs + cover:Contour + slope:totalP2O5_lbs + 
                                 totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:total.depth + 
                                 slope:Contour,
                              data = cso.train, method = "lm", trControl = train.control)
cso.stepForward.model
cso.forward.pred <- predict(cso.stepForward.model, cso.test)
cso.RMSE.forward <- round(sqrt(mean((cso.forward.pred - cso.test$PI)^2)),3)
cso.adj.rsquared.forward <- round((summary(cso.stepForward.model)$adj.r.squared),3)
cso.rsquared.forward <- round((summary(cso.stepForward.model)$r.squared),3)
cso.stepForwardLength <- length(coef(cso.stepForward.model$finalModel))
models <- data.frame(Model = "Full Step Forward", RMSE = cso.RMSE.forward, Rsquared = cso.rsquared.forward, adj.Rsquared = cso.adj.rsquared.forward, no.coef = cso.stepForwardLength)
models
plot(cso.test$PI ~ cso.forward.pred)


cso.stepBack <- readRDS("stepAICmodels/csoPI_stepBack.rds")
cso.stepBack$anova
cso.stepBack.mod <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                            OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                            k + total.depth + LSsurgo + Erosion:cover + Erosion:tillage + 
                            Erosion:slope + Erosion:Contour + Erosion:initialP + Erosion:OM + 
                            Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + Erosion:slopelenusle.r + 
                            Erosion:silt + Erosion:k + Erosion:total.depth + Erosion:LSsurgo + 
                            cover:tillage + cover:slope + cover:Contour + cover:initialP + 
                            cover:totalP2O5_lbs + cover:slopelenusle.r + cover:total.depth + 
                            tillage:slope + tillage:Contour + tillage:initialP + tillage:OM + 
                            tillage:totalP2O5_lbs + tillage:total_DM_lbs + tillage:slopelenusle.r + 
                            tillage:silt + tillage:k + tillage:total.depth + tillage:LSsurgo + 
                            slope:Contour + slope:initialP + slope:OM + slope:totalP2O5_lbs + 
                            slope:slopelenusle.r + slope:silt + slope:k + slope:total.depth + 
                            slope:LSsurgo + Contour:initialP + Contour:OM + Contour:totalP2O5_lbs + 
                            Contour:total_DM_lbs + Contour:slopelenusle.r + Contour:silt + 
                            Contour:LSsurgo + initialP:OM + initialP:total_DM_lbs + initialP:slopelenusle.r + 
                            initialP:silt + initialP:k + initialP:total.depth + initialP:LSsurgo + 
                            OM:totalP2O5_lbs + OM:total_DM_lbs + OM:slopelenusle.r + 
                            OM:silt + OM:k + OM:total.depth + OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
                            totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
                            totalP2O5_lbs:total.depth + totalP2O5_lbs:LSsurgo + total_DM_lbs:k + 
                            total_DM_lbs:total.depth + slopelenusle.r:silt + slopelenusle.r:k + 
                            slopelenusle.r:total.depth + slopelenusle.r:LSsurgo + silt:k + 
                            silt:total.depth + silt:LSsurgo + k:total.depth + k:LSsurgo + 
                            total.depth:LSsurgo,
                         data = cso.train, method = "lm", trControl = train.control)
cso.stepBack.mod
cso.back.pred <- predict(cso.stepBack.mod, cso.test)
cso.RMSE.back <- round(sqrt(mean((cso.back.pred - cso.test$PI)^2)),3)
cso.adj.rsquared.back <- round((summary(cso.stepBack.mod)$adj.r.squared),3)
cso.rsquared.back <- round((summary(cso.stepBack.mod)$r.squared),3)
cso.stepBackLength <- length(coef(cso.stepBack.mod$finalModel))
models <- add_row(models, Model = "Full Step Backward", RMSE = cso.RMSE.back, Rsquared = cso.rsquared.back, adj.Rsquared = cso.adj.rsquared.back, 
                  no.coef = cso.stepBackLength)
models
plot(cso.test$PI ~ cso.back.pred)


# with cover --------------------------------------------------------------

# null hypothesis
csocc.best.guess <- round(mean(csocc$PI),2)

#partition data
set.seed(123)
csocc.inTrain <- createDataPartition(y = csocc$PI, p = 0.7, list = FALSE)
csocc.train <- csocc[csocc.inTrain,]
csocc.test <- csocc[-csocc.inTrain,]

# Evaluate RMSE
csocc.RMSE.baseline <- round(sqrt(mean((csocc.best.guess-csocc.test$PI)^2)),2)

#load models
csocc.stepForward <- readRDS("stepAICmodels/csoCcPI_stepForward.rds")
csocc.stepForward$anova

csocc.stepForward.model <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                                   OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                                   k + total.depth + LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + 
                                   Erosion:initialP + Erosion:silt + OM:k + Erosion:total_DM_lbs + 
                                   OM:silt + Erosion:tillage + totalP2O5_lbs:silt + initialP:silt + 
                                   initialP:LSsurgo + tillage:initialP + slope:slopelenusle.r + 
                                   silt:k + tillage:total_DM_lbs + OM:slopelenusle.r + slopelenusle.r:total.depth + 
                                   initialP:total.depth + Erosion:total.depth + initialP:slopelenusle.r + 
                                   totalP2O5_lbs:LSsurgo + tillage:slope + Erosion:Contour + 
                                   Contour:initialP + slope:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
                                   cover:initialP + OM:LSsurgo + initialP:total_DM_lbs + slope:OM + 
                                   initialP:OM + OM:total.depth + OM:totalP2O5_lbs + totalP2O5_lbs:k + 
                                   tillage:Contour + slope:k + Erosion:slopelenusle.r + cover:slope + 
                                   tillage:OM + Contour:slopelenusle.r + Contour:LSsurgo + totalP2O5_lbs:total.depth + 
                                   Contour:OM + slope:total.depth + slopelenusle.r:k + k:total.depth + 
                                   tillage:slopelenusle.r + Erosion:slope + k:LSsurgo + total.depth:LSsurgo + 
                                   Erosion:LSsurgo + tillage:LSsurgo + Erosion:cover + total_DM_lbs:k + 
                                   Erosion:k + silt:LSsurgo + tillage:total.depth + cover:totalP2O5_lbs + 
                                   slope:silt + slopelenusle.r:silt + silt:total.depth + OM:total_DM_lbs + 
                                   tillage:totalP2O5_lbs + cover:Contour + cover:slopelenusle.r + 
                                   totalP2O5_lbs:slopelenusle.r + slope:totalP2O5_lbs + initialP:totalP2O5_lbs + 
                                   slope:initialP + tillage:silt + total_DM_lbs:total.depth + 
                                   initialP:k + slope:Contour + Contour:totalP2O5_lbs + Contour:total_DM_lbs + 
                                   Contour:total.depth + cover:tillage + cover:k,
                                data = csocc.train, method = "lm", trControl = train.control)
csocc.stepForward.model
csocc.forward.pred <- predict(csocc.stepForward.model, csocc.test)
csocc.RMSE.forward <- round(sqrt(mean((csocc.forward.pred - csocc.test$PI)^2)),3)
csocc.adj.rsquared.forward <- round((summary(csocc.stepForward.model)$adj.r.squared),3)
csocc.rsquared.forward <- round((summary(csocc.stepForward.model)$r.squared),3)
csocc.stepForwardLength <- length(coef(csocc.stepForward.model$finalModel))
models <- add_row(models, Model = "Cover Step Forward", RMSE = csocc.RMSE.forward, Rsquared = csocc.rsquared.forward, adj.Rsquared = csocc.adj.rsquared.forward, no.coef = csocc.stepForwardLength)
models
plot(csocc.test$PI ~ csocc.forward.pred)


csocc.stepBack <- readRDS("stepAICmodels/csoCcPI_stepBack.rds")
csocc.stepBack$anova
csocc.stepBack.mod <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                              OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                              k + total.depth + LSsurgo + Erosion:cover + Erosion:tillage + 
                              Erosion:slope + Erosion:Contour + Erosion:initialP + Erosion:OM + 
                              Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + Erosion:slopelenusle.r + 
                              Erosion:silt + Erosion:k + Erosion:total.depth + Erosion:LSsurgo + 
                              cover:tillage + cover:slope + cover:Contour + cover:initialP + 
                              cover:totalP2O5_lbs + cover:slopelenusle.r + cover:total.depth + 
                              tillage:slope + tillage:Contour + tillage:initialP + tillage:OM + 
                              tillage:totalP2O5_lbs + tillage:total_DM_lbs + tillage:slopelenusle.r + 
                              tillage:silt + tillage:total.depth + tillage:LSsurgo + slope:Contour + 
                              slope:initialP + slope:OM + slope:totalP2O5_lbs + slope:slopelenusle.r + 
                              slope:silt + slope:k + slope:total.depth + slope:LSsurgo + 
                              Contour:initialP + Contour:OM + Contour:totalP2O5_lbs + Contour:total_DM_lbs + 
                              Contour:slopelenusle.r + Contour:total.depth + Contour:LSsurgo + 
                              initialP:OM + initialP:totalP2O5_lbs + initialP:total_DM_lbs + 
                              initialP:slopelenusle.r + initialP:silt + initialP:k + initialP:total.depth + 
                              initialP:LSsurgo + OM:totalP2O5_lbs + OM:total_DM_lbs + OM:slopelenusle.r + 
                              OM:silt + OM:k + OM:total.depth + OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
                              totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
                              totalP2O5_lbs:total.depth + totalP2O5_lbs:LSsurgo + total_DM_lbs:k + 
                              total_DM_lbs:total.depth + slopelenusle.r:silt + slopelenusle.r:k + 
                              slopelenusle.r:total.depth + silt:k + silt:total.depth + 
                              silt:LSsurgo + k:total.depth + total.depth:LSsurgo,
                           data = csocc.train, method = "lm", trControl = train.control)
csocc.stepBack.mod
csocc.back.pred <- predict(csocc.stepBack.mod, csocc.test)
csocc.RMSE.back <- round(sqrt(mean((csocc.back.pred - csocc.test$PI)^2)),3)
csocc.adj.rsquared.back <- round((summary(csocc.stepBack.mod)$adj.r.squared),3)
csocc.rsquared.back <- round((summary(csocc.stepBack.mod)$r.squared),3)
csocc.stepBackLength <- length(coef(csocc.stepBack.mod$finalModel))
models <- add_row(models, Model = "Cover Step Backward", RMSE = csocc.RMSE.back, Rsquared = csocc.rsquared.back, adj.Rsquared = csocc.adj.rsquared.back, 
                  no.coef = csocc.stepBackLength)
models
plot(csocc.test$PI ~ csocc.back.pred) 


# no cover ----------------------------------------------------------------

# null hypothesis
csonc.best.guess <- round(mean(csonc$PI),2)

#partition data
set.seed(123)
csonc.inTrain <- createDataPartition(y = csonc$PI, p = 0.7, list = FALSE)
csonc.train <- csonc[csonc.inTrain,]
csonc.test <- csonc[-csonc.inTrain,]

# Evaluate RMSE
csonc.RMSE.baseline <- round(sqrt(mean((csonc.best.guess-csonc.test$PI)^2)),2)

#load models
csonc.stepForward <- readRDS("stepAICmodels/csoNcPI_stepForward.rds")
csonc.stepForward$anova

csonc.stepForward.model <- train(PI ~ Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
                                   total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                                   LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + Erosion:initialP + 
                                   Erosion:silt + tillage:initialP + OM:k + Erosion:total_DM_lbs + 
                                   Erosion:tillage + initialP:LSsurgo + initialP:silt + OM:silt + 
                                   totalP2O5_lbs:silt + Contour:initialP + totalP2O5_lbs:LSsurgo + 
                                   tillage:total.depth + slopelenusle.r:LSsurgo + tillage:LSsurgo + 
                                   Contour:slopelenusle.r + tillage:totalP2O5_lbs + initialP:total.depth + 
                                   initialP:total_DM_lbs + slopelenusle.r:total.depth + silt:k + 
                                   tillage:silt + OM:total.depth + slope:LSsurgo + Erosion:total.depth + 
                                   Erosion:slope + tillage:slopelenusle.r + tillage:total_DM_lbs + 
                                   Erosion:Contour + tillage:Contour + Contour:LSsurgo + Contour:totalP2O5_lbs + 
                                   OM:totalP2O5_lbs + totalP2O5_lbs:total_DM_lbs + Contour:k + 
                                   total_DM_lbs:k + Erosion:slopelenusle.r + Erosion:LSsurgo + 
                                   k:total.depth + silt:total.depth + Erosion:k + k:LSsurgo + 
                                   slope:initialP + initialP:OM + initialP:k + Contour:OM + 
                                   initialP:slopelenusle.r + tillage:OM + tillage:slope + slopelenusle.r:k + 
                                   slope:k + OM:LSsurgo + slope:OM + silt:LSsurgo + slope:silt + 
                                   slopelenusle.r:silt + total.depth:LSsurgo + slope:total.depth + 
                                   Contour:silt + OM:total_DM_lbs + tillage:k + totalP2O5_lbs:slopelenusle.r + 
                                   total_DM_lbs:total.depth + slope:totalP2O5_lbs + initialP:totalP2O5_lbs + 
                                   Contour:total.depth,
                                data = csonc.train, method = "lm", trControl = train.control)
csonc.stepForward.model
csonc.forward.pred <- predict(csonc.stepForward.model, csonc.test)
csonc.RMSE.forward <- round(sqrt(mean((csonc.forward.pred - csonc.test$PI)^2)),3)
csonc.adj.rsquared.forward <- round((summary(csonc.stepForward.model)$adj.r.squared),3)
csonc.rsquared.forward <- round((summary(csonc.stepForward.model)$r.squared),3)
csonc.stepForwardLength <- length(coef(csonc.stepForward.model$finalModel))
models <- add_row(models, Model = "No Cover Step Forward", RMSE = csonc.RMSE.forward, Rsquared = csonc.rsquared.forward, adj.Rsquared = csonc.adj.rsquared.forward, no.coef = csonc.stepForwardLength)
models
plot(csonc.test$PI ~ csonc.forward.pred)


csonc.stepBack <- readRDS("stepAICmodels/csoNcPI_stepBack.rds")
csonc.stepBack$anova
csonc.stepBack.mod <- train(PI ~ Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
                              total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                              LSsurgo + Erosion:tillage + Erosion:slope + Erosion:Contour + 
                              Erosion:initialP + Erosion:OM + Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + 
                              Erosion:slopelenusle.r + Erosion:silt + Erosion:k + Erosion:total.depth + 
                              Erosion:LSsurgo + tillage:slope + tillage:Contour + tillage:initialP + 
                              tillage:OM + tillage:totalP2O5_lbs + tillage:total_DM_lbs + 
                              tillage:slopelenusle.r + tillage:silt + tillage:k + tillage:total.depth + 
                              tillage:LSsurgo + slope:initialP + slope:OM + slope:totalP2O5_lbs + 
                              slope:silt + slope:k + slope:total.depth + slope:LSsurgo + 
                              Contour:initialP + Contour:OM + Contour:totalP2O5_lbs + Contour:slopelenusle.r + 
                              Contour:silt + Contour:total.depth + Contour:LSsurgo + initialP:OM + 
                              initialP:totalP2O5_lbs + initialP:total_DM_lbs + initialP:slopelenusle.r + 
                              initialP:silt + initialP:k + initialP:total.depth + initialP:LSsurgo + 
                              OM:totalP2O5_lbs + OM:total_DM_lbs + OM:silt + OM:k + OM:total.depth + 
                              OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + totalP2O5_lbs:slopelenusle.r + 
                              totalP2O5_lbs:silt + totalP2O5_lbs:LSsurgo + total_DM_lbs:k + 
                              total_DM_lbs:total.depth + slopelenusle.r:silt + slopelenusle.r:k + 
                              slopelenusle.r:total.depth + slopelenusle.r:LSsurgo + silt:k + 
                              silt:total.depth + silt:LSsurgo + k:total.depth + k:LSsurgo + 
                              total.depth:LSsurgo,
                           data = csonc.train, method = "lm", trControl = train.control)
csonc.stepBack.mod
csonc.back.pred <- predict(csonc.stepBack.mod, csonc.test)
csonc.RMSE.back <- round(sqrt(mean((csonc.back.pred - csonc.test$PI)^2)),3)
csonc.adj.rsquared.back <- round((summary(csonc.stepBack.mod)$adj.r.squared),3)
csonc.rsquared.back <- round((summary(csonc.stepBack.mod)$r.squared),3)
csonc.stepBackLength <- length(coef(csonc.stepBack.mod$finalModel))
models <- add_row(models, Model = "Cover Step Backward", RMSE = csonc.RMSE.back, Rsquared = csonc.rsquared.back, adj.Rsquared = csonc.adj.rsquared.back, 
                  no.coef = csonc.stepBackLength)
models
plot(csonc.test$PI ~ csonc.back.pred) 



# predictions -------------------------------------------------------------

csoPred <- predict(cso.stepBack.mod, cso)
cso$pred <- csoPred
csoccPred <- predict(csocc.stepBack.mod, csocc)
csocc$CoverPred <- csoccPred
csoncPred <- predict(csonc.stepBack.mod, csonc)
csonc$CoverPred <- csoncPred

# bind cover data sets
coverSet <- bind_rows(csocc, csonc)

# join cc and coverSet
csoWithPred <- left_join(cso, coverSet)

ggplot(csoWithPred, aes(x = pred, y = CoverPred, color = cover)) +
  geom_point()

lowPI <- csoWithPred %>% filter(PI < 5)

ggplot(lowPI, aes(x = pred, y = CoverPred, color = cover)) +
  geom_point()

fig <- plot_ly(csoWithPred, x = ~pred, y = ~CoverPred, z = ~PI, color = ~cover,
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

hist(csoWithPred$PI, breaks = 40)

## USE full model = stepBack.mod

saveRDS(cso.stepBack.mod, "models/CsoPI.rds")

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
        cover:totalP2O5_lbs + cover:slopelenusle.r + cover:total.depth + 
        tillage:slope + tillage:Contour + tillage:initialP + tillage:OM + 
        tillage:totalP2O5_lbs + tillage:total_DM_lbs + tillage:slopelenusle.r + 
        tillage:silt + tillage:k + tillage:total.depth + tillage:LSsurgo + 
        slope:Contour + slope:initialP + slope:OM + slope:totalP2O5_lbs + 
        slope:slopelenusle.r + slope:silt + slope:k + slope:total.depth + 
        slope:LSsurgo + Contour:initialP + Contour:OM + Contour:totalP2O5_lbs + 
        Contour:total_DM_lbs + Contour:slopelenusle.r + Contour:silt + 
        Contour:LSsurgo + initialP:OM + initialP:total_DM_lbs + initialP:slopelenusle.r + 
        initialP:silt + initialP:k + initialP:total.depth + initialP:LSsurgo + 
        OM:totalP2O5_lbs + OM:total_DM_lbs + OM:slopelenusle.r + 
        OM:silt + OM:k + OM:total.depth + OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
        totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
        totalP2O5_lbs:total.depth + totalP2O5_lbs:LSsurgo + total_DM_lbs:k + 
        total_DM_lbs:total.depth + slopelenusle.r:silt + slopelenusle.r:k + 
        slopelenusle.r:total.depth + slopelenusle.r:LSsurgo + silt:k + 
        silt:total.depth + silt:LSsurgo + k:total.depth + k:LSsurgo + 
        total.depth:LSsurgo,
      data = cso.train)

tidy(stepBackTidy) %>%
  bind_cols(cso.stepBack.mod$finalModel$coefficients) %>%
  dplyr::select(-c(std.error:p.value)) %>%
  gt()

saveRDS(stepBackTidy, "models/CSO_tidyPI.rds")


