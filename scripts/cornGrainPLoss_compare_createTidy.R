# this script creates the tidy model for PI corn grain in the tainter creek
# after comparing the step forward and step backward full models with the models subsetted by cover

#load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(MASS)
library(plotly)

#load data
#load data
cg <- read_csv("/Users/elissachasen/Desktop/forCondor/PIdata/cgPI.csv") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct()
cgNc <- cg %>%
  filter(cover == "nc")
cgCc <- cg %>%
  filter(cover == "cc" | cover == "gcis" | cover == "gcds")

# global settings
train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times


# full model --------------------------------------------------------------

# null hypothesis
cg.best.guess <- round(mean(cg$PI),2)

#partition data
set.seed(123)
cg.inTrain <- createDataPartition(y = cg$PI, p = 0.7, list = FALSE)
cg.train <- cg[cg.inTrain,]
cg.test <- cg[-cg.inTrain,]

# Evaluate RMSE
cg.RMSE.baseline <- round(sqrt(mean((cg.best.guess-cg.test$PI)^2)),2)

#load models
cg.stepForward <- readRDS("stepAICmodels/cornGrainPI_stepForward.rds")
cg.stepForward$anova

cg.stepForward.model <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                                OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                                k + total.depth + LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + 
                                Erosion:initialP + Erosion:silt + Erosion:tillage + tillage:initialP + 
                                Erosion:total_DM_lbs + OM:k + initialP:LSsurgo + initialP:silt + 
                                OM:silt + totalP2O5_lbs:silt + cover:initialP + tillage:totalP2O5_lbs + 
                                tillage:slope + Contour:initialP + slope:slopelenusle.r + 
                                cover:tillage + initialP:total.depth + silt:k + slopelenusle.r:total.depth + 
                                Erosion:total.depth + totalP2O5_lbs:LSsurgo + Contour:slopelenusle.r + 
                                OM:total.depth + tillage:silt + cover:totalP2O5_lbs + initialP:total_DM_lbs + 
                                OM:slopelenusle.r + Erosion:LSsurgo + Erosion:Contour + tillage:Contour + 
                                totalP2O5_lbs:total_DM_lbs + tillage:slopelenusle.r + tillage:total_DM_lbs + 
                                Erosion:slopelenusle.r + cover:slopelenusle.r + Erosion:slope + 
                                Contour:LSsurgo + total_DM_lbs:k + OM:total_DM_lbs + Contour:k + 
                                tillage:total.depth + tillage:OM + initialP:OM + slope:LSsurgo + 
                                OM:LSsurgo + slope:OM + tillage:LSsurgo + slope:k + initialP:k + 
                                Erosion:k + silt:LSsurgo + slopelenusle.r:k + slope:silt + 
                                k:total.depth + slopelenusle.r:silt + slope:totalP2O5_lbs + 
                                silt:total.depth + Erosion:cover + Contour:OM + total_DM_lbs:silt + 
                                cover:LSsurgo + totalP2O5_lbs:slopelenusle.r + cover:silt + 
                                cover:Contour + slopelenusle.r:LSsurgo + OM:totalP2O5_lbs + 
                                totalP2O5_lbs:k + tillage:k + slope:total.depth + total.depth:LSsurgo + 
                                total_DM_lbs:total.depth + k:LSsurgo + slope:initialP + initialP:slopelenusle.r + 
                                Contour:silt + cover:k + cover:OM + cover:slope,
                           data = cg.train, method = "lm", trControl = train.control)
cg.stepForward.model
cg.forward.pred <- predict(cg.stepForward.model, cg.test)
cg.RMSE.forward <- round(sqrt(mean((cg.forward.pred - cg.test$PI)^2)),3)
cg.adj.rsquared.forward <- round((summary(cg.stepForward.model)$adj.r.squared),3)
cg.rsquared.forward <- round((summary(cg.stepForward.model)$r.squared),3)
cg.stepForwardLength <- length(coef(cg.stepForward.model$finalModel))
models <- data.frame(Model = "Full Step Forward", RMSE = cg.RMSE.forward, Rsquared = cg.rsquared.forward, adj.Rsquared = cg.adj.rsquared.forward, no.coef = cg.stepForwardLength)
models
plot(cg.test$PI ~ cg.forward.pred)


cg.stepBack <- readRDS("stepAICmodels/cornGrainPI_stepBack.rds")
cg.stepBack$anova
cg.stepBack.mod <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                        OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                        k + total.depth + LSsurgo + Erosion:cover + Erosion:tillage + 
                        Erosion:slope + Erosion:Contour + Erosion:initialP + Erosion:OM + 
                        Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + Erosion:slopelenusle.r + 
                        Erosion:silt + Erosion:k + Erosion:total.depth + Erosion:LSsurgo + 
                        cover:tillage + cover:slope + cover:Contour + cover:initialP + 
                        cover:OM + cover:totalP2O5_lbs + cover:slopelenusle.r + cover:silt + 
                        cover:k + cover:LSsurgo + tillage:slope + tillage:Contour + 
                        tillage:initialP + tillage:OM + tillage:totalP2O5_lbs + tillage:total_DM_lbs + 
                        tillage:slopelenusle.r + tillage:silt + tillage:k + tillage:total.depth + 
                        tillage:LSsurgo + slope:initialP + slope:OM + slope:totalP2O5_lbs + 
                        slope:slopelenusle.r + slope:silt + slope:k + slope:total.depth + 
                        slope:LSsurgo + Contour:initialP + Contour:OM + Contour:totalP2O5_lbs + 
                        Contour:total_DM_lbs + Contour:slopelenusle.r + Contour:silt + 
                        Contour:LSsurgo + initialP:OM + initialP:total_DM_lbs + initialP:slopelenusle.r + 
                        initialP:silt + initialP:k + initialP:total.depth + initialP:LSsurgo + 
                        OM:totalP2O5_lbs + OM:total_DM_lbs + OM:slopelenusle.r + 
                        OM:silt + OM:k + OM:total.depth + OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
                        totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
                        totalP2O5_lbs:LSsurgo + total_DM_lbs:k + total_DM_lbs:total.depth + 
                        slopelenusle.r:silt + slopelenusle.r:k + slopelenusle.r:total.depth + 
                        slopelenusle.r:LSsurgo + silt:k + silt:total.depth + silt:LSsurgo + 
                        k:total.depth + k:LSsurgo + total.depth:LSsurgo,
                      data = cg.train, method = "lm", trControl = train.control)
cg.stepBack.mod
cg.back.pred <- predict(cg.stepBack.mod, cg.test)
cg.RMSE.back <- round(sqrt(mean((cg.back.pred - cg.test$PI)^2)),3)
cg.adj.rsquared.back <- round((summary(cg.stepBack.mod)$adj.r.squared),3)
cg.rsquared.back <- round((summary(cg.stepBack.mod)$r.squared),3)
cg.stepBackLength <- length(coef(cg.stepBack.mod$finalModel))
models <- add_row(models, Model = "Full Step Backward", RMSE = cg.RMSE.back, Rsquared = cg.rsquared.back, adj.Rsquared = cg.adj.rsquared.back, 
                  no.coef = cg.stepBackLength)
models
plot(cg.test$PI ~ cg.back.pred)


# with cover --------------------------------------------------------------

# null hypothesis
cgcc.best.guess <- round(mean(cgCc$PI),2)

#partition data
set.seed(123)
cgcc.inTrain <- createDataPartition(y = cgCc$PI, p = 0.7, list = FALSE)
cgcc.train <- cgCc[cgcc.inTrain,]
cgcc.test <- cgCc[-cgcc.inTrain,]

# Evaluate RMSE
cgcc.RMSE.baseline <- round(sqrt(mean((cgcc.best.guess-cgcc.test$PI)^2)),2)

#load models
cgcc.stepForward <- readRDS("stepAICmodels/cornGrainCoverPI_stepForward.rds")
cgcc.stepForward$anova

cgcc.stepForward.model <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                                  OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                                  k + total.depth + LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + 
                                  Erosion:initialP + Erosion:silt + OM:k + Erosion:total_DM_lbs + 
                                  OM:silt + totalP2O5_lbs:silt + initialP:silt + initialP:LSsurgo + 
                                  silt:k + OM:slopelenusle.r + tillage:totalP2O5_lbs + cover:initialP + 
                                  tillage:initialP + tillage:slope + initialP:total.depth + 
                                  slope:slopelenusle.r + slopelenusle.r:total.depth + Erosion:cover + 
                                  Erosion:Contour + Contour:initialP + totalP2O5_lbs:LSsurgo + 
                                  Erosion:total.depth + cover:totalP2O5_lbs + initialP:slopelenusle.r + 
                                  slope:LSsurgo + initialP:total_DM_lbs + totalP2O5_lbs:total_DM_lbs + 
                                  cover:tillage + Erosion:LSsurgo + cover:k + Erosion:slopelenusle.r + 
                                  Erosion:slope + tillage:slopelenusle.r + OM:total.depth + 
                                  cover:slopelenusle.r + Erosion:tillage + initialP:OM + tillage:OM + 
                                  Contour:slopelenusle.r + Contour:LSsurgo + k:total.depth + 
                                  Contour:OM + tillage:Contour + tillage:total_DM_lbs + total_DM_lbs:k + 
                                  OM:total_DM_lbs + tillage:total.depth + cover:total.depth + 
                                  OM:LSsurgo + slope:OM + slope:k + cover:LSsurgo + tillage:LSsurgo + 
                                  silt:total.depth + Erosion:k + cover:OM + total_DM_lbs:total.depth + 
                                  slopelenusle.r:k + k:LSsurgo + silt:LSsurgo + slope:silt + 
                                  slopelenusle.r:silt + initialP:k + cover:slope + OM:totalP2O5_lbs + 
                                  totalP2O5_lbs:k + slope:totalP2O5_lbs + tillage:silt + slope:total.depth + 
                                  total.depth:LSsurgo + cover:Contour + Contour:k + totalP2O5_lbs:slopelenusle.r + 
                                  Contour:total.depth + slope:initialP + Contour:total_DM_lbs + 
                                  Contour:totalP2O5_lbs + slope:total_DM_lbs,
                              data = cgcc.train, method = "lm", trControl = train.control)
cgcc.stepForward.model
cgcc.forward.pred <- predict(cgcc.stepForward.model, cgcc.test)
cgcc.RMSE.forward <- round(sqrt(mean((cgcc.forward.pred - cgcc.test$PI)^2)),3)
cgcc.adj.rsquared.forward <- round((summary(cgcc.stepForward.model)$adj.r.squared),3)
cgcc.rsquared.forward <- round((summary(cgcc.stepForward.model)$r.squared),3)
cgcc.stepForwardLength <- length(coef(cgcc.stepForward.model$finalModel))
models <- add_row(models, Model = "Cover Step Forward", RMSE = cgcc.RMSE.forward, Rsquared = cgcc.rsquared.forward, adj.Rsquared = cgcc.adj.rsquared.forward, no.coef = cgcc.stepForwardLength)
models
plot(cgcc.test$PI ~ cgcc.forward.pred)


cgcc.stepBack <- readRDS("stepAICmodels/cornGrainCoverPI_stepBack.rds")
cgcc.stepBack$anova
cgcc.stepBack.mod <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
                             OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
                             k + total.depth + LSsurgo + Erosion:cover + Erosion:tillage + 
                             Erosion:slope + Erosion:Contour + Erosion:initialP + Erosion:OM + 
                             Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + Erosion:slopelenusle.r + 
                             Erosion:silt + Erosion:k + Erosion:total.depth + Erosion:LSsurgo + 
                             cover:tillage + cover:slope + cover:Contour + cover:initialP + 
                             cover:OM + cover:totalP2O5_lbs + cover:slopelenusle.r + cover:k + 
                             cover:total.depth + cover:LSsurgo + tillage:slope + tillage:Contour + 
                             tillage:initialP + tillage:OM + tillage:totalP2O5_lbs + tillage:total_DM_lbs + 
                             tillage:slopelenusle.r + tillage:silt + tillage:total.depth + 
                             tillage:LSsurgo + slope:initialP + slope:OM + slope:totalP2O5_lbs + 
                             slope:total_DM_lbs + slope:slopelenusle.r + slope:silt + 
                             slope:k + slope:total.depth + slope:LSsurgo + Contour:initialP + 
                             Contour:OM + Contour:totalP2O5_lbs + Contour:total_DM_lbs + 
                             Contour:slopelenusle.r + Contour:k + Contour:total.depth + 
                             Contour:LSsurgo + initialP:OM + initialP:total_DM_lbs + initialP:slopelenusle.r + 
                             initialP:silt + initialP:k + initialP:total.depth + initialP:LSsurgo + 
                             OM:totalP2O5_lbs + OM:total_DM_lbs + OM:slopelenusle.r + 
                             OM:silt + OM:k + OM:total.depth + OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
                             totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
                             totalP2O5_lbs:LSsurgo + total_DM_lbs:k + total_DM_lbs:total.depth + 
                             slopelenusle.r:silt + slopelenusle.r:k + slopelenusle.r:total.depth + 
                             silt:k + silt:total.depth + silt:LSsurgo + k:total.depth + 
                             total.depth:LSsurgo,
                         data = cgcc.train, method = "lm", trControl = train.control)
cgcc.stepBack.mod
cgcc.back.pred <- predict(cgcc.stepBack.mod, cgcc.test)
cgcc.RMSE.back <- round(sqrt(mean((cgcc.back.pred - cgcc.test$PI)^2)),3)
cgcc.adj.rsquared.back <- round((summary(cgcc.stepBack.mod)$adj.r.squared),3)
cgcc.rsquared.back <- round((summary(cgcc.stepBack.mod)$r.squared),3)
cgcc.stepBackLength <- length(coef(cgcc.stepBack.mod$finalModel))
models <- add_row(models, Model = "Cover Step Backward", RMSE = cgcc.RMSE.back, Rsquared = cgcc.rsquared.back, adj.Rsquared = cgcc.adj.rsquared.back, 
                  no.coef = cgcc.stepBackLength)
models
plot(cgcc.test$PI ~ cgcc.back.pred) 


# no cover ----------------------------------------------------------------

# null hypothesis
cgnc.best.guess <- round(mean(cgNc$PI),2)

#partition data
set.seed(123)
cgnc.inTrain <- createDataPartition(y = cgNc$PI, p = 0.7, list = FALSE)
cgnc.train <- cgNc[cgnc.inTrain,]
cgnc.test <- cgNc[-cgnc.inTrain,]

# Evaluate RMSE
cgnc.RMSE.baseline <- round(sqrt(mean((cgnc.best.guess-cgnc.test$PI)^2)),2)

#load models
cgnc.stepForward <- readRDS("stepAICmodels/cornGrainNoCoverPI_stepForward.rds")
cgnc.stepForward$anova

cgnc.stepForward.model <- train(PI ~ Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
                                  total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                                  LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + Erosion:initialP + 
                                  Erosion:silt + Erosion:tillage + tillage:initialP + Erosion:total_DM_lbs + 
                                  OM:k + initialP:LSsurgo + initialP:silt + totalP2O5_lbs:silt + 
                                  OM:silt + tillage:total_DM_lbs + Contour:initialP + tillage:total.depth + 
                                  tillage:LSsurgo + slope:slopelenusle.r + Contour:slopelenusle.r + 
                                  initialP:total.depth + tillage:silt + initialP:total_DM_lbs + 
                                  totalP2O5_lbs:LSsurgo + Erosion:slope + tillage:totalP2O5_lbs + 
                                  silt:k + slopelenusle.r:total.depth + OM:total.depth + Erosion:total.depth + 
                                  tillage:slopelenusle.r + Erosion:slopelenusle.r + total_DM_lbs:k + 
                                  slope:initialP + OM:total_DM_lbs + totalP2O5_lbs:total_DM_lbs + 
                                  tillage:Contour + Erosion:Contour + OM:LSsurgo + slope:k + 
                                  Contour:LSsurgo + slope:OM + slope:LSsurgo + slopelenusle.r:LSsurgo + 
                                  slopelenusle.r:k + k:LSsurgo + k:total.depth + slope:totalP2O5_lbs + 
                                  silt:LSsurgo + Erosion:k + total_DM_lbs:silt + OM:slopelenusle.r + 
                                  Contour:OM + initialP:OM + initialP:k + slope:silt + silt:total.depth + 
                                  tillage:OM + initialP:slopelenusle.r + slopelenusle.r:silt + 
                                  tillage:slope + Erosion:LSsurgo + Contour:silt + tillage:k + 
                                  Contour:k + totalP2O5_lbs:slopelenusle.r + total_DM_lbs:slopelenusle.r + 
                                  totalP2O5_lbs:k + OM:totalP2O5_lbs + total.depth:LSsurgo + 
                                  slope:total.depth + Contour:total_DM_lbs,
                                data = cgnc.train, method = "lm", trControl = train.control)
cgnc.stepForward.model
cgnc.forward.pred <- predict(cgnc.stepForward.model, cgnc.test)
cgnc.RMSE.forward <- round(sqrt(mean((cgnc.forward.pred - cgnc.test$PI)^2)),3)
cgnc.adj.rsquared.forward <- round((summary(cgnc.stepForward.model)$adj.r.squared),3)
cgnc.rsquared.forward <- round((summary(cgnc.stepForward.model)$r.squared),3)
cgnc.stepForwardLength <- length(coef(cgnc.stepForward.model$finalModel))
models <- add_row(models, Model = "No Cover Step Forward", RMSE = cgnc.RMSE.forward, Rsquared = cgnc.rsquared.forward, adj.Rsquared = cgnc.adj.rsquared.forward, no.coef = cgnc.stepForwardLength)
models
plot(cgnc.test$PI ~ cgnc.forward.pred)


cgnc.stepBack <- readRDS("stepAICmodels/cornGrainNoCoverPI_stepBack.rds")
cgnc.stepBack$anova
cgnc.stepBack.mod <- train(PI ~ Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
                             total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                             LSsurgo + Erosion:tillage + Erosion:slope + Erosion:Contour + 
                             Erosion:initialP + Erosion:OM + Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + 
                             Erosion:slopelenusle.r + Erosion:silt + Erosion:k + Erosion:total.depth + 
                             Erosion:LSsurgo + tillage:slope + tillage:Contour + tillage:initialP + 
                             tillage:OM + tillage:totalP2O5_lbs + tillage:total_DM_lbs + 
                             tillage:slopelenusle.r + tillage:silt + tillage:k + tillage:total.depth + 
                             tillage:LSsurgo + slope:initialP + slope:OM + slope:totalP2O5_lbs + 
                             slope:total_DM_lbs + slope:silt + slope:k + slope:total.depth + 
                             slope:LSsurgo + Contour:initialP + Contour:OM + Contour:total_DM_lbs + 
                             Contour:slopelenusle.r + Contour:silt + Contour:k + Contour:LSsurgo + 
                             initialP:OM + initialP:total_DM_lbs + initialP:slopelenusle.r + 
                             initialP:silt + initialP:k + initialP:total.depth + initialP:LSsurgo + 
                             OM:totalP2O5_lbs + OM:total_DM_lbs + OM:slopelenusle.r + 
                             OM:silt + OM:k + OM:total.depth + OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
                             totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
                             totalP2O5_lbs:LSsurgo + total_DM_lbs:k + total_DM_lbs:LSsurgo + 
                             slopelenusle.r:silt + slopelenusle.r:k + slopelenusle.r:total.depth + 
                             slopelenusle.r:LSsurgo + silt:k + silt:total.depth + silt:LSsurgo + 
                             k:total.depth + k:LSsurgo + total.depth:LSsurgo,
                           data = cgnc.train, method = "lm", trControl = train.control)
cgnc.stepBack.mod
cgnc.back.pred <- predict(cgnc.stepBack.mod, cgnc.test)
cgnc.RMSE.back <- round(sqrt(mean((cgnc.back.pred - cgnc.test$PI)^2)),3)
cgnc.adj.rsquared.back <- round((summary(cgnc.stepBack.mod)$adj.r.squared),3)
cgnc.rsquared.back <- round((summary(cgnc.stepBack.mod)$r.squared),3)
cgnc.stepBackLength <- length(coef(cgnc.stepBack.mod$finalModel))
models <- add_row(models, Model = "Cover Step Backward", RMSE = cgnc.RMSE.back, Rsquared = cgnc.rsquared.back, adj.Rsquared = cgnc.adj.rsquared.back, 
                  no.coef = cgnc.stepBackLength)
models
plot(cgnc.test$PI ~ cgnc.back.pred) 



# predictions -------------------------------------------------------------

cgPred <- predict(cg.stepBack.mod, cg)
cg$pred <- cgPred
cgCcPred <- predict(cgcc.stepBack.mod, cgCc)
cgCc$CoverPred <- cgCcPred
cgNcPred <- predict(cgnc.stepBack.mod, cgNc)
cgNc$CoverPred <- cgNcPred

# bind cover data sets
coverSet <- bind_rows(cgCc, cgNc)

# join cc and coverSet
cgWithPred <- left_join(cg, coverSet)

ggplot(subset(cgWithPred, PI < 50), aes(x = pred, y = CoverPred, color = cover)) +
  geom_point()


fig <- plot_ly(cgWithPred, x = ~pred, y = ~CoverPred, z = ~PI, color = ~cover,
               text = ~paste('full pred:', pred, '<br>subset pred:', CoverPred, '<br>PI:', PI))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Full data prediction'),
                                   yaxis = list(title = 'Subset data prediction'),
                                   zaxis = list(title = 'SnapPlus PI')))
fig

## USE full model = stepBack.mod

saveRDS(cg.stepBack.mod, "models/CornGrainPI.rds")

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
        cover:OM + cover:totalP2O5_lbs + cover:slopelenusle.r + cover:silt + 
        cover:k + cover:LSsurgo + tillage:slope + tillage:Contour + 
        tillage:initialP + tillage:OM + tillage:totalP2O5_lbs + tillage:total_DM_lbs + 
        tillage:slopelenusle.r + tillage:silt + tillage:k + tillage:total.depth + 
        tillage:LSsurgo + slope:initialP + slope:OM + slope:totalP2O5_lbs + 
        slope:slopelenusle.r + slope:silt + slope:k + slope:total.depth + 
        slope:LSsurgo + Contour:initialP + Contour:OM + Contour:totalP2O5_lbs + 
        Contour:total_DM_lbs + Contour:slopelenusle.r + Contour:silt + 
        Contour:LSsurgo + initialP:OM + initialP:total_DM_lbs + initialP:slopelenusle.r + 
        initialP:silt + initialP:k + initialP:total.depth + initialP:LSsurgo + 
        OM:totalP2O5_lbs + OM:total_DM_lbs + OM:slopelenusle.r + 
        OM:silt + OM:k + OM:total.depth + OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
        totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
        totalP2O5_lbs:LSsurgo + total_DM_lbs:k + total_DM_lbs:total.depth + 
        slopelenusle.r:silt + slopelenusle.r:k + slopelenusle.r:total.depth + 
        slopelenusle.r:LSsurgo + silt:k + silt:total.depth + silt:LSsurgo + 
        k:total.depth + k:LSsurgo + total.depth:LSsurgo,
      data = cg.train)

tidy(stepBackTidy) %>%
  bind_cols(cg.stepBack.mod$finalModel$coefficients) %>%
  dplyr::select(-c(std.error:p.value)) %>%
  gt()

saveRDS(stepBackTidy, "models/CornGrain_tidyPI.rds")
