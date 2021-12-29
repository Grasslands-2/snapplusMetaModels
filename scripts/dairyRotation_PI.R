# this script tests the step forward and step backwards PI models for dairy rotation in tainter creek
# it saves the step forward model - this was created before the for condor files

#load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(tidymodels)

#load data
snap <- read.table("../data/JoinedSnapSurgo.txt", sep = "|", header = TRUE)
snap <- snap %>%
  mutate_if(is.character, as.factor)
summary(snap)

dr <- snap %>% 
  filter(crop == "dr") %>%
  dplyr::select(c(PI, Erosion, cover, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, silt, k, OM, initialP, totalP2O5_lbs, total.depth, LSsurgo)) %>%
  mutate(Contour = as.factor(Contour),
         tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  droplevels()

summary(dr)

best.guess <- round(mean(dr$PI),2)


#partition data
set.seed(0731)
inTrain <- createDataPartition(y = dr$PI, p = 0.7, list = FALSE)
train <- dr[inTrain,]
test <- dr[-inTrain,]

# Evaluate RMSE
RMSE.baseline <- round(sqrt(mean((best.guess-test$PI)^2)),2)

train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times

nointerx.lm <- lm(PI~., data = train)
stepForward <- stepAIC(nointerx.lm, scope = . ~ .^2, direction = "forward")
stepForward$anova
length(stepForward$coefficients) #186
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


# allinterx.lm <- lm(PI~.^2,data = train)
# stepBack <- stepAIC(allinterx.lm, direction = "backward")
# stepBack$anova
# length(stepBack$coefficients) #142
# stepBack.mod <- train(PI ~ Erosion + cover + tillage + slope + Contour + initialP + 
#                         OM + totalP2O5_lbs + total_DM_lbs + slopelenusle.r + silt + 
#                         k + total.depth + LSsurgo + Erosion:cover + Erosion:tillage + 
#                         Erosion:slope + Erosion:Contour + Erosion:initialP + Erosion:OM + 
#                         Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + Erosion:slopelenusle.r + 
#                         Erosion:silt + Erosion:k + Erosion:total.depth + Erosion:LSsurgo + 
#                         cover:tillage + cover:slope + cover:Contour + cover:initialP + 
#                         cover:totalP2O5_lbs + cover:total_DM_lbs + cover:slopelenusle.r + 
#                         cover:silt + cover:k + cover:total.depth + cover:LSsurgo + 
#                         tillage:slope + tillage:Contour + tillage:initialP + tillage:OM + 
#                         tillage:totalP2O5_lbs + tillage:total_DM_lbs + tillage:slopelenusle.r + 
#                         tillage:silt + tillage:k + tillage:total.depth + tillage:LSsurgo + 
#                         slope:Contour + slope:initialP + slope:OM + slope:totalP2O5_lbs + 
#                         slope:total_DM_lbs + slope:slopelenusle.r + slope:silt + 
#                         slope:total.depth + slope:LSsurgo + Contour:initialP + Contour:OM + 
#                         Contour:total_DM_lbs + Contour:slopelenusle.r + Contour:k + 
#                         Contour:total.depth + initialP:OM + initialP:totalP2O5_lbs + 
#                         initialP:total_DM_lbs + initialP:slopelenusle.r + initialP:silt + 
#                         initialP:total.depth + initialP:LSsurgo + OM:totalP2O5_lbs + 
#                         OM:total_DM_lbs + OM:slopelenusle.r + OM:silt + OM:k + OM:total.depth + 
#                         OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + totalP2O5_lbs:slopelenusle.r + 
#                         totalP2O5_lbs:silt + totalP2O5_lbs:k + totalP2O5_lbs:total.depth + 
#                         totalP2O5_lbs:LSsurgo + total_DM_lbs:slopelenusle.r + total_DM_lbs:silt + 
#                         total_DM_lbs:k + total_DM_lbs:total.depth + total_DM_lbs:LSsurgo + 
#                         slopelenusle.r:silt + slopelenusle.r:total.depth + slopelenusle.r:LSsurgo + 
#                         silt:k + silt:total.depth + silt:LSsurgo + k:total.depth + 
#                         k:LSsurgo + total.depth:LSsurgo,
#                       data = train, method = "lm", trControl = train.control)
# stepBack.mod
# back.pred <- predict(stepBack.mod, test)
# RMSE.back <- round(sqrt(mean((back.pred - test$PI)^2)),3)
# adj.rsquared.back <- round((summary(stepBack.mod)$adj.r.squared),3)
# rsquared.back <- round((summary(stepBack.mod)$r.squared),3)
# stepBackLength <- length(coef(stepBack.mod$finalModel))
# summary(stepBack.mod)
# models <- add_row(models, Model = "Step Backward", RMSE = RMSE.back, Rsquared = rsquared.back, adj.Rsquared = adj.rsquared.back, 
#                   no.coef = stepBackLength)
# models

#stepBack

pred <- predict(stepBack.mod, ccPI)
RMSE <- sqrt(mean((pred - ccPI$PI)^2))
ccPI$pred <- pred

saveRDS(stepForward.model, "models/dairyRotationPI.rds")


