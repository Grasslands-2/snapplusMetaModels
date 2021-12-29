#load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(MASS)

#load data
snap <- read.table("data/JoinedSnapSurgo.txt", sep = "|", header = TRUE)
snap <- snap %>%
  mutate_if(is.character, as.factor)
summary(snap)

# continuous corn, no cover, Erosion
ccErosion <- snap %>% 
  filter(crop == "cc",
         cover == "nc") %>% 
  #dplyr::select(c(Erosion, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()



summary(ccErosion)


# Erosion -----------------------------------------------------------------
set.seed(0731)
inTrain <- createDataPartition(y = ccErosion$Erosion, p = 0.7, list = FALSE)
train <- ccErosion[inTrain,]
test <- ccErosion[-inTrain,]

best.guess <- round(mean(ccErosion$Erosion),2)
RMSE.baseline <- round(sqrt(mean((best.guess-train$Erosion)^2)),2)

rf100 <- randomForest(Erosion ~ ., data = train, ntree = 100, mtry = 7, importance = TRUE, do.trace = TRUE)
rf100 #99.98
pred_rf100 <- predict(rf100, test)
RMSE.100 <- sqrt(mean((pred_rf100 - test$Erosion)^2)) #0.09
plot(rf100)

rf75 <- randomForest(Erosion ~ ., data = train, ntree = 75, mtry = 7, importance = TRUE, do.trace = TRUE)
rf75 #99.98
pred_rf75 <- predict(rf75, test)
RMSE.75 <- sqrt(mean((pred_rf75 - test$Erosion)^2)) #0.10

rf50 <- randomForest(Erosion ~ ., data = train, ntree = 50, mtry = 7, importance = TRUE, do.trace = TRUE)
rf50 #99.98
pred_rf50 <- predict(rf50, test)
RMSE.50 <- sqrt(mean((pred_rf50 - test$Erosion)^2)) #0.10

saveRDS(rf50, "models/ContCorn_NoCoverErosion.rds")
png("modelFits/ContCorn_NoCover_Erosion.png")
plot(pred_rf50 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 50, mtry:7")
abline(a = 0, b = 1)
text(x = 6, y = 50, "RMSE = 0.10")
text(x = 8.5, y = 45, "% Var Explained = 99.98")
dev.off()


# Phosphorus loss ---------------------------------------------------------


ccPI <- snap %>% 
  filter(crop == "cc",
         cover == "nc") %>% 
  dplyr::select(c(PI, Erosion, tillage, slope, Contour, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
           silt, k, total.depth, LSsurgo)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  mutate_if(is.character, as.factor) %>%
  droplevels()

summary(ccPI)

rm(snap)

## null hypothesis
best.guess <- round(mean(ccPI$PI),2)


#partition data
set.seed(0731)
inTrain <- createDataPartition(y = ccPI$PI, p = 0.7, list = FALSE)
train <- ccPI[inTrain,]
test <- ccPI[-inTrain,]

# Evaluate RMSE
RMSE.baseline <- round(sqrt(mean((best.guess-test$PI)^2)),2)

train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times

nointerx.lm <- lm(PI~., data = train)
stepForward <- stepAIC(nointerx.lm, scope = . ~ .^2, direction = "forward")
stepForward$anova
length(stepForward$coefficients) #130
stepForward.model <- train(PI ~ Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
                             total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                             LSsurgo + Erosion:OM + Erosion:totalP2O5_lbs + Erosion:initialP + 
                             Erosion:silt + Erosion:total_DM_lbs + tillage:initialP + 
                             Erosion:tillage + OM:k + initialP:LSsurgo + initialP:silt + 
                             totalP2O5_lbs:silt + OM:silt + tillage:totalP2O5_lbs + slopelenusle.r:LSsurgo + 
                             Contour:initialP + tillage:total.depth + initialP:total.depth + 
                             tillage:LSsurgo + tillage:silt + totalP2O5_lbs:LSsurgo + 
                             totalP2O5_lbs:total_DM_lbs + slopelenusle.r:total.depth + 
                             silt:k + silt:total.depth + Contour:slopelenusle.r + Erosion:total.depth + 
                             k:LSsurgo + slope:LSsurgo + tillage:total_DM_lbs + slopelenusle.r:k + 
                             tillage:slopelenusle.r + Contour:total_DM_lbs + OM:LSsurgo + 
                             slope:OM + slope:k + total_DM_lbs:k + OM:total_DM_lbs + initialP:total_DM_lbs + 
                             slope:total.depth + total.depth:LSsurgo + tillage:Contour + 
                             Erosion:Contour + Contour:LSsurgo + slope:initialP + Erosion:slopelenusle.r + 
                             initialP:slopelenusle.r + total_DM_lbs:total.depth + OM:total.depth + 
                             k:total.depth + Erosion:k + silt:LSsurgo + tillage:slope + 
                             OM:totalP2O5_lbs + totalP2O5_lbs:k + initialP:k + initialP:OM + 
                             Contour:OM + slope:silt + slopelenusle.r:silt + OM:slopelenusle.r + 
                             tillage:OM + tillage:k + Erosion:slope + Erosion:LSsurgo,
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


allinterx.lm <- lm(PI~.^2,data = train)
stepBack <- stepAIC(allinterx.lm, direction = "backward")
stepBack$anova
length(stepBack$coefficients) #133
stepBack.mod <- train(PI ~ Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
                        total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                        LSsurgo + Erosion:tillage + Erosion:slope + Erosion:Contour + 
                        Erosion:initialP + Erosion:OM + Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + 
                        Erosion:slopelenusle.r + Erosion:silt + Erosion:k + Erosion:total.depth + 
                        Erosion:LSsurgo + tillage:slope + tillage:Contour + tillage:initialP + 
                        tillage:OM + tillage:totalP2O5_lbs + tillage:total_DM_lbs + 
                        tillage:slopelenusle.r + tillage:silt + tillage:k + tillage:total.depth + 
                        tillage:LSsurgo + slope:initialP + slope:OM + slope:totalP2O5_lbs + 
                        slope:silt + slope:k + slope:total.depth + slope:LSsurgo + 
                        Contour:initialP + Contour:OM + Contour:total_DM_lbs + Contour:slopelenusle.r + 
                        Contour:silt + Contour:k + Contour:LSsurgo + initialP:OM + 
                        initialP:total_DM_lbs + initialP:slopelenusle.r + initialP:silt + 
                        initialP:k + initialP:total.depth + initialP:LSsurgo + OM:totalP2O5_lbs + 
                        OM:total_DM_lbs + OM:slopelenusle.r + OM:silt + OM:k + OM:total.depth + 
                        OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + totalP2O5_lbs:slopelenusle.r + 
                        totalP2O5_lbs:silt + totalP2O5_lbs:k + totalP2O5_lbs:LSsurgo + 
                        total_DM_lbs:k + total_DM_lbs:total.depth + slopelenusle.r:silt + 
                        slopelenusle.r:k + slopelenusle.r:total.depth + slopelenusle.r:LSsurgo + 
                        silt:k + silt:total.depth + silt:LSsurgo + k:total.depth + 
                        total.depth:LSsurgo,
                      data = train, method = "lm", trControl = train.control)
stepBack.mod
back.pred <- predict(stepBack.mod, test)
RMSE.back <- round(sqrt(mean((back.pred - test$PI)^2)),3)
adj.rsquared.back <- round((summary(stepBack.mod)$adj.r.squared),3)
rsquared.back <- round((summary(stepBack.mod)$r.squared),3)
stepBackLength <- length(coef(stepBack.mod$finalModel))
summary(stepBack.mod)
models <- add_row(models, Model = "Step Backward", RMSE = RMSE.back, Rsquared = rsquared.back, adj.Rsquared = adj.rsquared.back, 
                  no.coef = stepBackLength)
models

#stepBack
final.mod <- lm(PI ~  Erosion + tillage + slope + Contour + initialP + OM + totalP2O5_lbs + 
                  total_DM_lbs + slopelenusle.r + silt + k + total.depth + 
                  LSsurgo + Erosion:tillage + Erosion:slope + Erosion:Contour + 
                  Erosion:initialP + Erosion:OM + Erosion:totalP2O5_lbs + Erosion:total_DM_lbs + 
                  Erosion:slopelenusle.r + Erosion:silt + Erosion:k + Erosion:total.depth + 
                  Erosion:LSsurgo + tillage:slope + tillage:Contour + tillage:initialP + 
                  tillage:OM + tillage:totalP2O5_lbs + tillage:total_DM_lbs + 
                  tillage:slopelenusle.r + tillage:silt + tillage:k + tillage:total.depth + 
                  tillage:LSsurgo + slope:initialP + slope:OM + slope:totalP2O5_lbs + 
                  slope:silt + slope:k + slope:total.depth + slope:LSsurgo + 
                  Contour:initialP + Contour:OM + Contour:total_DM_lbs + Contour:slopelenusle.r + 
                  Contour:silt + Contour:k + Contour:LSsurgo + initialP:OM + 
                  initialP:total_DM_lbs + initialP:slopelenusle.r + initialP:silt + 
                  initialP:k + initialP:total.depth + initialP:LSsurgo + OM:totalP2O5_lbs + 
                  OM:total_DM_lbs + OM:slopelenusle.r + OM:silt + OM:k + OM:total.depth + 
                  OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + totalP2O5_lbs:slopelenusle.r + 
                  totalP2O5_lbs:silt + totalP2O5_lbs:k + totalP2O5_lbs:LSsurgo + 
                  total_DM_lbs:k + total_DM_lbs:total.depth + slopelenusle.r:silt + 
                  slopelenusle.r:k + slopelenusle.r:total.depth + slopelenusle.r:LSsurgo + 
                  silt:k + silt:total.depth + silt:LSsurgo + k:total.depth + 
                  total.depth:LSsurgo,
                data = ccPI)

pred <- predict(stepBack.mod, ccPI)
RMSE <- sqrt(mean((pred - ccPI$PI)^2))
adj.rsquared <- round((summary(final.mod)$adj.r.squared),3)
rsquared <- round((summary(final.mod)$r.squared),3)
ccPI$pred <- pred

saveRDS(stepBack.mod, "models/ContCorn_NoCoverPI.rds")

library(ggplot2)
png("modelFits/ContCorn_NoCover_PI.png")
ggplot(ccPI, aes(x = PI, y = pred)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  ylab("Predicted PI") +
  xlab("SnapPlus PI") +
  annotate("text", x = 20, y = 100, label = paste("RMSE = ", RMSE.back)) +
  annotate("text", x = 20, y = 90, label = paste("R^2 = ", adj.rsquared.back))
dev.off()


