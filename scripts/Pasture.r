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


# all pasture ------------------------------------------------------

pt <- snap %>%
  filter(crop == "pt",
         Contour == 0) %>%
  dplyr::select(c(Erosion, rotational, density, slope, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  droplevels()

# c1 <- ptrt %>% filter(Contour == "1")
# c0 <- ptrt %>% filter(Contour == "0")
# mean(c1$Erosion)
# hist(c1$Erosion)
# mean(c0$Erosion)
# hist(c0$Erosion)
summary(pt)

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = pt$Erosion, p = 0.8, list = FALSE)
train <- pt[inTrain,]
test <- pt[-inTrain,]

best.guess <- mean(pt$Erosion) # for comparison
RMSE.baseline <- sqrt(mean((best.guess-train$Erosion)^2)) # for comparison

# rf500_full <- randomForest(Erosion ~., data = train, ntree = 500, mtry = 5, importance = TRUE, do.trace = TRUE)
# rf500_full # 99.83
# plot(rf500_full)
# varImp(rf500_full)
# pred500full <- predict(rf500_full, test)
# RMSE.500.full <- sqrt(mean((pred500full - test$Erosion)^2)) #0.0001
# plot(pred500full ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 500, mtry: 5, all variables")


rf100 <- randomForest(Erosion ~., data = train, ntree = 100, mtry = 6, importance = TRUE, do.trace = TRUE)
rf100 # 99.9
plot(rf100)
varImp(rf100)
pred100 <- predict(rf100, test)
RMSE.100 <- sqrt(mean((pred100 - test$Erosion)^2)) #0.0001
plot(pred100 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 100, mtry: 5, no OM, no Contour")


rf75 <- randomForest(Erosion ~., data = train, ntree = 75, mtry = 6, importance = TRUE, do.trace = TRUE)
rf75 # 99.99
plot(rf75)
varImp(rf75)
pred75 <- predict(rf75, test)
RMSE.75 <- sqrt(mean((pred75 - test$Erosion)^2)) #0.0001
png("modelFits/PastureErosion.png")
plot(pred75 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "Pasture: trees: 75, mtry: 6")
abline(a = 0, b = 1)
text(x = 4, y = 12, "RMSE = 0.02")
text(x = 5, y = 11, "% Var Explained = 99.99")
dev.off()

saveRDS(rf75, "models/AllPastureErosion.rds")


# phosphorus loss ---------------------------------------------------------

ptrtPI <- snap %>%
  filter(crop == "pt",
         rotational == "rt", 
         Contour == 0) %>%
  dplyr::select(c(PI, Erosion, slope, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, total.depth, k, LSsurgo)) %>%
  droplevels()

summary(ptrtPI)
## null hypothesis
best.guess <- round(mean(ptrtPI$PI),2)


#partition data
set.seed(0731)
inTrain <- createDataPartition(y = ptrtPI$PI, p = 0.8, list = FALSE)
train <- ptrtPI[inTrain,]
test <- ptrtPI[-inTrain,]

# Evaluate RMSE
RMSE.baseline <- round(sqrt(mean((best.guess-test$PI)^2)),2)

train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times

nointerx.lm <- lm(PI~., data = train)
stepForward <- stepAIC(nointerx.lm, scope = . ~ .^2, direction = "forward")
stepForward$anova
length(stepForward$coefficients) #50
stepForward.model <- train(PI ~ Erosion + slope + initialP + OM + totalP2O5_lbs + total_DM_lbs + 
                             slopelenusle.r + silt + total.depth + k + LSsurgo + OM:k + 
                             initialP:k + OM:slopelenusle.r + silt:k + Erosion:OM + slopelenusle.r:total.depth + 
                             initialP:total.depth + initialP:silt + Erosion:total.depth + 
                             totalP2O5_lbs:total_DM_lbs + Erosion:total_DM_lbs + Erosion:slopelenusle.r + 
                             slope:LSsurgo + total_DM_lbs:silt + Erosion:slope + Erosion:LSsurgo + 
                             OM:silt + total.depth:LSsurgo + silt:total.depth + Erosion:initialP + 
                             initialP:slopelenusle.r + Erosion:k + slopelenusle.r:silt + 
                             OM:LSsurgo + slope:OM + slopelenusle.r:k + slope:silt + slope:k + 
                             Erosion:silt + k:LSsurgo + silt:LSsurgo + slope:initialP + 
                             OM:total.depth + slope:total.depth + total_DM_lbs:k,
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
length(stepBack$coefficients) #48
stepBack.mod <- train(PI ~ Erosion + slope + initialP + OM + totalP2O5_lbs + total_DM_lbs + 
                        slopelenusle.r + silt + total.depth + k + LSsurgo + Erosion:slope + 
                        Erosion:initialP + Erosion:OM + Erosion:total_DM_lbs + Erosion:slopelenusle.r + 
                        Erosion:silt + Erosion:total.depth + Erosion:k + Erosion:LSsurgo + 
                        slope:initialP + slope:total_DM_lbs + slope:silt + slope:total.depth + 
                        slope:k + slope:LSsurgo + initialP:slopelenusle.r + initialP:silt + 
                        initialP:total.depth + initialP:k + OM:slopelenusle.r + OM:silt + 
                        OM:total.depth + OM:k + OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
                        total_DM_lbs:silt + total_DM_lbs:LSsurgo + slopelenusle.r:silt + 
                        slopelenusle.r:total.depth + slopelenusle.r:k + silt:total.depth + 
                        silt:k + silt:LSsurgo + total.depth:LSsurgo + k:LSsurgo,
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
plot(back.pred ~ test$PI)

final.mod <- train(PI ~ Erosion + slope + initialP + OM + totalP2O5_lbs + total_DM_lbs + 
                     slopelenusle.r + silt + total.depth + k + LSsurgo + Erosion:slope + 
                     Erosion:initialP + Erosion:OM + Erosion:total_DM_lbs + Erosion:slopelenusle.r + 
                     Erosion:silt + Erosion:total.depth + Erosion:k + Erosion:LSsurgo + 
                     slope:initialP + slope:total_DM_lbs + slope:silt + slope:total.depth + 
                     slope:k + slope:LSsurgo + initialP:slopelenusle.r + initialP:silt + 
                     initialP:total.depth + initialP:k + OM:slopelenusle.r + OM:silt + 
                     OM:total.depth + OM:k + OM:LSsurgo + totalP2O5_lbs:total_DM_lbs + 
                     total_DM_lbs:silt + total_DM_lbs:LSsurgo + slopelenusle.r:silt + 
                     slopelenusle.r:total.depth + slopelenusle.r:k + silt:total.depth + 
                     silt:k + silt:LSsurgo + total.depth:LSsurgo + k:LSsurgo,
                   data = ptrtPI, method = "lm", trControl = train.control)
final.mod
pred <- predict(final.mod, ptrtPI)
RMSE <- sqrt(mean((pred - ptrtPI$PI)^2))
adj.rsquared <- round((summary(final.mod)$adj.r.squared),3)
rsquared <- round((summary(final.mod)$r.squared),3)
ptrtPI$pred <- pred

library(ggplot2)

saveRDS(stepBack.mod, "models/RotationalPasturePI.rds")

ptrtPI <- snap %>%
  filter(crop == "pt",
         rotational == "rt", 
         Contour == 0) %>%
  mutate(soil = paste(SoilSeries, SoilSymbol)) %>%
  dplyr::select(c(PI, Erosion, slope, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, total.depth, k, LSsurgo, soil)) %>%
  droplevels()

ptrtPI$pred <- pred

library(ggplot2)
png("modelFits/RotationalPasture_PI.png")
ggplot(ptrtPI, aes(x = PI, y = pred)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  ylab("Predicted PI") +
  xlab("SnapPlus PI") +
  annotate("text", x = 0.5, y = 2.0, label = paste("RMSE = ", RMSE.back)) +
  annotate("text", x = 0.5, y = 1.8, label = paste("R^2 = ", adj.rsquared.back)) 
dev.off()


