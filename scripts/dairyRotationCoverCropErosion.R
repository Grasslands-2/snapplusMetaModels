#load libraries
library(tidyverse)
library(caret)
library(randomForest)

#load data
snap <- read.table("data/JoinedSnapSurgo.txt", sep = "|", header = TRUE)
snap <- snap %>%
  mutate_if(is.character, as.factor)
summary(snap)

# dairy rotation, cover crop
drcc <- snap %>% 
  filter(crop == "dr",
         cover == "cc" 
         | cover == "gcis"
         | cover == "gcds") %>%
  select(c(Erosion, cover, tillage, slope, OM, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k, total.depth, LSsurgo)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()
# select(c(Erosion, tillage, slope, Contour, totalManureDM_lbs, slopelenusle.r, sand, silt, clay, k))
summary(drcc)

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = drcc$Erosion, p = 0.7, list = FALSE)
train <- drcc[inTrain,]
test <- drcc[-inTrain,]

best.guess <- round(mean(drcc$Erosion),2)
RMSE.baseline <- round(sqrt(mean((best.guess-train$Erosion)^2)),2)

drcc_rf100 <- randomForest(Erosion ~., data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
drcc_rf100 #99.96
pred_rf100 <- predict(drcc_rf100, test)
RMSE.50 <- sqrt(mean((pred_rf100 - test$Erosion)^2)) #0.07
varImp(drcc_rf100)
varImpPlot(drcc_rf100)
importance(drcc_rf100)

rf100 <- randomForest(Erosion ~.-total.depth - LSsurgo, data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
rf100 #99.98
pred100 <- predict(rf100, test)
RMSE.100 <- sqrt(mean((pred100 - test$Erosion)^2)) #0.05
varImp(rf100)
varImpPlot(rf100)
importance(rf100)

rf100.1 <- randomForest(Erosion ~.-total.depth - LSsurgo - OM, data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
rf100.1 #99.98
pred100.1 <- predict(rf100.1, test)
RMSE.100.1 <- sqrt(mean((pred100.1 - test$Erosion)^2)) #0.05
varImp(rf100.1)
varImpPlot(rf100.1)
importance(rf100.1)

# dairy rotation, cover crop
drcc <- snap %>% 
  filter(crop == "dr",
         cover == "cc" 
         | cover == "gcis"
         | cover == "gcds") %>%
  select(c(Erosion, cover, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = drcc$Erosion, p = 0.7, list = FALSE)
train <- drcc[inTrain,]
test <- drcc[-inTrain,]

rf75 <- randomForest(Erosion ~., data = train, ntree = 75, mtry = 8, importance = TRUE, do.trace = TRUE)
rf75 #99.98
pred75 <- predict(rf75, test)
RMSE75 <- sqrt(mean((pred75 - test$Erosion)^2)) #0.05
plot(pred75 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 75, mtry:8")

rf75.7 <- randomForest(Erosion ~., data = train, ntree = 75, mtry = 7, importance = TRUE, do.trace = TRUE)
rf75.7 #99.97
pred75.7 <- predict(rf75.7, test)
RMSE75.7 <- sqrt(mean((pred75.7 - test$Erosion)^2)) #0.05
plot(pred75.7 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 75, mtry:7")

rf50.7 <- randomForest(Erosion ~., data = train, ntree = 50, mtry = 7, importance = TRUE, do.trace = TRUE)
rf50.7 #99.97
pred50.7 <- predict(rf50.7, test)
RMSE50.7 <- sqrt(mean((pred50.7 - test$Erosion)^2)) #0.06
png("modelFits/DairyRotationCoverCropErosion.png")
plot(pred50.7 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 50, mtry:7")
abline(a = 0, b = 1)
text(x = 3, y = 25, "RMSE = 0.047")
text(x = 3.5, y = 20, "% Var Explained = 99.9")
dev.off()
saveRDS(rf50.7, "models/DairyRotationCoverCropErosion.rds")

rf50.8 <- randomForest(Erosion ~., data = train, ntree = 50, mtry = 8, importance = TRUE, do.trace = TRUE)
rf50.8 #99.98
pred50.8 <- predict(rf50.8, test)
RMSE50.8 <- sqrt(mean((pred50.8 - test$Erosion)^2)) #0.05
plot(pred50.8 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 50, mtry:8")

rf50.6 <- randomForest(Erosion ~., data = train, ntree = 50, mtry = 6, importance = TRUE, do.trace = TRUE)
rf50.6 #99.95
pred50.6 <- predict(rf50.6, test)
RMSE50.6 <- sqrt(mean((pred50.6 - test$Erosion)^2)) #0.08
plot(pred50.6 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 50, mtry:6")
