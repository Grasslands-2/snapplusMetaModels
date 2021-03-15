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
drnc <- snap %>% 
  filter(crop == "dr",
         cover == "nc") %>% 
select(c(Erosion, tillage, slope, OM, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k, total.depth, LSsurgo)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()
 #select(c(Erosion, tillage, slope, Contour, totalManureDM_lbs, slopelenusle.r, sand, silt, clay, k))
summary(drnc)

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = drnc$Erosion, p = 0.7, list = FALSE)
train <- drnc[inTrain,]
test <- drnc[-inTrain,]

best.guess <- round(mean(drnc$Erosion),2)
RMSE.baseline <- round(sqrt(mean((best.guess-train$Erosion)^2)),2)

rf100 <- randomForest(Erosion ~., data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
rf100 #99.95
pred_rf100 <- predict(rf100, test)
RMSE.100 <- sqrt(mean((pred_rf100 - test$Erosion)^2)) #0.16
varImp(rf100)
varImpPlot(rf100)
importance(rf100)

rf100.1 <- randomForest(Erosion ~.-total.depth - LSsurgo, data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
rf100.1 #99.97
pred100.1 <- predict(rf100.1, test)
RMSE.100.1 <- sqrt(mean((pred100.1 - test$Erosion)^2)) #0.13
varImpPlot(rf100.1)

rf100.2 <- randomForest(Erosion ~. - LSsurgo, data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
rf100.2 #99.95
pred100.2 <- predict(rf100.2, test)
RMSE.100.2 <- sqrt(mean((pred100.2 - test$Erosion)^2)) #0.15

rf100.3 <- randomForest(Erosion ~. - total.depth, data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
rf100.3 #99.96
pred100.3 <- predict(rf100.3, test)
RMSE.100.3 <- sqrt(mean((pred100.3 - test$Erosion)^2)) #0.15

rf100.4 <- randomForest(Erosion ~.-total.depth - LSsurgo - OM, data = train, ntree = 100, mtry = 7, importance = TRUE, do.trace = TRUE)
rf100.4 #99.97
pred100.4 <- predict(rf100.4, test)
RMSE.100.4 <- sqrt(mean((pred100.4 - test$Erosion)^2)) #0.13
png("modelFits/dairyRotationNoCover.png")
plot(pred100.4 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 100, mtry:7")
abline(a = 0, b = 1)
text(x = 5, y = 45, "RMSE = 0.13")
text(x = 6, y = 40, "% Var Explained = 99.9")
dev.off()
plot(rf100.4)
varImp(rf100.4)
varImpPlot(rf100.4)
importance(rf100.4)

rf100.5 <- randomForest(Erosion ~.-total.depth - LSsurgo - silt, data = train, ntree = 100, mtry = 7, importance = TRUE, do.trace = TRUE)
rf100.5 #99.97
pred100.5 <- predict(rf100.5, test)
RMSE.100.5 <- sqrt(mean((pred100.5 - test$Erosion)^2)) #0.13


# dairy rotation, cover crop
drnc <- snap %>% 
  filter(crop == "dr",
         cover == "nc") %>%
  select(c(Erosion, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = drcc$Erosion, p = 0.7, list = FALSE)
train <- drcc[inTrain,]
test <- drcc[-inTrain,]

rf75 <- randomForest(Erosion ~., data = train, ntree = 75, mtry = 8, importance = TRUE, do.trace = TRUE)
rf75 #99.94
pred75 <- predict(rf75, test)
RMSE75 <- sqrt(mean((pred75 - test$Erosion)^2)) #0.17
plot(pred75 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 75, mtry:8")

rf75.7 <- randomForest(Erosion ~., data = train, ntree = 75, mtry = 7, importance = TRUE, do.trace = TRUE)
rf75.7 #99.89
pred75.7 <- predict(rf75.7, test)
RMSE75.7 <- sqrt(mean((pred75.7 - test$Erosion)^2)) #.22
plot(pred75.7 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 75, mtry:7")


