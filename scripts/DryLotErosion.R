#load libraries
library(tidyverse)
library(caret)
library(randomForest)

#load data
snap <- read.table("data/JoinedSnapSurgo.txt", sep = "|", header = TRUE)
snap <- snap %>%
  mutate_if(is.character, as.factor)
summary(snap)

# dry lot
dl <- snap %>% filter(crop == "dl") %>% 
  select(c(Erosion, density, slope, slopelenusle.r, OM, sand, silt, clay, k, total.depth, LSsurgo, total_DM_lbs)) %>%
  droplevels()

summary(dl)

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = dl$Erosion, p = 0.7, list = FALSE)
train <- dl[inTrain,]
test <- dl[-inTrain,]

best.guess <- mean(dl$Erosion) # for comparison
RMSE.baseline <- sqrt(mean((best.guess-train$Erosion)^2)) # for comparison

rf500_full <- randomForest(Erosion ~., data = train, ntree = 500, mtry = 5, importance = TRUE, do.trace = TRUE)
rf500_full # 98.32
plot(rf500_full)
varImp(rf500_full)
importance(rf500_full)
pred500full <- predict(rf500_full, test)
RMSE.500.full <- sqrt(mean((pred500full - test$Erosion)^2)) #1.3

rf250 <- randomForest(Erosion ~.-total.depth, data = train, ntree = 250, mtry = 5, importance = TRUE, do.trace = TRUE)
rf250 # 98.55
plot(rf250)
varImp(rf250)
importance(rf250)
pred250 <- predict(rf250, test)
RMSE.250 <- sqrt(mean((pred250 - test$Erosion)^2)) #1.18

rf250.1 <- randomForest(Erosion ~.-total.depth-OM, data = train, ntree = 250, mtry = 5, importance = TRUE, do.trace = TRUE)
rf250.1 # 98.59
plot(rf250.1)
varImp(rf250.1)
importance(rf250.1)
pred250.1 <- predict(rf250.1, test)
RMSE.250.1 <- sqrt(mean((pred250.1 - test$Erosion)^2)) #1.17

rf250.2 <- randomForest(Erosion ~.-total.depth-OM-total_DM_lbs, data = train, ntree = 250, mtry = 5, importance = TRUE, do.trace = TRUE)
rf250.2 # 98.78
plot(rf250.2)
varImp(rf250.2)
importance(rf250.2)
pred250.2 <- predict(rf250.2, test)
RMSE.250.2 <- sqrt(mean((pred250.2 - test$Erosion)^2)) #1.02

rf250.3 <- randomForest(Erosion ~.-total.depth-OM-total_DM_lbs-LSsurgo, data = train, ntree = 250, mtry = 5, importance = TRUE, do.trace = TRUE)
rf250.3 # 98.24
plot(rf250.3)
varImp(rf250.3)
importance(rf250.3)
pred250.3 <- predict(rf250.3, test)
RMSE.250.3 <- sqrt(mean((pred250.3 - test$Erosion)^2)) #1.14


rf250.4 <- randomForest(Erosion ~.-total.depth-OM-total_DM_lbs-k, data = train, ntree = 250, mtry = 5, importance = TRUE, do.trace = TRUE)
rf250.4 # 98.48
plot(rf250.4)
varImp(rf250.4)
importance(rf250.4)
pred250.4 <- predict(rf250.4, test)
RMSE.250.4 <- sqrt(mean((pred250.4 - test$Erosion)^2)) #1.28

rf100 <- randomForest(Erosion ~.-total.depth-OM-total_DM_lbs, data = train, ntree = 100, mtry = 5, importance = TRUE, do.trace = TRUE)
rf100 # 98.72
plot(rf100)
varImp(rf100)
importance(rf100)
pred100 <- predict(rf100, test)
RMSE.100 <- sqrt(mean((pred100 - test$Erosion)^2)) #1.02

rf75 <- randomForest(Erosion ~.-total.depth-OM-total_DM_lbs, data = train, ntree = 75, mtry = 5, importance = TRUE, do.trace = TRUE)
rf75 # 98.67
plot(rf75)
varImp(rf75)
importance(rf75)
pred75 <- predict(rf75, test)
RMSE.75 <- sqrt(mean((pred75 - test$Erosion)^2)) #1.1

rf75.6 <- randomForest(Erosion ~.-total.depth-OM-total_DM_lbs-LSsurgo, data = train, ntree = 75, mtry = 6, importance = TRUE, do.trace = TRUE)
rf75.6 # 99.06
plot(rf75.6)
varImp(rf75.6)
importance(rf75.6)
pred75.6 <- predict(rf75.6, test)
RMSE.75.6 <- sqrt(mean((pred75.6 - test$Erosion)^2)) #0.93


png("modelFits/DryLotErosion.png")
plot(pred75.6 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "Dry Lot: trees: 75, mtry: 6")
abline(a = 0, b = 1)
text(x = 10, y = 50, "RMSE = 0.93")
text(x = 10, y = 45, "% Var Explained = 99.06")
dev.off()

saveRDS(rf75.6, "models/DryLotErosion.rds")
