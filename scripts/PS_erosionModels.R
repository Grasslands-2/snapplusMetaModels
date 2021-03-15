#load libraries
library(tidyverse)
library(caret)
library(randomForest)

#load data
snap <- read.table("data/JoinedSnapSurgo.txt", sep = "|", header = TRUE)
snap <- snap %>%
  mutate_if(is.character, as.factor)
summary(snap)


# continuous pasture ------------------------------------------------------

ps <- snap %>%
  filter(crop == "ps") %>%
  mutate(Contour = as.factor(Contour)) %>%
  dplyr::select(c(Erosion, tillage, slope, OM, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k, total.depth, LSsurgo)) %>%
  droplevels()

summary(ps)

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = ps$Erosion, p = 0.7, list = FALSE)
train <- ps[inTrain,]
test <- ps[-inTrain,]

best.guess <- mean(ps$Erosion) # for comparison
RMSE.baseline <- sqrt(mean((best.guess-train$Erosion)^2)) # for comparison

rf500_full <- randomForest(Erosion ~., data = train, ntree = 500, mtry = 5, importance = TRUE, do.trace = TRUE)
rf500_full # 99.78
plot(rf500_full)
varImp(rf500_full)
importance(rf500_full)
pred500full <- predict(rf500_full, test)
RMSE.500.full <- sqrt(mean((pred500full - test$Erosion)^2)) #0.33

rf250 <- randomForest(Erosion ~.-total_DM_lbs, data = train, ntree = 250, mtry = 5, importance = TRUE, do.trace = TRUE)
rf250 # 99.69
plot(rf250)
varImp(rf250)
importance(rf250)
pred250 <- predict(rf250, test)
RMSE.250 <- sqrt(mean((pred250 - test$Erosion)^2)) #0.39

rf250.1 <- randomForest(Erosion ~.-LSsurgo, data = train, ntree = 250, mtry = 5, importance = TRUE, do.trace = TRUE)
rf250.1 # 99.84
plot(rf250.1)
varImp(rf250.1)
importance(rf250.1)
pred250.1 <- predict(rf250.1, test)
RMSE.250.1 <- sqrt(mean((pred250.1 - test$Erosion)^2)) #0.28

rf250.2 <- randomForest(Erosion ~.-LSsurgo - total.depth, data = train, ntree = 250, mtry = 5, importance = TRUE, do.trace = TRUE)
rf250.2 # 99.91
plot(rf250.2)
varImp(rf250.2)
importance(rf250.2)
pred250.2 <- predict(rf250.2, test)
RMSE.250.2 <- sqrt(mean((pred250.2 - test$Erosion)^2)) #0.20

rf250.3 <- randomForest(Erosion ~.-LSsurgo - total.depth - OM, data = train, ntree = 250, mtry = 5, importance = TRUE, do.trace = TRUE)
rf250.3 # 99.95
plot(rf250.3)
varImp(rf250.3)
importance(rf250.3)
pred250.3 <- predict(rf250.3, test)
RMSE.250.3 <- sqrt(mean((pred250.3 - test$Erosion)^2)) #0.15
plot(pred250.3 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion")

rf100 <- randomForest(Erosion ~.-LSsurgo - total.depth - OM, data = train, ntree = 100, mtry = 5, importance = TRUE, do.trace = TRUE)
rf100 # 99.95
plot(rf100)
varImp(rf100)
pred100 <- predict(rf100, test)
RMSE.100 <- sqrt(mean((pred100 - test$Erosion)^2)) #0.16
plot(pred100 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 100, mtry: 5, no OM, no Contour")


rf75 <- randomForest(Erosion ~.-LSsurgo - total.depth - OM, data = train, ntree = 75, mtry = 5, importance = TRUE, do.trace = TRUE)
rf75 # 99.95
plot(rf75)
varImp(rf75)
pred75 <- predict(rf75, test)
RMSE.75 <- sqrt(mean((pred75 - test$Erosion)^2)) #0.16
plot(pred75 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 75, mtry:5")
abline(a = 0, b = 1)
text(x = 1, y = 12, "RMSE = 0.0001")
text(x = 2, y = 10, "% Var Explained = 99.9")

rf75.6 <- randomForest(Erosion ~.-LSsurgo - total.depth - OM, data = train, ntree = 75, mtry = 6, importance = TRUE, do.trace = TRUE)
rf75.6 # 99.98
plot(rf75.6)
varImp(rf75.6)
pred75.6 <- predict(rf75.6, test)
RMSE.75.6 <- sqrt(mean((pred75.6 - test$Erosion)^2)) #0.09
png("modelFits/pastureSeeding.png")
plot(pred75.6 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "Pasture Seeding: trees: 75, mtry: 6")
abline(a = 0, b = 1)
text(x = 10, y = 50, "RMSE = 0.09")
text(x = 10, y = 45, "% Var Explained = 99.98")
dev.off()

saveRDS(rf75.6, "models/PastureSeedingErosion.rds")
