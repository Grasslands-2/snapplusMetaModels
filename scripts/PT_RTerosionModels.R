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

ptrt <- snap %>%
  filter(crop == "pt",
         rotational == "rt") %>%
  dplyr::select(c(Erosion, slope, total_DM_lbs, slopelenusle.r, sand, silt, clay, k, OM, Contour)) %>%
  droplevels()

c1 <- ptrt %>% filter(Contour == "1")
c0 <- ptrt %>% filter(Contour == "0")
mean(c1$Erosion)
mean(c0$Erosion)
summary(ptrt)

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = ptrt$Erosion, p = 0.7, list = FALSE)
train <- ptrt[inTrain,]
test <- ptrt[-inTrain,]

best.guess <- mean(ptrt$Erosion) # for comparison
RMSE.baseline <- sqrt(mean((best.guess-train$Erosion)^2)) # for comparison

rf500_full <- randomForest(Erosion ~., data = train, ntree = 500, mtry = 5, importance = TRUE, do.trace = TRUE)
rf500_full # 99.83
plot(rf500_full)
varImp(rf500_full)
pred500full <- predict(rf500_full, test)
RMSE.500.full <- sqrt(mean((pred500full - test$Erosion)^2)) #0.0001
plot(pred500full ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 500, mtry: 5, all variables")

rf500 <- randomForest(Erosion ~.-Contour, data = train, ntree = 500, mtry = 5, importance = TRUE, do.trace = TRUE)
rf500 # 99.84
plot(rf500)
varImp(rf500)
pred500 <- predict(rf500, test)
RMSE.500 <- sqrt(mean((pred500 - test$Erosion)^2)) #0.0001
plot(pred500 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 500, mtry: 5, no Contour")

f500 <- randomForest(Erosion ~.-Contour - OM, data = train, ntree = 500, mtry = 5, importance = TRUE, do.trace = TRUE)
f500 # 99.91
plot(f500)
varImp(f500)
pred500f <- predict(f500, test)
RMSE.500f <- sqrt(mean((pred500f - test$Erosion)^2)) #0.0001
plot(pred500f ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 500, mtry: 5, no Contour, no OM")


rf100 <- randomForest(Erosion ~.-OM - Contour, data = train, ntree = 100, mtry = 5, importance = TRUE, do.trace = TRUE)
rf100 # 99.9
plot(rf100)
varImp(rf100)
pred100 <- predict(rf100, test)
RMSE.100 <- sqrt(mean((pred100 - test$Erosion)^2)) #0.0001
plot(pred100 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 100, mtry: 5, no OM, no Contour")


rf75 <- randomForest(Erosion ~.-OM - Contour, data = train, ntree = 75, mtry = 5, importance = TRUE, do.trace = TRUE)
rf75 # 99.9
plot(rf75)
varImp(rf75)
pred75 <- predict(rf75, test)
RMSE.75 <- sqrt(mean((pred75 - test$Erosion)^2)) #0.0001
plot(pred75 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 75, mtry: 5, no OM, no Contour")
abline(a = 0, b = 1)
text(x = 1, y = 12, "RMSE = 0.0001")
text(x = 2, y = 10, "% Var Explained = 99.9")

rf75.6 <- randomForest(Erosion ~.-OM - Contour, data = train, ntree = 75, mtry = 6, importance = TRUE, do.trace = TRUE)
rf75.6 # 99.96
plot(rf75.6)
varImp(rf75.6)
pred75.6 <- predict(rf75.6, test)
RMSE.75.6 <- sqrt(mean((pred75.6 - test$Erosion)^2)) #0.00007
png("modelFits/rotationalPasture.png")
plot(pred75 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "Rotational Pasture: trees: 75, mtry: 6")
abline(a = 0, b = 1)
text(x = 0.004, y = 0.017, "RMSE = 6.6E-5")
text(x = 0.004, y = 0.014, "% Var Explained = 99.96")
dev.off()

saveRDS(rf75.6, "models/RotationalPastureErosion.rds")
