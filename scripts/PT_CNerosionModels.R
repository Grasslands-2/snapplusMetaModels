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

pt_cn <- snap %>%
  filter(crop == "pt",
         rotational == "cn") %>%
  dplyr::select(c(Erosion, density, slope, total_DM_lbs, slopelenusle.r, sand, silt, clay, k, OM)) %>%
  droplevels()

c1 <- pt_cn %>% filter(Contour == "1")
c0 <- pt_cn %>% filter(Contour == "0")
mean(c1$Erosion)
mean(c0$Erosion)
summary(pt_cn)

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = pt_cn$Erosion, p = 0.7, list = FALSE)
ptcn_train <- pt_cn[inTrain,]
ptcn_test <- pt_cn[-inTrain,]

best.guess <- round(mean(pt_cn$Erosion),2) # for comparison
RMSE.baseline <- round(sqrt(mean((best.guess-ptcn_train$Erosion)^2)),2) # for comparison

rf500_full <- randomForest(Erosion ~., data = ptcn_train, ntree = 500, mtry = 5, importance = TRUE, do.trace = TRUE)
rf500_full # 99.96
plot(rf500_full)
varImp(rf500_full)
pred500full <- predict(rf500_full, ptcn_test)
RMSE.500.full <- sqrt(mean((pred500full - ptcn_test$Erosion)^2)) #0.05
plot(pred500full ~ ptcn_test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 500, mtry: 5, all variables")
abline(a = 0, b = 1)
text(x = 1, y = 12, "RMSE = 0.05")
text(x = 2, y = 10, "% Var Explained = 99.96")

rf500 <- randomForest(Erosion ~.-OM, data = ptcn_train, ntree = 500, mtry = 5, importance = TRUE, do.trace = TRUE)
rf500 # 99.99
plot(rf500)
varImp(rf500)
pred500 <- predict(rf500, ptcn_test)
RMSE.500 <- sqrt(mean((pred500 - ptcn_test$Erosion)^2)) #0.03
plot(pred500 ~ ptcn_test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 500, mtry: 5, no OM")
abline(a = 0, b = 1)
text(x = 1, y = 12, "RMSE = 0.03")
text(x = 2, y = 10, "% Var Explained = 99.99")

rf100 <- randomForest(Erosion ~.-OM, data = ptcn_train, ntree = 100, mtry = 5, importance = TRUE, do.trace = TRUE)
rf100 # 99.98
plot(rf100)
varImp(rf100)
pred100 <- predict(rf100, ptcn_test)
RMSE.100 <- sqrt(mean((pred100 - ptcn_test$Erosion)^2)) #0.04
plot(pred100 ~ ptcn_test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 100, mtry: 5, no OM")
abline(a = 0, b = 1)
text(x = 1, y = 12, "RMSE = 0.04")
text(x = 2, y = 10, "% Var Explained = 99.98")

rf75 <- randomForest(Erosion ~.-OM, data = ptcn_train, ntree = 75, mtry = 5, importance = TRUE, do.trace = TRUE)
rf75 # 99.97
plot(rf75)
varImp(rf75)
pred75 <- predict(rf75, ptcn_test)
RMSE.75 <- sqrt(mean((pred75 - ptcn_test$Erosion)^2)) #0.04
plot(pred75 ~ ptcn_test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 75, mtry: 5, no OM")
abline(a = 0, b = 1)
text(x = 1, y = 12, "RMSE = 0.04")
text(x = 2, y = 10, "% Var Explained = 99.97")

rf75.6 <- randomForest(Erosion ~.-OM, data = ptcn_train, ntree = 75, mtry = 6, importance = TRUE, do.trace = TRUE)
rf75.6 # 99.97
plot(rf75.6)
varImp(rf75.6)
pred75.6 <- predict(rf75.6, ptcn_test)
RMSE.75.6 <- sqrt(mean((pred75.6 - ptcn_test$Erosion)^2)) #0.04
png("modelFits/continuousPasture.png")
plot(pred75 ~ ptcn_test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "Continuous Pasture: trees: 75, mtry: 6")
abline(a = 0, b = 1)
text(x = 1, y = 12, "RMSE = 0.01")
text(x = 2, y = 10, "% Var Explained = 100")
dev.off()

saveRDS(rf75.6, "models/ContinuousPastureErosion.rds")
