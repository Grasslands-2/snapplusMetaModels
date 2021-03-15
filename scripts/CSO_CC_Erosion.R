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
csocc <- snap %>% 
  filter(crop == "cso",
         cover == "cc" 
         | cover == "gcis"
         | cover == "gcds") %>%
  select(c(Erosion, cover, tillage, slope, OM, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k, total.depth, LSsurgo)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()
# select(c(Erosion, tillage, slope, Contour, totalManureDM_lbs, slopelenusle.r, sand, silt, clay, k))
summary(csocc)

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = csocc$Erosion, p = 0.7, list = FALSE)
train <- csocc[inTrain,]
test <- csocc[-inTrain,]

best.guess <- round(mean(csocc$Erosion),2)
RMSE.baseline <- round(sqrt(mean((best.guess-train$Erosion)^2)),2)

rf100 <- randomForest(Erosion ~., data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
rf100 #99.98
pred_rf100 <- predict(rf100, test)
RMSE.100 <- sqrt(mean((pred_rf100 - test$Erosion)^2)) #0.08
varImp(rf100)
varImpPlot(rf100)
importance(rf100)

rf100.1 <- randomForest(Erosion ~. - LSsurgo, data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
rf100.1 #99.98
pred_rf100.1 <- predict(rf100.1, test)
RMSE.100.1 <- sqrt(mean((pred_rf100.1 - test$Erosion)^2)) #0.07
varImp(rf100)
varImpPlot(rf100.1)
importance(rf100)

rf100.2 <- randomForest(Erosion ~. - LSsurgo - silt, data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
rf100.2 #99.99
pred_rf100.2 <- predict(rf100.2, test)
RMSE.100.2 <- sqrt(mean((pred_rf100.2 - test$Erosion)^2)) #0.055

csocc <- snap %>% 
  filter(crop == "cso",
       cover == "cc" 
       | cover == "gcis"
       | cover == "gcds") %>%
  select(c(Erosion, cover, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = csocc$Erosion, p = 0.7, list = FALSE)
train <- csocc[inTrain,]
test <- csocc[-inTrain,]

rf100.3 <- randomForest(Erosion ~., data = train, ntree = 100, mtry = 8, importance = TRUE, do.trace = TRUE)
rf100.3 #99.99
plot(rf100.3)
pred_rf100.3 <- predict(rf100.3, test)
RMSE.100.3 <- sqrt(mean((pred_rf100.3 - test$Erosion)^2)) #0.045
varImp(rf100)
varImpPlot(rf100)
importance(rf100)

rf75 <- randomForest(Erosion ~., data = train, ntree = 75, mtry = 8, importance = TRUE, do.trace = TRUE)
rf75 #99.99
pred_rf75 <- predict(rf75, test)
RMSE.75 <- sqrt(mean((pred_rf75 - test$Erosion)^2)) #0.045
varImp(rf100)

rf50 <- randomForest(Erosion ~., data = train, ntree = 50, mtry = 8, importance = TRUE, do.trace = TRUE)
rf50 #99.99
pred_rf50 <- predict(rf50, test)
RMSE.50 <- sqrt(mean((pred_rf50 - test$Erosion)^2)) #0.046
saveRDS(rf50, "models/CSO_CoverErosion.rds")
png("modelFits/CSO_Cover_Erosion.png")
plot(pred_rf50 ~ test$Erosion, xlab = "Erosion", ylab = "Predicted Erosion", main = "trees: 50, mtry:8")
abline(a = 0, b = 1)
text(x = 5, y = 35, "RMSE = 0.05")
text(x = 6, y = 30, "% Var Explained = 99.9")
dev.off()
