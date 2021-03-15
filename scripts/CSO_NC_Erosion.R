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
csonc <- snap %>% 
  filter(crop == "cso",
         cover == "nc") %>% 
  select(c(Erosion, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

summary(csonc)

#partition data
set.seed(0731)
inTrain <- createDataPartition(y = csonc$Erosion, p = 0.7, list = FALSE)
train <- csonc[inTrain,]
test <- csonc[-inTrain,]

best.guess <- round(mean(csonc$Erosion),2)
RMSE.baseline <- round(sqrt(mean((best.guess-train$Erosion)^2)),2)

rf100 <- randomForest(Erosion ~ ., data = train, ntree = 100, mtry = 7, importance = TRUE, do.trace = TRUE)
rf100 #99.97
pred_rf100 <- predict(rf100, test)
RMSE.100 <- sqrt(mean((pred_rf100 - test$Erosion)^2)) #0.16

rf75 <- randomForest(Erosion ~ ., data = train, ntree = 75, mtry = 7, importance = TRUE, do.trace = TRUE)
rf75 #99.97
pred_rf75 <- predict(rf75, test)
RMSE.75 <- sqrt(mean((pred_rf75 - test$Erosion)^2)) #0.16
