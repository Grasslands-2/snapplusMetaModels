#load libraries
library(tidyverse)
library(randomForest)
library(tidymodels)
library(caret)

#load data
cc <- read_csv("forCondor/curveData/ccCurve.csv")

cc <- cc %>% 
  mutate_if(is.character, factor) 

summary(cc)

# encode dummy vars
# ddum <- dummyVars("~.", data = cc) 
# ccDum <- data.table(predict(ddum, newdata = cc)) 
# rm(ddum) #

#partition data
set.seed(123)
split <- initial_split(cc, strata = ffCN)
train <- training(split)
test <- testing(split)

best.guess <- round(mean(cc$ffCN),2)
RMSE.baseline <- round(sqrt(mean((best.guess-train$ffCN)^2)),2)

# library(VSURF)
# library(data.table)
# set.seed(0731)
# 
# 
# surf.mod <- VSURF(y = train$ffCN, x= train[,2:28], ntree = 50)


set.seed(234)
rf50 <- randomForest(ffCN ~., data = train, ntree = 50, importance = TRUE, do.trace = TRUE)
rf50 #99.69
pred <- predict(rf50, test)
RMSE1 <- sqrt(mean((pred - test$ffCN)^2))
plot(pred ~ test$ffCN)
varImpPlot(rf50)

set.seed(345)
rf50_2 <- randomForest(ffCN ~.- slope, data = train, ntree = 50, mtry = 4, importance = TRUE, do.trace = TRUE)
rf50_2 #99.76 # 
pred_2 <- predict(rf50_2, test)
RMSE_2 <- sqrt(mean((pred_2 - test$ffCN)^2))
plot(pred_2 ~ test$ffCN)
varImpPlot(rf50_2)

set.seed(456)
rf50_3 <- randomForest(ffCN ~.- LSsurgo, data = train, mtry = 4, ntree = 50, importance = TRUE, do.trace = TRUE)
rf50_3 #99.79
pred_3 <- predict(rf50_3, test)
RMSE_3 <- sqrt(mean((pred_3 - test$ffCN)^2))
plot(pred_3 ~ test$ffCN)
varImpPlot(rf50_3)

set.seed(567)
rf50_4 <- randomForest(ffCN ~.- LSsurgo - slope, data = train, mtry = 4, ntree = 50, importance = TRUE, do.trace = TRUE)
rf50_4 #99.79
pred_4 <- predict(rf50_4, test)
RMSE_4 <- sqrt(mean((pred_4 - test$ffCN)^2))
plot(pred_4 ~ test$ffCN)
varImpPlot(rf50_4)

set.seed(678)
rf50_5 <- randomForest(ffCN ~.- LSsurgo - slope - k, data = train, mtry = 4, ntree = 50, importance = TRUE, do.trace = TRUE)
rf50_5 #99.89
pred_5 <- predict(rf50_5, test)
RMSE_5 <- sqrt(mean((pred_5 - test$ffCN)^2))
plot(pred_5 ~ test$ffCN)
varImpPlot(rf50_5)

set.seed(789)
rf50_6 <- randomForest(ffCN ~.- LSsurgo - slope - k - OM, data = train, mtry = 4, ntree = 50, importance = TRUE, do.trace = TRUE)
rf50_6 #99.96
pred_6 <- predict(rf50_6, test)
RMSE_6 <- sqrt(mean((pred_6 - test$ffCN)^2))
plot(pred_6 ~ test$ffCN)
varImpPlot(rf50_6)

set.seed(890)
rf50_7 <- randomForest(ffCN ~.- LSsurgo - slope - k - OM - slopelenusle.r, data = train, mtry = 4, ntree = 50, importance = TRUE, do.trace = TRUE)
rf50_7 #99.97
pred_7 <- predict(rf50_7, test)
RMSE_7 <- sqrt(mean((pred_7 - test$ffCN)^2))
plot(pred_7 ~ test$ffCN)
varImpPlot(rf50_7)

set.seed(910)
rf50_8 <- randomForest(ffCN ~.- slope - k - OM - slopelenusle.r, data = train, mtry = 4, ntree = 50, importance = TRUE, do.trace = TRUE)
rf50_8 #99.95
pred_8 <- predict(rf50_8, test)
RMSE_8 <- sqrt(mean((pred_8 - test$ffCN)^2))
plot(pred_8 ~ test$ffCN)
varImpPlot(rf50_7)

set.seed(101)
rf50_9 <- randomForest(ffCN ~.- LSsurgo - slope - k - slopelenusle.r, data = train, mtry = 4, ntree = 50, importance = TRUE, do.trace = TRUE)
rf50_9 #99.94
pred_9 <- predict(rf50_9, test)
RMSE_9 <- sqrt(mean((pred_9 - test$ffCN)^2))
plot(pred_9 ~ test$ffCN)
varImpPlot(rf50_7)


#### CHOSE Mod 7
library(plotrix)
library(tree)
library(reprtree)

reprtree:::plot.getTree(rf50)




pred.all <- predict(rf50, test, predict.all = TRUE)
pred.rf.int <- t(apply( pred.all$individual, 1, function(x){ 
  c( mean(x) + c(-1.96,1.96)*sd(x), quantile(x, c(0.025,0.975)) )}))
pred.rf.int
#https://stats.stackexchange.com/questions/56895/do-the-predictions-of-a-random-forest-model-have-a-prediction-interval
newdata = test[1,]
pred.test <- predict(rf50, newdata, predict.all = TRUE)
mean.rf <- pred.test$aggregate
sd.rf <- mean(sqrt(rf50$mse))
sd <- sd(pred.test$individual)
pred.rf.int3 <- cbind(mean.rf - 1.96*sd, mean.rf + 1.96*sd)
pred.rf.int3
pred.test


pred <-  predict(rf50, test)
RMSE <- sqrt(mean((pred - test$ffCN)^2)) #0.07
varImp(rf50)
varImpPlot(rf50)
importance(rf50)
plot(pred1 ~ test$ffCN)

set.seed(345)
rf50.1 <- randomForest(ffCN ~.-total.depth, data = train, ntree = 50, mtry = 7, importance = TRUE, do.trace = TRUE)
rf50.1 #99.77
pred.all.1 <- predict(rf50.1, test, predict.all = TRUE)
pred.1 <-  predict(rf50.1, test)
RMSE.1 <- sqrt(mean((pred.1 - test$ffCN)^2)) #0.07
varImp(rf50.1)
varImpPlot(rf50.1)
importance(rf50.1)
plot(pred.1 ~ test$ffCN)

set.seed(456)
rf50.2 <- randomForest(ffCN ~.-total.depth - slope, data = train, ntree = 50, mtry = 7, importance = TRUE, do.trace = TRUE)
rf50.2 #99.69
pred.all.2 <- predict(rf50.2, test, predict.all = TRUE)
pred.2 <-  predict(rf50.2, test)
RMSE.2 <- sqrt(mean((pred.2 - test$ffCN)^2)) #0.07
varImp(rf50.1)
varImpPlot(rf50.2)
importance(rf50.1)
plot(pred.2 ~ test$ffCN)

test <- test %>%
  mutate(pred = pred) %>%
  mutate(dif = pred/ffCN)



dlCurve <- read_csv("forCondor/curveData/dlCurve.csv")

set.seed(123)
split <- initial_split(dlCurve, strata = ffCN)
train <- training(split)
test <- testing(split)

best.guess <- round(mean(dlCurve$ffCN),2)
RMSE.baseline <- round(sqrt(mean((best.guess-train$ffCN)^2)),2)

rf <- randomForest(ffCN ~ hydgrp + total_DM_lbs + density + sand +  clay + silt, data = train, mtry = 4, ntree = 50, importance  = TRUE, do.trace = TRUE)
rf

pred <- predict(rf, test)
test <- test %>%
  mutate(pred = pred) %>%
  select(-c(ManureApp:County, rotational, initialP, SoilT:SCI, mukey:cokey)) %>%
  mutate(diff = pred/ffCN)
RMSE <- sqrt(mean((pred - test$ffCN)^2))
plot(pred ~ test$ffCN)
varImpPlot(rf)

set.seed(123)
split <- initial_split(dlCurve, prop = 0.9, strata = ffCN)
train <- training(split)
test <- testing(split)

rf <- randomForest(ffCN ~ hydgrp + total_DM_lbs + density + sand + k + clay + silt + slopelenusle.r, data = train, ntree = 50, importance  = TRUE, do.trace = TRUE)
rf
pred <- predict(rf, test)
test <- test %>%
  mutate(pred = pred) %>%
  select(-c(ManureApp:County, rotational, initialP, SoilT:SCI, mukey:cokey)) %>%
  mutate(diff = pred/ffCN)
RMSE <- sqrt(mean((pred - test$ffCN)^2))
plot(pred ~ test$ffCN)
varImpPlot(rf)

write.csv(test, "test.csv", row.names = FALSE, quote = FALSE)
