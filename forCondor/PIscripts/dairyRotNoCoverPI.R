library(tidyverse)
library(MASS)
library(caret)

dat <- read_csv("DairyRotNoCoverPI.csv") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour))

summary(dat)

set.seed(0731)
inTrain <- createDataPartition(y = dat$PI, p = 0.7, list = FALSE)
train <- dat[inTrain,]
test <- dat[-inTrain,]

train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times

nointerx.lm <- lm(PI~., data = train)
stepForward <- stepAIC(nointerx.lm, scope = . ~ .^2, direction = "forward")
saveRDS(stepForward, "dairyRotNoCoverPI_stepForward.rds")

allinterx.lm <- lm(PI~.^2,data = train)
stepBack <- stepAIC(allinterx.lm, direction = "backward")
saveRDS(stepBack, "dairyRotNoCoverPI_stepBack.rds")