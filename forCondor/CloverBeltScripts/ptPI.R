library(tidyverse)
library(MASS)
library(caret)

#load data
clark <- read_csv("../../clarkJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
#head(clark)
mara <- read_csv("../../maraJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
#head(mara)
taylor <- read_csv("../../taylorJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
# 
full <- bind_rows(clark, mara, taylor)
#head(full)

#head(full)
datR <- full %>%
  filter(crop == "pt", 
         Contour == "0") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, density, rotational, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  filter(OM < 20) %>%
  distinct() %>%
  droplevels()

summary(datR)

set.seed(0731)
inTrain <- createDataPartition(y = datR$PI, p = 0.7, list = FALSE)
trainR <- datR[inTrain,]
testR <- datR[-inTrain,]

train.control <- trainControl(method = "cv", number = 10) #cross validate 10 times

nointerx.lmR <- lm(PI~., data = trainR)
stepForwardR <- stepAIC(nointerx.lmR, scope = . ~ .^2, direction = "forward")
saveRDS(stepForwardR, "ptPIR_stepForward.rds")

allinterx.lmR <- lm(PI~.^2,data = trainR)
stepBackR <- stepAIC(allinterx.lmR, direction = "backward")
saveRDS(stepBackR, "ptPIR_stepBack.rds")


# without R ---------------------------------------------------------------


dat <- full %>%
  filter(crop == "pt", 
         Contour == "0") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, density, rotational, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k)) %>%
  filter(OM < 20) %>%
  distinct() %>%
  droplevels()

summary(dat)

set.seed(0731)
inTrain <- createDataPartition(y = dat$PI, p = 0.7, list = FALSE)
train <- dat[inTrain,]
test <- dat[-inTrain,]

nointerx.lm <- lm(PI~., data = train)
stepForward <- stepAIC(nointerx.lm, scope = . ~ .^2, direction = "forward")
saveRDS(stepForward, "ptPI_stepForward.rds")

allinterx.lm <- lm(PI~.^2,data = train)
stepBack <- stepAIC(allinterx.lm, direction = "backward")
saveRDS(stepBack, "ptPI_stepBack.rds")