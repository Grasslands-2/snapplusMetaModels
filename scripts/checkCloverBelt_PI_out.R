# this script compares PI model output from clover belt snapplus runs
# and creates final model for use in grazescape/smartscape

#load libraries
library(tidyverse)
library(tidymodels)
library(MASS)
library(caret)


#load data
clark <- read_csv("data/clarkJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
mara <- read_csv("data/maraJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
taylor <- read_csv("data/taylorJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
full <- bind_rows(clark, mara, taylor)
rm(list = c("clark", "mara", "taylor"))


# cont corn ---------------------------------------------------------------

#clean data
cc <- full %>%
  filter(crop == "cc") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  filter(OM < 20) %>%
  distinct() %>%
  droplevels()

summary(cc)


# load models -------------------------------------------------------------

# *without R --------------------------------------------------------------

ccBack <- readRDS("modelsFromCondor/CloverBeltMods/lowPI/ccPI_stepBack_20OM.rds")

ccBack$anova

# null hypothesis
cc.best.guess <- round(mean(cc$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cc$PI, p = 0.7, list = FALSE)
train <- cc[inTrain,]
test <- cc[-inTrain,]

# Evaluate RMSE
cc.RMSE.baseline <- round(sqrt(mean((cc.best.guess-test$PI)^2)),2)

#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

stepBackTidy <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
        Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
        Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
        Erosion:total.depth + Erosion:OM + Erosion:silt + Erosion:k + 
        cover:tillage + cover:Contour + cover:initialP + cover:total_DM_lbs + 
        cover:totalP2O5_lbs + cover:slope + cover:slopelenusle.r + 
        cover:LSsurgo + cover:OM + cover:silt + cover:k + tillage:Contour + 
        tillage:initialP + tillage:total_DM_lbs + tillage:totalP2O5_lbs + 
        tillage:slope + tillage:slopelenusle.r + tillage:LSsurgo + 
        tillage:total.depth + tillage:OM + tillage:silt + tillage:k + 
        Contour:initialP + Contour:total_DM_lbs + Contour:totalP2O5_lbs + 
        Contour:slope + Contour:LSsurgo + Contour:silt + Contour:k + 
        initialP:total_DM_lbs + initialP:slope + initialP:slopelenusle.r + 
        initialP:LSsurgo + initialP:total.depth + initialP:OM + initialP:silt + 
        initialP:k + total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + 
        total_DM_lbs:slopelenusle.r + total_DM_lbs:LSsurgo + total_DM_lbs:total.depth + 
        total_DM_lbs:OM + total_DM_lbs:silt + total_DM_lbs:k + totalP2O5_lbs:slope + 
        totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:LSsurgo + totalP2O5_lbs:total.depth + 
        totalP2O5_lbs:OM + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
        slope:slopelenusle.r + slope:LSsurgo + slope:total.depth + 
        slope:OM + slope:silt + slope:k + slopelenusle.r:LSsurgo + 
        slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
        slopelenusle.r:k + LSsurgo:total.depth + LSsurgo:OM + LSsurgo:k + 
        total.depth:OM + total.depth:silt + total.depth:k + OM:silt + 
        OM:k + silt:k,
      data = train)

cc_pred <- stepBackTidy %>%
  predict(train) %>%
  bind_cols(train)

ggplot(cc_pred, aes(x = PI, y = .pred)) +
  geom_point()

cc_rmse <- round(sqrt(mean((cc_pred$.pred - cc_pred$PI)^2)),3)
#0.316

saveRDS(stepBackTidy, "models/cloverBelt_cc_PI.RDS")

# *with R --------------------------------------------------------------

ccBackR <- readRDS("modelsFromCondor/CloverBeltMods/PI/ccPIR_stepBack.rds")

ccBackR$anova

# null hypothesis
cc.best.guess <- round(mean(cc$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cc$PI, p = 0.7, list = FALSE)
train <- cc[inTrain,]
test <- cc[-inTrain,]

# Evaluate RMSE
cc.RMSE.baseline <- round(sqrt(mean((cc.best.guess-test$PI)^2)),2)

#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

stepBackTidyR <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + R_factor + Erosion:cover + Erosion:tillage + 
        Erosion:Contour + Erosion:initialP + Erosion:total_DM_lbs + 
        Erosion:totalP2O5_lbs + Erosion:slope + Erosion:slopelenusle.r + 
        Erosion:LSsurgo + Erosion:total.depth + Erosion:OM + Erosion:silt + 
        Erosion:k + Erosion:R_factor + cover:tillage + cover:Contour + 
        cover:initialP + cover:total_DM_lbs + cover:slope + cover:slopelenusle.r + 
        cover:LSsurgo + cover:total.depth + cover:OM + cover:k + 
        cover:R_factor + tillage:Contour + tillage:initialP + tillage:total_DM_lbs + 
        tillage:totalP2O5_lbs + tillage:slope + tillage:slopelenusle.r + 
        tillage:LSsurgo + tillage:total.depth + tillage:OM + tillage:silt + 
        tillage:k + tillage:R_factor + Contour:initialP + Contour:total_DM_lbs + 
        Contour:slope + Contour:slopelenusle.r + Contour:LSsurgo + 
        Contour:total.depth + Contour:OM + Contour:k + Contour:R_factor + 
        initialP:total_DM_lbs + initialP:slope + initialP:slopelenusle.r + 
        initialP:LSsurgo + initialP:total.depth + initialP:OM + initialP:silt + 
        initialP:k + initialP:R_factor + total_DM_lbs:totalP2O5_lbs + 
        total_DM_lbs:slope + total_DM_lbs:LSsurgo + total_DM_lbs:total.depth + 
        total_DM_lbs:k + total_DM_lbs:R_factor + totalP2O5_lbs:slope + 
        totalP2O5_lbs:OM + totalP2O5_lbs:silt + totalP2O5_lbs:R_factor + 
        slope:slopelenusle.r + slope:LSsurgo + slope:total.depth + 
        slope:OM + slope:silt + slope:k + slope:R_factor + slopelenusle.r:LSsurgo + 
        slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
        slopelenusle.r:k + slopelenusle.r:R_factor + LSsurgo:total.depth + 
        LSsurgo:OM + LSsurgo:silt + LSsurgo:k + LSsurgo:R_factor + 
        total.depth:OM + total.depth:silt + total.depth:k + total.depth:R_factor + 
        OM:silt + OM:k + OM:R_factor + silt:k + silt:R_factor + k:R_factor,
      data = train)

cc_predR <- stepBackTidyR %>%
  predict(train) %>%
  bind_cols(train)

ggplot(cc_predR, aes(x = PI, y = .pred)) +
  geom_point()

ccR_rmse <- round(sqrt(mean((cc_predR$.pred - cc_predR$PI)^2)),3)


# corn grain --------------------------------------------------------------


#clean data
cg <- full %>%
  filter(crop == "cg") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  filter(OM < 20) %>%
  distinct() %>%
  droplevels()

# null hypothesis
cg.best.guess <- round(mean(cg$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cg$PI, p = 0.7, list = FALSE)
train <- cg[inTrain,]
test <- cg[-inTrain,]

# Evaluate RMSE
cg.RMSE.baseline <- round(sqrt(mean((cg.best.guess-test$PI)^2)),2)


# with R ------------------------------------------------------------------

cgR_mod <- readRDS("modelsFromCondor/CloverBeltMods/PI/cgPIR_stepBack.rds")

cgR_mod$anova

stepBackTidy_cgR <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + R_factor + Erosion:cover + Erosion:tillage + 
        Erosion:Contour + Erosion:initialP + Erosion:total_DM_lbs + 
        Erosion:totalP2O5_lbs + Erosion:slope + Erosion:slopelenusle.r + 
        Erosion:LSsurgo + Erosion:total.depth + Erosion:OM + Erosion:silt + 
        Erosion:k + Erosion:R_factor + cover:tillage + cover:Contour + 
        cover:initialP + cover:totalP2O5_lbs + cover:slope + cover:slopelenusle.r + 
        cover:LSsurgo + cover:total.depth + cover:OM + cover:silt + 
        cover:k + cover:R_factor + tillage:Contour + tillage:initialP + 
        tillage:total_DM_lbs + tillage:totalP2O5_lbs + tillage:slope + 
        tillage:slopelenusle.r + tillage:LSsurgo + tillage:total.depth + 
        tillage:OM + tillage:silt + tillage:k + tillage:R_factor + 
        Contour:initialP + Contour:total_DM_lbs + Contour:slope + 
        Contour:slopelenusle.r + Contour:LSsurgo + Contour:total.depth + 
        Contour:OM + Contour:silt + Contour:k + Contour:R_factor + 
        initialP:total_DM_lbs + initialP:slope + initialP:slopelenusle.r + 
        initialP:LSsurgo + initialP:total.depth + initialP:OM + initialP:silt + 
        initialP:k + initialP:R_factor + total_DM_lbs:totalP2O5_lbs + 
        total_DM_lbs:slope + total_DM_lbs:LSsurgo + total_DM_lbs:total.depth + 
        total_DM_lbs:k + total_DM_lbs:R_factor + totalP2O5_lbs:slope + 
        totalP2O5_lbs:LSsurgo + totalP2O5_lbs:OM + totalP2O5_lbs:silt + 
        totalP2O5_lbs:R_factor + slope:slopelenusle.r + slope:LSsurgo + 
        slope:OM + slope:silt + slope:k + slope:R_factor + slopelenusle.r:LSsurgo + 
        slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
        slopelenusle.r:k + slopelenusle.r:R_factor + LSsurgo:total.depth + 
        LSsurgo:OM + LSsurgo:silt + LSsurgo:k + LSsurgo:R_factor + 
        total.depth:OM + total.depth:silt + total.depth:k + total.depth:R_factor + 
        OM:silt + OM:k + OM:R_factor + silt:k + k:R_factor,
      data = train)

cg_predR <- stepBackTidy_cgR %>%
  predict(train) %>%
  bind_cols(train)

ggplot(cg_predR, aes(x = PI, y = .pred)) +
  geom_point()

cgR_rmse <- round(sqrt(mean((cg_predR$.pred - cg_predR$PI)^2)),3)


# without R ---------------------------------------------------------------

cg_mod <- readRDS("modelsFromCondor/CloverBeltMods/lowPI/cgPI_stepBack.rds")

cg_mod$anova

stepBackTidy_cg <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
        Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
        Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
        Erosion:total.depth + Erosion:OM + Erosion:silt + Erosion:k + 
        cover:tillage + cover:Contour + cover:initialP + cover:total_DM_lbs + 
        cover:totalP2O5_lbs + cover:slope + cover:slopelenusle.r + 
        cover:LSsurgo + cover:OM + cover:silt + cover:k + tillage:Contour + 
        tillage:initialP + tillage:total_DM_lbs + tillage:totalP2O5_lbs + 
        tillage:slope + tillage:slopelenusle.r + tillage:LSsurgo + 
        tillage:total.depth + tillage:OM + tillage:silt + tillage:k + 
        Contour:initialP + Contour:total_DM_lbs + Contour:slope + 
        Contour:LSsurgo + Contour:OM + Contour:silt + Contour:k + 
        initialP:total_DM_lbs + initialP:slope + initialP:LSsurgo + 
        initialP:total.depth + initialP:OM + initialP:silt + initialP:k + 
        total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + total_DM_lbs:LSsurgo + 
        total_DM_lbs:total.depth + total_DM_lbs:OM + total_DM_lbs:k + 
        totalP2O5_lbs:slope + totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:LSsurgo + 
        totalP2O5_lbs:total.depth + totalP2O5_lbs:OM + totalP2O5_lbs:silt + 
        totalP2O5_lbs:k + slope:slopelenusle.r + slope:LSsurgo + 
        slope:total.depth + slope:OM + slope:silt + slope:k + slopelenusle.r:LSsurgo + 
        slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
        slopelenusle.r:k + LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + 
        LSsurgo:k + total.depth:OM + total.depth:silt + total.depth:k + 
        OM:silt + OM:k + silt:k,
      data = train)

cg_pred <- stepBackTidy_cg %>%
  predict(train) %>%
  bind_cols(train)

ggplot(cg_pred, aes(x = PI, y = .pred)) +
  geom_point()

cg_rmse <- round(sqrt(mean((cg_pred$.pred - cg_pred$PI)^2)),3)
#0.393

saveRDS(stepBackTidy_cg, "models/cloverBelt_cg_PI.RDS")


# corn soy oats -----------------------------------------------------------

#clean data
cso <- full %>%
  filter(crop == "cso") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  filter(OM < 20) %>%
  distinct() %>%
  droplevels()

# null hypothesis
cso.best.guess <- round(mean(cso$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cso$PI, p = 0.7, list = FALSE)
train <- cso[inTrain,]
test <- cso[-inTrain,]

# Evaluate RMSE
cso.RMSE.baseline <- round(sqrt(mean((cso.best.guess-test$PI)^2)),2)


# with R ------------------------------------------------------------------

csoR_mod <- readRDS("modelsFromCondor/CloverBeltMods/PI/csoPIR_stepBack.rds")

csoR_mod$anova

stepBackTidy_csoR <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + R_factor + Erosion:cover + Erosion:tillage + 
        Erosion:Contour + Erosion:initialP + Erosion:total_DM_lbs + 
        Erosion:totalP2O5_lbs + Erosion:slope + Erosion:slopelenusle.r + 
        Erosion:LSsurgo + Erosion:total.depth + Erosion:OM + Erosion:silt + 
        Erosion:R_factor + cover:tillage + cover:Contour + cover:initialP + 
        cover:total_DM_lbs + cover:slope + cover:LSsurgo + cover:OM + 
        cover:k + cover:R_factor + tillage:Contour + tillage:initialP + 
        tillage:total_DM_lbs + tillage:slope + tillage:slopelenusle.r + 
        tillage:LSsurgo + tillage:total.depth + tillage:OM + tillage:silt + 
        tillage:k + tillage:R_factor + Contour:initialP + Contour:total_DM_lbs + 
        Contour:slope + Contour:slopelenusle.r + Contour:LSsurgo + 
        Contour:total.depth + Contour:OM + Contour:k + Contour:R_factor + 
        initialP:total_DM_lbs + initialP:slope + initialP:slopelenusle.r + 
        initialP:LSsurgo + initialP:total.depth + initialP:OM + initialP:silt + 
        initialP:k + initialP:R_factor + total_DM_lbs:totalP2O5_lbs + 
        total_DM_lbs:slope + total_DM_lbs:slopelenusle.r + total_DM_lbs:OM + 
        total_DM_lbs:k + totalP2O5_lbs:slope + totalP2O5_lbs:LSsurgo + 
        totalP2O5_lbs:total.depth + totalP2O5_lbs:OM + totalP2O5_lbs:silt + 
        totalP2O5_lbs:k + totalP2O5_lbs:R_factor + slope:slopelenusle.r + 
        slope:LSsurgo + slope:total.depth + slope:OM + slope:silt + 
        slope:k + slope:R_factor + slopelenusle.r:LSsurgo + slopelenusle.r:total.depth + 
        slopelenusle.r:OM + slopelenusle.r:silt + slopelenusle.r:k + 
        slopelenusle.r:R_factor + LSsurgo:total.depth + LSsurgo:OM + 
        LSsurgo:silt + LSsurgo:k + LSsurgo:R_factor + total.depth:OM + 
        total.depth:silt + total.depth:k + total.depth:R_factor + 
        OM:silt + OM:k + OM:R_factor + silt:k + silt:R_factor + k:R_factor,
      data = train)

cso_predR <- stepBackTidy_csoR %>%
  predict(train) %>%
  bind_cols(train)

ggplot(cso_predR, aes(x = PI, y = .pred)) +
  geom_point()

csoR_rmse <- round(sqrt(mean((cso_predR$.pred - cso_predR$PI)^2)),3)


# without R ---------------------------------------------------------------

cso_mod <- readRDS("modelsFromCondor/CloverBeltMods/lowPI/csoPI_stepBack.rds")

cso_mod$anova

stepBackTidy_cso <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
        Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
        Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
        Erosion:total.depth + Erosion:OM + Erosion:silt + Erosion:k + 
        cover:tillage + cover:Contour + cover:initialP + cover:total_DM_lbs + 
        cover:totalP2O5_lbs + cover:slope + cover:LSsurgo + cover:OM + 
        cover:silt + cover:k + tillage:Contour + tillage:initialP + 
        tillage:total_DM_lbs + tillage:totalP2O5_lbs + tillage:slope + 
        tillage:slopelenusle.r + tillage:LSsurgo + tillage:total.depth + 
        tillage:OM + tillage:silt + tillage:k + Contour:initialP + 
        Contour:total_DM_lbs + Contour:totalP2O5_lbs + Contour:slope + 
        Contour:LSsurgo + Contour:silt + Contour:k + initialP:total_DM_lbs + 
        initialP:slope + initialP:slopelenusle.r + initialP:LSsurgo + 
        initialP:total.depth + initialP:OM + initialP:silt + initialP:k + 
        total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + total_DM_lbs:slopelenusle.r + 
        total_DM_lbs:LSsurgo + total_DM_lbs:total.depth + total_DM_lbs:OM + 
        total_DM_lbs:k + totalP2O5_lbs:slope + totalP2O5_lbs:slopelenusle.r + 
        totalP2O5_lbs:LSsurgo + totalP2O5_lbs:total.depth + totalP2O5_lbs:OM + 
        totalP2O5_lbs:silt + totalP2O5_lbs:k + slope:slopelenusle.r + 
        slope:LSsurgo + slope:total.depth + slope:OM + slope:silt + 
        slope:k + slopelenusle.r:LSsurgo + slopelenusle.r:total.depth + 
        slopelenusle.r:OM + slopelenusle.r:silt + slopelenusle.r:k + 
        LSsurgo:total.depth + LSsurgo:OM + LSsurgo:k + total.depth:OM + 
        total.depth:silt + total.depth:k + OM:silt + OM:k + silt:k,
      data = train)

cso_pred <- stepBackTidy_cso %>%
  predict(train) %>%
  bind_cols(train)

ggplot(cso_pred, aes(x = PI, y = .pred)) +
  geom_point()

cso_rmse <- round(sqrt(mean((cso_pred$.pred - cso_pred$PI)^2)),3)
#0.382
saveRDS(stepBackTidy_cso, "models/cloverBelt_cso_PI.RDS")


# dairy rotation ----------------------------------------------------------

##TODO still need to save this model
#clean data
dr <- full %>%
  filter(crop == "dr") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour),
         tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  filter(OM < 20) %>%
  distinct() %>%
  droplevels()

levels(dr$tillage)

# null hypothesis
dr.best.guess <- round(mean(dr$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = dr$PI, p = 0.7, list = FALSE)
train <- dr[inTrain,]
test <- dr[-inTrain,]

# Evaluate RMSE
dr.RMSE.baseline <- round(sqrt(mean((dr.best.guess-test$PI)^2)),2)


# with R ------------------------------------------------------------------

drR_mod <- readRDS("modelsFromCondor/CloverBeltMods/PI/drPIR_stepBack.rds")

drR_mod$anova

stepBackTidy_drR <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + R_factor + Erosion:cover + Erosion:tillage + 
        Erosion:Contour + Erosion:initialP + Erosion:total_DM_lbs + 
        Erosion:totalP2O5_lbs + Erosion:slope + Erosion:slopelenusle.r + 
        Erosion:LSsurgo + Erosion:total.depth + Erosion:OM + Erosion:silt + 
        Erosion:R_factor + cover:tillage + cover:Contour + cover:initialP + 
        cover:slope + cover:LSsurgo + cover:OM + cover:k + cover:R_factor + 
        tillage:Contour + tillage:initialP + tillage:total_DM_lbs + 
        tillage:totalP2O5_lbs + tillage:slope + tillage:slopelenusle.r + 
        tillage:LSsurgo + tillage:total.depth + tillage:OM + tillage:silt + 
        tillage:k + tillage:R_factor + Contour:initialP + Contour:total_DM_lbs + 
        Contour:slope + Contour:slopelenusle.r + Contour:LSsurgo + 
        Contour:total.depth + Contour:OM + Contour:k + Contour:R_factor + 
        initialP:total_DM_lbs + initialP:slope + initialP:slopelenusle.r + 
        initialP:LSsurgo + initialP:total.depth + initialP:OM + initialP:silt + 
        initialP:k + initialP:R_factor + total_DM_lbs:totalP2O5_lbs + 
        total_DM_lbs:slope + total_DM_lbs:LSsurgo + total_DM_lbs:k + 
        total_DM_lbs:R_factor + totalP2O5_lbs:slope + totalP2O5_lbs:slopelenusle.r + 
        totalP2O5_lbs:total.depth + totalP2O5_lbs:OM + totalP2O5_lbs:silt + 
        totalP2O5_lbs:R_factor + slope:slopelenusle.r + slope:LSsurgo + 
        slope:OM + slope:silt + slope:k + slope:R_factor + slopelenusle.r:LSsurgo + 
        slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
        slopelenusle.r:k + slopelenusle.r:R_factor + LSsurgo:total.depth + 
        LSsurgo:OM + LSsurgo:silt + LSsurgo:k + LSsurgo:R_factor + 
        total.depth:OM + total.depth:silt + total.depth:k + total.depth:R_factor + 
        OM:silt + OM:k + OM:R_factor + silt:k + silt:R_factor + k:R_factor,
      data = train)

dr_predR <- stepBackTidy_drR %>%
  predict(train) %>%
  bind_cols(train)

ggplot(dr_predR, aes(x = PI, y = .pred)) +
  geom_point()

drR_rmse <- round(sqrt(mean((dr_predR$.pred - dr_predR$PI)^2)),3)


# without R ---------------------------------------------------------------

dr_mod <- readRDS("modelsFromCondor/CloverBeltMods/PI/drPI_stepBack.rds")

dr_mod$anova

stepBackTidy_dr <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
        Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
        Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
        Erosion:total.depth + Erosion:OM + Erosion:silt + Erosion:k + 
        cover:tillage + cover:Contour + cover:initialP + cover:slope + 
        cover:slopelenusle.r + cover:LSsurgo + cover:total.depth + 
        cover:OM + cover:silt + tillage:Contour + tillage:initialP + 
        tillage:total_DM_lbs + tillage:totalP2O5_lbs + tillage:slope + 
        tillage:slopelenusle.r + tillage:LSsurgo + tillage:total.depth + 
        tillage:OM + tillage:silt + tillage:k + Contour:initialP + 
        Contour:total_DM_lbs + Contour:slope + Contour:slopelenusle.r + 
        Contour:LSsurgo + Contour:total.depth + Contour:OM + Contour:k + 
        initialP:total_DM_lbs + initialP:slope + initialP:slopelenusle.r + 
        initialP:LSsurgo + initialP:total.depth + initialP:OM + initialP:silt + 
        initialP:k + total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + 
        total_DM_lbs:LSsurgo + total_DM_lbs:k + totalP2O5_lbs:slope + 
        totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:LSsurgo + totalP2O5_lbs:total.depth + 
        totalP2O5_lbs:OM + totalP2O5_lbs:silt + slope:slopelenusle.r + 
        slope:LSsurgo + slope:total.depth + slope:OM + slope:silt + 
        slope:k + slopelenusle.r:LSsurgo + slopelenusle.r:total.depth + 
        slopelenusle.r:OM + slopelenusle.r:silt + slopelenusle.r:k + 
        LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + LSsurgo:k + 
        total.depth:OM + total.depth:silt + total.depth:k + OM:silt + 
        silt:k,
      data = train)

dr_pred <- stepBackTidy_dr %>%
  predict(train) %>%
  bind_cols(train)

ggplot(dr_pred, aes(x = PI, y = .pred)) +
  geom_point()

dr_rmse <- round(sqrt(mean((dr_pred$.pred - dr_pred$PI)^2)),3)

saveRDS(stepBackTidy_dr, "models/cloverBelt_dr_PI.RDS")



# pasture seeding ---------------------------------------------------------

#clean data
ps <- full %>%
  filter(crop == "ps") %>%
  mutate_if(is.character, as.factor)%>%
  dplyr::select(c(PI, Erosion, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  filter(OM < 20) %>%
  distinct() %>%
  droplevels()

# null hypothesis
ps.best.guess <- round(mean(ps$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = ps$PI, p = 0.7, list = FALSE)
train <- ps[inTrain,]
test <- ps[-inTrain,]

# Evaluate RMSE
ps.RMSE.baseline <- round(sqrt(mean((ps.best.guess-test$PI)^2)),2)


# with R ------------------------------------------------------------------

psR_mod <- readRDS("modelsFromCondor/CloverBeltMods/PI/psPIR_stepBack.rds")

psR_mod$anova

stepBackTidy_psR <- 
  lm_mod %>%
  fit(PI ~ Erosion + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + R_factor + Erosion:tillage + Erosion:Contour + 
        Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
        Erosion:slope + Erosion:LSsurgo + Erosion:total.depth + Erosion:OM + 
        Erosion:silt + Erosion:k + Erosion:R_factor + tillage:Contour + 
        tillage:initialP + tillage:slope + tillage:slopelenusle.r + 
        tillage:LSsurgo + tillage:total.depth + tillage:OM + tillage:silt + 
        tillage:k + tillage:R_factor + Contour:initialP + Contour:totalP2O5_lbs + 
        Contour:slope + Contour:slopelenusle.r + Contour:LSsurgo + 
        Contour:total.depth + Contour:OM + Contour:silt + Contour:k + 
        Contour:R_factor + initialP:total_DM_lbs + initialP:slope + 
        initialP:slopelenusle.r + initialP:LSsurgo + initialP:total.depth + 
        initialP:OM + initialP:silt + initialP:k + initialP:R_factor + 
        total_DM_lbs:totalP2O5_lbs + total_DM_lbs:silt + totalP2O5_lbs:slope + 
        totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:k + totalP2O5_lbs:R_factor + 
        slope:slopelenusle.r + slope:LSsurgo + slope:silt + slope:k + 
        slope:R_factor + slopelenusle.r:LSsurgo + slopelenusle.r:total.depth + 
        slopelenusle.r:OM + slopelenusle.r:silt + slopelenusle.r:k + 
        slopelenusle.r:R_factor + LSsurgo:total.depth + LSsurgo:OM + 
        LSsurgo:silt + LSsurgo:k + LSsurgo:R_factor + total.depth:OM + 
        total.depth:silt + total.depth:k + total.depth:R_factor + 
        OM:silt + OM:k + OM:R_factor + silt:k + k:R_factor,
      data = train)

ps_predR <- stepBackTidy_psR %>%
  predict(train) %>%
  bind_cols(train)

ggplot(ps_predR, aes(x = PI, y = .pred)) +
  geom_point()

psR_rmse <- round(sqrt(mean((ps_predR$.pred - ps_predR$PI)^2)),3)


# without R ---------------------------------------------------------------

ps_mod <- readRDS("modelsFromCondor/CloverBeltMods/lowPI/psPI_stepBack.rds")

ps_mod$anova

stepBackTidy_ps <- 
  lm_mod %>%
  fit(PI ~ Erosion + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + Erosion:tillage + Erosion:Contour + Erosion:initialP + 
        Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + Erosion:slope + 
        Erosion:slopelenusle.r + Erosion:LSsurgo + Erosion:total.depth + 
        Erosion:OM + Erosion:silt + Erosion:k + tillage:Contour + 
        tillage:initialP + tillage:total_DM_lbs + tillage:totalP2O5_lbs + 
        tillage:slope + tillage:slopelenusle.r + tillage:LSsurgo + 
        tillage:total.depth + tillage:OM + tillage:silt + tillage:k + 
        Contour:initialP + Contour:totalP2O5_lbs + Contour:slope + 
        Contour:LSsurgo + Contour:total.depth + Contour:OM + Contour:silt + 
        Contour:k + initialP:total_DM_lbs + initialP:slope + initialP:slopelenusle.r + 
        initialP:LSsurgo + initialP:total.depth + initialP:OM + initialP:silt + 
        initialP:k + total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + 
        total_DM_lbs:slopelenusle.r + total_DM_lbs:LSsurgo + total_DM_lbs:OM + 
        total_DM_lbs:silt + total_DM_lbs:k + totalP2O5_lbs:slopelenusle.r + 
        totalP2O5_lbs:LSsurgo + totalP2O5_lbs:total.depth + totalP2O5_lbs:k + 
        slope:slopelenusle.r + slope:LSsurgo + slope:total.depth + 
        slope:OM + slope:silt + slope:k + slopelenusle.r:LSsurgo + 
        slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
        slopelenusle.r:k + LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + 
        LSsurgo:k + total.depth:OM + total.depth:silt + total.depth:k + 
        OM:silt + OM:k + silt:k,
      data = train)

ps_pred <- stepBackTidy_ps %>%
  predict(train) %>%
  bind_cols(train)

ggplot(ps_pred, aes(x = PI, y = .pred)) +
  geom_point()

ps_rmse <- round(sqrt(mean((ps_pred$.pred - ps_pred$PI)^2)),3)
#0.389
saveRDS(stepBackTidy_ps, "models/cloverBelt_ps_PI.RDS")

# pasture ---------------------------------------------------------
  
  #clean data
  pt <- full %>%
  filter(crop == "pt", 
         Contour == "0") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, density, rotational, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  filter(OM < 20) %>%
  distinct() %>%
  droplevels()

# null hypothesis
pt.best.guess <- round(mean(pt$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = pt$PI, p = 0.7, list = FALSE)
train <- pt[inTrain,]
test <- pt[-inTrain,]

# Evaluate RMSE
pt.RMSE.baseline <- round(sqrt(mean((pt.best.guess-test$PI)^2)),2)


# with R ------------------------------------------------------------------

ptR_mod <- readRDS("modelsFromCondor/CloverBeltMods/PI/ptPIR_stepBack.rds")

ptR_mod$anova

stepBackTidy_ptR <- 
  lm_mod %>%
  fit(PI ~ Erosion + density + initialP + total_DM_lbs + totalP2O5_lbs + 
        slope + slopelenusle.r + LSsurgo + total.depth + OM + silt + 
        k + R_factor + Erosion:density + Erosion:initialP + Erosion:total_DM_lbs + 
        Erosion:totalP2O5_lbs + Erosion:slope + Erosion:slopelenusle.r + 
        Erosion:LSsurgo + Erosion:total.depth + Erosion:OM + Erosion:silt + 
        Erosion:k + Erosion:R_factor + density:initialP + density:total_DM_lbs + 
        density:slope + density:slopelenusle.r + density:LSsurgo + 
        density:total.depth + density:OM + density:k + density:R_factor + 
        initialP:slope + initialP:slopelenusle.r + initialP:LSsurgo + 
        initialP:total.depth + initialP:OM + initialP:silt + initialP:k + 
        initialP:R_factor + total_DM_lbs:totalP2O5_lbs + total_DM_lbs:total.depth + 
        total_DM_lbs:OM + total_DM_lbs:k + totalP2O5_lbs:slope + 
        totalP2O5_lbs:LSsurgo + totalP2O5_lbs:k + totalP2O5_lbs:R_factor + 
        slope:slopelenusle.r + slope:LSsurgo + slope:OM + slope:silt + 
        slope:k + slope:R_factor + slopelenusle.r:LSsurgo + slopelenusle.r:total.depth + 
        slopelenusle.r:OM + slopelenusle.r:k + slopelenusle.r:R_factor + 
        LSsurgo:silt + LSsurgo:k + LSsurgo:R_factor + total.depth:OM + 
        total.depth:silt + total.depth:k + total.depth:R_factor + 
        OM:silt + OM:k + OM:R_factor + silt:k + silt:R_factor,
      data = train)

pt_predR <- stepBackTidy_ptR %>%
  predict(train) %>%
  bind_cols(train)

ggplot(pt_predR, aes(x = PI, y = .pred)) +
  geom_point()

ptR_rmse <- round(sqrt(mean((pt_predR$.pred - pt_predR$PI)^2)),3)


# without R ---------------------------------------------------------------

pt_mod <- readRDS("modelsFromCondor/CloverBeltMods/lowPI/ptPI_stepBack.rds")

pt_mod$anova

stepBackTidy_pt <- 
  lm_mod %>%
  fit(PI ~ Erosion + density + initialP + total_DM_lbs + totalP2O5_lbs + 
        slope + slopelenusle.r + LSsurgo + total.depth + OM + silt + 
        k + Erosion:density + Erosion:initialP + Erosion:total_DM_lbs + 
        Erosion:totalP2O5_lbs + Erosion:slope + Erosion:slopelenusle.r + 
        Erosion:LSsurgo + Erosion:total.depth + Erosion:OM + Erosion:silt + 
        Erosion:k + density:initialP + density:total_DM_lbs + density:totalP2O5_lbs + 
        density:slope + density:slopelenusle.r + density:LSsurgo + 
        density:total.depth + density:OM + density:silt + density:k + 
        initialP:slope + initialP:slopelenusle.r + initialP:LSsurgo + 
        initialP:total.depth + initialP:OM + initialP:silt + initialP:k + 
        total_DM_lbs:totalP2O5_lbs + total_DM_lbs:k + totalP2O5_lbs:slope + 
        totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:LSsurgo + totalP2O5_lbs:total.depth + 
        totalP2O5_lbs:OM + totalP2O5_lbs:k + slope:LSsurgo + slope:total.depth + 
        slope:OM + slope:k + slopelenusle.r:LSsurgo + slopelenusle.r:total.depth + 
        slopelenusle.r:OM + slopelenusle.r:silt + slopelenusle.r:k + 
        LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + LSsurgo:k + 
        total.depth:OM + total.depth:silt + total.depth:k + OM:silt + 
        OM:k + silt:k,
      data = train)

pt_pred <- stepBackTidy_pt %>%
  predict(train) %>%
  bind_cols(train)

ggplot(pt_pred, aes(x = PI, y = .pred)) +
  geom_point()

pt_rmse <- round(sqrt(mean((pt_pred$.pred - pt_pred$PI)^2)),3)
#0.252
saveRDS(stepBackTidy_pt, "models/cloverBelt_pt_PI.RDS")

# dry lot ---------------------------------------------------------

#clean data
dl <- full %>%
  filter(crop == "dl") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, density, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  filter(OM < 20) %>%
  distinct() %>%
  droplevels()

# null hypothesis
dl.best.guess <- round(mean(dl$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = dl$PI, p = 0.7, list = FALSE)
train <- dl[inTrain,]
test <- dl[-inTrain,]

# Evaluate RMSE
dl.RMSE.baseline <- round(sqrt(mean((dl.best.guess-test$PI)^2)),2)


# with R ------------------------------------------------------------------

dlR_mod <- readRDS("modelsFromCondor/CloverBeltMods/PI/dlPIR_stepBack.rds")

dlR_mod$anova

stepBackTidy_dlR <- 
  lm_mod %>%
  fit(PI ~ Erosion + density + initialP + slope + slopelenusle.r + 
        LSsurgo + total.depth + OM + silt + k + R_factor + Erosion:density + 
        Erosion:initialP + Erosion:slope + Erosion:LSsurgo + Erosion:total.depth + 
        Erosion:OM + Erosion:silt + Erosion:R_factor + density:initialP + 
        density:slope + density:LSsurgo + density:total.depth + density:silt + 
        initialP:slope + initialP:slopelenusle.r + initialP:total.depth + 
        initialP:OM + initialP:silt + initialP:k + slope:slopelenusle.r + 
        slope:LSsurgo + slope:total.depth + slope:OM + slope:k + 
        slope:R_factor + slopelenusle.r:LSsurgo + slopelenusle.r:k + 
        slopelenusle.r:R_factor + LSsurgo:total.depth + LSsurgo:OM + 
        LSsurgo:silt + LSsurgo:R_factor + total.depth:OM + total.depth:silt + 
        total.depth:k + total.depth:R_factor + OM:silt + OM:k + OM:R_factor + 
        silt:k,
      data = train)

dl_predR <- stepBackTidy_dlR %>%
  predict(train) %>%
  bind_cols(train)

ggplot(dl_predR, aes(x = PI, y = .pred)) +
  geom_point()

dlR_rmse <- round(sqrt(mean((dl_predR$.pred - dl_predR$PI)^2)),3)


# without R ---------------------------------------------------------------

dl_mod <- readRDS("modelsFromCondor/CloverBeltMods/lowPI/dlPI_stepBack.rds")

dl_mod$anova

stepBackTidy_dl <- 
  lm_mod %>%
  fit(PI ~ Erosion + density + initialP + slope + slopelenusle.r + 
        LSsurgo + total.depth + OM + silt + k + Erosion:density + 
        Erosion:initialP + Erosion:slope + Erosion:slopelenusle.r + 
        Erosion:LSsurgo + Erosion:OM + Erosion:silt + Erosion:k + 
        density:initialP + density:slope + density:OM + density:silt + 
        density:k + initialP:slope + initialP:OM + initialP:silt + 
        initialP:k + slope:slopelenusle.r + slope:LSsurgo + slope:total.depth + 
        slope:OM + slope:silt + slope:k + slopelenusle.r:LSsurgo + 
        slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:k + 
        LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + LSsurgo:k + 
        total.depth:OM + total.depth:silt + total.depth:k + OM:silt + 
        OM:k + silt:k,
      data = train)

dl_pred <- stepBackTidy_dl %>%
  predict(train) %>%
  bind_cols(train)

ggplot(dl_pred, aes(x = PI, y = .pred)) +
  geom_point()

dl_rmse <- round(sqrt(mean((dl_pred$.pred - dl_pred$PI)^2)),3)
#1.6
saveRDS(stepBackTidy_dl, "models/cloverBelt_dl_PI.RDS")


# diagnostics -------------------------------------------------------------

names(full)

cc <- full %>%
  filter(crop == "cc") %>%
  mutate_if(is.character, as.factor) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

summary(cc)

# load model 
cc_mod <- readRDS("models/cloverBelt_cc_PI.RDS")

cc_pred <- cc_mod %>%
  predict(cc) %>%
  bind_cols(cc)

ggplot(cc_pred, aes(x = .pred, y = PI)) +
  geom_point()

cc_pred <- cc_pred %>%
  mutate(check = PI/.pred)

hist(cc_pred$check)
summary(cc_pred$check)

ggplot(cc_pred, aes(x = .pred, y = PI, color = County)) +
  geom_point() 

cc_pred %>%
  filter(County == "marathon") %>%
  ggplot(aes(x = .pred, y = PI)) +
  geom_point()

cc_pred %>%
  filter(County != "marathon") %>%
  ggplot(aes(x = .pred, y = PI, color = cover)) +
  geom_point()

cc_pred %>%
  filter(County != "marathon") %>%
  ggplot(aes(x = .pred, y = PI, color = tillage)) +
  geom_point()

cc_pred %>%
  filter(County != "marathon",
         .pred < 55) %>%
  ggplot(aes(x = .pred, y = PI, color = tillage, shape = cover)) +
  geom_point()

cc_pred_sub <- cc_pred %>%
  filter(County != "marathon",
         between(.pred, 10, 55),
         between(PI, 15, 80)) %>%
  droplevels()

summary(cc_pred_sub)

ggplot(cc_pred_sub, aes(x = OM, y = check)) +
  geom_point()

summary(cc$OM)


# < 10 OM test -------------------------------------------------------------

# cont corn ---------------------------------------------------------------

#clean data
cc10 <- full %>%
  filter(crop == "cc") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  filter(OM < 10) %>%
  distinct() %>%
  droplevels()

summary(cc)


# load models -------------------------------------------------------------

# *without R --------------------------------------------------------------

cc10_back <- readRDS("modelsFromCondor/CloverBeltMods/PI/ccPI_stepBack_OM10.rds")

cc10_back$anova

# null hypothesis
cc10.best.guess <- round(mean(cc10$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cc10$PI, p = 0.7, list = FALSE)
train <- cc10[inTrain,]
test <- cc10[-inTrain,]

# Evaluate RMSE
cc10.RMSE.baseline <- round(sqrt(mean((cc10.best.guess-test$PI)^2)),2)

#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

stepBackTidy_om10 <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
        Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
        Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
        Erosion:total.depth + Erosion:OM + Erosion:silt + Erosion:k + 
        cover:tillage + cover:Contour + cover:initialP + cover:total_DM_lbs + 
        cover:totalP2O5_lbs + cover:slope + cover:slopelenusle.r + 
        cover:LSsurgo + cover:OM + cover:silt + cover:k + tillage:Contour + 
        tillage:initialP + tillage:total_DM_lbs + tillage:totalP2O5_lbs + 
        tillage:slope + tillage:slopelenusle.r + tillage:LSsurgo + 
        tillage:total.depth + tillage:OM + tillage:silt + tillage:k + 
        Contour:initialP + Contour:total_DM_lbs + Contour:totalP2O5_lbs + 
        Contour:slope + Contour:slopelenusle.r + Contour:LSsurgo + 
        Contour:total.depth + Contour:OM + Contour:silt + Contour:k + 
        initialP:total_DM_lbs + initialP:slope + initialP:slopelenusle.r + 
        initialP:LSsurgo + initialP:total.depth + initialP:OM + initialP:silt + 
        initialP:k + total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + 
        total_DM_lbs:slopelenusle.r + total_DM_lbs:total.depth + 
        total_DM_lbs:OM + total_DM_lbs:k + totalP2O5_lbs:slope + 
        totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:LSsurgo + totalP2O5_lbs:total.depth + 
        totalP2O5_lbs:OM + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
        slope:slopelenusle.r + slope:LSsurgo + slope:total.depth + 
        slope:OM + slope:silt + slope:k + slopelenusle.r:LSsurgo + 
        slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
        slopelenusle.r:k + LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + 
        LSsurgo:k + total.depth:OM + total.depth:silt + total.depth:k + 
        OM:silt + OM:k + silt:k,
      data = train)

cc_pred_om10 <- stepBackTidy_om10 %>%
  predict(train) %>%
  bind_cols(train)

ggplot(cc_pred_om10, aes(x = PI, y = .pred)) +
  geom_point()

cc_rmse_om10 <- round(sqrt(mean((cc_pred_om10$.pred - cc_pred_om10$PI)^2)),3)

#saveRDS(stepBackTidy_lowOM, "models/cloverBelt_cc_PI_lowOM.RDS")

# < 15 OM test -------------------------------------------------------------

# cont corn ---------------------------------------------------------------

#clean data
cc15 <- full %>%
  filter(crop == "cc") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  filter(OM < 15) %>%
  distinct() %>%
  droplevels()


# load models -------------------------------------------------------------

# *without R --------------------------------------------------------------

cc15_back <- readRDS("modelsFromCondor/CloverBeltMods/PI/ccPI_stepBack_15OM.rds")

cc15_back$anova

# null hypothesis
cc15.best.guess <- round(mean(cc15$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cc15$PI, p = 0.7, list = FALSE)
train <- cc15[inTrain,]
test <- cc15[-inTrain,]

# Evaluate RMSE
cc15.RMSE.baseline <- round(sqrt(mean((cc15.best.guess-test$PI)^2)),2)

#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

stepBackTidy_om15 <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
        Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
        Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
        Erosion:total.depth + Erosion:OM + Erosion:silt + Erosion:k + 
        cover:tillage + cover:Contour + cover:initialP + cover:total_DM_lbs + 
        cover:totalP2O5_lbs + cover:slope + cover:slopelenusle.r + 
        cover:LSsurgo + cover:OM + cover:silt + cover:k + tillage:Contour + 
        tillage:initialP + tillage:total_DM_lbs + tillage:totalP2O5_lbs + 
        tillage:slope + tillage:slopelenusle.r + tillage:LSsurgo + 
        tillage:total.depth + tillage:OM + tillage:silt + tillage:k + 
        Contour:initialP + Contour:total_DM_lbs + Contour:totalP2O5_lbs + 
        Contour:slope + Contour:slopelenusle.r + Contour:LSsurgo + 
        Contour:total.depth + Contour:OM + Contour:silt + Contour:k + 
        initialP:total_DM_lbs + initialP:slope + initialP:slopelenusle.r + 
        initialP:LSsurgo + initialP:total.depth + initialP:OM + initialP:silt + 
        initialP:k + total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + 
        total_DM_lbs:slopelenusle.r + total_DM_lbs:LSsurgo + total_DM_lbs:total.depth + 
        total_DM_lbs:OM + total_DM_lbs:k + totalP2O5_lbs:slope + 
        totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:LSsurgo + totalP2O5_lbs:total.depth + 
        totalP2O5_lbs:OM + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
        slope:slopelenusle.r + slope:LSsurgo + slope:total.depth + 
        slope:OM + slope:silt + slope:k + slopelenusle.r:LSsurgo + 
        slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
        slopelenusle.r:k + LSsurgo:total.depth + LSsurgo:OM + LSsurgo:silt + 
        LSsurgo:k + total.depth:OM + total.depth:silt + total.depth:k + 
        OM:silt + OM:k + silt:k,
      data = train)

cc_pred_om15 <- stepBackTidy_om15 %>%
  predict(train) %>%
  bind_cols(train)

ggplot(cc_pred_om15, aes(x = PI, y = .pred)) +
  geom_point()

cc_rmse_om15 <- round(sqrt(mean((cc_pred_om15$.pred - cc_pred_om15$PI)^2)),3)

#saveRDS(stepBackTidy_lowOM, "models/cloverBelt_cc_PI_lowOM.RDS")

# < 20 OM test -------------------------------------------------------------

# cont corn ---------------------------------------------------------------

#clean data
cc20 <- full %>%
  filter(crop == "cc") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  filter(OM < 20) %>%
  distinct() %>%
  droplevels()

# load models -------------------------------------------------------------

# *without R --------------------------------------------------------------

cc20_back <- readRDS("modelsFromCondor/CloverBeltMods/PI/ccPI_stepBack_20OM.rds")

cc20_back$anova

# null hypothesis
cc20.best.guess <- round(mean(cc20$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cc20$PI, p = 0.7, list = FALSE)
train <- cc20[inTrain,]
test <- cc20[-inTrain,]

# Evaluate RMSE
cc20.RMSE.baseline <- round(sqrt(mean((cc20.best.guess-test$PI)^2)),2)

#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

stepBackTidy_om20 <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        OM + silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
        Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
        Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
        Erosion:total.depth + Erosion:OM + Erosion:silt + Erosion:k + 
        cover:tillage + cover:Contour + cover:initialP + cover:total_DM_lbs + 
        cover:totalP2O5_lbs + cover:slope + cover:slopelenusle.r + 
        cover:LSsurgo + cover:OM + cover:silt + cover:k + tillage:Contour + 
        tillage:initialP + tillage:total_DM_lbs + tillage:totalP2O5_lbs + 
        tillage:slope + tillage:slopelenusle.r + tillage:LSsurgo + 
        tillage:total.depth + tillage:OM + tillage:silt + tillage:k + 
        Contour:initialP + Contour:total_DM_lbs + Contour:totalP2O5_lbs + 
        Contour:slope + Contour:LSsurgo + Contour:silt + Contour:k + 
        initialP:total_DM_lbs + initialP:slope + initialP:slopelenusle.r + 
        initialP:LSsurgo + initialP:total.depth + initialP:OM + initialP:silt + 
        initialP:k + total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + 
        total_DM_lbs:slopelenusle.r + total_DM_lbs:LSsurgo + total_DM_lbs:total.depth + 
        total_DM_lbs:OM + total_DM_lbs:silt + total_DM_lbs:k + totalP2O5_lbs:slope + 
        totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:LSsurgo + totalP2O5_lbs:total.depth + 
        totalP2O5_lbs:OM + totalP2O5_lbs:silt + totalP2O5_lbs:k + 
        slope:slopelenusle.r + slope:LSsurgo + slope:total.depth + 
        slope:OM + slope:silt + slope:k + slopelenusle.r:LSsurgo + 
        slopelenusle.r:total.depth + slopelenusle.r:OM + slopelenusle.r:silt + 
        slopelenusle.r:k + LSsurgo:total.depth + LSsurgo:OM + LSsurgo:k + 
        total.depth:OM + total.depth:silt + total.depth:k + OM:silt + 
        OM:k + silt:k,
      data = train)

cc_pred_om20 <- stepBackTidy_om20 %>%
  predict(train) %>%
  bind_cols(train)

ggplot(cc_pred_om20, aes(x = PI, y = .pred)) +
  geom_point()

cc_rmse_om20 <- round(sqrt(mean((cc_pred_om20$.pred - cc_pred_om20$PI)^2)),3)

#saveRDS(stepBackTidy_lowOM, "models/cloverBelt_cc_PI_lowOM.RDS")


# no OM test --------------------------------------------------------------

# *without R --------------------------------------------------------------

ccBack_noOM <- readRDS("modelsFromCondor/CloverBeltMods/PI/ccPInoOM_stepBack.rds")

ccBack_noOM$anova

# null hypothesis
cc.best.guess <- round(mean(cc$PI),2)

#partition data
set.seed(123)
inTrain <- createDataPartition(y = cc$PI, p = 0.7, list = FALSE)
train <- cc[inTrain,]
test <- cc[-inTrain,]

# Evaluate RMSE
cc.RMSE.baseline <- round(sqrt(mean((cc.best.guess-test$PI)^2)),2)

#define the model
lm_mod <- 
  linear_reg() %>% 
  set_engine("lm")

stepBackTidy_noOM <- 
  lm_mod %>%
  fit(PI ~ Erosion + cover + tillage + Contour + initialP + total_DM_lbs + 
        totalP2O5_lbs + slope + slopelenusle.r + LSsurgo + total.depth + 
        silt + k + Erosion:cover + Erosion:tillage + Erosion:Contour + 
        Erosion:initialP + Erosion:total_DM_lbs + Erosion:totalP2O5_lbs + 
        Erosion:slope + Erosion:slopelenusle.r + Erosion:LSsurgo + 
        Erosion:total.depth + Erosion:silt + Erosion:k + cover:tillage + 
        cover:Contour + cover:initialP + cover:total_DM_lbs + cover:slope + 
        cover:slopelenusle.r + cover:LSsurgo + cover:total.depth + 
        cover:silt + cover:k + tillage:Contour + tillage:initialP + 
        tillage:total_DM_lbs + tillage:slope + tillage:slopelenusle.r + 
        tillage:LSsurgo + tillage:total.depth + tillage:silt + tillage:k + 
        Contour:initialP + Contour:total_DM_lbs + Contour:slope + 
        Contour:slopelenusle.r + Contour:LSsurgo + Contour:total.depth + 
        Contour:silt + Contour:k + initialP:total_DM_lbs + initialP:slopelenusle.r + 
        initialP:LSsurgo + initialP:total.depth + initialP:silt + 
        initialP:k + total_DM_lbs:totalP2O5_lbs + total_DM_lbs:slope + 
        total_DM_lbs:LSsurgo + total_DM_lbs:total.depth + total_DM_lbs:k + 
        totalP2O5_lbs:slope + totalP2O5_lbs:slopelenusle.r + totalP2O5_lbs:silt + 
        slope:slopelenusle.r + slope:LSsurgo + slope:total.depth + 
        slope:silt + slope:k + slopelenusle.r:LSsurgo + slopelenusle.r:total.depth + 
        slopelenusle.r:silt + slopelenusle.r:k + LSsurgo:total.depth + 
        LSsurgo:silt + LSsurgo:k + total.depth:silt + total.depth:k + 
        silt:k,
      data = train)

cc_pred_noOM <- stepBackTidy_noOM %>%
  predict(train) %>%
  bind_cols(train)

ggplot(cc_pred_noOM, aes(x = PI, y = .pred)) +
  geom_point()

cc_rmse_noOM <- round(sqrt(mean((cc_pred_noOM$.pred - cc_pred_noOM$PI)^2)),3)




