# this script examines the metrics of the curve number models from tainter creek

library(tidyverse)
library(tidymodels)
library(randomForest)


# cont corn ---------------------------------------------------------------

cc <- read_csv("forCondor/curveData/ccCurve.csv")
ccMod <- readRDS("modelsFromCondor/ContCornFFCN.rds")

ccMod
cc <- ccMod %>%
  predict(cc) %>%
  bind_cols(cc)

cc %>% 
  ggplot(aes(x = ffCN, y = .pred)) +
  geom_point()

mean(ccMod$fit$mse) # 0.05


# corn grain --------------------------------------------------------------

cg <- read_csv("forCondor/curveData/cgCurve.csv")
cgMod <- readRDS("modelsFromCondor/CornGrainFFCN.rds")

cgMod %>%
  predict(cg) %>%
  bind_cols(cg) %>%
  ggplot(aes(x = ffCN, y = .pred)) +
  geom_point()

mean(cgMod$fit$mse) # 0.06


# corn soy oat ------------------------------------------------------------

cso <- read_csv("forCondor/curveData/csoCurve.csv")
csoMod <- readRDS("modelsFromCondor/CornSoyOatFFCN.rds")

csoMod %>%
  predict(cso) %>%
  bind_cols(cso) %>%
  ggplot(aes(x = ffCN, y = .pred)) +
  geom_point()

mean(csoMod$fit$mse) # 0.06


# dairy rotation ----------------------------------------------------------

dr <- read_csv("forCondor/curveData/drCurve.csv")
drMod <- readRDS("modelsFromCondor/drFFCN.rds")

drMod %>%
  predict(dr) %>%
  bind_cols(dr) %>%
  ggplot(aes(x = ffCN, y = .pred)) +
  geom_point()

mean(drMod$fit$mse) # 0.04


# pasture seeding ---------------------------------------------------------

ps <- read_csv("forCondor/curveData/psCurve.csv")
nrow(distinct(ps)) #this might explain the better fit??
psMod <- readRDS("modelsFromCondor/psFFCN.rds")

psMod %>%
  predict(ps) %>%
  bind_cols(ps) %>%
  ggplot(aes(x = ffCN, y = .pred)) +
  geom_point()

mean(psMod$fit$mse) # 0.01


# pasture -----------------------------------------------------------------

pt <- read_csv("forCondor/curveData/ptCurve.csv")
nrow(distinct(pt)) #this might explain the better fit??
ptMod <- readRDS("modelsFromCondor/ptFFCN.rds")

ptMod %>%
  predict(pt) %>%
  bind_cols(pt) %>%
  ggplot(aes(x = ffCN, y = .pred)) +
  geom_point()

mean(ptMod$fit$mse) # 0.004


# dry lot -----------------------------------------------------------------

dl <- read_csv("forCondor/curveData/dlCurve.csv")

dlMod <- readRDS("modelsFromCondor/dlFFCN.rds")

dlMod %>%
  predict(dl) %>%
  bind_cols(dl) %>%
  ggplot(aes(x = ffCN, y = .pred)) +
  geom_point()

mean(dlMod$fit$mse) # 0.006
