# this script tests to see how well the models created from tainter creek data work with the marathon and kewaunee data sets

library(tidyverse)
library(tidymodels)
library(randomForest)

#load marathon and kewaunee data
maraSnap <- read.csv("data/maraJoinSnapSurgo.csv")
kewaSnap <- read.csv("data/kewaJoinSnapSurgo.csv")
tainterSnap <- read.csv("data/TainterSnapSurgo.csv")

snap <- rbind(maraSnap, kewaSnap) %>%
  mutate(Contour = as.factor(Contour))

tainterSnap <- tainterSnap %>% 
  mutate(Contour = as.factor(Contour))

#load erosion models
cc_erosion <- readRDS("modelsFromCondor/tainterCreekMods/erosion/ContCornErosion.rds")
cg_erosion <- readRDS("modelsFromCondor/tainterCreekMods/erosion/cornGrainErosion.rds")
cso_erosion <- readRDS("modelsFromCondor/tainterCreekMods/erosion/cornSoyOatErosion.rds")
dr_erosion <- readRDS("modelsFromCondor/tainterCreekMods/erosion/dairyRotationErosion.rds")
dl_erosion <- readRDS("modelsFromCondor/tainterCreekMods/erosion/dryLotErosionErosion.rds")
ps_erosion <- readRDS("modelsFromCondor/tainterCreekMods/erosion/pastureSeedingErosionErosion.rds")
pt_erosion <- readRDS("modelsFromCondor/tainterCreekMods/erosion/pastureErosion.rds")

#load PI models 
cc_PI <- readRDS("modelsFromCondor/tainterCreekMods/PI/ContCornTidyPI.rds")
cg_PI <- readRDS("modelsFromCondor/tainterCreekMods/PI/CornGrain_tidyPI.rds") ## not permitted
cso_PI <- readRDS("modelsFromCondor/tainterCreekMods/PI/CSO_tidyPI.rds")
dr_PI <- readRDS("modelsFromCondor/tainterCreekMods/PI/dairyRot_tidyPI.rds")
dl_PI <- readRDS("modelsFromCondor/tainterCreekMods/PI/DryLot_tidyPI.rds")
ps_PI <- readRDS("modelsFromCondor/tainterCreekMods/PI/pastureSeedingTidyPI.rds")
pt_PI <- readRDS("modelsFromCondor/tainterCreekMods/PI/tidyLM_PI_pasture.rds")

#subset snap data
#cont corn
ccNew <- snap %>% filter(crop == "cc")
ccTainter <- tainterSnap %>% filter(crop == "cc")

ccWithPred <- cc_erosion %>%
  predict(ccNew) %>%
  bind_cols(ccNew) %>%
  distinct()

ccTainterPred <- cc_erosion %>%
  predict(ccTainter) %>%
  bind_cols(ccTainter) %>%
  distinct()

ccWithPred_PI <- cc_PI %>%
  predict(ccNew) %>%
  bind_cols(ccNew) %>%
  distinct()

ccTainterPred_PI <- cc_PI %>%
  predict(ccTainter) %>%
  bind_cols(ccTainter) %>%
  distinct()

ggplot(ccWithPred, aes(x = Erosion, y = .pred)) +
  geom_point()

ggplot(ccTainterPred, aes(x = Erosion, y = .pred)) +
  geom_point()

ggplot(ccWithPred_PI, aes(x = PI, y = .pred)) +
  geom_point()

ggplot(ccTainterPred_PI, aes(x = PI, y = .pred)) +
  geom_point()

#corn grain
cgNew <- snap %>% filter(crop == "cg")
cgTainter <- tainterSnap %>% filter(crop == "cg")

cgWithPred <- cg_erosion %>%
  predict(cgNew) %>%
  bind_cols(cgNew) %>%
  distinct()

cgTainterPred <- cg_erosion %>%
  predict(cgTainter) %>%
  bind_cols(cgTainter) %>%
  distinct()

ggplot(cgWithPred, aes(x = Erosion, y = .pred)) +
  geom_point()

ggplot(cgTainterPred, aes(x = Erosion, y = .pred)) +
  geom_point()


# pasture
psNew <- snap %>% filter(crop == "ps")
psTainter <- tainterSnap %>% filter(crop == "ps")

psWithPred <- ps_erosion %>%
  predict(psNew) %>%
  bind_cols(psNew) %>%
  distinct()

psTainterPred <- ps_erosion %>%
  predict(psTainter) %>%
  bind_cols(psTainter) %>%
  distinct()

ggplot(psWithPred, aes(x = Erosion, y = .pred)) +
  geom_point()

ggplot(psTainterPred, aes(x = Erosion, y = .pred)) +
  geom_point()

# Ploss
ccWithPI <- cc_PI %>%
  predict(ccNew) %>%
  bind_cols(ccNew) %>%
  distinct()

ccTainterPI<- cc_PI %>%
  predict(ccTainter) %>%
  bind_cols(ccTainter) %>%
  distinct()

ggplot(ccWithPI, aes(x = PI, y = .pred)) +
  geom_point()

ggplot(ccTainterPI, aes(x = PI, y = .pred)) +
  geom_point()

psWithPI <- ps_PI %>%
  predict(psNew) %>%
  bind_cols(psNew) %>%
  distinct()

psTainterPI <- ps_PI %>%
  predict(psTainter) %>%
  bind_cols(psTainter) %>%
  distinct()

ggplot(psWithPI, aes(x = PI, y = .pred)) +
  geom_point()

ggplot(psTainterPI, aes(x = PI, y = .pred)) +
  geom_point()

