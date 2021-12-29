# this script compares erosion models from clover belt snap plus runs

#load libraries
library(tidyverse)
library(randomForest)
library(tidymodels)


#load data
clark <- read_csv("data/clarkJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
mara <- read_csv("data/maraJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
taylor <- read_csv("data/taylorJoinSnapSurgo.csv", col_types = list( "SoilSymbol" = col_character()))
full <- bind_rows(clark, mara, taylor)
rm(list = c("clark", "mara", "taylor"))


# cont corn ---------------------------------------------------------------

# * without R ---------------------------------------------------------------

# clean data
cc <- full %>%
  filter(crop == "cc") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

#load model
cc_mod <- readRDS("modelsFromCondor/CloverBeltMods/erosion/ContCornErosionCloverBelt.rds")

cc_mod$fit$importance

# predictions
pred_cc <- cc_mod %>%
  predict(cc) %>%
  bind_cols(cc)

cc_rmse <- round(sqrt(mean((pred_cc$.pred - pred_cc$Erosion)^2)),3)
# plot data
ggplot(pred_cc, aes(x = Erosion, y = .pred)) +
  geom_point() +
  ggtitle("Cont Corn no R - rmse 0.11")


# with R ------------------------------------------------------------------

# clean data
ccR <- full %>%
  filter(crop == "cc") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

#load model
cc_modR <- readRDS("modelsFromCondor/CloverBeltMods/ContCornErosionCloverBelt_withR.rds")

cc_modR$fit$importance

# predictions
pred_ccR <- cc_modR %>%
  predict(ccR) %>%
  bind_cols(ccR)

# plot data
ggplot(pred_ccR, aes(x = Erosion, y = .pred)) +
  geom_point() +
  ggtitle("Cont Corn with R - rmse 0.13")



# corn grain --------------------------------------------------------------

# * without R ---------------------------------------------------------------

# clean data
cg <- full %>%
  filter(crop == "cg") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

#load model
cg_mod <- readRDS("modelsFromCondor/CloverBeltMods/CornGrainErosionCloverBelt.rds")

cg_mod$fit$importance

# predictions
pred_cg <- cg_mod %>%
  predict(cg) %>%
  bind_cols(cg)

# plot data
ggplot(pred_cg, aes(x = Erosion, y = .pred)) +
  geom_point() +
  ggtitle("corn grain no R - rmse 0.22")


# with R ------------------------------------------------------------------

# clean data
cgR <- full %>%
  filter(crop == "cg") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

#load model
cg_modR <- readRDS("modelsFromCondor/CloverBeltMods/CornGrainErosionCloverBelt_withR.rds")
cg_modR$fit$importance

# predictions
pred_cgR <- cg_modR %>%
  predict(cgR) %>%
  bind_cols(cgR)

# plot data
ggplot(pred_cgR, aes(x = Erosion, y = .pred)) +
  geom_point()  +
  ggtitle("Corn grain with R - rmse 0.23")


# cso ---------------------------------------------------------------------

# * without R ---------------------------------------------------------------

# clean data
cso <- full %>%
  filter(crop == "cso") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

#load model
cso_mod <- readRDS("modelsFromCondor/CloverBeltMods/csoErosionCloverBelt.rds")

cso_mod$fit$importance

# predictions
pred_cso <- cso_mod %>%
  predict(cso) %>%
  bind_cols(cso)

# plot data
ggplot(pred_cso, aes(x = Erosion, y = .pred)) +
  geom_point() +
  ggtitle("corn soy oats, no R - rmse 0.21")


# with R ------------------------------------------------------------------

# clean data
csoR <- full %>%
  filter(crop == "cso") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

#load model
cso_modR <- readRDS("modelsFromCondor/CloverBeltMods/csoErosionCloverBelt_withR.rds")
cso_modR$fit$importance

# predictions
pred_csoR <- cso_modR %>%
  predict(csoR) %>%
  bind_cols(csoR)

# plot data
ggplot(pred_csoR, aes(x = Erosion, y = .pred)) +
  geom_point()  +
  ggtitle("Corn soy oats with R - rmse 0.21")


# dr ----------------------------------------------------------------------
# * without R ---------------------------------------------------------------

# clean data
dr <- full %>%
  filter(crop == "dr") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

#load model
dr_mod <- readRDS("modelsFromCondor/CloverBeltMods/drErosionCloverBelt.rds")

dr_mod$fit$importance

# predictions
pred_dr <- dr_mod %>%
  predict(dr) %>%
  bind_cols(dr)

# plot data
ggplot(pred_dr, aes(x = Erosion, y = .pred)) +
  geom_point() +
  ggtitle("dairy rotation, no R - rmse 0.16")


# with R ------------------------------------------------------------------

# clean data
drR <- full %>%
  filter(crop == "dr") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, cover, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

#load model
dr_modR <- readRDS("modelsFromCondor/CloverBeltMods/drErosionCloverBelt_withR.rds")
dr_modR$fit$importance

# predictions
pred_drR <- dr_modR %>%
  predict(drR) %>%
  bind_cols(drR)

# plot data
ggplot(pred_drR, aes(x = Erosion, y = .pred)) +
  geom_point()  +
  ggtitle("dairy rotation with R - rmse 0.14")

# ps ----------------------------------------------------------------------
#* without R ---------------------------------------------------------------
  
  # clean data
  ps <- full %>%
  filter(crop == "ps") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

#load model
ps_mod <- readRDS("modelsFromCondor/CloverBeltMods/psErosionCloverBelt.rds")

ps_mod$fit$importance

# predictions
pred_ps <- ps_mod %>%
  predict(ps) %>%
  bind_cols(ps)

# plot data
ggplot(pred_ps, aes(x = Erosion, y = .pred)) +
  geom_point() +
  ggtitle("pasture seeding, no R - rmse 0.16")


# with R ------------------------------------------------------------------

# clean data
psR <- full %>%
  filter(crop == "ps") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, tillage, Contour, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

#load model
ps_modR <- readRDS("modelsFromCondor/CloverBeltMods/psErosionCloverBelt_withR.rds")
ps_modR$fit$importance

# predictions
pred_psR <- ps_modR %>%
  predict(psR) %>%
  bind_cols(psR)

# plot data
ggplot(pred_psR, aes(x = Erosion, y = .pred)) +
  geom_point()  +
  ggtitle("pasture seeding with R - rmse 0.14")

# pt ----------------------------------------------------------------------
#* without R ---------------------------------------------------------------

# clean data
pt <- full %>%
  filter(crop == "pt", 
         Contour == "0") %>%
  filter(OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, rotational, density, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k)) %>%
  distinct() %>%
  droplevels()


#load model
pt_mod1 <- readRDS("modelsFromCondor/CloverBeltMods/erosion/ptErosionCloverBelt.rds")
pt_mod2 <- readRDS("modelsFromCondor/CloverBeltMods/ptErosionCloverBelt.rds")

pt_mod$fit$importance

# predictions
pred_pt <- pt_mod1 %>%
  predict(pt) %>%
  bind_cols(pt)

pred_pt2 <- pt_mod2 %>%
  predict(pt) %>%
  bind_cols(pt) %>%
  rename(pred2 = .pred)
 
compare_pred <- left_join(pred_pt, pred_pt2)

# plot data
ggplot(pred_pt, aes(x = Erosion, y = .pred)) +
  geom_point() +
  ggtitle("pasture, no R - rmse 0.04")


# with R ------------------------------------------------------------------

# clean data
ptR <- full %>%
  filter(crop == "pt", 
         Contour == "0") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, rotational, density, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k, R_factor)) %>%
  distinct() %>%
  droplevels()


#load model
pt_modR <- readRDS("modelsFromCondor/CloverBeltMods/ptErosionCloverBelt_withR.rds")
pt_modR$fit$importance

# predictions
pred_ptR <- pt_modR %>%
  predict(ptR) %>%
  bind_cols(ptR)

# plot data
ggplot(pred_ptR, aes(x = Erosion, y = .pred)) +
  geom_point()  +
  ggtitle("pasture  with R - rmse 0.03")

# dl ----------------------------------------------------------------------
#* without R ---------------------------------------------------------------

# clean data
dl <- full %>%
  filter(crop == "dl") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, density, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k, R_factor)) %>%
  distinct() %>%
  droplevels()


#load model
dl_mod <- readRDS("modelsFromCondor/CloverBeltMods/dlErosionCloverBelt.rds")

dl_mod$fit$importance

# predictions
pred_dl <- dl_mod %>%
  predict(dl) %>%
  bind_cols(dl)

# plot data
ggplot(pred_dl, aes(x = Erosion, y = .pred)) +
  geom_point() +
  ggtitle("dry lot, no R - rmse 2.36")


# with R ------------------------------------------------------------------

# clean data
dlR <- full %>%
  filter(crop == "dl") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(Erosion, density, total_DM_lbs, slope, slopelenusle.r, sand, silt, clay, k, R_factor)) %>%
  distinct() %>%
  droplevels()


#load model
dl_modR <- readRDS("modelsFromCondor/CloverBeltMods/dlErosionCloverBelt_withR.rds")
dl_modR$fit$importance

# predictions
pred_dlR <- dl_modR %>%
  predict(dlR) %>%
  bind_cols(dlR)

# plot data
ggplot(pred_dlR, aes(x = Erosion, y = .pred)) +
  geom_point()  +
  ggtitle("dry lot with R - rmse 1.01")


