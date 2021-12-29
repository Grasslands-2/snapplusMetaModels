# this script checks the metrics of the ffcn models created from the clover belt snapplus runs

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
  dplyr::select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
cc_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/ContCornFFCN_CloverBelt.rds")

cc_mod$fit$importance

# predictions
pred_cc <- cc_mod %>%
  predict(cc) %>%
  bind_cols(cc)

# plot data
ggplot(pred_cc, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("Cont Corn no R - rmse 0.44")


# * with R ------------------------------------------------------------------

# clean data
ccR <- full %>%
  filter(crop == "cc",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
cc_modR <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/ContCornFFCN_CloverBelt_withR.rds")

cc_modR$fit$importance

# predictions
pred_ccR <- cc_modR %>%
  predict(ccR) %>%
  bind_cols(ccR)

# plot data
ggplot(pred_ccR, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("Cont Corn with R - rmse 0.08")

# * low OM ------------------------------------------------------------------

# clean data
cc <- full %>%
  filter(crop == "cc", 
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
cc_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcnLowOM/ContCornFFCN_CloverBelt.rds")


# predictions
pred_cc <- cc_mod %>%
  predict(cc) %>%
  bind_cols(cc)

# plot data
ggplot(pred_cc, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("Cont Corn without R - rmse 0.43")



# corn grain --------------------------------------------------------------

# * without R ---------------------------------------------------------------

# clean data
cg <- full %>%
  filter(crop == "cg") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
cg_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/cornGrainFFCN_CloverBelt.rds")

cg_mod$fit$importance

# predictions
pred_cg <- cg_mod %>%
  predict(cg) %>%
  bind_cols(cg)

# plot data
ggplot(pred_cg, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("corn grain no R - rmse 0.48")


# * with R ------------------------------------------------------------------

# clean data
cgR <- full %>%
  filter(crop == "cg",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
cg_modR <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/cornGrainFFCN_CloverBelt_withR.rds")
cg_modR$fit$importance

# predictions
pred_cgR <- cg_modR %>%
  predict(cgR) %>%
  bind_cols(cgR)

# plot data
ggplot(pred_cgR, aes(x = ffCN, y = .pred)) +
  geom_point()  +
  ggtitle("Corn grain with R - rmse 0.12")

# * low om ------------------------------------------------------------------

# clean data
cg <- full %>%
  filter(crop == "cg",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
cg_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcnLowOM/cornGrainFFCN_CloverBelt.rds")

cg_mod$fit$importance

# predictions
pred_cg <- cg_mod %>%
  predict(cg) %>%
  bind_cols(cg)

# plot data
ggplot(pred_cg, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("corn grain no R - rmse 0.50")


# cso ---------------------------------------------------------------------

# * without R ---------------------------------------------------------------

# clean data
cso <- full %>%
  filter(crop == "cso") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
cso_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/csoFFCN_CloverBelt.rds")

cso_mod$fit$importance

# predictions
pred_cso <- cso_mod %>%
  predict(cso) %>%
  bind_cols(cso)

# plot data
ggplot(pred_cso, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("corn soy oats, no R - rmse 0.47")


# * with R ------------------------------------------------------------------

# clean data
csoR <- full %>%
  filter(crop == "cso",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
cso_modR <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/csoFFCN_CloverBelt_withR.rds")
cso_modR$fit$importance

# predictions
pred_csoR <- cso_modR %>%
  predict(csoR) %>%
  bind_cols(csoR)

# plot data
ggplot(pred_csoR, aes(x = ffCN, y = .pred)) +
  geom_point()  +
  ggtitle("Corn soy oats with R - rmse 0.12")

# * low OM ---------------------------------------------------------------

# clean data
cso <- full %>%
  filter(crop == "cso",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
cso_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcnLowOM/csoFFCN_CloverBelt.rds")

cso_mod$fit$importance

# predictions
pred_cso <- cso_mod %>%
  predict(cso) %>%
  bind_cols(cso)

# plot data
ggplot(pred_cso, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("corn soy oats, no R - rmse 0.47")



# dr ----------------------------------------------------------------------

# * without R ---------------------------------------------------------------

# clean data
dr <- full %>%
  filter(crop == "dr") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
dr_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/drFFCN_CloverBelt.rds")

dr_mod$fit$importance

# predictions
pred_dr <- dr_mod %>%
  predict(dr) %>%
  bind_cols(dr)

# plot data
ggplot(pred_dr, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("dairy rotation, no R - rmse 0.42")


# * with R ------------------------------------------------------------------

# clean data
drR <- full %>%
  filter(crop == "dr",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
dr_modR <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/drFFCN_CloverBelt_withR.rds")
dr_modR$fit$importance

# predictions
pred_drR <- dr_modR %>%
  predict(drR) %>%
  bind_cols(drR)

# plot data
ggplot(pred_drR, aes(x = ffCN, y = .pred)) +
  geom_point()  +
  ggtitle("dairy rotation with R - rmse 0.08")

# * low OM ---------------------------------------------------------------

# clean data
dr <- full %>%
  filter(crop == "dr",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  select(c(ffCN,cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()

#load model
dr_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcnLowOM/drFFCN_CloverBelt.rds")

dr_mod$fit$importance

# predictions
pred_dr <- dr_mod %>%
  predict(dr) %>%
  bind_cols(dr)

# plot data
ggplot(pred_dr, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("dairy rotation, no R - rmse 0.42")


# ps ----------------------------------------------------------------------

#* without R ---------------------------------------------------------------

# clean data
ps <- full %>%
  filter(crop == "ps") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
  droplevels()

#load model
ps_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/psFFCN_CloverBelt.rds")

ps_mod$fit$importance

# predictions
pred_ps <- ps_mod %>%
  predict(ps) %>%
  bind_cols(ps)

# plot data
ggplot(pred_ps, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("pasture seeding, no R - rmse 0.31")


# * with R ------------------------------------------------------------------

# clean data
psR <- full %>%
  filter(crop == "ps",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN, tillage, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()


#load model
ps_modR <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/psFFCN_CloverBelt_withR.rds")
ps_modR$fit$importance

# predictions
pred_psR <- ps_modR %>%
  predict(psR) %>%
  bind_cols(psR)

# plot data
ggplot(pred_psR, aes(x = ffCN, y = .pred)) +
  geom_point()  +
  ggtitle("pasture seeding with R - rmse 0.07")

#* low OM ---------------------------------------------------------------

# clean data
ps <- full %>%
  filter(crop == "ps",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
  droplevels()

#load model
ps_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcnLowOM/psFFCN_CloverBelt.rds")

ps_mod$fit$importance

# predictions
pred_ps <- ps_mod %>%
  predict(ps) %>%
  bind_cols(ps)

# plot data
ggplot(pred_ps, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("pasture seeding, no R - rmse 0.31")

# pt ----------------------------------------------------------------------

#* without R ---------------------------------------------------------------

# clean data
pt <- full %>%
  filter(crop == "pt") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN, rotational, density, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()


#load model
pt_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/ptFFCN_CloverBelt.rds")

pt_mod$fit$importance

# predictions
pred_pt <- pt_mod %>%
  predict(pt) %>%
  bind_cols(pt)

# plot data
ggplot(pred_pt, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("pasture, no R - rmse 0.27")


# * with R ------------------------------------------------------------------

# clean data
ptR <- full %>%
  filter(crop == "pt",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN, rotational, density, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()


#load model
pt_modR <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/ptFFCN_CloverBelt_withR.rds")
pt_modR$fit$importance

# predictions
pred_ptR <- pt_modR %>%
  predict(ptR) %>%
  bind_cols(ptR)

# plot data
ggplot(pred_ptR, aes(x = ffCN, y = .pred)) +
  geom_point()  +
  ggtitle("pasture  with R - rmse 0.05")

#* low OM ---------------------------------------------------------------

# clean data
pt <- full %>%
  filter(crop == "pt",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN, rotational, density, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()


#load model
pt_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcnLowOM/ptFFCN_CloverBelt.rds")

pt_mod$fit$importance

# predictions
pred_pt <- pt_mod %>%
  predict(pt) %>%
  bind_cols(pt)

# plot data
ggplot(pred_pt, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("pasture, no R - rmse 0.27")


# dl ----------------------------------------------------------------------
#* without R ---------------------------------------------------------------

# clean data
dl <- full %>%
  filter(crop == "dl") %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN, density, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()


#load model
dl_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/dlFFCN_CloverBelt.rds")

dl_mod$fit$importance

# predictions
pred_dl <- dl_mod %>%
  predict(dl) %>%
  bind_cols(dl)

# plot data
ggplot(pred_dl, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("dry lot, no R - rmse 0.49")


# * with R ------------------------------------------------------------------

# clean data
dlR <- full %>%
  filter(crop == "dl",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN, density, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()


#load model
dl_modR <- readRDS("modelsFromCondor/CloverBeltMods/ffcn/dlFFCN_CloverBelt_withR.rds")
dl_modR$fit$importance

# predictions
pred_dlR <- dl_modR %>%
  predict(dlR) %>%
  bind_cols(dlR)

# plot data
ggplot(pred_dlR, aes(x = ffCN, y = .pred)) +
  geom_point()  +
  ggtitle("dry lot with R - rmse 0.17")

#* low OM---------------------------------------------------------------

# clean data
dl <- full %>%
  filter(crop == "dl",
         OM < 20) %>%
  mutate_if(is.character, as.factor) %>%
  select(c(ffCN, density, hydgrp, total_DM_lbs, sand, silt, clay, R_factor)) %>%
  distinct() %>%
  droplevels()


#load model
dl_mod <- readRDS("modelsFromCondor/CloverBeltMods/ffcnLowOM/dlFFCN_CloverBelt.rds")

dl_mod$fit$importance

# predictions
pred_dl <- dl_mod %>%
  predict(dl) %>%
  bind_cols(dl)

# plot data
ggplot(pred_dl, aes(x = ffCN, y = .pred)) +
  geom_point() +
  ggtitle("dry lot, no R - rmse 0.53")


