# this script assesses the distribution of OM in the clover belt 
# to determine how many observations are lost if we omit different levels of OM

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

#corn df
#clean data
cc <- full %>%
  filter(crop == "cc") %>%
  mutate_if(is.character, as.factor) %>%
  dplyr::select(c(PI, Erosion, cover, tillage, Contour, initialP, total_DM_lbs, totalP2O5_lbs, slope, slopelenusle.r, LSsurgo,
                  total.depth, OM, silt, k, R_factor)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  distinct() %>%
  droplevels()

hist(cc$OM)

ccLowOM <- cc %>%
  filter(OM < 20)

nrow(ccLowOM)/nrow(cc)
# 0.92

ccLowestOM <- cc %>%
  filter(OM < 10)

nrow(ccLowestOM)/nrow(cc)
# 0.89