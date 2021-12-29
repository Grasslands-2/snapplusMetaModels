# this script compares erosion model outputs for the tainter creek area for full crop rotations and no cover vs cover subsets
# for continuous corn, corn grain, and dairy rotation
# it uses predict and bind_cols functions and creates 3d plots for comparison using plotly

#load library
library(tidyverse)
library(tidymodels)
library(randomForest)
library(caret)
library(plotly)

#load data
dr <- read_csv("/Users/elissachasen/Desktop/forCondor/erosionData/dairyRotationErosion.csv") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour))
drNc <- dr %>%
  filter(cover == "nc")
drCc <- dr %>%
  filter(cover == "cc" | cover == "gcis" | cover == "gcds")

#models
drMod <- readRDS("modelsFromCondor/dairyRotationErosion.rds")
drCcMod <- readRDS("modelsFromCondor/dairyRotationCoverErosion.rds")
drNcMod <- readRDS("modelsFromCondor/dairyRotationNoCoverErosion.rds")
#predictions
drWithPred <- drMod %>%
  predict(dr) %>%
  bind_cols(dr) %>%
  distinct()
drNcWithPred <- drNcMod %>%
  predict(drNc) %>%
  bind_cols(drNc)  %>%
  distinct()
drCcWithPred <- drCcMod %>%
  predict(drCc) %>%
  bind_cols(drCc) %>%
  distinct()
# row bind cover subsets
drCovers <- bind_rows(drNcWithPred, drCcWithPred) %>%
  mutate(coverPred = .pred) %>%
  dplyr::select(-.pred)
#leftJoin drCovers and dr to compare predictions
DrWithPreds <- left_join(drWithPred, drCovers)
summary(DrWithPreds)
ggplot(DrWithPreds, aes(x = .pred, y = coverPred, color = cover)) +
  geom_point()


fig <- plot_ly(DrWithPreds, x = ~.pred, y = ~coverPred, z = ~Erosion, color = ~cover,
               text = ~paste('full pred:', .pred, '<br>subset pred:', coverPred, '<br>Erosion:', Erosion))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Full data prediction'),
                                   yaxis = list(title = 'Subset data prediction'),
                                   zaxis = list(title = 'SnapPlus Erosion')))

fig
rm(list = ls())


# continuous corn
#load data
cc <- read_csv("/Users/elissachasen/Desktop/forCondor/ccErosion.csv") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour))
ccNc <- cc %>%
  filter(cover == "nc")
ccCc <- cc %>%
  filter(cover == "cc" | cover == "gcis" | cover == "gcds")

#models
ccMod <- readRDS("modelsFromCondor/ContCornErosion.rds")
ccCcMod <- readRDS("modelsFromCondor/ContCornWithCoverErosion.rds")
ccNcMod <- readRDS("modelsFromCondor/ContCornNoCoverErosion.rds")
#predictions
ccWithPred <- ccMod %>%
  predict(cc) %>%
  bind_cols(cc) %>%
  dplyr::arrange(cover, tillage, slope, Contour) %>%
  distinct()
ccNcWithPred <- ccNcMod %>%
  predict(ccNc) %>%
  bind_cols(ccNc) %>%
  distinct()
ccCcWithPred <- ccCcMod %>%
  predict(ccCc) %>%
  bind_cols(ccCc) %>%
  distinct()

# row bind cover subsets
ccCovers <- bind_rows(ccCcWithPred, ccNcWithPred) %>%
  mutate(coverPreds = .pred) %>%
  select(-.pred) %>%
  dplyr::arrange(cover, tillage, slope, Contour)
#leftJoin drCovers and dr to compare predictions

ccWithPreds <- left_join(ccWithPred, ccCovers) 

summary(ccWithPreds)
ggplot(ccWithPreds, aes(x = .pred, y = coverPreds, color = cover)) +
  geom_point()


fig <- plot_ly(ccWithPreds, x = ~.pred, y = ~coverPreds, z = ~Erosion, color = ~cover,
               text = ~paste('full pred:', .pred, '<br>subset pred:', coverPreds, '<br>Erosion:', Erosion))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Full data prediction'),
                                   yaxis = list(title = 'Subset data prediction'),
                                   zaxis = list(title = 'SnapPlus Erosion')))

fig

#corn grain
cg <- read_csv("/Users/elissachasen/Desktop/forCondor/erosionData/cornGrainErosion.csv") %>%
  mutate_if(is.character, factor) %>%
  mutate(Contour = as.factor(Contour))
cgNc <- cg %>%
  filter(cover == "nc")
cgCc <- cg %>%
  filter(cover == "cc" | cover == "gcis" | cover == "gcds")

#models
cgMod <- readRDS("modelsFromCondor/cornGrainErosion.rds")
cgCcMod <- readRDS("modelsFromCondor/cornGrainCoverErosion.rds")
cgNcMod <- readRDS("modelsFromCondor/cornGrainNoCoverErosion.rds")
#predictions
cgWithPred <- cgMod %>%
  predict(cg) %>%
  bind_cols(cg) %>%
  distinct()
cgNcWithPred <- cgNcMod %>%
  predict(cgNc) %>%
  bind_cols(cgNc) %>%
  distinct()
cgCcWithPred <- cgCcMod %>%
  predict(cgCc) %>%
  bind_cols(cgCc) %>%
  distinct()

# row bind cover subsets
cgCovers <- bind_rows(cgCcWithPred, cgNcWithPred) %>%
  mutate(coverPreds = .pred) %>%
  dplyr::select(-.pred)
#leftJoin drCovers and dr to compare predictions

cgWithPreds <- left_join(cgWithPred, cgCovers) 

summary(cgWithPreds)
ggplot(cgWithPreds, aes(x = .pred, y = coverPreds, color = cover)) +
  geom_point()


fig <- plot_ly(cgWithPreds, x = ~.pred, y = ~coverPreds, z = ~Erosion, color = ~cover,
               text = ~paste('full pred:', .pred, '<br>subset pred:', coverPreds, '<br>Erosion:', Erosion))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Full data prediction'),
                                   yaxis = list(title = 'Subset data prediction'),
                                   zaxis = list(title = 'SnapPlus Erosion')))

fig
