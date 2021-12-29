# this script is beginning to look at decreasing the file sizes of tidy models with the butcher package
# the goal is to be able to speed the loading time of the shiny app

library(tidymodels)
library(butcher)

# load models
ccErosion <- readRDS("modelsFromCondor/ContCornErosion.rds")

newCCerosion <- butcher(ccErosion)
new <- butcher(newCCerosion)
new
saveRDS(newCCerosion, "modelsFromCondor/tainterCreek_ccErosion.rds")

ccErosion
newCCerosion


modButchcc <- ccErosion %>%
  axe_call() %>%
  axe_ctrl() %>%
  axe_env()

butch <- butcher(ccErosion)

lobstr::obj_size(ccErosion)
lobstr::obj_size(modButchcc)
lobstr::obj_size(butch)

ccPI <- readRDS("models/ContCornTidyPI.rds")
lobstr::obj_size(ccPI)
ccPI_butch <- butcher(ccPI)
lobstr::obj_size(ccPI_butch)

ccPI
ccPI_butch
ccPI_butch