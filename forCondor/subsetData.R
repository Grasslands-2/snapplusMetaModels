#load libraries
library(tidyverse)
library(ggplot2)
library(gt)

#load data
#snap <- read.table("data/joinedSnapSurgo.txt", sep = "|", header = TRUE) #joinedSnapSurgo.txt
#snap <- read.table(file.choose(), sep = "|", header = TRUE)
# snap <- snap %>%
#   mutate_if(is.character, factor)
# summary(snap)
# soil_sum <- snap %>%
#   group_by(County, SoilSymbol, SoilSeries) %>%
#   tally()
# crop_sum <- snap %>%
#   group_by(crop) %>%
#   tally() 

crop_sum %>% gt()

snap <- read.csv("data/TainterSnapSurgo.csv")

# continuous corn all -----------------------------------------------------


ccErosion <- snap %>%
  filter(crop == "cc") %>%
  dplyr::select(c(Erosion, cover, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

summary(ccErosion)

write.csv(ccErosion, "ccErosion.csv", row.names = FALSE, quote = FALSE)

ccPI <- snap %>%
  filter(crop == "cc") %>%
  dplyr::select(c(PI, Erosion, cover, tillage, slope, Contour, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo)) %>%
  mutate(Contour = as.factor(Contour))

write.csv(ccPI, "ccPI.csv", row.names = FALSE, quote = FALSE)

ccCurve <- snap %>%
  filter(crop == "cc",
         initialP == 25,
         Contour == 0) %>%
  dplyr::select(c(ffCN, cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
  droplevels()

summary(ccCurve)

write.csv(ccCurve, "forCondor/curveData/ccCurve.csv", row.names = FALSE, quote = FALSE)


# continuous corn no cover ------------------------------------------------


ccncErosion <- snap %>%
  filter(crop == "cc",
         cover == "nc") %>%
  dplyr::select(c(Erosion, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

summary(ccncErosion)

write.csv(ccncErosion, "ContCornNoCoverErosion.csv", row.names = FALSE, quote = FALSE)


# continuous corn with cover ----------------------------------------------


ccccErosion <- snap %>%
  filter(crop == "cc",
         cover == "cc" 
         |cover == "gcis"
         |cover == "gcds") %>%
  dplyr::select(c(Erosion, cover, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

summary(ccccErosion)

write.csv(ccccErosion, "ContCornWithCoverErosion.csv", row.names = FALSE, quote = FALSE)


# corn grain --------------------------------------------------------------


cg <- snap %>%
  filter(crop == "cg") %>%
  dplyr::select(c(Erosion, cover, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

summary(cg)

write.csv(cg, "cornGrainErosion.csv", row.names = FALSE, quote = FALSE)

cgPI <- snap %>%
  filter(crop == "cg") %>%
  dplyr::select(c(PI, Erosion, cover, tillage, slope, Contour, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo))

write.csv(cgPI, "cgPI.csv", row.names = FALSE, quote = FALSE)

cgCurve <- snap %>%
  filter(crop == "cg",
         initialP == 25,
         Contour == 0) %>%
  dplyr::select(c(ffCN, cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
  droplevels()

summary(cgCurve)

write.csv(cgCurve, "forCondor/curveData/cgCurve.csv", row.names = FALSE, quote = FALSE)

# corn grain with cover ---------------------------------------------------


cgcc <- snap %>%
  filter(crop == "cg",
         cover == "cc" 
         |cover == "gcis"
         |cover == "gcds") %>%
  dplyr::select(c(Erosion, cover, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

summary(cgcc)

write.csv(cgcc, "cornGrainCoverErosion.csv", row.names = FALSE, quote = FALSE)

cgccPI <- snap %>%
  filter(crop == "cg",
         cover == "cc" 
         |cover == "gcis"
         |cover == "gcds") %>%
  dplyr::select(c(PI, Erosion, cover, tillage, slope, Contour, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo))

write.csv(cgccPI, "cornGrainCoverPI.csv", row.names = FALSE, quote = FALSE)


# corn grain no cover -----------------------------------------------------


cgnc <- snap %>%
  filter(crop == "cg",
         cover == "nc") %>%
  dplyr::select(c(Erosion, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

summary(cgnc)

write.csv(cgnc, "cornGrainNoCoverErosion.csv", row.names = FALSE, quote = FALSE)

cgNcPI <- snap %>%
  filter(crop == "cg",
         cover == "nc") %>%
  dplyr::select(c(PI, Erosion, tillage, slope, Contour, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo))

write.csv(cgNcPI, "cornGrainNoCoverPI.csv", row.names = FALSE, quote = FALSE)

# dairy rotation ----------------------------------------------------------

dr <- snap %>%
  filter(crop == "dr") %>%
  dplyr::select(c(Erosion, cover, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour),
         tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  droplevels()

summary(dr)

write.csv(dr, "dairyRotationErosion.csv", row.names = FALSE, quote = FALSE)

drCurve <- snap %>%
  filter(crop == "dr",
         initialP == 25,
         Contour == 0) %>%
  dplyr::select(c(ffCN, cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  mutate(tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  distinct() %>%
  droplevels()

summary(drCurve)

write.csv(drCurve, "forCondor/curveData/drCurve.csv", row.names = FALSE, quote = FALSE)


# dairy rotation no cover -------------------------------------------------

drnc <- snap %>%
  filter(crop == "dr",
         cover == "nc") %>%
  dplyr::select(c(Erosion, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour),
         tillage = recode(tillage, 
                          "sm" = "sv")) %>%
  droplevels()

summary(drnc)

write.csv(drnc, "dairyRotationNoCoverErosion.csv", row.names = FALSE, quote = FALSE)

drNcPI <- snap %>%
  filter(crop == "dr",
         cover == "nc") %>%
  dplyr::select(c(PI, Erosion, tillage, slope, Contour, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo))

write.csv(drNcPI, "DairyRotNoCoverPI.csv", row.names = FALSE, quote = FALSE)


# dairy rotation with cover -----------------------------------------------



drccPI <- snap %>%
  filter(crop == "dr",
         cover == "cc" 
         |cover == "gcis"
         |cover == "gcds") %>%
  dplyr::select(c(PI, Erosion, cover, tillage, slope, Contour, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo))

write.csv(drccPI, "DairyRotCoverPI.csv", row.names = FALSE, quote = FALSE)


# corn soy oat ------------------------------------------------------------

cso <- snap %>%
  filter(crop == "cso") %>%
  dplyr::select(c(Erosion, cover, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

summary(cso)

write.csv(cso, "cornSoyOatErosion.csv", row.names = FALSE, quote = FALSE)

csoPI <- snap %>%
  filter(crop == "cso") %>%
  dplyr::select(c(PI, Erosion, cover, tillage, slope, Contour, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo))

write.csv(csoPI, "csoPI.csv", row.names = FALSE, quote = FALSE)


csoCurve <- snap %>%
  filter(crop == "cso",
         initialP == 25,
         Contour == 0) %>%
  dplyr::select(c(ffCN, cover, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
  droplevels()

summary(csoCurve)

write.csv(csoCurve, "forCondor/curveData/csoCurve.csv", row.names = FALSE, quote = FALSE)

# corn soy oat with cover -------------------------------------------------

csoCcPI <- snap %>%
  filter(crop == "cso",
         cover == "cc"
         | cover == "gcds"
         | cover == "gcis") %>%
  dplyr::select(c(PI, Erosion, cover, tillage, slope, Contour, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo))

write.csv(csoCcPI, "csoCcPI.csv", row.names = FALSE, quote = FALSE)


# corn soy oats no cover --------------------------------------------------

csoNcPI <- snap %>%
  filter(crop == "cso",
         cover == "nc") %>%
  dplyr::select(c(PI, Erosion, tillage, slope, Contour, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo))

write.csv(csoNcPI, "csoNcPI.csv", row.names = FALSE, quote = FALSE)


# pasture seeding ---------------------------------------------------------

ps <- snap %>%
  filter(crop == "ps") %>%
  dplyr::select(c(Erosion, tillage, slope, Contour, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  mutate(Contour = as.factor(Contour)) %>%
  droplevels()

summary(ps)

write.csv(ps, "pastureSeedingErosion.csv", row.names = FALSE, quote = FALSE)

psPI <- snap %>%
  filter(crop == "ps") %>%
  dplyr::select(c(PI, Erosion,tillage, slope, Contour, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo))

write.csv(psPI, "psPI.csv", row.names = FALSE, quote = FALSE)

psCurve <- snap %>%
  filter(crop == "ps",
         initialP == 25,
         Contour == 0) %>%
  dplyr::select(c(ffCN, tillage, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
  droplevels()

summary(psCurve)

write.csv(psCurve, "forCondor/curveData/psCurve.csv", row.names = FALSE, quote = FALSE)


# dry lot -----------------------------------------------------------------

dl <- snap %>%
  filter(crop == "dl") %>%
  dplyr::select(c(Erosion, density, slope, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  droplevels()

summary(dl)

write.csv(dl, "dryLotErosion.csv", row.names = FALSE, quote = FALSE)

dlPI <- snap %>%
  filter(crop == "dl") %>%
  distinct() %>%
  dplyr::select(c(PI, Erosion, density, slope, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo))

write.csv(dlPI, "dlPI.csv", row.names = FALSE, quote = FALSE)

dlCurve <- snap %>%
  filter(crop == "dl",
         initialP == 25) %>%
  distinct() %>%
  dplyr::select(c(ffCN, hydgrp, total_DM_lbs, density, sand,  clay, silt)) %>%
  droplevels()

summary(dlCurve)

write.csv(dlCurve, "forCondor/curveData/dlCurve.csv", row.names = FALSE, quote = FALSE)


# pasture -----------------------------------------------------------------

pt <- snap %>%
  filter(crop == "pt", 
         Contour == "0") %>%
  dplyr::select(c(Erosion, density, rotational, slope, total_DM_lbs, slopelenusle.r, sand, silt, clay, k)) %>%
  droplevels()

summary(pt)

write.csv(pt, "pastureErosion.csv", row.names = FALSE, quote = FALSE)

ptCurve <- snap %>%
  filter(crop == "pt",
         initialP == 25, 
         Contour == 0) %>%
  dplyr::select(c(ffCN, rotational, density, hydgrp, total_DM_lbs, sand, silt, clay)) %>%
  distinct() %>%
  droplevels()

summary(ptCurve)

write.csv(ptCurve, "forCondor/curveData/ptCurve.csv", row.names = FALSE, quote = FALSE)

ptPI <- snap %>%
  filter(crop == "pt",
         Contour == "0") %>%
  dplyr::select(c(PI, Erosion, density, rotational, slope, initialP, OM, totalP2O5_lbs, total_DM_lbs, slopelenusle.r, 
                  silt, k, total.depth, LSsurgo))

write.csv(ptPI, "ptPI.csv", row.names = FALSE, quote = FALSE)
