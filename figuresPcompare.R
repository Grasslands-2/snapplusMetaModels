library(tidyverse)

#load data
snap <- read.table("data/JoinedSnapSurgo.txt", sep = "|", header = TRUE)
snap <- snap %>%
  mutate_if(is.character, as.factor)
summary(snap)

sum_snap <- snap %>%
  group_by(County, SoilSeries, SoilSymbol) %>%
  summarise(count = n())


#  first question: dominant soil types in tainter creek ------------------

soil <- read.table("../../../Soil Data/SSURGO data/Final Clean Soil/FinalSoilCrawfordVernon.txt", sep = "|", header = TRUE)
colnames(soil)
majorSoils <- soil %>%
  filter(majcompflag == "Yes") %>%
  arrange(desc(muacres))

head(majorSoils)

# Vernon - Elbaville 1125F, Dorerton 1125F, Pepin 125D2, Seaton 115D2
# Crawford - Elbaville 1125F, Dorerton 1125F, Pepin 125D2, Seaton 115D2


# soils with less steep slopes --------------------------------------------

#Algansee crawford 1658A
#orion 628A vernon
#Arenzville 626A crawford or vernon
#crawford chaseburg 616B


# filter snap by above soils ----------------------------------------------

elba <- snap %>%
  filter(SoilSeries == "elbaville", SoilSymbol == "1125F")
dor <- snap %>%
  filter(SoilSeries == "dorerton", SoilSymbol == "1125F")
pep <- snap %>%
  filter(SoilSeries == "pepin", SoilSymbol == "125D2")
seat <- snap %>%
  filter(SoilSeries == "seaton", SoilSymbol == "115D2")

alga <- snap %>%
  filter(SoilSeries == "algansee", SoilSymbol == "1658A")
orion <- snap %>%
  filter(SoilSeries == "orion", SoilSymbol == "628A")
aren <- snap %>%
  filter(SoilSeries == "arenzville", SoilSymbol == "626A")
chase <- snap %>%
  filter(SoilSeries == "chaseburg", SoilSymbol == "616B")




# bind soils together, group by soil, create low, mid and high slope --------

elba_repSlope <- elba %>%
  summarise(repSlope = round(mean(slope)))
elbaSlope <- elba %>% semi_join(elba_repSlope, by = c("slope" = "repSlope"))

dor_repSlope <- dor %>%
  summarise(repSlope = round(mean(slope)))
dorSlope <- dor %>% semi_join(dor_repSlope, by = c("slope" = "repSlope"))

pep_repSlope <- pep %>%
  summarise(repSlope = round(mean(slope)))
pepSlope <- pep %>% semi_join(pep_repSlope, by = c("slope" = "repSlope"))

seat_repSlope <- seat %>%
  summarise(repSlope = round(median(slope)))
seatSlope <- seat %>% semi_join(seat_repSlope, by = c("slope" = "repSlope"))

majSnapSoils <- bind_rows(elbaSlope, dorSlope, pepSlope, seatSlope) %>%
  filter(initialP == 25,
         Contour == 0,
         ManureApp == 100) %>%
  droplevels()
summary(majSnapSoils)

alga_repSlope <- alga %>%
  summarise(repSlope = round(median(slope)))
algaSlope <- alga %>% semi_join(alga_repSlope, by = c("slope" = "repSlope"))

orion_repSlope <- orion %>%
  summarise(repSlope = round(median(slope)))
orionSlope <- orion %>% semi_join(orion_repSlope, by = c("slope" = "repSlope"))

aren_repSlope <- aren %>%
  summarise(repSlope = round(median(slope)))
arenSlope <- aren %>% semi_join(aren_repSlope, by = c("slope" = "repSlope"))

chase_repSlope <- data.frame(repSlope = 2.5)
chaseSlope <- chase %>% semi_join(chase_repSlope, by = c("slope" = "repSlope"))

SnapSoils <- bind_rows(elbaSlope, dorSlope, pepSlope, seatSlope, algaSlope, orionSlope, arenSlope, chaseSlope) %>%
  filter(initialP == 25,
         Contour == 0,
         ManureApp == 100) %>%
  droplevels()
summary(SnapSoils)

ccncnt <- SnapSoils %>%
  filter(crop == "cc", cover == "nc", tillage == "nt")
ccccnt <- SnapSoils %>%
  filter(crop == "cc", cover == "cc", tillage == "nt")
ccgcisnt <- SnapSoils %>%
  filter(crop == "cc", cover == "gcis", tillage == "nt")
drncnt <- SnapSoils %>%
  filter(crop == "dr", cover == "nc", tillage == "nt")
drccnt <- SnapSoils %>%
  filter(crop == "dr", cover == "cc", tillage == "nt")
drgcisnt <- SnapSoils %>%
  filter(crop == "dr", cover == "gcis", tillage == "nt")
ptrt <- SnapSoils %>%
  filter(crop == "pt", rotational == "rt")

compare <- bind_rows(ccncnt, ccccnt, ccgcisnt, drncnt, drccnt, drgcisnt, ptrt)
compare <- compare %>%
  mutate(Soil = paste(SoilSeries, SoilSymbol),
         management = paste(crop, cover, tillage, rotational)) %>%
  mutate(management = fct_reorder(management, desc(Erosion)))

ggplot(compare, aes(x = management, y = PI, fill = management)) +
  geom_col() +
  facet_wrap(~Soil) + 
  theme(axis.text.x = element_blank(),
        legend.text = element_text(size = 16),
        strip.text.x = element_text(size = 12))

ggplot(compare, aes(x = management, y = Erosion, fill = management)) +
  geom_col() +
  facet_wrap(~Soil) + 
  theme(axis.text.x = element_blank(),
        legend.text = element_text(size = 16),
        strip.text.x = element_text(size = 12))

dr <- snap %>%
  filter(crop == "dr")
mean(dr$Erosion) # 4.7
summary(dr$Erosion) # max = 52
mean(dr$PI) # 8.2

cc <- snap %>%
  filter(crop == "cc")
mean(cc$Erosion) # 4.74
summary(cc$Erosion) #max = 61
mean(cc$PI) #8.06

ccncsn <- SnapSoils %>%
  filter(crop == "cc", cover == "nc", tillage == "sn")
ccccsn <- SnapSoils %>%
  filter(crop == "cc", cover == "cc", tillage == "sn")
ccgcissc <- SnapSoils %>%
  filter(crop == "cc", cover == "gcis", tillage == "sc")
drncsn <- SnapSoils %>%
  filter(crop == "dr", cover == "nc", tillage == "sn")
drccsc <- SnapSoils %>%
  filter(crop == "dr", cover == "cc", tillage == "sc")
drgcissc <- SnapSoils %>%
  filter(crop == "dr", cover == "gcis", tillage == "sc")
ptrt <- SnapSoils %>%
  filter(crop == "pt", rotational == "rt")

compare2 <- bind_rows(ccncsn, ccccsn, ccgcissc, drncsn, drccsc, drgcissc, ptrt)
compare2 <- compare2 %>%
  mutate(Soil = paste(SoilSeries, SoilSymbol),
         management = paste(crop, cover, tillage, rotational))

ggplot(compare2, aes(x = reorder(management, - PI), y = PI, fill = management)) +
  geom_col() +
  facet_wrap(~Soil)

ggplot(compare2, aes(x = reorder(management, - Erosion), y = Erosion, fill = management)) +
  geom_col() +
  facet_wrap(~Soil)
