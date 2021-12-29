# this script uses the spreadsheet of wisconsin climates by county
# which has the R_factor for each county and the monthly precipitation and erosivity
# and cleans the spreadsheet into tidy format for later use

library(tidyverse)

weather <- read_csv("data/WIClimates by county R2.csv")

summary(weather)

# convert wide to long
# select columns with e vs p
ero <- weather %>%
  select(c(County, R_Factor, contains("(e)"))) %>%
  mutate(var= "erosion") %>%
  rename_with(~ gsub(" (e)", "", .x, fixed = TRUE))
prec <- weather %>%
  select(c(County, R_Factor, contains("(p)"))) %>%
  mutate(var = "precip") %>%
  rename_with(~ gsub(" (p)", "", .x, fixed = TRUE))

weather_clean <- bind_rows(ero, prec)
write.csv(weather_clean, "data/weatherByCounty.csv", row.names = FALSE, quote = FALSE)

summary(weather_clean$R_Factor)
