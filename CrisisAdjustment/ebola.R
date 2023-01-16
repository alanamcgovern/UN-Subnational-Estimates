
library(tidyverse)
library(rgdal)

# Data from this paper:
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5145133/

# Backer JA, Wallinga J. Spatiotemporal analysis of the 2014
# Ebola epidemic in West Africa. PLoS computational biology.
# 2016 Dec 8;12(12):e1005210.

df <- read.table("CrisisAdjustment/ebola.txt", header=T)

df <- df %>%
  pivot_longer(cols = starts_with("W")) %>%
  group_by(country, prefecture) %>%
  summarise(deaths = sum(value)) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(prop_deaths = deaths / sum(deaths))

# UN-IGME national crisis adjustments
nat <- readxl::read_xlsx("CrisisAdjustment/Crisis_Under5_deaths_2022.xlsx")


# Guinea ------------------------------------------------------------------

# get Guinea inputs
df_GIN <- df %>% filter(country == "GUINEA")
gadm_GIN <- readOGR(
  dsn = "CrisisAdjustment/gadm41_GIN_shp",
  layer = "gadm41_GIN_2"
)

# compare adm2 we have versus what we need (gadm)
adm2_GIN <- unique(gadm_GIN$NAME_2)
setdiff(adm2_GIN, df_GIN$prefecture) %>% sort()
setdiff(df_GIN$prefecture, adm2_GIN) %>% sort()

# add Mandiana which had no ebola
df_GIN <- rbind(df_GIN, data.frame(
  prefecture = "Mandiana",
  country = "GUINEA",
  deaths = 0,
  prop_deaths = 0
))

# map adm2 names to gadm adm2 names
adm2_map_GIN <- data.frame(
  gadm = sort(adm2_GIN),
  prefecture = sort(df_GIN$prefecture)
)
df_GIN <- merge(df_GIN, adm2_map_GIN, by = "prefecture")

# get national crisis death counts
nat_GIN <- nat %>%
  filter(Country == "Guinea") %>%
  mutate(years = floor(Year)) %>% # Year is given in mid-year, convert this
  select(years, nat_ed_0_1 = `Crisis d0`, nat_ed_1_5 = `Crisis d1-4`) %>%
  mutate(country = "GUINEA")

# combine
df_GIN <- merge(df_GIN, nat_GIN, by = "country", all = T)

# convert subnational death proportions to death counts
df_GIN <- df_GIN %>%
  mutate(ed_0_1 = nat_ed_0_1 * prop_deaths,
         ed_1_5 = nat_ed_1_5 * prop_deaths)

# light formatting
df_GIN <- df_GIN %>%
  mutate(level = "admin2",
         country = "Guinea") %>%
  select(country, level, gadm, years, ed_0_1, ed_1_5)

# aggregate to Admin1
df_GIN_adm1 <- merge(
  df_GIN, select(gadm_GIN@data, gadm = NAME_2, NAME_1),
  by = "gadm"
)
df_GIN_adm1 <- df_GIN_adm1 %>%
  mutate(level = "admin1") %>%
  select(country, level, gadm = NAME_1, years, ed_0_1, ed_1_5) %>%
  group_by(country, level, gadm, years) %>%
  summarise(ed_0_1 = sum(ed_0_1),
            ed_1_5 = sum(ed_1_5))

# combine
df_GIN <- rbind(df_GIN, df_GIN_adm1)

save(df_GIN, file = "CrisisAdjustment/crisis_GIN.rda")


# Liberia -----------------------------------------------------------------

# get Liberia inputs
df_LBR <- df %>% filter(country == "LIBERIA")
gadm_LBR <- readOGR(
  dsn = "CrisisAdjustment/gadm41_LBR_shp",
  layer = "gadm41_LBR_1"
)

# compare adm1 we have versus what we need (gadm)
adm1_LBR <- unique(gadm_LBR$NAME_1)
setdiff(adm1_LBR, df_LBR$prefecture) %>% sort()
setdiff(df_LBR$prefecture, adm1_LBR) %>% sort()

# fix difference in naming
df_LBR[df_LBR$prefecture == "Gbarpolu",]$prefecture <- "Gbapolu"

# get national crisis death counts
nat_LBR <- nat %>%
  filter(Country == "Liberia") %>%
  mutate(years = floor(Year)) %>% # Year is given in mid-year, convert this
  select(years, nat_ed_0_1 = `Crisis d0`, nat_ed_1_5 = `Crisis d1-4`) %>%
  mutate(country = "LIBERIA")

# combine
df_LBR <- merge(df_LBR, nat_LBR, by = "country", all = T)

# convert subnational death proportions to death counts
df_LBR <- df_LBR %>%
  mutate(ed_0_1 = nat_ed_0_1 * prop_deaths,
         ed_1_5 = nat_ed_1_5 * prop_deaths)

# light formatting
df_LBR <- df_LBR %>%
  mutate(level = "admin1",
         country = "Liberia") %>%
  select(country, level, gadm = prefecture, years, ed_0_1, ed_1_5)

save(df_LBR, file = "CrisisAdjustment/crisis_LBR.rda")


# Sierra Leone ------------------------------------------------------------

# Get Sierra Leone inputs
df_SLE <- df %>% filter(country == "SIERRA LEONE")
gadm_SLE <- readOGR(
  dsn = "CrisisAdjustment/gadm41_SLE_shp",
  layer = "gadm41_SLE_2"
)

# compare adm2 we have versus what we need (gadm)
adm2_SLE <- unique(gadm_SLE$NAME_2)
setdiff(adm2_SLE, df_SLE$prefecture) %>% sort()
setdiff(df_SLE$prefecture, adm2_SLE) %>% sort()

# fix difference in naming
df_SLE[df_SLE$prefecture == "Freetown",]$prefecture <- "Western Urban"
df_SLE[df_SLE$prefecture == "West Rural",]$prefecture <- "Western Rural"

# get national crisis death counts
nat_SLE <- nat %>%
  filter(Country == "Sierra Leone") %>%
  mutate(years = floor(Year)) %>% # Year is given in mid-year, convert this
  select(years, nat_ed_0_1 = `Crisis d0`, nat_ed_1_5 = `Crisis d1-4`) %>%
  mutate(country = "SIERRA LEONE")

# combine
df_SLE <- merge(df_SLE, nat_SLE, by = "country", all = T)

# convert subnational death proportions to death counts
df_SLE <- df_SLE %>%
  mutate(ed_0_1 = nat_ed_0_1 * prop_deaths,
         ed_1_5 = nat_ed_1_5 * prop_deaths)

# light formatting
df_SLE <- df_SLE %>%
  mutate(level = "admin2",
         country = "Sierra Leone") %>%
  select(country, level, gadm = prefecture, years, ed_0_1, ed_1_5)

# aggregate to Admin1
df_SLE_adm1 <- merge(
  df_SLE, select(gadm_SLE@data, gadm = NAME_2, NAME_1),
  by = "gadm"
)
df_SLE_adm1 <- df_SLE_adm1 %>%
  mutate(level = "admin1") %>%
  select(country, level, gadm = NAME_1, years, ed_0_1, ed_1_5) %>%
  group_by(country, level, gadm, years) %>%
  summarise(ed_0_1 = sum(ed_0_1),
            ed_1_5 = sum(ed_1_5))

# combine
df_SLE <- rbind(df_SLE, df_SLE_adm1)

save(df_SLE, file = "CrisisAdjustment/crisis_SLE.rda")


