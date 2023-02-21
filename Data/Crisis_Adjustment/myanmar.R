#' @title myanmar.R
#' @description Prepare crisis_MMR.rda for applyCrisisAdj.R
#' @author Jessica Godwin
#' 
#' Loads:
#' Crisis_Adjustment/Crisis_Under5_deaths_2022.xlsx - IGME excess deaths
#' Crisis_Adjustment/mmr_adm[level]_weights_u[age].rda - admin weights for Myanmar
#' Crisis_Adjustment/gadm41_MMR_shp - shapefiles
#' 

# Setup ####
rm(list = ls())

## Libraries ####
library(tidyverse)
library(rgdal)

## setwd() ####
setwd("U:/UN-Subnational-Estimates/Data/")

# Load Data ####

## UN-IGME national crisis adjustments ####
nat <- readxl::read_xlsx("Crisis_Adjustment/Crisis_Under5_deaths_2022.xlsx")

## Admin weights ####
load("Crisis_Adjustment/adm_weights/mmr_adm1_weights_u1.rda")
load("Crisis_Adjustment/adm_weights/mmr_adm1_weights_u5.rda")
load("Crisis_Adjustment/adm_weights/mmr_adm2_weights_u1.rda")
load("Crisis_Adjustment/adm_weights/mmr_adm2_weights_u5.rda")

## Shapefiles ####
gadm_MMR_2 <- readOGR(
  dsn = "Crisis_Adjustment/gadm41_MMR_shp",
  layer = "gadm41_MMR_2"
)

gadm_MMR_1 <- readOGR(
  dsn = "Crisis_Adjustment/gadm41_MMR_shp",
  layer = "gadm41_MMR_1"
)


# Create df_MMR shell ####
df_MMR <- unique(gadm_MMR_2@data[, c("NAME_2", "NAME_1")]) %>% 
  mutate(years = 2008,
         cyclone = ifelse(NAME_1 %in% c("Ayeyarwady", "Yangon"), 1, 0))
df_MMR$admin2_Internal <- paste0("admin2_", 1:nrow(df_MMR))
df_MMR$admin1_Internal <- paste0("admin1_", match(df_MMR$NAME_1,
                                                  gadm_MMR_1@data$NAME_1))
# Get national crisis death counts ####
nat_MMR <- nat %>%
  filter(Country == "Myanmar") %>%
  mutate(years = floor(Year)) %>% # Year is given in mid-year, convert this
  select(years, nat_ed_0_1 = `Crisis d0`, nat_ed_1_5 = `Crisis d1-4`)

# Convert subnational death proportions to death counts ####

## Make one row per admin area and level var ####
df_MMR <- df_MMR %>%
  left_join(nat_MMR,
            by = c("years")) %>%
  rename("admin1_GADM" = "NAME_1") %>% 
  rename("admin2_GADM" = "NAME_2") %>% 
  pivot_longer(contains("admin"),
               names_sep = "_",
               names_to = c("level", ".value")) %>% 
  unique()

## Join weights and distributed deaths ####
df_MMR <- df_MMR %>% 
  left_join(weight.adm1.u1,
            by = c("years", "Internal" = "region")) %>% 
  left_join(weight.adm1.u5,
            by = c("years", "Internal" = "region"),
            suffix = c("", "_1_5")) %>% 
  left_join(weight.adm2.u1,
            by = c("years", "Internal" = "region"),
            suffix = c("", "_2_1")) %>% 
  left_join(weight.adm2.u5,
            by = c("years", "Internal" = "region"),
            suffix = c("", "_2_5")) %>% 
  rename("proportion_1_1" = "proportion") %>% 
  mutate(across(contains("proportion_"),
                ~ (.x*cyclone)/sum(.x*cyclone, na.rm = TRUE))) %>% 
  mutate(ed_0_1 = ifelse(level == "admin1",
                         nat_ed_0_1 * proportion_1_1 * cyclone,
                         nat_ed_0_1 * proportion_2_1 * cyclone),
         ed_1_5 = ifelse(level == "admin1",
                         nat_ed_1_5 * proportion_1_5 * cyclone,
                         nat_ed_1_5 * proportion_2_5 * cyclone)) %>% 
  mutate(country = "Myanmar") %>%
  rename("gadm" = "GADM") %>% 
  select(country, level, gadm, years, ed_0_1, ed_1_5)

## Check work ####
### Check age totals ####
nat_MMR
df_MMR %>% 
  group_by(years, level) %>% 
  summarise(ed_0_1 = sum(ed_0_1),
            ed_1_5 = sum(ed_1_5))

### Check area totals ####

### Are EDs 0 in admin1/admin2 areas not in Yangon or Ayeyarwardy?
df_MMR %>% 
  group_by(years, level) %>% 
  summarise(nonzero_0_1 = sum(ed_0_1 != 0),
            nonzero_1_5 = sum(ed_1_5 != 0)) %>% 
  ungroup() %>% 
  mutate(n_affected_areas = c(2, 9))

### Do admin2 sums = admin1

df_MMR %>% 
  filter(gadm %in% c("Ayeyarwady", "Yangon"))
df_MMR %>% 
  filter(ed_0_1 > 0 & level == "admin1") %>% 
  mutate(admin1 = ifelse(grepl("Yangon", gadm), "Yangon", "Ayeyarwady")) %>% 
  group_by(years, admin1) %>% 
  summarise(ed_0_1 = sum(ed_0_1),
            ed_1_5 = sum(ed_1_5))

## save df_MMR ####

save(df_MMR,
     file = paste0("Crisis_Adjustment/crisis_MMR.rda"))
