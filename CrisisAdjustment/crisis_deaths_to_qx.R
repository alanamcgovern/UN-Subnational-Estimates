
# Take as input subnational excess deaths, population counts, and
# benchmarked nmr and u5mr without crisis adjustment, and save final
# crisis-adjusted qx

country <- "Guinea" # change this to run for another country

# setup -------------------------------------------------------------------

library(tidyverse)

country_code <- switch(country,
  "Guinea" = "GIN",
  "Liberia" = "LBR",
  "Sierra Leone" = "SLE",
  "Myanmar" = "MMR",
  "Haiti" = "HTI"
)

# inputs ------------------------------------------------------------------

# excess deaths for admin1 and/or admin2
if (country != "Haiti") {
  load(file = paste0("CrisisAdjustment/crisis_", country_code, ".rda"))
  deaths <- get(paste0("df_", country_code))
}

# population
# TODO: create these files, should have columns:
# country, level (i.e. admin1, admin2), gadm (area name), years, pop_0_1, pop_1_5
load(file = paste0("CrisisAdjustment/pop_", country_code, ".rda"))
pop <- get(paste0("pop_", country_code))

# final admin1 and admin2 benchmarked nmr and u5mr without crisis adjustment
# TODO: different file names?
load(paste0("Results/Betabinomial/U5MR/", country, "_res_adm1_unstrat_u5_allsurveys.rda"))
load(paste0("Results/Betabinomial/NMR/", country, "_res_adm1_unstrat_nmr_allsurveys.rda"))
load(paste0("Results/Betabinomial/U5MR/", country, "_res_adm2_unstrat_u5_allsurveys.rda"))
load(paste0("Results/Betabinomial/NMR/", country, "_res_adm2_unstrat_u5_allsurveys.rda"))

# TODO: fill in "xxx" with names of objects loaded in above
res_adm1_u5 <- xxx$overall
res_adm1_nmr <- xxx$overall
res_adm2_u5 <- xxx$overall
res_adm2_nmr <- xxx$overall

# load admin1.names and admin2.names (map from area id to name)
load(paste0("Data/", country, "/shapeFiles/gadm41_", country_code,
            "_shp/", country, "_Amat_Names.rda"))


# utilities ----------------------------------------------------------------

# Function for crisis-specific 5q0  from u1 and 1to4 deaths and population
# Input: df [data.frame; columns listed below]
# - ed_0_1: excess deaths age 0 years
# - ed_1_5: excess deaths age 1-4 years
# - pop_0_1: population age 0 years
# - pop_1_5: population age 1-4 years
get_ed_5q0 <- function (df) {
  df <- df %>%
    mutate(ed_1m0 = ed_0_1 / pop_0_1,
           ed_4m1 = ed_1_5 / pop_1_5) %>%
    mutate(ed_1q0 = ed_1m0 / (1 + (1 - (0.3)) * ed_1m0),
           ed_4q1 = 4 * ed_4m1 / (1 + (4 - 4 * (0.4)) * ed_4m1)) %>%
    mutate(ed_5q0 = 1 - (1 - ed_1q0) * (1 - ed_4q1))
  return(df)
}


# Special cases -----------------------------------------------------------

# Liberia only has admin1 ebola deaths; split to admin2 by pop
if (country == "Liberia") {
  
  # create starting point
  deaths_adm2 <- data.frame(
    country = "Liberia", level = "admin2",
    gadm = admin2.names$GADM
  )
  gadm <- rgdal::readOGR(
    dsn = "CrisisAdjustment/gadm41_LBR_shp",
    layer = "gadm41_LBR_2"
  )@data[, c("NAME_1", "NAME_2")]
  deaths_adm2 <- merge(deaths_adm2, gadm, by.x = "gadm", by.y = "NAME_2")
  
  # merge on admin1 deaths
  deaths_adm2 <- merge(deaths_adm2, deaths, by.x = "NAME_1", by.y = "gadm", all = T)
  
  # merge on population and split by proportion
  pop_adm2 <- pop %>% filter(level == "admin2")
  deaths_adm2 <- merge(deaths_adm2, pop_adm2, by = "gadm")
  deaths_adm2 <- deaths_adm2 %>%
    group_by(NAME_1) %>%
    summarise(prop_pop_0_1 = pop_0_1 / sum(pop_0_1),
              prop_pop_1_5 = pop_1_5 / sum(pop_1_5)) %>%
    mutate(ed_0_1 = ed_0_1 * prop_pop_0_1,
           ed_1_5 = ed_1_5 * prop_pop_1_5)
  deaths_adm2 <- deaths_adm2 %>%
    select(country, level, gadm, years, ed_0_1, ed_1_5)
  
  # combine
  deaths <- rbind(deaths, deaths_adm2)
}

# manually make deaths object, assign all deaths to Ouest admin1 and
# split to admin2 by population
if (country == "Haiti") {
  
  nat_ed_0_1 <- 4499 # from UN-IGME
  nat_ed_1_5 <- 59498 # from UN-IGME
  
  # admin1
  deaths_adm1 <- data.frame(
    country = "Haiti", level = "admin1",
    gadm = admin1.names$GADM, years = 2010
  )
  deaths_adm1 <- deaths_adm1 %>%
    mutate(ed_0_1 = ifelse(gadm == "Ouest", nat_ed_0_1, 0),
           ed_1_5 = ifelse(gadm == "Ouest", nat_ed_1_5, 0))
  
  # admin2
  deaths_adm2 <- data.frame(
    country = "Haiti", level = "admin2",
    gadm = admin2.names$GADM
  )
  gadm <- rgdal::readOGR(
    dsn = "CrisisAdjustment/gadm41_HTI_shp",
    layer = "gadm41_HTI_2"
  )@data[, c("NAME_1", "NAME_2")]
  deaths_adm2 <- merge(deaths_adm2, gadm, by.x = "gadm", by.y = "NAME_2")
  deaths_adm2 <- deaths_adm2 %>%
    mutate(ed_0_1 = ifelse(NAME_1 == "Ouest", nat_ed_0_1, 0),
           ed_1_5 = ifelse(NAME_1 == "Ouest", nat_ed_1_5, 0))
  pop_adm2 <- pop %>% filter(level == "admin2")
  deaths_adm2 <- merge(deaths_adm2, pop_adm2, by = "gadm")
  deaths_adm2 <- deaths_adm2 %>%
    group_by(NAME_1) %>%
    summarise(prop_pop_0_1 = pop_0_1 / sum(pop_0_1),
              prop_pop_1_5 = pop_1_5 / sum(pop_1_5)) %>%
    mutate(ed_0_1 = ed_0_1 * prop_pop_0_1,
           ed_1_5 = ed_1_5 * prop_pop_1_5)
  deaths_adm2 <- deaths_adm2 %>%
    select(country, level, gadm, years, ed_0_1, ed_1_5)
  
  # combine
  deaths <- rbind(deaths_adm1, deaths_adm2)
  
}

# Main code ---------------------------------------------------------------

# 1. merge all components together
# 2. crisis deaths and pop to crisis qx
# 3. add on to non-crisis results to get final u5mr

# admin1 u5mr
deaths_adm1 <- deaths %>% filter(level == "admin1")
pop_adm1 <- pop %>% filter(level == "admin1")
df <- merge(deaths_adm1, pop_adm1, by = c("gadm", "years"))
df <- get_ed_5q0(df) # convert deaths to qx
df <- merge(df, admin1.names, by.x = "gadm", by.y = "GADM", all=T)
if (nrow(df[is.na(df$GADM) | is.na(df$gadm),]) > 0) {
  stop("Incomplete merge of Admin 1 location information.")
}
df <- df %>% select(region = Internal, years, ed_5q0)
res_adm1_u5 <- merge(res_adm1_u5, df, by = c("region", "years")) # TODO: right now this will only include years w/ crisis adjustments
res_adm1_u5 %>%
  mutate(median = median + ed_5q0, # final qx = non-crisis qx + crisis qx
         lower = lower + ed_5q0,
         upper = upper + ed_5q0,
         variance = NULL,
         mean = NULL) # TODO: do we need variance and mean?
# TODO: just save this file? Any additional formatting? filename and location?

# admin2 u5mr
deaths_adm2 <- deaths %>% filter(level == "admin2")
pop_adm2 <- pop %>% filter(level == "admin2")
df <- merge(deaths_adm2, pop_adm2, by = c("gadm", "years"))
df <- get_ed_5q0(df) # convert deaths to qx
df <- merge(df, admin2.names, by.x = "gadm", by.y = "GADM", all=T)
if (nrow(df[is.na(df$GADM) | is.na(df$gadm),]) > 0) {
  stop("Incomplete merge of Admin 2 location information.")
}
df <- df %>% select(region = Internal, years, ed_5q0)
res_adm2_u5 <- merge(res_adm2_u5, df, by = c("region", "years"))
res_adm2_u5 %>%
  mutate(median = median + ed_5q0, # final qx = non-crisis qx + crisis qx
         lower = lower + ed_5q0,
         upper = upper + ed_5q0,
         variance = NULL,
         mean = NULL) # TODO: do we need variance and mean?
# TODO: just save this file? Any additional formatting? filename and location?

# TODO: for ebola we are not adjusting nmr. Still re-save?
# TODO: should we adjust nmr for other crises?

