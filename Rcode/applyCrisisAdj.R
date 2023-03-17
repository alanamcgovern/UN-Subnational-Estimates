
# Take as input subnational excess deaths, population counts, and
# benchmarked u5mr without crisis adjustment, and save final
# crisis-adjusted qx

rm(list = ls())
# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Guinea'
# Specify straification of final U5MR model (which was benchmarked)
mod_label <- c('strat_u5_bench','unstrat_u5_allsurveys_bench')[1]

# Load libraries and info ----------------------------------------------------------
library(tidyverse)

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

# Load data and results ------------------------------------------------------------------

# excess deaths for admin1 and/or admin2
if (country != "Haiti") {
  load(file = paste0(home.dir,"/Data/Crisis_Adjustment/crisis_", gadm.abbrev, ".rda"))
  deaths <- get(paste0("df_", gadm.abbrev))
}

# population weights
load(paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm1_weights_u5.rda'))
load(paste0(data.dir,'/worldpop/adm2_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm2_weights_u5.rda'))

# final admin1 and admin2 benchmarked u5mr without crisis adjustment
load(paste0(res.dir,"/Betabinomial/U5MR/", country, "_res_adm1_",mod_label,".rda"))
load(paste0(res.dir,"/Betabinomial/U5MR/", country, "_res_adm2_", mod_label, ".rda"))

# simplify names of objects above
res_adm1_u5 <- eval(str2lang(paste0('bb.res.adm1.',str_replace_all(mod_label,'_','.'),'$overall')))
res_adm2_u5 <- eval(str2lang(paste0('bb.res.adm2.',str_replace_all(mod_label,'_','.'),'$overall')))

# load admin1.names and admin2.names (map from area id to name)
load(paste0(data.dir,'/',poly.path,'/', country, '_Amat_Names.rda'))

# UN-IGME national crisis adjustments
# DO not use for deaths ONLY pop
igme <- readxl::read_xlsx(paste0(home.dir,
                                 "/Data/Crisis_Adjustment/",
                                 "Crisis_Under5_deaths_2022.xlsx"))

## UN-IGME national 2020 crisis adjustments
igme_2020 <- readxl::read_xlsx(paste0(home.dir,
                                      "/Data/Crisis_Adjustment/",
                                      "Crisis_Under5_deaths2020.xlsx"))
## 2022: Haiti Myanmar Crisis Adj ####

igme_2022 <- readxl::read_xlsx(paste0(home.dir,
                                      "/Data/Crisis_Adjustment/",
                                      "Haiti and Mynamr Crisis ",
                                      "Under 5.xlsx"))

igme_updated <- igme_2022 %>% 
  dplyr::select(LocName, Country.Code, Year, EventType,
         dth_both_infant, dth_both_x1to4) %>% 
  rename("Country" = "LocName", "ISO3Code" = "Country.Code",
         "Event" = "EventType", "Crisis d0" = "dth_both_infant",
         "Crisis d1-4" = "dth_both_x1to4") %>% 
  mutate(`Crisis d0-5` = `Crisis d0` + `Crisis d1-4`,
         ID = ISO3Code) %>% 
  relocate("ID", .after = "Year") %>% 
  relocate("Crisis d0-5", .after = "Event") %>% 
  bind_rows(igme_2020 %>% 
              filter(Country %in% c("Guinea", "Liberia",
                                    "Sierra Leone")))

# Prep population from UN-IGME pop and weights ----------------------------

# national pop
pop <- igme[igme$ISO3Code == gadm.abbrev, c("Country", "Year", "Pop 0", "Pop 1-4")]
names(pop) <- c("country", "years", "nat_pop_0_1", "nat_pop_1_5")
pop$years <- as.integer(floor(pop$years))

# Admin 1: merge on weights and format
pop_adm1 <- merge(weight.adm1.u1, weight.adm1.u5, by = c("region", "years"))
names(pop_adm1) <- c("region", "years", "prop_0_1", "prop_1_5")
pop_adm1$level <- "admin1"
pop_adm1 <- merge(pop_adm1, admin1.names, by.x = "region", by.y = "Internal")
names(pop_adm1)[names(pop_adm1) == "GADM"] <- "gadm"
pop_adm1 <- merge(pop_adm1, pop, by = "years")

# Admin 2: merge on weights and format
pop_adm2 <- merge(weight.adm2.u1, weight.adm2.u5, by = c("region", "years"))
names(pop_adm2) <- c("region", "years", "prop_0_1", "prop_1_5")
pop_adm2$level <- "admin2"
pop_adm2 <- merge(pop_adm2, admin2.names, by.x = "region", by.y = "Internal")
names(pop_adm2)[names(pop_adm2) == "GADM"] <- "gadm"
pop_adm2 <- merge(pop_adm2, pop, by = "years")

# combine and compute area-level population from weights and national pop
pop <- rbind(pop_adm1, pop_adm2)
pop$pop_0_1 <- pop$prop_0_1 * pop$nat_pop_0_1
pop$pop_1_5 <- pop$prop_1_5 * pop$nat_pop_1_5

# final columns: country, level (i.e. admin1, admin2), gadm (area name),
# region (area code), years, pop_0_1, pop_1_5
pop <- pop[, c("country", "level", "gadm", "region", "years", "pop_0_1", "pop_1_5")]


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
  
  #load data to get admin2 labels
  load(paste0(data.dir,'/',country,'_cluster_dat.rda'),envir = .GlobalEnv)
  
  deaths_adm2 <- mod.dat %>% dplyr::select(c(admin2.char,admin1.name,admin2.name)) %>% 
    rename(gadm=admin2.name,region=admin2.char) %>%unique()
  deaths_adm2$country <- 'Liberia'
  deaths_adm2$level <- 'admin2'
  
  # merge on admin1 deaths
  deaths_adm2 <- merge(deaths_adm2, deaths, by.x = "admin1.name", by.y = "gadm", all = T)
  
  # merge on population and split by proportion
  pop_adm2 <- pop %>% filter(level == "admin2")
  deaths_adm2 <- merge(deaths_adm2, pop_adm2, by = c("region",'years'))
  deaths_adm2 <- deaths_adm2 %>%
    group_by(admin1.name) %>%
    dplyr::mutate(prop_pop_0_1 = pop_0_1 / sum(pop_0_1),
              prop_pop_1_5 = pop_1_5 / sum(pop_1_5)) %>%
    dplyr::mutate(ed_0_1= ed_0_1 * prop_pop_0_1,
           ed_1_5 = ed_1_5 * prop_pop_1_5) %>% ungroup()
  deaths_adm2 <- deaths_adm2 %>%
    dplyr::select(country.x, level.x, gadm.x, region, years, ed_0_1, ed_1_5) %>% rename(country=country.x,level=level.x,gadm=gadm.x)
  
  # combine
  deaths <- merge(deaths,admin1.names,by.x='gadm',by.y='GADM') %>% select(-c(Internal))
  deaths <- rbind(deaths, deaths_adm2)
}

# manually make deaths object, assign all deaths to Ouest admin1 and
# split to admin2 by population
if (country == "Haiti") {
  
  nat_ed_0_1 <- igme_updated[igme_updated$Country == "Haiti",
                             "Crisis d0"]
  nat_ed_1_5 <- igme_updated[igme_updated$Country == "Haiti",
                             "Crisis d1-4"]
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
  gadm <- rgdal::readOGR(dsn = paste0(data.dir,'/',poly.path),encoding = "UTF-8", use_iconv = TRUE,
                         layer = as.character(poly.layer.adm2))@data[, c("NAME_1", "NAME_2")] # load the shape file of admin-1 regions
  deaths_adm2 <- merge(deaths_adm2, gadm, by.x = "gadm", by.y = "NAME_2")
  deaths_adm2 <- deaths_adm2 %>%
    mutate(ed_0_1 = ifelse(NAME_1 == "Ouest", nat_ed_0_1, 0),
           ed_1_5 = ifelse(NAME_1 == "Ouest", nat_ed_1_5, 0))
  pop_adm2 <- pop %>% filter(level == "admin2")
  deaths_adm2 <- merge(deaths_adm2, pop_adm2, by = "gadm")
  deaths_adm2 <- deaths_adm2 %>%
    group_by(NAME_1) %>%
    summarise(prop_pop_0_1 = pop_0_1 / sum(pop_0_1),
              prop_pop_1_5 = pop_1_5 / sum(pop_1_5),
              ed_0_1,
              ed_1_5) %>%
    mutate(ed_0_1 = ed_0_1 * prop_pop_0_1,
           ed_1_5 = ed_1_5 * prop_pop_1_5)
  deaths_adm2 <- deaths_adm2 %>%
    dplyr::select(country, level, gadm, years, ed_0_1, ed_1_5)
  
  # combine
  deaths <- rbind(deaths_adm1, deaths_adm2)
  
}


# Main code ---------------------------------------------------------------

# 1. merge all components together
# 2. crisis deaths and pop to crisis qx
# 3. add on to non-crisis results to get final u5mr

# admin1 u5mr
deaths_adm1 <- deaths %>%
  filter(level == "admin1") %>%
  left_join(igme_updated %>% 
            mutate(Year = floor(Year)) %>%
              dplyr::select(Country, Year, contains("Crisis ")),
          by = c("country" = "Country",
                 "years" = "Year")) %>% 
  rename("ed_0_1_orig" = "ed_0_1", "ed_1_5_orig" = "ed_1_5") %>%
  group_by(years) %>% 
  mutate(ed_0_1 = `Crisis d0`*(ed_0_1_orig/sum(ed_0_1_orig)),
         ed_1_5 = `Crisis d1-4`*(ed_1_5_orig/sum(ed_1_5_orig)))
pop_adm1 <- pop %>% filter(level == "admin1")
df <- merge(deaths_adm1, pop_adm1, by = c("gadm", "years"))
df <- get_ed_5q0(df) # convert deaths to qx
df <- merge(df, admin1.names, by.x = "region", by.y = "Internal", all=T)
if (nrow(df[is.na(df$GADM) | is.na(df$gadm),]) > 0) {
  stop("Incomplete merge of Admin 1 location information.")
}
df <- df %>% dplyr::select(region, years, ed_5q0)
res_adm1_u5_crisis <- merge(res_adm1_u5, df, by = c("region", "years"), all=T)
res_adm1_u5_crisis <- res_adm1_u5_crisis %>%
  mutate(ed_5q0 = ifelse(is.na(ed_5q0), 0, ed_5q0)) %>%
  mutate(median = median + ed_5q0, # final qx = non-crisis qx + crisis qx
         lower = lower + ed_5q0,
         upper = upper + ed_5q0) %>%
  dplyr::select(c(region,years,time,area,median,upper,lower,is.yearly))
save(res_adm1_u5_crisis, file=paste0(res.dir,"/Betabinomial/U5MR/", country,
                                "_res_adm1_", mod_label,"_crisis.rda"))

# admin2 u5mr
deaths_adm2 <- deaths %>% 
  filter(level == "admin2") %>%
  left_join(igme_updated %>% 
              mutate(Year = floor(Year)) %>%
              dplyr::select(Country, Year, contains("Crisis ")),
            by = c("country" = "Country",
                   "years" = "Year")) %>% 
  rename("ed_0_1_orig" = "ed_0_1", "ed_1_5_orig" = "ed_1_5") %>%
  group_by(years) %>% 
  mutate(ed_0_1 = `Crisis d0`*(ed_0_1_orig/sum(ed_0_1_orig)),
         ed_1_5 = `Crisis d1-4`*(ed_1_5_orig/sum(ed_1_5_orig)))
pop_adm2 <- pop %>% filter(level == "admin2")
df <- merge(deaths_adm2, pop_adm2, by = c("gadm", "years"))
df <- get_ed_5q0(df) # convert deaths to qx
df <- df %>% dplyr::select(region, years, ed_5q0)
res_adm2_u5_crisis <- merge(res_adm2_u5, df, by = c("region", "years"), all=T)
res_adm2_u5_crisis <- res_adm2_u5_crisis %>%
  mutate(ed_5q0 = ifelse(is.na(ed_5q0), 0, ed_5q0)) %>%
  mutate(median = median + ed_5q0, # final qx = non-crisis qx + crisis qx
         lower = lower + ed_5q0,
         upper = upper + ed_5q0) %>%
  dplyr::select(c(region,years,time,area,median,upper,lower,is.yearly))
save(res_adm2_u5_crisis,  file=paste0(res.dir,"/Betabinomial/U5MR/", country,
                                      "_res_adm2_", mod_label,"_crisis.rda"))



