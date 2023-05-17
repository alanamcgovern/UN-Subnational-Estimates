rm(list=ls())

# Country Name & Model Info ####
# Please capitalize the first letter of the country name and replace " "
# in the country name to "_" if there is.
country <- "Rwanda"


## MIGHT NEED TO BE CHANGED depending on what you fit
# specify time model for BB8
time.model <- c('rw2','ar1')[2]
# specify time model for smoothed direct
sd.time.model <- c("rw2", "ar1")[2]
# specify stratification for BB8 model
strata.model <- c("unstrat", "strat")[2]

# specify whether benchmarked or not -- this should be equal to '' unless trying to troubleshoot
bench.model <- ""

# Setup -----------------------------------------------
## Load libraries and info ----------------------------------------------------------

# Libraries
options(gsubfn.engine = "R")
library(data.table)
library(survey)
library(sp)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(raster)
library(maptools)
library(latticeExtra)
library(viridis)
library(xtable)
library(Hmisc)
library(spdep)
library(plotrix)
library(ggridges)
library(rgdal)
library(ggplot2)
library(SUMMER)
library(tidyverse)
library(Rfast)


## Functions ####

logit <- function(p){ log(p/(1-p))}
expit <- function(x){exp(x)/(1 + exp(x))}
# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

## Directories ####
home.dir <- paste(code.path.splitted[1:(length(code.path.splitted) - 2)],
                  collapse = "/")
# data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
data.dir <- paste0("R://Project/STAB/", country)
res.dir <- paste0(home.dir,'/Results/', country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir, '/Info/', info.name, sep = '')) # load the country info

## what is the equivalent of poly.label.adm2 that
## has the column name of admin1 names in the admin2 shapefile
if(exists('poly.label.adm2')){
  adm1_on_adm2 <- paste0(strsplit(poly.label.adm2, "\\$")[[1]][1], "$",
                         strsplit(poly.label.adm1, "\\$")[[1]][2])
  message("If your country does not use GADM shapefiles, ", 
          "you will need to specify this manually.\n")
}
if(country=='Sierra_Leone'){
  adm1_on_adm2 <- paste0('poly.adm2@data$district_2')
}

# Load Data ####
## Admin names  ------------------------------------------------------
setwd(data.dir)

load(paste0(poly.path,'/', country, '_Amat.rda'))
load(paste0(poly.path,'/', country, '_Amat_Names.rda'))

if(country == "Pakistan"){
  load(paste0(poly.path,'/', country, '_Amat_excluding_disputed.rda'))
  load(paste0(poly.path,'/', country, '_Amat_Names_excluding_disputed.rda'))
  
  disputed_areas_gadm <- unique(c(setdiff(admin1.names$GADM,admin1.names_excluding_disputed$GADM),
                                  setdiff(admin2.names$GADM,admin2.names_excluding_disputed$GADM)))
  disputed_areas_internal <- c(setdiff(admin1.names$Internal,admin1.names_excluding_disputed$Internal),
                               setdiff(admin2.names$Internal,admin2.names_excluding_disputed$Internal))
  
  admin1.names <- admin1.names_excluding_disputed
  admin2.names <- admin2.names_excluding_disputed
}
## IGME ------------------------------------------------------

{
  setwd(paste0(home.dir,'/Data/IGME'))
  
  ### U5MR: No Crisis ####
  
  igme.ests.u5.raw <- read.csv('igme2022_u5_nocrisis.csv')
  igme.ests.u5 <- igme.ests.u5.raw[igme.ests.u5.raw$ISO.Code==gadm.abbrev,]
  igme.ests.u5 <- data.frame(t(igme.ests.u5[,10:ncol(igme.ests.u5)]))
  names(igme.ests.u5) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.u5$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.u5),'X')) - 0.5
  igme.ests.u5 <- igme.ests.u5[igme.ests.u5$year %in% 2000:2021,]
  rownames(igme.ests.u5) <- NULL
  igme.ests.u5$OBS_VALUE <- igme.ests.u5$OBS_VALUE/1000
  igme.ests.u5$LOWER_BOUND <- igme.ests.u5$LOWER_BOUND/1000
  igme.ests.u5$UPPER_BOUND <- igme.ests.u5$UPPER_BOUND/1000
  igme.ests.u5.nocrisis <- igme.ests.u5
  
  ## U5MR ####
  igme.ests.u5.raw <- read.csv('igme2022_u5.csv')
  igme.ests.u5 <- igme.ests.u5.raw[igme.ests.u5.raw$ISO.Code==gadm.abbrev,]
  igme.ests.u5 <- data.frame(t(igme.ests.u5[,10:ncol(igme.ests.u5)]))
  names(igme.ests.u5) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.u5$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.u5),'X')) - 0.5
  igme.ests.u5 <- igme.ests.u5[igme.ests.u5$year %in% 2000:2021,]
  rownames(igme.ests.u5) <- NULL
  igme.ests.u5$OBS_VALUE <- igme.ests.u5$OBS_VALUE/1000
  igme.ests.u5$LOWER_BOUND <- igme.ests.u5$LOWER_BOUND/1000
  igme.ests.u5$UPPER_BOUND <- igme.ests.u5$UPPER_BOUND/1000
  
  
  ## NMR: No Crisis ####
  igme.ests.nmr.raw <- read.csv('igme2022_nmr_nocrisis.csv')
  igme.ests.nmr <- igme.ests.nmr.raw[igme.ests.nmr.raw$iso==gadm.abbrev,]
  igme.ests.nmr <- data.frame(t(igme.ests.nmr[,10:ncol(igme.ests.nmr)]))
  names(igme.ests.nmr) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.nmr$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.nmr),'X')) - 0.5
  igme.ests.nmr <- igme.ests.nmr[igme.ests.nmr$year %in% 2000:2021,]
  rownames(igme.ests.nmr) <- NULL
  igme.ests.nmr$OBS_VALUE <- igme.ests.nmr$OBS_VALUE/1000
  igme.ests.nmr$LOWER_BOUND <- igme.ests.nmr$LOWER_BOUND/1000
  igme.ests.nmr$UPPER_BOUND <- igme.ests.nmr$UPPER_BOUND/1000
  igme.ests.nmr.nocrisis <- igme.ests.nmr
  
  ## NMR ####
  igme.ests.nmr.raw <- read.csv('igme2022_nmr.csv')
  igme.ests.nmr <- igme.ests.nmr.raw[igme.ests.nmr.raw$iso==gadm.abbrev,]
  igme.ests.nmr <- data.frame(t(igme.ests.nmr[,10:ncol(igme.ests.nmr)]))
  names(igme.ests.nmr) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.nmr$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.nmr),'X')) - 0.5
  igme.ests.nmr <- igme.ests.nmr[igme.ests.nmr$year %in% 2000:2021,]
  rownames(igme.ests.nmr) <- NULL
  igme.ests.nmr$OBS_VALUE <- igme.ests.nmr$OBS_VALUE/1000
  igme.ests.nmr$LOWER_BOUND <- igme.ests.nmr$LOWER_BOUND/1000
  igme.ests.nmr$UPPER_BOUND <- igme.ests.nmr$UPPER_BOUND/1000
}

## Admin Weights ####
load(paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm1_weights_u5.rda'))
if(exists('poly.layer.adm2')){
  load(paste0(data.dir,'/worldpop/adm2_weights_u1.rda'))
  load(paste0(data.dir,'/worldpop/adm2_weights_u5.rda'))
}

## Model data ####

if(strata.model=='unstrat'){
  load(paste0(data.dir, '/', country, '_cluster_dat.rda'), envir = .GlobalEnv)
}else{
  load(paste0(data.dir, '/', country,
              '_cluster_dat_1frame.rda'), envir = .GlobalEnv)
}

surveys <- unique(mod.dat$survey)
end.year <- max(mod.dat$survey)
end.proj.year <- 2021


plot.years <- 2000:end.proj.year
n_years <- length(plot.years)

if(((end.year-beg.year+1) %% 3) == 0){
  beg.period.years <- seq(beg.year,end.year,3) 
  end.period.years <- beg.period.years + 2 
}else if(((end.year-beg.year+1) %% 3)==1){
  beg.period.years <- c(beg.year,beg.year+2,seq(beg.year+4,end.year,3))
  end.period.years <- c(beg.year+1,beg.year+3,seq(beg.year+6,end.year,3))
}else if(((end.year-beg.year+1) %% 3)==2){
  beg.period.years <- c(beg.year,seq(beg.year+2,end.year,3))
  end.period.years <- c(beg.year+1,seq(beg.year+4,end.year,3))
}

period.years <- paste(beg.period.years, end.period.years, sep = "-")

if(end.year==end.proj.year){
  pane.years <- (end.period.years+beg.period.years)/2
}else{
  beg.proj.years <- seq(end.year+1,2021,3)
  end.proj.years <- beg.proj.years+2
  pane.years <- (c((end.period.years + beg.period.years)/2, (end.proj.years+beg.proj.years)/2))
  pane.years <- pane.years[pane.years<=end.proj.year]
}

## Polygon files ####
setwd(data.dir)
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

if(exists('poly.layer.adm2')){
  poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                       layer = as.character(poly.layer.adm2))} # load the shape file of admin-2 regions

# set coordinate reference system to be equal
if(exists("poly.adm2")){
  proj4string(poly.adm1)  <- proj4string(poly.adm2)
}

if(country=='Malawi'){
  adm1_adm2_link <- read.csv(paste0(poly.path,'/MWI_adm1_to_adm2.csv'))
  polyfile_merge <- merge(poly.adm2@data,adm1_adm2_link,by.x='NAME_1',by.y='Admin.2')
  polyfile_merge$DHSREGEN <- polyfile_merge$Admin.1
  poly.adm2@data <- polyfile_merge[,!names(polyfile_merge) == 'Admin.1']
}

if(country=='Pakistan'){
  poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                       layer = as.character("gadm41_PAK_1_excluding_disputed")) # load the shape file of admin-1 regions
  
  if(exists('poly.layer.adm2')){
    poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                         layer = as.character("gadm41_PAK_2_excluding_disputed"))} # load the shape file of admin-2 regions
  
  # set coordinate reference system to be equal
  if(exists("poly.adm2")){
    proj4string(poly.adm1)  <- proj4string(poly.adm2)
  }
}

# Load Model Results ####

## Admin-1 ####

{
  setwd(res.dir)
  ### BB8 #####
  ## NMR File name
  if(strata.model == "unstrat"){
    nmr.filename <- paste0(country, '_res_adm1_', time.model, 
                           "_", strata.model, "_nmr_allsurveys.rda")
  }else{
    nmr.filename <- paste0(country, '_res_adm1_', time.model, 
                           "_", strata.model, "_nmr.rda")
  }
  strata_str <- ifelse(strata.model == "unstrat", "unstratified", "stratified")
  bench_str <- ifelse(bench.model == "bench", "benchmarked", "unbenchmarked")
  
  if(bench.model == "bench"){  
    message("Please specify bench.model as an empty string",
            " to load unbenchmarked estimates.")
  }else if(nmr.filename %in% list.files("Betabinomial/NMR/")){
    message("Loading Admin-1 ", bench_str, " ",
            strata_str, " BB8 NMR results using all surveys ",
            "from \n", nmr.filename, ".\n" )
  }else if(gsub(paste0(time.model, "_"), "", nmr.filename) %in% 
           list.files("Betabinomial/NMR/")){
    nmr.filename <- gsub(paste0(time.model, "_"), "", nmr.filename)
    message("Loading Admin-1 ", bench_str, " ", strata_str, 
            " BB8 NMR results using all surveys ",
            "from \n", nmr.filename, ".\n" )
  }else if(gsub("_allsurveys", "", nmr.filename) %in% 
           list.files("Betabinomial/NMR/")){
    nmr.filename <- gsub("_allsurveys", "", nmr.filename)
    message("Loading Admin-1 ", bench_str, " ",
            strata_str, " BB8 NMR results using surveys ",
            "from a single frame from \n", nmr.filename, ".\n" )
  }else if(bench.model == "" & (paste0(country, '_res_adm1_',
                                       strata.model, "_nmr.rda") %in% 
                                list.files("Betabinomial/NMR/"))){
    
    nmr.filename <- gsub(paste0(time.model, "_"), "", nmr.filename)
    nmr.filename <- gsub("_allsurveys", "", nmr.filename)
    message("Loading Admin-1", bench_str, " ", strata_str, 
            " BB8 NMR results using surveys ",
            "from a single frame from \n", nmr.filename, ".\n" )
  }else{
    message("The Admin-1 BB8 NMR results specified don't exist.\n")
  }
  
  ## U5MR file name
  if(strata.model == "unstrat"){
    u5.filename <- paste0(country, '_res_adm1_', time.model, 
                          "_", strata.model, "_u5_allsurveys.rda")
  }else{
    u5.filename <- paste0(country, '_res_adm1_', time.model, 
                          "_", strata.model, "_u5.rda")
  }
  strata_str <- ifelse(strata.model == "unstrat", "unstratified", "stratified")
  bench_str <- ifelse(bench.model == "bench", "benchmarked", "unbenchmarked")
  
  if(bench.model == "bench"){  
    message("Please specify bench.model as an empty string",
            " to load unbenchmarked estimates.")
  }else if(u5.filename %in% list.files("Betabinomial/U5MR/")){
    message("Loading Admin-1 ", bench_str, " ",
            strata_str, " BB8 U5MR results using all surveys ",
            "from \n", u5.filename, ".\n" )
  }else if(gsub(paste0(time.model, "_"), "", u5.filename) %in% 
           list.files("Betabinomial/U5MR/")){
    u5.filename <- gsub(paste0(time.model, "_"), "", u5.filename)
    message("Loading Admin-1 ", bench_str, " ", strata_str, 
            " BB8 U5MR results using all surveys ",
            "from \n", u5.filename, ".\n" )
  }else if(gsub("_allsurveys", "", u5.filename) %in% 
           list.files("Betabinomial/U5MR/")){
    u5.filename <- gsub("_allsurveys", "", u5.filename)
    message("Loading Admin-1 ", bench_str, " ",
            strata_str, " BB8 U5MR results using surveys ",
            "from a single frame from \n", u5.filename, ".\n" )
  }else if(bench.model == "" & (paste0(country, '_res_adm1_',
                                       strata.model, "_u5.rda") %in% 
                                list.files("Betabinomial/U5MR/"))){
    
    u5.filename <- gsub(paste0(time.model, "_"), "", u5.filename)
    u5.filename <- gsub("_allsurveys", "", u5.filename)
    message("Loading Admin-1", bench_str, " ", strata_str, 
            " BB8 U5MR results using surveys ",
            "from a single frame from \n", u5.filename, ".\n" )
  }else{
    message("The Admin-1 BB8 U5MR results specified don't exist.\n")
  }
  
  
  if(strata.model == "unstrat"){
    load(file = paste0('Betabinomial/NMR/', nmr.filename))
    load(file = paste0('Betabinomial/U5MR/', u5.filename))
    
    
    if(exists('bb.res.adm1.unstrat.nmr.allsurveys')){
      bb.res.adm1.unstrat.nmr <- bb.res.adm1.unstrat.nmr.allsurveys
    }
    if(exists('bb.res.adm1.unstrat.u5.allsurveys')){
      bb.res.adm1.unstrat.u5 <- bb.res.adm1.unstrat.u5.allsurveys
    }
    admin1.unstrat.nmr.BB8 <- bb.res.adm1.unstrat.nmr
    admin1.unstrat.u5.BB8 <- bb.res.adm1.unstrat.u5
    
    
    
  }else{
    load(file = paste0('Betabinomial/NMR/', nmr.filename))
    load(file = paste0('Betabinomial/U5MR/', u5.filename))
    
    admin1.strat.nmr.BB8 <- bb.res.adm1.strat.nmr
    admin1.strat.u5.BB8 <- bb.res.adm1.strat.u5
  }
}

## Admin-2#####
if(exists('poly.layer.adm2')){
  
  ### BB8 ####
  
  if(strata.model == ""){
    nmr.filename <- paste0(country, '_res_adm2_', time.model, 
                           "_", strata.model, "_nmr_allsurveys.rda")
  }else{
    nmr.filename <- paste0(country, '_res_adm2_', time.model, 
                           "_", strata.model, "_nmr.rda")
  }
  strata_str <- ifelse(strata.model == "unstrat", "unstratified", "stratified")
  bench_str <- ifelse(bench.model == "bench", "benchmarked", "unbenchmarked")
  
  
  if(bench.model == "bench"){  
    message("Please specify bench.model as an empty string",
            " to load unbenchmarked estimates.")
  }else if(nmr.filename %in% list.files("Betabinomial/NMR/")){
    message("Loading Admin-2 ", bench_str, " ",
            strata_str, " BB8 NMR results using all surveys ",
            "from \n", nmr.filename, ".\n" )
  }else if(gsub(paste0(time.model, "_"), "", nmr.filename) %in% 
           list.files("Betabinomial/NMR/")){
    nmr.filename <- gsub(paste0(time.model, "_"), "", nmr.filename)
    message("Loading Admin-2 ", bench_str, " ", strata_str, 
            " BB8 NMR results using all surveys ",
            "from \n", nmr.filename, ".\n" )
  }else if(gsub("_allsurveys", "", nmr.filename) %in% 
           list.files("Betabinomial/NMR/")){
    nmr.filename <- gsub("_allsurveys", "", nmr.filename)
    message("Loading Admin-2 ", bench_str, " ",
            strata_str, " BB8 NMR results using surveys ",
            "from a single frame from \n", nmr.filename, ".\n" )
  }else if(bench.model == "" & (paste0(country, '_res_adm2_',
                                       strata.model, "_nmr.rda") %in% 
                                list.files("Betabinomial/NMR/"))){
    
    nmr.filename <- gsub(paste0(time.model, "_"), "", nmr.filename)
    nmr.filename <- gsub("_allsurveys", "", nmr.filename)
    message("Loading Admin-2", bench_str, " ", strata_str, 
            " BB8 NMR results using surveys ",
            "from a single frame from \n", nmr.filename, ".\n" )
  }else{
    message("The Admin-2 BB8 NMR results specified don't exist.\n")
  }
  
  if(strata.model == ""){
    u5.filename <- paste0(country, '_res_adm2_', time.model, 
                          "_", strata.model, "_u5_allsurveys.rda")
  }else{
    u5.filename <- paste0(country, '_res_adm2_', time.model, 
                          "_", strata.model, "_u5.rda")
  }
  strata_str <- ifelse(strata.model == "unstrat", "unstratified", "stratified")
  bench_str <- ifelse(bench.model == "bench", "benchmarked", "unbenchmarked")
  
  if(u5.filename %in% list.files("Betabinomial/U5MR/")){
    message("Loading Admin-2 ", bench_str, " ",
            strata_str, " BB8 U5MR results using all surveys ",
            "from \n", u5.filename, ".\n" )
  }else if(gsub(paste0(time.model, "_"), "", u5.filename) %in% 
           list.files("Betabinomial/U5MR/")){
    u5.filename <- gsub(paste0(time.model, "_"), "", u5.filename)
    message("Loading Admin-2 ", bench_str, " ", strata_str, 
            " BB8 U5MR results using all surveys ",
            "from \n", u5.filename, ".\n" )
  }else if(gsub("_allsurveys", "", u5.filename) %in% 
           list.files("Betabinomial/U5MR/")){
    u5.filename <- gsub("_allsurveys", "", u5.filename)
    message("Loading Admin-2 ", bench_str, " ",
            strata_str, " BB8 U5MR results using surveys ",
            "from a single frame from \n", u5.filename, ".\n" )
  }else if(bench.model == "bench" &
           (paste0(country, '_res_adm2_',
                   strata.model, "_u5_bench.rda") %in% 
            list.files("Betabinomial/U5MR/"))){
    
    u5.filename <- gsub(paste0(time.model, "_"), "", u5.filename)
    u5.filename <- gsub("_allsurveys", "", u5.filename)
    message("Loading Admin-2", bench_str, " ", strata_str, 
            " BB8 U5MR results using surveys ",
            "from a single frame from \n", u5.filename, ".\n" )
  }else if(bench.model == "" & (paste0(country, '_res_adm2_',
                                       strata.model, "_u5.rda") %in% 
                                list.files("Betabinomial/U5MR/"))){
    
    u5.filename <- gsub(paste0(time.model, "_"), "", u5.filename)
    u5.filename <- gsub("_allsurveys", "", u5.filename)
    message("Loading Admin-2", bench_str, " ", strata_str, 
            " BB8 U5MR results using surveys ",
            "from a single frame from \n", u5.filename, ".\n" )
  }else{
    message("The Admin-2 BB8 U5MR results specified don't exist.\n")
  }
  
  
  if(strata.model == "unstrat"){
    load(file = paste0('Betabinomial/NMR/', nmr.filename))
    load(file = paste0('Betabinomial/U5MR/', u5.filename))
    
    if(exists('bb.res.adm2.unstrat.nmr.allsurveys')){
      bb.res.adm2.unstrat.nmr <- bb.res.adm2.unstrat.nmr.allsurveys
    }
    if(exists('bb.res.adm2.unstrat.u5.allsurveys')){
      bb.res.adm2.unstrat.u5 <- bb.res.adm2.unstrat.u5.allsurveys
    }
    
    admin2.unstrat.nmr.BB8 <- bb.res.adm2.unstrat.nmr
    admin2.unstrat.u5.BB8 <- bb.res.adm2.unstrat.u5
  }else{
    load(file = paste0('Betabinomial/NMR/', nmr.filename))
    load(file = paste0('Betabinomial/U5MR/', u5.filename))
    
    admin2.strat.nmr.BB8 <- bb.res.adm2.strat.nmr
    admin2.strat.u5.BB8 <- bb.res.adm2.strat.u5
  }  
}

# Grab posterior draws ####

## NMR ####

### Admin 1 ####

if(strata.model == "strat"){
  admin1.nmr.draws.list <- lapply(admin1.strat.nmr.BB8$draws.est.overall,
                                  function(draws_list){
                                    data.frame(years = draws_list$years,
                                               region = draws_list$region,
                                               draws = draws_list$draws,
                                               idx = 1:length(draws_list$draws))
                                  })
}else{
  admin1.nmr.draws.list <- lapply(admin1.unstrat.nmr.BB8$draws.est.overall,
                                  function(draws_list){
                                    data.frame(years = draws_list$years,
                                               region = draws_list$region,
                                               draws = draws_list$draws,
                                               idx = 1:length(draws_list$draws))
                                  })
  
}

admin1.nmr.draws <- do.call(rbind.data.frame, admin1.nmr.draws.list) %>%
  left_join(admin1.names,
            by = c("region" = "Internal")) %>% 
  left_join(weight.adm1.u1,
            by = c("region", "years"))




### Admin 2 ####

if(strata.model == "strat"){
  admin2.nmr.draws.list <- lapply(admin2.strat.nmr.BB8$draws.est.overall,
                                  function(draws_list){
                                    data.frame(years = draws_list$years,
                                               region = draws_list$region,
                                               draws = draws_list$draws,
                                               idx = 1:length(draws_list$draws))
                                  })
}else{
  admin2.nmr.draws.list <- lapply(admin2.unstrat.nmr.BB8$draws.est.overall,
                                  function(draws_list){
                                    data.frame(years = draws_list$years,
                                               region = draws_list$region,
                                               draws = draws_list$draws,
                                               idx = 1:length(draws_list$draws))
                                  })
  
}

admin2.nmr.draws <- do.call(rbind.data.frame, admin2.nmr.draws.list) %>%
  left_join(admin2.names,
            by = c("region" = "Internal")) %>% 
  left_join(weight.adm2.u1,
            by = c("region", "years"))

## U5MR ####

### Admin 1 ####

if(strata.model == "strat"){
  admin1.u5.draws.list <- lapply(admin1.strat.u5.BB8$draws.est.overall,
                                 function(draws_list){
                                   data.frame(years = draws_list$years,
                                              region = draws_list$region,
                                              draws = draws_list$draws,
                                              idx = 1:length(draws_list$draws))
                                 })
}else{
  admin1.u5.draws.list <- lapply(admin1.unstrat.u5.BB8$draws.est.overall,
                                 function(draws_list){
                                   data.frame(years = draws_list$years,
                                              region = draws_list$region,
                                              draws = draws_list$draws,
                                              idx = 1:length(draws_list$draws))
                                 })
}

admin1.u5.draws <- do.call(rbind.data.frame, admin1.u5.draws.list) %>%
  left_join(admin1.names,
            by = c("region" = "Internal")) %>% 
  left_join(weight.adm1.u5,
            by = c("region", "years"))




### Admin 2 ####
if(strata.model == "strat"){
  admin2.u5.draws.list <- lapply(admin2.strat.u5.BB8$draws.est.overall,
                                 function(draws_list){
                                   data.frame(years = draws_list$years,
                                              region = draws_list$region,
                                              draws = draws_list$draws,
                                              idx = 1:length(draws_list$draws))
                                 })
}else{
  admin2.u5.draws.list <- lapply(admin2.unstrat.u5.BB8$draws.est.overall,
                                 function(draws_list){
                                   data.frame(years = draws_list$years,
                                              region = draws_list$region,
                                              draws = draws_list$draws,
                                              idx = 1:length(draws_list$draws))
                                 })
}
admin2.u5.draws <- do.call(rbind.data.frame, admin2.u5.draws.list) %>%
  left_join(admin2.names,
            by = c("region" = "Internal")) %>% 
  left_join(weight.adm2.u5,
            by = c("region", "years"))

# Aggregate to posterior national draws ###

## NMR ####
admin1.nmr.draws.agg <- admin1.nmr.draws %>%
  ungroup() %>% 
  group_by(years, idx) %>% 
  dplyr::summarize(draws = sum(proportion*draws),
            proportion = sum(proportion))


admin2.nmr.draws.agg <- admin2.nmr.draws %>% 
  ungroup() %>% 
  group_by(years, idx) %>% 
  dplyr::summarize(draws = sum(proportion*draws),
            proportion = sum(proportion))

## U5MR ####
admin1.u5.draws.agg <- admin1.u5.draws %>% 
  ungroup() %>% 
  group_by(years, idx) %>% 
  dplyr::summarize(draws = sum(proportion*draws),
            proportion = sum(proportion))


admin2.u5.draws.agg <- admin2.u5.draws %>% 
  ungroup() %>% 
  group_by(years, idx) %>% 
  dplyr::summarize(draws = sum(proportion*draws),
            proportion = sum(proportion))

# Get benchmark ratios ####

## NMR ####
admin1.nmr.bench <- admin1.nmr.draws.agg %>% 
  ungroup() %>% 
  group_by(years) %>% 
  dplyr::summarize(median = median(draws)) %>% 
  left_join(igme.ests.nmr.nocrisis,
            by = c("years" = "year")) %>% 
  mutate(ratio = median/OBS_VALUE)

admin2.nmr.bench <- admin2.nmr.draws.agg %>% 
  ungroup() %>% 
  group_by(years) %>% 
  dplyr::summarize(median = median(draws)) %>% 
  left_join(igme.ests.nmr.nocrisis,
            by = c("years" = "year")) %>% 
  mutate(ratio = median/OBS_VALUE)

## U5MR ####

admin1.u5.bench <- admin1.u5.draws.agg %>% 
  ungroup() %>% 
  group_by(years) %>% 
  dplyr::summarize(median = median(draws)) %>% 
  left_join(igme.ests.u5.nocrisis,
            by = c("years" = "year")) %>% 
  mutate(ratio = median/OBS_VALUE)

admin2.u5.bench <- admin2.u5.draws.agg %>% 
  ungroup() %>% 
  group_by(years) %>% 
  dplyr::summarize(median = median(draws)) %>% 
  left_join(igme.ests.u5.nocrisis,
            by = c("years" = "year")) %>% 
  mutate(ratio = median/OBS_VALUE)

# Apply benchmark ####

admin1.nmr.res.bench <- admin1.nmr.draws %>% 
  left_join(admin1.nmr.bench %>% 
              dplyr::select(years, ratio),
            by = c("years" = "years")) %>% 
  mutate(outcome = "nmr", level = "Admin1")

admin2.nmr.res.bench <- admin2.nmr.draws %>% 
  left_join(admin2.nmr.bench %>% 
              dplyr::select(years, ratio),
            by = c("years" = "years")) %>% 
  mutate(outcome = "nmr", level = "Admin2")


admin1.u5.res.bench <- admin1.u5.draws %>% 
  left_join(admin1.nmr.bench %>% 
              dplyr::select(years, ratio),
            by = c("years" = "years")) %>% 
  mutate(outcome = "u5", level = "Admin1")

admin2.u5.res.bench <- admin2.u5.draws %>% 
  left_join(admin2.u5.bench %>% 
              dplyr::select(years, ratio),
            by = c("years" = "years")) %>% 
  mutate(outcome = "u5", level = "Admin2")



all.res.bench <- admin1.nmr.res.bench %>%
  bind_rows(admin1.u5.res.bench,
            admin2.nmr.res.bench,
            admin2.u5.res.bench) %>% 
  group_by(outcome, level, region, years) %>% 
  mutate(draws = draws/ratio) %>% 
  group_by(outcome, level, region, years, GADM) %>% 
  dplyr::summarize(variance = var(draws),
            median = median(draws),
            mean = mean(draws),
            upper = quantile(draws, 0.975),
            lower = quantile(draws, 0.025))



igme_2023 <-  data.frame(outcome = "u5",
                         level = "National",
                         region = "IGME 2023",
                         years = 2000:2021,
                         GADM = country,
                         median = NA,
                         upper = NA, lower = NA)
igme_2023[, c("median", "upper", "lower")] <-
  igme.ests.u5[, c(2, 3, 1)]

plot_tmp <- rbind.data.frame(all.res.bench %>%
                               filter(outcome == "u5" & level == "Admin1") %>% 
                               dplyr::select(-variance, -mean),
                             igme_2023)

cols <- rainbow(nrow(admin1.names))

plot_u5 <- plot_tmp %>% 
  ungroup() %>% 
  mutate(years = as.numeric(as.character(years))) %>% 
  ggplot(aes(x = years, y = median)) +
  geom_line(aes(color = region), size = 1) + 
  scale_colour_manual(name = "Estimate",
                      values = c(cols, "black")) +
  theme_classic() +
  ggtitle(country)


ggsave(filename = "./Figures/IGMECompare_PostHoc.png", plot_u5)

# Overwrite old results object ####

res_names <- names(bb.res.adm1.strat.nmr$overall)

## NMR ####

if(strata.model == "strat"){
  bb.res.adm1.strat.nmr$overall <- all.res.bench %>% 
    filter(outcome == "nmr" & level == "Admin1") %>% 
    left_join(bb.res.adm1.strat.nmr$overall,
              by = c("region", "years" = "years.num"),
              suffix = c("", "_old")) %>% 
    rename("years.num" = "years") %>% 
    rename("years" = "years_old") %>%
    ungroup() %>% 
    dplyr::select(-contains("_new"), -contains("_old"),
           -outcome, -level, -GADM) %>% 
    as.data.frame()
  bb.res.adm1.strat.nmr$overall <- bb.res.adm1.strat.nmr$overall[, res_names]
  
  bb.res.adm2.strat.nmr$overall <- all.res.bench %>% 
    filter(outcome == "nmr" & level == "Admin2") %>% 
    left_join(bb.res.adm2.strat.nmr$overall,
              by = c("region", "years" = "years.num"),
              suffix = c("", "_old")) %>% 
    rename("years.num" = "years") %>% 
    rename("years" = "years_old") %>%
    ungroup() %>% 
    dplyr::select(-contains("_new"), -contains("_old"),
           -outcome, -level, -GADM) %>% 
    as.data.frame()
  bb.res.adm2.strat.nmr$overall <- bb.res.adm2.strat.nmr$overall[, res_names]
}else{
  bb.res.adm1.unstrat.nmr$overall <- all.res.bench %>% 
    filter(outcome == "nmr" & level == "Admin1") %>% 
    left_join(bb.res.adm1.unstrat.nmr$overall,
              by = c("region", "years" = "years.num"),
              suffix = c("", "_old")) %>% 
    rename("years.num" = "years") %>% 
    rename("years" = "years_old") %>%
    ungroup() %>% 
    dplyr::select(-contains("_new"), -contains("_old"),
           -outcome, -level, -GADM) %>% 
    as.data.frame()
  bb.res.adm1.unstrat.nmr$overall <- bb.res.adm1.unstrat.nmr$overall[, res_names]
  
  bb.res.adm2.unstrat.nmr$overall <- all.res.bench %>% 
    filter(outcome == "nmr" & level == "Admin2") %>% 
    left_join(bb.res.adm2.unstrat.nmr$overall,
              by = c("region", "years" = "years.num"),
              suffix = c("", "_old")) %>% 
    rename("years.num" = "years") %>% 
    rename("years" = "years_old") %>%
    ungroup() %>% 
    dplyr::select(-contains("_new"), -contains("_old"),
           -outcome, -level, -GADM) %>% 
    as.data.frame()
  bb.res.adm2.unstrat.nmr$overall <- bb.res.adm2.unstrat.nmr$overall[, res_names]
  
}


## U5MR ####

if(strata.model == "strat"){
  bb.res.adm1.strat.u5$overall <- all.res.bench %>% 
    filter(outcome == "u5" & level == "Admin1") %>% 
    left_join(bb.res.adm1.strat.nmr$overall,
              by = c("region", "years" = "years.num"),
              suffix = c("", "_old")) %>% 
    rename("years.num" = "years") %>% 
    rename("years" = "years_old") %>%
    ungroup() %>% 
    dplyr::select(-contains("_new"), -contains("_old"),
           -outcome, -level, -GADM) %>% 
    as.data.frame()
  bb.res.adm1.strat.u5$overall <- bb.res.adm1.strat.u5$overall[, res_names]
  
  bb.res.adm2.strat.u5$overall <- all.res.bench %>% 
    filter(outcome == "u5" & level == "Admin2") %>% 
    left_join(bb.res.adm2.strat.u5$overall,
              by = c("region", "years" = "years.num"),
              suffix = c("", "_old")) %>% 
    rename("years.num" = "years") %>% 
    rename("years" = "years_old") %>%
    ungroup() %>% 
    dplyr::select(-contains("_new"), -contains("_old"),
           -outcome, -level, -GADM) %>% 
    as.data.frame()
  bb.res.adm2.strat.u5$overall <- bb.res.adm2.strat.u5$overall[, res_names]
}else{
  bb.res.adm1.unstrat.u5$overall <- all.res.bench %>% 
    filter(outcome == "u5" & level == "Admin1") %>% 
    left_join(bb.res.adm1.unstrat.nmr$overall,
              by = c("region", "years" = "years.num"),
              suffix = c("", "_old")) %>% 
    rename("years.num" = "years") %>% 
    rename("years" = "years_old") %>%
    ungroup() %>% 
    dplyr::select(-contains("_new"), -contains("_old"),
           -outcome, -level, -GADM) %>% 
    as.data.frame()
  bb.res.adm1.unstrat.u5$overall <- bb.res.adm1.unstrat.u5$overall[, res_names]
  
  bb.res.adm2.unstrat.u5$overall <- all.res.bench %>% 
    filter(outcome == "u5" & level == "Admin2") %>% 
    left_join(bb.res.adm2.unstrat.u5$overall,
              by = c("region", "years" = "years.num"),
              suffix = c("", "_old")) %>% 
    rename("years.num" = "years") %>% 
    rename("years" = "years_old") %>%
    ungroup() %>% 
    dplyr::select(-contains("_new"), -contains("_old"),
           -outcome, -level, -GADM) %>% 
    as.data.frame()
  bb.res.adm2.unstrat.u5$overall <- bb.res.adm2.unstrat.u5$overall[, res_names]
}

# Save objects ####

setwd(res.dir)

if(strata.model == "strat"){
  bb.res.adm1.strat.nmr.bench <- bb.res.adm1.strat.nmr
  save(bb.res.adm1.strat.nmr.bench, 
       file = paste0('Betabinomial/NMR/', country, '_res_adm1_', time.model, 
              "_", strata.model, "_nmr_bench.rda"))
  
  bb.res.adm2.strat.nmr.bench <- bb.res.adm2.strat.nmr
  save(bb.res.adm2.strat.nmr.bench, 
       file = paste0('Betabinomial/NMR/', country, '_res_adm2_', time.model, 
              "_", strata.model, "_nmr_bench.rda"))
  
  bb.res.adm1.strat.u5.bench <- bb.res.adm1.strat.u5
  save(bb.res.adm1.strat.u5.bench, 
       file = paste0('Betabinomial/U5MR/', country, '_res_adm1_', time.model, 
              "_", strata.model, "_u5_bench.rda"))
  
  bb.res.adm2.strat.u5.bench <- bb.res.adm2.strat.u5
  save(bb.res.adm2.strat.u5.bench, 
       file = paste0('Betabinomial/U5MR/', country, '_res_adm2_', time.model, 
                     "_", strata.model, "_u5_bench.rda"))
}else{
  bb.res.adm1.unstrat.nmr.bench <- bb.res.adm1.unstrat.nmr
  save(bb.res.adm1.unstrat.nmr.bench, 
       paste0('Betabinomial/NMR/', country, '_res_adm1_', time.model, 
              "_", strata.model, "_nmr_bench.rda"))
  bb.res.adm2.unstrat.nmr.bench <- bb.res.adm2.unstrat.nmr
  save(bb.res.adm2.unstrat.nmr.bench, 
       paste0('Betabinomial/NMR/', country, '_res_adm2_', time.model, 
              "_", strata.model, "_nmr_bench.rda"))
  bb.res.adm1.unstrat.u5.bench <- bb.res.adm1.unstrat.u5
  save(bb.res.adm1.unstrat.u5, 
       paste0('Betabinomial/U5MR/', country, '_res_adm1_', time.model, 
              "_", strata.model, "_u5_bench.rda"))
  
  bb.res.adm2.unstrat.u5.bench <- bb.res.adm2.unstrat.u5
  save(bb.res.adm2.unstrat.u5, 
       paste0('Betabinomial/U5MR/', country, '_res_adm2_', time.model, 
              "_", strata.model, "_u5_bench.rda"))
  
}
