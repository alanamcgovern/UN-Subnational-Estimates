# install.packages("lintr")
# library(lintr)
# lint(filename = 
# "U://UN-Subnational-Estimates/Rcode/Archived Plots/CountrySummary_Plot.R")
rm(list=ls())

# Country Name & Model Info ####
# Please capitalize the first letter of the country name and replace " "
# in the country name to "_" if there is.
country <- "Guinea"

## MIGHT NEED TO BE CHANGED depending on what you fit
# specify time model for BB8
time.model <- c('rw2','ar1')[2]
# specify time model for smoothed direct
sd.time.model <- c("rw2", "ar1")[2]
# specify stratification for BB8 model

# specify whether benchmarked or not
strata.model <- c("unstrat", "strat")[2]

# specify whether benchmarked or not -- this should be equal to 'bench' unless trying to troubleshoot
bench.model <- c("", "bench")[2]

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
library(rasterVis)
library(plotrix)
library(ggridges)
library(rgdal)
library(ggplot2)
library(SUMMER)
library(tidyverse)
library(Rfast)


# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

## Directories ####
home.dir <- paste(code.path.splitted[1:(length(code.path.splitted) - 2)],
                  collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data

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

if(!dir.exists(paste0(res.dir,  '/Figures/Summary'))){
  dir.create(paste0(res.dir, '/Figures/Summary'))
}
if(!dir.exists(paste0(res.dir,  '/Figures/Summary/U5MR'))){
  dir.create(paste0(res.dir,  '/Figures/Summary/U5MR'))
}
if(!dir.exists(paste0(res.dir,'/Figures/Summary/NMR'))){
  dir.create(paste0(res.dir, '/Figures/Summary/NMR'))
}

# Load helper functions ####
setwd(home.dir)
source("Rcode/Report_Plot_00.R")

# Load Data ####
## Load admin names  ------------------------------------------------------
setwd(data.dir)

load(paste0(poly.path,'/', country, '_Amat.rda'))
load(paste0(poly.path,'/', country, '_Amat_Names.rda'))


## Load IGME estimates ------------------------------------------------------

{
  setwd(paste0(home.dir,'/Data/IGME'))
  
  ## U5MR
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
  
  
  ## NMR
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

## load admin1 and admin2 weights ####
load(paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm1_weights_u5.rda'))
if(exists('poly.layer.adm2')){
  load(paste0(data.dir,'/worldpop/adm2_weights_u1.rda'))
  load(paste0(data.dir,'/worldpop/adm2_weights_u5.rda'))
}

## Load model data ####

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

beg.proj.years <- seq(end.year+1,2021,3)
end.proj.years <- beg.proj.years+2
pane.years <- (c((end.period.years + beg.period.years)/2, (end.proj.years+beg.proj.years)/2))
pane.years <- pane.years[pane.years<=end.proj.year]
est.period.idx <- 1:length(beg.period.years)
pred.period.idx <- (length(beg.period.years)+1):(length(beg.period.years)+length(beg.proj.years))

## Load Polygon files ####
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

if(country=='Uganda'){
  poly.adm1.poly <- SpatialPolygons(poly.adm1@polygons)
  poly.adm1 <- unionSpatialPolygons(poly.adm1.poly,
                                    IDs = match(poly.adm1@data$ADM1_EN,
                                                unique(poly.adm1@data$ADM1_EN)))
  proj4string(poly.adm1) <- proj4string(poly.adm2)
  merge.dat <- poly.adm2@data %>% group_by(ADM1_EN) %>% summarise(n = n(), 
                                                                  ADM1_PCODE = unique(ADM1_PCODE))
  poly.adm1 <- SpatialPolygonsDataFrame(poly.adm1, merge.dat)
  
}


# Load Model Results ####
## National ####
{
  setwd(res.dir)
  
  ### Direct ####
  
  nmr.filename <- paste0(country, '_direct_natl_yearly_nmr.rda')
  
  if(nmr.filename %in% list.files("Direct/NMR/")){
    message("Loading natl NMR results from \n", nmr.filename, ".\n" )
  }else{
    warning("The national Direct NMR results specified don't exist.\n")
  }
  
  
  u5.filename <- paste0(country, '_direct_natl_yearly_u5.rda')
  
  if(u5.filename %in% list.files("Direct/U5MR/")){
    message("Loading natl U5MR results from \n", u5.filename, ".\n" )
    
  }else{
    warning("The national BB8 U5MR results specified don't exist.\n")
  }
  
  load(file = paste0("Direct/NMR/",  nmr.filename))
  load(file = paste0("Direct/U5MR/",  u5.filename))
  natl.dir.est.nmr <- direct.natl.yearly.nmr[direct.natl.yearly.nmr$years %in%
                                               beg.year:end.proj.year, "mean"]
  natl.dir.lower.nmr <- direct.natl.yearly.nmr[direct.natl.yearly.nmr$years %in% 
                                                 beg.year:end.proj.year, "lower"]
  natl.dir.upper.nmr <- direct.natl.yearly.nmr[direct.natl.yearly.nmr$years %in% 
                                                 beg.year:end.proj.year, "upper"]
  natl.dir.year.nmr <- direct.natl.yearly.nmr[direct.natl.yearly.nmr$years %in% 
                                                beg.year:end.proj.year, "years"]
  natl.dir.svy.nmr <- direct.natl.yearly.nmr[direct.natl.yearly.nmr$years %in%
                                               beg.year:end.proj.year, "surveyYears"]
  natl.dir.est.u5 <- direct.natl.yearly.u5[direct.natl.yearly.u5$years %in% 
                                             beg.year:end.proj.year, "mean"]
  natl.dir.lower.u5 <- direct.natl.yearly.u5[direct.natl.yearly.u5$years %in% 
                                               beg.year:end.proj.year, "lower"]
  natl.dir.upper.u5 <- direct.natl.yearly.u5[direct.natl.yearly.u5$years %in% 
                                               beg.year:end.proj.year, "upper"]
  natl.dir.year.u5 <- direct.natl.yearly.u5[direct.natl.yearly.u5$years %in%
                                              beg.year:end.proj.year, "years"]
  natl.dir.svy.u5 <- direct.natl.yearly.u5[direct.natl.yearly.u5$years %in%
                                             beg.year:end.proj.year, "surveyYears"]
  natl.dir.frame <- data.frame()
  natl.dir.frame <- data.frame(lower_nmr=natl.dir.lower.nmr, 
                               median_nmr=natl.dir.est.nmr,
                               upper_nmr=natl.dir.upper.nmr, 
                               lower_u5=natl.dir.lower.u5,
                               median_u5=natl.dir.est.u5, 
                               upper_u5=natl.dir.upper.u5, 
                               method='natl.dir.yearly',
                               years=natl.dir.year.u5,
                               surveyYears = natl.dir.svy.u5)
  
  ### SD yearly ####
  
  ## is time model in file string?
  nmr.filename <- paste0(country, '_res_natl_', sd.time.model,
                         '_yearly_nmr_SmoothedDirect.rda')
  
  if(nmr.filename %in% list.files("Direct/NMR/")){
    message("Loading natl NMR results from \n", nmr.filename, ".\n" )
  }else if(gsub(paste0(sd.time.model, "_"), "", nmr.filename) %in% 
           list.files("Direct/NMR/")){
    nmr.filename <- gsub(paste0(sd.time.model, "_"), "", nmr.filename)
    message("Loading natl NMR results from \n", nmr.filename, ".\n" )
  }else{
    message("The national Smoothed Direct MR results specified don't exist.\n")
  }
  
  
  u5.filename <- paste0(country, '_res_natl_', time.model,
                        '_yearly_u5_SmoothedDirect.rda')
  
  if(u5.filename %in% list.files("Direct/U5MR/")){
    message("Loading natl U5MR results from \n", u5.filename, ".\n" )
    
  }else if(gsub(paste0(sd.time.model, "_"), "", u5.filename) %in% 
           list.files("Direct/U5MR/")){
    u5.filename <- gsub(paste0(sd.time.model, "_"), "", u5.filename)
    message("Loading natl U5MR results from \n", u5.filename, ".\n" )
  }else{
    message("The national BB8 U5MR results specified don't exist.\n")
  }
  
  load(file = paste0("Direct/NMR/",  nmr.filename))
  load(file = paste0("Direct/U5MR/",  u5.filename))
  natl.sd.est.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in%
                                           beg.year:end.proj.year, "median"]
  natl.sd.lower.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in% 
                                             beg.year:end.proj.year, "lower"]
  natl.sd.upper.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in% 
                                             beg.year:end.proj.year, "upper"]
  natl.sd.year.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in% 
                                            beg.year:end.proj.year, "years"]
  
  natl.sd.est.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% 
                                         beg.year:end.proj.year, "median"]
  natl.sd.lower.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% 
                                           beg.year:end.proj.year, "lower"]
  natl.sd.upper.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% 
                                           beg.year:end.proj.year, "upper"]
  natl.sd.year.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in%
                                          beg.year:end.proj.year, "years"]
  
  natl.sd.frame<-data.frame()
  natl.sd.frame<-data.frame(lower_nmr=natl.sd.lower.nmr, median_nmr=natl.sd.est.nmr,upper_nmr=natl.sd.upper.nmr, 
                            lower_u5=natl.sd.lower.u5, median_u5=natl.sd.est.u5, upper_u5=natl.sd.upper.u5, 
                            method='natl.sd.yearly', years=natl.sd.year.u5)
  
}

{
  #### BB8 ####
  nmr.filename <- paste0(country, '_res_natl_', time.model, 
                         "_", strata.model, "_nmr_allsurveys.rda")
  strata_str <- ifelse(strata.model == "unstrat", "unstratified", "stratified")
  
  if(nmr.filename %in% list.files("Betabinomial/NMR/")){
    message("Loading natl ", strata_str, " BB8 NMR results using all surveys ",
            "from \n", nmr.filename, ".\n" )
  }else if(gsub(paste0(time.model, "_"), "", nmr.filename) %in% 
           list.files("Betabinomial/NMR/")){
    nmr.filename <- gsub(paste0(time.model, "_"), "", nmr.filename)
    message("Loading natl ", strata_str, " BB8 NMR results using all surveys ",
            "from \n", nmr.filename, ".\n" )
  }else if(gsub("_allsurveys", "", nmr.filename) %in% 
           list.files("Betabinomial/NMR/")){
    nmr.filename <- gsub("_allsurveys", "", nmr.filename)
    message("Loading natl ", strata_str, " BB8 NMR results using surveys ",
            "from a single frame from \n", nmr.filename, ".\n" )
  }else if(paste0(country, '_res_natl_',
                  strata.model, "_nmr.rda") %in% 
           list.files("Betabinomial/NMR/")){
    
    nmr.filename <- gsub(paste0(time.model, "_"), "", nmr.filename)
    nmr.filename <- gsub("_allsurveys", "", nmr.filename)
    message("Loading natl ", strata_str, " BB8 NMR results using surveys ",
            "from a single frame from \n", nmr.filename, ".\n" )
  }else{
    message("The national BB8 NMR results specified don't exist.\n")
  }
  
  u5.filename <- paste0(country, '_res_natl_', time.model, 
                        "_", strata.model, "_u5_allsurveys.rda")
  strata_str <- ifelse(strata.model == "unstrat", "unstratified", "stratified")
  
  if(u5.filename %in% list.files("Betabinomial/U5MR/")){
    message("Loading natl ", strata_str, " BB8 U5MR results using all surveys ",
            "from \n", u5.filename, ".\n" )
  }else if(gsub(paste0(time.model, "_"), "", u5.filename) %in% 
           list.files("Betabinomial/U5MR/")){
    u5.filename <- gsub(paste0(time.model, "_"), "", u5.filename)
    message("Loading natl ", strata_str, " BB8 U5MR results using all surveys ",
            "from \n", u5.filename, ".\n" )
  }else if(gsub("_allsurveys", "", u5.filename) %in% 
           list.files("Betabinomial/U5MR/")){
    u5.filename <- gsub("_allsurveys", "", u5.filename)
    message("Loading natl ", strata_str, " BB8 U5MR results using surveys ",
            "from a single frame from \n", u5.filename, ".\n" )
  }else if(paste0(country, '_res_natl_',
                  strata.model, "_u5.rda") %in% 
           list.files("Betabinomial/U5MR/")){
    
    u5.filename <- gsub(paste0(time.model, "_"), "", u5.filename)
    u5.filename <- gsub("_allsurveys", "", u5.filename)
    message("Loading natl ", strata_str, " BB8 U5MR results using surveys ",
            "from a single frame from \n", u5.filename, ".\n" )
  }else{
    message("The national BB8 U5MR results specified don't exist.\n")
  }
  
  if(file.exists(paste0('Betabinomial/NMR/', nmr.filename))){
    load(file = paste0('Betabinomial/NMR/', nmr.filename))
  }
  if(file.exists(paste0('Betabinomial/U5MR/', u5.filename))){
    load(file = paste0('Betabinomial/U5MR/', u5.filename))
  }  
  
  if(exists('bb.res.natl.unstrat.nmr.allsurveys')){
    bb.res.natl.unstrat.nmr <- bb.res.natl.unstrat.nmr.allsurveys
  }
  if(exists('bb.res.natl.unstrat.u5.allsurveys')){
    bb.res.natl.unstrat.u5 <- bb.res.natl.unstrat.u5.allsurveys
  }
  
  if(exists('bb.res.natl.unstrat.nmr') & exists('bb.res.natl.unstrat.u5')){
    natl.bb.unstrat.frame <- 
      data.frame(lower_nmr = bb.res.natl.unstrat.nmr$overall$lower, 
                 median_nmr = bb.res.natl.unstrat.nmr$overall$median,
                 upper_nmr = bb.res.natl.unstrat.nmr$overall$upper,
                 lower_u5 = bb.res.natl.unstrat.u5$overall$lower,
                 median_u5 = bb.res.natl.unstrat.u5$overall$median, 
                 upper_u5 = bb.res.natl.unstrat.u5$overall$upper,
                 method='natl.bb.unstrat',years=bb.res.natl.unstrat.nmr$overall$years)
  }else if(exists('bb.res.natl.unstrat.nmr')){
    natl.bb.unstrat.frame <- 
      data.frame(lower_nmr = bb.res.natl.unstrat.nmr$overall$lower,
                 median_nmr = bb.res.natl.unstrat.nmr$overall$median,
                 upper_nmr = bb.res.natl.unstrat.nmr$overall$upper,
                 lower_u5 = NA, median_u5 = NA, upper_u5 = NA,
                 method='natl.bb.unstrat',years=bb.res.natl.unstrat.nmr$overall$years)
  }else if(exists('bb.res.natl.unstrat.u5')){
    natl.bb.unstrat.frame <- 
      data.frame(lower_nmr =NA, median_nmr = NA,
                 upper_nmr = NA,
                 lower_u5 = bb.res.natl.unstrat.u5$overall$lower,
                 median_u5 = bb.res.natl.unstrat.u5$overall$median, 
                 upper_u5 = bb.res.natl.unstrat.u5$overall$upper,
                 method='natl.bb.unstrat',
                 years=bb.res.natl.unstrat.u5$overall$years)
  }
  
  if(exists('bb.res.natl.strat.nmr') & exists('bb.res.natl.strat.u5')){
    natl.bb.strat.frame <- 
      data.frame(lower_nmr = bb.res.natl.strat.nmr$overall$lower,
                 median_nmr = bb.res.natl.strat.nmr$overall$median,
                 upper_nmr = bb.res.natl.strat.nmr$overall$upper,
                 lower_u5 = bb.res.natl.strat.u5$overall$lower,
                 median_u5 = bb.res.natl.strat.u5$overall$median,
                 upper_u5 = bb.res.natl.strat.u5$overall$upper,
                 method='natl.bb.strat', years=bb.res.natl.strat.nmr$overall$years)
  }else if(exists('bb.res.natl.strat.nmr')){
    natl.bb.strat.frame <- 
      data.frame(lower_nmr = bb.res.natl.strat.nmr$overall$lower,
                 median_nmr = bb.res.natl.strat.nmr$overall$median,
                 upper_nmr = bb.res.natl.strat.nmr$overall$upper,
                 lower_u5 = NA, median_u5 = NA, upper_u5 = NA,
                 method='natl.bb.strat',years=bb.res.natl.strat.nmr$overall$years)
  }else if(exists('bb.res.natl.strat.u5')){
    natl.bb.strat.frame <-
      data.frame(lower_nmr =NA, median_nmr = NA, upper_nmr = NA,
                 lower_u5 = bb.res.natl.strat.u5$overall$lower,
                 median_u5 = bb.res.natl.strat.u5$overall$median, 
                 upper_u5 = bb.res.natl.strat.u5$overall$upper,
                 method='natl.bb.strat',years=bb.res.natl.strat.u5$overall$years)
  }
  
}

## Admin-1 ####

{
  ### Direct ####
  nmr.filename <- paste0(country, '_direct_admin1_nmr.rda')
  
  if(nmr.filename %in% list.files("Direct/NMR/")){
    message("Loading natl NMR results from \n", nmr.filename, ".\n" )
  }else{
    message("The national Direct NMR results specified don't exist.\n")
  }
  
  
  u5.filename <- paste0(country, '_direct_admin1_u5.rda')
  
  if(u5.filename %in% list.files("Direct/U5MR/")){
    message("Loading natl U5MR results from \n", u5.filename, ".\n" )
    
  }else{
    message("The national BB8 U5MR results specified don't exist.\n")
  }
  
  load(file = paste0("Direct/NMR/",  nmr.filename))
  load(file = paste0("Direct/U5MR/",  u5.filename))
  adm1.dir.reg <- direct.admin1.nmr$region
  adm1.dir.est.nmr <- direct.admin1.nmr[direct.admin1.nmr$years %in%
                                          period.years, "mean"]
  adm1.dir.lower.nmr <- direct.admin1.nmr[direct.admin1.nmr$years %in% 
                                            period.years, "lower"]
  adm1.dir.upper.nmr <- direct.admin1.nmr[direct.admin1.nmr$years %in% 
                                            period.years, "upper"]
  adm1.dir.year.nmr <- direct.admin1.nmr[direct.admin1.nmr$years %in% 
                                           period.years, "years"]
  adm1.dir.svy.nmr <- direct.admin1.nmr[direct.admin1.nmr$years %in%
                                          period.years, "surveyYears"]
  adm1.dir.est.u5 <- direct.admin1.u5[direct.admin1.u5$years %in% 
                                        period.years, "mean"]
  adm1.dir.lower.u5 <- direct.admin1.u5[direct.admin1.u5$years %in% 
                                          period.years, "lower"]
  adm1.dir.upper.u5 <- direct.admin1.u5[direct.admin1.u5$years %in% 
                                          period.years, "upper"]
  adm1.dir.year.u5 <- direct.admin1.u5[direct.admin1.u5$years %in%
                                         period.years, "years"]
  adm1.dir.svy.u5 <- direct.admin1.u5[direct.admin1.u5$years %in%
                                        period.years, "surveyYears"]
  adm1.dir.frame <- data.frame()
  adm1.dir.frame <- data.frame(region = adm1.dir.reg,
                               lower_nmr=adm1.dir.lower.nmr, 
                               median_nmr=adm1.dir.est.nmr,
                               upper_nmr=adm1.dir.upper.nmr, 
                               lower_u5=adm1.dir.lower.u5,
                               median_u5=adm1.dir.est.u5, 
                               upper_u5=adm1.dir.upper.u5, 
                               method='adm1.dir',
                               years=adm1.dir.year.u5,
                               surveyYears = adm1.dir.svy.u5)
  
  
  
  ## SD 3-year ####
  nmr.filename <- paste0(country, '_res_admin1_', sd.time.model,
                         '_nmr_SmoothedDirect.rda')
  
  if(nmr.filename %in% list.files("Direct/NMR/")){
    message("Loading Admin-1 NMR results from \n", nmr.filename, ".\n" )
  }else if(gsub(paste0(sd.time.model, "_"), "", nmr.filename) %in% 
           list.files("Direct/NMR/")){
    nmr.filename <- gsub(paste0(sd.time.model, "_"), "", nmr.filename)
    message("Loading Admin-1NMR results from \n", nmr.filename, ".\n" )
  }else{
    message("The Admin-1 Smoothed Direct NMR results specified don't exist.\n")
  }
  
  u5.filename <- paste0(country, '_res_admin1_', time.model, 
                        "_u5_SmoothedDirect.rda")
  
  if(u5.filename %in% list.files("Direct/U5MR/")){
    message("Loading Admin 1 U5MR results from \n", u5.filename, ".\n" )
  }else if(gsub(paste0(sd.time.model, "_"), "", u5.filename) %in% 
           list.files("Direct/U5MR/")){
    u5.filename <- gsub(paste0(sd.time.model, "_"), "", u5.filename)
    message("Loading Admin1 U5MR results from \n", u5.filename, ".\n" )
  }else{
    message("The Admin 1 Smoothed Direct U5MR results specified don't exist.\n")
  }
  
  load(file = paste0('Direct/NMR/', nmr.filename))
  admin1.sd.nmr <- res.admin1.nmr
  load(file = paste0('Direct/U5MR/', u5.filename))
  admin1.sd.u5 <- res.admin1.u5
}
{
  ### SD yearly ####
  nmr.filename <- paste0(country, '_res_admin1_', sd.time.model,
                         '_nmr_SmoothedDirect_yearly.rda')
  
  if(nmr.filename %in% list.files("Direct/NMR/")){
    message("Loading yearly Admin-1 NMR results from \n", nmr.filename, ".\n" )
  }else if(gsub(paste0(sd.time.model, "_"), "", nmr.filename) %in% 
           list.files("Direct/NMR/")){
    nmr.filename <- gsub(paste0(sd.time.model, "_"), "", nmr.filename)
    message("Loading yearly Admin-1NMR results from \n", nmr.filename, ".\n" )
  }else{
    message("The yearly Admin-1 Smoothed Direct NMR results specified don't exist.\n")
  }
  
  u5.filename <- paste0(country, '_res_admin1_', time.model, 
                        "_u5_SmoothedDirect_yearly.rda")
  
  if(u5.filename %in% list.files("Direct/U5MR/")){
    message("Loading yearly Admin 1 U5MR results from \n", u5.filename, ".\n" )
  }else if(gsub(paste0(sd.time.model, "_"), "", u5.filename) %in% 
           list.files("Direct/U5MR/")){
    u5.filename <- gsub(paste0(sd.time.model, "_"), "", u5.filename)
    message("Loading yearly Admin1 U5MR results from \n", u5.filename, ".\n" )
  }else{
    message("The yearly Admin 1 Smoothed Direct U5MR results specified don't exist.\n")
  }
  
  load(file = paste0('Direct/NMR/', nmr.filename))
  admin1.sd.yearly.nmr <- sd.admin1.yearly.nmr
  load(file = paste0('Direct/U5MR/', u5.filename))
  admin1.sd.yearly.u5 <- sd.admin1.yearly.u5
}
{
  ### BB8 #####
  
  ## NMR File name
  if(bench.model == ""){
    nmr.filename <- paste0(country, '_res_adm1_', time.model, 
                           "_", strata.model, "_nmr_allsurveys.rda")
  }else{
    nmr.filename <- paste0(country, '_res_adm1_', time.model, 
                           "_", strata.model, "_nmr_allsurveys_bench.rda")
  }
  strata_str <- ifelse(strata.model == "unstrat", "unstratified", "stratified")
  bench_str <- ifelse(bench.model == "bench", "benchmarked", "unbenchmarked")
  
  if(nmr.filename %in% list.files("Betabinomial/NMR/")){
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
  }else if(bench.model == "bench" & (paste0(country, '_res_adm1_',
                                            strata.model, "_nmr_bench.rda") %in% 
                                     list.files("Betabinomial/NMR/"))){
    
    nmr.filename <- gsub(paste0(time.model, "_"), "", nmr.filename)
    nmr.filename <- gsub("_allsurveys", "", nmr.filename)
    message("Loading Admin-1", bench_str, " ", strata_str, 
            " BB8 NMR results using surveys ",
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
  if(bench.model == ""){
    u5.filename <- paste0(country, '_res_adm1_', time.model, 
                          "_", strata.model, "_u5_allsurveys.rda")
  }else{
    u5.filename <- paste0(country, '_res_adm1_', time.model, 
                          "_", strata.model, "_u5_allsurveys_bench.rda")
  }
  strata_str <- ifelse(strata.model == "unstrat", "unstratified", "stratified")
  bench_str <- ifelse(bench.model == "bench", "benchmarked", "unbenchmarked")
  
  if(u5.filename %in% list.files("Betabinomial/U5MR/")){
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
  }else if(bench.model == "bench" &
           (paste0(country, '_res_adm1_',
                   strata.model, "_u5_bench.rda") %in% 
            list.files("Betabinomial/U5MR/"))){
    
    u5.filename <- gsub(paste0(time.model, "_"), "", u5.filename)
    u5.filename <- gsub("_allsurveys", "", u5.filename)
    message("Loading Admin-1", bench_str, " ", strata_str, 
            " BB8 U5MR results using surveys ",
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
  
  #check if crisis adjusted file exists
  u5.filename.crisis <- gsub('.rda','_crisis.rda',u5.filename)
  if(u5.filename.crisis %in% list.files("Betabinomial/U5MR/")){
    u5.filename <- u5.filename.crisis
    message("Loading crisis-adjusted estimates from above model")
  }
  
  if(strata.model == "unstrat"){
    load(file = paste0('Betabinomial/NMR/', nmr.filename))
    load(file = paste0('Betabinomial/U5MR/', u5.filename))
    
    ##Load benchmarks
    nmr.bench.file <- gsub(paste0(country, "_res_"), "", 
                           nmr.filename)
    nmr.bench.file <- gsub("_allsurveys", "", nmr.bench.file)
    nmr.bench.file <- gsub("_bench", "_benchmarks", nmr.bench.file)
    
    u5.bench.file <- gsub(paste0(country, "_res_"), "", u5.filename)
    u5.bench.file <- gsub("_crisis", "", u5.bench.file)
    u5.bench.file <- gsub("_allsurveys", "", u5.bench.file)
    u5.bench.file <- gsub("_bench", "_benchmarks", u5.bench.file)
    
    load(file = paste0('Betabinomial/NMR/', nmr.bench.file))
    adm1.nmr.benchmarks <- bench.adj
    load(file = paste0("Betabinomial/U5MR/", u5.bench.file))
    adm1.u5.benchmarks <- bench.adj
    
    
    if(exists('bb.res.adm1.unstrat.nmr.allsurveys')){
      bb.res.adm1.unstrat.nmr <- bb.res.adm1.unstrat.nmr.allsurveys
    }
    if(exists('bb.res.adm1.unstrat.u5.allsurveys')){
      bb.res.adm1.unstrat.u5 <- bb.res.adm1.unstrat.u5.allsurveys
    }
    if(exists('bb.res.adm1.unstrat.nmr.allsurveys.bench')){
      bb.res.adm1.unstrat.nmr.bench <- bb.res.adm1.unstrat.nmr.allsurveys.bench
    }
    if(exists('bb.res.adm1.unstrat.u5.allsurveys.bench')){
      bb.res.adm1.unstrat.u5.bench <- bb.res.adm1.unstrat.u5.allsurveys.bench
    }
    
    if(bench.model == ""){
      admin1.unstrat.nmr.BB8 <- bb.res.adm1.unstrat.nmr$overall
      admin1.unstrat.u5.BB8 <- bb.res.adm1.unstrat.u5$overall
    }else{
      admin1.unstrat.nmr.BB8.bench <-  bb.res.adm1.unstrat.nmr.bench$overall
      if(grepl('crisis',u5.filename)){
        admin1.unstrat.u5.BB8.bench <- res_adm1_u5_crisis
      }else{
        admin1.unstrat.u5.BB8.bench <- bb.res.adm1.unstrat.u5.bench$overall
      }
    }
    
    
  }else{
    load(file = paste0('Betabinomial/NMR/', nmr.filename))
    load(file = paste0('Betabinomial/U5MR/', u5.filename))
    
    ##Load benchmarks
    nmr.bench.file <- gsub(paste0(country, "_res_"), "", 
                           nmr.filename)
    nmr.bench.file <- gsub("_allsurveys", "", nmr.bench.file)
    nmr.bench.file <- gsub("_bench", "_benchmarks", nmr.bench.file)
    
    u5.bench.file <- gsub(paste0(country, "_res_"), "", 
                          u5.filename)
    u5.bench.file <- gsub("_crisis", "", u5.bench.file)
    u5.bench.file <- gsub("_allsurveys", "", u5.bench.file)
    u5.bench.file <- gsub("_bench", "_benchmarks", u5.bench.file)
    
    load(file = paste0('Betabinomial/NMR/', nmr.bench.file))
    adm1.nmr.benchmarks <- bench.adj
    load(file = paste0("Betabinomial/U5MR/", u5.bench.file))
    adm1.u5.benchmarks <- bench.adj
    
    if(bench.model == ""){
      admin1.strat.nmr.BB8 <- bb.res.adm1.strat.nmr$overall
      admin1.strat.u5.BB8 <- bb.res.adm1.strat.u5$overall
    }else{
      admin1.strat.nmr.BB8.bench <-  bb.res.adm1.strat.nmr.bench$overall
      if(grepl('crisis',u5.filename)){
        admin1.strat.u5.BB8.bench <- res_adm1_u5_crisis
      }else{
        admin1.strat.u5.BB8.bench <- bb.res.adm1.strat.u5.bench$overall
      }
    }
  }  
}

## Admin-2#####
if(exists('poly.layer.adm2')){
  ### Direct ####
  
  ## is time model in file string?
  nmr.filename <- paste0(country, '_direct_admin2_nmr.rda')
  
  if(nmr.filename %in% list.files("Direct/NMR/")){
    message("Loading 3-year Admin-2 NMR direct estimates from \n", 
            nmr.filename, ".\n" )
  }else{
    message("The 3-year Admin-2 NMR direct estimates specified don't exist.\n")
  }
  
  
  u5.filename <- paste0(country, '_direct_admin2_u5.rda')
  
  if(u5.filename %in% list.files("Direct/U5MR/")){
    message("Loading 3-year Admin-2 U5MR direct estimates from \n", 
            u5.filename, ".\n" )
  }else{
    message("The 3-year Admin-2 U5MR direct estimates specified don't exist.\n")
  }
  
  load(file = paste0("Direct/NMR/",  nmr.filename))
  load(file = paste0("Direct/U5MR/",  u5.filename))
  adm2.dir.reg <- direct.admin2.nmr$region
  adm2.dir.est.nmr <- direct.admin2.nmr[direct.admin2.nmr$years %in%
                                          period.years, "mean"]
  adm2.dir.lower.nmr <- direct.admin2.nmr[direct.admin2.nmr$years %in% 
                                            period.years, "lower"]
  adm2.dir.upper.nmr <- direct.admin2.nmr[direct.admin2.nmr$years %in% 
                                            period.years, "upper"]
  adm2.dir.year.nmr <- direct.admin2.nmr[direct.admin2.nmr$years %in% 
                                           period.years, "years"]
  adm2.dir.svy.nmr <- direct.admin2.nmr[direct.admin2.nmr$years %in%
                                          period.years, "surveyYears"]
  adm2.dir.est.u5 <- direct.admin2.u5[direct.admin2.u5$years %in% 
                                        period.years, "mean"]
  adm2.dir.lower.u5 <- direct.admin2.u5[direct.admin2.u5$years %in% 
                                          period.years, "lower"]
  adm2.dir.upper.u5 <- direct.admin2.u5[direct.admin2.u5$years %in% 
                                          period.years, "upper"]
  adm2.dir.year.u5 <- direct.admin2.u5[direct.admin2.u5$years %in%
                                         period.years, "years"]
  adm2.dir.svy.u5 <- direct.admin2.u5[direct.admin2.u5$years %in%
                                        period.years, "surveyYears"]
  adm2.dir.frame <- data.frame()
  adm2.dir.frame <- data.frame(region = adm2.dir.reg,
                               lower_nmr=adm2.dir.lower.nmr, 
                               median_nmr=adm2.dir.est.nmr,
                               upper_nmr=adm2.dir.upper.nmr, 
                               lower_u5=adm2.dir.lower.u5,
                               median_u5=adm2.dir.est.u5, 
                               upper_u5=adm2.dir.upper.u5, 
                               method='adm2.dir',
                               years=adm2.dir.year.u5,
                               surveyYears = adm2.dir.svy.u5)
  
  
  
  ## SD 3-year ####
  {
    nmr.filename <- paste0(country, '_res_admin2_', sd.time.model,
                           '_nmr_SmoothedDirect.rda')
    
    if(nmr.filename %in% list.files("Direct/NMR/")){
      message("Loading Admin-2 NMR results from \n", nmr.filename, ".\n" )
      load(file = paste0('Direct/NMR/', nmr.filename))
      admin2.sd.nmr <- res.admin2.nmr
    }else if(gsub(paste0(sd.time.model, "_"), "", nmr.filename) %in% 
             list.files("Direct/NMR/")){
      nmr.filename <- gsub(paste0(sd.time.model, "_"), "", nmr.filename)
      message("Loading Admin-2 NMR results from \n", nmr.filename, ".\n" )
      load(file = paste0('Direct/NMR/', nmr.filename))
      admin2.sd.nmr <- res.admin2.nmr
    }else{
      message("The Admin-2 Smoothed Direct NMR results specified don't exist.\n")
    }
    
    u5.filename <- paste0(country, '_res_admin2_', time.model, 
                          "_u5_SmoothedDirect.rda")
    
    if(u5.filename %in% list.files("Direct/U5MR/")){
      message("Loading Admin 2 U5MR results from \n", u5.filename, ".\n" )
      
      load(file = paste0('Direct/U5MR/', u5.filename))
      admin2.sd.u5 <- res.admin2.u5
    }else if(gsub(paste0(sd.time.model, "_"), "", u5.filename) %in% 
             list.files("Direct/U5MR/")){
      u5.filename <- gsub(paste0(sd.time.model, "_"), "", u5.filename)
      message("Loading Admin2 U5MR results from \n", u5.filename, ".\n" )
      load(file = paste0('Direct/U5MR/', u5.filename))
      admin2.sd.u5 <- res.admin2.u5
    }else{
      message("The Admin 2 Smoothed Direct U5MR results specified don't exist.\n")
    }
  }
  ## SD yearly ####
  
  nmr.filename <- paste0(country, '_res_admin2_', sd.time.model,
                         '_nmr_SmoothedDirect_yearly.rda')
  
  if(nmr.filename %in% list.files("Direct/NMR/")){
    message("Loading yearly Admin-2 NMR results from \n", nmr.filename, ".\n" )
    load(file = paste0('Direct/NMR/', nmr.filename))
    admin2.sd.yearly.nmr <- sd.admin2.yearly.nmr
  }else if(gsub(paste0(sd.time.model, "_"), "", nmr.filename) %in% 
           list.files("Direct/NMR/")){
    nmr.filename <- gsub(paste0(sd.time.model, "_"), "", nmr.filename)
    message("Loading yearly Admin-1NMR results from \n", nmr.filename, ".\n" )
    load(file = paste0('Direct/NMR/', nmr.filename))
    admin2.sd.yearly.nmr <- sd.admin2.yearly.nmr
  }else{
    message("The yearly Admin-2 Smoothed Direct NMR results specified don't exist.\n")
  }
  
  u5.filename <- paste0(country, '_res_admin2_', time.model, 
                        "_u5_SmoothedDirect_yearly.rda")
  
  if(u5.filename %in% list.files("Direct/U5MR/")){
    message("Loading yearly Admin-2 U5MR results from \n", u5.filename, ".\n" )
    
    load(file = paste0('Direct/U5MR/', u5.filename))
    admin2.sd.yearly.u5 <- sd.admin2.yearly.u5
  }else if(gsub(paste0(sd.time.model, "_"), "", u5.filename) %in% 
           list.files("Direct/U5MR/")){
    u5.filename <- gsub(paste0(sd.time.model, "_"), "", u5.filename)
    message("Loading yearly Admin-2 U5MR results from \n", u5.filename, ".\n" )
    
    load(file = paste0('Direct/U5MR/', u5.filename))
    admin2.sd.yearly.u5 <- sd.admin2.yearly.u5
  }else{
    message("The yearly Admin 2 Smoothed Direct U5MR results specified don't exist.\n")
  }
  
  ## BB8 ####
  
  if(bench.model == ""){
    nmr.filename <- paste0(country, '_res_adm2_', time.model, 
                           "_", strata.model, "_nmr_allsurveys.rda")
  }else{
    nmr.filename <- paste0(country, '_res_adm2_', time.model, 
                           "_", strata.model, "_nmr_allsurveys_bench.rda")
  }
  strata_str <- ifelse(strata.model == "unstrat", "unstratified", "stratified")
  bench_str <- ifelse(bench.model == "bench", "benchmarked", "unbenchmarked")
  
  if(nmr.filename %in% list.files("Betabinomial/NMR/")){
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
  }else if(bench.model == "bench" & (paste0(country, '_res_adm2_',
                                            strata.model, "_nmr_bench.rda") %in% 
                                     list.files("Betabinomial/NMR/"))){
    
    nmr.filename <- gsub(paste0(time.model, "_"), "", nmr.filename)
    nmr.filename <- gsub("_allsurveys", "", nmr.filename)
    message("Loading Admin-2", bench_str, " ", strata_str, 
            " BB8 NMR results using surveys ",
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
  
  if(bench.model == ""){
    u5.filename <- paste0(country, '_res_adm2_', time.model, 
                          "_", strata.model, "_u5_allsurveys.rda")
  }else{
    u5.filename <- paste0(country, '_res_adm2_', time.model, 
                          "_", strata.model, "_u5_allsurveys_bench.rda")
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
  
  #check if crisis adjusted file exists
  u5.filename.crisis <- gsub('.rda','_crisis.rda',u5.filename)
  if(u5.filename.crisis %in% list.files("Betabinomial/U5MR/")){
    u5.filename <- u5.filename.crisis
    message("Loading crisis-adjusted estimates from above model")
  }
  
  if(strata.model == "unstrat"){
    load(file = paste0('Betabinomial/NMR/', nmr.filename))
    load(file = paste0('Betabinomial/U5MR/', u5.filename))
    
    ##Load benchmarks
    nmr.bench.file <- gsub(paste0(country, "_res_"), "", 
                           nmr.filename)
    nmr.bench.file <- gsub("_allsurveys", "", nmr.bench.file)
    nmr.bench.file <- gsub("_bench", "_benchmarks", nmr.bench.file)
    
    u5.bench.file <- gsub(paste0(country, "_res_"), "", 
                          u5.filename)
    u5.bench.file <- gsub("_crisis", "", u5.bench.file)
    u5.bench.file <- gsub("_allsurveys", "", u5.bench.file)
    u5.bench.file <- gsub("_bench", "_benchmarks", u5.bench.file)
    
    load(file = paste0('Betabinomial/NMR/', nmr.bench.file))
    adm2.nmr.benchmarks <- bench.adj
    load(file = paste0("Betabinomial/U5MR/", u5.bench.file))
    adm2.u5.benchmarks <- bench.adj
    
    if(exists('bb.res.adm2.unstrat.nmr.allsurveys')){
      bb.res.adm2.unstrat.nmr <- bb.res.adm2.unstrat.nmr.allsurveys
    }
    if(exists('bb.res.adm2.unstrat.u5.allsurveys')){
      bb.res.adm2.unstrat.u5 <- bb.res.adm2.unstrat.u5.allsurveys
    }
    if(exists('bb.res.adm2.unstrat.nmr.allsurveys.bench')){
      bb.res.adm2.unstrat.nmr.bench <- bb.res.adm2.unstrat.nmr.allsurveys.bench
    }
    if(exists('bb.res.adm2.unstrat.u5.allsurveys.bench')){
      bb.res.adm2.unstrat.u5.bench <- bb.res.adm2.unstrat.u5.allsurveys.bench
    }
    
    if(bench.model == ""){
      admin2.unstrat.nmr.BB8 <- bb.res.adm2.unstrat.nmr$overall
      admin2.unstrat.u5.BB8 <- bb.res.adm2.unstrat.u5$overall
    }else{
      admin2.unstrat.nmr.BB8.bench <-  bb.res.adm2.unstrat.nmr.bench$overall
      if(grepl('crisis',u5.filename)){
        admin2.unstrat.u5.BB8.bench <- res_adm2_u5_crisis
      }else{
        admin2.unstrat.u5.BB8.bench <- bb.res.adm2.unstrat.u5.bench$overall
      }
    }
    
    
  }else{
    load(file = paste0('Betabinomial/NMR/', nmr.filename))
    load(file = paste0('Betabinomial/U5MR/', u5.filename))
    
    ##Load benchmarks
    nmr.bench.file <- gsub(paste0(country, "_res_"), "", 
                           nmr.filename)
    nmr.bench.file <- gsub("_allsurveys", "", nmr.bench.file)
    nmr.bench.file <- gsub("_bench", "_benchmarks", nmr.bench.file)
    
    u5.bench.file <- gsub(paste0(country, "_res_"), "", 
                          u5.filename)
    u5.bench.file <- gsub("_crisis", "", u5.bench.file)
    u5.bench.file <- gsub("_allsurveys", "", u5.bench.file)
    u5.bench.file <- gsub("_bench", "_benchmarks", u5.bench.file)
    
    load(file = paste0('Betabinomial/NMR/', nmr.bench.file))
    adm2.nmr.benchmarks <- bench.adj
    load(file = paste0("Betabinomial/U5MR/", u5.bench.file))
    adm2.u5.benchmarks <- bench.adj
    
    
    if(bench.model == ""){
      admin2.strat.nmr.BB8 <- bb.res.adm2.strat.nmr$overall
      admin2.strat.u5.BB8 <- bb.res.adm2.strat.u5$overall
    }else{
      admin2.strat.nmr.BB8.bench <-  bb.res.adm2.strat.nmr.bench$overall
      if(grepl('crisis',u5.filename)){
        admin2.strat.u5.BB8.bench <- res_adm2_u5_crisis
      }else{
        admin2.strat.u5.BB8.bench <- bb.res.adm2.strat.u5.bench$overall
      }
    }
  }  
}

## IGME estimates ####
igme.frame <- 
  as.data.frame(cbind(igme.ests.nmr$LOWER_BOUND,
                      igme.ests.nmr$OBS_VALUE,igme.ests.nmr$UPPER_BOUND,
                      igme.ests.u5$LOWER_BOUND,
                      igme.ests.u5$OBS_VALUE,igme.ests.u5$UPPER_BOUND))
colnames(igme.frame) = c("lower_nmr", "median_nmr", "upper_nmr",
                         "lower_u5", "median_u5", "upper_u5")
igme.frame$method <- "igme"
igme.frame$years <- beg.year:max(igme.ests.nmr$year)

# National Plots ####
model_cols <- brewer.pal(n = 12, name = "Paired")
survey_cols <- rainbow(length(surveys))

## Spaghetti: 3 panel ####
setwd(res.dir)

### Specify Natl model ####

if(strata.model == "unstrat"){
  tmp_plot <- natl.bb.unstrat.frame
}else{
  tmp_plot <- natl.bb.strat.frame
}

for(outcome in c("nmr", "u5")){
  tmp.dir <- ifelse(outcome == "u5", "u5mr", outcome)
  
  pdf(paste0("Figures/Summary/", toupper(tmp.dir), "/",
             country, "_natl_", strata.model,
             "_", time.model, "_", outcome,
             "_Spaghetti.pdf"),
      height = 2.67, width = 8)
  {
    tmp.area <- tmp_plot[,c("method", "years", 
                            paste0(c("lower", "upper", "median"),
                                   "_", outcome))]
    names(tmp.area)[3:5] <- c("lower", "upper", "median")
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.area$median <- tmp.area$median*1000
    tmp.area$upper <- tmp.area$upper*1000
    tmp.area$lower <- tmp.area$lower*1000
    tmp.area$years.num <- as.numeric(paste0(tmp.area$years))
    
    
    par(mfrow = c(1,3),lend=1)
    if(dim(tmp.area)[1] != 0 & 
       !(sum(is.na(tmp.area$median)) == nrow(tmp.area))){
      plot.max <- max(1000*natl.dir.frame[ , paste0("median_", outcome)] + 25,
                      na.rm = T)
    }else{
      plot.max <- 0.25*1000
    }
    
    if (nrow(tmp.area) >0 & sum(is.na(tmp.area[, "median"]))
        == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", ylab = toupper(tmp.dir),
           ylim = c(0, ceiling(plot.max)),
           xlim = range(plot_years),
           type = 'l', col = cols[1], lwd = 2,
           main = country)
      legend('topright', bty = 'n', col = c(survey_cols, 
                                            'grey80', 'black'),
                                            lwd = 2, lty = 1,
             
             legend = c(surveys, "IGME", "BB8"))
      
    } else {
      
      for(survey in surveys){
        tmp <- natl.dir.frame[natl.dir.frame$surveyYears == survey,
                              c("method", "years", "surveyYears",
                                paste0(c("lower", "median", "upper"),
                                       "_", outcome))]
        names(tmp)[4:6] <- c("lower", "median", "upper")
        tmp[ , c("lower", "median", "upper")] <- 
          1000*tmp[ , c("lower", "median", "upper")]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = toupper(tmp.dir),
                 ylim = c(0, plot.max),
                 xlim = c(2000, end.proj.year),
                 type = 'l', col = survey_cols[svy.idx], lwd = 2,
                 main = country)
            
            lines(2000:end.year, tmp$median, cex = tmp$cex2,
                  type = 'l', col = survey_cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            
            points(2000:end.year, tmp$median, pch = 19,
                   col = alpha(survey_cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
          }else{
            plot(NA,
                 xlab = "Year", ylab = toupper(tmp.dir),
                 ylim = c(0, plot.max),
                 xlim = c(2000, end.year),
                 type = 'l', col = survey_cols[svy.idx], lwd = 2,
                 main =  country)
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(2000:end.year, tmp$median, cex = tmp$cex2,
                  type = 'l', col = survey_cols[svy.idx],
                  lwd = 2)
            points(2000:end.year, tmp$median, pch = 19,
                   col = alpha(survey_cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      igme.years <- igme.frame$years
      lines(igme.frame$years, 1000*igme.frame[,paste0("median_", outcome)],
            lty = 1, lwd = 2, col = "grey80")
      
      
      lines(tmp.area$years.num,
            tmp.area$median,
            col = "black",
            lwd = 2, lty = 1)
      
      
      legend('topright', bty = 'n', 
             col = c(survey_cols, "grey88", 'black'),
             lwd = 2, lty = c(rep(1, length(survey_cols) + 2)),
             legend = c(surveys, "IGME", "BB8"),
             cex = 0.6)
      
      for(survey in surveys){
        tmp <- natl.dir.frame[natl.dir.frame$surveyYears == survey,
                              c("method", "years", "surveyYears",
                                paste0(c("lower", "median", "upper"),
                                       "_", outcome))]
        names(tmp)[4:6] <- c("lower", "median", "upper")
        
        tmp[ , c("lower", "median", "upper")] <- 
          1000*tmp[ , c("lower", "median", "upper")]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx == 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = toupper(tmp.dir),
                 ylim = c(0, plot.max),
                 xlim = c(2000, end.proj.year),
                 type = 'l', col = survey_cols[svy.idx], lwd = 2,
                 main = country)
            
            poly.years <- tmp$years[!is.na(tmp$upper)]
            
            polygon(x = c(poly.years,
                          rev(poly.years)),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha(survey_cols[svy.idx], 0.25),
                    border = FALSE)
            
            
          }else{
            plot(NA,
                 xlab = "Year", ylab = toupper(tmp.dir),
                 ylim = c(0, plot.max),
                 xlim = c(2000, end.proj.year),
                 type = 'l', col = survey_cols[svy.idx], lwd = 2,
                 main =  paste0(country))
            
            lines(2000:end.year, tmp$median, cex = tmp$cex2,
                  type = 'l', col = survey_cols[svy.idx],
                  lwd = 2)
            points(2000:end.year, tmp$median, pch = 19,
                   col = alpha(survey_cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          }
        }else{
          poly.years <- tmp$years[!is.na(tmp$upper)]
          
          polygon(x = c(poly.years,
                        rev(poly.years)),
                  y = c(tmp$upper[!is.na(tmp$upper)],
                        rev(tmp$lower[!is.na(tmp$lower)])),
                  col = alpha(survey_cols[svy.idx], 0.25),
                  border = FALSE)
        }
        
      }  
      
      legend('topright', bty = 'n',
             fill = alpha(survey_cols, 0.25),
             border = survey_cols,
             legend = surveys,
             cex = 0.6)
      
      for(survey in surveys){
        tmp <- natl.dir.frame[natl.dir.frame$surveyYears == survey,
                              c("method", "years", "surveyYears",
                                paste0(c("lower", "median", "upper"),
                                       "_", outcome))]
        names(tmp)[4:6] <- c("lower", "median", "upper")
        
        tmp[ , c("lower", "median", "upper")] <- 
          1000*tmp[ , c("lower", "median", "upper")]
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx == 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = toupper(tmp.dir),
                 ylim = c(0, plot.max),
                 xlim = c(2000, end.proj.year),
                 type = 'l', col = survey_cols[svy.idx], lwd = 2,
                 main = country)
            
          }
        }  
      }
      
      igme.years <- igme.frame$years
      polygon(x = c(igme.years, rev(igme.years)),
              y = 1000*c(igme.frame[,paste0("upper_", outcome)],
                         rev(igme.frame[,paste0("lower_", outcome)])),
              col = alpha("grey80", 0.35),
              border = FALSE)
      
      polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
              y = c(tmp.area$upper, rev(tmp.area$lower)),
              col = alpha('black', 0.35),
              border = FALSE)
      lines(tmp.area$years.num,
            tmp.area$median,
            col = 'black',
            lwd = 2, lty = 1)
      legend('topright', bty = 'n',
             fill = alpha(c("grey80",'black'), .25),
             border = c("grey80",'black'),  cex = 0.65,
             legend = c( 'IGME', 'BB8'))
    }
    
    
  }
  dev.off()
}


# Admin-2 Plots ####
setwd(res.dir)

## Specify Admin-2 Result Object ####

tmp_plot <- list()
if(strata.model == "unstrat"){
  if(bench.model == ""){
    tmp_plot$nmr <- admin2.unstrat.nmr.BB8
    tmp_plot$u5 <- admin2.unstrat.u5.BB8
  }else{
    tmp_plot$nmr <- admin2.unstrat.nmr.BB8.bench
    tmp_plot$u5 <- admin2.unstrat.u5.BB8.bench
  }
}else{
  if(bench.model == ""){
    tmp_plot$nmr <- admin2.strat.nmr.BB8
    tmp_plot$u5 <- admin2.strat.u5.BB8
  }else{
    tmp_plot$nmr <- admin2.strat.nmr.BB8.bench
    tmp_plot$u5 <- admin2.strat.u5.BB8.bench
  }
}


## Spaghetti: Medians within Admin-1 ####


# change the region names from Internal to GADM for legend labels

tmp_plot <- lapply(tmp_plot, function(res.admin2){
  
  # change the region names from Internal to GADM for legend labels
  res.admin2$region1.gadm <- 
    res.admin2$region.gadm <- 
    res.admin2$region.orig <- 
    res.admin2$region
  
  # If this isn't working for you, you may need to edit 
  # the way adm1_on_adm2 is defined
  for (i in 1:nrow(admin2.names)) {
    res.admin2$region[as.character(res.admin2$region) == 
                        as.character(admin2.names$Internal[i])] <-
      paste(as.character(admin2.names$GADM[i]),
            eval(str2lang(adm1_on_adm2))[i], sep =", ")
    res.admin2$region.gadm[as.character(res.admin2$region.orig) == 
                             as.character(admin2.names$Internal[i])] <-
      paste(as.character(admin2.names$GADM[i]))
    res.admin2$region1.gadm[as.character(res.admin2$region.orig) == 
                              as.character(admin2.names$Internal[i])] <-
      paste(eval(str2lang(adm1_on_adm2))[i])
    
  }
  
  res.admin2
})

# make the plot

numberAreasPerPage <- 21
numberAreasTotal <- nrow(admin2.names)
numberPages <- ceiling(numberAreasTotal/numberAreasPerPage)

# order data by median magnitude in 2021
tmp_plot$u5$years.num <- as.numeric(paste0(tmp_plot$u5$years))
areaOrder <- tmp_plot$u5 %>% 
  filter(years.num == end.proj.year) %>%
  arrange(median) %>% 
  dplyr::select(region.orig) %>% unlist()

# loop and make plots
for (i in 1:numberPages) {
  if (i != numberPages) {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
  } else {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
  }
  
  for(outcome in c("nmr", "u5")){
    tmp <- tmp_plot[[outcome]]
    tmp <- tmp[tmp$region.orig %in% areas,]
    tmp.dir <- ifelse(outcome == "u5", "u5mr", outcome)
    bench_str <- ifelse(bench.model == "", bench.model,
                        paste0("_", bench.model))
    pdf(paste0("Figures/Summary/", toupper(tmp.dir), "/",
               country, '_Admin2_', outcome, '_SpaghettiAll_',
               time.model, '_', strata.model, bench_str, "_",
               i,'.pdf'))
    par(lend=1)
    
    g <- ggplot(tmp, aes(x = years.num, 
                         y = median*1000,
                         col = region)) +
      geom_line() +
      geom_point() +
      theme_light() +
      xlab("Year") +
      ylab(paste0(toupper(tmp.dir),
                  ": deaths per 1000 live births")) +
      ggtitle(country) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 8)) +
      guides(col = guide_legend(title = "Admin-2", ncol = 3)) +
      ylim(c(0, max(tmp$median)*1000))
    print(g)
    dev.off()
  }
}

## Spaghetti: 6 per page ####
bench_str <- ifelse(bench.model == "", bench.model,
                    paste0("_", bench.model))

for(outcome in c("nmr", "u5")){
  tmp.dir <- ifelse(outcome == "u5", "u5mr", outcome)
  
  pdf(paste0("Figures/Summary/", toupper(tmp.dir), "/",
             country, '_Admin2_', outcome, "_",
             time.model, "_", strata.model, bench_str, "_", 
             "Spaghetti_6per.pdf"),
      height = 9, width = 6)
  {
    par(mfrow = c(3,2), lend=1)
    
    area.idx <- 0
    for(area in admin2.names$Internal){
      area.idx <- area.idx + 1
      tmp.area <- tmp_plot[[outcome]][,c("region", "region.orig", "years.num", 
                                         "lower", "upper", "median")] %>% 
        filter(region.orig == area)
      tmp.area$width <- tmp.area$upper - tmp.area$lower
      tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
      tmp.area$cex2[tmp.area$cex2 > 6] <- 6
      tmp.area[,c("median", "lower","upper")] <-
        tmp.area[,c("median", "lower","upper")]*1000
      tmp.area$years.num <- as.numeric(paste0(tmp.area$years))
      
      
      if(dim(tmp.area)[1] != 0 &
         !(sum(is.na(tmp.area$median)) == nrow(tmp.area))){
        plot.max <- max(max(c(1000*adm2.dir.frame[adm2.dir.frame$region.orig == 
                                               as.character(area),
                                             paste0("median_", outcome)],
                              tmp.area$median), na.rm = TRUE),
                        na.rm = T) + 25
      }else{
        plot.max <- 25
      }
      
      if (nrow(tmp.area) > 0 & sum(is.na(tmp.area$median)) == nrow(tmp.area)){
        plot(NA,
             xlab = "Year", ylab = toupper(tmp.dir),
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year + 1),
             type = 'l', col = survey_cols[1], lwd = 2,
             main = admin2.names$GADM[area.idx])
        legend('topright', bty = 'n', col = c(cols, 'black'),
               lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
        
      } else {
        
        for(survey in surveys){
          tmp <- adm2.dir.frame[adm2.dir.frame$surveyYears == survey &
                                  adm2.dir.frame$region == 
                                  admin2.names$Internal[area.idx],]
          svy.idx <- match(survey, surveys) 
          
          
          if(svy.idx == 1){
            if(dim(tmp)[1] != 0){
              plot(NA,
                   xlab = "Year", ylab = toupper(tmp.dir),
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.proj.year + 1),
                   type = 'l', col = survey_cols[svy.idx], lwd = 2,
                   main = admin2.names$GADM[area.idx])
              
              lines(pane.years[1:nrow(tmp)],
                    1000*tmp[, paste0("median_", outcome)],
                    cex = tmp$cex2,
                    type = 'l', col = survey_cols[svy.idx],
                    lwd = 2)
              points(pane.years[1:nrow(tmp)],
                     1000*tmp[, paste0("median_", outcome)],
                     pch = 19,
                     col = alpha(survey_cols[svy.idx], 0.35),
                     cex = tmp$cex2)
              
            }else{
              plot(NA,
                   xlab = "Year", ylab = toupper(tmp.dir),
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.proj.year + 1),
                   type = 'l', col = survey_cols[svy.idx], lwd = 2,
                   main =  paste0(admin2.names$GADM[area.idx]))
            }
          }else{
            if(dim(tmp)[1] != 0){
              lines(pane.years[1:nrow(tmp)],
                    1000*tmp[,paste0("median_", outcome)],
                    cex = tmp$cex2,
                    type = 'l', col = survey_cols[svy.idx],
                    lwd = 2)
              points(pane.years[1:nrow(tmp)],
                     1000*tmp[, paste0("median_", outcome)],
                     pch = 19,
                     col = alpha(survey_cols[svy.idx], 0.35),
                     cex = tmp$cex2)
            } 
          }
          
        }
        
        lines(tmp.area$years.num, tmp.area$median, 
              col = 'black', lwd = 2, lty = 1)
        
        polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
                y = c(tmp.area$upper, rev(tmp.area$lower)),
                col = alpha('black', 0.25), 
                border = FALSE)
        
        legend('topright', bty = 'n', col = c(survey_cols, 'black'),
               lwd = 2, lty = c(rep(1, length(cols)+1)),
               legend = c(surveys, "BB8"),
               cex = 0.6)
        
      }
    }
    dev.off()
  }
}

## Maps ####
### Make map ####
years_vt <- plot.years
n_years <- length(years_vt)
bench_str <- ifelse(bench.model == "", "", "_bench")
country_code_dt <- data.table(Country = country,
                              code = gadm.abbrev)
admin_level_dt <- data.table(Admin = "admin2", level = 2)
admin_name_dt <- as.data.table(admin2.names)

for(outcome in c("nmr", "u5")){
  
  tmp.dir <- ifelse(outcome == "u5", "u5mr", outcome)
  
  
  data_plot_dt <- NULL
  
  for(year in years_vt){
    cond <- tmp_plot[[outcome]][tmp_plot[[outcome]]$years.num == year,]
  
    # create plotting area names (just admin 1 name if admin = 1,
    # or 'admin2,\n admin1' if admin = 2)
    # if this part is not working, please revist how
    # adm1_on_adm2 is defined
    admin_name_dt$nameToPlot <- eval(str2lang(adm1_on_adm2))
    
    # create data to plot
    data_plot_dt_year <- 
      data.table(Year = year, 
                 Internal = cond$region.orig,
                 GADM = admin_name_dt[match(admin_name_dt$Internal,
                                            cond$region.orig), GADM],
                 nameToPlot = admin_name_dt[match(admin_name_dt$Internal,
                                                  cond$region.orig), nameToPlot],
                 U5MR_median = cond$median)
    
    data_plot_dt_year[, "NAME_2" := GADM]
    
    data_plot_dt <- rbind(data_plot_dt, data_plot_dt_year)
  }
  
 
  rowcount <- ceiling(length(years_vt)/5)
  
  pdf(paste0("Figures/Summary/", toupper(tmp.dir),
             "/", country, "_adm2_", time.model, "_",
             strata.model, "_", outcome, bench_str, "_medianmap.pdf"),
      width = 10, height = 3.5*rowcount)
  {
    data_plot_dt_df <- as.data.frame(data_plot_dt)
    print(SUMMER::mapPlot(data = data_plot_dt_df, 
                          "Year", is.long = T,
                          values = "U5MR_median", direction = -1,
                          geo = poly.adm2, ncol = 5,
                          by.data = "GADM",
                          legend.label = toupper(tmp.dir),
                          per1000 = TRUE,
                          by.geo = strsplit(poly.label.adm2, "\\$")[[1]][2]))
  }
  dev.off()
  
  pdf(paste0("Figures/Summary/", toupper(tmp.dir),
             "/", country, "_adm2_", time.model, "_",
             strata.model, "_", outcome, bench_str, "_medianmap_20002021.pdf"),
      width = 7, height = 7)
  {
    
    data_plot_dt_df <- as.data.frame(data_plot_dt)
    print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year %in% 
                                                   c(2000, 2010, 2015, 2021),],
                          is.long = T, 
                          variables = "Year", 
                          values = "U5MR_median",direction = -1,
                          geo = poly.adm2, ncol = 2,
                          legend.label = toupper(tmp.dir),
                          per1000 = TRUE,
                          by.data = "GADM",
                          by.geo = strsplit(poly.label.adm2, "\\$")[[1]][2]))
  }
  dev.off()
  
}

## Benchmarks ####

if(bench.model == "bench"){
  y_lims <- 1 + c(-1,1)*max(abs(1- range(adm2.nmr.benchmarks$ratio,
                                         na.rm = TRUE)), na.rm = TRUE)
  
  pdf(paste0("Figures/Summary/NMR/",
             country, "_Admin2_nmr_",
             time.model, "_", strata.model, "_benchmarks.pdf"))
  {
    plot(adm2.nmr.benchmarks$years,
         adm2.nmr.benchmarks$ratio,
         xlab = "Year", ylab = "(Unbenchmarked Median)/(IGME Median)",
         ylim = y_lims, main = "",
         type = "n")
    abline(h = 1)
    lines(adm2.nmr.benchmarks$years,
          adm2.nmr.benchmarks$ratio,
          lwd = 2, col = "firebrick")
  }
  dev.off()
  
  
  y_lims <- 1 + c(-1,1)*max(abs(1- range(adm2.u5.benchmarks$ratio,
                                         na.rm = TRUE)), na.rm = TRUE)
  
  pdf(paste0("Figures/Summary/U5MR/",
             country, "_Admin2_u5_",
             time.model, "_", strata.model, "_benchmarks.pdf"))
  {
    plot(adm2.u5.benchmarks$years,
         adm2.u5.benchmarks$ratio,
         xlab = "Year", ylab = "(Unbenchmarked Median)/(IGME Median)",
         ylim = y_lims, main = "",
         type = "n")
    abline(h = 1)
    lines(adm2.u5.benchmarks$years,
          adm2.u5.benchmarks$ratio,
          lwd = 2, col = "firebrick")
  }
  dev.off()
}
# Admin-1 Plots ####
## Specify Admin-1 Result Object ####

tmp_plot <- list()
if(strata.model == "unstrat"){
  if(bench.model == ""){
    tmp_plot$nmr <- admin1.unstrat.nmr.BB8
    tmp_plot$u5 <- admin1.unstrat.u5.BB8
  }else{
    tmp_plot$nmr <- admin1.unstrat.nmr.BB8.bench
    tmp_plot$u5 <- admin1.unstrat.u5.BB8.bench
  }
}else{
  if(bench.model == ""){
    tmp_plot$nmr <- admin1.strat.nmr.BB8
    tmp_plot$u5 <- admin1.strat.u5.BB8
  }else{
    tmp_plot$nmr <- admin1.strat.nmr.BB8.bench
    tmp_plot$u5 <- admin1.strat.u5.BB8.bench
  }
}


## Spaghetti: Medians ####


# change the region names from Internal to GADM for legend labels

tmp_plot <- lapply(tmp_plot, function(res.admin1){
  res.admin1$region.orig <- res.admin1$region
  for (i in 1:nrow(admin1.names)) {
    res.admin1$region[as.character(res.admin1$region) == 
                        as.character(admin1.names$Internal[i])] <- 
      paste(as.character(admin1.names$GADM[i]))
  }
  
  
  res.admin1
})

# make the plot

numberAreasPerPage <- 21
numberAreasTotal <- nrow(admin1.names)
numberPages <- ceiling(numberAreasTotal/numberAreasPerPage)

# order data by median magnitude in 2021
tmp_plot$u5$years.num <- as.numeric(paste0(tmp_plot$u5$years))
areaOrder <- tmp_plot$u5 %>% 
  filter(years.num == end.proj.year) %>%
  arrange(median) %>% 
  dplyr::select(region.orig) %>% unlist()

# loop and make plots
for (i in 1:numberPages) {
  if (i != numberPages) {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
  } else {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
  }
  
  for(outcome in c("nmr", "u5")){
    tmp <- tmp_plot[[outcome]]
    tmp <- tmp[tmp$region.orig %in% areas,]
    tmp.dir <- ifelse(outcome == "u5", "u5mr", outcome)
    bench_str <- ifelse(bench.model == "", bench.model,
                        paste0("_", bench.model))

    pdf(paste0("Figures/Summary/", toupper(tmp.dir), "/",
               country, '_Admin1_', outcome, '_SpaghettiAll_',
               time.model, '_', strata.model, bench_str, "_",
               i,'.pdf'))
    par(lend=1)
    
    g <- ggplot(tmp, aes(x = years.num, 
                         y = median*1000,
                         col = region)) +
      geom_line() +
      geom_point() +
      theme_light() +
      xlab("Year") +
      ylab(paste0(toupper(tmp.dir),
                  ": deaths per 1000 live births")) +
      ggtitle(country) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 8)) +
      guides(col = guide_legend(title = "Admin-1", ncol = 3)) +
      ylim(c(0, max(tmp$median)*1000))
    print(g)
    dev.off()
  }
}
## Spaghetti: 6 per page ####
bench_str <- ifelse(bench.model == "", bench.model,
                    paste0("_", bench.model))

for(outcome in c("nmr", "u5")){
  tmp.dir <- ifelse(outcome == "u5", "u5mr", outcome)
  
  pdf(paste0("Figures/Summary/", toupper(tmp.dir), "/",
             country, '_Admin1_', outcome, "_",
             time.model, "_", strata.model, bench_str,  "_",
             "Spaghetti_6per.pdf"),
      height = 9, width = 6)
  {
    par(mfrow = c(3,2), lend=1)
    
    area.idx <- 0
    for(area in admin1.names$Internal){
      area.idx <- area.idx + 1
      tmp.area <- tmp_plot[[outcome]][,c("region", "region.orig", "years.num", 
                                         "lower", "upper", "median")] %>% 
        filter(region.orig == area)
      tmp.area$width <- tmp.area$upper - tmp.area$lower
      tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
      tmp.area$cex2[tmp.area$cex2 > 6] <- 6
      tmp.area[,c("median", "lower","upper")] <-
        tmp.area[,c("median", "lower","upper")]*1000
      
      
      if(dim(tmp.area)[1] != 0 &
         !(sum(is.na(tmp.area$median)) == nrow(tmp.area))){
        max(max(c(1000*adm1.dir.frame[adm1.dir.frame$region.orig == 
                                        as.character(area),
                                      paste0("median_", outcome)],
                  tmp.area$median), na.rm = TRUE),
            na.rm = T) + 25
      }else{
        plot.max <- 25
      }
      
      if (nrow(tmp.area) > 0 & sum(is.na(tmp.area$median)) == nrow(tmp.area)){
        plot(NA,
             xlab = "Year", ylab = toupper(tmp.dir),
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year + 1),
             type = 'l', col = survey_cols[1], lwd = 2,
             main = admin1.names$GADM[area.idx])
        legend('topright', bty = 'n', col = c(cols, "grey80", 'black'),
               lwd = 2, lty = 1, legend = c(surveys, "IGME", "Betabinomial"))
        
      } else {
        
        for(survey in surveys){
          tmp <- adm1.dir.frame[adm1.dir.frame$surveyYears == survey &
                                  adm1.dir.frame$region == 
                                  admin1.names$Internal[area.idx],]
          svy.idx <- match(survey, surveys) 
          
          
          if(svy.idx == 1){
            if(dim(tmp)[1] != 0){
              plot(NA,
                   xlab = "Year", ylab = toupper(tmp.dir),
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.proj.year + 1),
                   type = 'l', col = survey_cols[svy.idx], lwd = 2,
                   main = admin1.names$GADM[area.idx])
              
              lines(pane.years[1:nrow(tmp)],
                    1000*tmp[, paste0("median_", outcome)],
                    cex = tmp$cex2,
                    type = 'l', col = survey_cols[svy.idx],
                    lwd = 2)
              
              points(pane.years[1:nrow(tmp)],
                     1000*tmp[, paste0("median_", outcome)],
                     pch = 19,
                     col = alpha(survey_cols[svy.idx], 0.35),
                     cex = tmp$cex2)
              
            }else{
              plot(NA,
                   xlab = "Year", ylab = toupper(tmp.dir),
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.proj.year + 1),
                   type = 'l', col = survey_cols[svy.idx], lwd = 2,
                   main =  paste0(admin1.names$GADM[area.idx]))
            }
          }else{
            if(dim(tmp)[1] != 0){
              lines(pane.years[1:nrow(tmp)],
                    1000*tmp[,paste0("median_", outcome)],
                    cex = tmp$cex2,
                    type = 'l', col = survey_cols[svy.idx],
                    lwd = 2)
              points(pane.years[1:nrow(tmp)],
                     1000*tmp[, paste0("median_", outcome)],
                     pch = 19,
                     col = alpha(survey_cols[svy.idx], 0.35),
                     cex = tmp$cex2)
            } 
          }
          
        }
        
        lines(tmp.area$years.num,tmp.area$median, 
              col = 'black', lwd = 2, lty = 1)
        
        polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
                y = c(tmp.area$upper, rev(tmp.area$lower)),
                col = alpha('black', 0.25), 
                border = FALSE)
        
        legend('topright', bty = 'n', col = c(survey_cols, 'black'),
               lwd = 2, lty = c(rep(1, length(cols)+1)),
               legend = c(surveys, "BB8"),
               cex = 0.6)
        
      }
    }
    dev.off()
  }
}

## Maps ####
### Make map ####
### Make map ####
years_vt <- plot.years
n_years <- length(years_vt)
bench_str <- ifelse(bench.model == "", "", "_bench")
country_code_dt <- data.table(Country = country,
                              code = gadm.abbrev)
country_code_dt <- data.table(Country = country,
                              code = gadm.abbrev)
admin_level_dt <- data.table(Admin = "admin1", level = 2)
admin_name_dt <- as.data.table(admin1.names)

for(outcome in c("nmr", "u5")){
  
  tmp.dir <- ifelse(outcome == "u5", "u5mr", outcome)

  
  data_plot_dt <- NULL
  
  for(year in years_vt){
    
    cond <- tmp_plot[[outcome]][tmp_plot[[outcome]]$years.num == year,]
    
    # create plotting area names (just admin 1 name if admin = 1,
    # or 'admin2,\n admin1' if admin = 2)
    admin_name_dt$nameToPlot <- eval(str2lang(poly.label.adm1))
    
    # create data to plot
    data_plot_dt_year <- 
      data.table(Year = year, 
                 Internal = cond$region.orig,
                 GADM = cond$region,
                 nameToPlot = admin_name_dt[match(admin_name_dt$Internal,
                                                  cond$region.orig), nameToPlot],
                 U5MR_median = cond$median)
    
    data_plot_dt_year[, "NAME_1" := GADM]
    
    data_plot_dt <- rbind(data_plot_dt, data_plot_dt_year)
  }
  
  rowcount <- ceiling(length(years_vt)/5)
  
  pdf(paste0("Figures/Summary/", toupper(tmp.dir),
             "/", country, "_adm1_", time.model, "_",
             strata.model, "_", outcome,bench_str, "_medianmap.pdf"),
      width = 10, height = 3.5*rowcount)
  {
    data_plot_dt_df <- as.data.frame(data_plot_dt)
    print(SUMMER::mapPlot(data = data_plot_dt_df, 
                          "Year", is.long = T,
                          values = "U5MR_median", direction = -1,
                          geo = poly.adm1, ncol = 5,
                          by.data = "GADM",
                          legend.label = toupper(tmp.dir),
                          per1000 = TRUE,
                          by.geo = strsplit(poly.label.adm1, "\\$")[[1]][2]))
  }
  dev.off()
  
  pdf(paste0("Figures/Summary/", toupper(tmp.dir),
             "/", country, "_adm1_", time.model, "_",
             strata.model, "_", outcome,bench_str, "_medianmap_20002021.pdf"),
      width = 7, height = 7)
  {
    
    data_plot_dt_df <- as.data.frame(data_plot_dt)
    print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year %in% 
                                                   c(2000, 2010, 2015, 2021),],
                          is.long = T, 
                          variables = "Year", 
                          values = "U5MR_median",direction = -1,
                          geo = poly.adm1, ncol = 2,
                          legend.label = toupper(tmp.dir),
                          per1000 = TRUE,
                          by.data = "GADM",
                          by.geo = strsplit(poly.label.adm1, "\\$")[[1]][2]))
  }
  dev.off()
  
}


if(bench.model == "bench"){
  y_lims <- 1 + c(-1,1)*max(abs(1- range(adm1.nmr.benchmarks$ratio,
                                         na.rm = TRUE)), na.rm = TRUE)
  
  pdf(paste0("Figures/Summary/NMR/",
             country, "_Admin1_nmr_",
             time.model, "_", strata.model, "_benchmarks.pdf"))
  {
    plot(adm1.nmr.benchmarks$years,
         adm1.nmr.benchmarks$ratio,
         xlab = "Year", ylab = "(Unbenchmarked Median)/(IGME Median)",
         ylim = y_lims, main = "",
         type = "n")
    abline(h = 1)
    lines(adm1.nmr.benchmarks$years,
          adm1.nmr.benchmarks$ratio,
          lwd = 2, col = "firebrick")
  }
  dev.off()
  
  y_lims <- 1 + c(-1,1)*max(abs(1- range(adm1.u5.benchmarks$ratio,
                                         na.rm = TRUE)), na.rm = TRUE)
  
  pdf(paste0("Figures/Summary/U5MR/",
             country, "_Admin1_u5_",
             time.model, "_", strata.model, "_benchmarks.pdf"))
  {
    plot(adm1.u5.benchmarks$years,
         adm1.u5.benchmarks$ratio,
         xlab = "Year", ylab = "(Unbenchmarked Median)/(IGME Median)",
         ylim = y_lims, main = "",
         type = "n")
    abline(h = 1)
    lines(adm1.u5.benchmarks$years,
          adm1.u5.benchmarks$ratio,
          lwd = 2, col = "firebrick")
  }
  dev.off()
}
