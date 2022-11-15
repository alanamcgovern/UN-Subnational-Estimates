rm(list=ls())

# ENTER COUNTRY OF INTEREST AND FINAL ESTIMATE INFO -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Lesotho'

## Setup -----------------------------------------------
#### Load libraries and info ----------------------------------------------------------

# Libraries
options(gsubfn.engine = "R")
library(rgdal)
library(Rfast)
library(tidyverse)
library(RColorBrewer)

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

if(!dir.exists(paste0(res.dir,  '/Figures/Summary'))){
  dir.create(paste0(res.dir, '/Figures/Summary'))}
if(!dir.exists(paste0(res.dir,  '/Figures/Summary/U5MR'))){
  dir.create(paste0(res.dir,  '/Figures/Summary/U5MR'))}
if(!dir.exists(paste0(res.dir,'/Figures/Summary/NMR'))){
  dir.create(paste0(res.dir, '/Figures/Summary/NMR'))}

#### Load polygon files  ------------------------------------------------------
setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm0)) # load the national shape file
# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

if(exists('poly.layer.adm2')){
  poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                       layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions
}

# set coordinate reference system to be equal
if(exists("poly.adm2")){
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0(poly.path,'/', country, '_Amat.rda'))
load(paste0(poly.path,'/', country, '_Amat_Names.rda'))


#### Load IGME estimates ------------------------------------------------------
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
  igme.ests.u5$SD <- (igme.ests.u5$UPPER_BOUND - igme.ests.u5$LOWER_BOUND)/(2*1.645*1000)
  igme.ests.u5$LOWER_BOUND <- igme.ests.u5$OBS_VALUE - 1.96*igme.ests.u5$SD
  igme.ests.u5$UPPER_BOUND <- igme.ests.u5$OBS_VALUE + 1.96*igme.ests.u5$SD
  
  ## NMR
  igme.ests.nmr.raw <- read.csv('igme2022_nmr.csv')
  igme.ests.nmr <- igme.ests.nmr.raw[igme.ests.nmr.raw$iso==gadm.abbrev,]
  igme.ests.nmr <- data.frame(t(igme.ests.nmr[,10:ncol(igme.ests.nmr)]))
  names(igme.ests.nmr) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.nmr$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.nmr),'X')) - 0.5
  igme.ests.nmr <- igme.ests.nmr[igme.ests.nmr$year %in% 2000:2021,]
  rownames(igme.ests.nmr) <- NULL
  igme.ests.nmr$OBS_VALUE <- igme.ests.nmr$OBS_VALUE/1000
  igme.ests.nmr$SD <- (igme.ests.nmr$UPPER_BOUND - igme.ests.nmr$LOWER_BOUND)/(2*1.645*1000)
  igme.ests.nmr$LOWER_BOUND <- igme.ests.nmr$OBS_VALUE - 1.96*igme.ests.nmr$SD
  igme.ests.nmr$UPPER_BOUND <- igme.ests.nmr$OBS_VALUE + 1.96*igme.ests.nmr$SD
}

#### load admin1 and admin2 weights ####
load(paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm1_weights_u5.rda'))
if(exists('poly.adm2')){
  load(paste0(data.dir,'/worldpop/adm2_weights_u1.rda'))
  load(paste0(data.dir,'/worldpop/adm2_weights_u5.rda'))
}

#### Parameters ####

## MIGHT NEED TO BE CHANGED depending on what you fit
time.model <- c('rw2','ar1')[2]

load(paste0(data.dir, '/', country, '_cluster_dat_1frame.rda'), envir = .GlobalEnv)
end.year.1frame <- max(mod.dat$survey)

load(paste0(data.dir, '/', country, '_cluster_dat.rda'), envir = .GlobalEnv)
end.year <- max(mod.dat$survey)
end.proj.year <- 2021

plot.years <- 2000:end.proj.year
n_years <- length(plot.years)

if(((end.year-beg.year+1) %% 3)==0){
  beg.period.years <- seq(beg.year,end.year,3) 
  end.period.years <- beg.period.years + 2 
}else if(((end.year-beg.year+1) %% 3)==1){
  beg.period.years <- c(beg.year,beg.year+2,seq(beg.year+4,end.year,3))
  end.period.years <- c(beg.year+1,beg.year+3,seq(beg.year+6,end.year,3))
}else if(((end.year-beg.year+1) %% 3)==2){
  beg.period.years <- c(beg.year,seq(beg.year+2,end.year,3))
  end.period.years <- c(beg.year+1,seq(beg.year+4,end.year,3))
}

if(end.year>end.year.1frame & end.year>2018){
  beg.proj.years <- seq(end.year+1,2021,3)
}else{
  beg.proj.years <- seq(end.year+1,2020,3)
}
end.proj.years <- beg.proj.years+2
pane.years <- (c((end.period.years + beg.period.years)/2, (end.proj.years+beg.proj.years)/2))
pane.years <- pane.years[pane.years<=end.proj.year]
est.period.idx <- 1:length(beg.period.years)
pred.period.idx <- (length(beg.period.years)+1):(length(beg.period.years)+length(beg.proj.years))

##### function to organize posterior draws from BB8 ####

draw_1y_adm<-function(admin_draws, year_num,admin_vec, nsim=1000){
  
  # year_num: year of comparison
  # nsim: number of posterior draws
  # admin_vec: vector of admin index
  # admin_draws: posterior draws (as a list from SUMMER output)
  
  # prepare reference frame for draws 
  # ID corresponds to specific year, region
  draw_ID<-c(1:length(admin_draws))
  draw_year<-vector()
  draw_region<-vector()
  
  for( i in draw_ID){
    tmp_d<-admin_draws[[i]]
    draw_year[i]<-tmp_d$years
    draw_region[i]<-tmp_d$region
  }
  
  draw_ref<-data.frame(id=draw_ID,year=draw_year,
                       region=draw_region)
  
  draw_frame<-matrix( nrow = nsim, ncol = length(admin_vec))
  
  for(i in 1:length(admin_vec)){
    admin_i<-admin_vec[i]
    id_draw_set<-draw_ref[draw_ref$year==year_num&
                            draw_ref$region==admin_i,]$id 
    
    draw_set<-admin_draws[[id_draw_set]]$draws
    
    draw_frame[,i]<-draw_set
    #print(mean(r_frame[,c(admin_i)]))
  }
  
  colnames(draw_frame)<-admin_vec
  
  return(draw_frame)
}

#### prepare national level models ####
setwd(res.dir)

  ### national yearly smooth direct
{
  load(file = paste0('Direct/NMR/', country, '_res_natl_',time.model,'_yearly_nmr_SmoothedDirect.rda'))  
  load(file = paste0('Direct/U5MR/', country, '_res_natl_',time.model,'_yearly_u5_SmoothedDirect.rda'))
  
  natl.sd.est.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in% beg.year:end.proj.year, "median"]
  natl.sd.lower.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in% beg.year:end.proj.year, "lower"]
  natl.sd.upper.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in% beg.year:end.proj.year, "upper"]
  natl.sd.year.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in% beg.year:end.proj.year, "years"]
  
  natl.sd.est.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% beg.year:end.proj.year, "median"]
  natl.sd.lower.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% beg.year:end.proj.year, "lower"]
  natl.sd.upper.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% beg.year:end.proj.year, "upper"]
  natl.sd.year.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% beg.year:end.proj.year, "years"]
  
  natl.sd.frame<-data.frame()
  natl.sd.frame<-data.frame(lower_nmr=natl.sd.lower.nmr, median_nmr=natl.sd.est.nmr,upper_nmr=natl.sd.upper.nmr, 
                            lower_u5=natl.sd.lower.u5, median_u5=natl.sd.est.u5, upper_u5=natl.sd.upper.u5, 
                            method='natl.sd.yearly', years=natl.sd.year.u5)

}

  ### national betabinomial models
{
  if(file.exists(paste0('Betabinomial/NMR/', country, '_res_natl_unstrat_nmr.rda'))){
    load(file = paste0('Betabinomial/NMR/', country, '_res_natl_unstrat_nmr.rda'))}
  if(file.exists(paste0('Betabinomial/NMR/', country, '_res_natl_strat_nmr.rda'))){
    load(file = paste0('Betabinomial/NMR/', country, '_res_natl_strat_nmr.rda'))}
  if(file.exists(paste0('Betabinomial/NMR/', country, '_res_natl_unstrat_nmr_allsurveys.rda'))){
    load(file = paste0('Betabinomial/NMR/', country, '_res_natl_unstrat_nmr_allsurveys.rda'))}
  if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_natl_unstrat_u5.rda'))){
    load(file = paste0('Betabinomial/U5MR/', country, '_res_natl_unstrat_u5.rda'))}  
  if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_natl_strat_u5.rda'))){
    load(file = paste0('Betabinomial/U5MR/', country, '_res_natl_strat_u5.rda'))}  
  if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_natl_unstrat_u5_allsurveys.rda'))){
    load(file = paste0('Betabinomial/U5MR/', country, '_res_natl_unstrat_u5_allsurveys.rda'))}  
  
  if(exists('bb.res.natl.unstrat.nmr') & exists('bb.res.natl.unstrat.u5')){
    natl.bb.unstrat.frame <- data.frame(lower_nmr = bb.res.natl.unstrat.nmr$overall$lower, median_nmr = bb.res.natl.unstrat.nmr$overall$median, upper_nmr = bb.res.natl.unstrat.nmr$overall$upper,
                                        lower_u5 = bb.res.natl.unstrat.u5$overall$lower, median_u5 = bb.res.natl.unstrat.u5$overall$median, upper_u5 = bb.res.natl.unstrat.u5$overall$upper,
                                        method='natl.bb.unstrat',years=bb.res.natl.unstrat.nmr$overall$years)
  }else if(exists('bb.res.natl.unstrat.nmr')){
    natl.bb.unstrat.frame <- data.frame(lower_nmr = bb.res.natl.unstrat.nmr$overall$lower, median_nmr = bb.res.natl.unstrat.nmr$overall$median, upper_nmr = bb.res.natl.unstrat.nmr$overall$upper,
                                        lower_u5 = NA, median_u5 = NA, upper_u5 = NA,
                                        method='natl.bb.unstrat',years=bb.res.natl.unstrat.nmr$overall$years)
  }else if(exists('bb.res.natl.unstrat.u5')){
    natl.bb.unstrat.frame <- data.frame(lower_nmr =NA, median_nmr = NA, upper_nmr = NA,
                                        lower_u5 = bb.res.natl.unstrat.u5$overall$lower, median_u5 = bb.res.natl.unstrat.u5$overall$median, upper_u5 = bb.res.natl.unstrat.u5$overall$upper,
                                        method='natl.bb.unstrat',years=bb.res.natl.unstrat.u5$overall$years)
  }
  
  if(exists('bb.res.natl.strat.nmr') & exists('bb.res.natl.strat.u5')){
    natl.bb.strat.frame <- data.frame(lower_nmr = bb.res.natl.strat.nmr$overall$lower, median_nmr = bb.res.natl.strat.nmr$overall$median, upper_nmr = bb.res.natl.strat.nmr$overall$upper,
                                        lower_u5 = bb.res.natl.strat.u5$overall$lower, median_u5 = bb.res.natl.strat.u5$overall$median, upper_u5 = bb.res.natl.strat.u5$overall$upper,
                                        method='natl.bb.strat',years=bb.res.natl.strat.nmr$overall$years)
  }else if(exists('bb.res.natl.strat.nmr')){
    natl.bb.strat.frame <- data.frame(lower_nmr = bb.res.natl.strat.nmr$overall$lower, median_nmr = bb.res.natl.strat.nmr$overall$median, upper_nmr = bb.res.natl.strat.nmr$overall$upper,
                                        lower_u5 = NA, median_u5 = NA, upper_u5 = NA,
                                        method='natl.bb.strat',years=bb.res.natl.strat.nmr$overall$years)
  }else if(exists('bb.res.natl.strat.u5')){
    natl.bb.strat.frame <- data.frame(lower_nmr =NA, median_nmr = NA, upper_nmr = NA,
                                        lower_u5 = bb.res.natl.strat.u5$overall$lower, median_u5 = bb.res.natl.strat.u5$overall$median, upper_u5 = bb.res.natl.strat.u5$overall$upper,
                                        method='natl.bb.strat',years=bb.res.natl.strat.u5$overall$years)
  }
  
  if(exists('bb.res.natl.unstrat.nmr.allsurveys') & exists('bb.res.natl.unstrat.u5.allsurveys')){
    natl.bb.unstrat.allsurveys.frame <- data.frame(lower_nmr = bb.res.natl.unstrat.nmr.allsurveys$overall$lower, median_nmr = bb.res.natl.unstrat.nmr.allsurveys$overall$median, upper_nmr = bb.res.natl.unstrat.nmr.allsurveys$overall$upper,
                                        lower_u5 = bb.res.natl.unstrat.u5.allsurveys$overall$lower, median_u5 = bb.res.natl.unstrat.u5.allsurveys$overall$median, upper_u5 = bb.res.natl.unstrat.u5.allsurveys$overall$upper,
                                        method='natl.bb.unstrat.allsurveys',years=bb.res.natl.unstrat.nmr.allsurveys$overall$years)
  }else if(exists('bb.res.natl.unstrat.nmr.allsurveys')){
    natl.bb.unstrat.allsurveys.frame <- data.frame(lower_nmr = bb.res.natl.unstrat.nmr.allsurveys$overall$lower, median_nmr = bb.res.natl.unstrat.nmr.allsurveys$overall$median, upper_nmr = bb.res.natl.unstrat.nmr.allsurveys$overall$upper,
                                        lower_u5 = NA, median_u5 = NA, upper_u5 = NA,
                                        method='natl.bb.unstrat.allsurveys',years=bb.res.natl.unstrat.nmr.allsurveys$overall$years)
  }else if(exists('bb.res.natl.unstrat.u5.allsurveys')){
    natl.bb.unstrat.allsurveys.frame <- data.frame(lower_nmr =NA, median_nmr = NA, upper_nmr = NA,
                                        lower_u5 = bb.res.natl.unstrat.u5.allsurveys$overall$lower, median_u5 = bb.res.natl.unstrat.u5.allsurveys$overall$median, upper_u5 = bb.res.natl.unstrat.u5.allsurveys$overall$upper,
                                        method='natl.bb.unstrat.allsurveys',years=bb.res.natl.unstrat.u5.allsurveys$overall$years)
  }
}

#### prepare admin1 level models ####

  ### smooth direct admin1 3-year window
  {
  load(file = paste0('Direct/NMR/', country, '_res_admin1_',time.model,'_nmr_SmoothedDirect.rda'))
  admin1.sd.nmr <- res.admin1.nmr
  load(file = paste0('Direct/U5MR/', country, '_res_admin1_',time.model,'_u5_SmoothedDirect.rda'))  
  admin1.sd.u5 <- res.admin1.u5
  
  sd.adm1.to.natl.frame = matrix(NA, nrow = length(pane.years), ncol =  6)
  
  for (i in 1:length(pane.years)){
    year = round((pane.years)[i])
    
    adm1.pop.nmr <- weight.adm1.u1[weight.adm1.u1$years==year,]
    sd.nmr.tmp <- admin1.sd.nmr[admin1.sd.nmr$years.num==sort(unique(admin1.sd.nmr$years.num))[i],]
    sd.nmr.tmp <- merge(sd.nmr.tmp,adm1.pop.nmr,by='region')[,c('region','years.x','lower','median','upper','proportion')]
    sd.nmr.tmp$wt.lower <- sd.nmr.tmp$lower * sd.nmr.tmp$proportion
    sd.nmr.tmp$wt.median <- sd.nmr.tmp$median * sd.nmr.tmp$proportion
    sd.nmr.tmp$wt.upper <- sd.nmr.tmp$upper * sd.nmr.tmp$proportion
    
    adm1.pop.u5 <- weight.adm1.u5[weight.adm1.u5$years==year,]
    sd.u5.tmp <- admin1.sd.u5[admin1.sd.u5$years.num==sort(unique(admin1.sd.nmr$years.num))[i],]
    sd.u5.tmp <- merge(sd.u5.tmp,adm1.pop.u5,by='region')[,c('region','years.x','lower','median','upper','proportion')]
    sd.u5.tmp$wt.lower <- sd.u5.tmp$lower * sd.u5.tmp$proportion
    sd.u5.tmp$wt.median <- sd.u5.tmp$median * sd.u5.tmp$proportion
    sd.u5.tmp$wt.upper <- sd.u5.tmp$upper * sd.u5.tmp$proportion
    
    sd.adm1.to.natl.frame[i, ] = c(sum(sd.nmr.tmp$wt.lower),sum(sd.nmr.tmp$wt.median),sum(sd.nmr.tmp$wt.upper),sum(sd.u5.tmp$wt.lower),sum(sd.u5.tmp$wt.median),sum(sd.u5.tmp$wt.upper))
  }
  
  sd.adm1.to.natl.frame<-as.data.frame(sd.adm1.to.natl.frame)
  colnames(sd.adm1.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
  sd.adm1.to.natl.frame$method <- "aggre.sd.adm1"
  sd.adm1.to.natl.frame$years = pane.years
  sd.adm1.to.natl.frame <- sd.adm1.to.natl.frame[sd.adm1.to.natl.frame$years<=2021,]
}
  ### smooth direct admin1 yearly
  {
  load(file = paste0('Direct/NMR/', country, '_res_admin1_',time.model,'_nmr_SmoothedDirect_yearly.rda'))
  admin1.sd.yearly.nmr <- sd.admin1.yearly.nmr
  load(file = paste0('Direct/U5MR/', country, '_res_admin1_',time.model,'_u5_SmoothedDirect_yearly.rda'))  
  admin1.sd.yearly.u5 <- sd.admin1.yearly.u5
  
  sd.adm1.yl.to.natl.frame = matrix(NA, nrow = n_years, ncol =  6)
  
  for (i in 1:n_years){
    year = (beg.year:end.proj.year)[i]
    
    adm1.pop.nmr <- weight.adm1.u1[weight.adm1.u1$years==year,]
    sd.nmr.tmp <- admin1.sd.yearly.nmr[admin1.sd.yearly.nmr$years.num==year,]
    sd.nmr.tmp <- merge(sd.nmr.tmp,adm1.pop.nmr,by='region')[,c('region','years.x','lower','median','upper','proportion')]
    sd.nmr.tmp$wt.lower <- sd.nmr.tmp$lower * sd.nmr.tmp$proportion
    sd.nmr.tmp$wt.median <- sd.nmr.tmp$median * sd.nmr.tmp$proportion
    sd.nmr.tmp$wt.upper <- sd.nmr.tmp$upper * sd.nmr.tmp$proportion
    
    adm1.pop.u5 <- weight.adm1.u5[weight.adm1.u5$years==year,]
    sd.u5.tmp <- admin1.sd.yearly.u5[admin1.sd.yearly.u5$years.num==year,]
    sd.u5.tmp <- merge(sd.u5.tmp,adm1.pop.u5,by='region')[,c('region','years.x','lower','median','upper','proportion')]
    sd.u5.tmp$wt.lower <- sd.u5.tmp$lower * sd.u5.tmp$proportion
    sd.u5.tmp$wt.median <- sd.u5.tmp$median * sd.u5.tmp$proportion
    sd.u5.tmp$wt.upper <- sd.u5.tmp$upper * sd.u5.tmp$proportion
    
    sd.adm1.yl.to.natl.frame[i, ] = c(sum(sd.nmr.tmp$wt.lower),sum(sd.nmr.tmp$wt.median),sum(sd.nmr.tmp$wt.upper),sum(sd.u5.tmp$wt.lower),sum(sd.u5.tmp$wt.median),sum(sd.u5.tmp$wt.upper))
  }
  
  sd.adm1.yl.to.natl.frame<-as.data.frame(sd.adm1.yl.to.natl.frame)
  colnames(sd.adm1.yl.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
  sd.adm1.yl.to.natl.frame$method <- "aggre.sd.yearly.adm1"
  sd.adm1.yl.to.natl.frame$years = beg.year:end.proj.year
}
  ### BB8 admin1 unstratified
  {
  
  if(file.exists(paste0('Betabinomial/NMR/', country, '_res_adm1_unstrat_nmr.rda'))){
    load(file = paste0('Betabinomial/NMR/', country, '_res_adm1_unstrat_nmr.rda'))
    res.unstrat.admin1.nmr <- bb.res.adm1.unstrat.nmr
    admin1.unstrat.nmr.BB8<-res.unstrat.admin1.nmr$overall}
  if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_adm1_unstrat_u5.rda'))){
    load(file = paste0('Betabinomial/U5MR/', country, '_res_adm1_unstrat_u5.rda'))
    res.unstrat.admin1.u5 <- bb.res.adm1.unstrat.u5
    admin1.unstrat.u5.BB8<-res.unstrat.admin1.u5$overall} 
  
  if(exists('admin1.unstrat.nmr.BB8') | exists('admin1.unstrat.u5.BB8')){
    
    BB8.adm1.unstrat.to.natl.frame <- matrix(NA, nrow = n_years, ncol =  6)
    for (i in 1:n_years){
      year = (beg.year:end.proj.year)[i]
      
      if(exists('admin1.unstrat.nmr.BB8')){
        adm1.pop.nmr <- weight.adm1.u1[weight.adm1.u1$years==year,]
        admin1.unstrat.nmr.BB8.draw<-draw_1y_adm(admin_draws=res.unstrat.admin1.nmr$draws.est.overall,
                                               year_num=year,
                                               admin_vec=admin1.names$Internal)
        natl.tmp.nmr <- admin1.unstrat.nmr.BB8.draw %*% adm1.pop.nmr$proportion
        BB8.adm1.unstrat.to.natl.frame[i, 1:3] = c(quantile(natl.tmp.nmr, probs = c(0.025, 0.5, 0.975)))
      }
      
      if(exists('admin1.unstrat.u5.BB8')){
        adm1.pop.u5 <- weight.adm1.u5[weight.adm1.u5$years==year,]
        admin1.unstrat.u5.BB8.draw<-draw_1y_adm(admin_draws=res.unstrat.admin1.u5$draws.est.overall,
                                              year_num=year,
                                              admin_vec=admin1.names$Internal)
        natl.tmp.u5 <- admin1.unstrat.u5.BB8.draw %*% adm1.pop.u5$proportion
        BB8.adm1.unstrat.to.natl.frame[i, 4:6] = c(quantile(natl.tmp.u5, probs = c(0.025, 0.5, 0.975)))
      }
      
    }
    
    BB8.adm1.unstrat.to.natl.frame<-as.data.frame(BB8.adm1.unstrat.to.natl.frame)
    colnames(BB8.adm1.unstrat.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
    BB8.adm1.unstrat.to.natl.frame$method <- "aggre.adm1.unstrat.BB8"
    BB8.adm1.unstrat.to.natl.frame$years = beg.year:end.proj.year
  }
  }
  ### BB8 admin1 unstratified, all surveys
  {
  
  if(file.exists(paste0('Betabinomial/NMR/', country, '_res_adm1_unstrat_nmr_allsurveys.rda'))){
    load(file = paste0('Betabinomial/NMR/', country, '_res_adm1_unstrat_nmr_allsurveys.rda'))
    res.unstrat.admin1.nmr.allsurveys <- bb.res.adm1.unstrat.nmr.allsurveys
    admin1.unstrat.nmr.allsurveys.BB8<-res.unstrat.admin1.nmr.allsurveys$overall}
  if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_adm1_unstrat_u5_allsurveys.rda'))){
    load(file = paste0('Betabinomial/U5MR/', country, '_res_adm1_unstrat_u5_allsurveys.rda'))
    res.unstrat.admin1.u5.allsurveys <- bb.res.adm1.unstrat.u5.allsurveys
    admin1.unstrat.u5.allsurveys.BB8<-res.unstrat.admin1.u5.allsurveys$overall} 
  
  if(exists('admin1.unstrat.nmr.allsurveys.BB8') | exists('admin1.unstrat.u5.allsurveys.BB8')){
    
    BB8.adm1.unstrat.allsurveys.to.natl.frame <- matrix(NA, nrow = n_years, ncol =  6)
    for (i in 1:n_years){
      year = (beg.year:end.proj.year)[i]
      
      if(exists('admin1.unstrat.nmr.allsurveys.BB8')){
        adm1.pop.nmr <- weight.adm1.u1[weight.adm1.u1$years==year,]
        admin1.unstrat.nmr.allsurveys.BB8.draw<-draw_1y_adm(admin_draws=res.unstrat.admin1.nmr.allsurveys$draws.est.overall,
                                                 year_num=year,
                                                 admin_vec=admin1.names$Internal)
        natl.tmp.nmr <- admin1.unstrat.nmr.allsurveys.BB8.draw %*% adm1.pop.nmr$proportion
        BB8.adm1.unstrat.allsurveys.to.natl.frame[i, 1:3] = c(quantile(natl.tmp.nmr, probs = c(0.025, 0.5, 0.975)))
      }
      
      if(exists('admin1.unstrat.u5.allsurveys.BB8')){
        adm1.pop.u5 <- weight.adm1.u5[weight.adm1.u5$years==year,]
        admin1.unstrat.u5.allsurveys.BB8.draw<-draw_1y_adm(admin_draws=res.unstrat.admin1.u5.allsurveys$draws.est.overall,
                                                year_num=year,
                                                admin_vec=admin1.names$Internal)
        natl.tmp.u5 <- admin1.unstrat.u5.allsurveys.BB8.draw %*% adm1.pop.u5$proportion
        BB8.adm1.unstrat.allsurveys.to.natl.frame[i, 4:6] = c(quantile(natl.tmp.u5, probs = c(0.025, 0.5, 0.975)))
      }
      
    }
    
    BB8.adm1.unstrat.allsurveys.to.natl.frame<-as.data.frame(BB8.adm1.unstrat.allsurveys.to.natl.frame)
    colnames(BB8.adm1.unstrat.allsurveys.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
    BB8.adm1.unstrat.allsurveys.to.natl.frame$method <- "aggre.adm1.unstrat.allsurveys.BB8"
    BB8.adm1.unstrat.allsurveys.to.natl.frame$years = beg.year:end.proj.year
  }
}
  ### BB8 admin1 stratified
  {
  
  if(file.exists(paste0('Betabinomial/NMR/', country, '_res_adm1_strat_nmr.rda'))){
    load(file = paste0('Betabinomial/NMR/', country, '_res_adm1_strat_nmr.rda'))
    res.strat.admin1.nmr <- bb.res.adm1.strat.nmr
    admin1.strat.nmr.BB8<-res.strat.admin1.nmr$overall}
  if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_adm1_strat_u5.rda'))){
    load(file = paste0('Betabinomial/U5MR/', country, '_res_adm1_strat_u5.rda'))
    res.strat.admin1.u5 <- bb.res.adm1.strat.u5
    admin1.strat.u5.BB8<-res.strat.admin1.u5$overall} 
  
  if(exists('admin1.strat.nmr.BB8') | exists('admin1.strat.u5.BB8')){
  
  BB8.adm1.strat.to.natl.frame <- matrix(NA, nrow = n_years, ncol =  6)
  for (i in 1: n_years){
    year = (beg.year:end.proj.year)[i]
    
    if(exists('admin1.strat.nmr.BB8')){
    adm1.pop.nmr <- weight.adm1.u1[weight.adm1.u1$years==year,]
    admin1.strat.nmr.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin1.nmr$draws.est.overall,
                                           year_num=year,
                                           admin_vec=admin1.names$Internal)
    natl.tmp.nmr <- admin1.strat.nmr.BB8.draw %*% adm1.pop.nmr$proportion
    BB8.adm1.strat.to.natl.frame[i, 1:3] = c(quantile(natl.tmp.nmr, probs = c(0.025, 0.5, 0.975)))
    }
    
    if(exists('admin1.strat.u5.BB8')){
    adm1.pop.u5 <- weight.adm1.u5[weight.adm1.u5$years==year,]
    admin1.strat.u5.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin1.u5$draws.est.overall,
                                          year_num=year,
                                          admin_vec=admin1.names$Internal)
    natl.tmp.u5 <- admin1.strat.u5.BB8.draw %*% adm1.pop.u5$proportion
    BB8.adm1.strat.to.natl.frame[i, 4:6] = c(quantile(natl.tmp.u5, probs = c(0.025, 0.5, 0.975)))
    }

  }
  
  BB8.adm1.strat.to.natl.frame<-as.data.frame(BB8.adm1.strat.to.natl.frame)
  colnames(BB8.adm1.strat.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
  BB8.adm1.strat.to.natl.frame$method <- "aggre.adm1.strat.BB8"
  BB8.adm1.strat.to.natl.frame$years = beg.year:end.proj.year
  }
}

#### prepare admin2 level models ####
if(exists('poly.adm2')){
  ### smooth direct admin2 3-year window
  {
    load(file = paste0('Direct/NMR/', country, '_res_admin2_',time.model,'_nmr_SmoothedDirect.rda'))
    admin2.sd.nmr <- res.admin2.nmr
    load(file = paste0('Direct/U5MR/', country, '_res_admin2_',time.model,'_u5_SmoothedDirect.rda'))  
    admin2.sd.u5 <- res.admin2.u5
    
    sd.adm2.to.natl.frame = matrix(NA, nrow = length(pane.years), ncol =  6)
    
    for (i in 1:length(pane.years)){
      year = round((pane.years)[i])
      
      adm2.pop.nmr <- weight.adm2.u1[weight.adm2.u1$years==year,]
      sd.nmr.tmp <- admin2.sd.nmr[admin2.sd.nmr$years.num==sort(unique(admin2.sd.nmr$years.num))[i],]
      sd.nmr.tmp <- merge(sd.nmr.tmp,adm2.pop.nmr,by='region')[,c('region','years.x','lower','median','upper','proportion')]
      sd.nmr.tmp$wt.lower <- sd.nmr.tmp$lower * sd.nmr.tmp$proportion
      sd.nmr.tmp$wt.median <- sd.nmr.tmp$median * sd.nmr.tmp$proportion
      sd.nmr.tmp$wt.upper <- sd.nmr.tmp$upper * sd.nmr.tmp$proportion
      
      adm2.pop.u5 <- weight.adm2.u5[weight.adm2.u5$years==year,]
      sd.u5.tmp <- admin2.sd.u5[admin2.sd.u5$years.num==sort(unique(admin2.sd.nmr$years.num))[i],]
      sd.u5.tmp <- merge(sd.u5.tmp,adm2.pop.u5,by='region')[,c('region','years.x','lower','median','upper','proportion')]
      sd.u5.tmp$wt.lower <- sd.u5.tmp$lower * sd.u5.tmp$proportion
      sd.u5.tmp$wt.median <- sd.u5.tmp$median * sd.u5.tmp$proportion
      sd.u5.tmp$wt.upper <- sd.u5.tmp$upper * sd.u5.tmp$proportion
      
      sd.adm2.to.natl.frame[i, ] = c(sum(sd.nmr.tmp$wt.lower),sum(sd.nmr.tmp$wt.median),sum(sd.nmr.tmp$wt.upper),sum(sd.u5.tmp$wt.lower),sum(sd.u5.tmp$wt.median),sum(sd.u5.tmp$wt.upper))
    }
    
    sd.adm2.to.natl.frame<-as.data.frame(sd.adm2.to.natl.frame)
    colnames(sd.adm2.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
    sd.adm2.to.natl.frame$method <- "aggre.sd.adm2"
    sd.adm2.to.natl.frame$years = pane.years
    sd.adm2.to.natl.frame <- sd.adm2.to.natl.frame[sd.adm2.to.natl.frame$years<=2021,]
  }
  ### smooth direct admin2 yearly
  {
    if(file.exists(paste0('Direct/NMR/', country, '_res_admin2_',time.model,'_nmr_SmoothedDirect_yearly.rda'))){
      load(file = paste0('Direct/NMR/', country, '_res_admin2_',time.model,'_nmr_SmoothedDirect_yearly.rda'))
      admin2.sd.yearly.nmr <- sd.admin2.yearly.nmr}
    if(file.exists(paste0('Direct/U5MR/', country, '_res_admin2_',time.model,'_u5_SmoothedDirect_yearly.rda'))){
      load(file = paste0('Direct/U5MR/', country, '_res_admin2_',time.model,'_u5_SmoothedDirect_yearly.rda'))  
      admin2.sd.yearly.u5 <- sd.admin2.yearly.u5}
    
    if(exists('admin2.sd.yearly.nmr') | exists('admin2.sd.yearly.u5')){
    sd.adm2.yl.to.natl.frame = matrix(NA, nrow = n_years, ncol =  6)
    
    for (i in 1:n_years){
      year = (beg.year:end.proj.year)[i]
      
      if(exists('admin2.sd.yearly.nmr')){
      adm2.pop.nmr <- weight.adm2.u1[weight.adm2.u1$years==year,]
      sd.nmr.tmp <- admin2.sd.yearly.nmr[admin2.sd.yearly.nmr$years.num==year,]
      sd.nmr.tmp <- merge(sd.nmr.tmp,adm2.pop.nmr,by='region')[,c('region','years.x','lower','median','upper','proportion')]
      sd.nmr.tmp$wt.lower <- sd.nmr.tmp$lower * sd.nmr.tmp$proportion
      sd.nmr.tmp$wt.median <- sd.nmr.tmp$median * sd.nmr.tmp$proportion
      sd.nmr.tmp$wt.upper <- sd.nmr.tmp$upper * sd.nmr.tmp$proportion
      sd.adm2.yl.to.natl.frame[i, 1:3] <- c(sum(sd.nmr.tmp$wt.lower),sum(sd.nmr.tmp$wt.median),sum(sd.nmr.tmp$wt.upper))
      }
      
      if(exists('admin2.sd.yearly.u5')){
      adm2.pop.u5 <- weight.adm2.u5[weight.adm2.u5$years==year,]
      sd.u5.tmp <- admin2.sd.yearly.u5[admin2.sd.yearly.u5$years.num==year,]
      sd.u5.tmp <- merge(sd.u5.tmp,adm2.pop.u5,by='region')[,c('region','years.x','lower','median','upper','proportion')]
      sd.u5.tmp$wt.lower <- sd.u5.tmp$lower * sd.u5.tmp$proportion
      sd.u5.tmp$wt.median <- sd.u5.tmp$median * sd.u5.tmp$proportion
      sd.u5.tmp$wt.upper <- sd.u5.tmp$upper * sd.u5.tmp$proportion
      
      sd.adm2.yl.to.natl.frame[i, 4:6] <- c(sum(sd.u5.tmp$wt.lower),sum(sd.u5.tmp$wt.median),sum(sd.u5.tmp$wt.upper))
      }
    }
    
    sd.adm2.yl.to.natl.frame<-as.data.frame(sd.adm2.yl.to.natl.frame)
    colnames(sd.adm2.yl.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
    sd.adm2.yl.to.natl.frame$method <- "aggre.sd.yearly.adm2"
    sd.adm2.yl.to.natl.frame$years = beg.year:end.proj.year
    }
  }
  ### BB8 admin2 unstratified
  {
    
    if(file.exists(paste0('Betabinomial/NMR/', country, '_res_adm2_unstrat_nmr.rda'))){
      load(file = paste0('Betabinomial/NMR/', country, '_res_adm2_unstrat_nmr.rda'))
      res.unstrat.admin2.nmr <- bb.res.adm2.unstrat.nmr
      admin2.unstrat.nmr.BB8<-res.unstrat.admin2.nmr$overall}
    if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_adm2_unstrat_u5.rda'))){
      load(file = paste0('Betabinomial/U5MR/', country, '_res_adm2_unstrat_u5.rda'))
      res.unstrat.admin2.u5 <- bb.res.adm2.unstrat.u5
      admin2.unstrat.u5.BB8<-res.unstrat.admin2.u5$overall} 
    
    if(exists('admin2.unstrat.nmr.BB8') | exists('admin2.unstrat.u5.BB8')){
      
      BB8.adm2.unstrat.to.natl.frame <- matrix(NA, nrow = n_years, ncol =  6)
      for (i in 1: n_years){
        year = (beg.year:end.proj.year)[i]
        
        if(exists('admin2.unstrat.nmr.BB8')){
          adm2.pop.nmr <- weight.adm2.u1[weight.adm2.u1$years==year,]
          admin2.unstrat.nmr.BB8.draw<-draw_1y_adm(admin_draws=res.unstrat.admin2.nmr$draws.est.overall,
                                                 year_num=year,
                                                 admin_vec=admin2.names$Internal)
          natl.tmp.nmr <- admin2.unstrat.nmr.BB8.draw %*% adm2.pop.nmr$proportion
          BB8.adm2.unstrat.to.natl.frame[i, 1:3] = c(quantile(natl.tmp.nmr, probs = c(0.025, 0.5, 0.975)))
        }
        
        if(exists('admin2.unstrat.u5.BB8')){
          adm2.pop.u5 <- weight.adm2.u5[weight.adm2.u5$years==year,]
          admin2.unstrat.u5.BB8.draw<-draw_1y_adm(admin_draws=res.unstrat.admin2.u5$draws.est.overall,
                                                year_num=year,
                                                admin_vec=admin2.names$Internal)
          natl.tmp.u5 <- admin2.unstrat.u5.BB8.draw %*% adm2.pop.u5$proportion
          BB8.adm2.unstrat.to.natl.frame[i, 4:6] = c(quantile(natl.tmp.u5, probs = c(0.025, 0.5, 0.975)))
        }
        
      }
      
      BB8.adm2.unstrat.to.natl.frame<-as.data.frame(BB8.adm2.unstrat.to.natl.frame)
      colnames(BB8.adm2.unstrat.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
      BB8.adm2.unstrat.to.natl.frame$method <- "aggre.adm2.unstrat.BB8"
      BB8.adm2.unstrat.to.natl.frame$years = beg.year:end.proj.year
    }
  }
  ### BB8 admin2 unstratified, all surveys
  {
    
    if(file.exists(paste0('Betabinomial/NMR/', country, '_res_adm2_unstrat_nmr_allsurveys.rda'))){
      load(file = paste0('Betabinomial/NMR/', country, '_res_adm2_unstrat_nmr_allsurveys.rda'))
      res.unstrat.admin2.nmr.allsurveys <- bb.res.adm2.unstrat.nmr.allsurveys
      admin2.unstrat.nmr.allsurveys.BB8<-res.unstrat.admin2.nmr.allsurveys$overall}
    if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_adm2_unstrat_u5_allsurveys.rda'))){
      load(file = paste0('Betabinomial/U5MR/', country, '_res_adm2_unstrat_u5_allsurveys.rda'))
      res.unstrat.admin2.u5.allsurveys <- bb.res.adm2.unstrat.u5.allsurveys
      admin2.unstrat.u5.allsurveys.BB8<-res.unstrat.admin2.u5.allsurveys$overall} 
    
    if(exists('admin2.unstrat.nmr.allsurveys.BB8') | exists('admin2.unstrat.u5.allsurveys.BB8')){
      
      BB8.adm2.unstrat.allsurveys.to.natl.frame <- matrix(NA, nrow = n_years, ncol =  6)
      for (i in 1: n_years){
        year = (beg.year:end.proj.year)[i]
        
        if(exists('admin2.unstrat.nmr.allsurveys.BB8')){
          adm2.pop.nmr <- weight.adm2.u1[weight.adm2.u1$years==year,]
          admin2.unstrat.nmr.allsurveys.BB8.draw<-draw_1y_adm(admin_draws=res.unstrat.admin2.nmr.allsurveys$draws.est.overall,
                                                   year_num=year,
                                                   admin_vec=admin2.names$Internal)
          natl.tmp.nmr <- admin2.unstrat.nmr.allsurveys.BB8.draw %*% adm2.pop.nmr$proportion
          BB8.adm2.unstrat.allsurveys.to.natl.frame[i, 1:3] = c(quantile(natl.tmp.nmr, probs = c(0.025, 0.5, 0.975)))
        }
        
        if(exists('admin2.unstrat.u5.allsurveys.BB8')){
          adm2.pop.u5 <- weight.adm2.u5[weight.adm2.u5$years==year,]
          admin2.unstrat.u5.allsurveys.BB8.draw<-draw_1y_adm(admin_draws=res.unstrat.admin2.u5.allsurveys$draws.est.overall,
                                                  year_num=year,
                                                  admin_vec=admin2.names$Internal)
          natl.tmp.u5 <- admin2.unstrat.u5.allsurveys.BB8.draw %*% adm2.pop.u5$proportion
          BB8.adm2.unstrat.allsurveys.to.natl.frame[i, 4:6] = c(quantile(natl.tmp.u5, probs = c(0.025, 0.5, 0.975)))
        }
        
      }
      
      BB8.adm2.unstrat.allsurveys.to.natl.frame<-as.data.frame(BB8.adm2.unstrat.allsurveys.to.natl.frame)
      colnames(BB8.adm2.unstrat.allsurveys.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
      BB8.adm2.unstrat.allsurveys.to.natl.frame$method <- "aggre.adm2.unstrat.allsurveys.BB8"
      BB8.adm2.unstrat.allsurveys.to.natl.frame$years = beg.year:end.proj.year
    }
  }
  ### BB8 admin2 stratified
  {
    
    if(file.exists(paste0('Betabinomial/NMR/', country, '_res_adm2_strat_nmr.rda'))){
      load(file = paste0('Betabinomial/NMR/', country, '_res_adm2_strat_nmr.rda'))
      res.strat.admin2.nmr <- bb.res.adm2.strat.nmr
      admin2.strat.nmr.BB8<-res.strat.admin2.nmr$overall}
    if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_adm2_strat_u5.rda'))){
      load(file = paste0('Betabinomial/U5MR/', country, '_res_adm2_strat_u5.rda'))
      res.strat.admin2.u5 <- bb.res.adm2.strat.u5
      admin2.strat.u5.BB8<-res.strat.admin2.u5$overall} 
    
    if(exists('admin2.strat.nmr.BB8') | exists('admin2.strat.u5.BB8')){
      
      BB8.adm2.strat.to.natl.frame <- matrix(NA, nrow = n_years, ncol =  6)
      for (i in 1: n_years){
        year = (beg.year:end.proj.year)[i]
        
        if(exists('admin2.strat.nmr.BB8')){
          adm2.pop.nmr <- weight.adm2.u1[weight.adm2.u1$years==year,]
          admin2.strat.nmr.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin2.nmr$draws.est.overall,
                                                 year_num=year,
                                                 admin_vec=admin2.names$Internal)
          natl.tmp.nmr <- admin2.strat.nmr.BB8.draw %*% adm2.pop.nmr$proportion
          BB8.adm2.strat.to.natl.frame[i, 1:3] = c(quantile(natl.tmp.nmr, probs = c(0.025, 0.5, 0.975)))
        }
        
        if(exists('admin2.strat.u5.BB8')){
          adm2.pop.u5 <- weight.adm2.u5[weight.adm2.u5$years==year,]
          admin2.strat.u5.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin2.u5$draws.est.overall,
                                                year_num=year,
                                                admin_vec=admin2.names$Internal)
          natl.tmp.u5 <- admin2.strat.u5.BB8.draw %*% adm2.pop.u5$proportion
          BB8.adm2.strat.to.natl.frame[i, 4:6] = c(quantile(natl.tmp.u5, probs = c(0.025, 0.5, 0.975)))
        }
        
      }
      
      BB8.adm2.strat.to.natl.frame<-as.data.frame(BB8.adm2.strat.to.natl.frame)
      colnames(BB8.adm2.strat.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
      BB8.adm2.strat.to.natl.frame$method <- "aggre.adm2.strat.BB8"
      BB8.adm2.strat.to.natl.frame$years = beg.year:end.proj.year
    }
  }
}

#### prepare IGME estimates ####
  igme.frame <- as.data.frame(cbind(igme.ests.nmr$LOWER_BOUND,igme.ests.nmr$OBS_VALUE,igme.ests.nmr$UPPER_BOUND,
                                    igme.ests.u5$LOWER_BOUND,igme.ests.u5$OBS_VALUE,igme.ests.u5$UPPER_BOUND))
  colnames(igme.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
  igme.frame$method <- "igme"
  igme.frame$years <- beg.year:max(igme.ests.nmr$year)
  
#### final plot ####
  
  methods <- c("natl.sd.frame","sd.adm1.to.natl.frame","sd.adm1.yl.to.natl.frame","sd.adm2.to.natl.frame","sd.adm2.yl.to.natl.frame",
               'natl.bb.unstrat.frame','natl.bb.strat.frame','natl.bb.unstrat.allsurveys.frame',
               "BB8.adm1.unstrat.to.natl.frame","BB8.adm1.strat.to.natl.frame",'BB8.adm1.unstrat.allsurveys.to.natl.frame',
               "BB8.adm2.unstrat.to.natl.frame","BB8.adm2.strat.to.natl.frame",'BB8.adm2.unstrat.allsurveys.to.natl.frame',
               "igme.frame")[1:15]
  methods.include <- which(sapply(methods,exists))
  
  natl.all <- data.frame()
  for(i in methods.include){
    if(nrow(natl.all)==0){
      natl.all <- eval(str2lang(methods[i]))
      natl.all$years <- as.numeric(paste(natl.all$years))
    }else{
      natl.all <- rbind(natl.all,eval(str2lang(methods[i])))
    }
  }
  
  natl.all$years <- as.numeric(natl.all$years)
  cols <- brewer.pal(n = 12, name = "Paired")
  
  #fix y axis
  y_limits_nmr <- c(min(natl.all$median_nmr,na.rm = T)*1000-1,max(natl.all$median_nmr,na.rm = T)*1000+1)
  y_limits_u5 <- c(min(natl.all$median_u5,na.rm = T)*1000-1,max(natl.all$median_u5,na.rm = T)*1000+1)
  
  ## Compare smoothed direct estimates ---------------
  # USE THIS ARGUMENT TO PICK METHODS TO PLOT -- may have to change this line
  methods.use <- c("natl.sd.yearly","aggre.sd.adm1","aggre.sd.yearly.adm1","aggre.sd.adm2","aggre.sd.yearly.adm2","igme")[c(1:6)]
  
  ##IF you have made a comparison plot before that you don't want to overwrite, make sure to change the name of the PDF!
  pdf(paste0(res.dir, "/Figures/Summary/NMR/",
             country, "_comparison_nmr_sd_",time.model, ".pdf"),height = 6,width = 6)
  {
  
  plot(NA, xlim=c(min(plot.years),max(plot.years)), ylim=y_limits_nmr,
       xlab='Year', ylab='Median NMR deaths per 1000 live births',
       las=2, xaxp=c(min(plot.years),max(plot.years),n_years-1))
  
  #plot methods
  for(method in methods.use){
    tmp<- natl.all[natl.all$method==method,]
    if(method %in% c("aggre.sd.adm1","aggre.sd.adm2","natl.sd")){
      lines(pane.years,tmp$median_nmr*1000,col=cols[which(method==methods.use)],lwd=1.5)
      points(pane.years,tmp$median_nmr*1000,col=cols[which(method==methods.use)],pch=20,cex=1)
    }else{
    lines(plot.years,tmp$median_nmr*1000,col=cols[which(method==methods.use)],lwd=1.5)
    points(plot.years,tmp$median_nmr*1000,col=cols[which(method==methods.use)],pch=20,cex=1)
    }
  }
  
  #plot credible intervals for IGME and natl.smoothed.yearly
  polygon(x=c(plot.years, rev(plot.years)),
            y=c(natl.all[natl.all$method=="igme",]$lower_nmr*1000, rev(natl.all[natl.all$method=="igme",]$upper_nmr*1000)),
            col=alpha(cols[methods.use=="igme"],0.25), border=F)
  polygon(x=c(plot.years, rev(plot.years)),
          y=c(natl.all[natl.all$method=="natl.sd.yearly",]$lower_nmr*1000, rev(natl.all[natl.all$method=="natl.sd.yearly",]$upper_nmr*1000)),
          col=alpha(cols[methods.use=="natl.sd.yearly"],0.25), border=F)
  
  #add grid
  suppressWarnings(grid())
  #add legend
  legend('topright', bty = 'n',
         pch = c(rep(20, length(methods.use))),
         lty = c(rep(1, length(methods.use))),
         col = cols[1:length(methods.use)],
         legend = methods.use)
  }
  dev.off()
 
  
  ##IF you have made a comparison plot before that you don't want to overwrite, make sure to change the name of the PDF!
  pdf(paste0(res.dir, "/Figures/Summary/U5MR/",
             country, "_comparison_u5_sd_",time.model, ".pdf"),height = 6,width = 6)
    {
      
      plot(NA, xlim=c(min(plot.years),max(plot.years)), ylim=y_limits_u5,
           xlab='Year', ylab='Median U5MR deaths per 1000 live births',
           las=2, xaxp=c(min(plot.years),max(plot.years),n_years-1))
      
      #plot methods
      for(method in methods.use){
        tmp<- natl.all[natl.all$method==method,]
        if(method %in% c("aggre.sd.adm1","aggre.sd.adm2","natl.sd")){
          lines(pane.years,tmp$median_u5*1000,col=cols[which(method==methods.use)],lwd=1.5)
          points(pane.years,tmp$median_u5*1000,col=cols[which(method==methods.use)],pch=20,cex=1)
        }else{
          lines(plot.years,tmp$median_u5*1000,col=cols[which(method==methods.use)],lwd=1.5)
          points(plot.years,tmp$median_u5*1000,col=cols[which(method==methods.use)],pch=20,cex=1)
        }
      }
      
      #plot credible intervals for IGME and natl.smoothed.yearly
      polygon(x=c(plot.years, rev(plot.years)),
              y=c(natl.all[natl.all$method=="igme",]$lower_u5*1000, rev(natl.all[natl.all$method=="igme",]$upper_u5*1000)),
              col=alpha(cols[methods.use=="igme"],0.25), border=F)
      polygon(x=c(plot.years, rev(plot.years)),
              y=c(natl.all[natl.all$method=="natl.sd.yearly",]$lower_u5*1000, rev(natl.all[natl.all$method=="natl.sd.yearly",]$upper_u5*1000)),
              col=alpha(cols[methods.use=="natl.sd.yearly"],0.25), border=F)
      
      #add grid
      suppressWarnings(grid())
      #add legend
      legend('topright', bty = 'n',
             pch = c(rep(20, length(methods.use))),
             lty = c(rep(1, length(methods.use))),
             col = cols[1:length(methods.use)],
             legend = methods.use)
    }
  dev.off()
  
  ## Compare betabinomial estimates ---------------
 
  # USE THIS ARGUMENT TO PICK METHODS TO PLOT -- may have to change this line
  methods.use <- c("natl.sd.yearly","natl.bb.unstrat","natl.bb.strat","natl.bb.unstrat.allsurveys",
                   "aggre.adm1.unstrat.BB8","aggre.adm1.strat.BB8","aggre.adm1.unstrat.allsurveys.BB8",
                   "aggre.adm2.unstrat.BB8","aggre.adm2.strat.BB8", "aggre.adm2.unstrat.allsurveys.BB8",
                   "igme")[c(1,3,4,6,7,9:11)]
  
  ##IF you have made a comparison plot before that you don't want to overwrite, make sure to change the name of the  PDF!
  pdf(paste0(res.dir, "/Figures/Summary/NMR/",
             country, "_comparison_nmr_bb8.pdf"),height = 6,width = 6)
  {
    
    plot(NA, xlim=c(min(plot.years),max(plot.years)), ylim=y_limits_nmr,
         xlab='Year', ylab='Median NMR deaths per 1000 live births',
         las=2, xaxp=c(min(plot.years),max(plot.years),n_years-1))
    
    #plot methods
    for(method in methods.use){
      tmp<- natl.all[natl.all$method==method,]
      if(method %in% c("aggre.sd.adm1","aggre.sd.adm2","natl.sd")){
        lines(pane.years,tmp$median_nmr*1000,col=cols[which(method==methods.use)],lwd=1.5)
        points(pane.years,tmp$median_nmr*1000,col=cols[which(method==methods.use)],pch=20,cex=1)
      }else{
        lines(plot.years,tmp$median_nmr*1000,col=cols[which(method==methods.use)],lwd=1.5)
        points(plot.years,tmp$median_nmr*1000,col=cols[which(method==methods.use)],pch=20,cex=1)
      }
    }
    
    #plot credible intervals for IGME and natl.smoothed.yearly
    polygon(x=c(plot.years, rev(plot.years)),
            y=c(natl.all[natl.all$method=="igme",]$lower_nmr*1000, rev(natl.all[natl.all$method=="igme",]$upper_nmr*1000)),
            col=alpha(cols[methods.use=="igme"],0.25), border=F)
    polygon(x=c(plot.years, rev(plot.years)),
            y=c(natl.all[natl.all$method=="natl.sd.yearly",]$lower_nmr*1000, rev(natl.all[natl.all$method=="natl.sd.yearly",]$upper_nmr*1000)),
            col=alpha(cols[methods.use=="natl.sd.yearly"],0.25), border=F)
    
    #add grid
    suppressWarnings(grid())
    #add legend
    legend('topright', bty = 'n',
           pch = c(rep(20, length(methods.use))),
           lty = c(rep(1, length(methods.use))),
           col = cols[1:length(methods.use)],
           legend = methods.use)
  }
  dev.off()
  
  ##IF you have made a comparison plot before that you don't want to overwrite, make sure to change the name of the PDF!
  pdf(paste0(res.dir, "/Figures/Summary/U5MR/",
             country, "_comparison_u5_bb8.pdf"),height = 6,width = 6)
  { 
    {
      
      plot(NA, xlim=c(min(plot.years),max(plot.years)), ylim=y_limits_u5,
           xlab='Year', ylab='Median U5MR deaths per 1000 live births',
           las=2, xaxp=c(min(plot.years),max(plot.years),n_years-1))
      
      #plot methods
      for(method in methods.use){
        tmp<- natl.all[natl.all$method==method,]
        if(method %in% c("aggre.sd.adm1","aggre.sd.adm2","natl.sd")){
          lines(pane.years,tmp$median_u5*1000,col=cols[which(method==methods.use)],lwd=1.5)
          points(pane.years,tmp$median_u5*1000,col=cols[which(method==methods.use)],pch=20,cex=1)
        }else{
          lines(plot.years,tmp$median_u5*1000,col=cols[which(method==methods.use)],lwd=1.5)
          points(plot.years,tmp$median_u5*1000,col=cols[which(method==methods.use)],pch=20,cex=1)
        }
      }
      
      #plot credible intervals for IGME and natl.smoothed.yearly
      polygon(x=c(plot.years, rev(plot.years)),
              y=c(natl.all[natl.all$method=="igme",]$lower_u5*1000, rev(natl.all[natl.all$method=="igme",]$upper_u5*1000)),
              col=alpha(cols[methods.use=="igme"],0.25), border=F)
      polygon(x=c(plot.years, rev(plot.years)),
              y=c(natl.all[natl.all$method=="natl.sd.yearly",]$lower_u5*1000, rev(natl.all[natl.all$method=="natl.sd.yearly",]$upper_u5*1000)),
              col=alpha(cols[methods.use=="natl.sd.yearly"],0.25), border=F)
      
      #add grid
      suppressWarnings(grid())
      #add legend
      legend('topright', bty = 'n',
             pch = c(rep(20, length(methods.use))),
             lty = c(rep(1, length(methods.use))),
             col = cols[1:length(methods.use)],
             legend = methods.use)
    }
  }
  dev.off()
  
  
  
  
  
  
  