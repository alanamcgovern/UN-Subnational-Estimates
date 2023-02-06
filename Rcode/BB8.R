rm(list = ls())
## ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Senegal'

## Libraries -----------------------------------------------
library(SUMMER)
library(INLA)
inla.setOption(inla.mode="experimental")
options(gsubfn.engine = "R")
library(rgdal)
library(tidyverse)

## Retrieve directories and country info -----------------------------------------------
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0("R://Project/STAB/",country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

source(file=paste0(home.dir, '/Rcode/getBB8.R'))
source(file=paste0(home.dir, '/Rcode/smoothCluster_mod.R'))

## Load admin names -----------------------------------------------
setwd(data.dir)

load(paste0(poly.path,'/', country, '_Amat.rda'))
load(paste0(poly.path,'/', country, '_Amat_Names.rda'))

## Load data -----------------------------------------------
load(paste0(country,'_cluster_dat_1frame.rda'),
     envir = .GlobalEnv)

mod.dat$years <- as.numeric(as.character(mod.dat$years))
mod.dat$country <- as.character(country)
survey_years <- unique(mod.dat$survey)
end.proj.year <- 2021

## Load IGME estimates ------------------------------------------------------
{
  setwd(paste0(home.dir,'/Data/IGME'))
  
  ## U5MR
  igme.ests.u5.raw <- read.csv('igme2022_u5_nocrisis.csv')
  igme.ests.u5 <- igme.ests.u5.raw[igme.ests.u5.raw$ISO.Code==gadm.abbrev,]
  igme.ests.u5 <- data.frame(t(igme.ests.u5[,10:ncol(igme.ests.u5)]))
  names(igme.ests.u5) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.u5$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.u5),'X')) - 0.5
  igme.ests.u5 <- igme.ests.u5[igme.ests.u5$year %in% beg.year:end.proj.year,]
  rownames(igme.ests.u5) <- NULL
  igme.ests.u5$OBS_VALUE <- igme.ests.u5$OBS_VALUE/1000
  igme.ests.u5$SD <- (igme.ests.u5$UPPER_BOUND - igme.ests.u5$LOWER_BOUND)/(2*1.645*1000)
  igme.ests.u5$LOWER_BOUND <- igme.ests.u5$OBS_VALUE - 1.96*igme.ests.u5$SD
  igme.ests.u5$UPPER_BOUND <- igme.ests.u5$OBS_VALUE + 1.96*igme.ests.u5$SD
  
  ## NMR
  igme.ests.nmr.raw <- read.csv('igme2022_nmr_nocrisis.csv')
  igme.ests.nmr <- igme.ests.nmr.raw[igme.ests.nmr.raw$iso==gadm.abbrev,]
  igme.ests.nmr <- data.frame(t(igme.ests.nmr[,10:ncol(igme.ests.nmr)]))
  names(igme.ests.nmr) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.nmr$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.nmr),'X')) - 0.5
  igme.ests.nmr <- igme.ests.nmr[igme.ests.nmr$year %in% beg.year:end.proj.year,]
  rownames(igme.ests.nmr) <- NULL
  igme.ests.nmr$OBS_VALUE <- igme.ests.nmr$OBS_VALUE/1000
  igme.ests.nmr$SD <- (igme.ests.nmr$UPPER_BOUND - igme.ests.nmr$LOWER_BOUND)/(2*1.645*1000)
  igme.ests.nmr$LOWER_BOUND <- igme.ests.nmr$OBS_VALUE - 1.96*igme.ests.nmr$SD
  igme.ests.nmr$UPPER_BOUND <- igme.ests.nmr$OBS_VALUE + 1.96*igme.ests.nmr$SD
}

## Load Admin 1 and 2 population proportions (for benchmarking) ------------------------------------------------------


load(paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm1_weights_u5.rda'))

if(exists('poly.layer.adm2')){
load(paste0(data.dir,'/worldpop/adm2_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm2_weights_u5.rda'))
}

#update them if necessary
if((max(weight.adm1.u1$years)<end.proj.year)){
  weight.adm1.u1 <- rbind(weight.adm1.u1,data.frame(region=rep(weight.adm1.u1[weight.adm1.u1$years==2020,]$region,(end.proj.year - 2020)),
                                                    proportion=rep(weight.adm1.u1[weight.adm1.u1$years==2020,]$proportion,(end.proj.year - 2020)),
                                                    years=sort(rep(2021:end.proj.year,length(admin1.names$GADM)))))
  weight.adm1.u5 <- rbind(weight.adm1.u5,data.frame(region=rep(weight.adm1.u5[weight.adm1.u5$years==2020,]$region,(end.proj.year - 2020)),
                                                    proportion=rep(weight.adm1.u5[weight.adm1.u5$years==2020,]$proportion,(end.proj.year - 2020)),
                                                    years=sort(rep(2021:end.proj.year,length(admin1.names$GADM)))))
  if(exists('poly.layer.adm2')){
    weight.adm2.u1 <- rbind(weight.adm2.u1,data.frame(region=rep(weight.adm2.u1[weight.adm2.u1$years==2020,]$region,(end.proj.year - 2020)),
                                                      proportion=rep(weight.adm2.u1[weight.adm2.u1$years==2020,]$proportion,(end.proj.year - 2020)),
                                                      years=sort(rep(2021:end.proj.year,length(admin2.names$GADM)))))
    weight.adm2.u5 <- rbind(weight.adm2.u5,data.frame(region=rep(weight.adm2.u5[weight.adm2.u5$years==2020,]$region,(end.proj.year - 2020)),
                                                      proportion=rep(weight.adm2.u5[weight.adm2.u5$years==2020,]$proportion,(end.proj.year - 2020)),
                                                      years=sort(rep(2021:end.proj.year,length(admin2.names$GADM)))))
  }
}else if((max(weight.adm1.u1$years)>end.proj.year)){
  weight.adm1.u1 <- weight.adm1.u1[weight.adm1.u1$years<=end.proj.year,]
  weight.adm1.u5 <- weight.adm1.u5[weight.adm1.u5$years<=end.proj.year,]
  if(exists('poly.layer.adm2')){
    weight.adm2.u1 <- weight.adm2.u1[weight.adm2.u1$years<=end.proj.year,]
    weight.adm2.u5 <- weight.adm2.u5[weight.adm2.u5$years<=end.proj.year,]
  }
}

save(weight.adm1.u1,file=paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
save(weight.adm1.u5,file=paste0(data.dir,'/worldpop/adm1_weights_u5.rda'))
if(exists('poly.layer.adm2')){
  save(weight.adm2.u1,file=paste0(data.dir,'/worldpop/adm2_weights_u1.rda'))
  save(weight.adm2.u5,file=paste0(data.dir,'/worldpop/adm2_weights_u5.rda'))
}

## Load intercept priors for benchmarking -----------------------------------------------
load(paste0(data.dir,'/',country,'_age_int_priors_bench.rda'))

## Load HIV Adjustment info -----------------------------------------------

if(doHIVAdj){
  load(paste0(home.dir,'/Data/HIV/',
              'HIVAdjustments.rda'),
       envir = .GlobalEnv)
  hiv.adj <- hiv.adj[hiv.adj$country == country,]
  if(unique(hiv.adj$area)[1] == country){
    natl.unaids <- T
  }else{
    natl.unaids <- F}
  
  if(natl.unaids){
    adj.frame <- hiv.adj
    adj.varnames <- c("country", "survey", "years")
  }else{ adj.frame <- hiv.adj
    mod.dat$area <- mod.dat$admin1.name
    if(country=='Mozambique'){
      mod.dat[mod.dat$area=='Maputo City',]$area <- 'Maputo'
    }
  adj.varnames <- c("country", "area","survey", "years")
  }
  adj.frame <- adj.frame[adj.frame$survey %in% survey_years,c(adj.varnames,"ratio")]
}else{
  adj.frame <- expand.grid(years = beg.year:end.proj.year,country = country)
  adj.frame$ratio <- 1
  adj.varnames <- c("country", "years")
}

## Load UR proportions -----------------------------------------------
if(dir.exists(paths = paste0(res.dir,'/UR/'))){
  setwd(paste0(res.dir,'/UR'))
  weight.strata.natl.u5 <- readRDS(paste0('U5_fraction/','natl_u5_urban_weights.rds'))
  weight.strata.natl.u5$rural <- 1-weight.strata.natl.u5$urban
  weight.strata.adm1.u5 <- readRDS(paste0('U5_fraction/','admin1_u5_urban_weights.rds'))

  weight.strata.natl.u1 <- readRDS(paste0('U1_fraction/','natl_u1_urban_weights.rds'))
  weight.strata.natl.u1$rural <- 1-weight.strata.natl.u1$urban
  weight.strata.adm1.u1 <- readRDS(paste0('U1_fraction/','admin1_u1_urban_weights.rds'))
  
  if(exists('poly.layer.adm2')){
  weight.strata.adm2.u5 <- readRDS(paste0('U5_fraction/','admin2_u5_urban_weights.rds'))
  weight.strata.adm2.u1 <- readRDS(paste0('U1_fraction/','admin2_u1_urban_weights.rds'))
  }
  
  #adjust if necessary
  if(end.proj.year > max(weight.strata.natl.u1)){
    weight.strata.natl.u1 <- rbind(weight.strata.natl.u1,
                                   data.frame(years=2021:end.proj.year,
                                              urban=rep(weight.strata.natl.u1[weight.strata.natl.u1$years==2020,]$urban,end.proj.year-2020),
                                              rural=1-rep(weight.strata.natl.u1[weight.strata.natl.u1$years==2020,]$urban,end.proj.year-2020)))
    weight.strata.natl.u5 <- rbind(weight.strata.natl.u5,
                                   data.frame(years=2021:end.proj.year,
                                              urban=rep(weight.strata.natl.u5[weight.strata.natl.u5$years==2020,]$urban,end.proj.year-2020),
                                              rural=1-rep(weight.strata.natl.u5[weight.strata.natl.u5$years==2020,]$urban,end.proj.year-2020)))
    weight.strata.adm1.u1 <- rbind(weight.strata.adm1.u1,
                                   data.frame(region=rep(admin1.names$Internal,end.proj.year-2020),
                                              years=sort(rep(2021:end.proj.year,nrow(admin1.names))),
                                              urban=rep(weight.strata.adm1.u1[weight.strata.adm1.u1$years==2020,]$urban,end.proj.year-2020),
                                              rural=1-rep(weight.strata.adm1.u1[weight.strata.adm1.u1$years==2020,]$urban,end.proj.year-2020)))
    weight.strata.adm1.u5 <- rbind(weight.strata.adm1.u5,
                                   data.frame(region=rep(admin1.names$Internal,end.proj.year-2020),
                                              years=sort(rep(2021:end.proj.year,nrow(admin1.names))),
                                              urban=rep(weight.strata.adm1.u5[weight.strata.adm1.u5$years==2020,]$urban,end.proj.year-2020),
                                              rural=1-rep(weight.strata.adm1.u5[weight.strata.adm1.u5$years==2020,]$urban,end.proj.year-2020)))
    if(exists('poly.layer.adm2')){
    weight.strata.adm2.u1 <- rbind(weight.strata.adm2.u1,
                                   data.frame(region=rep(admin2.names$Internal,end.proj.year-2020),
                                              years=sort(rep(2021:end.proj.year,nrow(admin2.names))),
                                              urban=rep(weight.strata.adm2.u1[weight.strata.adm2.u1$years==2020,]$urban,end.proj.year-2020),
                                              rural=1-rep(weight.strata.adm2.u1[weight.strata.adm2.u1$years==2020,]$urban,end.proj.year-2020)))
    weight.strata.adm2.u5 <- rbind(weight.strata.adm2.u5,
                                   data.frame(region=rep(admin2.names$Internal,end.proj.year-2020),
                                              years=sort(rep(2021:end.proj.year,nrow(admin2.names))),
                                              urban=rep(weight.strata.adm2.u5[weight.strata.adm2.u5$years==2020,]$urban,end.proj.year-2020),
                                              rural=1-rep(weight.strata.adm2.u5[weight.strata.adm2.u5$years==2020,]$urban,end.proj.year-2020)))
    }
  }else if(end.proj.year < max(weight.strata.natl.u1)){
    weight.strata.natl.u1 <- weight.strata.natl.u1[weight.strata.natl.u1$years<=end.proj.year,]
    weight.strata.natl.u5 <- weight.strata.natl.u5[weight.strata.natl.u5$years<=end.proj.year,]
    weight.strata.adm1.u1 <- weight.strata.adm1.u1[weight.strata.adm1.u1$years<=end.proj.year,]
    weight.strata.adm1.u5 <- weight.strata.adm1.u5[weight.strata.adm1.u5$years<=end.proj.year,]
    if(exists('poly.layer.adm2')){
    weight.strata.adm2.u1 <- weight.strata.adm2.u1[weight.strata.adm2.u1$years<=end.proj.year,]
    weight.strata.adm2.u5 <- weight.strata.adm2.u5[weight.strata.adm2.u5$years<=end.proj.year,]
    }
  }
}

## Fit BB8 models w surveys from same sampling frame  -----------------------------------------------
setwd(paste0(res.dir))

time_mod <- c("rw2", "ar1")[1]

if(!dir.exists("Betabinomial/")){
  dir.create("Betabinomial/")
  dir.create("Betabinomial/NMR/")
  dir.create("Betabinomial/U5MR/")
}

### NMR -----------------------------------------------
  #### National Unstrat ----------------------------------------------

bb.natl.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                              Amat=NULL, admin.level='National',
                              stratified=F, weight.strata=NULL,
                              outcome='nmr', time.model =time_mod,
                              adj.frame=adj.frame, adj.varnames=adj.varnames,
                              doBenchmark=F,nsim = 1000)

bb.fit.natl.unstrat.nmr <- bb.natl.unstrat.nmr[[1]]
bb.res.natl.unstrat.nmr <- bb.natl.unstrat.nmr[[2]]

bb.temporals.natl.unstrat.nmr <- getDiag(bb.natl.unstrat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.natl.unstrat.nmr <- bb.fit.natl.unstrat.nmr$fit$summary.hyperpar
bb.fixed.natl.unstrat.nmr <- bb.fit.natl.unstrat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_natl_', time_mod, '_unstrat_nmr.txt'))
summary(bb.fit.natl.unstrat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.natl.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_natl_', time_mod, '_unstrat_nmr.rda'))
save(bb.hyperpar.natl.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_natl_', time_mod, '_unstrat_nmr.rda'))
save(bb.fixed.natl.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_natl_', time_mod, '_unstrat_nmr.rda'))
# save results
save(bb.res.natl.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_natl_', time_mod, '_unstrat_nmr.rda'))

#### National Strat -----------------------------------------------
bb.natl.strat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                            Amat=NULL, admin.level='National',
                            stratified=T, weight.strata=weight.strata.natl.u1,
                            outcome='nmr', time.model=time_mod,
                            adj.frame=adj.frame, adj.varnames=adj.varnames,
                            doBenchmark=F,nsim = 1000)
bb.fit.natl.strat.nmr <- bb.natl.strat.nmr[[1]]
bb.res.natl.strat.nmr <- bb.natl.strat.nmr[[2]]

bb.temporals.natl.strat.nmr <- getDiag(bb.natl.strat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.natl.strat.nmr <- bb.fit.natl.strat.nmr$fit$summary.hyperpar
bb.fixed.natl.strat.nmr <- bb.fit.natl.strat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_natl_', time_mod, '_strat_nmr.txt'))
summary(bb.fit.natl.strat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.natl.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_natl_', time_mod, '_strat_nmr.rda'))
save(bb.hyperpar.natl.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_natl_', time_mod, '_strat_nmr.rda'))
save(bb.fixed.natl.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_natl_', time_mod, '_strat_nmr.rda'))
# save results
save(bb.res.natl.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_natl_', time_mod, '_strat_nmr.rda'))

#### Admin1 Unstrat ----------------------------------------------
bb.adm1.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                              Amat=admin1.mat, admin.level='Admin1',
                              stratified=F, weight.strata=NULL,
                              outcome='nmr', 
                              time.model=time_mod, st.time.model=time_mod,
                              adj.frame=adj.frame, adj.varnames=adj.varnames,
                              doBenchmark=F,nsim = 1000)

bb.fit.adm1.unstrat.nmr <- bb.adm1.unstrat.nmr[[1]]
bb.res.adm1.unstrat.nmr <- bb.adm1.unstrat.nmr[[2]]

bb.temporals.adm1.unstrat.nmr <- getDiag(bb.adm1.unstrat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm1.unstrat.nmr <- bb.fit.adm1.unstrat.nmr$fit$summary.hyperpar
bb.fixed.adm1.unstrat.nmr <- bb.fit.adm1.unstrat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm1_', time_mod, '_unstrat_nmr.txt'))
summary(bb.fit.adm1.unstrat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm1.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_adm1_', time_mod, '_unstrat_nmr.rda'))
save(bb.hyperpar.adm1.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm1_', time_mod, '_unstrat_nmr.rda'))
save(bb.fixed.adm1.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_adm1_', time_mod, '_unstrat_nmr.rda'))
# save results
save(bb.res.adm1.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm1_', time_mod, '_unstrat_nmr.rda'))

  #### Admin1 Strat -----------------------------------------------
bb.adm1.strat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                            Amat=admin1.mat, admin.level='Admin1',
                            stratified=T, weight.strata=weight.strata.adm1.u1,
                            outcome='nmr',
                            time.model=time_mod, st.time.model=time_mod,
                            adj.frame=adj.frame, adj.varnames=adj.varnames,
                            doBenchmark=F,nsim = 1000)
bb.fit.adm1.strat.nmr <- bb.adm1.strat.nmr[[1]]
bb.res.adm1.strat.nmr <- bb.adm1.strat.nmr[[2]]

bb.temporals.adm1.strat.nmr <- getDiag(bb.adm1.strat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm1.strat.nmr <- bb.fit.adm1.strat.nmr$fit$summary.hyperpar
bb.fixed.adm1.strat.nmr <- bb.fit.adm1.strat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm1_', time_mod, '_strat_nmr.txt'))
summary(bb.fit.adm1.strat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm1.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_adm1_', time_mod, '_strat_nmr.rda'))
save(bb.hyperpar.adm1.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm1_', time_mod, '_strat_nmr.rda'))
save(bb.fixed.adm1.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_adm1_', time_mod, '_strat_nmr.rda'))
# save results
save(bb.res.adm1.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm1_', time_mod, '_strat_nmr.rda'))

  #### Admin2 Unstrat ----------------------------------------------
bb.adm2.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                              Amat=admin2.mat, admin.level='Admin2',
                              stratified=F, weight.strata=NULL,
                              outcome='nmr',
                              time.model=time_mod, st.time.model=time_mod,
                              adj.frame=adj.frame, adj.varnames=adj.varnames,
                              doBenchmark=F,nsim = 1000)

bb.fit.adm2.unstrat.nmr <- bb.adm2.unstrat.nmr[[1]]
bb.res.adm2.unstrat.nmr <- bb.adm2.unstrat.nmr[[2]]

bb.temporals.adm2.unstrat.nmr <- getDiag(bb.adm2.unstrat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm2.unstrat.nmr <- bb.fit.adm2.unstrat.nmr$fit$summary.hyperpar
bb.fixed.adm2.unstrat.nmr <- bb.fit.adm2.unstrat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm2_', time_mod, '_unstrat_nmr.txt'))
summary(bb.fit.adm2.unstrat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm2.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_adm2_', time_mod, '_unstrat_nmr.rda'))
save(bb.hyperpar.adm2.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm2_', time_mod, '_unstrat_nmr.rda'))
save(bb.fixed.adm2.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_adm2_', time_mod, '_unstrat_nmr.rda'))
# save results
save(bb.res.adm2.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm2_', time_mod, '_unstrat_nmr.rda'))

  #### Admin2 Strat -----------------------------------------------
bb.adm2.strat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                            Amat=admin2.mat, admin.level='Admin2',
                            stratified=T, weight.strata=weight.strata.adm2.u1,
                            outcome='nmr',
                            time.model=time_mod, st.time.model=time_mod,
                            adj.frame=adj.frame, adj.varnames=adj.varnames,
                            doBenchmark=F,nsim = 1000)
bb.fit.adm2.strat.nmr <- bb.adm2.strat.nmr[[1]]
bb.res.adm2.strat.nmr <- bb.adm2.strat.nmr[[2]]

bb.temporals.adm2.strat.nmr <- getDiag(bb.adm2.strat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm2.strat.nmr <- bb.fit.adm2.strat.nmr$fit$summary.hyperpar
bb.fixed.adm2.strat.nmr <- bb.fit.adm2.strat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm2_', time_mod, '_strat_nmr.txt'))
summary(bb.fit.adm2.strat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm2.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_adm2_', time_mod, '_strat_nmr.rda'))
save(bb.hyperpar.adm2.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm2_', time_mod, '_strat_nmr.rda'))
save(bb.fixed.adm2.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_adm2_', time_mod, '_strat_nmr.rda'))
# save results
save(bb.res.adm2.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm2_', time_mod, '_strat_nmr.rda'))

### U5MR -----------------------------------------------
  #### National Unstrat ----------------------------------------------
bb.natl.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=NULL, admin.level='National',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr', time.model=time_mod,
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim=1000)
bb.fit.natl.unstrat.u5 <- bb.natl.unstrat.u5[[1]]
bb.res.natl.unstrat.u5 <- bb.natl.unstrat.u5[[2]]

bb.temporals.natl.unstrat.u5 <- getDiag(bb.natl.unstrat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.natl.unstrat.u5 <- bb.fit.natl.unstrat.u5$fit$summary.hyperpar
bb.fixed.natl.unstrat.u5 <- bb.fit.natl.unstrat.u5$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_natl_', time_mod, '_unstrat_u5.txt'))
summary(bb.fit.natl.unstrat.u5)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.natl.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_natl_', time_mod, '_unstrat_u5.rda'))
save(bb.hyperpar.natl.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_natl_', time_mod, '_unstrat_u5.rda'))
save(bb.fixed.natl.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_natl_', time_mod, '_unstrat_u5.rda'))
# save results
save(bb.res.natl.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_natl_', time_mod, '_unstrat_u5.rda'))


  #### National Strat -----------------------------------------------
bb.natl.strat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                           Amat=NULL, admin.level='National',
                           stratified=T, weight.strata=weight.strata.natl.u5,
                           outcome='u5mr', time.model=time_mod,
                           adj.frame=adj.frame, adj.varnames=adj.varnames,
                           doBenchmark=F,nsim=1000)
bb.fit.natl.strat.u5 <- bb.natl.strat.u5[[1]]
bb.res.natl.strat.u5 <- bb.natl.strat.u5[[2]]

bb.temporals.natl.strat.u5 <- getDiag(bb.natl.strat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.natl.strat.u5 <- bb.fit.natl.strat.u5$fit$summary.hyperpar
bb.fixed.natl.strat.u5 <- bb.fit.natl.strat.u5$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_natl_', time_mod, '_strat_u5.txt'))
summary(bb.fit.natl.strat.u5)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.natl.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_natl_', time_mod, '_strat_u5.rda'))
save(bb.hyperpar.natl.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_natl_', time_mod, '_strat_u5.rda'))
save(bb.fixed.natl.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_natl_', time_mod, '_strat_u5.rda'))
# save results
save(bb.res.natl.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_natl_', time_mod, '_strat_u5.rda'))

  #### Admin1 Unstrat ----------------------------------------------
bb.adm1.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin1.mat, admin.level='Admin1',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr',
                             time.model=time_mod, st.time.model=time_mod,
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 1000)
bb.fit.adm1.unstrat.u5 <- bb.adm1.unstrat.u5[[1]]
bb.res.adm1.unstrat.u5 <- bb.adm1.unstrat.u5[[2]]

bb.temporals.adm1.unstrat.u5 <- getDiag(bb.adm1.unstrat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm1.unstrat.u5 <- bb.fit.adm1.unstrat.u5$fit$summary.hyperpar
bb.fixed.adm1.unstrat.u5 <- bb.fit.adm1.unstrat.u5$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_', time_mod, '_unstrat_u5.txt'))
summary(bb.fit.adm1.unstrat.u5)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm1.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm1_', time_mod, '_unstrat_u5.rda'))
save(bb.hyperpar.adm1.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm1_', time_mod, '_unstrat_u5.rda'))
save(bb.fixed.adm1.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm1_', time_mod, '_unstrat_u5.rda'))
# save results
save(bb.res.adm1.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm1_', time_mod, '_unstrat_u5.rda'))

  #### Admin1 Strat -----------------------------------------------

bb.adm1.strat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                           Amat=admin1.mat, admin.level='Admin1',
                           stratified=T, weight.strata=weight.strata.adm1.u5,
                           outcome='u5mr',
                           time.model=time_mod, st.time.model=time_mod,
                           adj.frame=adj.frame, adj.varnames=adj.varnames,
                           doBenchmark=F,nsim=1000)
bb.fit.adm1.strat.u5 <- bb.adm1.strat.u5[[1]]
bb.res.adm1.strat.u5 <- bb.adm1.strat.u5[[2]]

bb.temporals.adm1.strat.u5 <- getDiag(bb.adm1.strat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm1.strat.u5 <- bb.fit.adm1.strat.u5$fit$summary.hyperpar
bb.fixed.adm1.strat.u5 <- bb.fit.adm1.strat.u5$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_', time_mod, '_strat_u5.txt'))
summary(bb.fit.adm1.strat.u5)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm1.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm1_', time_mod, '_strat_u5.rda'))
save(bb.hyperpar.adm1.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm1_', time_mod, '_strat_u5.rda'))
save(bb.fixed.adm1.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm1_', time_mod, '_strat_u5.rda'))
# save results
save(bb.res.adm1.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm1_', time_mod, '_strat_u5.rda'))

  #### Admin2 Unstrat ---------------------------------------------
bb.adm2.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr',
                             time.model=time_mod, st.time.model=time_mod,
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 1000)
bb.fit.adm2.unstrat.u5 <- bb.adm2.unstrat.u5[[1]]
bb.res.adm2.unstrat.u5 <- bb.adm2.unstrat.u5[[2]]

bb.temporals.adm2.unstrat.u5 <- getDiag(bb.adm2.unstrat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm2.unstrat.u5 <- bb.fit.adm2.unstrat.u5$fit$summary.hyperpar
bb.fixed.adm2.unstrat.u5 <- bb.fit.adm2.unstrat.u5$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_', time_mod, '_unstrat_u5.txt'))
summary(bb.fit.adm2.unstrat.u5)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm2.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm2_', time_mod, '_unstrat_u5.rda'))
save(bb.hyperpar.adm2.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm2_', time_mod, '_unstrat_u5.rda'))
save(bb.fixed.adm2.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm2_', time_mod, '_unstrat_u5.rda'))
# save results
save(bb.res.adm2.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm2_', time_mod, '_unstrat_u5.rda'))

  #### Admin2 Strat -----------------------------------------------
  bb.adm2.strat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=T, weight.strata=weight.strata.adm2.u5,
                             outcome='u5mr',
                             time.model=time_mod, st.time.model=time_mod,
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 1000)
  bb.fit.adm2.strat.u5 <- bb.adm2.strat.u5[[1]]
  bb.res.adm2.strat.u5 <- bb.adm2.strat.u5[[2]]
  
  bb.temporals.adm2.strat.u5 <- getDiag(bb.adm2.strat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm2.strat.u5 <- bb.fit.adm2.strat.u5$fit$summary.hyperpar
  bb.fixed.adm2.strat.u5 <- bb.fit.adm2.strat.u5$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_', time_mod, '_strat_u5.txt'))
  summary(bb.fit.adm2.strat.u5)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm2.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm2_', time_mod, '_strat_u5.rda'))
  save(bb.hyperpar.adm2.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm2_', time_mod, '_strat_u5.rda'))
  save(bb.fixed.adm2.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm2_', time_mod, '_strat_u5.rda'))
  # save results
  save(bb.res.adm2.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm2_', time_mod, '_strat_u5.rda'))
  
## Get benchmarked stratified models  -----------------------------------------------
### NMR -----------------------------------------------
  #### Admin1 Strat, Benchmarked -----------------------------------------------
  bb.adm1.strat.nmr.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                    Amat=admin1.mat, admin.level='Admin1',
                                    stratified=T, weight.strata=weight.strata.adm1.u1,
                                    outcome='nmr',
                                    time.model=time_mod, st.time.model=time_mod,
                                    adj.frame=adj.frame, adj.varnames=adj.varnames,
                                    weight.region = weight.adm1.u1,
                                    igme.ests = igme.ests.nmr,
                                    int.priors.prec.bench = 10, # parameter to play around with if getting very low acceptance
                                    doBenchmark=T, nsim = 1000)
  
  bb.fit.adm1.strat.nmr.bench <- bb.adm1.strat.nmr.bench[[1]]
  bb.res.adm1.strat.nmr.bench <- bb.adm1.strat.nmr.bench[[2]]
  
  bb.temporals.adm1.strat.nmr.bench <- getDiag(bb.adm1.strat.nmr.bench$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm1.strat.nmr.bench <- bb.fit.adm1.strat.nmr.bench$fit$summary.hyperpar
  bb.fixed.adm1.strat.nmr.bench <- bb.fit.adm1.strat.nmr.bench$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm1_', time_mod, '_strat_nmr_bench.txt'))
  summary(bb.fit.adm1.strat.nmr.bench)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm1.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_temporals_adm1_', time_mod, '_strat_nmr_bench.rda'))
  save(bb.hyperpar.adm1.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm1_', time_mod, '_strat_nmr_bench.rda'))
  save(bb.fixed.adm1.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_fixed_adm1_', time_mod, '_strat_nmr_bench.rda'))
  # save results
  save(bb.res.adm1.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_res_adm1_', time_mod, '_strat_nmr_bench.rda'))
  
  # save traceplots from benchmarking for 12 random combinations of admin area and year
  pdf(file = paste0('Betabinomial/NMR/',country,'_adm1_', time_mod, '_strat_nmr_bench_trace.pdf'))
  par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
  ind <- sort(sample(1:length(bb.res.adm1.strat.nmr.bench$draws.est.overall),12))
  for(i in ind){
    bb.draws.tmp <- bb.res.adm1.strat.nmr.bench$draws.est.overall[[i]]$draws
    plot(bb.draws.tmp,type='l', ylab=' ',xlab='',
         main=paste0(bb.res.adm1.strat.nmr.bench$draws.est.overall[[i]]$region,", ", bb.res.adm1.strat.nmr.bench$draws.est.overall[[i]]$years))
  }
  dev.off()
  
  #### Admin2 Strat, Benchmarked -----------------------------------------------
  bb.adm2.strat.nmr.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                    Amat=admin2.mat, admin.level='Admin2',
                                    stratified=T, weight.strata=weight.strata.adm2.u1,
                                    outcome='nmr',
                                    time.model=time_mod, st.time.model=time_mod,
                                    adj.frame=adj.frame, adj.varnames=adj.varnames,
                                    weight.region = weight.adm2.u1, 
                                    igme.ests = igme.ests.nmr,
                                    int.priors.prec.bench = 10, # parameter to play around with if getting very low acceptance
                                    doBenchmark=T,nsim = 1000)
  bb.fit.adm2.strat.nmr.bench <- bb.adm2.strat.nmr.bench[[1]]
  bb.res.adm2.strat.nmr.bench <- bb.adm2.strat.nmr.bench[[2]]
  
  bb.temporals.adm2.strat.nmr.bench <- getDiag(bb.adm2.strat.nmr.bench$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm2.strat.nmr.bench <- bb.fit.adm2.strat.nmr.bench$fit$summary.hyperpar
  bb.fixed.adm2.strat.nmr.bench <- bb.fit.adm2.strat.nmr.bench$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm2_', time_mod, '_strat_nmr_bench.txt'))
  summary(bb.fit.adm2.strat.nmr.bench)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm2.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_temporals_adm2_', time_mod, '_strat_nmr_bench.rda'))
  save(bb.hyperpar.adm2.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm2_', time_mod, '_strat_nmr_bench.rda'))
  save(bb.fixed.adm2.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_fixed_adm2_', time_mod, '_strat_nmr_bench.rda'))
  # save results
  save(bb.res.adm2.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_res_adm2_', time_mod, '_strat_nmr_bench.rda'))
  
  # save traceplots from benchmarking for 12 random combinations of admin area and year
  pdf(file = paste0('Betabinomial/NMR/',country,'_adm2_', time_mod, '_strat_nmr_bench_trace.pdf'))
  par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
  ind <- sort(sample(1:length(bb.res.adm2.strat.nmr.bench$draws.est.overall),12))
  for(i in ind){
    bb.draws.tmp <- bb.res.adm2.strat.nmr.bench$draws.est.overall[[i]]$draws
    plot(bb.draws.tmp,type='l',
         ylab=' ',xlab='',main=paste0(bb.res.adm2.strat.nmr.bench$draws.est.overall[[i]]$region,", ", bb.res.adm2.strat.nmr.bench$draws.est.overall[[i]]$years))
  }
  dev.off()
  
### U5MR -----------------------------------------------
  #### Admin1 Strat, Benchmarked -----------------------------------------------
  
  bb.adm1.strat.u5.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                   Amat=admin1.mat, admin.level='Admin1',
                                   stratified=T, weight.strata=weight.strata.adm1.u5,
                                   outcome='u5mr',
                                   time.model=time_mod, st.time.model=time_mod,
                                   adj.frame=adj.frame, adj.varnames=adj.varnames,
                                   weight.region = weight.adm1.u5, 
                                   igme.ests = igme.ests.u5,
                                   int.priors.bench = int.priors.bench,
                                   int.priors.prec.bench = 10, # can change this if getting very small acceptance
                                   doBenchmark=T,nsim=1000)
  bb.fit.adm1.strat.u5.bench <- bb.adm1.strat.u5.bench[[1]]
  bb.res.adm1.strat.u5.bench <- bb.adm1.strat.u5.bench[[2]]
  
  bb.temporals.adm1.strat.u5.bench <- getDiag(bb.adm1.strat.u5.bench$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm1.strat.u5.bench <- bb.fit.adm1.strat.u5.bench$fit$summary.hyperpar
  bb.fixed.adm1.strat.u5.bench <- bb.fit.adm1.strat.u5.bench$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_', time_mod, '_strat_u5_bench.txt'))
  summary(bb.fit.adm1.strat.u5.bench)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm1.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm1_', time_mod, '_strat_u5_bench.rda'))
  save(bb.hyperpar.adm1.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm1_', time_mod, '_strat_u5_bench.rda'))
  save(bb.fixed.adm1.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm1_', time_mod, '_strat_u5_bench.rda'))
  # save results
  save(bb.res.adm1.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_res_adm1_', time_mod, '_strat_u5_bench.rda'))
  
  # save traceplots from benchmarking for 12 random combinations of admin area and year
  pdf(file = paste0('Betabinomial/U5MR/',country,'_adm1_', time_mod, '_strat_u5_bench_trace.pdf'))
  par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
  ind <- sort(sample(1:length(bb.res.adm1.strat.u5.bench$draws.est.overall),12))
  for(i in ind){
    bb.draws.tmp <- bb.res.adm1.strat.u5.bench$draws.est.overall[[i]]$draws
    plot(bb.draws.tmp,type='l', ylab=' ',xlab='',
         main=paste0(bb.res.adm1.strat.u5.bench$draws.est.overall[[i]]$region,", ", bb.res.adm1.strat.u5.bench$draws.est.overall[[i]]$years))
  }
  dev.off()
  
  #### Admin2 Strat, Benchmarked -----------------------------------------------
  bb.adm2.strat.u5.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                   Amat=admin2.mat, admin.level='Admin2',
                                   stratified=T, weight.strata=weight.strata.adm2.u5,
                                   outcome='u5mr',
                                   time.model=time_mod, st.time.model=time_mod,
                                   adj.frame=adj.frame, adj.varnames=adj.varnames,
                                   weight.region = weight.adm2.u5, 
                                   igme.ests = igme.ests.u5,
                                   int.priors.bench = int.priors.bench,
                                   int.priors.prec.bench = 10, # parameter to play around with if getting very low acceptance
                                   doBenchmark=T,nsim=1000)
  bb.fit.adm2.strat.u5.bench <- bb.adm2.strat.u5.bench[[1]]
  bb.res.adm2.strat.u5.bench <- bb.adm2.strat.u5.bench[[2]]
  
  bb.temporals.adm2.strat.u5.bench <- getDiag(bb.adm2.strat.u5.bench$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm2.strat.u5.bench <- bb.fit.adm2.strat.u5.bench$fit$summary.hyperpar
  bb.fixed.adm2.strat.u5.bench <- bb.fit.adm2.strat.u5.bench$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_', time_mod, '_strat_u5_bench.txt'))
  summary(bb.fit.adm2.strat.u5.bench)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm2.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm2_', time_mod, '_strat_u5_bench.rda'))
  save(bb.hyperpar.adm2.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm2_', time_mod, '_strat_u5_bench.rda'))
  save(bb.fixed.adm2.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm2_', time_mod, '_strat_u5_bench.rda'))
  # save results
  save(bb.res.adm2.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_res_adm2_', time_mod, '_strat_u5_bench.rda'))
  
  # save traceplots from benchmarking for 12 random combinations of admin area and year
  pdf(file = paste0('Betabinomial/U5MR/',country,'_adm2_', time_mod, '_strat_u5_bench_trace.pdf'))
  par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
  ind <- sort(sample(1:length(bb.res.adm2.strat.u5.bench$draws.est.overall),12))
  for(i in ind){
    bb.draws.tmp <- bb.res.adm2.strat.u5.bench$draws.est.overall[[i]]$draws
    plot(bb.draws.tmp,type='l', ylab=' ',xlab='',
         main=paste0(bb.res.adm2.strat.u5.bench$draws.est.overall[[i]]$region,", ", bb.res.adm2.strat.u5.bench$draws.est.overall[[i]]$years))
  }
  dev.off()
## Fit BB8 models w all surveys (from different sampling frames) -----------------------------------------------
## Load data -----------------------------------------------
  setwd(data.dir)
  load(paste0(country,'_cluster_dat.rda'),
       envir = .GlobalEnv)
  
  mod.dat$years <- as.numeric(as.character(mod.dat$years))
  mod.dat$country <- as.character(country)
  survey_years <- unique(mod.dat$survey)

## Load HIV Adjustment info -----------------------------------------------
  
  if(doHIVAdj){
    load(paste0(home.dir,'/Data/HIV/',
                'HIVAdjustments.rda'),
         envir = .GlobalEnv)
    hiv.adj <- hiv.adj[hiv.adj$country == country,]
    if(unique(hiv.adj$area)[1] == country){
      natl.unaids <- T
    }else{
      natl.unaids <- F}
    
    if(natl.unaids){
      adj.frame <- hiv.adj
      adj.varnames <- c("country", "survey", "years")
    }else{ adj.frame <- hiv.adj
    mod.dat$area <- mod.dat$admin1.name
    if(country=='Mozambique'){
      mod.dat[mod.dat$area=='Maputo City',]$area <- 'Maputo'
    }
    adj.varnames <- c("country", "area","survey", "years")
    }
    adj.frame <- adj.frame[adj.frame$survey %in% survey_years,c(adj.varnames,"ratio")]
  }else{
    adj.frame <- expand.grid(years = beg.year:end.proj.year,country = country)
    adj.frame$ratio <- 1
    adj.varnames <- c("country", "years")
  }
  
### NMR -----------------------------------------------
  setwd(paste0(res.dir))
  #### National Unstrat ----------------------------------------------
  bb.natl.unstrat.nmr.allsurveys <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                Amat=NULL, admin.level='National',
                                stratified=F, weight.strata=NULL,
                                outcome='nmr', time.model=time_mod,
                                adj.frame=adj.frame, adj.varnames=adj.varnames,
                                doBenchmark=F,nsim = 1000)
  
  bb.fit.natl.unstrat.nmr.allsurveys <- bb.natl.unstrat.nmr.allsurveys[[1]]
  bb.res.natl.unstrat.nmr.allsurveys <- bb.natl.unstrat.nmr.allsurveys[[2]]
  
  bb.temporals.natl.unstrat.nmr.allsurveys <- getDiag(bb.natl.unstrat.nmr.allsurveys$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.natl.unstrat.nmr.allsurveys <- bb.fit.natl.unstrat.nmr.allsurveys$fit$summary.hyperpar
  bb.fixed.natl.unstrat.nmr.allsurveys <- bb.fit.natl.unstrat.nmr.allsurveys$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/NMR/',country,'_fit_natl_', time_mod, '_unstrat_nmr_allsurveys.txt'))
  summary(bb.fit.natl.unstrat.nmr.allsurveys)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.natl.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_temporals_natl_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  save(bb.hyperpar.natl.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_hyperpar_natl_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  save(bb.fixed.natl.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_fixed_natl_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  # save results
  save(bb.res.natl.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_res_natl_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  
  #### Admin1 Unstrat ----------------------------------------------
  bb.adm1.unstrat.nmr.allsurveys <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                Amat=admin1.mat, admin.level='Admin1',
                                stratified=F, weight.strata=NULL,
                                outcome='nmr',
                                time.model=time_mod, st.time.model=time_mod,
                                adj.frame=adj.frame, adj.varnames=adj.varnames,
                                doBenchmark=F,nsim = 1000)
  
  bb.fit.adm1.unstrat.nmr.allsurveys <- bb.adm1.unstrat.nmr.allsurveys[[1]]
  bb.res.adm1.unstrat.nmr.allsurveys <- bb.adm1.unstrat.nmr.allsurveys[[2]]
  
  bb.temporals.adm1.unstrat.nmr.allsurveys <- getDiag(bb.adm1.unstrat.nmr.allsurveys$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm1.unstrat.nmr.allsurveys <- bb.fit.adm1.unstrat.nmr.allsurveys$fit$summary.hyperpar
  bb.fixed.adm1.unstrat.nmr.allsurveys <- bb.fit.adm1.unstrat.nmr.allsurveys$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm1_', time_mod, '_unstrat_nmr_allsurveys.txt'))
  summary(bb.fit.adm1.unstrat.nmr.allsurveys)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm1.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_temporals_adm1_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  save(bb.hyperpar.adm1.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm1_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  save(bb.fixed.adm1.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_fixed_adm1_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  # save results
  save(bb.res.adm1.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_res_adm1_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  
  #### Admin2 Unstrat ----------------------------------------------
  bb.adm2.unstrat.nmr.allsurveys <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                           Amat=admin2.mat, admin.level='Admin2',
                                           stratified=F, weight.strata=NULL,
                                           outcome='nmr',
                                           time.model=time_mod, st.time.model=time_mod,
                                           adj.frame=adj.frame, adj.varnames=adj.varnames,
                                           doBenchmark=F,nsim = 1000)
  
  bb.fit.adm2.unstrat.nmr.allsurveys <- bb.adm2.unstrat.nmr.allsurveys[[1]]
  bb.res.adm2.unstrat.nmr.allsurveys <- bb.adm2.unstrat.nmr.allsurveys[[2]]
  
  bb.temporals.adm2.unstrat.nmr.allsurveys <- getDiag(bb.adm2.unstrat.nmr.allsurveys$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm2.unstrat.nmr.allsurveys <- bb.fit.adm2.unstrat.nmr.allsurveys$fit$summary.hyperpar
  bb.fixed.adm2.unstrat.nmr.allsurveys <- bb.fit.adm2.unstrat.nmr.allsurveys$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm2_', time_mod, '_unstrat_nmr_allsurveys.txt'))
  summary(bb.fit.adm2.unstrat.nmr.allsurveys)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm2.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_temporals_adm2_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  save(bb.hyperpar.adm2.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm2_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  save(bb.fixed.adm2.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_fixed_adm2_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  # save results
  save(bb.res.adm2.unstrat.nmr.allsurveys,file=paste0('Betabinomial/NMR/',country,'_res_adm2_', time_mod, '_unstrat_nmr_allsurveys.rda'))
  
### U5MR -----------------------------------------------
  setwd(paste0(res.dir))
  #### National Unstrat ----------------------------------------------
  bb.natl.unstrat.u5.allsurveys <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                           Amat=NULL, admin.level='National',
                                           stratified=F, weight.strata=NULL,
                                           outcome='u5mr', time.model=time_mod,
                                           adj.frame=adj.frame, adj.varnames=adj.varnames,
                                           doBenchmark=F,nsim = 1000)
  
  bb.fit.natl.unstrat.u5.allsurveys <- bb.natl.unstrat.u5.allsurveys[[1]]
  bb.res.natl.unstrat.u5.allsurveys <- bb.natl.unstrat.u5.allsurveys[[2]]
  
  bb.temporals.natl.unstrat.u5.allsurveys <- getDiag(bb.natl.unstrat.u5.allsurveys$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.natl.unstrat.u5.allsurveys <- bb.fit.natl.unstrat.u5.allsurveys$fit$summary.hyperpar
  bb.fixed.natl.unstrat.u5.allsurveys <- bb.fit.natl.unstrat.u5.allsurveys$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/U5MR/',country,'_fit_natl_', time_mod, '_unstrat_u5_allsurveys.txt'))
  summary(bb.fit.natl.unstrat.u5.allsurveys)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.natl.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_temporals_natl_', time_mod, '_unstrat_u5_allsurveys.rda'))
  save(bb.hyperpar.natl.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_natl_', time_mod, '_unstrat_u5_allsurveys.rda'))
  save(bb.fixed.natl.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_fixed_natl_', time_mod, '_unstrat_u5_allsurveys.rda'))
  # save results
  save(bb.res.natl.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_res_natl_', time_mod, '_unstrat_u5_allsurveys.rda'))
  
  #### Admin1 Unstrat ----------------------------------------------
  bb.adm1.unstrat.u5.allsurveys <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                           Amat=admin1.mat, admin.level='Admin1',
                                           stratified=F, weight.strata=NULL,
                                           outcome='u5mr',
                                           time.model=time_mod, st.time.model=time_mod,
                                           adj.frame=adj.frame, adj.varnames=adj.varnames,
                                           doBenchmark=F,nsim = 1000)
  
  bb.fit.adm1.unstrat.u5.allsurveys <- bb.adm1.unstrat.u5.allsurveys[[1]]
  bb.res.adm1.unstrat.u5.allsurveys <- bb.adm1.unstrat.u5.allsurveys[[2]]
  
  bb.temporals.adm1.unstrat.u5.allsurveys <- getDiag(bb.adm1.unstrat.u5.allsurveys$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm1.unstrat.u5.allsurveys <- bb.fit.adm1.unstrat.u5.allsurveys$fit$summary.hyperpar
  bb.fixed.adm1.unstrat.u5.allsurveys <- bb.fit.adm1.unstrat.u5.allsurveys$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_', time_mod, '_unstrat_u5_allsurveys.txt'))
  summary(bb.fit.adm1.unstrat.u5.allsurveys)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm1.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm1_', time_mod, '_unstrat_u5_allsurveys.rda'))
  save(bb.hyperpar.adm1.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm1_', time_mod, '_unstrat_u5_allsurveys.rda'))
  save(bb.fixed.adm1.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm1_', time_mod, '_unstrat_u5_allsurveys.rda'))
  # save results
  save(bb.res.adm1.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_res_adm1_', time_mod, '_unstrat_u5_allsurveys.rda'))
  
  #### Admin2 Unstrat ----------------------------------------------
  bb.adm2.unstrat.u5.allsurveys <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                          Amat=admin2.mat, admin.level='Admin2',
                                          stratified=F, weight.strata=NULL,
                                          outcome='u5mr',
                                          time.model=time_mod, st.time.model=time_mod,
                                          adj.frame=adj.frame, adj.varnames=adj.varnames,
                                          doBenchmark=F,nsim = 1000)
  
  bb.fit.adm2.unstrat.u5.allsurveys <- bb.adm2.unstrat.u5.allsurveys[[1]]
  bb.res.adm2.unstrat.u5.allsurveys <- bb.adm2.unstrat.u5.allsurveys[[2]]
  
  bb.temporals.adm2.unstrat.u5.allsurveys <- getDiag(bb.adm2.unstrat.u5.allsurveys$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm2.unstrat.u5.allsurveys <- bb.fit.adm2.unstrat.u5.allsurveys$fit$summary.hyperpar
  bb.fixed.adm2.unstrat.u5.allsurveys <- bb.fit.adm2.unstrat.u5.allsurveys$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_', time_mod, '_unstrat_u5_allsurveys.txt'))
  summary(bb.fit.adm2.unstrat.u5.allsurveys)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm2.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm2_', time_mod, '_unstrat_u5_allsurveys.rda'))
  save(bb.hyperpar.adm2.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm2_', time_mod, '_unstrat_u5_allsurveys.rda'))
  save(bb.fixed.adm2.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm2_', time_mod, '_unstrat_u5_allsurveys.rda'))
  # save results
  save(bb.res.adm2.unstrat.u5.allsurveys,file=paste0('Betabinomial/U5MR/',country,'_res_adm2_', time_mod, '_unstrat_u5_allsurveys.rda'))
## Get benchmarked unstratified models (using all surveys)  -----------------------------------------------
### NMR -----------------------------------------------
  #### Admin1 Unstrat,  Benchmarked ----------------------------------------------
  bb.adm1.strat.nmr.allsurveys.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                    Amat=admin1.mat, admin.level='Admin1',
                                    stratified=F, weight.strata=NULL,
                                    outcome='nmr',
                                    time.model=time_mod, st.time.model=time_mod,
                                    adj.frame=adj.frame, adj.varnames=adj.varnames,
                                    weight.region = weight.adm1.u1,
                                    igme.ests = igme.ests.nmr,
                                    int.priors.prec.bench = 10, # parameter to play around with if getting very low acceptance
                                    doBenchmark=T, nsim = 1000)
  
  bb.fit.adm1.strat.nmr.allsurveys.bench <- bb.adm1.strat.nmr.allsurveys.bench[[1]]
  bb.res.adm1.strat.nmr.allsurveys.bench <- bb.adm1.strat.nmr.allsurveys.bench[[2]]
  
  bb.temporals.adm1.strat.nmr.allsurveys.bench <- getDiag(bb.adm1.strat.nmr.allsurveys.bench$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm1.strat.nmr.allsurveys.bench <- bb.fit.adm1.strat.nmr.allsurveys.bench$fit$summary.hyperpar
  bb.fixed.adm1.strat.nmr.allsurveys.bench <- bb.fit.adm1.strat.nmr.allsurveys.bench$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm1_', time_mod, '_strat_nmr_allsurveys_bench.txt'))
  summary(bb.fit.adm1.strat.nmr.allsurveys.bench)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm1.strat.nmr.allsurveys.bench,file=paste0('Betabinomial/NMR/',country,'_temporals_adm1_', time_mod, '_strat_nmr_allsurveys_bench.rda'))
  save(bb.hyperpar.adm1.strat.nmr.allsurveys.bench,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm1_', time_mod, '_strat_nmr_allsurveys_bench.rda'))
  save(bb.fixed.adm1.strat.nmr.allsurveys.bench,file=paste0('Betabinomial/NMR/',country,'_fixed_adm1_', time_mod, '_strat_nmr_allsurveys_bench.rda'))
  # save results
  save(bb.res.adm1.strat.nmr.allsurveys.bench,file=paste0('Betabinomial/NMR/',country,'_res_adm1_', time_mod, '_strat_nmr_allsurveys_bench.rda'))
  
  # save traceplots from benchmarking for 12 random combinations of admin area and year
  pdf(file = paste0('Betabinomial/NMR/',country,'_adm1_', time_mod, '_strat_nmr_allsurveys_bench_trace.pdf'))
  par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
  ind <- sort(sample(1:length(bb.res.adm1.strat.nmr.allsurveys.bench$draws.est.overall),12))
  for(i in ind){
    bb.draws.tmp <- bb.res.adm1.strat.nmr.allsurveys.bench$draws.est.overall[[i]]$draws
    plot(bb.draws.tmp,type='l', ylab=' ',xlab='',
         main=paste0(bb.res.adm1.strat.nmr.allsurveys.bench$draws.est.overall[[i]]$region,", ", bb.res.adm1.strat.nmr.allsurveys.bench$draws.est.overall[[i]]$years))
  }
  dev.off()
  
  #### Admin2 Unstrat, Benchmarked ----------------------------------------------
  bb.adm2.strat.nmr.allsurveys.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                               Amat=admin2.mat, admin.level='Admin2',
                                               stratified=F, weight.strata=NULL,
                                               outcome='nmr',
                                               time.model=time_mod, st.time.model=time_mod,
                                               adj.frame=adj.frame, adj.varnames=adj.varnames,
                                               weight.region = weight.adm2.u1,
                                               igme.ests = igme.ests.nmr,
                                               int.priors.prec.bench = 10, # parameter to play around with if getting very low acceptance
                                               doBenchmark=T, nsim = 1000)
  
  bb.fit.adm2.strat.nmr.allsurveys.bench <- bb.adm2.strat.nmr.allsurveys.bench[[1]]
  bb.res.adm2.strat.nmr.allsurveys.bench <- bb.adm2.strat.nmr.allsurveys.bench[[2]]
  
  bb.temporals.adm2.strat.nmr.allsurveys.bench <- getDiag(bb.adm2.strat.nmr.allsurveys.bench$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm2.strat.nmr.allsurveys.bench <- bb.fit.adm2.strat.nmr.allsurveys.bench$fit$summary.hyperpar
  bb.fixed.adm2.strat.nmr.allsurveys.bench <- bb.fit.adm2.strat.nmr.allsurveys.bench$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm2_', time_mod, '_strat_nmr_allsurveys_bench.txt'))
  summary(bb.fit.adm2.strat.nmr.allsurveys.bench)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm2.strat.nmr.allsurveys.bench,file=paste0('Betabinomial/NMR/',country,'_temporals_adm2_', time_mod, '_strat_nmr_allsurveys_bench.rda'))
  save(bb.hyperpar.adm2.strat.nmr.allsurveys.bench,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm2_', time_mod, '_strat_nmr_allsurveys_bench.rda'))
  save(bb.fixed.adm2.strat.nmr.allsurveys.bench,file=paste0('Betabinomial/NMR/',country,'_fixed_adm2_', time_mod, '_strat_nmr_allsurveys_bench.rda'))
  # save results
  save(bb.res.adm2.strat.nmr.allsurveys.bench,file=paste0('Betabinomial/NMR/',country,'_res_adm2_', time_mod, '_strat_nmr_allsurveys_bench.rda'))
  
  # save traceplots from benchmarking for 12 random combinations of admin area and year
  pdf(file = paste0('Betabinomial/NMR/',country,'_adm2_', time_mod, '_strat_nmr_allsurveys_bench_trace.pdf'))
  par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
  ind <- sort(sample(1:length(bb.res.adm2.strat.nmr.allsurveys.bench$draws.est.overall),12))
  for(i in ind){
    bb.draws.tmp <- bb.res.adm2.strat.nmr.allsurveys.bench$draws.est.overall[[i]]$draws
    plot(bb.draws.tmp,type='l', ylab=' ',xlab='',
         main=paste0(bb.res.adm2.strat.nmr.allsurveys.bench$draws.est.overall[[i]]$region,", ", bb.res.adm2.strat.nmr.allsurveys.bench$draws.est.overall[[i]]$years))
  }
  dev.off()
### U5MR -----------------------------------------------
  #### Admin1 Unstrat,  Benchmarked ----------------------------------------------
  bb.adm1.strat.u5.allsurveys.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                               Amat=admin1.mat, admin.level='Admin1',
                                               stratified=F, weight.strata=NULL,
                                               outcome='u5mr',
                                               time.model=time_mod, st.time.model=time_mod,
                                               adj.frame=adj.frame, adj.varnames=adj.varnames,
                                               weight.region = weight.adm1.u1,
                                               igme.ests = igme.ests.u5,
                                               int.priors.prec.bench = 10, # parameter to play around with if getting very low acceptance
                                               doBenchmark=T, nsim = 1000)
  
  bb.fit.adm1.strat.u5.allsurveys.bench <- bb.adm1.strat.u5.allsurveys.bench[[1]]
  bb.res.adm1.strat.u5.allsurveys.bench <- bb.adm1.strat.u5.allsurveys.bench[[2]]
  
  bb.temporals.adm1.strat.u5.allsurveys.bench <- getDiag(bb.adm1.strat.u5.allsurveys.bench$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm1.strat.u5.allsurveys.bench <- bb.fit.adm1.strat.u5.allsurveys.bench$fit$summary.hyperpar
  bb.fixed.adm1.strat.u5.allsurveys.bench <- bb.fit.adm1.strat.u5.allsurveys.bench$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_', time_mod, '_strat_u5_allsurveys_bench.txt'))
  summary(bb.fit.adm1.strat.u5.allsurveys.bench)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm1.strat.u5.allsurveys.bench,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm1_', time_mod, '_strat_u5_allsurveys_bench.rda'))
  save(bb.hyperpar.adm1.strat.u5.allsurveys.bench,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm1_', time_mod, '_strat_u5_allsurveys_bench.rda'))
  save(bb.fixed.adm1.strat.u5.allsurveys.bench,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm1_', time_mod, '_strat_u5_allsurveys_bench.rda'))
  # save results
  save(bb.res.adm1.strat.u5.allsurveys.bench,file=paste0('Betabinomial/U5MR/',country,'_res_adm1_', time_mod, '_strat_u5_allsurveys_bench.rda'))
  
  # save traceplots from benchmarking for 12 random combinations of admin area and year
  pdf(file = paste0('Betabinomial/U5MR/',country,'_adm1_', time_mod, '_strat_u5_allsurveys_bench_trace.pdf'))
  par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
  ind <- sort(sample(1:length(bb.res.adm1.strat.u5.allsurveys.bench$draws.est.overall),12))
  for(i in ind){
    bb.draws.tmp <- bb.res.adm1.strat.u5.allsurveys.bench$draws.est.overall[[i]]$draws
    plot(bb.draws.tmp,type='l', ylab=' ',xlab='',
         main=paste0(bb.res.adm1.strat.u5.allsurveys.bench$draws.est.overall[[i]]$region,", ", bb.res.adm1.strat.u5.allsurveys.bench$draws.est.overall[[i]]$years))
  }
  dev.off()
  
  #### Admin2 Unstrat, Benchmarked ----------------------------------------------
  bb.adm2.strat.u5.allsurveys.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                               Amat=admin2.mat, admin.level='Admin2',
                                               stratified=F, weight.strata=NULL,
                                               outcome='u5mr',
                                               time.model=time_mod, st.time.model=time_mod,
                                               adj.frame=adj.frame, adj.varnames=adj.varnames,
                                               weight.region = weight.adm2.u1,
                                               igme.ests = igme.ests.u5,
                                               int.priors.prec.bench = 10, # parameter to play around with if getting very low acceptance
                                               doBenchmark=T, nsim = 1000)
  
  bb.fit.adm2.strat.u5.allsurveys.bench <- bb.adm2.strat.u5.allsurveys.bench[[1]]
  bb.res.adm2.strat.u5.allsurveys.bench <- bb.adm2.strat.u5.allsurveys.bench[[2]]
  
  bb.temporals.adm2.strat.u5.allsurveys.bench <- getDiag(bb.adm2.strat.u5.allsurveys.bench$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm2.strat.u5.allsurveys.bench <- bb.fit.adm2.strat.u5.allsurveys.bench$fit$summary.hyperpar
  bb.fixed.adm2.strat.u5.allsurveys.bench <- bb.fit.adm2.strat.u5.allsurveys.bench$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_', time_mod, '_strat_u5_allsurveys_bench.txt'))
  summary(bb.fit.adm2.strat.u5.allsurveys.bench)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm2.strat.u5.allsurveys.bench,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm2_', time_mod, '_strat_u5_allsurveys_bench.rda'))
  save(bb.hyperpar.adm2.strat.u5.allsurveys.bench,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm2_', time_mod, '_strat_u5_allsurveys_bench.rda'))
  save(bb.fixed.adm2.strat.u5.allsurveys.bench,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm2_', time_mod, '_strat_u5_allsurveys_bench.rda'))
  # save results
  save(bb.res.adm2.strat.u5.allsurveys.bench,file=paste0('Betabinomial/U5MR/',country,'_res_adm2_', time_mod, '_strat_u5_allsurveys_bench.rda'))
  
  # save traceplots from benchmarking for 12 random combinations of admin area and year
  pdf(file = paste0('Betabinomial/U5MR/',country,'_adm2_', time_mod, '_strat_u5_allsurveys_bench_trace.pdf'))
  par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
  ind <- sort(sample(1:length(bb.res.adm2.strat.u5.allsurveys.bench$draws.est.overall),12))
  for(i in ind){
    bb.draws.tmp <- bb.res.adm2.strat.u5.allsurveys.bench$draws.est.overall[[i]]$draws
    plot(bb.draws.tmp,type='l', ylab=' ',xlab='',
         main=paste0(bb.res.adm2.strat.u5.allsurveys.bench$draws.est.overall[[i]]$region,", ", bb.res.adm2.strat.u5.allsurveys.bench$draws.est.overall[[i]]$years))
  }
  dev.off()
  