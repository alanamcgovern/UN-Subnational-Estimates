rm(list = ls())
## ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Lesotho'

## Libraries -----------------------------------------------
library(SUMMER)
library(INLA)
options(gsubfn.engine = "R")
library(rgdal)
library(tidyverse)

## Retrieve directories and country info -----------------------------------------------
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

source(file=paste0(home.dir, '/Rcode/getBB8.R'))
source(file=paste0(home.dir, '/Rcode/smoothCluster_mod.R'))

## Load polygon files -----------------------------------------------
setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm0)) # load the national shape file
poly.adm1 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions
if(exists('poly.layer.adm2')){
  poly.adm2 <- readOGR(dsn = poly.path,
                       layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions
  proj4string(poly.adm0) <- proj4string(poly.adm1) <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0(poly.path,'/', country, '_Amat.rda'))
load(paste0(poly.path,'/', country, '_Amat_Names.rda'))

## Load data -----------------------------------------------
load(paste0(country,'_cluster_dat_1frame.rda'),
     envir = .GlobalEnv)

mod.dat$years <- as.numeric(as.character(mod.dat$years))
mod.dat$country <- as.character(country)
survey_years <- unique(mod.dat$survey)
end.proj.year <- 2021

## Load National IGME estimates ------------------------------------------------------
setwd(paste0(home.dir,'/Data/IGME'))

## U5MR
igme.ests.u5 <- read.csv(paste0(country.abbrev,'_u5_igme_est.csv'), header = T)
names(igme.ests.u5) <- c('year','OBS_VALUE','LOWER_BOUND','UPPER_BOUND')
igme.ests.u5$year <- igme.ests.u5$year-0.5
igme.ests.u5$OBS_VALUE <- igme.ests.u5$OBS_VALUE/1000
igme.ests.u5 <- igme.ests.u5[igme.ests.u5$year %in% beg.year:end.proj.year,]
igme.ests.u5 <- igme.ests.u5[order(igme.ests.u5$year),]
igme.ests.u5$SD <- (igme.ests.u5$UPPER_BOUND - igme.ests.u5$LOWER_BOUND)/(2*1.645*1000)
igme.ests.u5$LOWER_BOUND <- igme.ests.u5$OBS_VALUE - 1.96*igme.ests.u5$SD
igme.ests.u5$UPPER_BOUND <- igme.ests.u5$OBS_VALUE + 1.96*igme.ests.u5$SD

## NMR
igme.ests.nmr <- read.csv(paste0(country.abbrev,'_nmr_igme_est.csv'),  header = T)
names(igme.ests.nmr) <- c('year','OBS_VALUE','LOWER_BOUND','UPPER_BOUND')
igme.ests.nmr$year <- igme.ests.nmr$year-0.5
igme.ests.nmr$OBS_VALUE <- igme.ests.nmr$OBS_VALUE/1000
igme.ests.nmr <- igme.ests.nmr[igme.ests.nmr$year %in% beg.year:end.proj.year,]
igme.ests.nmr <- igme.ests.nmr[order(igme.ests.nmr$year),]
igme.ests.nmr$SD <- (igme.ests.nmr$UPPER_BOUND - igme.ests.nmr$LOWER_BOUND)/(2*1.645*1000)
igme.ests.nmr$LOWER_BOUND <- igme.ests.nmr$OBS_VALUE - 1.96*igme.ests.nmr$SD
igme.ests.nmr$UPPER_BOUND <- igme.ests.nmr$OBS_VALUE + 1.96*igme.ests.nmr$SD

## Load Admin 1 and 2 population proportions (for benchmarking) ------------------------------------------------------

load(paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm1_weights_u5.rda'))
if(exists('poly.adm2')){
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
  if(country == "Kenya"){
    load(paste0(folder.name,
                '/', country,
                '_HIVNameKey.rda'), envir = .GlobalEnv)
    adj.frame$area <- tolower(adj.frame$area)
    hiv.name.key$OTHREGNA <- gsub(" ", "", hiv.name.key$OTHREGNA)
    hiv.name.key$OTHREGNA <- tolower(hiv.name.key$OTHREGNA)
    mod.dat$area <- hiv.name.key$OTHREGNA[match(mod.dat$admin1.name,
                                                hiv.name.key$DHSREGEN)]
  }else if(sum(!(admin1.names$GADM %in% hiv.adj$area)) != 0){
    mod.dat$area <- mod.dat$admin1.name
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
  
  if(exists('poly.adm2')){
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
    if(exists('poly.adm2')){
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
    if(exists('poly.adm2')){
    weight.strata.adm2.u1 <- weight.strata.adm2.u1[weight.strata.adm2.u1$years<=end.proj.year,]
    weight.strata.adm2.u5 <- weight.strata.adm2.u5[weight.strata.adm2.u5$years<=end.proj.year,]
    }
  }
}

## Fit BB8 models -----------------------------------------------
setwd(paste0(res.dir))

### NMR -----------------------------------------------
  #### National Unstrat ----------------------------------------------

bb.natl.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                              Amat=NULL, admin.level='National',
                              stratified=F, weight.strata=NULL,
                              outcome='nmr', time.model='ar1',
                              adj.frame=adj.frame, adj.varnames=adj.varnames,
                              doBenchmark=F,nsim = 1000)

bb.fit.natl.unstrat.nmr <- bb.natl.unstrat.nmr[[1]]
bb.res.natl.unstrat.nmr <- bb.natl.unstrat.nmr[[2]]

bb.temporals.natl.unstrat.nmr <- getDiag(bb.natl.unstrat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.natl.unstrat.nmr <- bb.fit.natl.unstrat.nmr$fit$summary.hyperpar
bb.fixed.natl.unstrat.nmr <- bb.fit.natl.unstrat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_natl_unstrat_nmr.txt'))
summary(bb.fit.natl.unstrat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.natl.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_natl_unstrat_nmr.rda'))
save(bb.hyperpar.natl.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_natl_unstrat_nmr.rda'))
save(bb.fixed.natl.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_natl_unstrat_nmr.rda'))
# save results
save(bb.res.natl.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_natl_unstrat_nmr.rda'))

  #### National Strat -----------------------------------------------
bb.natl.strat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                            Amat=NULL, admin.level='National',
                            stratified=T, weight.strata=weight.strata.natl.u1,
                            outcome='nmr', time.model='ar1',
                            adj.frame=adj.frame, adj.varnames=adj.varnames,
                            doBenchmark=F,nsim = 1000)
bb.fit.natl.strat.nmr <- bb.natl.strat.nmr[[1]]
bb.res.natl.strat.nmr <- bb.natl.strat.nmr[[2]]

bb.temporals.natl.strat.nmr <- getDiag(bb.natl.strat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.natl.strat.nmr <- bb.fit.natl.strat.nmr$fit$summary.hyperpar
bb.fixed.natl.strat.nmr <- bb.fit.natl.strat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_natl_strat_nmr.txt'))
summary(bb.fit.natl.strat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.natl.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_natl_strat_nmr.rda'))
save(bb.hyperpar.natl.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_natl_strat_nmr.rda'))
save(bb.fixed.natl.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_natl_strat_nmr.rda'))
# save results
save(bb.res.natl.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_natl_strat_nmr.rda'))

  #### Admin1 Unstrat ----------------------------------------------
bb.adm1.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                              Amat=admin1.mat, admin.level='Admin1',
                              stratified=F, weight.strata=NULL,
                              outcome='nmr',
                              time.model='ar1', st.time.model='ar1',
                              adj.frame=adj.frame, adj.varnames=adj.varnames,
                              doBenchmark=F,nsim = 1000)

bb.fit.adm1.unstrat.nmr <- bb.adm1.unstrat.nmr[[1]]
bb.res.adm1.unstrat.nmr <- bb.adm1.unstrat.nmr[[2]]

bb.temporals.adm1.unstrat.nmr <- getDiag(bb.adm1.unstrat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm1.unstrat.nmr <- bb.fit.adm1.unstrat.nmr$fit$summary.hyperpar
bb.fixed.adm1.unstrat.nmr <- bb.fit.adm1.unstrat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm1_unstrat_nmr.txt'))
summary(bb.fit.adm1.unstrat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm1.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_adm1_unstrat_nmr.rda'))
save(bb.hyperpar.adm1.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm1_unstrat_nmr.rda'))
save(bb.fixed.adm1.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_adm1_unstrat_nmr.rda'))
# save results
save(bb.res.adm1.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm1_unstrat_nmr.rda'))

  #### Admin1 Strat -----------------------------------------------
bb.adm1.strat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                            Amat=admin1.mat, admin.level='Admin1',
                            stratified=T, weight.strata=weight.strata.adm1.u1,
                            outcome='nmr',
                            time.model='ar1', st.time.model='ar1',
                            adj.frame=adj.frame, adj.varnames=adj.varnames,
                            doBenchmark=F,nsim = 1000)
bb.fit.adm1.strat.nmr <- bb.adm1.strat.nmr[[1]]
bb.res.adm1.strat.nmr <- bb.adm1.strat.nmr[[2]]

bb.temporals.adm1.strat.nmr <- getDiag(bb.adm1.strat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm1.strat.nmr <- bb.fit.adm1.strat.nmr$fit$summary.hyperpar
bb.fixed.adm1.strat.nmr <- bb.fit.adm1.strat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm1_strat_nmr.txt'))
summary(bb.fit.adm1.strat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm1.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_adm1_strat_nmr.rda'))
save(bb.hyperpar.adm1.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm1_strat_nmr.rda'))
save(bb.fixed.adm1.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_adm1_strat_nmr.rda'))
# save results
save(bb.res.adm1.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm1_strat_nmr.rda'))

  #### Admin1 Strat, Benchmarked -----------------------------------------------
bb.adm1.strat.nmr.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                  Amat=admin1.mat, admin.level='Admin1',
                                  stratified=T, weight.strata=weight.strata.adm1.u1,
                                  outcome='nmr',
                                  time.model='ar1', st.time.model='ar1',
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
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm1_strat_nmr_bench.txt'))
summary(bb.fit.adm1.strat.nmr.bench)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm1.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_temporals_adm1_strat_nmr_bench.rda'))
save(bb.hyperpar.adm1.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm1_strat_nmr_bench.rda'))
save(bb.fixed.adm1.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_fixed_adm1_strat_nmr_bench.rda'))
# save results
save(bb.res.adm1.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_res_adm1_strat_nmr_bench.rda'))

# save traceplots from benchmarking for 12 random combinations of admin area and year
pdf(file = paste0('Betabinomial/NMR/',country,'_adm1_strat_nmr_bench_trace.pdf'))
par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
ind <- sort(sample(1:length(bb.res.adm1.strat.nmr.bench$draws.est.overall),12))
for(i in ind){
  bb.draws.tmp <- bb.res.adm1.strat.nmr.bench$draws.est.overall[[i]]$draws
  plot(bb.draws.tmp,type='l', ylab=' ',xlab='',
       main=paste0(bb.res.adm1.strat.nmr.bench$draws.est.overall[[i]]$region,", ", bb.res.adm1.strat.nmr.bench$draws.est.overall[[i]]$years))
}
dev.off()

  #### Admin2 Unstrat ----------------------------------------------
bb.adm2.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                              Amat=admin2.mat, admin.level='Admin2',
                              stratified=F, weight.strata=NULL,
                              outcome='nmr',
                              time.model='ar1', st.time.model='ar1',
                              adj.frame=adj.frame, adj.varnames=adj.varnames,
                              doBenchmark=F,nsim = 1000)

bb.fit.adm2.unstrat.nmr <- bb.adm2.unstrat.nmr[[1]]
bb.res.adm2.unstrat.nmr <- bb.adm2.unstrat.nmr[[2]]

bb.temporals.adm2.unstrat.nmr <- getDiag(bb.adm2.unstrat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm2.unstrat.nmr <- bb.fit.adm2.unstrat.nmr$fit$summary.hyperpar
bb.fixed.adm2.unstrat.nmr <- bb.fit.adm2.unstrat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm2_unstrat_nmr.txt'))
summary(bb.fit.adm2.unstrat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm2.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_adm2_unstrat_nmr.rda'))
save(bb.hyperpar.adm2.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm2_unstrat_nmr.rda'))
save(bb.fixed.adm2.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_adm2_unstrat_nmr.rda'))
# save results
save(bb.res.adm2.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm2_unstrat_nmr.rda'))

  #### Admin2 Strat -----------------------------------------------
bb.adm2.strat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                            Amat=admin2.mat, admin.level='Admin2',
                            stratified=T, weight.strata=weight.strata.adm2.u1,
                            outcome='nmr',
                            time.model='ar1', st.time.model='ar1',
                            adj.frame=adj.frame, adj.varnames=adj.varnames,
                            doBenchmark=F,nsim = 1000)
bb.fit.adm2.strat.nmr <- bb.adm2.strat.nmr[[1]]
bb.res.adm2.strat.nmr <- bb.adm2.strat.nmr[[2]]

bb.temporals.adm2.strat.nmr <- getDiag(bb.adm2.strat.nmr$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm2.strat.nmr <- bb.fit.adm2.strat.nmr$fit$summary.hyperpar
bb.fixed.adm2.strat.nmr <- bb.fit.adm2.strat.nmr$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm2_strat_nmr.txt'))
summary(bb.fit.adm2.strat.nmr)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm2.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_temporals_adm2_strat_nmr.rda'))
save(bb.hyperpar.adm2.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm2_strat_nmr.rda'))
save(bb.fixed.adm2.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_fixed_adm2_strat_nmr.rda'))
# save results
save(bb.res.adm2.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm2_strat_nmr.rda'))

  #### Admin2 Strat, Benchmarked -----------------------------------------------
bb.adm2.strat.nmr.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                  Amat=admin2.mat, admin.level='Admin2',
                                  stratified=T, weight.strata=weight.strata.adm2.u1,
                                  outcome='nmr',
                                  time.model='ar1', st.time.model='ar1',
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
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm2_strat_nmr_bench.txt'))
summary(bb.fit.adm2.strat.nmr.bench)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm2.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_temporals_adm2_strat_nmr_bench.rda'))
save(bb.hyperpar.adm2.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_hyperpar_adm2_strat_nmr_bench.rda'))
save(bb.fixed.adm2.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_fixed_adm2_strat_nmr_bench.rda'))
# save results
save(bb.res.adm2.strat.nmr.bench,file=paste0('Betabinomial/NMR/',country,'_res_adm2_strat_nmr_bench.rda'))

# save traceplots from benchmarking for 12 random combinations of admin area and year
pdf(file = paste0('Betabinomial/NMR/',country,'_adm2_strat_nmr_bench_trace.pdf'))
par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
ind <- sort(sample(1:length(bb.res.adm2.strat.nmr.bench$draws.est.overall),12))
for(i in ind){
  bb.draws.tmp <- bb.res.adm2.strat.nmr.bench$draws.est.overall[[i]]$draws
  plot(bb.draws.tmp,type='l',
       ylab=' ',xlab='',main=paste0(bb.res.adm2.strat.nmr.bench$draws.est.overall[[i]]$region,", ", bb.res.adm2.strat.nmr.bench$draws.est.overall[[i]]$years))
}
dev.off()

### U5MR -----------------------------------------------
  #### National Unstrat ----------------------------------------------
bb.natl.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=NULL, admin.level='National',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr', time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim=1000)
bb.fit.natl.unstrat.u5 <- bb.natl.unstrat.u5[[1]]
bb.res.natl.unstrat.u5 <- bb.natl.unstrat.u5[[2]]

bb.temporals.natl.unstrat.u5 <- getDiag(bb.natl.unstrat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.natl.unstrat.u5 <- bb.fit.natl.unstrat.u5$fit$summary.hyperpar
bb.fixed.natl.unstrat.u5 <- bb.fit.natl.unstrat.u5$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_natl_unstrat_u5.txt'))
summary(bb.fit.natl.unstrat.u5)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.natl.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_natl_unstrat_u5.rda'))
save(bb.hyperpar.natl.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_natl_unstrat_u5.rda'))
save(bb.fixed.natl.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_natl_unstrat_u5.rda'))
# save results
save(bb.res.natl.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_natl_unstrat_u5.rda'))


  #### National Strat -----------------------------------------------
bb.natl.strat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                           Amat=NULL, admin.level='National',
                           stratified=T, weight.strata=weight.strata.natl.u5,
                           outcome='u5mr', time.model='ar1',
                           adj.frame=adj.frame, adj.varnames=adj.varnames,
                           doBenchmark=F,nsim=1000)
bb.fit.natl.strat.u5 <- bb.natl.strat.u5[[1]]
bb.res.natl.strat.u5 <- bb.natl.strat.u5[[2]]

bb.temporals.natl.strat.u5 <- getDiag(bb.natl.strat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.natl.strat.u5 <- bb.fit.natl.strat.u5$fit$summary.hyperpar
bb.fixed.natl.strat.u5 <- bb.fit.natl.strat.u5$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_natl_strat_u5.txt'))
summary(bb.fit.natl.strat.u5)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.natl.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_natl_strat_u5.rda'))
save(bb.hyperpar.natl.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_natl_strat_u5.rda'))
save(bb.fixed.natl.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_natl_strat_u5.rda'))
# save results
save(bb.res.natl.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_natl_strat_u5.rda'))

  #### Admin1 Unstrat ----------------------------------------------
bb.adm1.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin1.mat, admin.level='Admin1',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 1000)
bb.fit.adm1.unstrat.u5 <- bb.adm1.unstrat.u5[[1]]
bb.res.adm1.unstrat.u5 <- bb.adm1.unstrat.u5[[2]]

bb.temporals.adm1.unstrat.u5 <- getDiag(bb.adm1.unstrat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm1.unstrat.u5 <- bb.fit.adm1.unstrat.u5$fit$summary.hyperpar
bb.fixed.adm1.unstrat.u5 <- bb.fit.adm1.unstrat.u5$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_unstrat_u5.txt'))
summary(bb.fit.adm1.unstrat.u5)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm1.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm1_unstrat_u5.rda'))
save(bb.hyperpar.adm1.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm1_unstrat_u5.rda'))
save(bb.fixed.adm1.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm1_unstrat_u5.rda'))
# save results
save(bb.res.adm1.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm1_unstrat_u5.rda'))

  #### Admin1 Strat -----------------------------------------------

bb.adm1.strat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                           Amat=admin1.mat, admin.level='Admin1',
                           stratified=T, weight.strata=weight.strata.adm1.u5,
                           outcome='u5mr',
                           time.model='ar1', st.time.model='ar1',
                           adj.frame=adj.frame, adj.varnames=adj.varnames,
                           doBenchmark=F,nsim=1000)
bb.fit.adm1.strat.u5 <- bb.adm1.strat.u5[[1]]
bb.res.adm1.strat.u5 <- bb.adm1.strat.u5[[2]]

bb.temporals.adm1.strat.u5 <- getDiag(bb.adm1.strat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm1.strat.u5 <- bb.fit.adm1.strat.u5$fit$summary.hyperpar
bb.fixed.adm1.strat.u5 <- bb.fit.adm1.strat.u5$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_strat_u5.txt'))
summary(bb.fit.adm1.strat.u5)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm1.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm1_strat_u5.rda'))
save(bb.hyperpar.adm1.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm1_strat_u5.rda'))
save(bb.fixed.adm1.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm1_strat_u5.rda'))
# save results
save(bb.res.adm1.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm1_strat_u5.rda'))

  #### Admin1 Strat, Benchmarked -----------------------------------------------

bb.adm1.strat.u5.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                                 Amat=admin1.mat, admin.level='Admin1',
                                 stratified=T, weight.strata=weight.strata.adm1.u5,
                                 outcome='u5mr',
                                 time.model='ar1', st.time.model='ar1',
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
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_strat_u5_bench.txt'))
summary(bb.fit.adm1.strat.u5.bench)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm1.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm1_strat_u5_bench.rda'))
save(bb.hyperpar.adm1.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm1_strat_u5_bench.rda'))
save(bb.fixed.adm1.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm1_strat_u5_bench.rda'))
# save results
save(bb.res.adm1.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_res_adm1_strat_u5_bench.rda'))

# save traceplots from benchmarking for 12 random combinations of admin area and year
pdf(file = paste0('Betabinomial/U5MR/',country,'_adm1_strat_u5_bench_trace.pdf'))
par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
ind <- sort(sample(1:length(bb.res.adm1.strat.u5.bench$draws.est.overall),12))
for(i in ind){
  bb.draws.tmp <- bb.res.adm1.strat.u5.bench$draws.est.overall[[i]]$draws
  plot(bb.draws.tmp,type='l', ylab=' ',xlab='',
       main=paste0(bb.res.adm1.strat.u5.bench$draws.est.overall[[i]]$region,", ", bb.res.adm1.strat.u5.bench$draws.est.overall[[i]]$years))
}
dev.off()

  #### Admin2 Unstrat ---------------------------------------------
bb.adm2.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 1000)
bb.fit.adm2.unstrat.u5 <- bb.adm2.unstrat.u5[[1]]
bb.res.adm2.unstrat.u5 <- bb.adm2.unstrat.u5[[2]]

bb.temporals.adm2.unstrat.u5 <- getDiag(bb.adm2.unstrat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
bb.hyperpar.adm2.unstrat.u5 <- bb.fit.adm2.unstrat.u5$fit$summary.hyperpar
bb.fixed.adm2.unstrat.u5 <- bb.fit.adm2.unstrat.u5$fit$summary.fixed

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_unstrat_u5.txt'))
summary(bb.fit.adm2.unstrat.u5)
sink(file=NULL)
# save smaller components of fit
save(bb.temporals.adm2.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm2_unstrat_u5.rda'))
save(bb.hyperpar.adm2.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm2_unstrat_u5.rda'))
save(bb.fixed.adm2.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm2_unstrat_u5.rda'))
# save results
save(bb.res.adm2.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm2_unstrat_u5.rda'))

  #### Admin2 Strat -----------------------------------------------
  bb.adm2.strat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=T, weight.strata=weight.strata.adm2.u5,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 1000)
  bb.fit.adm2.strat.u5 <- bb.adm2.strat.u5[[1]]
  bb.res.adm2.strat.u5 <- bb.adm2.strat.u5[[2]]
  
  bb.temporals.adm2.strat.u5 <- getDiag(bb.adm2.strat.u5$fit,field = "time",year_label=beg.year:end.proj.year)
  bb.hyperpar.adm2.strat.u5 <- bb.fit.adm2.strat.u5$fit$summary.hyperpar
  bb.fixed.adm2.strat.u5 <- bb.fit.adm2.strat.u5$fit$summary.fixed
  
  # save summary of fit to a txt file
  sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_strat_u5.txt'))
  summary(bb.fit.adm2.strat.u5)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm2.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm2_strat_u5.rda'))
  save(bb.hyperpar.adm2.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm2_strat_u5.rda'))
  save(bb.fixed.adm2.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm2_strat_u5.rda'))
  # save results
  save(bb.res.adm2.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm2_strat_u5.rda'))
  
  #### Admin2 Strat, Benchmarked -----------------------------------------------
  bb.adm2.strat.u5.bench <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=T, weight.strata=weight.strata.adm2.u5,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
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
  sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_strat_u5_bench.txt'))
  summary(bb.fit.adm2.strat.u5.bench)
  sink(file=NULL)
  # save smaller components of fit
  save(bb.temporals.adm2.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_temporals_adm2_strat_u5_bench.rda'))
  save(bb.hyperpar.adm2.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_hyperpar_adm2_strat_u5_bench.rda'))
  save(bb.fixed.adm2.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_fixed_adm2_strat_u5_bench.rda'))
  # save results
  save(bb.res.adm2.strat.u5.bench,file=paste0('Betabinomial/U5MR/',country,'_res_adm2_strat_u5_bench.rda'))
  
  # save traceplots from benchmarking for 12 random combinations of admin area and year
  pdf(file = paste0('Betabinomial/U5MR/',country,'_adm2_strat_u5_bench_trace.pdf'))
  par(mfrow=c(4,3),mar=c(3,2,2,1) + 0.1)
  ind <- sort(sample(1:length(bb.res.adm2.strat.u5.bench$draws.est.overall),12))
  for(i in ind){
    bb.draws.tmp <- bb.res.adm2.strat.u5.bench$draws.est.overall[[i]]$draws
    plot(bb.draws.tmp,type='l', ylab=' ',xlab='',
         main=paste0(bb.res.adm2.strat.u5.bench$draws.est.overall[[i]]$region,", ", bb.res.adm2.strat.u5.bench$draws.est.overall[[i]]$years))
  }
  dev.off()