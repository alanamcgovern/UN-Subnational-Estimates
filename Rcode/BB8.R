rm(list = ls())
## ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'

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

## Load polygon files -----------------------------------------------
setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm0))
poly.adm1 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm1))
poly.adm2 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm2))

proj4string(poly.adm0) <- proj4string(poly.adm1) <- proj4string(poly.adm2)
load(paste0('shapeFiles_gadm/', country, '_Amat.rda'))
load(paste0('shapeFiles_gadm/', country, '_Amat_Names.rda'))

## Load data -----------------------------------------------
load(paste0(country,'_cluster_dat.rda'),
     envir = .GlobalEnv)

mod.dat$years <- as.numeric(as.character(mod.dat$years))
mod.dat<-mod.dat[as.numeric(mod.dat$years)>=beg.year,]
mod.dat$country <- as.character(country)

## Load National IGME estimates ------------------------------------------------------
setwd(paste0(data.dir,'/igme'))

## U5MR
igme.ests.u5 <- read.csv(paste0(country.abbrev,'_u5_igme_est.csv'), header = T)
names(igme.ests.u5) <- c('year','OBS_VALUE','LOWER_BOUND','UPPER_BOUND')
igme.ests.u5$year <- igme.ests.u5$year-0.5
igme.ests.u5$OBS_VALUE <- igme.ests.u5$OBS_VALUE/1000
igme.ests.u5 <- igme.ests.u5[igme.ests.u5$year %in% beg.year:end.proj.year,]
igme.ests.u5 <- igme.ests.u5[order(igme.ests.u5$year),]
igme.ests.u5$SD <- (igme.ests.u5$UPPER_BOUND - igme.ests.u5$LOWER_BOUND)/(2*1.645*1000)

## NMR
igme.ests.nmr <- read.csv(paste0(country.abbrev,'_nmr_igme_est.csv'),  header = T)
names(igme.ests.nmr) <- c('year','OBS_VALUE','LOWER_BOUND','UPPER_BOUND')
igme.ests.nmr$year <- igme.ests.nmr$year-0.5
igme.ests.nmr$OBS_VALUE <- igme.ests.nmr$OBS_VALUE/1000
igme.ests.nmr <- igme.ests.nmr[igme.ests.nmr$year %in% beg.year:end.proj.year,]
igme.ests.nmr <- igme.ests.nmr[order(igme.ests.nmr$year),]
igme.ests.nmr$SD <- (igme.ests.nmr$UPPER_BOUND - igme.ests.nmr$LOWER_BOUND)/(2*1.645*1000)

## Get Admin 1 and 2 population proportions by region (for benchmarking) ------------------------------------------------------

## Admin 1
weight.region.adm1.u5 <- expand.grid(region = admin1.names$Internal, years = beg.year:end.proj.year)
weight.region.adm1.u5$proportion <- NA

weight.region.adm1.nmr <- expand.grid(region = admin1.names$Internal, years = beg.year:end.proj.year)
weight.region.adm1.nmr$proportion <- NA

for (i in 1:nrow(weight.region.adm1.u5)) {
  numerator <- sum(mod.dat$total[mod.dat$admin1.char==weight.region.adm1.u5[i,1] & mod.dat$years==weight.region.adm1.u5[i,2]])
  denominator <- sum(mod.dat$total[mod.dat$years==weight.region.adm1.u5[i,2]])
  weight.region.adm1.u5[i, "proportion"] <- numerator/denominator
  
  numerator <- sum(mod.dat$total[mod.dat$age==0 & mod.dat$admin1.char==weight.region.adm1.nmr[i,1] & mod.dat$years==weight.region.adm1.nmr[i,2]])
  denominator <- sum(mod.dat$total[mod.dat$age==0 & mod.dat$years==weight.region.adm1.nmr[i,2]])
  weight.region.adm1.nmr[i, "proportion"] <- numerator/denominator
  
  # Note: this makes the region weights for projected years the same as the last year of data (most recent sruvey year)
  if(weight.region.adm1.u5[i,2]>max(end.years)){
    numerator <- sum(mod.dat$total[mod.dat$admin1.char==weight.region.adm1.u5[i,1] & mod.dat$years==max(end.years)])
    denominator <- sum(mod.dat$total[mod.dat$years==max(end.years)])
    weight.region.adm1.u5[i, "proportion"] <- numerator/denominator
    
    numerator <- sum(mod.dat$total[mod.dat$age==0 & mod.dat$admin1.char==weight.region.adm1.nmr[i,1] & mod.dat$years==max(end.years)])
    denominator <- sum(mod.dat$total[mod.dat$age==0 & mod.dat$years==max(end.years)])
    weight.region.adm1.nmr[i, "proportion"] <- numerator/denominator
    
  }
}

## Admin 2
weight.region.adm2.u5 <- expand.grid(region = admin2.names$Internal, years = beg.year:max(end.years))
weight.region.adm2.u5$proportion <- NA

weight.region.adm2.nmr <- expand.grid(region = admin2.names$Internal, years = beg.year:max(end.years))
weight.region.adm2.nmr$proportion <- NA

for (i in 1:nrow(weight.region.adm2.u5)) {
  numerator <- sum(mod.dat$total[mod.dat$admin2.char==weight.region.adm2.u5[i,1] & mod.dat$years==weight.region.adm2.u5[i,2]])
  denominator <- sum(mod.dat$total[mod.dat$years==weight.region.adm2.u5[i,2]])
  weight.region.adm2.u5[i, "proportion"] <- numerator/denominator
  
  numerator <- sum(mod.dat$total[mod.dat$age==0 & mod.dat$admin2.char==weight.region.adm2.nmr[i,1] & mod.dat$years==weight.region.adm2.nmr[i,2]])
  denominator <- sum(mod.dat$total[mod.dat$age==0 & mod.dat$years==weight.region.adm2.nmr[i,2]])
  weight.region.adm2.nmr[i, "proportion"] <- numerator/denominator
}

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
  
}else{
  adj.frame <- expand.grid(years = beg.year:end.year,country = country)
  adj.frame$ratio <- 1
  adj.varnames <- c("country", "years")
}

## Load UR proportions -----------------------------------------------
setwd(paste0(res.dir,'/UR'))
weight.strata.natl.u5 <- readRDS(paste0('U5_fraction/','natl_u5_urban_weights.rds'))
weight.strata.natl.u5$rural <- 1-weight.strata.natl.u5$urban
weight.strata.adm1.u5 <- readRDS(paste0('U5_fraction/','admin1_u5_urban_weights.rds'))
weight.strata.adm2.u5 <- readRDS(paste0('U5_fraction/','admin2_u5_urban_weights.rds'))

weight.strata.natl.u1 <- readRDS(paste0('U1_fraction/','natl_u1_urban_weights.rds'))
weight.strata.natl.u1$rural <- 1-weight.strata.natl.u1$urban
weight.strata.adm1.u1 <- readRDS(paste0('U1_fraction/','admin1_u1_urban_weights.rds'))
weight.strata.adm2.u1 <- readRDS(paste0('U1_fraction/','admin2_u1_urban_weights.rds'))

## Fit BB8 models -----------------------------------------------
setwd(paste0(res.dir))

### National U5MR -----------------------------------------------

#### Unstratified (< 1 min)
time_tmp <- Sys.time()
bb.natl.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=NULL, admin.level='National',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim=10000)
time_natl_unstrat_u5 <- Sys.time() - time_tmp

bb.fit.natl.unstrat.u5 <- bb.natl.unstrat.u5[[1]]
bb.res.natl.unstrat.u5 <- bb.natl.unstrat.u5[[2]]


# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_natl_unstrat_u5.txt'))
summary(bb.fit.natl.unstrat.u5)
sink(file=NULL)
# save results
#save(bb.fit.natl.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_fit_natl_unstrat_u5.rda'))
save(bb.res.natl.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_natl_unstrat_u5.rda'))

#### Stratified (< 5 min)
time_tmp <- Sys.time()
bb.natl.strat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=NULL, admin.level='National',
                             stratified=T, weight.strata=weight.strata.natl.u5,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim=10000)
time_natl_strat_u5 <- Sys.time() - time_tmp

bb.fit.natl.strat.u5 <- bb.natl.strat.u5[[1]]
bb.res.natl.strat.u5 <- bb.natl.strat.u5[[2]]

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_natl_strat_u5.txt'))
summary(bb.fit.natl.strat.u5)
sink(file=NULL)
# save results
#save(bb.fit.natl.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_fit_natl_strat_u5.rda'))
save(bb.res.natl.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_natl_strat_u5.rda'))

### Admin1 U5MR -----------------------------------------------

#### Unstratified (can't get it to run)
time_tmp <- Sys.time()
bb.adm1.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin1.mat, admin.level='Admin1',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 10000)
time_adm1_unstrat_u5 <- Sys.time() - time_tmp

bb.fit.adm1.unstrat.u5 <- bb.adm1.unstrat.u5[[1]]
bb.res.adm1.unstrat.u5 <- bb.adm1.unstrat.u5[[2]]

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_unstrat_u5.txt'))
summary(bb.fit.adm1.unstrat.u5)
sink(file=NULL)
# save results
#save(bb.fit.adm1.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_unstrat_u5.rda'))
save(bb.res.adm1.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm1_unstrat_u5.rda'))


#### Stratified (< 30 min)
time_tmp <- Sys.time()
bb.adm1.strat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin1.mat, admin.level='Admin1',
                             stratified=T, weight.strata=weight.strata.adm1.u5,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim=10000)
time_adm1_strat_u5 <- Sys.time() - time_tmp

bb.fit.adm1.strat.u5 <- bb.adm1.strat.u5[[1]]
bb.res.adm1.strat.u5 <- bb.adm1.strat.u5[[2]]

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_strat_u5.txt'))
summary(bb.fit.adm1.strat.u5)
sink(file=NULL)
# save results
#save(bb.fit.adm1.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_fit_adm1_strat_u5.rda'))
save(bb.res.adm1.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm1_strat_u5.rda'))



### Admin2 U5MR -----------------------------------------------

#### Unstratified (< 10 min)
time_tmp <- Sys.time()
bb.adm2.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 10000)
time_adm2_unstrat_u5 <- Sys.time() - time_tmp

bb.fit.adm2.unstrat.u5 <- bb.adm2.unstrat.u5[[1]]
bb.res.adm2.unstrat.u5 <- bb.adm2.unstrat.u5[[2]]

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_unstrat_u5.txt'))
summary(bb.fit.adm2.unstrat.u5)
sink(file=NULL)
# save results
#save(bb.fit.adm2.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_unstrat_u5.rda'))
save(bb.res.adm2.unstrat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm2_unstrat_u5.rda'))


#### Stratified (< 15 min)
time_tmp <- Sys.time()
bb.adm2.strat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=T, weight.strata=weight.strata.adm2.u5,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 10000)
time_adm2_strat_u5 <- Sys.time() - time_tmp

bb.fit.adm2.strat.u5 <- bb.adm2.strat.u5[[1]]
bb.res.adm2.strat.u5 <- bb.adm2.strat.u5[[2]]

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_strat_u5.txt'))
summary(bb.fit.adm2.strat.u5)
sink(file=NULL)
# save results
#save(bb.fit.adm2.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_fit_adm2_strat_u5.rda'))
save(bb.res.adm2.strat.u5,file=paste0('Betabinomial/U5MR/',country,'_res_adm2_strat_u5.rda'))


### National NMR -----------------------------------------------

#### Unstratified (< 1 min)
time_tmp <- Sys.time()
bb.natl.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=NULL, admin.level='National',
                             stratified=F, weight.strata=NULL,
                             outcome='nmr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 10000)
time_natl_unstrat_nmr <- Sys.time() - time_tmp

bb.fit.natl.unstrat.nmr <- bb.natl.unstrat.nmr[[1]]
bb.res.natl.unstrat.nmr <- bb.natl.unstrat.nmr[[2]]

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_natl_unstrat_nmr.txt'))
summary(bb.fit.natl.unstrat.nmr)
sink(file=NULL)
# save results
#save(bb.fit.natl.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_fit_natl_unstrat_nmr.rda'))
save(bb.res.natl.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_natl_unstrat_nmr.rda'))


#### Stratified (< 1 min)
time_tmp <- Sys.time()
bb.natl.strat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                           Amat=NULL, admin.level='National',
                           stratified=T, weight.strata=weight.strata.natl.u1,
                           outcome='nmr',
                           time.model='ar1', st.time.model='ar1',
                           adj.frame=adj.frame, adj.varnames=adj.varnames,
                           doBenchmark=F,nsim = 10000)
time_natl_strat_nmr <- Sys.time() - time_tmp

bb.fit.natl.strat.nmr <- bb.natl.strat.nmr[[1]]
bb.res.natl.strat.nmr <- bb.natl.strat.nmr[[2]]

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_natl_strat_nmr.txt'))
summary(bb.fit.natl.strat.nmr)
sink(file=NULL)
# save results
#save(bb.fit.natl.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_fit_natl_strat_nmr.rda'))
save(bb.res.natl.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_natl_strat_nmr.rda'))

### Admin1 NMR -----------------------------------------------

#### Unstratified (< 10 mins)
time_tmp <- Sys.time()
bb.adm1.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin1.mat, admin.level='Admin1',
                             stratified=F, weight.strata=NULL,
                             outcome='nmr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 10000)
time_adm1_unstrat_nmr <- Sys.time() - time_tmp

bb.fit.adm1.unstrat.nmr <- bb.adm1.unstrat.nmr[[1]]
bb.res.adm1.unstrat.nmr <- bb.adm1.unstrat.nmr[[2]]

bb.res.bench.adm1.unstrat.nmr <- Benchmark_mod(bb.res.adm1.unstrat.nmr,igme.ests.nmr,weight.region = weight.region.adm1.nmr,estVar = 'OBS_VALUE',sdVar = 'SD',timeVar = 'year')

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm1_unstrat_nmr.txt'))
summary(bb.fit.adm1.unstrat.nmr)
sink(file=NULL)
# save results
#save(bb.fit.adm1.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_fit_adm1_unstrat_nmr.rda'))
save(bb.res.adm1.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm1_unstrat_nmr.rda'))

#### Stratified (< 5 min)
time_tmp <- Sys.time()
bb.adm1.strat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin1.mat, admin.level='Admin1',
                             stratified=T, weight.strata=weight.strata.adm1.u1,
                             outcome='nmr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 1000)
time_adm1_strat_nmr <- Sys.time() - time_tmp

bb.fit.adm1.strat.nmr <- bb.adm1.strat.nmr[[1]]
bb.res.adm1.strat.nmr <- bb.adm1.strat.nmr[[2]]

bb.res.bench.adm1.strat.nmr <- Benchmark_mod(bb.res.adm1.strat.nmr,igme.ests.nmr,weight.region = weight.region.adm1.nmr,estVar = 'OBS_VALUE',sdVar = 'SD',timeVar = 'year')

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm1_strat_nmr.txt'))
summary(bb.fit.adm1.strat.nmr)
sink(file=NULL)
# save results
#save(bb.fit.adm1.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_fit_adm1_strat_nmr.rda'))
save(bb.res.adm1.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm1_strat_nmr.rda'))

### Admin2 NMR -----------------------------------------------

#### Unstratified (< 2 min)
time_tmp <- Sys.time()
bb.adm2.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=F, weight.strata=NULL,
                             outcome='nmr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 10000)
time_adm2_unstrat_nmr <- Sys.time() - time_tmp

bb.fit.adm2.unstrat.nmr <- bb.adm2.unstrat.nmr[[1]]
bb.res.adm2.unstrat.nmr <- bb.adm2.unstrat.nmr[[2]]

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm2_unstrat_nmr.txt'))
summary(bb.fit.adm2.unstrat.nmr)
sink(file=NULL)
# save results
#save(bb.fit.adm2.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_fit_adm2_unstrat_nmr.rda'))
save(bb.res.adm2.unstrat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm2_unstrat_nmr.rda'))


#### Stratified (< 5 min)
time_tmp <- Sys.time()
bb.adm2.strat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=T, weight.strata=weight.strata.adm2.u1,
                             outcome='nmr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,nsim = 10000)
time_adm2_strat_nmr <- Sys.time() - time_tmp

bb.fit.adm2.strat.nmr <- bb.adm2.strat.nmr[[1]]
bb.res.adm2.strat.nmr <- bb.adm2.strat.nmr[[2]]

# save summary of fit to a txt file
sink(file=paste0('Betabinomial/NMR/',country,'_fit_adm2_strat_nmr.txt'))
summary(bb.fit.adm2.strat.nmr)
sink(file=NULL)
# save results
#save(bb.fit.adm2.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_fit_adm2_strat_nmr.rda'))
save(bb.res.adm2.strat.nmr,file=paste0('Betabinomial/NMR/',country,'_res_adm2_strat_nmr.rda'))


## Polygon Plots -----------------------------------------------
### Admin-1 U5MR est for 2010 on -----------------------------------------------
### Admin-1 U5MR CI width for 2010 on -----------------------------------------------
### Admin-2 U5MR est for most recent year -----------------------------------------------
### Admin-2 U5MR CI width for most recent year -----------------------------------------------
### Admin-2 U5MR est for 2010 on -----------------------------------------------
### Admin-2 U5MR CI width for 2010 on -----------------------------------------------

### Admin-1 NMR est for 2010 on -----------------------------------------------
### Admin-1 NMR CI width for 2010 on -----------------------------------------------
### Admin-2 NMR est for most recent year -----------------------------------------------
### Admin-2 NMR CI width for most recent year -----------------------------------------------
### Admin-2 NMR est for 2010 on -----------------------------------------------
### Admin-2 NMR CI width for 2010 on -----------------------------------------------

## Spaghetti Plots -----------------------------------------------
### National U5MR -----------------------------------------------
### Admin-1 U5MR w CI -----------------------------------------------
### Admin-1 U5MR w/o CI -----------------------------------------------
### Admin-2 U5MR w/o CI, w legend -----------------------------------------------
### Admin-2 U5MR w/o CI, w/o legend -----------------------------------------------

### National NMR -----------------------------------------------
### Admin-1 NMR w CI -----------------------------------------------
### Admin-1 NMR w/o CI -----------------------------------------------
### Admin-2 NMR w/o CI, w legend -----------------------------------------------
### Admin-2 NMR w/o CI, w/o legend -----------------------------------------------




