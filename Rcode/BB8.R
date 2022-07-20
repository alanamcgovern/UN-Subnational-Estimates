rm(list = ls())
### ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'

### Libraries -----------------------------------------------
library(SUMMER)
library(INLA)
options(gsubfn.engine = "R")
library(rgdal)

### Retrieve directories and country info -----------------------------------------------
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

### Load polygon files -----------------------------------------------
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

### Load data -----------------------------------------------
load(paste0(country,'_cluster_dat.rda'),
     envir = .GlobalEnv)

mod.dat$years <- as.numeric(as.character(mod.dat$years))
mod.dat<-mod.dat[as.numeric(mod.dat$years)>=beg.year,]
mod.dat$country <- as.character(country)

### Load HIV Adjustment info -----------------------------------------------

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

### Load UR proportions -----------------------------------------------
setwd(paste0(res.dir,'/UR'))
weight.strata.natl.u5 <- readRDS(paste0('U5_fraction/','natl_u5_urban_weights.rds'))
weight.strata.natl.u5$rural <- 1-weight.strata.natl.u5$urban
weight.strata.adm1.u5 <- readRDS(paste0('U5_fraction/','admin1_u5_urban_weights.rds'))
weight.strata.adm2.u5 <- readRDS(paste0('U5_fraction/','admin2_u5_urban_weights.rds'))

weight.strata.natl.u1 <- readRDS(paste0('U1_fraction/','natl_u1_urban_weights.rds'))
weight.strata.natl.u1$rural <- 1-weight.strata.natl.u1$urban
weight.strata.adm1.u1 <- readRDS(paste0('U1_fraction/','admin1_u1_urban_weights.rds'))
weight.strata.adm2.u1 <- readRDS(paste0('U1_fraction/','admin2_u1_urban_weights.rds'))

# Fit BB8 models -----------------------------------------------
setwd(paste0(res.dir))

### National U5MR -----------------------------------------------

#### Unstratified
bb.natl.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=NULL, admin.level='National',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,doHIVAdj=doHIVAdj)

#### Stratified
bb.natl.strat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=NULL, admin.level='National',
                             stratified=T, weight.strata=weight.strata.natl.u5,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,doHIVAdj=doHIVAdj)


### Admin1 U5MR -----------------------------------------------

#### Unstratified
bb.natl.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin1.mat, admin.level='Admin1',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,doHIVAdj=doHIVAdj)

#### Stratified
bb.natl.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin1.mat, admin.level='Admin1',
                             stratified=T, weight.strata=weight.strata.adm1.u5,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,doHIVAdj=doHIVAdj)

### Admin2 U5MR -----------------------------------------------

#### Unstratified
bb.natl.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=F, weight.strata=NULL,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,doHIVAdj=doHIVAdj)

#### Stratified
bb.natl.unstrat.u5 <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=T, weight.strata=weight.strata.adm2.u5,
                             outcome='u5mr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,doHIVAdj=doHIVAdj)

## National NMR -----------------------------------------------

#### Unstratified
bb.natl.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=NULL, admin.level='National',
                             stratified=F, weight.strata=NULL,
                             outcome='nmr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,doHIVAdj=doHIVAdj)

#### Stratified
bb.natl.strat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                           Amat=NULL, admin.level='National',
                           stratified=T, weight.strata=weight.strata.natl.u1,
                           outcome='nmr',
                           time.model='ar1', st.time.model='ar1',
                           adj.frame=adj.frame, adj.varnames=adj.varnames,
                           doBenchmark=F,doHIVAdj=doHIVAdj)


### Admin1 NMR -----------------------------------------------

#### Unstratified
bb.natl.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin1.mat, admin.level='Admin1',
                             stratified=F, weight.strata=NULL,
                             outcome='nmr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,doHIVAdj=doHIVAdj)

#### Stratified
bb.natl.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin1.mat, admin.level='Admin1',
                             stratified=T, weight.strata=weight.strata.adm1.u1,
                             outcome='nmr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,doHIVAdj=doHIVAdj)

### Admin2 U5MR -----------------------------------------------

#### Unstratified
bb.natl.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=F, weight.strata=NULL,
                             outcome='nmr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,doHIVAdj=doHIVAdj)

#### Stratified
bb.natl.unstrat.nmr <- getBB8(mod.dat, country, beg.year=beg.year, end.year=end.proj.year,
                             Amat=admin2.mat, admin.level='Admin2',
                             stratified=T, weight.strata=weight.strata.adm2.u1,
                             outcome='nmr',
                             time.model='ar1', st.time.model='ar1',
                             adj.frame=adj.frame, adj.varnames=adj.varnames,
                             doBenchmark=F,doHIVAdj=doHIVAdj)


