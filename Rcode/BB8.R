################################################################
#########   load libraries
################################################################
rm(list = ls())

#### Libraries ####
library(SUMMER)
library(INLA)
options(gsubfn.engine = "R")
library(rgdal)

#### ----------------------------------------------------------
#### ----------------------------------------------------------
# ENTER COUNTRY BEING ANALYZED
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'
#### ----------------------------------------------------------
#### ----------------------------------------------------------

code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
load(file = paste0(home.dir,'/Info/',country, "_general_info.Rdata", sep=''))

################################################################
#########   load polygon files
################################################################

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

################################################################
#########   load data
################################################################
load(paste0(country,'_cluster_dat.rda'),
     envir = .GlobalEnv)

mod.dat$years <- as.numeric(as.character(mod.dat$years))
mod.dat<-mod.dat[as.numeric(mod.dat$years)>=beg.year,]
mod.dat$country <- as.character(country)

################################################################
#########  Add HIV Adjustment -- is this sufficient or is there something else I need to add after model fit?
################################################################

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

##################################################################
###### Fit BB8 model
##################################################################

setwd(paste0(res.dir))

## _____________________________________________________________________
## National unstratified
## _____________________________________________________________________

mod.dat$strata <- NA
mod.dat$region <- "All"
message(paste0('Starting unstratified national model for ',country,'\n'))

fit.natl.unstrat <- smoothCluster(data = mod.dat, family = "betabinomial",
                                  Amat = NULL,
                                  year_label = c(beg.year:end.proj.year),
                                  time.model = "rw2",
                                  pc.st.slope.u = 1, pc.st.slope.alpha = 0.01,
                                  bias.adj = adj.frame,
                                  bias.adj.by = adj.varnames,
                                  overdisp.mean = -7.5,
                                  overdisp.prec = 0.39,
                                  survey.effect = TRUE)

res.natl.unstrat <- getSmoothed(inla_mod = fit.natl.unstrat, 
                                year_range = c(beg.year:end.proj.year),
                                year_label = c(beg.year:end.proj.year),
                                nsim = 1000, 
                                draws = NULL, save.draws = TRUE,save.draws.est=TRUE)

save(fit.natl.unstrat, file = paste0('Betabinomial/',country,'_fit_natl_unstrat.rda'))
save(res.natl.unstrat, file = paste0('Betabinomial/',country,'_res_natl_unstrat.rda'))

## _____________________________________________________________________
## National stratified
## _____________________________________________________________________

mod.dat$strata <- mod.dat$urban
mod.dat$region <- "All"
message(paste0('Starting stratified national model for ',country,'\n'))

fit.natl.strat <- smoothCluster(data = mod.dat, family = "betabinomial",
                                Amat = NULL,
                                year_label = c(beg.year:end.proj.year),
                                strata.time.effect = TRUE,
                                time.model = "rw2",
                                pc.st.slope.u = 1, pc.st.slope.alpha = 0.01,
                                bias.adj = adj.frame,
                                bias.adj.by = adj.varnames,
                                overdisp.mean = -7.5,
                                overdisp.prec = 0.39,
                                survey.effect = TRUE)

# load the national urban population fraction, this is needed to weight urban/rural estimators
natl.urb.weights <- readRDS(paste0('UR/U5_fraction/natl_urban_weights.rds'))
natl.urb.weights$rural <- 1- natl.urb.weights$urban

res.natl.strat <- getSmoothed(inla_mod = fit.natl.strat, 
                              year_range = c(beg.year:end.proj.year),
                              year_label = c(beg.year:end.proj.year),
                              nsim = 1000, 
                              weight.strata = natl.urb.weights, 
                              weight.frame = NULL,
                              draws = NULL, save.draws = TRUE,save.draws.est=TRUE)

save(fit.natl.strat, file = paste0('Betabinomial/',country,'_fit_natl_strat.rda'))
save(res.natl.strat, file = paste0('Betabinomial/',country,'_res_natl_strat.rda'))


## _____________________________________________________________________
## Admin1 unstratified
## _____________________________________________________________________
mod.dat$strata <- NA
mod.dat$region <- mod.dat$admin1.char
message(paste0('Starting unstratified admin1 model ',country, '\n'))

fit.admin1.unstrat <- smoothCluster(data = mod.dat, 
                                    family = "betabinomial",
                                    Amat = admin1.mat, 
                                    year_label = c(beg.year:end.proj.year),
                                    time.model = "rw2", 
                                    st.time.model = 'ar1',
                                    pc.st.slope.u = 1, 
                                    pc.st.slope.alpha = 0.01,
                                    type.st = 4,
                                    bias.adj = adj.frame,
                                    bias.adj.by = adj.varnames,
                                    survey.effect = TRUE,
                                    age.groups = levels(mod.dat$age),
                                    age.n = c(1, 11, 12, 12, 12, 12),
                                    age.rw.group = c(1, 2, 3, 3, 3, 3),
                                    overdisp.mean = -7.5,
                                    overdisp.prec = 0.39)

res.admin1.unstrat <- getSmoothed(fit.admin1.unstrat,
                                  nsim = 1000,
                                  save.draws.est = TRUE,
                                  draws = NULL,
                                  save.draws = TRUE)

save(fit.admin1.unstrat, file = paste0('Betabinomial/',country,'_fit_admin1_',st.time.model,'_unstrat.rda'))
save(res.admin1.unstrat, file = paste0('Betabinomial/',country,'_res_admin1_',st.time.model,'_unstrat.rda'))


