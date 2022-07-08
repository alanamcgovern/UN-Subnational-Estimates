################################################################
################################################################
######## function to generate national, admin1 and admin2 BB8 estimates (stratified and unstratified)
################################################################
################################################################

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
# enter country being analyzed
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'
#### ----------------------------------------------------------
#### ----------------------------------------------------------

code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
load(file = paste0(home.dir,'/Info/',country, "_general_info.Rdata", sep=''))

##################################################################
###### Parameters for BB8 function
##################################################################
## Parameters ####
beg.year <- beg.year
end.year <- max(survey_years)
st.time.model <- 'ar1' #other options are 'rw1', 'rw2'
doBenchmark <- FALSE
doNatl <- TRUE
doAdmin1 <- TRUE
doAdmin2 <- TRUE
stratified <- TRUE
doHIVAdj <- FALSE

##################################################################
###### Define BB8 function
##################################################################

getBB8 <- function(mod.dat, country, beg.year, end.year,
                    doNatl, doAdmin1, doAdmin2, stratified, st.time.model,
                    doHIVAdj, doBenchmark){
  
  ################################################################
  #########  National level model
  ################################################################
  
  if(doNatl){
    
    if(stratified==F){
      
      mod.dat$strata <- NA
      message(paste0('Starting unstratified national model for ',country,'\n'))
      
      fit.natl.unstrat <- smoothCluster(data = mod.dat, family = "betabinomial",
                                        Amat = NULL,
                                        year_label = c(beg.year:end.year),
                                        time.model = "rw2",
                                        pc.st.slope.u = 1, pc.st.slope.alpha = 0.01,
                                        bias.adj = adj.frame,
                                        bias.adj.by = adj.varnames,
                                        overdisp.mean = -7.5,
                                        overdisp.prec = 0.39,
                                        survey.effect = FALSE)
      
      res.natl.unstrat <- getSmoothed(inla_mod = fit.natl.unstrat, 
                                      year_range = c(beg.year:end.year),
                                      year_label = c(beg.year:end.year),
                                      nsim = 1000, 
                                      draws = NULL, save.draws = TRUE,save.draws.est=TRUE)
      
      setwd(paste0(res.dir,'/Betabinomial/'))
      
      save(fit.natl.unstrat, file = paste0(country,'_fit_natl_unstrat.rda'))
      
      save(res.natl.unstrat, file = paste0(country,'_res_natl_unstrat.rda'))
      
      ### Benchmarking #### -- add later, does it depend on whether or not the model is stratified?
      if(doBenchmark & !doHIVAdj){
      }else if(doBenchmark & doHIVAdj){}
      
    } else{
      ## stratified model
      mod.dat$strata <- mod.dat$urban
      message(paste0('Starting stratified national model for ',country,'\n'))
      
      fit.natl.strat <- smoothCluster(data = mod.dat, family = "betabinomial",
                                        Amat = NULL,
                                        year_label = c(beg.year:2019),
                                        strata.time.effect = TRUE,
                                        time.model = "rw2",
                                        pc.st.slope.u = 1, pc.st.slope.alpha = 0.01,
                                        bias.adj = adj.frame,
                                        bias.adj.by = adj.varnames,
                                        overdisp.mean = -7.5,
                                        overdisp.prec = 0.39,
                                        survey.effect = TRUE)
      
      # load the national urban population fraction, this is needed to weight urban/rural estimators
      setwd(paste0(res.dir,'/UR'))
      
      natl.urb.weights <- readRDS(paste0('U5_fraction/natl_urban_weights.rds'))
      natl.urb.weights$rural <- 1- natl.urb.weights$urban
      
      res.natl.strat <- getSmoothed(inla_mod = fit.natl.strat, 
                                      year_range = c(beg.year:end.year),
                                      year_label = c(beg.year:end.year),
                                      nsim = 1000, 
                                      weight.strata = natl.urb.weights, 
                                      weight.frame = NULL,
                                      draws = NULL, save.draws = TRUE,save.draws.est=TRUE)
      
      setwd(paste0(res.dir,'/Betabinomial'))
      
      save(fit.natl.strat, file = paste0(country,'_fit_natl_strat.rda'))
      
      save(res.natl.strat, file = paste0(country,'_res_natl_strat.rda'))
      
      ### Benchmarking #### -- add later, does it depend on whether or not the model is stratified?
      if(doBenchmark & !doHIVAdj){
      }else if(doBenchmark & doHIVAdj){}
      
    }    
  }
  
  
  ################################################################
  #########  Admin 1 level model
  ################################################################
  
  if(doAdmin1){
    mod.dat$region <- mod.dat$admin1.char
    
    ### unstratified admin1 model
    if(stratified==F){
      mod.dat$strata <- NA
      message(paste0('Starting unstratified admin1 model ',country, '\n'))
      
      fit.admin1.unstrat <- smoothCluster(data = mod.dat, 
                                          family = "betabinomial",
                                          Amat = admin1.mat, 
                                          year_label = c(beg.year:end.year),
                                          time.model = "rw2", 
                                          st.time.model = st.time.model,
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
      
      setwd(paste0(res.dir,'/Betabinomial'))
      
      save(fit.admin1.unstrat, file = paste0(country,'_fit_admin1_',st.time.model,'_unstrat.rda'))
      
      save(res.admin1.unstrat, file = paste0(country,'_res_admin1_',st.time.model,'_unstrat.rda'))
      
      ### Benchmarking #### -- add later, does it depend on whether or not the model is stratified?
      if(doBenchmark & !doHIVAdj){
      }else if(doBenchmark & doHIVAdj){}
    
    ### stratified admin1 model  
      }else{
      mod.dat$strata <- mod.dat$urban
      message(paste0('Starting stratified admin1 model for ',country, '\n'))
      
      fit.admin1.strat <- smoothCluster(data = mod.dat, 
                                          family = "betabinomial",
                                          Amat = admin1.mat, 
                                          year_label = c(beg.year:end.year),
                                          time.model = "rw2", 
                                          st.time.model = st.time.model,
                                          pc.st.slope.u = 1, 
                                          pc.st.slope.alpha = 0.01,
                                          type.st = 4,
                                          bias.adj = adj.frame,
                                          bias.adj.by = adj.varnames,
                                          survey.effect = TRUE,
                                          strata.time.effect = T,
                                          age.groups = levels(mod.dat$age),
                                          age.n = c(1, 11, 12, 12, 12, 12),
                                          age.rw.group = c(1, 2, 3, 3, 3, 3),
                                          overdisp.mean = -7.5,
                                          overdisp.prec = 0.39)
      
      setwd(paste0(res.dir,'/UR'))
      weight.strata.adm1 <- readRDS(paste0('U5_fraction/admin1_urban_weights.rds'))
      
      # sample for national U5MR estimators and compute the estimates
      res.admin1.strat <- getSmoothed(inla_mod = fit.admin1.strat, 
                                      year_range = beg.year:end.year, 
                                      year_label = beg.year:end.year, nsim = 1000, 
                                      weight.strata = weight.strata.adm1, 
                                      weight.frame = NULL,
                                      draws = NULL, save.draws = TRUE)
      
      setwd(paste0(res.dir,'/Betabinomial'))
      
      save(fit.admin1.strat, file = paste0(country,'_fit_admin1_',st.time.model,'_strat.rda'))
      
      save(res.admin1.strat, file = paste0(country,'_res_admin1_',st.time.model,'_strat.rda'))
      
      
      ### Benchmarking #### -- add later, does it depend on whether or not the model is stratified?
      if(doBenchmark & !doHIVAdj){
      }else if(doBenchmark & doHIVAdj){}
    }    
    
  }

  ################################################################
  #########  Admin 2 level model
  ################################################################

  if(doAdmin2){
    mod.dat$region <- mod.dat$admin2.char
    
    if(stratified==F){
      mod.dat$strata <- NA
      message(paste0('Starting unstratified admin2 model for ',country, '\n'))
      
      fit.admin2.unstrat <- smoothCluster(data = mod.dat, 
                                          family = "betabinomial",
                                          Amat = admin2.mat, 
                                          year_label = c(beg.year:end.year),
                                          time.model = "rw2", 
                                          st.time.model = st.time.model,
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
      
      res.admin2.unstrat <- getSmoothed(inla_mod = fit.admin2.unstrat,
                                        nsim = 1000,
                                        save.draws.est = TRUE,
                                        draws = NULL,
                                        save.draws = TRUE)
      
      setwd(paste0(res.dir,'/Betabinomial'))
      
      save(fit.admin2.unstrat, file = paste0(country,'_fit_admin2_',st.time.model,'_unstrat.rda'))
      
      save(res.admin2.unstrat, file = paste0(country,'_res_admin2_',st.time.model,'_unstrat.rda'))
      
      ### Benchmarking #### -- add later, does it depend on whether or not the model is stratified?
      if(doBenchmark & !doHIVAdj){
      }else if(doBenchmark & doHIVAdj){}
      
      
      
    
      }else{
        mod.dat$strata <- mod.dat$urban
        message(paste0('Starting stratified admin2 model for ',country, '\n'))
        
        fit.admin2.strat <- smoothCluster(data = mod.dat, 
                                          family = "betabinomial",
                                          Amat = admin2.mat, 
                                          year_label = c(beg.year:end.year),
                                          time.model = "rw2", 
                                          st.time.model = st.time.model,
                                          pc.st.slope.u = 1, 
                                          pc.st.slope.alpha = 0.01,
                                          type.st = 4,
                                          bias.adj = adj.frame,
                                          bias.adj.by = adj.varnames,
                                          survey.effect = TRUE,
                                          strata.time.effect = T,
                                          age.groups = levels(mod.dat$age),
                                          age.n = c(1, 11, 12, 12, 12, 12),
                                          age.rw.group = c(1, 2, 3, 3, 3, 3),
                                          overdisp.mean = -7.5,
                                          overdisp.prec = 0.39)
        
        setwd(paste0(res.dir,'/UR'))
        weight.strata.adm2 <- readRDS(paste0('U5_fraction/','admin2_urban_weights.rds'))
        
        # sample for national U5MR estimators and compute the estimates
        res.admin2.strat <- getSmoothed(inla_mod = fit.admin2.strat, 
                                        year_range = beg.year:end.year, 
                                        year_label = beg.year:end.year, nsim = 1000, 
                                        weight.strata = weight.strata.adm2, 
                                        weight.frame = NULL,
                                        draws = NULL, save.draws = TRUE)
        
        setwd(paste0(res.dir,'/Betabinomial'))
        
        save(fit.admin2.strat, file = paste0(country,'_fit_admin2_',st.time.model,'_strat.rda'))
        
        save(res.admin2.strat, file = paste0(country,'_res_admin2_',st.time.model,'_strat.rda'))
        
        
        ### Benchmarking #### -- add later, does it depend on whether or not the model is stratified?
        if(doBenchmark & !doHIVAdj){
        }else if(doBenchmark & doHIVAdj){}
    }
  }
}
  

