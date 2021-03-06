
### Parameters -----------------------------------------------
## options for time.model and st.time.model = 'ar1', 'rw1', 'rw2'
## weight.strata is NULL unless stratified = T
## options for admin.level = 'National', 'Admin1','Admin2'
## Amat = NULL iff admin.level = 'National'
## options for outcome = 'u5mr', 'nmr'
## adj.frame and adj.varnames are calculated in HIV adjustment step -- might change when benchmarking is implemented
## doBenchmark is not yet functional
## doHIVAdj may not be necessary, waiting to see how benchmark is implemented

##################################################################
###### Define BB8 function
##################################################################

getBB8 <- function(mod.dat, country, beg.year, end.year, Amat,
                    time.model, st.time.model, stratified, weight.strata,
                    admin.level, outcome,
                    doBenchmark, doHIVAdj, adj.frame,adj.varnames){
  
  ## Check inputs -----------------------------------------------
  if(!(admin.level %in% c('National','Admin1','Admin2'))){
    stop('Enter a valid admin level (National, Admin1, or Admin2)')
  }
  if(!(outcome %in% c('u5mr','nmr'))){
    stop('Enter a valid outcome measure (u5mr or nmr)')
  }
  if((admin.level!='National') == (is.null(Amat)==T)){
    stop('Admin level and adjacency matrix are not compatible')}
  if(!(time.model %in% c('ar1','rw1','rw2'))){
    stop('Enter a valid main temporal effect type (ar1, rw1, or rw2)')
  }
  if(!(st.time.model %in% c('ar1','rw1','rw2'))){
    stop('Enter a valid temporal interaction effect type (ar1, rw1, or rw2)')
  }
  if(stratified & is.null(weight.strata)){
    stop('Specify urban/rural stratification weights')
  }
  
  ## Set stratification parameters (still need to specify more for getSmoothed) -----------------------------------------------
  if(stratified){
    mod.dat$strata <- mod.dat$urban
    st.time.model <- T
  }else{
    mod.dat$strata <- NA
    st.time.model <- F
    weight.strata <- NULL
  }
  
  ## Set outcome specific parameters -----------------------------------------------
  if(outcome=='nmr'){
    mod.dat <- mod.dat %>% filter(age=='0')
    age.n <- age.rw.group <- age.strata.fixed.group <- c(1)
  }else{
    age.n <- c(1, 11, 12, 12, 12, 12)
    age.rw.group <- c(1, 2, 3, 3, 3, 3)
    age.strata.fixed.group <- c(1, 2, 3, 4, 5, 6)
  }

  ## Fit model -----------------------------------------------
  
  if(admin.level=='National'){
    mod.dat$region <- "All"
    bb.fit <- smoothCluster(data = mod.dat, family = "betabinomial",
                            Amat = NULL,
                            year_label = c(beg.year:end.year),
                            time.model = time.model,
                            pc.st.slope.u = 1, pc.st.slope.alpha = 0.01,
                            bias.adj = adj.frame,
                            bias.adj.by = adj.varnames,
                            overdisp.mean = -7.5,
                            overdisp.prec = 0.39,
                            survey.effect = TRUE)
  }else{
    if(admin.level=='Admin1'){
      mod.dat$region <- mod.dat$admin1.char
    }elseif(admin.level=='Admin2'){
      mod.dat$region <- mod.dat$admin2.char
    }
    bb.fit <- smoothCluster(data = mod.dat, family = "betabinomial",
                            Amat = Amat, 
                            year_label = c(beg.year:end.year),
                            time.model = time.model, st.time.model = st.time.model,
                            pc.st.slope.u = 1,  pc.st.slope.alpha = 0.01,
                            type.st = 4,
                            bias.adj = adj.frame, bias.adj.by = adj.varnames,
                            survey.effect = TRUE,
                            strata.time.effect = strata.time.effect,
                            age.groups = levels(mod.dat$age),
                            age.n = age.n,
                            age.rw.group = age.rw.group,
                            age.strata.fixed.group = age.strata.fixed.group,
                            overdisp.mean = -7.5, overdisp.prec = 0.39)
  }
  
  ## Get smooth estimates -----------------------------------------------
  bb.res <- getSmoothed(inla_mod = bb.fit, 
                                  year_range = beg.year:end.year, 
                                  year_label = beg.year:end.year, nsim = 1000, 
                                  weight.strata = weight.strata, 
                                  weight.frame = NULL,
                                  draws = NULL, save.draws = TRUE)
  
  ## Return fit and estimates -----------------------------------------------
  ## need to figure out how to return this
  
}