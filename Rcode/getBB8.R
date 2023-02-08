
### Parameters -----------------------------------------------
## mod.dat: survey data
## country: country of interest
## beg.year: first year for which estimates are being calculated
## end.year: last year for which estimates are being calculated
## Amat: adjacency matrix (must agree with admin level, NULL if admin.level='National')
## admin.level: desired admin level (options = c('National', 'Admin1', 'Admin2'))
## time.model: temporal trend (options = c('ar1', 'rw1', 'rw2'))
## st.time.model: temporal component of space-time trend (options = c('ar1', 'rw1', 'rw2'))
## stratified: logical indicator for inclusion urban/rural stratification
## weight.strata: urban/rural population weights at the desired admin level (NULL if stratified=F)
## outcome: desired outcome to be modeled (options = c('u5mr', 'nmr'))
## adj.frame: data frame of HIV adjustments
## adj.varnames: names of relevant columns in adj.frame
## nsim: number of posterior draws to be taken
## fit.only: specify as true if you only want to fit model and not take posterior draws

##################################################################
###### Define BB8 function
##################################################################

getBB8 <- function(mod.dat, country, beg.year, end.year, Amat,
                   time.model, st.time.model = NULL, 
                   stratified, weight.strata = NULL,
                   admin.level,
                   outcome,
                   adj.frame, adj.varnames, 
                   nsim=1000, fit.only=F){
  
  ## Check inputs -----------------------------------------------
  if(!(admin.level %in% c('National','Admin1','Admin2'))){
    stop('Enter a valid admin level (National, Admin1, or Admin2)')
  }
  if(!(outcome %in% c('u5mr','nmr'))){
    stop('Enter a valid outcome measure (u5mr or nmr)')
  }
  if((admin.level!='National') == (is.null(Amat)==T)){
    stop('Admin level and adjacency matrix are not compatible')}
  if((admin.level!='National') & (is.null(st.time.model)==T)){
    stop('Specify the temporal component of the space-time interaction term (st.time.model)')}
  if(!(time.model %in% c('ar1','rw1','rw2'))){
    stop('Enter a valid main temporal effect type (ar1, rw1, or rw2)')
  }
  if(stratified & is.null(weight.strata)){
    stop('Specify urban/rural stratification weights')
  }
  
  ## Set stratification parameters -----------------------------------------------
  if(stratified){
    mod.dat$strata <- mod.dat$urban
    strata.time.effect <- T
  }else{
    mod.dat$strata <- NA
    strata.time.effect <- F
    weight.strata <- NULL
  }
  
  ## Set admin-level parameters -----------------------------------------------
  if(admin.level=='National'){
    mod.dat$region <- "All"
    st.time.model = NULL
  }else if(admin.level=='Admin1'){
    mod.dat$region <- mod.dat$admin1.char
  }else if(admin.level=='Admin2'){
    mod.dat$region <- mod.dat$admin2.char}
  ## Set outcome specific parameters -----------------------------------------------
  if(outcome=='nmr'){
    mod.dat <- mod.dat %>% filter(age=='0')
    age.groups <- c('0')
    age.n <- age.rw.group <- age.strata.fixed.group <- c(1)
  }else{
    age.groups <- levels(mod.dat$age)
    age.n <- c(1, 11, 12, 12, 12, 12)
    age.rw.group <- c(1, 2, 3, 3, 3, 3)
    age.strata.fixed.group <- c(1, 2, 3, 4, 5, 6)
  }
  
  ## Fit model -----------------------------------------------
    if(length(unique(mod.dat$region))<5 & !is.null(Amat)){
      message('Due to small number of areas (<5), the model will include a fixed spatial effect (rather than BYM2) and a Type 1 space-time interaction term.')
      suppressMessages({
        bb.fit <- smoothCluster_mod(data = mod.dat, family = "betabinomial",
                                    Amat = Amat, 
                                    year_label = c(beg.year:end.year),
                                    time.model = time.model,
                                    type.st = 1,
                                    spat.fixed=T,
                                    bias.adj = adj.frame, bias.adj.by = adj.varnames,
                                    survey.effect = TRUE, strata.time.effect = strata.time.effect, 
                                    age.groups = age.groups, age.n = age.n,
                                    age.rw.group = age.rw.group, 
                                    age.strata.fixed.group = age.strata.fixed.group,
                                    overdisp.mean = -7.5, overdisp.prec = 0.39,
                                    control.inla = list(strategy = "adaptive", int.strategy = "auto"))
      })
      
    }else{
      bb.fit <- smoothCluster(data = mod.dat, family = "betabinomial",
                              Amat = Amat, 
                              year_label = c(beg.year:end.year),
                              time.model = time.model, st.time.model = st.time.model,
                              pc.st.slope.u = 1,  pc.st.slope.alpha = 0.01,
                              type.st = 4,
                              bias.adj = adj.frame, bias.adj.by = adj.varnames,
                              survey.effect = TRUE,
                              strata.time.effect = strata.time.effect,
                              age.groups = age.groups,
                              age.n = age.n,
                              age.rw.group = age.rw.group,
                              age.strata.fixed.group = age.strata.fixed.group,
                              overdisp.mean = -7.5, overdisp.prec = 0.39,
                              control.inla = list(strategy = "adaptive", int.strategy = "auto"))
    }
    
    ## Get posterior draws -----------------------------------------------
  if(!fit.only){
    bb.res <- getSmoothed(inla_mod = bb.fit, 
                          year_range = beg.year:end.year, 
                          year_label = beg.year:end.year, nsim = nsim, 
                          weight.strata = weight.strata, 
                          weight.frame = NULL,
                          CI = 0.9,
                          draws = NULL, save.draws = TRUE)
  }
  
  ## Return fit and estimates -----------------------------------------------
  if(!fit.only){
    out <- list(bb.fit,bb.res)
    names(out) <- c('fit','results')
  }
  if(fit.only){
    out <- list(bb.fit)
    names(out) <- c('fit')
  }
  return(out)
  
}