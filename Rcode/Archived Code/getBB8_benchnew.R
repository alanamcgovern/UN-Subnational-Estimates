
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
## forBenchmark: logical indicator for whether fit will be used for benchmarking (NOTE: if forBenchmark=T, you will only get a model fit to use for benchmarking)
## priors.bench.weibull: priors for benchmarking (calculated from Weibull)
## priors.bench.hand: priors for benchmarking (calculated from data)
## int.priors.prec.bench: precision on priors (only used when using priors.bench.hand)
## igme.ests: IGME estimates
## adj.frame: data frame of HIV adjustments
## adj.varnames: names of relevant columns in adj.frame
## nsim: if forBenchmark=F, nsim is the number of posterior draws to be taken; if forBenchmark=T, nsim is the target number of posterior draws to be accepted

##################################################################
###### Define BB8 function
##################################################################

getBB8 <- function(mod.dat, country, beg.year, end.year, Amat,
                   time.model, st.time.model = NULL, 
                   stratified, weight.strata = NULL,
                   admin.level,
                   outcome,
                   forBenchmark, 
                   priors.bench.weibull=NULL,
                   priors.bench.hand=NULL,
                   int.priors.prec.bench=NULL, igme.ests,
                   adj.frame, adj.varnames, nsim=1000){
  
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
  
  ## Set parameters for benchmarking -----------------------------------------------
   if(forBenchmark){
     if(outcome=='nmr'){
       if(!stratified){
         if(is.null(priors.bench.weibull)){
         int.adj = list(
           mean=list(`(Intercept)`=logit(mean(igme.ests$OBS_VALUE))), 
           prec=list(`(Intercept)`=int.priors.prec.bench))
         }else{
          int.adj=list(
           mean=list(`(Intercept)`= priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_mean), 
           prec=list(`(Intercept)`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_sd))}
       }else if(stratified){
         if(is.null(priors.bench.weibull)){
          int.adj = list(
          mean=list(`age.intercept0:rural`= logit(mean(igme.ests$OBS_VALUE)),
                    `age.intercept0:urban`= logit(mean(igme.ests$OBS_VALUE))),
          prec=list(`age.intercept0:rural`=int.priors.prec.bench,
                    `age.intercept0:urban`=int.priors.prec.bench))
         }else{
          int.adj =list(
           mean=list(`age.intercept0:rural`= priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_mean,
                     `age.intercept0:urban`= priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_mean),
           prec=list(`age.intercept0:rural`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_sd,
                     `age.intercept0:urban`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_sd))}
         }
     }else if(outcome=='u5mr'){
       if(!stratified){
         if(is.null(priors.bench.weibull)){
         int.adj = list(
           mean=list(`age.intercept0`=logit(priors.bench.hand[1]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept1-11`=logit(priors.bench.hand[2]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept12-23`=logit(priors.bench.hand[3]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept24-35`=logit(priors.bench.hand[4]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept36-47`=logit(priors.bench.hand[5]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept48-59`=logit(priors.bench.hand[6]*mean(igme.ests$OBS_VALUE))),
           prec=list(`age.intercept0`=int.priors.prec.bench,
                     `age.intercept1-11`=int.priors.prec.bench,
                     `age.intercept12-23`=int.priors.prec.bench,
                     `age.intercept24-35`=int.priors.prec.bench,
                     `age.intercept36-47`=int.priors.prec.bench,
                     `age.intercept48-59`=int.priors.prec.bench))
         }else{int.adj=list(
           mean=list(`age.intercept0`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_mean,
                     `age.intercept1-11`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a2_mean,
                     `age.intercept12-23`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a3_mean,
                     `age.intercept24-35`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a4_mean,
                     `age.intercept36-47`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a5_mean,
                     `age.intercept48-59`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a6_mean),
           prec=list(`age.intercept0`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_sd,
                     `age.intercept1-11`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a2_sd,
                     `age.intercept12-23`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a3_sd,
                     `age.intercept24-35`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a4_sd,
                     `age.intercept36-47`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a5_sd,
                     `age.intercept48-59`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a6_sd))}
       }else if(stratified){
         if(is.null(priors.bench.weibull)){
         int.adj = list(
           mean=list(`age.intercept0:rural`=logit(priors.bench.hand[1]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept1-11:rural`=logit(priors.bench.hand[2]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept12-23:rural`=logit(priors.bench.hand[3]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept24-35:rural`=logit(priors.bench.hand[4]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept36-47:rural`=logit(priors.bench.hand[5]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept48-59:rural`=logit(priors.bench.hand[6]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept0:urban`=logit(priors.bench.hand[1]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept1-11:urban`=logit(priors.bench.hand[2]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept12-23:urban`=logit(priors.bench.hand[3]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept24-35:urban`=logit(priors.bench.hand[4]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept36-47:urban`=logit(priors.bench.hand[5]*mean(igme.ests$OBS_VALUE)),
                     `age.intercept48-59:urban`=logit(priors.bench.hand[6]*mean(igme.ests$OBS_VALUE))),
           prec=list(`age.intercept0:rural`=int.priors.prec.bench,
                     `age.intercept1-11:rural`=int.priors.prec.bench,
                     `age.intercept12-23:rural`=int.priors.prec.bench,
                     `age.intercept24-35:rural`=int.priors.prec.bench,
                     `age.intercept36-47:rural`=int.priors.prec.bench,
                     `age.intercept48-59:rural`=int.priors.prec.bench,
                     `age.intercept0:urban`=int.priors.prec.bench,
                     `age.intercept1-11:urban`=int.priors.prec.bench,
                     `age.intercept12-23:urban`=int.priors.prec.bench,
                     `age.intercept24-35:urban`=int.priors.prec.bench,
                     `age.intercept36-47:urban`=int.priors.prec.bench,
                     `age.intercept48-59:urban`=int.priors.prec.bench))
         }else{
          int.adj=list(
           mean=list(`age.intercept0:rural`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_mean,
                     `age.intercept1-11:rural`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a2_mean,
                     `age.intercept12-23:rural`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a3_mean,
                     `age.intercept24-35:rural`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a4_mean,
                     `age.intercept36-47:rural`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a5_mean,
                     `age.intercept48-59:rural`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a6_mean,
                     `age.intercept0:urban`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_mean,
                     `age.intercept1-11:urban`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a2_mean,
                     `age.intercept12-23:urban`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a3_mean,
                     `age.intercept24-35:urban`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a4_mean,
                     `age.intercept36-47:urban`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a5_mean,
                     `age.intercept48-59:urban`=priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a6_mean),
            prec=list(`age.intercept0:rural`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_sd,
                      `age.intercept1-11:rural`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a2_sd,
                      `age.intercept12-23:rural`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a3_sd,
                      `age.intercept24-35:rural`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a4_sd,
                      `age.intercept36-47:rural`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a5_sd,
                      `age.intercept48-59:rural`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a6_sd,
                      `age.intercept0:urban`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a1_sd,
                      `age.intercept1-11:urban`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a2_sd,
                      `age.intercept12-23:urban`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a3_sd,
                      `age.intercept24-35:urban`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a4_sd,
                      `age.intercept36-47:urban`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a5_sd,
                      `age.intercept48-59:urban`=1/priors.bench.weibull[round(nrow(priors.bench.weibull)/2),]$a6_sd))}
       }
     }
   }
  
  ## Unbenchmarked model -----------------------------------------------
  if(!forBenchmark){
    ## Fit model 
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
    bb.res <- getSmoothed(inla_mod = bb.fit, 
                          year_range = beg.year:end.year, 
                          year_label = beg.year:end.year, nsim = nsim, 
                          weight.strata = weight.strata, 
                          weight.frame = NULL,
                          CI = 0.9,
                          draws = NULL, save.draws = TRUE)
  }
  
  ## Benchmarked model  -----------------------------------------------
  if(forBenchmark){
    
    ## fit model with adjusted intercepts
    if(length(unique(mod.dat$region))<5 & !is.null(Amat)){
      message('Due to small number of areas (<5), the model will include a fixed spatial effect (rather than BYM2) and a Type 1 space-time interaction term.')
      suppressMessages({
        bb.fit.adj <- smoothCluster_mod(data = mod.dat, family = "betabinomial",
                                        Amat = Amat, 
                                        year_label = c(beg.year:end.year),
                                        time.model = time.model, 
                                        spat.fixed = T,
                                        type.st = 1,
                                        bias.adj = adj.frame, bias.adj.by = adj.varnames,
                                        survey.effect = TRUE,
                                        strata.time.effect = strata.time.effect,
                                        age.groups = age.groups,
                                        age.n = age.n,
                                        age.rw.group = age.rw.group,
                                        age.strata.fixed.group = age.strata.fixed.group,
                                        overdisp.mean = -7.5, overdisp.prec = 0.39,
                                        control.fixed = int.adj,
                                        control.inla = list(strategy='adaptive',int.strategy='auto'))
      })
      
    }else{
      bb.fit.adj <- smoothCluster(data = mod.dat, family = "betabinomial",
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
                                  control.fixed = int.adj,
                                  control.inla = list(strategy='adaptive',int.strategy='auto'))
    }
    
  }
  
  ## Return fit and estimates -----------------------------------------------
  if(!forBenchmark){
    out <- list(bb.fit,bb.res)
    names(out) <- c('fit','results')
  }
  if(forBenchmark){
    out <- list(bb.fit.adj)
    names(out) <- c('fit_bench')
  }
  return(out)
  
}