
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
## weight.region: admin-level population weights (NULL if admin.level='National')
## outcome: desired outcome to be modeled (options = c('u5mr', 'nmr'))
## doBenchmark: logical indicator for calculation of benchmarked estimates (NOTE: if doBenchmark=T, ONLY benchmarked estimates will be returned)
## igme.ests: data frame of national igme estimates for benchmarking (NULL if doBenchmark=F)
## int.priors.bench: data-specific component of mean on intercept priors for fitting adjusted model for benchmarking
## int.priors.prec.bench: precision of intercept priors for fitting adjusted model for benchmarking
## adj.frame: data frame of HIV adjustments
## adj.varnames: names of relevant columns in adj.frame
## nsim: if doBenchmark=F, nsim is the number of posterior draws to be taken; if doBenchmark=T, nsim is the target number of posterior draws to be accepted

### useful for troubleshooting ----------------------------------------------
     # end.year=end.proj.year
     #        Amat=admin1.mat
     #        admin.level='Admin1'
     #        stratified=T
     #        weight.strata=weight.strata.adm1.u1
     #        outcome='nmr'
     #        time.model='ar1'
     #        st.time.model='ar1'
     #        weight.region = weight.adm1.u1
     #        igme.ests = igme.ests.nmr
     #        int.priors.bench = int.priors.bench
     #        int.priors.prec.bench = 10
     #        doBenchmark=T
     #        nsim=1000

##################################################################
###### Define BB8 function
##################################################################

getBB8 <- function(mod.dat, country, beg.year, end.year, Amat,
                   time.model, st.time.model = NULL, 
                   stratified, weight.strata = NULL,
                   admin.level, weight.region = NULL,
                   outcome,
                   doBenchmark, igme.ests = NULL, 
                   int.priors.bench = NULL, int.priors.prec.bench=10,
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
  if(doBenchmark & is.null(igme.ests)){
    stop('To perform benchmarking, IGME estimates (igme.ests) must be provided')
  }
  if(doBenchmark & is.null(weight.region)){
    stop('To perform benchmarking, admin-level weights must be provided')
  }
  if(outcome=='u5mr' & doBenchmark  & sum(is.nan(int.priors.bench)) + sum(is.na(int.priors.bench)) >0){
    stop('Priors on intercepts (priors.int.bench) contain missing values')
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
  if(doBenchmark){
    if(outcome=='nmr'){
      if(!stratified){
        int.adj = list(
          mean=list(`(Intercept):1`=logit(mean(igme.ests$OBS_VALUE))), 
          prec=list(`(Intercept):1`=int.priors.prec.bench))
      }else if(stratified){
        int.adj = list(
          mean=list(`age.intercept0:rural:1`= logit(mean(igme.ests$OBS_VALUE)),
                    `age.intercept0:urban:1`= logit(mean(igme.ests$OBS_VALUE))), 
          prec=list(`age.intercept0:rural:1`=int.priors.prec.bench,
                    `age.intercept0:urban:1`=int.priors.prec.bench))
      }
    }else if(outcome=='u5mr'){
      if(!stratified){
        int.adj = list(
          mean=list(`age.intercept0:1`=logit((sum(mod.dat[mod.dat$age=='0',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
                    `age.intercept1-11:1`=logit((sum(mod.dat[mod.dat$age=='1-11',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)), 
                    `age.intercept12-23:1`=logit((sum(mod.dat[mod.dat$age=='12-23',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
                    `age.intercept24-35:1`=logit((sum(mod.dat[mod.dat$age=='24-35',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
                    `age.intercept36-47:1`=logit((sum(mod.dat[mod.dat$age=='36-47',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
                    `age.intercept48-59:1`=logit((sum(mod.dat[mod.dat$age=='48-59',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE))),
          prec=list(`age.intercept0:1`=int.priors.prec.bench,
                    `age.intercept1-11:1`=int.priors.prec.bench, 
                    `age.intercept12-23:1`=int.priors.prec.bench,
                    `age.intercept24-35:1`=int.priors.prec.bench,
                    `age.intercept36-47:1`=int.priors.prec.bench,
                    `age.intercept48-59:1`=int.priors.prec.bench))
      }else if(stratified){
        int.adj = list(
          mean=list(`age.intercept0:rural:1`=logit(int.priors.bench[1]*mean(igme.ests$OBS_VALUE)),
                    `age.intercept1-11:rural:1`=logit(int.priors.bench[2]*mean(igme.ests$OBS_VALUE)), 
                    `age.intercept12-23:rural:1`=logit(int.priors.bench[3]*mean(igme.ests$OBS_VALUE)),
                    `age.intercept24-35:rural:1`=logit(int.priors.bench[4]*mean(igme.ests$OBS_VALUE)),
                    `age.intercept36-47:rural:1`=logit(int.priors.bench[5]*mean(igme.ests$OBS_VALUE)),
                    `age.intercept48-59:rural:1`=logit(int.priors.bench[6]*mean(igme.ests$OBS_VALUE)),
                    `age.intercept0:urban:1`=logit(int.priors.bench[1]*mean(igme.ests$OBS_VALUE)),
                    `age.intercept1-11:urban:1`=logit(int.priors.bench[2]*mean(igme.ests$OBS_VALUE)),
                    `age.intercept12-23:urban:1`=logit(int.priors.bench[3]*mean(igme.ests$OBS_VALUE)),
                    `age.intercept24-35:urban:1`=logit(int.priors.bench[4]*mean(igme.ests$OBS_VALUE)),
                    `age.intercept36-47:urban:1`=logit(int.priors.bench[5]*mean(igme.ests$OBS_VALUE)),
                    `age.intercept48-59:urban:1`=logit(int.priors.bench[6]*mean(igme.ests$OBS_VALUE))),
          prec=list(`age.intercept0:rural:1`=int.priors.prec.bench,
                    `age.intercept1-11:rural:1`=int.priors.prec.bench, 
                    `age.intercept12-23:rural:1`=int.priors.prec.bench,
                    `age.intercept24-35:rural:1`=int.priors.prec.bench,
                    `age.intercept36-47:rural:1`=int.priors.prec.bench,
                    `age.intercept48-59:rural:1`=int.priors.prec.bench,
                    `age.intercept0:urban:1`=int.priors.prec.bench,
                    `age.intercept1-11:urban:1`=int.priors.prec.bench, 
                    `age.intercept12-23:urban:1`=int.priors.prec.bench,
                    `age.intercept24-35:urban:1`=int.priors.prec.bench,
                    `age.intercept36-47:urban:1`=int.priors.prec.bench,
                    `age.intercept48-59:urban:1`=int.priors.prec.bench))
      }
    }
  }
  
  ## Unbenchmarked model -----------------------------------------------
  if(!doBenchmark){
    ## Fit model 
    if(length(unique(mod.dat$region))<5 & !is.null(Amat)){
      message('Due to small number of areas (<5), the model will include a fixed spatial effect (rather than BYM2) and a Type 1 space-time interaction term.')
      suppressMessages({
        bb.fit <- smoothCluster_mod(data = mod.dat, family = "betabinomial",
                                    Amat = Amat, 
                                    year_label = c(beg.year:end.year),
                                    time.model = time.model,
                                    pc.st.slope.u = 1,  pc.st.slope.alpha = 0.01,
                                    type.st = 2, spat.fixed=T,
                                    bias.adj = adj.frame, bias.adj.by = adj.varnames,
                                    survey.effect = TRUE, strata.time.effect = strata.time.effect, 
                                    age.groups = age.groups, age.n = age.n,
                                    age.rw.group = age.rw.group, 
                                    age.strata.fixed.group = age.strata.fixed.group,
                                    overdisp.mean = -7.5, overdisp.prec = 0.39,
                                    control.inla = list(strategy = "adaptive", int.strategy = "eb"))
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
                              control.inla = list(strategy = "adaptive", int.strategy = "eb"))
    }
    
    
    ## Get posterior draws -----------------------------------------------
    bb.res <- getSmoothed(inla_mod = bb.fit, 
                          year_range = beg.year:end.year, 
                          year_label = beg.year:end.year, nsim = nsim, 
                          weight.strata = weight.strata, 
                          weight.frame = NULL,
                          draws = NULL, save.draws = TRUE)
  }
  
  ## Benchmarked model  -----------------------------------------------
  if(doBenchmark){
    
    ## fit model with adjusted intercepts
    if(length(unique(mod.dat$region))<5 & !is.null(Amat)){
      message('Due to small number of areas (<5), the model will include a fixed spatial effect (rather than BYM2) and a Type 1 space-time interaction term.')
      suppressMessages({
        bb.fit.adj <- smoothCluster_mod(data = mod.dat, family = "betabinomial",
                                        Amat = Amat, 
                                        year_label = c(beg.year:end.year),
                                        time.model = time.model, 
                                        pc.st.slope.u = 1,  pc.st.slope.alpha = 0.01,
                                        type.st = 1, spat.fixed = T,
                                        bias.adj = adj.frame, bias.adj.by = adj.varnames,
                                        survey.effect = TRUE,
                                        strata.time.effect = strata.time.effect,
                                        age.groups = age.groups,
                                        age.n = age.n,
                                        age.rw.group = age.rw.group,
                                        age.strata.fixed.group = age.strata.fixed.group,
                                        overdisp.mean = -7.5, overdisp.prec = 0.39,
                                        control.fixed = int.adj,
                                        control.inla = list(strategy='adaptive',int.strategy='eb'))
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
                                  control.inla = list(strategy='adaptive',int.strategy='eb'))
    }
    
    ## Get posterior draws from adjusted model
    bb.res.tmp <- getSmoothed(inla_mod = bb.fit.adj, 
                              year_range = beg.year:end.year, 
                              year_label = beg.year:end.year, nsim = 10000, 
                              weight.strata = weight.strata, 
                              weight.frame = NULL,
                              CI=0.95, draws = NULL, save.draws = TRUE)
    
    ## Get approximation of acceptance ratio
    suppressMessages({
      bb.res.bench <- Benchmark(bb.res.tmp,igme.ests,weight.region = weight.region,
                                estVar = 'OBS_VALUE',sdVar = 'SD',timeVar = 'year',method = 'MH')
      bench.acr <- bb.res.bench$accept.ratio
    })
    
    ## Now we know how many draws we need to get a certain number (nsim) accepted
    if(bench.acr<0.001){
      stop(paste0('Acceptance ratio is approximately ', bench.acr, ', which is very small. Check specification of priors on intercepts (int.priors.bench).'))
    }
    message('Acceptance rate is ', bench.acr, '. Taking approximately ', round(nsim/bench.acr,-3), ' posterior draws to achieve approximately ', nsim, ' accepted draws.')
    
    # update number of draws that have been accepted
    tot_accepted_draws <- round(bench.acr*10000)
    
    message(paste0(tot_accepted_draws, ' posterior draws have been accepted.'))
    
    # while less than nsim draws have been accepted, take more samples 
    while (tot_accepted_draws < nsim) {
      message('Taking 10000 more posterior draws.')
      suppressMessages({
        tmp <- getSmoothed(inla_mod = bb.fit.adj, 
                           year_range = beg.year:end.year, 
                           year_label = beg.year:end.year, nsim = 10000, 
                           weight.strata = weight.strata, 
                           weight.frame = NULL,
                           CI=0.95, save.draws = TRUE)
        
        # combine draws with previously taken draws
        bb.res.tmp$draws <- c(bb.res.tmp$draws,tmp$draws)
        for (i in 1:length(tmp$draws.est)) {
          bb.res.tmp$draws.est[[i]]$draws <- c(bb.res.tmp$draws.est[[i]]$draws, tmp$draws.est[[i]]$draws)
        }
        for (i in 1:length(tmp$draws.est.overall)) {
          bb.res.tmp$draws.est.overall[[i]]$draws <- c(bb.res.tmp$draws.est.overall[[i]]$draws, tmp$draws.est.overall[[i]]$draws)
        }
        
        # run benchmarking again
        bb.res.bench <- Benchmark(bb.res.tmp,igme.ests,weight.region = weight.region,
                                  estVar = 'OBS_VALUE',sdVar = 'SD',timeVar = 'year',method = 'MH')
      })
      
      # add accepted draws to tot_accepted_draws
      tot_accepted_draws <- round(bench.acr*length(bb.res.bench$draws))
      
      message(paste0(tot_accepted_draws, ' posterior draws have been accepted.'))
    }
    
  }
  
  ## Return fit and estimates -----------------------------------------------
  if(!doBenchmark){
    out <- list(bb.fit,bb.res)
    names(out) <- c('fit','results')
  }
  if(doBenchmark){
    out <- list(bb.fit.adj,bb.res.bench)
    names(out) <- c('fit_bench','results_bench')
  }
  return(out)
  
}