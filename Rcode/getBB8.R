
### Parameters -----------------------------------------------
## options for time.model and st.time.model = 'ar1', 'rw1', 'rw2'
## weight.strata is NULL unless stratified = T
## options for admin.level = 'National', 'Admin1','Admin2'
## Amat = NULL iff admin.level = 'National'
## options for outcome = 'u5mr', 'nmr'
## adj.frame and adj.varnames are calculated in HIV adjustment step
# 
# 
data = mod.dat
end.year=end.proj.year
       Amat=admin1.mat
       admin.level='Admin1'
       stratified=T
       weight.strata=weight.strata.adm1.u5
       outcome='u5mr'
       time.model='ar1'
       st.time.model='ar1'
       weight.region = weight.adm1.u5
       igme.ests = igme.ests.u5
       int.priors.bench = int.priors.bench
       doBenchmark=T
       nsim=1000

##################################################################
###### Define BB8 function
##################################################################

getBB8 <- function(mod.dat, country, beg.year, end.year, Amat,
                    time.model, st.time.model, 
                    stratified, weight.strata = NULL,
                    admin.level, weight.region = NULL,
                    outcome,
                    doBenchmark, igme.ests = NULL, int.priors.bench = NULL,
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
          prec=list(`(Intercept):1`=10))
      }else if(stratified){
        int.adj = list(
          mean=list(`age.intercept0:rural:1`= logit(mean(igme.ests$OBS_VALUE)),
                    `age.intercept0:urban:1`= logit(mean(igme.ests$OBS_VALUE))), 
          prec=list(`age.intercept0:rural:1`=10,
                    `age.intercept0:urban:1`=10))
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
          prec=list(`age.intercept0:1`=10,
                    `age.intercept1-11:1`=10, 
                    `age.intercept12-23:1`=10,
                    `age.intercept24-35:1`=10,
                    `age.intercept36-47:1`=10,
                    `age.intercept48-59:1`=10))
      }else if(stratified){
        int.adj = list(
          # mean=list(`age.intercept0:rural:1`=logit((sum(mod.dat[mod.dat$age=='0',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
          #           `age.intercept1-11:rural:1`=logit((sum(mod.dat[mod.dat$age=='1-11',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)), 
          #           `age.intercept12-23:rural:1`=logit((sum(mod.dat[mod.dat$age=='12-23',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
          #           `age.intercept24-35:rural:1`=logit((sum(mod.dat[mod.dat$age=='24-35',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
          #           `age.intercept36-47:rural:1`=logit((sum(mod.dat[mod.dat$age=='36-47',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
          #           `age.intercept48-59:rural:1`=logit((sum(mod.dat[mod.dat$age=='48-59',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
          #           `age.intercept0:urban:1`=logit((sum(mod.dat[mod.dat$age=='0',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
          #           `age.intercept1-11:urban:1`=logit((sum(mod.dat[mod.dat$age=='1-11',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)), 
          #           `age.intercept12-23:urban:1`=logit((sum(mod.dat[mod.dat$age=='12-23',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
          #           `age.intercept24-35:urban:1`=logit((sum(mod.dat[mod.dat$age=='24-35',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
          #           `age.intercept36-47:urban:1`=logit((sum(mod.dat[mod.dat$age=='36-47',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE)),
          #           `age.intercept48-59:urban:1`=logit((sum(mod.dat[mod.dat$age=='48-59',]$Y))/(sum(mod.dat$Y))*mean(igme.ests$OBS_VALUE))),
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
          prec=list(`age.intercept0:rural:1`=10,
                    `age.intercept1-11:rural:1`=10, 
                    `age.intercept12-23:rural:1`=10,
                    `age.intercept24-35:rural:1`=10,
                    `age.intercept36-47:rural:1`=10,
                    `age.intercept48-59:rural:1`=10,
                    `age.intercept0:urban:1`=10,
                    `age.intercept1-11:urban:1`=10, 
                    `age.intercept12-23:urban:1`=10,
                    `age.intercept24-35:urban:1`=10,
                    `age.intercept36-47:urban:1`=10,
                    `age.intercept48-59:urban:1`=10))
      }
    }
  }
  
  ## Unbenchmarked model -----------------------------------------------
  if(!doBenchmark){
    ## Fit model 
   # bb.fit <- smoothCluster_mod(data = mod.dat, family = "betabinomial", ## change back function call!!
   #                          Amat = Amat, 
   #                          year_label = c(beg.year:end.year),
   #                          time.model = time.model, st.time.model = 'rw2',
   #                          pc.st.slope.u = 1,  pc.st.slope.alpha = 0.01,
   #                          ## change back!!
   #                          type.st = 1, spat.fixed=T,
   #                          bias.adj = adj.frame, bias.adj.by = adj.varnames,
   #                          survey.effect = TRUE,
   #                          strata.time.effect = strata.time.effect,
   #                          age.groups = age.groups,
   #                          age.n = age.n,
   #                          age.rw.group = age.rw.group,
   #                          age.strata.fixed.group = age.strata.fixed.group,
   #                          overdisp.mean = -7.5, overdisp.prec = 0.39)
    
    bb.fit <- smoothCluster(data = mod.dat, family = "betabinomial", ## change back function call!!
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
                                overdisp.mean = -7.5, overdisp.prec = 0.39)
  
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
    
    ## Get posterior draws from adjusted model
    bb.res.tmp <- getSmoothed(inla_mod = bb.fit.adj, 
                          year_range = beg.year:end.year, 
                          year_label = beg.year:end.year, nsim = 10000, 
                          weight.strata = weight.strata, 
                          weight.frame = NULL,
                          CI=0.95, draws = NULL, save.draws = TRUE)
    
    ## Get approximation of acceptance ratio
    suppressMessages({
    bench.acr <- Benchmark(bb.res.tmp,igme.ests,weight.region = weight.region,
                                    estVar = 'OBS_VALUE',sdVar = 'SD',timeVar = 'year',method = 'MH')$accept.ratio
    })
    
    ## Now we know how many draws we need to get a certain number (nsim) accepted
    if(bench.acr<0.01){
      stop(paste0('Acceptance ratio is approximately ', bench.acr, ', which is very small. Check specification of priors on intercepts (int.adj).'))
    }
    
    message('Acceptance rate is ', bench.acr, '. Taking ', round(nsim/bench.acr), ' posterior draws to achieve approximately ', nsim, ' accepted draws.')
    bb.res.adj <- getSmoothed(inla_mod = bb.fit.adj, 
                                year_range = beg.year:end.year, 
                                year_label = beg.year:end.year, nsim = round(nsim/bench.acr), 
                                weight.strata = weight.strata, 
                                weight.frame = NULL,
                                CI=0.95, save.draws = TRUE)
    
    ## Final benchmark
    bb.res.bench <- Benchmark(bb.res.adj,igme.ests,weight.region = weight.region,
                              estVar = 'OBS_VALUE',sdVar = 'SD',timeVar = 'year',method = 'MH')

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
