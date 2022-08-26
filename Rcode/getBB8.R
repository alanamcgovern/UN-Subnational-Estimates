
### Parameters -----------------------------------------------
## options for time.model and st.time.model = 'ar1', 'rw1', 'rw2'
## weight.strata is NULL unless stratified = T
## options for admin.level = 'National', 'Admin1','Admin2'
## Amat = NULL iff admin.level = 'National'
## options for outcome = 'u5mr', 'nmr'
## adj.frame and adj.varnames are calculated in HIV adjustment step -- might change when benchmarking is implemented
## doBenchmark is not yet functional
# 
   end.year=end.proj.year
          Amat=admin1.mat
          admin.level='Admin1'
          stratified=T
          weight.strata=weight.strata.adm1.u1
          weight.region = weight.region.adm1.nmr
          outcome='nmr'
          time.model='ar1' 
          st.time.model='ar1'
          doBenchmark=T
          igme.ests <- igme.ests.nmr
          nsim = 1000

##################################################################
###### Define BB8 function
##################################################################

getBB8 <- function(mod.dat, country, beg.year, end.year, Amat,
                    time.model, st.time.model, 
                    stratified, weight.strata,
                    admin.level, weight.region,
                    outcome,
                    doBenchmark, igme.ests,
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
          mean=list(`(Intercept):1`=logit(mean(igme.ests.nmr$OBS_VALUE))), 
          #precision=5, acr=0.002 ;precision=1, acr=0.006;
          prec=list(`(Intercept):1`=.5))
      }else if(stratified){
        int.adj = list(
          mean=list(`age.intercept0:rural:1`= logit(mean(igme.ests.nmr$OBS_VALUE)),
                    `age.intercept0:urban:1`= logit(mean(igme.ests.nmr$OBS_VALUE))), 
          prec=list(`age.intercept0:rural:1`=10,
                    `age.intercept0:urban:1`=10))
      }
    }else if(outcome=='u5mr'){
      if(!stratified){
        
      }else if(stratified){
        
      }
    }
  }
  
  ## Fit model -----------------------------------------------
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
                            overdisp.mean = -7.5, overdisp.prec = 0.39)
  
  ## Get posterior draws -----------------------------------------------
  bb.res <- getSmoothed(inla_mod = bb.fit, 
                                  year_range = beg.year:end.year, 
                                  year_label = beg.year:end.year, nsim = nsim, 
                                  weight.strata = weight.strata, 
                                  weight.frame = NULL,
                                  draws = NULL, save.draws = TRUE)
  
  ## Benchmark  -----------------------------------------------
  if(doBenchmark){
    message('Starting benchmarking...')
    
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
                                control.fixed = int.adj)
    
    ## Get posterior draws from adjusted model
    bb.res.adj.tmp <- getSmoothed(inla_mod = bb.fit.adj, 
                          year_range = beg.year:end.year, 
                          year_label = beg.year:end.year, nsim = 50000, 
                          weight.strata = weight.strata, 
                          weight.frame = NULL,
                          CI=0.95, draws = NULL, save.draws = TRUE)
    
    ## Get approximation of acceptance ratio
    bench.acr <- 0
    suppressMessages({
    for(i in 1:5){
    bench.tmp <- Benchmark(bb.res.adj.tmp,igme.ests,weight.region = weight.region,
                                    estVar = 'OBS_VALUE',sdVar = 'SD',timeVar = 'year',method = 'MH')
    bench.acr <- bench.acr + bench.tmp$accept.ratio/5
    }})
    
    ## Now we know how many draws we need to get a certain number (nsim) accepted
    message('Acceptance rate is ', bench.acr, '. Taking ', round(nsim/bench.acr), ' posterior draws to acheive approximately ', nsim, ' accepted draws.')
    bb.res.adj <- getSmoothed(inla_mod = bb.fit.adj, 
                                 year_range = beg.year:end.year, 
                                 year_label = beg.year:end.year, nsim = round(nsim/bench.acr), 
                                 weight.strata = weight.strata, 
                                 weight.frame = NULL,
                                 CI=0.95, save.draws = TRUE)
    
    ## Final benchmark
    bb.res.bench <- Benchmark(bb.res.adj,igme.ests,weight.region = weight.region,
                              estVar = 'OBS_VALUE',sdVar = 'SD',timeVar = 'year',method = 'MH')
   
    # traceplots of fitted values for a single region/time
    year <- bb.res.bench$draws.est.overall[[35]]$years
    region <- bb.res.bench$draws.est.overall[[35]]$region
    
    data.frame(iteration = 1:length(bb.res.bench$draws.est.overall[[35]]$draws[c(1:10000)%%50 ==0]),
               fitted_val = bb.res.bench$draws.est.overall[[35]]$draws[c(1:10000)%%50 ==0]) %>%
      ggplot(aes(iteration, fitted_val)) +
      geom_line() +
      ggtitle(paste0("Year ", year, ", region ", region)) 
  }
   
  ## Return fit and estimates -----------------------------------------------
  out <- list(bb.fit,bb.res,bb.res.bench)
  names(out) <- c('fit','results','results_bench')
  return(out)
  
}
