#function to run Taylor's benchmarking on a model fit with getBB8_benchnew: (not used in pipeline)

getBB8.bench <- function(bb.fit, beg.year, end.year, 
         weight.strata = NULL, weight.region,
         igme.ests, acc.draws){

  # add ':1' to names of fixed priors because of SUMMER bug
  names(bb.fit$control.fixed$mean) <- paste0(names(bb.fit$control.fixed$mean),':1')
  names(bb.fit$control.fixed$prec) <- paste0(names(bb.fit$control.fixed$prec),':1')
  message('Taking 10000 posterior draws.')

  suppressMessages({
   ## Get posterior draws from adjusted model
    bb.res.tmp <- getSmoothed(inla_mod = bb.fit, 
                            year_range = beg.year:end.year, 
                            year_label = beg.year:end.year, nsim = 1e5, 
                            weight.strata = weight.strata, 
                            weight.frame = NULL,
                            CI=0.9, draws = NULL, save.draws = TRUE)
    ## Get approximation of acceptance ratio
    bb.res.bench <- Benchmark(bb.res.tmp,igme.ests,weight.region = weight.region,
                            estVar = 'OBS_VALUE',sdVar = 'SD',timeVar = 'year',method = 'Rejection')
    bench.acr <- bb.res.bench$accept.ratio
  })

  # update number of draws that have been accepted
  tot_accepted_draws <- round(bench.acr*10000)

  message(paste0(tot_accepted_draws, ' posterior draws have been accepted.'))

  # while less than necessary draws have been accepted, take more samples 
  while (tot_accepted_draws < acc.draws) {
    message('Taking 10000 more posterior draws.')
    suppressMessages({
      tmp <- getSmoothed(inla_mod = bb.fit, 
                       year_range = beg.year:end.year, 
                       year_label = beg.year:end.year, nsim = 10000, 
                       weight.strata = weight.strata, 
                       weight.frame = NULL,
                       CI=0.9, draws=NULL, save.draws = TRUE)
    
      # combine draws with previously taken draws
      bb.res.tmp$draws <- c(bb.res.tmp$draws,tmp$draws)
      for (i in 1:length(tmp$draws.est)) {
        bb.res.tmp$draws.est[[i]]$draws <- c(bb.res.tmp$draws.est[[i]]$draws, tmp$draws.est[[i]]$draws)
      }
      for (i in 1:length(tmp$draws.est.overall)) {
        bb.res.tmp$draws.est.overall[[i]]$draws <- c(bb.res.tmp$draws.est.overall[[i]]$draws, tmp$draws.est.overall[[i]]$draws)
      }
      bb.res.tmp$nsim <- length(bb.res.tmp$draws)
    
      # run benchmarking again
      bb.res.bench <- Benchmark(bb.res.tmp,igme.ests,weight.region = weight.region,
                              estVar = 'OBS_VALUE',sdVar = 'SD',timeVar = 'year',method = 'Rejection')
    })
  
    # add accepted draws to tot_accepted_draws
    tot_accepted_draws <- round(bb.res.bench$accept.ratio*length(bb.res.bench$draws))
  
    message(paste0(tot_accepted_draws, ' posterior draws have been accepted.'))
  }
  
  return(bb.res.bench)
}