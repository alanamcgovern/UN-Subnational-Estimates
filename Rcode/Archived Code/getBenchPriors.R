#function to get Weibull priors for intercepts (for benchmarking)
#nmr_df and u5mr_df should be the igme estimates

getBenchPriors <- function(nmr_df,u5mr_df){

  # get normal asymptotic distribution for logit estimates
  # estimated from upper bound of 90% confidence interval and medians
  se_nmr_logitscale <- (logit(nmr_df$UPPER_BOUND) - logit(nmr_df$OBS_VALUE)) / 1.645
  se_u5mr_logitscale <- (logit(u5mr_df$UPPER_BOUND) - logit(u5mr_df$OBS_VALUE)) / 1.645

  # get multivariate normal samples for nmr and u5mr on logit scale 
  # (have to assume time is independent, we don't have any estimated covariance structures)

  nmr_logit_samps <- matrix(nrow = length(se_u5mr_logitscale), ncol = 1000)
  u5mr_logit_samps <- matrix(nrow = length(se_nmr_logitscale), ncol = 1000)
  for (i in 1:length(se_u5mr_logitscale)) {
    nmr_logit_samps[i,] <- rnorm(1000, mean = logit(nmr_df$OBS_VALUE[i]), sd = se_nmr_logitscale[i])
    u5mr_logit_samps[i,] <- rnorm(1000, mean = logit(u5mr_df$OBS_VALUE[i]), sd = se_u5mr_logitscale[i])
  }

  # P(x < 1) = nmr = F(1)
  # P(x < 60) = u5mr = F(60)

  ## Two equations:
  # nmr = 1 - exp(-(1/lambda)^k)
  # u5mr = 1 - exp(-(60/lambda)^k)
  # solve for (lambda, k)

  fn <- function(lambda, k) {
    a <- 1 - exp(-((1/12)/lambda)^k)
    b <- 1 - exp(-(5/lambda)^k)
    return(c(a, b))
  }
  fn2 <- function(x, nmr, u5mr) crossprod( fn(x[1], x[2]) - c(nmr, u5mr))
  optim_res <- optim(c(1,1), fn2, gr = NULL, 
                   nmr_logit_samps[1,1] %>% expit,
                   u5mr_logit_samps[1,1] %>% expit)

  # confirm these are the same (they're quite close and optim converged so we're good)
  #pweibull(1/12, shape = optim_res$par[2], scale = optim_res$par[1])
  #nmr_logit_samps[1,1] %>% expit

  #pweibull(5, shape = optim_res$par[2], scale = optim_res$par[1])
  #u5mr_logit_samps[1,1] %>% expit

  # set up data frame to contain all weibull parameters across samples
  weibull_par_df <- data.frame(shape = NA, scale = NA, year = NA)
  # inefficient but it works...
  for (i in 1:nrow(nmr_logit_samps)) {
    for (j in 1:ncol(nmr_logit_samps)) {
      optim_res <- optim(c(1,1), fn2, gr = NULL, 
                       nmr_logit_samps[i,j] %>% expit,
                       u5mr_logit_samps[i,j] %>% expit)
      weibull_par_df <- rbind(weibull_par_df, data.frame(shape = optim_res$par[2],
                                                       scale = optim_res$par[1],
                                                       year = nmr_df$year[i]))
    }
    print(i)
  }

  # remove row with NA
  weibull_par_df <- weibull_par_df[-1,] 

  # get estimates of P(x < 1), P(x < 12 | x > 1), P(x < 24 | x > 12), ...
  weibull_par_df$a1 <- pweibull(1/12, shape = weibull_par_df$shape, scale = weibull_par_df$scale)
  weibull_par_df$a2 <- 1 - (1 - pweibull(1, shape = weibull_par_df$shape, scale = weibull_par_df$scale)) / (1 - weibull_par_df$a1)
  weibull_par_df$a3 <- 1 - (1 - pweibull(2, shape = weibull_par_df$shape, scale = weibull_par_df$scale)) / (1 - weibull_par_df$a2)
  weibull_par_df$a4 <- 1 - (1 - pweibull(3, shape = weibull_par_df$shape, scale = weibull_par_df$scale)) / (1 - weibull_par_df$a3)
  weibull_par_df$a5 <- 1 - (1 - pweibull(4, shape = weibull_par_df$shape, scale = weibull_par_df$scale)) / (1 - weibull_par_df$a4)
  weibull_par_df$a6 <- 1 - (1 - pweibull(5, shape = weibull_par_df$shape, scale = weibull_par_df$scale)) / (1 - weibull_par_df$a5)

  # get estimates of shape and scale parameters for each year
  intercept_prior_df <- weibull_par_df %>%
    mutate(a1 = logit(a1), a2 = logit(a2), a3 = logit(a3),
         a4 = logit(a4), a5 = logit(a5), a6 = logit(a6)) %>%
    group_by(year) %>%
    summarise(a1_mean = mean(a1),
            a1_sd = sd(a1),
            a2_mean = mean(a2),
            a2_sd = sd(a2),
            a3_mean = mean(a3),
            a3_sd = sd(a3),
            a4_mean = mean(a4),
            a4_sd = sd(a4),
            a5_mean = mean(a5),
            a5_sd = sd(a5),
            a6_mean = mean(a6),
            a6_sd = sd(a6))

  return(intercept_prior_df)
  
}