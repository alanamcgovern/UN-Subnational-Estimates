Benchmark_mod <- function (fitted, national, estVar, sdVar, timeVar = NULL, weight.region = NULL) 
{
  if (!is(fitted, "SUMMERprojlist")) {
    if (is(fitted, "SUMMERproj")) {
      stop("Please use the full returned object from getSmoothed() with 'save.draws = TRUE'")
    }
    else {
      stop("The argument 'fitted' needs to be a returned object from getSmoothed() with 'save.draws = TRUE'")
    }
  }
  if (!is.null(fitted$final)) {
    stop("Benchmarking on multiple-frame model not implemented yet.")
  }
  if (is.null(fitted$CI)) {
    stop("Please rerun the getSmoothed() function with a valid CI.")
  }else {
    lowerCI <- (1 - fitted$CI)/2
    upperCI <- 1 - (1 - fitted$CI)/2
  }
  is.time <- length(unique(fitted$stratified$years)) > 1
  ntime <- max(fitted$overall$time)
  nregion <- max(fitted$overall$area)
  years <- fitted$overall$years[match(1:max(fitted$overall$time), 
                                      fitted$overall$time)]
  if (is.null(weight.region)) {
    stop("weight.region argument is required.")
  }
  if ("region" %in% colnames(weight.region) == FALSE) {
    stop("There is no 'region' column in weight.region.")
  }
  if ("years" %in% colnames(weight.region) == FALSE && is.time) {
    stop("There is no 'years' column in weight.region.")
  }
  if ("proportion" %in% colnames(weight.region) == FALSE) {
    if (is.time && dim(weight.region)[2] == 3) {
      tmp <- colnames(weight.region)[!colnames(weight.region) %in% 
                                       c("region", "years")]
      weight.region$proportion <- weight.region[, tmp]
      message(paste0("Use ", tmp, " column as the population proportion."))
    }
    else if (!is.time && dim(weight.region)[2] == 2) {
      tmp <- colnames(weight.region)[!colnames(weight.region) %in% 
                                       c("region")]
      weight.region$proportion <- weight.region[, tmp]
      message(paste0("Use ", tmp, " column as the population proportion."))
    }
    else {
      stop("Cannot determine the column indicating population proportion. Specify the column name to be 'proportion'.")
    }
  }
  if (sum(!fitted$stratified$region %in% weight.region$region) > 
      0 && nregion > 1) {
    stop("weight.region$region does not contain all the regions in the fitted model.")
  }
 
  if (is.time) {
    if (sum(!weight.region$years %in% national[,timeVar]) > 0) {
      weight.region <- weight.region[weight.region$years %in% 
                                       national$years, ]
    }
    if (sum(!fitted$stratified$years %in% weight.region$years) > 
        0) {
      tmp <- unique(fitted$stratified$years[fitted$stratified$years %in% 
                                              weight.region$years])
      warning(paste0("weight.region$years does not contain all the time periods in the fitted model. Benchmarking only performed for the following period:\n", 
                     paste(tmp, collapse = ", ")))
    }
  }
  if (is.time) {
    for (j in unique(weight.region$years)) {
      sub <- which(weight.region$years == j)
      if (abs(1 - sum(weight.region$proportion[sub])) > 
          0.001) {
        stop(paste0("Population proportion in ", j, " does not sum to 1."))
      }
    }
  }
  else {
    if (abs(1 - sum(weight.region$proportion)) > 0.001) {
      stop(paste0("Population proportion does not sum to 1."))
    }
  }
  t_sub <- 1:ntime
  nat <- data.frame(est = national[, estVar], sd = national[, 
                                                            sdVar])
  if (!is.null(timeVar)) {
    nat$years = national[, timeVar]
    order <- match(years, nat$years)
    nat <- nat[order[which(!is.na(order))], ]
    t_sub <- 1:dim(nat)[1]
  }
  n0 <- length(fitted$draws)
  q_mat <- matrix(0, nrow = ntime, ncol = n0)
  if (nregion == 1) {
    for (i in 1:length(fitted$draws.est.overall)) {
      index <- which(years == fitted$draws.est.overall[[i]]$years)
      q_mat[index, ] <- fitted$draws.est.overall[[i]]$draws
    }
  }
  else {
    for (i in 1:length(fitted$draws.est.overall)) {
      index <- which(years == fitted$draws.est.overall[[i]]$years)
      tmp <- which(weight.region$years == fitted$draws.est.overall[[i]]$years & 
                     weight.region$region == fitted$draws.est.overall[[i]]$region)
      if (length(tmp) == 0) 
        next
      tmp_pop <- weight.region$proportion[tmp]
      q_mat[index, ] <- q_mat[index, ] + fitted$draws.est.overall[[i]]$draws * 
        tmp_pop
    }
  }
  q_mat <- q_mat[t_sub, ]
  U <- runif(n0, 0, 1)
  fitted_list <- list()
  prop_accepted <- 0
  accept_ratio <- matrix(0, nrow = nrow(q_mat), ncol = ncol(q_mat))
  for (i in 1:dim(q_mat)[1]) {
    for (j in 1:ncol(q_mat)) {
      accept_ratio[i, j] <- exp((-1/(2 * nat$sd[i]^2)) * 
                                  (q_mat[i, j] - nat$est[i])^2)
    }
  }
  multi_accepted_samps <- rep(FALSE, ncol(accept_ratio))
  for (i in 1:ncol(accept_ratio)) {
    multi_accepted_samps[i] <- (U[i] < prod(accept_ratio[, 
                                                         i]))
  }
  acc <- which(multi_accepted_samps == TRUE)
  fitted$msg <- paste0(fitted$msg, "\nThe posterior draws have been benchmarked to external information. The acceptance ratio is ", 
                       round(length(acc)/n0, 3), "\n")
  message("The posterior draws have been benchmarked to external information. The acceptance ratio is ", 
          round(length(acc)/n0, 3), "\n")
  if (length(acc) == 0) {
    stop("All posterior samples have been rejected. Please rerun getSmoothed() with a larger 'nsim' argument.")
  }
  for (i in 1:length(fitted$draws.est)) {
    fitted$draws.est[[i]]$draws <- fitted$draws.est[[i]]$draws[acc]
  }
  for (i in 1:length(fitted$draws.est.overall)) {
    fitted$draws.est.overall[[i]]$draws <- fitted$draws.est.overall[[i]]$draws[acc]
  }
  fitted$stratified$variance <- fitted$stratified$median <- fitted$stratified$mean <- fitted$stratified$lower <- fitted$stratified$upper <- NA
  for (i in 1:length(fitted$draws.est)) {
    y <- fitted$draws.est[[i]]$years
    r <- fitted$draws.est[[i]]$region
    s <- fitted$draws.est[[i]]$strata
    j <- which(fitted$stratified$years == y & fitted$stratified$region == 
                 r & fitted$stratified$strata == s)
    fitted$stratified[j, c("lower", "median", "upper")] <- stats::quantile(fitted$draws.est[[j]]$draws, 
                                                                           c(lowerCI, 0.5, upperCI))
    fitted$stratified[j, "mean"] <- mean(fitted$draws.est[[j]]$draws)
    fitted$stratified[j, "variance"] <- var(fitted$draws.est[[j]]$draws)
  }
  fitted$overall$variance <- fitted$overall$median <- fitted$overall$mean <- fitted$overall$lower <- fitted$overall$upper <- NA
  for (i in 1:length(fitted$draws.est.overall)) {
    y <- fitted$draws.est.overall[[i]]$years
    r <- fitted$draws.est.overall[[i]]$region
    j <- which(fitted$overall$years == y & fitted$overall$region == 
                 r)
    fitted$overall[j, c("lower", "median", "upper")] <- stats::quantile(fitted$draws.est.overall[[j]]$draws, 
                                                                        c(lowerCI, 0.5, upperCI))
    fitted$overall[j, "mean"] <- mean(fitted$draws.est.overall[[j]]$draws)
    fitted$overall[j, "variance"] <- var(fitted$draws.est.overall[[j]]$draws)
  }
  fitted$nsim <- length(acc)
  fitted$accept.ratio <- length(acc)/n0
  fitted$benchmarked <- TRUE
  return(fitted)
}
