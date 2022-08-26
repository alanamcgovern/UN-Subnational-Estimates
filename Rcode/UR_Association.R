rm(list = ls())
# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'

# Load libraries -----------------------------------------------
library(INLA)
library(tidyverse)
options(pillar.sigfig=6)

# Load info -----------------------------------------------
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

# Load cluster data and direct estimates  ----------------------------------------------------------
setwd(paste0(data.dir))
load(paste0(country,'_cluster_dat.rda'),
     envir = .GlobalEnv)

setwd(paste0(res.dir))
load(paste0('Direct/U5MR/',country, '_direct_natl_yearly_u5.rda'))
load(paste0('Direct/U5MR/',country, "_res_natl_yearly_u5_SmoothedDirect.rda"))

## Calculate Admin 1 population proportions by region (for aggregation) ------------------------------------------------------

## Admin 1
weight.region.adm1.u5 <- expand.grid(region = sort(unique(mod.dat$admin1.name)), years = beg.year:max(end.years))
weight.region.adm1.u5$proportion <- NA

for (i in 1:nrow(weight.region.adm1.u5)) {
  numerator <- sum(mod.dat$total[mod.dat$admin1.name==weight.region.adm1.u5[i,1] & mod.dat$years==weight.region.adm1.u5[i,2]])
  denominator <- sum(mod.dat$total[mod.dat$years==weight.region.adm1.u5[i,2]])
  weight.region.adm1.u5[i, "proportion"] <- numerator/denominator
}

## Admin 1 x UR
weight.region.adm1.ur.u5 <- expand.grid(region = sort(unique(mod.dat$admin1.name)), strata = c('rural','urban'), years = beg.year:max(end.years))
weight.region.adm1.ur.u5$proportion <- NA
for (i in 1:nrow(weight.region.adm1.ur.u5)) {
  numerator <- sum(mod.dat$total[mod.dat$admin1.name==weight.region.adm1.ur.u5[i,1] & mod.dat$years==weight.region.adm1.ur.u5[i,3] & mod.dat$urban==weight.region.adm1.ur.u5[i,2]])
  denominator <- sum(mod.dat$total[mod.dat$years==weight.region.adm1.ur.u5[i,3]])
  weight.region.adm1.ur.u5[i, "proportion"] <- numerator/denominator
}

# Fit model  ----------------------------------------------------------
## Model w/o urban/rural  ----------------------------------------------------------
mod1 <- inla(formula=Y ~ -1 + admin1.name + f(years,model='iid'), family='betabinomial', data=mod.dat, Ntrials = total,
             control.compute= list(config=T), control.inla = list(strategy = 'adaptive',int.strategy = 'auto'))

# draw samples from posterior
select <- as.list(rep(0,length(unique(mod.dat$admin1.name))+1))
names(select) <- mod1$misc$configs$contents$tag[2:(length(select)+1)]
nsim <- 1000
mod1.samples <- inla.posterior.sample(n=nsim, result=mod1,intern=T, selection=select)

# reformat samples to prepare for aggregation
num.years <- length(unique(mod.dat$years))
num.admin1 <- length(unique(mod.dat$admin1.name))
fields <- sort(unique(mod.dat$admin1.name))
sample.restruct <- matrix(NA,nsim,num.years*num.admin1)
colnames(sample.restruct) <- rep(paste0(' '),ncol(sample.restruct))
for(j in 1:num.admin1){
  for(i in 1:nsim){
    for(k in 1:num.years){
      sample.restruct[i,(j-1)*num.years+k] <- mod1.samples[[i]]$latent[num.years + j,] + mod1.samples[[i]]$latent[k,]
      colnames(sample.restruct)[(j-1)*num.years+k] <- paste0(fields[j],':',k)
    }
  }
}
mod1.draws <- expit(sample.restruct)
mod1.hazards <- 1-(1-mod1.draws)^60

mod1.res <- t(apply(mod1.hazards,2,function(x){
    c(median(x),quantile(x,0.025),quantile(x,0.975))
  }))
colnames(mod1.res)[1] <- 'median'
mod1.res <- as.data.frame(mod1.res)

# aggregate admin-level samples to national
mod1.res$year <- unique(mod.dat$years)
mod1.res <- mod1.res[order(mod1.res$year),]
mod1.res <- cbind(mod1.res,weight.region.adm1.u5)
# double check that region names line up
mod1.res
mod1.agg <- mod1.res %>% group_by(year) %>% summarise(sum(median*proportion))
colnames(mod1.agg) <- c('years','mod1_median')
mod1.agg$years <- beg.year:max(end.years)




## Model w urban intercept  ----------------------------------------------------------
mod2 <- inla(formula=Y ~ -1 + admin1.name + urban+ f(years,model='iid'), family='betabinomial', data=mod.dat, Ntrials = total,
             control.compute= list(config=T), control.inla = list(strategy = 'adaptive',int.strategy = 'auto'))
# draw samples from posterior
select <- as.list(rep(0,length(unique(mod.dat$admin1.name))+2))
names(select) <- mod2$misc$configs$contents$tag[2:(length(select)+1)]
nsim <- 1000
mod2.samples <- inla.posterior.sample(n=nsim, result=mod2,intern=T, selection=select)

# reformat samples to prepare for aggregation
num.years <- length(unique(mod.dat$years))
num.admin1 <- length(unique(mod.dat$admin1.name))
fields <- sort(unique(mod.dat$admin1.name))
sample.restruct <- matrix(NA,nsim,2*num.years*num.admin1)
colnames(sample.restruct) <- rep(paste0(' '),ncol(sample.restruct))
for(j in 1:num.admin1){
  for(i in 1:nsim){
    for(k in 1:num.years){
      sample.restruct[i,(j-1)*num.years+k] <- mod2.samples[[i]]$latent[num.years + j,] + mod2.samples[[i]]$latent[k,]
      sample.restruct[i,num.years*num.admin1+(j-1)*num.years+k] <- mod2.samples[[i]]$latent[num.years + j,] + mod2.samples[[i]]$latent[k,] + tail(mod2.samples[[i]]$latent,1)
      colnames(sample.restruct)[(j-1)*num.years+k] <- paste0(fields[j],':rural:',k)
      colnames(sample.restruct)[num.years*num.admin1+(j-1)*num.years+k] <- paste0(fields[j],':urban:',k)
    }
  }
}
mod2.draws <- expit(sample.restruct)
mod2.hazards <- 1-(1-mod2.draws)^60

mod2.res <- t(apply(mod2.hazards,2,function(x){
  c(median(x),quantile(x,0.025),quantile(x,0.975))
}))
colnames(mod2.res)[1] <- c('median')
mod2.res <- as.data.frame(mod2.res)

# aggregate admin-level samples to national
mod2.res$year <- unique(mod.dat$years)
mod2.res <- mod2.res[order(mod2.res$year),]
mod2.res <- cbind(mod2.res,weight.region.adm1.ur.u5)
# double check that region names and strata line up
mod2.res
mod2.agg <- mod2.res%>% group_by(year) %>% summarise(sum(median*proportion))
colnames(mod2.agg) <- c('years','mod2_median')
mod2.agg$years <- beg.year:max(end.years)

## Model w urban interaction  ----------------------------------------------------------
mod3 <- inla(formula=Y ~ -1 + admin1.name + urban + admin1.name*urban, family='betabinomial', data=mod.dat, Ntrials = total,
             control.compute= list(config=T), control.inla = list(strategy = 'adaptive',int.strategy = 'auto'))

## Comparing Models ----------------------------------------------------------

# F-test for signficance of interecept
pchisq(2*(mod2$mlik[1] - mod1$mlik[1]),length(unique(mod.dat$admin1.name)),lower.tail = F)
# F-test for signficance of intercepts and interactions
pchisq(2*(mod3$mlik[1] - mod1$mlik[1]),length(unique(mod.dat$admin1.name)),lower.tail = F)
# F-test for significance of interactions
pchisq(2*(mod3$mlik[1] - mod2$mlik[1]),length(unique(mod.dat$admin1.name))-1,lower.tail = F)

## U5MR
plot.max <- max(res.natl.yearly.u5$upper+.025, na.rm = T)

plot(NA, xlab = "Year", ylab = "U5MR",
     ylim = c(0, plot.max), xlim = c(beg.year, max(end.years)),
     type = 'l', col = 'black', main = country)
  
lines(res.natl.yearly.u5$years.num, res.natl.yearly.u5$median,
        col = 'black', lwd = 2)
lines(res.natl.yearly.u5$years.num, res.natl.yearly.u5$upper,
        col = 'black', lty = 2)
lines(res.natl.yearly.u5$years.num,res.natl.yearly.u5$lower, 
        col = 'black', lty = 2)
lines(mod1.agg$years, mod1.agg$mod1_median, col='blue',lwd=2)
lines(mod2.agg$years, mod2.agg$mod2_median, col='red',lwd=2)
legend('topright', bty = 'n',
         col = c('black','blue','red'),
         lwd = 2, legend = c("Smoothed",'No urban intercept','Urban intercept'))


