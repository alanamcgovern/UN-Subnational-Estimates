rm(list=ls())

# ENTER COUNTRY OF INTEREST AND FINAL ESTIMATE INFO -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'

## Setup -----------------------------------------------
#### Load libraries and info ----------------------------------------------------------

# Libraries
options(gsubfn.engine = "R")
library(rgdal)
library(Rfast)

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

if(!dir.exists(paste0(res.dir,  '/Figures/Summary'))){
  dir.create(paste0(res.dir, '/Figures/Summary'))}
if(!dir.exists(paste0(res.dir,  '/Figures/Summary/U5MR'))){
  dir.create(paste0(res.dir,  '/Figures/Summary/U5MR'))}
if(!dir.exists(paste0(res.dir,'/Figures/Summary/NMR'))){
  dir.create(paste0(res.dir, '/Figures/Summary/NMR'))}

#### Load model data ----------------------------------------------------------

load(paste0(data.dir, '/', country, '_cluster_dat.rda'), envir = .GlobalEnv)

mod.dat$years <- as.numeric(as.character(mod.dat$years))
dat.years <- sort(unique(mod.dat$years))
mod.dat$strata.orig <- mod.dat$strata
mod.dat$strata <- mod.dat$urban
mod.dat$country <- as.character(country)
survey_years <- unique(mod.dat$survey)

if(max(survey_years)>2018){
  end.proj.year <- 2022
}else{
  end.proj.year <- 2020
}

#### Load polygon files  ------------------------------------------------------
setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm0)) # load the national shape file
# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

if(exists('poly.layer.adm2')){
  poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                       layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions
}

# set coordinate reference system to be equal
if(exists("poly.adm2")){
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0(poly.path,'/', country, '_Amat.rda'))
load(paste0(poly.path,'/', country, '_Amat_Names.rda'))


#### Load National IGME estimates ------------------------------------------------------
setwd(paste0(home.dir,'/Data/IGME'))

## U5MR
igme.ests.u5 <- read.csv(paste0(country.abbrev,'_u5_igme_est.csv'), header = T)
names(igme.ests.u5) <- c('year','OBS_VALUE','LOWER_BOUND','UPPER_BOUND')
igme.ests.u5$year <- igme.ests.u5$year-0.5
igme.ests.u5$OBS_VALUE <- igme.ests.u5$OBS_VALUE/1000
igme.ests.u5 <- igme.ests.u5[igme.ests.u5$year %in% beg.year:end.proj.year,]
igme.ests.u5 <- igme.ests.u5[order(igme.ests.u5$year),]
igme.ests.u5$SD <- (igme.ests.u5$UPPER_BOUND - igme.ests.u5$LOWER_BOUND)/(2*1.645*1000)
igme.ests.u5$LOWER_BOUND <- igme.ests.u5$OBS_VALUE - 1.96*igme.ests.u5$SD
igme.ests.u5$UPPER_BOUND <- igme.ests.u5$OBS_VALUE + 1.96*igme.ests.u5$SD

## NMR
igme.ests.nmr <- read.csv(paste0(country.abbrev,'_nmr_igme_est.csv'),  header = T)
names(igme.ests.nmr) <- c('year','OBS_VALUE','LOWER_BOUND','UPPER_BOUND')
igme.ests.nmr$year <- igme.ests.nmr$year-0.5
igme.ests.nmr$OBS_VALUE <- igme.ests.nmr$OBS_VALUE/1000
igme.ests.nmr <- igme.ests.nmr[igme.ests.nmr$year %in% beg.year:end.proj.year,]
igme.ests.nmr <- igme.ests.nmr[order(igme.ests.nmr$year),]
igme.ests.nmr$SD <- (igme.ests.nmr$UPPER_BOUND - igme.ests.nmr$LOWER_BOUND)/(2*1.645*1000)
igme.ests.nmr$LOWER_BOUND <- igme.ests.nmr$OBS_VALUE - 1.96*igme.ests.nmr$SD
igme.ests.nmr$UPPER_BOUND <- igme.ests.nmr$OBS_VALUE + 1.96*igme.ests.nmr$SD


#### load admin1 and admin2 weights ####
load(paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm1_weights_u5.rda'))
if(exists('poly.adm2')){
  load(paste0(data.dir,'/worldpop/adm2_weights_u1.rda'))
  load(paste0(data.dir,'/worldpop/adm2_weights_u5.rda'))
}

#### Parameters ####

plot.years <- 2000:end.proj.year
n_years <- length(plot.years)

cols <- rainbow(8+1+1+1)
cols <- cols[c(1,3,7,2,4,11,9,5,6,8,10)]

survey.legends <- unique(mod.dat[,c("survey","survey.type")])
survey.legends <- survey.legends[order(survey.legends$survey),]
survey_names <- paste0(survey.legends$survey.type, ' ', survey.legends$survey)

end.year <- max(survey_years)
est.idx <- 1:(end.year-beg.year+1)
pred.idx <- (end.year-beg.year+2):n_years
if(((end.year-beg.year+1) %% 3)==0){
  beg.period.years <- seq(beg.year,end.year,3) 
  end.period.years <- beg.period.years + 2 
}else if(((end.year-beg.year+1) %% 3)==1){
  beg.period.years <- c(beg.year,beg.year+2,seq(beg.year+4,end.year,3))
  end.period.years <- c(beg.year+1,beg.year+3,seq(beg.year+6,end.year,3))
}else if(((end.year-beg.year+1) %% 3)==2){
  beg.period.years <- c(beg.year,seq(beg.year+2,end.year,3))
  end.period.years <- c(beg.year+1,seq(beg.year+4,end.year,3))
}

beg.proj.years <- seq(end.year+1,end.proj.year,3)
end.proj.years <- beg.proj.years+2

pane.years <- c((end.period.years + beg.period.years)/2, (end.proj.years+beg.proj.years)/2)
est.period.idx <- 1:length(beg.period.years)
pred.period.idx <- (length(beg.period.years)+1):(length(beg.period.years)+length(beg.proj.years))

##### function to get posterior draws from BB8 ####

draw_1y_adm<-function(admin_draws, year_num,admin_vec, nsim=1000){
  
  # year_num: year of comparison
  # nsim: number of posterior draws
  # admin_vec: vector of admin index
  # admin_draws: posterior draws (as a list from SUMMER output)
  
  # prepare reference frame for draws 
  # ID corresponds to specific year, region
  draw_ID<-c(1:length(admin_draws))
  draw_year<-vector()
  draw_region<-vector()
  
  for( i in draw_ID){
    tmp_d<-admin_draws[[i]]
    draw_year[i]<-tmp_d$years
    draw_region[i]<-tmp_d$region
  }
  
  draw_ref<-data.frame(id=draw_ID,year=draw_year,
                       region=draw_region)
  
  draw_frame<-matrix( nrow = nsim, ncol = length(admin_vec))
  
  for(i in 1:length(admin_vec)){
    admin_i<-admin_vec[i]
    id_draw_set<-draw_ref[draw_ref$year==year_num&
                            draw_ref$region==admin_i,]$id 
    
    draw_set<-admin_draws[[id_draw_set]]$draws
    
    draw_frame[,i]<-draw_set
    #print(mean(r_frame[,c(admin_i)]))
  }
  
  colnames(draw_frame)<-admin_vec
  
  return(draw_frame)
}

#### prepare national level models ####
setwd(res.dir)

  ### national yearly direct
{
  load(file = paste0('Direct/NMR/', country, '_direct_natl_yearly_nmr.rda'))  
  load(file = paste0('Direct/U5MR/', country, '_direct_natl_yearly_u5.rda')) 
  
  direct.natl.yearly.nmr <- direct.natl.yearly.nmr[!is.na(direct.natl.yearly.nmr$mean),]
  natl.direct.yearly.nmr.draw = expit(Rfast::rmvnorm(10000, mu = direct.natl.yearly.nmr$logit.est,sigma = direct.natl.yearly.nmr$var.est*diag(nrow(direct.natl.yearly.nmr))))
  natl.direct.yearly.nmr = t(apply(natl.direct.yearly.nmr.draw, 2, quantile, probs = c(0.025, 0.5, 0.975)))
  colnames(natl.direct.yearly.nmr) = c("lower_nmr", "median_nmr", "upper_nmr")
  
  direct.natl.yearly.u5 <- direct.natl.yearly.u5[!is.na(direct.natl.yearly.u5$mean),]
  natl.direct.yearly.u5.draw = expit(Rfast::rmvnorm(10000, mu = direct.natl.yearly.u5$logit.est,sigma = direct.natl.yearly.u5$var.est*diag(nrow(direct.natl.yearly.u5))))
  natl.direct.yearly.u5 = t(apply(natl.direct.yearly.u5.draw, 2, quantile, probs = c(0.025, 0.5, 0.975)))
  colnames(natl.direct.yearly.u5) = c("lower_u5", "median_u5", "upper_u5")
  
  natl.direct.yearly.frame <- as.data.frame(cbind(natl.direct.yearly.nmr,natl.direct.yearly.u5))
  natl.direct.yearly.frame$method <- 'natl.direct'
  natl.direct.yearly.frame$years <- direct.natl.yearly.nmr$years
}
  
  ### national yearly smooth direct
{
  load(file = paste0('Direct/NMR/', country, '_res_natl_yearly_nmr_SmoothedDirect.rda'))  
  load(file = paste0('Direct/U5MR/', country, '_res_natl_yearly_u5_SmoothedDirect.rda'))
  
  natl.sd.est.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in% beg.year:end.proj.year, "median"]
  natl.sd.lower.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in% beg.year:end.proj.year, "lower"]
  natl.sd.upper.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in% beg.year:end.proj.year, "upper"]
  natl.sd.year.nmr <- res.natl.yearly.nmr[res.natl.yearly.nmr$years %in% beg.year:end.proj.year, "years"]
  
  natl.sd.est.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% beg.year:end.proj.year, "median"]
  natl.sd.lower.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% beg.year:end.proj.year, "lower"]
  natl.sd.upper.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% beg.year:end.proj.year, "upper"]
  natl.sd.year.u5 <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% beg.year:end.proj.year, "years"]
  
  natl.sd.frame<-data.frame()
  natl.sd.frame<-data.frame(lower_nmr=natl.sd.lower.nmr, median_nmr=natl.sd.est.nmr,upper_nmr=natl.sd.upper.nmr, 
                            lower_u5=natl.sd.lower.u5, median_u5=natl.sd.est.u5, upper_u5=natl.sd.upper.u5, 
                            method='natl.sd.yearly', years=natl.sd.year.u5)

}

  ### national betabinomial models
{
  if(file.exists(paste0('Betabinomial/NMR/', country, '_res_natl_unstrat_nmr.rda'))){
    load(file = paste0('Betabinomial/NMR/', country, '_res_natl_unstrat_nmr.rda'))}
  if(file.exists(paste0('Betabinomial/NMR/', country, '_res_natl_strat_nmr.rda'))){
    load(file = paste0('Betabinomial/NMR/', country, '_res_natl_strat_nmr.rda'))}
  if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_natl_unstrat_u5.rda'))){
    load(file = paste0('Betabinomial/U5MR/', country, '_res_natl_unstrat_u5.rda'))}  
  if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_natl_strat_u5.rda'))){
    load(file = paste0('Betabinomial/U5MR/', country, '_res_natl_strat_u5.rda'))}  
  
  if(exists('bb.res.natl.unstrat.nmr') & exists('bb.res.natl.unstrat.u5')){
    natl.bb.unstrat.frame <- data.frame(lower_nmr = bb.res.natl.unstrat.nmr$overall$lower, median_nmr = bb.res.natl.unstrat.nmr$overall$median, upper_nmr = bb.res.natl.unstrat.nmr$overall$upper,
                                        lower_u5 = bb.res.natl.unstrat.u5$overall$lower, median_u5 = bb.res.natl.unstrat.u5$overall$median, upper_u5 = bb.res.natl.unstrat.u5$overall$upper,
                                        method='natl.bb.unstrat',years=bb.res.natl.unstrat.nmr$overall$years)
  }else if(exists('bb.res.natl.unstrat.nmr')){
    natl.bb.unstrat.frame <- data.frame(lower_nmr = bb.res.natl.unstrat.nmr$overall$lower, median_nmr = bb.res.natl.unstrat.nmr$overall$median, upper_nmr = bb.res.natl.unstrat.nmr$overall$upper,
                                        lower_u5 = NA, median_u5 = NA, upper_u5 = NA,
                                        method='natl.bb.unstrat',years=bb.res.natl.unstrat.nmr$overall$years)
  }else if(exists('bb.res.natl.unstrat.u5')){
    natl.bb.unstrat.frame <- data.frame(lower_nmr =NA, median_nmr = NA, upper_nmr = NA,
                                        lower_u5 = bb.res.natl.unstrat.u5$overall$lower, median_u5 = bb.res.natl.unstrat.u5$overall$median, upper_u5 = bb.res.natl.unstrat.u5$overall$upper,
                                        method='natl.bb.unstrat',years=bb.res.natl.unstrat.u5$overall$years)
  }
  
  if(exists('bb.res.natl.strat.nmr') & exists('bb.res.natl.strat.u5')){
    natl.bb.strat.frame <- data.frame(lower_nmr = bb.res.natl.strat.nmr$overall$lower, median_nmr = bb.res.natl.strat.nmr$overall$median, upper_nmr = bb.res.natl.strat.nmr$overall$upper,
                                        lower_u5 = bb.res.natl.strat.u5$overall$lower, median_u5 = bb.res.natl.strat.u5$overall$median, upper_u5 = bb.res.natl.strat.u5$overall$upper,
                                        method='natl.bb.strat',years=bb.res.natl.strat.nmr$overall$years)
  }else if(exists('bb.res.natl.strat.nmr')){
    natl.bb.strat.frame <- data.frame(lower_nmr = bb.res.natl.strat.nmr$overall$lower, median_nmr = bb.res.natl.strat.nmr$overall$median, upper_nmr = bb.res.natl.strat.nmr$overall$upper,
                                        lower_u5 = NA, median_u5 = NA, upper_u5 = NA,
                                        method='natl.bb.strat',years=bb.res.natl.strat.nmr$overall$years)
  }else if(exists('bb.res.natl.strat.u5')){
    natl.bb.strat.frame <- data.frame(lower_nmr =NA, median_nmr = NA, upper_nmr = NA,
                                        lower_u5 = bb.res.natl.strat.u5$overall$lower, median_u5 = bb.res.natl.strat.u5$overall$median, upper_u5 = bb.res.natl.strat.u5$overall$upper,
                                        method='natl.bb.strat',years=bb.res.natl.strat.u5$overall$years)
  }
}

#### prepare admin1 level models ####

  ### smooth direct admin1 3-year window
{
  load(file = paste0('Direct/NMR/', country, '_res_admin1_nmr_SmoothedDirect.rda'))
  admin1.sd.nmr <- res.admin1.nmr
  load(file = paste0('Direct/U5MR/', country, '_res_admin1_u5_SmoothedDirect.rda'))  
  admin1.sd.u5 <- res.admin1.u5
  
  sd.adm1.to.natl.frame = matrix(NA, nrow = max(pred.period.idx), ncol =  6)
  
  for (i in 1:max(pred.period.idx)){
    year = round((pane.years)[i])
    
    adm1.pop.nmr <- weight.adm1.u1[weight.adm1.u1$years==year,]
    sd.nmr.tmp <- admin1.sd.nmr[admin1.sd.nmr$years.num==sort(unique(admin1.sd.nmr$years.num))[i],]
    sd.nmr.tmp <- merge(sd.nmr.tmp,adm1.pop.nmr,by='region')[,c('region','years.x','lower','median','upper','proportion')]
    sd.nmr.tmp$wt.lower <- sd.nmr.tmp$lower * sd.nmr.tmp$proportion
    sd.nmr.tmp$wt.median <- sd.nmr.tmp$median * sd.nmr.tmp$proportion
    sd.nmr.tmp$wt.upper <- sd.nmr.tmp$upper * sd.nmr.tmp$proportion
    
    adm1.pop.u5 <- weight.adm1.u5[weight.adm1.u5$years==year,]
    sd.u5.tmp <- admin1.sd.u5[admin1.sd.u5$years.num==sort(unique(admin1.sd.nmr$years.num))[i],]
    sd.u5.tmp <- merge(sd.u5.tmp,adm1.pop.u5,by='region')[,c('region','years.x','lower','median','upper','proportion')]
    sd.u5.tmp$wt.lower <- sd.u5.tmp$lower * sd.u5.tmp$proportion
    sd.u5.tmp$wt.median <- sd.u5.tmp$median * sd.u5.tmp$proportion
    sd.u5.tmp$wt.upper <- sd.u5.tmp$upper * sd.u5.tmp$proportion
    
    sd.adm1.to.natl.frame[i, ] = c(sum(sd.nmr.tmp$wt.lower),sum(sd.nmr.tmp$wt.median),sum(sd.nmr.tmp$wt.upper),sum(sd.u5.tmp$wt.lower),sum(sd.u5.tmp$wt.median),sum(sd.u5.tmp$wt.upper))
  }
  
  sd.adm1.to.natl.frame<-as.data.frame(sd.adm1.to.natl.frame)
  colnames(sd.adm1.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
  sd.adm1.to.natl.frame$method <- "aggre.sd.adm1"
  sd.adm1.to.natl.frame$years = pane.years
}
  ### smooth direct admin1 yearly
{
  load(file = paste0('Direct/NMR/', country, '_res_admin1_nmr_SmoothedDirect_yearly.rda'))
  admin1.sd.yearly.nmr <- sd.admin1.yearly.nmr
  load(file = paste0('Direct/U5MR/', country, '_res_admin1_u5_SmoothedDirect_yearly.rda'))  
  admin1.sd.yearly.u5 <- sd.admin1.yearly.u5
  
  sd.adm1.yl.to.natl.frame = matrix(NA, nrow = n_years, ncol =  6)
  
  for (i in 1:n_years){
    year = (beg.year:end.proj.year)[i]
    
    adm1.pop.nmr <- weight.adm1.u1[weight.adm1.u1$years==year,]
    sd.nmr.tmp <- admin1.sd.yearly.nmr[admin1.sd.yearly.nmr$years.num==year,]
    sd.nmr.tmp <- merge(sd.nmr.tmp,adm1.pop.nmr,by='region')[,c('region','years.x','lower','median','upper','proportion')]
    sd.nmr.tmp$wt.lower <- sd.nmr.tmp$lower * sd.nmr.tmp$proportion
    sd.nmr.tmp$wt.median <- sd.nmr.tmp$median * sd.nmr.tmp$proportion
    sd.nmr.tmp$wt.upper <- sd.nmr.tmp$upper * sd.nmr.tmp$proportion
    
    adm1.pop.u5 <- weight.adm1.u5[weight.adm1.u5$years==year,]
    sd.u5.tmp <- admin1.sd.yearly.u5[admin1.sd.yearly.u5$years.num==year,]
    sd.u5.tmp <- merge(sd.u5.tmp,adm1.pop.u5,by='region')[,c('region','years.x','lower','median','upper','proportion')]
    sd.u5.tmp$wt.lower <- sd.u5.tmp$lower * sd.u5.tmp$proportion
    sd.u5.tmp$wt.median <- sd.u5.tmp$median * sd.u5.tmp$proportion
    sd.u5.tmp$wt.upper <- sd.u5.tmp$upper * sd.u5.tmp$proportion
    
    sd.adm1.yl.to.natl.frame[i, ] = c(sum(sd.nmr.tmp$wt.lower),sum(sd.nmr.tmp$wt.median),sum(sd.nmr.tmp$wt.upper),sum(sd.u5.tmp$wt.lower),sum(sd.u5.tmp$wt.median),sum(sd.u5.tmp$wt.upper))
  }
  
  sd.adm1.yl.to.natl.frame<-as.data.frame(sd.adm1.yl.to.natl.frame)
  colnames(sd.adm1.yl.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
  sd.adm1.yl.to.natl.frame$method <- "aggre.sd.yearly.adm1"
  sd.adm1.yl.to.natl.frame$years = beg.year:end.proj.year
}
  ### BB8 admin1 stratified  -- change to benchmarked later
{
  
  if(file.exists(paste0('Betabinomial/NMR/', country, '_res_adm1_strat_nmr.rda'))){
    load(file = paste0('Betabinomial/NMR/', country, '_res_adm1_strat_nmr.rda'))
    res.strat.admin1.nmr <- bb.res.adm1.strat.nmr
    admin1.strat.nmr.BB8<-res.strat.admin1.nmr$overall}
  if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_adm1_strat_u5.rda'))){
    load(file = paste0('Betabinomial/U5MR/', country, '_res_adm1_strat_u5.rda'))
    res.strat.admin1.u5 <- bb.res.adm1.strat.u5
    admin1.strat.u5.BB8<-res.strat.admin1.u5$overall} 
  
  if(exists('admin1.strat.nmr.BB8') | exists('admin1.strat.u5.BB8')){
  
  BB8.adm1.to.natl.frame <- matrix(NA, nrow = n_years, ncol =  6)
  for (i in 1: n_years){
    year = (beg.year:end.proj.year)[i]
    
    if(exists('admin1.strat.nmr.BB8')){
    adm1.pop.nmr <- weight.adm1.u1[weight.adm1.u1$years==year,]
    admin1.strat.nmr.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin1.nmr$draws.est.overall,
                                           year_num=year,
                                           admin_vec=admin1.names$Internal)
    natl.tmp.nmr <- admin1.strat.nmr.BB8.draw %*% adm1.pop.nmr$proportion
    BB8.adm1.to.natl.frame[i, 1:3] = c(quantile(natl.tmp.nmr, probs = c(0.025, 0.5, 0.975)))
    }
    
    if(exists('admin1.strat.u5.BB8')){
    adm1.pop.u5 <- weight.adm1.u5[weight.adm1.u5$years==year,]
    admin1.strat.u5.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin1.u5$draws.est.overall,
                                          year_num=year,
                                          admin_vec=admin1.names$Internal)
    natl.tmp.u5 <- admin1.strat.u5.BB8.draw %*% adm1.pop.u5$proportion
    BB8.adm1.to.natl.frame[i, 4:6] = c(quantile(natl.tmp.u5, probs = c(0.025, 0.5, 0.975)))
    }

  }
  
  BB8.adm1.to.natl.frame<-as.data.frame(BB8.adm1.to.natl.frame)
  colnames(BB8.adm1.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
  BB8.adm1.to.natl.frame$method <- "aggre.adm1.strat.BB8"
  BB8.adm1.to.natl.frame$years = beg.year:end.proj.year
  }
}

#### prepare admin2 level models ####
if(exists('poly.adm2')){
  ### smooth direct admin2 3-year window
  {
    load(file = paste0('Direct/NMR/', country, '_res_admin2_nmr_SmoothedDirect.rda'))
    admin2.sd.nmr <- res.admin2.nmr
    load(file = paste0('Direct/U5MR/', country, '_res_admin2_u5_SmoothedDirect.rda'))  
    admin2.sd.u5 <- res.admin2.u5
    
    sd.adm2.to.natl.frame = matrix(NA, nrow = max(pred.period.idx), ncol =  6)
    
    for (i in 1:max(pred.period.idx)){
      year = round((pane.years)[i])
      
      adm2.pop.nmr <- weight.adm2.u1[weight.adm2.u1$years==year,]
      sd.nmr.tmp <- admin2.sd.nmr[admin2.sd.nmr$years.num==sort(unique(admin2.sd.nmr$years.num))[i],]
      sd.nmr.tmp <- merge(sd.nmr.tmp,adm2.pop.nmr,by='region')[,c('region','years.x','lower','median','upper','proportion')]
      sd.nmr.tmp$wt.lower <- sd.nmr.tmp$lower * sd.nmr.tmp$proportion
      sd.nmr.tmp$wt.median <- sd.nmr.tmp$median * sd.nmr.tmp$proportion
      sd.nmr.tmp$wt.upper <- sd.nmr.tmp$upper * sd.nmr.tmp$proportion
      
      adm2.pop.u5 <- weight.adm2.u5[weight.adm2.u5$years==year,]
      sd.u5.tmp <- admin2.sd.u5[admin2.sd.u5$years.num==sort(unique(admin2.sd.nmr$years.num))[i],]
      sd.u5.tmp <- merge(sd.u5.tmp,adm2.pop.u5,by='region')[,c('region','years.x','lower','median','upper','proportion')]
      sd.u5.tmp$wt.lower <- sd.u5.tmp$lower * sd.u5.tmp$proportion
      sd.u5.tmp$wt.median <- sd.u5.tmp$median * sd.u5.tmp$proportion
      sd.u5.tmp$wt.upper <- sd.u5.tmp$upper * sd.u5.tmp$proportion
      
      sd.adm2.to.natl.frame[i, ] = c(sum(sd.nmr.tmp$wt.lower),sum(sd.nmr.tmp$wt.median),sum(sd.nmr.tmp$wt.upper),sum(sd.u5.tmp$wt.lower),sum(sd.u5.tmp$wt.median),sum(sd.u5.tmp$wt.upper))
    }
    
    sd.adm2.to.natl.frame<-as.data.frame(sd.adm2.to.natl.frame)
    colnames(sd.adm2.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
    sd.adm2.to.natl.frame$method <- "aggre.sd.adm2"
    sd.adm2.to.natl.frame$years = pane.years
  }
  
  ### smooth direct admin2 yearly
  {
    if(file.exists(paste0('Direct/NMR/', country, '_res_admin2_nmr_SmoothedDirect_yearly.rda'))){
      load(file = paste0('Direct/NMR/', country, '_res_admin2_nmr_SmoothedDirect_yearly.rda'))
      admin2.sd.yearly.nmr <- sd.admin2.yearly.nmr}
    if(file.exists(paste0('Direct/U5MR/', country, '_res_admin2_u5_SmoothedDirect_yearly.rda'))){
      load(file = paste0('Direct/U5MR/', country, '_res_admin2_u5_SmoothedDirect_yearly.rda'))  
      admin2.sd.yearly.u5 <- sd.admin2.yearly.u5}
    
    if(exists('admin2.sd.yearly.nmr') | exists('admin2.sd.yearly.u5')){
    sd.adm2.yl.to.natl.frame = matrix(NA, nrow = n_years, ncol =  6)
    
    for (i in 1:n_years){
      year = (beg.year:end.proj.year)[i]
      
      if(exists('admin2.sd.yearly.nmr')){
      adm2.pop.nmr <- weight.adm2.u1[weight.adm2.u1$years==year,]
      sd.nmr.tmp <- admin2.sd.yearly.nmr[admin2.sd.yearly.nmr$years.num==year,]
      sd.nmr.tmp <- merge(sd.nmr.tmp,adm2.pop.nmr,by='region')[,c('region','years.x','lower','median','upper','proportion')]
      sd.nmr.tmp$wt.lower <- sd.nmr.tmp$lower * sd.nmr.tmp$proportion
      sd.nmr.tmp$wt.median <- sd.nmr.tmp$median * sd.nmr.tmp$proportion
      sd.nmr.tmp$wt.upper <- sd.nmr.tmp$upper * sd.nmr.tmp$proportion
      sd.adm2.yl.to.natl.frame[i, 1:3] <- c(sum(sd.nmr.tmp$wt.lower),sum(sd.nmr.tmp$wt.median),sum(sd.nmr.tmp$wt.upper))
      }
      
      if(exists('admin2.sd.yearly.u5')){
      adm2.pop.u5 <- weight.adm2.u5[weight.adm2.u5$years==year,]
      sd.u5.tmp <- admin2.sd.yearly.u5[admin2.sd.yearly.u5$years.num==year,]
      sd.u5.tmp <- merge(sd.u5.tmp,adm2.pop.u5,by='region')[,c('region','years.x','lower','median','upper','proportion')]
      sd.u5.tmp$wt.lower <- sd.u5.tmp$lower * sd.u5.tmp$proportion
      sd.u5.tmp$wt.median <- sd.u5.tmp$median * sd.u5.tmp$proportion
      sd.u5.tmp$wt.upper <- sd.u5.tmp$upper * sd.u5.tmp$proportion
      
      sd.adm2.yl.to.natl.frame[i, 4:6] <- c(sum(sd.u5.tmp$wt.lower),sum(sd.u5.tmp$wt.median),sum(sd.u5.tmp$wt.upper))
      }
    }
    
    sd.adm2.yl.to.natl.frame<-as.data.frame(sd.adm2.yl.to.natl.frame)
    colnames(sd.adm2.yl.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
    sd.adm2.yl.to.natl.frame$method <- "aggre.sd.yearly.adm2"
    sd.adm2.yl.to.natl.frame$years = beg.year:end.proj.year
    }
  }

### BB8 admin2 stratified -- change to benchmarked later
  {
    
    if(file.exists(paste0('Betabinomial/NMR/', country, '_res_adm2_strat_nmr.rda'))){
      load(file = paste0('Betabinomial/NMR/', country, '_res_adm2_strat_nmr.rda'))
      res.strat.admin2.nmr <- bb.res.adm2.strat.nmr
      admin2.strat.nmr.BB8<-res.strat.admin2.nmr$overall}
    if(file.exists(paste0('Betabinomial/U5MR/', country, '_res_adm2_strat_u5.rda'))){
      load(file = paste0('Betabinomial/U5MR/', country, '_res_adm2_strat_u5.rda'))
      res.strat.admin2.u5 <- bb.res.adm2.strat.u5
      admin2.strat.u5.BB8<-res.strat.admin2.u5$overall} 
    
    if(exists('admin2.strat.nmr.BB8') | exists('admin2.strat.u5.BB8')){
      
      BB8.adm2.to.natl.frame <- matrix(NA, nrow = n_years, ncol =  6)
      for (i in 1: n_years){
        year = (beg.year:end.proj.year)[i]
        
        if(exists('admin2.strat.nmr.BB8')){
          adm2.pop.nmr <- weight.adm2.u1[weight.adm2.u1$years==year,]
          admin2.strat.nmr.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin2.nmr$draws.est.overall,
                                                 year_num=year,
                                                 admin_vec=admin2.names$Internal)
          natl.tmp.nmr <- admin2.strat.nmr.BB8.draw %*% adm2.pop.nmr$proportion
          BB8.adm2.to.natl.frame[i, 1:3] = c(quantile(natl.tmp.nmr, probs = c(0.025, 0.5, 0.975)))
        }
        
        if(exists('admin2.strat.u5.BB8')){
          adm2.pop.u5 <- weight.adm2.u5[weight.adm2.u5$years==year,]
          admin2.strat.u5.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin2.u5$draws.est.overall,
                                                year_num=year,
                                                admin_vec=admin2.names$Internal)
          natl.tmp.u5 <- admin2.strat.u5.BB8.draw %*% adm2.pop.u5$proportion
          BB8.adm2.to.natl.frame[i, 4:6] = c(quantile(natl.tmp.u5, probs = c(0.025, 0.5, 0.975)))
        }
        
      }
      
      BB8.adm2.to.natl.frame<-as.data.frame(BB8.adm2.to.natl.frame)
      colnames(BB8.adm2.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
      BB8.adm2.to.natl.frame$method <- "aggre.adm2.strat.BB8"
      BB8.adm2.to.natl.frame$years = beg.year:end.proj.year
    }
  }
}

#### prepare IGME estimates ####
  igme.frame <- as.data.frame(cbind(igme.ests.nmr$LOWER_BOUND,igme.ests.nmr$OBS_VALUE,igme.ests.nmr$UPPER_BOUND,
                                    igme.ests.u5$LOWER_BOUND,igme.ests.u5$OBS_VALUE,igme.ests.u5$UPPER_BOUND))
  colnames(igme.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
  igme.frame$method <- "igme"
  igme.frame$years <- beg.year:2020
  
#### final plot ####
  
  methods <- c("natl.direct.yearly.frame","natl.sd.frame","sd.adm1.to.natl.frame","sd.adm1.yl.to.natl.frame","sd.adm2.to.natl.frame","sd.adm2.yl.to.natl.frame",
               'natl.bb.unstrat.frame','natl.bb.strat.frame',"BB8.adm1.to.natl.frame","BB8.adm2.to.natl.frame","igme.frame")
  methods.include <- which(sapply(methods,exists))
  methods.used <- methods[methods.include]
  
  natl.all <- data.frame()
  for(i in methods.include){
    if(nrow(natl.all)==0){
      natl.all <- eval(str2lang(methods[i]))
    }else{
      natl.all <- rbind(natl.all,eval(str2lang(methods[i])))
    }
  }
  
  natl.all$years.num <- as.numeric(natl.all$years)
  natl.all$is.yearly <- FALSE
  
  natl.to.plot <- natl.all
  # use this argument to only plot certain methods
  natl.to.plot <- natl.all %>% filter(!(method %in% c('natl.direct')))
  
  ##IF you have made a comparison plot before that you don't want to overwrite, make sure to change the name of the PDF!
 pdf(paste0(res.dir, "/Figures/Summary/NMR/",
             country, "_comparison_nmr.pdf"),height = 6,width = 6)
 
  natl.to.plot %>% ggplot(aes(x=as.numeric(years),y=median_nmr*1000,group=method,color=method)) + geom_line() +geom_point(alpha=0.3) + 
    ylab('Median NMR deaths per 1000 live births') + xlab('Year') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_continuous(breaks=beg.year:end.proj.year,labels=beg.year:end.proj.year)
  
  dev.off()
  
  ##IF you have made a comparison plot before that you don't want to overwrite, make sure to change the name of the PDF!
  pdf(paste0(res.dir, "/Figures/Summary/U5MR/",
             country, "_comparison_u5.pdf"),height = 6,width = 6)
  
 natl.to.plot %>% ggplot(aes(x=as.numeric(years),y=median_u5*1000,group=method,color=method)) + geom_line() +geom_point(alpha=0.3) + 
    ylab('Median U5MR deaths per 1000 live births') + xlab('Year') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_continuous(breaks=beg.year:end.proj.year,labels=beg.year:end.proj.year)
  
  dev.off()
  