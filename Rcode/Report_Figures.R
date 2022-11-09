rm(list=ls())

# ENTER COUNTRY OF INTEREST AND FINAL ESTIMATE INFO -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Guinea'
# The smallest admin level estimated to (for most countries this will be admin2)
res_admin_level <- c('admin1','admin2')[2]
# The type of model used for final estimates
res_method <- c('SmoothedDirect','BB8.unstrat','BB8.strat')[1]

## Setup -----------------------------------------------
#### Load libraries and info ----------------------------------------------------------

# Libraries
options(gsubfn.engine = "R")
library(rgdal)
library(Rfast)

library(data.table)
library(survey)
library(classInt)
library(sp)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(raster)
library(maptools)
library(latticeExtra)
library(viridis)
library(xtable)
library(Hmisc)
library(INLA)
library(spdep)
library(scales)
library(rasterVis)
library(plotrix)
library(ggridges)
library(ggplot2)
library(SUMMER)
library(parallel)
library(cartography)
library(rgeos)
library(graphics)


# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

if(!dir.exists(paste0(res.dir, '/Figures/Betabinomial'))){
  dir.create(paste0(res.dir, '/Figures/Betabinomial'))}
if(!dir.exists(paste0(res.dir,  '/Figures/Betabinomial/U5MR'))){
  dir.create(paste0(res.dir, '/Figures/Betabinomial/U5MR'))}
if(!dir.exists(paste0(res.dir, '/Figures/Betabinomial/NMR'))){
  dir.create(paste0(res.dir, '/Figures/Betabinomial/NMR'))}
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


## Load IGME estimates ------------------------------------------------------
{
  setwd(paste0(home.dir,'/Data/IGME'))
  
  ## U5MR
  igme.ests.u5.raw <- read.csv('igme2022_u5.csv')
  igme.ests.u5 <- igme.ests.u5.raw[igme.ests.u5.raw$ISO.Code==gadm.abbrev,]
  igme.ests.u5 <- data.frame(t(igme.ests.u5[,10:ncol(igme.ests.u5)]))
  names(igme.ests.u5) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.u5$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.u5),'X')) - 0.5
  igme.ests.u5 <- igme.ests.u5[igme.ests.u5$year %in% beg.year:end.proj.year,]
  rownames(igme.ests.u5) <- NULL
  igme.ests.u5$OBS_VALUE <- igme.ests.u5$OBS_VALUE/1000
  igme.ests.u5$SD <- (igme.ests.u5$UPPER_BOUND - igme.ests.u5$LOWER_BOUND)/(2*1.645*1000)
  igme.ests.u5$LOWER_BOUND <- igme.ests.u5$OBS_VALUE - 1.96*igme.ests.u5$SD
  igme.ests.u5$UPPER_BOUND <- igme.ests.u5$OBS_VALUE + 1.96*igme.ests.u5$SD
  
  ## NMR
  igme.ests.nmr.raw <- read.csv('igme2022_nmr.csv')
  igme.ests.nmr <- igme.ests.nmr.raw[igme.ests.nmr.raw$iso==gadm.abbrev,]
  igme.ests.nmr <- data.frame(t(igme.ests.nmr[,10:ncol(igme.ests.nmr)]))
  names(igme.ests.nmr) <- c('LOWER_BOUND','OBS_VALUE','UPPER_BOUND')
  igme.ests.nmr$year <-  as.numeric(stringr::str_remove(row.names(igme.ests.nmr),'X')) - 0.5
  igme.ests.nmr <- igme.ests.nmr[igme.ests.nmr$year %in% beg.year:end.proj.year,]
  rownames(igme.ests.nmr) <- NULL
  igme.ests.nmr$OBS_VALUE <- igme.ests.nmr$OBS_VALUE/1000
  igme.ests.nmr$SD <- (igme.ests.nmr$UPPER_BOUND - igme.ests.nmr$LOWER_BOUND)/(2*1.645*1000)
  igme.ests.nmr$LOWER_BOUND <- igme.ests.nmr$OBS_VALUE - 1.96*igme.ests.nmr$SD
  igme.ests.nmr$UPPER_BOUND <- igme.ests.nmr$OBS_VALUE + 1.96*igme.ests.nmr$SD
}

#### Parameters ####
outcome_vt <- c('nmr','u5')

if(res_method=='BB8.strat'){
  strat_vt <- c("strat","unstrat") 
}else{strat_vt <- "unstrat" }

if(res_admin_level=='admin2'){
  admin_vt <- c("adm1","adm2") ## admin level
  level_vt <- c(1,2) ## polygon level
}else if(res_admin_level=='admin1'){
  admin_vt <- c("adm1") ## admin level
  level_vt <- c(1) ## polygon level
}

admin_level_dt <- as.data.table(cbind(Admin = admin_vt, level = level_vt))

year_vt <- 2000:end.proj.year
n_years <- length(year_vt)
plot.years <- year_vt
est.idx <- which(seq(beg.year,   end.proj.year - 5, 5) < max(as.numeric(as.character(mod.dat$years))))

ages <- levels(mod.dat$age)
ages.strata <- c(paste0(ages,':urban'),paste0(ages,':rural'))
age.strata.cols <- rainbow(length(ages.strata))
age.cols[2] <- "orange"

cols <- rainbow(8+1+1+1)
cols <- cols[c(1,3,7,2,4,11,9,5,6,8,10)]

survey.legends <- unique(mod.dat[,c("survey","survey.type")])
survey.legends <- survey.legends[order(survey.legends$survey),]
survey_names <- paste0(survey.legends$survey.type, ' ', survey.legends$survey)

end.year <- max(survey_years)
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

periods <- paste(beg.period.years, end.period.years, sep = "-") # convert the periods into string
beg.proj.years <- seq(end.year+1,end.proj.year,3)
end.proj.years <- beg.proj.years+2
proj.per <- paste(beg.proj.years, end.proj.years, sep = "-") # add the 3-year period to be projected

periods.survey <- periods
## full time period (including projected years)
periods <- c(periods,proj.per)
pane.years <- c((end.period.years + beg.period.years)/2, (end.proj.years+beg.proj.years)/2)

## National figures ####

### Load results 
{
if(doHIVAdj){
  load(paste0(res.dir, '/Direct/U5MR/', country, '_directHIV_natl_yearly_u5.rda'), envir = .GlobalEnv)
}else{
  load(paste0(res.dir, '/Direct/U5MR/', country,  '_direct_natl_yearly_u5.rda'),  envir = .GlobalEnv)
}
load(paste0(res.dir, '/Direct/U5MR/', country,  '_res_natl_yearly_u5_SmoothedDirect.rda'),  envir = .GlobalEnv)
if(res_method=='BB8.unstrat'){
  load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_res_natl_unstrat_u5.rda'), envir = .GlobalEnv)
  bb.res.natl.u5 <- bb.res.natl.unstrat.u5
  load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_temporals_natl_unstrat_u5.rda'), envir = .GlobalEnv)
  load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_fixed_natl_unstrat_u5.rda'), envir = .GlobalEnv)
  load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_hyperpar_natl_unstrat_u5.rda'), envir = .GlobalEnv)
  bb.res.natl.u5$overall[,c("median", "lower", "upper")] <-
    bb.res.natl.u5$overall[,c("median", "lower", "upper")]*1000
}
if(res_method=='BB8.strat'){
  load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_res_natl_strat_u5.rda'), envir = .GlobalEnv)
  bb.res.natl.u5 <- bb.res.natl.strat.u5
  load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_temporals_natl_strat_u5.rda'), envir = .GlobalEnv)
  load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_fixed_natl_strat_u5.rda'), envir = .GlobalEnv)
  load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_hyperpar_natl_strat_u5.rda'), envir = .GlobalEnv)
  bb.res.natl.u5$overall[,c("median", "lower", "upper")] <-
    bb.res.natl.u5$overall[,c("median", "lower", "upper")]*1000
}

## Convert to U5MR (# of children per 1000)
res.natl.yearly.u5[,c("median", "lower", "upper")] <-
  res.natl.yearly.u5[,c("median", "lower", "upper")]*1000
direct.natl.yearly.u5[,c("mean", "upper", "lower")] <-
  direct.natl.yearly.u5[,c("mean", "upper", "lower")]*1000
}
### Spaghetti Plots 

pdf(paste0(res.dir,'/Figures/Summary/U5MR/', country, '_natl_u5_spaghetti.pdf'), height = 8, width = 8)
{
  tmp.area <- res.natl.yearly.u5[res.natl.yearly.u5$years %in% plot.years,]
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  
  if(res_method=='SmoothedDirect'){
    par(mfrow = c(3,1), lend=1)
  }else{par(mfrow = c(2,2), lend=1)}
  
  
  ## Set plot dimensions y-axis
  if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
    plot.max <- max(direct.natl.yearly.u5$mean + 25, na.rm = T)
  }else{
    plot.max <- 25
  }
  
  ### Plot 1: Estimates ####
  #### Plot Direct estimates
  for(survey in survey_years){
    
    tmp <- direct.natl.yearly.u5[direct.natl.yearly.u5$surveyYears == survey,]
    svy.idx <- match(survey, survey_years) 
    
    if(svy.idx== 1){
      if(dim(tmp)[1] != 0){
        plot(NA,
             xlab = "Year",
             ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.proj.year),
             main = "")
        
        lines(plot.years,
              tmp$mean[1:length(plot.years)], 
              cex = tmp$cex2,
              col = cols[svy.idx])
        
        points(plot.years,
               tmp$mean[1:length(plot.years)], 
               pch = 19,
               col = alpha(cols[svy.idx], 0.35),
               cex = tmp$cex2)
        
      }else{
        plot(NA,
             xlab = "Year",
             ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.proj.year),
             main =  country)
      }
    }else{
      if(dim(tmp)[1] != 0){
        lines(plot.years,
              tmp$mean[1:length(plot.years)], 
              cex = tmp$cex2,
              col = cols[svy.idx])
        points(plot.years,
               tmp$mean[1:length(plot.years)], 
               pch = 19,
               col = alpha(cols[svy.idx], 0.35),
               cex = tmp$cex2)
      } 
    }
  }
  
  #### Add IGME
  igme.years <- (igme.ests.u5$year)
  lines(igme.years, igme.ests.u5$OBS_VALUE*1000,
        lty = 1, lwd = 2, col = cols[10])
  
  ## Add Smooth Direct
  lines(plot.years,  tmp.area$median,  col = cols[11], lwd = 2)
  
  ## Add Betabinomial
  if(!(res_method=='SmoothedDirect')){
    lines(plot.years, bb.res.natl.u5$overall$median,  col = 'black', lwd = 2)
  }
  
  ### Plot 2: Direct Uncertainty ####
  #### Direct Estimates
  for(survey in survey_years){
    tmp <- direct.natl.yearly.u5[direct.natl.yearly.u5$surveyYears == survey,]
    tmp.est.idx <- which(!is.na(tmp$mean))
    svy.idx <- match(survey, survey_years) 
    
    if(svy.idx == 1){
      if(dim(tmp)[1] != 0){
        plot(NA,
             xlab = "Year", 
             ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.proj.year),
             main = "")
        
        polygon(x = c(plot.years[tmp.est.idx], rev(plot.years[tmp.est.idx])),
                y = c(tmp$upper[tmp.est.idx], rev(tmp$lower[tmp.est.idx])),
                col = alpha(cols[svy.idx], 0.25), border = FALSE)
        
      }else{
        plot(NA,
             xlab = "Year", 
             ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.proj.year),
             main = country)
      }
    }else{
      polygon(x = c(plot.years[tmp.est.idx],  rev(plot.years[tmp.est.idx])),
              y = c(tmp$upper[tmp.est.idx], rev(tmp$lower[tmp.est.idx])),
              col = alpha(cols[svy.idx], 0.25), border = FALSE)
    }
  }  
  
  if(res_method=='SmoothedDirect'){
    polygon(x = c(plot.years, rev(plot.years)),
            y = c(tmp.area$upper, rev(tmp.area$lower)),
            col = alpha(cols[11], 0.25), border = FALSE)
  }else{
  #### Add Betabinomial 
  polygon(x = c(bb.res.natl.u5$overall$years.num, rev(bb.res.natl.u5$overall$years.num)),
          y = c(bb.res.natl.u5$overall$upper, rev(bb.res.natl.u5$overall$lower)),
          col = alpha('black', 0.25), border = FALSE)
  }
  ### Plot 3: Smoothed Uncertainty ####  
  
  if(!(res_method=='SmoothedDirect')){
  plot(NA,
       xlab = "Year",
       ylab = "U5MR",
       ylim = c(0, plot.max),
       xlim = c(beg.year, end.proj.year),
       main = "")
  
  #### Add IGME
  igme.years <- (igme.ests.u5$year)
  polygon(x = c(igme.years, rev(igme.years)),
          y = c(igme.ests.u5$UPPER_BOUND*1000,
                rev(igme.ests.u5$LOWER_BOUND*1000)),
          col = alpha(cols[10], 0.25), border = FALSE)
  
  #### Add Smoothed direct
  polygon(x = c(plot.years, rev(plot.years)),
          y = c(res.natl.yearly.u5$upper[1:length(plot.years)], rev(res.natl.yearly.u5$lower[1:length(plot.years)])),
          col = alpha(cols[11], 0.25), border = FALSE)
  
  #### Add Betabinomial
  polygon(x = c(plot.years, rev(plot.years)),
          y = c(bb.res.natl.u5$overall$upper, rev(bb.res.natl.u5$overall$lower)),
          col = alpha('black', 0.25), border = FALSE)
  lines(bb.res.natl.u5$overall$years.num,bb.res.natl.u5$overall$median, 
        col = 'black', lwd = 2, lty = 1)
  
  ### Legend ####
  plot(NA,  xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
  legend('center', bty = 'n',
         pch = c(rep(19, length(survey_years)), rep(NA, 4)),
         lty = c(rep(1, length(survey_years)),  rep(NA, 4)),
         col = c(alpha(cols[1:length(survey_years)], .25), rep(NA, 4)),
         fill = c(rep(NA, length(survey_years)), alpha(c(cols[(10:11)], 'black'), .25)),
         border = c(rep(NA, length(survey_years)), cols[10:11], 'black'), cex = 1,
         legend = c(survey_names,  'IGME', 'Smoothed Direct', 'Betabinomial'))
  }else{
    plot(NA,  xlim = c(0,1), ylim = c(0,1), axes = F, xlab = "", ylab = "")
    legend('center', bty = 'n',
           pch = c(rep(19, length(survey_years)), rep(NA, 4)),
           lty = c(rep(1, length(survey_years)),  rep(NA, 4)),
           col = c(alpha(cols[1:length(survey_years)], .25), rep(NA, 4)),
           fill = c(rep(NA, length(survey_years)), alpha(c(cols[(10:11)]), .25)),
           border = c(rep(NA, length(survey_years)), cols[10:11]), cex = 1,
           legend = c(survey_names,  'IGME', 'Smoothed Direct'))
  }
}
dev.off() 

### Hazards over time

pdf(paste0(res.dir, '/Figures/Summary/U5MR/trend/', country, '_natl_u5_temporal.pdf'), 
    height = 5, width = 7.5)
{
  if(!(res_method=='SmoothedDirect')){
    if(res_method=='BB8.strat'){
      temporals <- bb.temporals.natl.strat.u5
      fixed <- bb.fixed.natl.strat.u5}
    if(res_method=='BB8.unstrat'){
      temporals <- bb.temporals.natl.unstrat.u5
      fixed <- bb.fixed.natl.unstrat.u5}
    
  for(age.strata in ages.strata){
      age <- ages[sapply(ages,grepl,age.strata)]
      age.idx <- which(age==ages)
      temporal.idx <- grepl(paste0(age.strata), temporals$group)
      fixed.idx <- grepl(paste0(age.strata),  row.names(fixed))|grepl(paste0('group',age.idx),  row.names(fixed))
      fixed.eff <- sum(fixed$`0.5quant`[fixed.idx])
      tmp <- data.frame(temporals[temporal.idx,] %>% group_by(years.num) %>% summarise(sum(median)))
      colnames(tmp) <- c('years.num','median')
      
      if(match(age.strata, ages.strata) == 1){
        plot.min <- min(outer(temporals$median, fixed$`0.5quant`,
                              FUN="+")) - 0.25
        plot.max <- max(outer(temporals$median, 
                              fixed$`0.5quant`, 
                              FUN="+")) + 0.25
        
        par(mfrow =c(1,1),
            lend=1)
        plot(NA,
             xlim = c(beg.year, end.proj.year),
             ylim = c(plot.min, plot.max),
             yaxt = 'n', xlab = "Year",
             ylab = "Monthly hazard",  main = "")
        labs <- round(expit(seq(plot.min, plot.max, 1))*1000,1)
        axis(2, at = seq(plot.min, plot.max, 1),
             labels = labs)
      }
      
      lines(beg.year:end.proj.year, tmp$median + fixed.eff,
            col = age.strata.cols[match(age.strata, ages.strata)],  lwd = 2) 
  }
  
  legend('topleft',  cex = 0.65, lwd = 2,
         col = c(age.strata.cols), legend = c(ages.strata), bty = 'n')
    }

dev.off()

}

## Admin1 figures ####
### Load results 
{
if(doHIVAdj){
  load(paste0(res.dir, '/Direct/U5MR/', country, '_directHIV_admin1_u5.rda'), envir = .GlobalEnv)
}else{
  load(paste0(res.dir, '/Direct/U5MR/', country,  '_direct_admin1_u5.rda'),  envir = .GlobalEnv)
}
load(paste0(res.dir, '/Direct/U5MR/', country,  '_res_admin1_u5_SmoothedDirect.rda'),  envir = .GlobalEnv)
load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_res_adm1_strat_u5.rda'), envir = .GlobalEnv)

load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_temporals_natl_strat_u5.rda'), envir = .GlobalEnv)
load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_fixed_natl_strat_u5.rda'), envir = .GlobalEnv)
load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_hyperpar_natl_strat_u5.rda'), envir = .GlobalEnv)

## Convert to U5MR (# of children per 1000)
res.admin1.u5$results[,c("median", "lower", "upper")] <-
  res.admin1.u5$results[,c("median", "lower", "upper")]*1000
res.admin1 <- res.admin1.u5
direct.admin1.u5[,c("mean", "upper", "lower")] <-
  direct.admin1.u5[,c("mean", "upper", "lower")]*1000
direct.admin1 <- direct.admin1.u5
bb.res.adm1.strat.u5$overall[,c("median", "lower", "upper")] <- 
  bb.res.adm1.strat.u5$overall[,c("median", "lower", "upper")]*1000
bb.admin1 <- bb.res.adm1.strat.u5

periods.strings <- lapply(str_split(direct.admin1$years,'-'),as.numeric)
direct.admin1$years.num <- unlist(lapply(periods.strings,function(x){(x[2]/2 + x[1]/2)}))
plot.period.direct.years <- sort(unique(direct.admin1$years.num))
periods.strings <- lapply(str_split(res.admin1$results$years,'-'),as.numeric)
res.admin1$results$years.num <- unlist(lapply(periods.strings,function(x){(x[2]/2 + x[1]/2)}))
plot.period.years <- sort(unique(direct.admin1$years.num))
plot.period.sd.years <- sort(unique(res.admin1$results$years.num))
}
### Spaghetti Uncertainty Plots (9 per)

pdf(paste0(res.dir,'/Figures/Summary/U5MR/',
           country, '_admin1_strat_u5_9per.pdf'), height = 9, width = 6)
{
  par(mfrow = c(3,3), lend=1)
  area.idx <- 0
  for(area in admin1.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin1$results[res.admin1$results$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    if(dim(tmp.area)[1] != 0 &  !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper, direct.admin1$upper[direct.admin1$region ==  as.character(area)]),  na.rm = T) + 25
    }else{
      plot.max <- 25
    }
    ### Plot 1: Estimates ####
    
    if (nrow(tmp.area) > 0 &sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA, xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year + 1),
           main = admin1.names$GADM[area.idx])
      legend('topright', bty = 'n',
             col = c(cols[1:length(survey_years)], 'black'),
             lwd = 2, legend = c(survey_names,  'Smoothed'), cex = 0.6)
    } else {
      
      #### Add Direct ####
      for(survey in survey_years){
        tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                               direct.admin1$region ==  as.character(admin1.names$Internal[area.idx]),]
        tmp.est.idx <- which(!is.na(tmp$mean))
        svy.idx <- match(survey, survey_years) 
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year),
                 main = admin1.names$GADM[area.idx])
            
            lines(plot.period.direct.years, tmp$mean, cex = tmp$cex2,
                  col = cols[svy.idx], lwd = 2)
            
            points(plot.period.direct.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.proj.year),
                 main =  admin1.names$GADM[area.idx])
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(plot.period.direct.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            
            points(plot.period.direct.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
      }
      
      #### Add Smoothed ####
      lines(tmp.area$years.num, 
            tmp.area$median,
            col = cols[length(survey_years)+1],
            lwd = 2)
      
      #### Add Betanomial ####
      tmp.bb <- bb.admin1$overall[bb.admin1$overall$region==area,]
      lines(tmp.bb$years.num,tmp.bb$median, col='black',lwd=2)
      
      #### Legend #####
      legend('topright', bty = 'n',
             col = c(cols[1:(length(survey_years)+1)], 'black'),
             lwd = 2, legend = c(survey_names, 'Smoothed','Betabinomial'), cex = 0.6)
    }
    
    ### Plot 2: Direct Uncertainty ####
    
    #### Add Direct ####
    for(survey in survey_years){
      tmp <- direct.admin1[direct.admin1$surveyYears == survey & direct.admin1$region ==  as.character(admin1.names$Internal[area.idx]),]
      tmp.est.idx <- which(!is.na(tmp$mean))
      svy.idx <- match(survey, survey_years) 
      
      if(svy.idx== 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year),
               main = admin1.names$GADM[area.idx])
          
          polygon(x = c(plot.period.direct.years[tmp.est.idx],
                        rev(plot.period.direct.years[tmp.est.idx])),
                  y = c(tmp$upper[tmp.est.idx],
                        rev(tmp$lower[tmp.est.idx])),
                  col = alpha(cols[svy.idx], 0.25), 
                  border = FALSE)
        }else{
          plot(NA,
               xlab = "Year",
               ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.proj.year),
               main =  admin1.names$GADM[area.idx])
        }
      }else{
        if(dim(tmp)[1] != 0){
          polygon(x = c(plot.period.direct.years[tmp.est.idx],
                        rev(plot.period.direct.years[tmp.est.idx])),
                  y = c(tmp$upper[tmp.est.idx],
                        rev(tmp$lower[tmp.est.idx])),
                  col = alpha(cols[svy.idx], 0.25), 
                  border = FALSE)
        } 
      }
    }
    
    #### Add Betabinomial ####
    polygon(x = c(plot.years, rev(plot.years)),
            y = c(tmp.bb$upper,  rev(tmp.bb$lower)),
            col = alpha('grey7', 0.25),   border = FALSE)
    
    #### Legend #####
    legend('topright', bty = 'n',
           col = c(cols[1:(length(survey_years))], 'grey7'),
           lwd = 2, legend = c(survey_names, 'Betabinomial'), cex = 0.6)
    
    ### Plot 3: Smoothed Uncertainty ####
    
    plot(NA, xlab = "Year", ylab = "U5MR",
         ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year),
         main = admin1.names$GADM[area.idx])
    
    #### Add Smoothed ####
    polygon(x = c(plot.period.sd.years, rev(plot.period.sd.years)),
            y = c(tmp.area$upper, rev(tmp.area$lower)),
            col = alpha(cols[length(survey_years)+1], 0.25), border = FALSE)
    
    #### Add Betabinomial ####
    polygon(x = c(plot.years, rev(plot.years)),
            y = c(tmp.bb$upper,  rev(tmp.bb$lower)),
            col = alpha('grey7', 0.25),   border = FALSE)
    
    #### Legend #####
    legend('topright', bty = 'n',
           col = c(cols[length(survey_years)+1], 'grey7'),
           lwd = 2, legend = c('Smoothed', 'Betabinomial'), cex = 0.6)
  }
}
dev.off()

## Admin2 figures ####
### Load results 
{
if(doHIVAdj){
  load(paste0(res.dir, '/Direct/U5MR/', country, '_directHIV_admin2_u5.rda'), envir = .GlobalEnv)
}else{
  load(paste0(res.dir, '/Direct/U5MR/', country,  '_direct_admin2_u5.rda'),  envir = .GlobalEnv)
}
load(paste0(res.dir, '/Direct/U5MR/', country,  '_res_admin2_u5_SmoothedDirect.rda'),  envir = .GlobalEnv)
load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_res_adm2_strat_u5.rda'), envir = .GlobalEnv)

load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_temporals_natl_strat_u5.rda'), envir = .GlobalEnv)
load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_fixed_natl_strat_u5.rda'), envir = .GlobalEnv)
load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_hyperpar_natl_strat_u5.rda'), envir = .GlobalEnv)

## Convert to U5MR (# of children per 1000)
res.admin2.u5$results[,c("median", "lower", "upper")] <-
  res.admin2.u5$results[,c("median", "lower", "upper")]*1000
res.admin2 <- res.admin2.u5
direct.admin2.u5[,c("mean", "upper", "lower")] <-
  direct.admin2.u5[,c("mean", "upper", "lower")]*1000
direct.admin2 <- direct.admin2.u5
bb.res.adm2.strat.u5$overall[,c("median", "lower", "upper")] <- 
  bb.res.adm2.strat.u5$overall[,c("median", "lower", "upper")]*1000
bb.admin2 <- bb.res.adm2.strat.u5

periods.strings <- lapply(str_split(direct.admin2$years,'-'),as.numeric)
direct.admin2$years.num <- unlist(lapply(periods.strings,function(x){(x[2]/2 + x[1]/2)}))
plot.period.direct.years <- sort(unique(direct.admin2$years.num))
periods.strings <- lapply(str_split(res.admin2$results$years,'-'),as.numeric)
res.admin2$results$years.num <- unlist(lapply(periods.strings,function(x){(x[2]/2 + x[1]/2)}))
plot.period.years <- sort(unique(direct.admin2$years.num))
plot.period.sd.years <- sort(unique(res.admin2$results$years.num))
}
### Spaghetti Uncertainty Plots (9 per)

pdf(paste0(res.dir,'/Figures/Summary/U5MR/',
           country, '_admin2_strat_u5_9per.pdf'), height = 9, width = 6)
{
  par(mfrow = c(3,3), lend=1)
  area.idx <- 0
  for(area in admin2.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin2$results[res.admin2$results$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    if(dim(tmp.area)[1] != 0 &  !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper, direct.admin1$upper[direct.admin2$region ==  as.character(area)]),  na.rm = T) + 25
    }else{
      plot.max <- 25
    }
    ### Plot 1: Estimates ####
    
    if (nrow(tmp.area) > 0 &sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA, xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year + 1),
           main = admin2.names$GADM[area.idx])
      legend('topright', bty = 'n',
             col = c(cols[1:length(survey_years)], 'black'),
             lwd = 2, legend = c(survey_names,  'Smoothed'), cex = 0.6)
    } else {
      
      #### Add Direct ####
      for(survey in survey_years){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region ==  as.character(admin2.names$Internal[area.idx]),]
        tmp.est.idx <- which(!is.na(tmp$mean))
        svy.idx <- match(survey, survey_years) 
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year),
                 main = admin2.names$GADM[area.idx])
            
            lines(plot.period.direct.years, tmp$mean, cex = tmp$cex2,
                  col = cols[svy.idx], lwd = 2)
            
            points(plot.period.direct.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          }else{
            plot(NA,
                 xlab = "Year",
                 ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.proj.year),
                 main =  admin2.names$GADM[area.idx])
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(plot.period.direct.years, tmp$mean,
                  cex = tmp$cex2,
                  col = cols[svy.idx],
                  lwd = 2)
            
            points(plot.period.direct.years, tmp$mean,
                   pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
      }
      
      #### Add Smoothed ####
      lines(tmp.area$years.num, 
            tmp.area$median,
            col = cols[length(survey_years)+1],
            lwd = 2)
      
      #### Add Betanomial ####
      tmp.bb <- bb.admin2$overall[bb.admin2$overall$region==area,]
      lines(tmp.bb$years.num,tmp.bb$median, col='black',lwd=2)
      
      #### Legend #####
      legend('topright', bty = 'n',
             col = c(cols[1:(length(survey_years)+1)], 'black'),
             lwd = 2, legend = c(survey_names, 'Smoothed','Betabinomial'), cex = 0.6)
    }
    
    ### Plot 2: Direct Uncertainty ####
    
    #### Add Direct ####
    for(survey in survey_years){
      tmp <- direct.admin2[direct.admin2$surveyYears == survey & direct.admin2$region ==  as.character(admin2.names$Internal[area.idx]),]
      tmp.est.idx <- which(!is.na(tmp$mean))
      svy.idx <- match(survey, survey_years) 
      
      if(svy.idx== 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year),
               main = admin2.names$GADM[area.idx])
          
          polygon(x = c(plot.period.direct.years[tmp.est.idx],
                        rev(plot.period.direct.years[tmp.est.idx])),
                  y = c(tmp$upper[tmp.est.idx],
                        rev(tmp$lower[tmp.est.idx])),
                  col = alpha(cols[svy.idx], 0.25), 
                  border = FALSE)
        }else{
          plot(NA,
               xlab = "Year",
               ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.proj.year),
               main =  admin2.names$GADM[area.idx])
        }
      }else{
        if(dim(tmp)[1] != 0){
          polygon(x = c(plot.period.direct.years[tmp.est.idx],
                        rev(plot.period.direct.years[tmp.est.idx])),
                  y = c(tmp$upper[tmp.est.idx],
                        rev(tmp$lower[tmp.est.idx])),
                  col = alpha(cols[svy.idx], 0.25), 
                  border = FALSE)
        } 
      }
    }
    
    #### Add Betabinomial ####
    polygon(x = c(plot.years, rev(plot.years)),
            y = c(tmp.bb$upper,  rev(tmp.bb$lower)),
            col = alpha('grey7', 0.25),   border = FALSE)
    
    #### Legend #####
    legend('topright', bty = 'n',
           col = c(cols[1:(length(survey_years))], 'grey7'),
           lwd = 2, legend = c(survey_names, 'Betabinomial'), cex = 0.6)
    
    ### Plot 3: Smoothed Uncertainty ####
    
    plot(NA, xlab = "Year", ylab = "U5MR",
         ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year),
         main = admin2.names$GADM[area.idx])
    
    #### Add Smoothed ####
    polygon(x = c(plot.period.sd.years, rev(plot.period.sd.years)),
            y = c(tmp.area$upper, rev(tmp.area$lower)),
            col = alpha(cols[length(survey_years)+1], 0.25), border = FALSE)
    
    #### Add Betabinomial ####
    polygon(x = c(plot.years, rev(plot.years)),
            y = c(tmp.bb$upper,  rev(tmp.bb$lower)),
            col = alpha('grey7', 0.25),   border = FALSE)
    
    #### Legend #####
    legend('topright', bty = 'n',
           col = c(cols[length(survey_years)+1], 'grey7'),
           lwd = 2, legend = c('Smoothed', 'Betabinomial'), cex = 0.6)
  }
}
dev.off()

## Organize posterior samples ####
n_samp <- 1000

setwd(paste0(res.dir,'/Betabinomial'))
for(outcome in outcome_vt){
  for(admin in admin_vt){
    for(strat in strat_vt){
        
        load(paste0(c('NMR','U5MR')[outcome==outcome_vt],'/',
                    country, "_res_",
                    admin, "_",strat,'_',outcome, ".rda"))
        
          res_obj <- get(paste0("bb.res.", admin,'.',strat,'.',outcome))
        
        draws_list <- res_obj$draws.est.overall
        n_admin <- length(draws_list)/n_years
        
        postsamp_mt_list <- vector(mode = "list", length = n_years)
        
        for (i in 1:n_years){
          # i <- 1
          postsamp_mt_list[[i]]$years <- years_vt[i]
          postsamp_mt <- matrix(0, nrow = n_admin, ncol = n_samp)
          
          for(j in 1:n_admin){
            # j <- 1
            postsamp_mt[j, ] <- draws_list[[n_years*(j-1)+i]]$draws
          }
          
          postsamp_mt_list[[i]]$postsamp_mt <- postsamp_mt
        }
        
        load(paste0(c('NMR','U5MR')[outcome==outcome_vt],'/',
                    country, "_res_",
                    admin, "_",strat,'_',outcome, ".rda"))
        
        save(postsamp_mt_list, 
             file = paste0(c('NMR','U5MR')[outcome==outcome_vt],'/', country, "_",
                          admin,"_", strat,'_',outcome, "_postsamp.RData"))
        
      }
    }
}  

## Trend plots ------------------------------------------------------
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/trend'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/U5MR/trend'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/trend'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/NMR/trend'))}

for(outcome in outcome_vt){
  for(strat in strat_vt){
    load(paste0(res.dir,'/Betabinomial/',c('NMR','U5MR')[outcome==outcome_vt],'/',
                country, "_res_adm1_",strat,'_',outcome, ".rda"))
    
    res_obj <- get(paste0("bb.res.adm1.",strat,'.',outcome))
    res_merged <- merge(res_obj$overall, admin1.names, 
                        by.x=c("region"),
                        by.y=c("Internal"))
    res_merged$region <- res_merged$GADM
    res_merged$width <- res_merged$upper - res_merged$lower
    class(res_merged) <- class(res_obj$overall)
    
    cols <- rainbow(nrow(admin1.names))
    
    pdf(paste0(res.dir, "/Figures/Betabinomial/",paste0(c('NMR','U5MR')[outcome==outcome_vt]),"/trend/",
               country, "_adm1_", strat,"_",outcome, ".pdf"),height = 6,width = 6)
    
    par(mfrow=c(1,1),lend=1)
    plot.max <- max(res_merged$upper+.025, na.rm = T)
    
    plot(NA, xlab = "Year", ylab = paste0(c('NMR','U5MR')[outcome==outcome_vt]),
         ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year), main = paste0(country,' - Admin 1 Regions'))
    legend('topright', bty = 'n',
           col = cols, lwd = 2,  legend = admin1.names$GADM)
    
    for(area in 1:dim(poly.adm1)[1]){
      tmp.area <- res_merged[res_merged$region == as.character(admin1.names$GADM[area]),]
      tmp.area$width <- tmp.area$upper - tmp.area$lower
      tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
      tmp.area$cex2[tmp.area$cex2 > 6] <- 6
      
      res.area <- res_merged[res_merged$region == as.character(admin1.names$GADM[area]),]
      
      lines(res.area$years.num,res.area$median,
            col = cols[area], lwd = 2)
    }  
    
    dev.off()
    
    load(paste0(res.dir,'/Betabinomial/',c('NMR','U5MR')[outcome==outcome_vt],'/',
                country, "_res_adm2_",strat,'_',outcome, ".rda"))
    
    res_obj <- get(paste0("bb.res.adm2.",strat,'.',outcome))
    res_merged <- merge(res_obj$overall, admin2.names, 
                        by.x=c("region"),
                        by.y=c("Internal"))
    res_merged$region <- res_merged$GADM
    res_merged$width <- res_merged$upper - res_merged$lower
    class(res_merged) <- class(res_obj$overall)
    
    cols <- rainbow(nrow(admin2.names))
    
    pdf(paste0(res.dir, "/Figures/Betabinomial/",paste0(c('NMR','U5MR')[outcome==outcome_vt]),"/trend/",
               country, "_adm2_", strat,"_",outcome, ".pdf"),height = 6,width = 6)
    
    par(mfrow=c(1,1),lend=1)
    plot.max <- max(res_merged$upper+.025, na.rm = T)
    
    plot(NA, xlab = "Year", ylab = paste0(c('NMR','U5MR')[outcome==outcome_vt]),
         ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year), main = paste0(country,' - Admin 2 Regions')) + theme(legend.position = 'none')
    
    for(area in 1:dim(poly.adm2)[1]){
      tmp.area <- res_merged[res_merged$region == as.character(admin2.names$GADM[area]),]
      tmp.area$width <- tmp.area$upper - tmp.area$lower
      tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
      tmp.area$cex2[tmp.area$cex2 > 6] <- 6
      
      res.area <- res_merged[res_merged$region == as.character(admin2.names$GADM[area]),]
      
      lines(res.area$years.num,res.area$median,
            col = cols[area], lwd = 2)
    }  
    
    dev.off()
    
  }
}



## Map plots ####
if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/U5MR/map'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/map'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/map'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/NMR/map'))}
admin <- res_admin_level
    
    map_shp <- c(poly.adm1,poly.adm2)[res_admin_level==c('admin1','admin2')]
    admin_name_dt <- as.data.table(get(paste0(res_admin_level, ".names")))
    
    for(outcome in outcome_vt){    
      for(strat in strat_vt){
        
        load(paste0(res.dir, "/Betabinomial/", c('NMR','U5MR')[outcome==outcome_vt], "/",
                    country,'_', res_admin_level, "_",
                    strat,'_', outcome, "_postsamp.RData"))
        
        data_plot_dt <- NULL
        
        for(year in year_vt){
          
          postsamp_mt <- postsamp_mt_list[[which(year==year_vt)]]$postsamp_mt
          
          # create plotting area names (just admin 1 name if admin = 1, or 'admin2,\n admin1' if admin = 2)
          if (res_admin_level == "admin1") {
            admin_name_dt$nameToPlot <- eval(str2lang(poly.label.adm1))
          } else if (res_admin_level == "admin2") {
            admin_name_dt$nameToPlot <- paste0(eval(str2lang(poly.label.adm2)),
                                               #need to change if using alternative GADM files
                                               ",\n", poly.adm2@data$NAME_1)
          }
          
          # create data to plot
          data_plot_dt_year <- data.table(Year = year, 
                                          Internal = admin_name_dt[, Internal],
                                          GADM = admin_name_dt[, GADM],
                                          nameToPlot = admin_name_dt[, nameToPlot],
                                          U5MR_mean = apply(postsamp_mt, 1, mean, na.rm = T),
                                          U5MR_median = apply(postsamp_mt, 1, median, na.rm = T),
                                          U5MR_sd = apply(postsamp_mt, 1, sd, na.rm = T),
                                          U5MR_low95 = apply(postsamp_mt, 1, quantile, 0.025, na.rm = T),
                                          U5MR_up95 = apply(postsamp_mt, 1, quantile, 0.975, na.rm = T),
                                          U5MR_low90 = apply(postsamp_mt, 1, quantile, 0.05, na.rm = T),
                                          U5MR_up90 = apply(postsamp_mt, 1, quantile, 0.95, na.rm = T),
                                          U5MR_low80 = apply(postsamp_mt, 1, quantile, 0.1, na.rm = T),
                                          U5MR_up80 = apply(postsamp_mt, 1, quantile, 0.9, na.rm = T))
          
          data_plot_dt <- rbind(data_plot_dt, data_plot_dt_year)
        }
        
        data_plot_dt[, "U5MR_wid95" := U5MR_up95 - U5MR_low95]
        data_plot_dt[, "U5MR_wid90" := U5MR_up90 - U5MR_low90]
        data_plot_dt[, "U5MR_wid80" := U5MR_up80 - U5MR_low80]
        
        #changes names of columns if plotting NMR
        if(outcome=='nmr'){
          colnames(data_plot_dt) <- str_replace(colnames(data_plot_dt),'U5','N')
        }
        rowcount <- ceiling(length(year_vt)/5)
        
        #### plot median for all years ####
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                   country, "_", res_admin_level, 
                   "_", strat,"_",outcome, "_median.pdf"),
            width = 7.5, height = 10)
        
        print(SUMMER::mapPlot(data = data_plot_dt_df, is.long = T,
                              variables = "Year",
                              values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_median"),direction = -1,
                              geo = map_shp[[1]], ncol = 5,
                              legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                              per1000 = TRUE,
                              by.data = "GADM",
                              by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[res_admin_level==c('admin1','admin2')])))
        dev.off()
        
        ### plot 95 CI width for all years
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                  country, "_", res_admin_level, 
                  "_", strat,"_",outcome, "_wid95.pdf"),
            width = 10, height = 3.5*rowcount)
        {
        print(SUMMER::mapPlot(data = data_plot_dt_df, is.long = T, 
                              variables = "Year", 
                              values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_wid95"),direction = -1,
                              geo = map_shp[[1]], ncol = 5,
                              by.data = "GADM",
                              legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                              per1000 = TRUE,
                              by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[res_admin_level==c('admin1','admin2')])))
        }
        dev.off()
        
        ### plot median for current year
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                   country, "_", res_admin_level, 
                   "_", strat,"_",outcome, "_", end.proj.year, ".pdf"),
            width = 3.5, height = 3.5)
        {
          print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year == end.proj.year,],
                                is.long = T, 
                                variables = "Year", 
                                values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_median"),direction = -1,
                                geo = map_shp[[1]], ncol = 5,
                                legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                                per1000 = TRUE,
                                by.data = "GADM",
                                by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[res_admin_level==c('admin1','admin2')])))
        }
        dev.off()
        
        ### plot 95 CI width for current year
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                   country, "_", res_admin_level, 
                   "_", strat,"_",outcome, "_wid95_", end.proj.year, ".pdf"),
            width = 3.5, height = 3.5)
        {
          print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year == end.proj.year,],
                                is.long = T, 
                                variables = "Year", 
                                values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_median"),direction = -1,
                                geo = map_shp[[1]], ncol = 5,
                                legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                                per1000 = TRUE,
                                by.data = "GADM",
                                by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[res_admin_level==c('admin1','admin2')])))
        }
        dev.off()
        
        
      }
    }


## Heatmap plots ####
  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/NMR/heatmap'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/heatmap'))
  }
  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/U5MR/heatmap'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/heatmap'))
  }
for(outcome in outcome_vt){
  for(admin in admin_vt){
    load(paste0(res.dir,"/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/",country,'_',admin,'_strat_',outcome,'_postsamp.RData'))
    #CHANGE LATER TO BENCHMARKED MODEL
    load(paste0(res.dir,"/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/",country,'_res_',admin,'_strat_',outcome,'.rda'))
    admin.est <- eval(str2lang(paste0("bb.res.",admin,".strat.",outcome)))
    admin.names <- eval(str2lang(paste0(c('admin1','admin2')[admin==admin_vt],'.names')))
    
    # get admin medians
    admin.est <- dplyr::left_join(admin.names, admin.est$overall[admin.est$overall$years == 2020, ],
      by = c("Internal" = "region")
    ) %>% arrange(median)
    # get admin1 draws
    admin_samp <- postsamp_mt_list[[10]]$postsamp_mt
    
    # get indices for all possible pairs of admin areas
    admin_comb <- combn(nrow(admin_samp), 2)
    # compute pairwise exceedance
    plot_dat <- data.frame(Region1 = admin.names$GADM[admin_comb[1,]],
                           Region2 = admin.names$GADM[admin_comb[2,]],
                           prob = apply(admin_comb, 2, function(x) 
                             mean(admin_samp[x[1],] > admin_samp[x[2],])))
    # complete the square of comparisons
    plot_dat <- plot_dat %>% bind_rows(plot_dat %>% 
                  mutate(temp = Region2,  Region2 = Region1,
                         Region1 = temp,  prob = 1 - prob) %>% 
                  dplyr::select(-temp)) %>%  mutate(heatmap = T)
    # get medians for plotting
    median_dat <- data.frame(Region1 = admin.est$GADM, Region2 = "Median", 
                             est = paste0(sprintf("%0.2f", round(admin.est$median, digits = 2))))  %>% 
                              mutate(heatmap = F)
  
    extra_cols <- c("", "Median", "Interval")
    # combine into single tibble for plotting
    plot_dat <- bind_rows(plot_dat, median_dat) %>%
      mutate(Region1 = factor(Region1, levels = admin.est$GADM),
             Region2 = factor(Region2, levels = c(admin.est$GADM, extra_cols)))
    
    # MIGHT NEED TO CHANGE THESE to adjust size of cell lables
    if(admin=='adm1'){size.text = 5}
    if(admin=='adm2'){size.text = 1}
    
    pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/heatmap/",
               country, "_", admin, "_strat_", outcome, "_heatmap.pdf"))
    # plot
    g_heat <- plot_dat %>% ggplot(aes(x=Region2, y=Region1, fill=prob)) + 
      theme_minimal() + 
      theme(legend.position="bottom", 
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size = 10)) +
      geom_tile() + coord_equal() + 
      geom_text(aes(label = sprintf("%0.2f", round(prob, digits = 2)),
                    color = prob > .5),  size = size.text) + 
      geom_text(data = plot_dat,
                aes(label = est), size = size.text) +
      scale_fill_viridis_c(name = "Probability of Region 1 > Region 2")+ 
      scale_color_manual(values = c("white", "black"),  guide = "none") 
    print(g_heat)
    dev.off()
    
  }
}
  
  
## Ridge plots ####

year_vt <- c(2005,2010,2015,2020)

  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/NMR/ridge'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/ridge'))
  }
  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/U5MR/ridge'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/ridge'))
  }
  
  manual.col <- colorRampPalette(viridis_pal(direction = -1)(10))(10)[c(1,3,6,9)]# 1000)
  
  for(admin in admin_vt){
    
    admin_name_dt <- as.data.table(get(paste0(c('admin1','admin2')[admin==admin_vt], ".names")))
    
    map_shp <- c(poly.adm1,poly.adm2)[admin==admin_vt][[1]]
    
    # create plotting area names (just admin 1 name if admin = 1, or 'admin2,\n admin1' if admin = 2)
    if (admin == "adm1") {
      admin_name_dt$toPlot <- eval(str2lang(poly.label.adm1))
    } else if (admin == "adm2") {
      admin_name_dt$toPlot <- paste0(eval(str2lang(poly.label.adm2)),
                                         #need to change if using alternative GADM files
                                         ",\n", poly.adm2@data$NAME_1)
    }
    
    for(outcome in outcome_vt){
      for(strat in strat_vt){
        load(paste0(res.dir,'/Betabinomial/',c('NMR','U5MR')[outcome==outcome_vt], "/",
                    country, "_",
                    admin, "_", strat,  "_",
                    outcome, "_postsamp.RData"))
        
        data_plot_dt <- NULL
        
        for(year in year_vt){
          
          postsamp_mt <- postsamp_mt_list[which(year==year_vt)][[1]]$postsamp_mt
          
          data_plot_dt_year <- data.table(Year = year, 
                                          Internal = rep(admin_name_dt[, Internal], 1000),
                                          GADM = rep(admin_name_dt[, GADM], 1000),
                                          toPlot = rep(admin_name_dt[, toPlot], 1000),
                                          U5MR = as.numeric(postsamp_mt))
          
          data_plot_dt <- rbind(data_plot_dt, data_plot_dt_year)
        }
        
        # change column name if NMR
        if(outcome=='nmr'){
          colnames(data_plot_dt) <- str_replace(colnames(data_plot_dt),'U5MR','NMR')
        }
        
        data_plot_dt_1 <- data_plot_dt[Year == year_vt[1], ]
        if(outcome=='u5'){
          data_plot_dt_1[, "U5MR_med" := median(U5MR), by = c("Year", "Internal")]
          data_plot_dt_1_order <- data_plot_dt_1[order(U5MR_med, U5MR)]
          data_plot_dt[, U5MRperc := as.numeric(U5MR)]
        }
        if(outcome=='nmr'){
          data_plot_dt_1[, "NMR_med" := median(NMR), by = c("Year", "Internal")]
          data_plot_dt_1_order <- data_plot_dt_1[order(NMR_med, NMR)]
          data_plot_dt[, NMRperc := as.numeric(NMR)]
        }
        
        toPlot_order <- unique(data_plot_dt_1_order$toPlot)
        data_plot_dt[, Year := as.factor(Year)]
        data_plot_dt[, Area := factor(toPlot, levels = rev(toPlot_order))]
        
        rowcount <- ceiling(length(year_vt)/2)
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/ridge/",
                   country, "_",
                   admin, "_", strat,"_", outcome, "_ridgeplot.pdf"),
            width = 12, height = nrow(admin_name_dt)/15*rowcount*4)
        
        if(outcome=='nmr'){
          p <- ggplot(data_plot_dt, aes(x = NMRperc, y = Area))
        }
        if(outcome=='u5'){
          p <- ggplot(data_plot_dt, aes(x = U5MRperc, y = Area))
        }
        p <- p + geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
          scale_fill_gradientn(colours = manual.col,
                               name = c('NMR','U5MR')[outcome==outcome_vt]) +
          facet_wrap( ~ Year, nrow = rowcount) + 
          theme(axis.text.y = element_text(size = 6)) +
          xlab("")
        print(p)
        
        dev.off()
        
        data_plot_dt_1 <- data_plot_dt[Year == 2020, ]
        if(outcome=='nmr'){
          data_plot_dt_1[, "NMR_med" := median(NMR), by = c("Year", "Internal")]
          data_plot_dt_1_order <- data_plot_dt_1[order(NMR_med, NMR)]
          data_plot_dt[, NMRperc := as.numeric(NMR)]
        }
        
        if(outcome=='u5'){
        data_plot_dt_1[, "U5MR_med" := median(U5MR), by = c("Year", "Internal")]
        data_plot_dt_1_order <- data_plot_dt_1[order(U5MR_med, U5MR)]
        data_plot_dt[, U5MRperc := as.numeric(U5MR)]
        }
        
        toPlot_order <- unique(data_plot_dt_1_order$toPlot)
        data_plot_dt[, Year := as.factor(Year)]
        data_plot_dt[, Area := factor(toPlot, levels = rev(toPlot_order))]
        
        ncolumns <- length(unique(data_plot_dt$Internal)) %/% 25
        if(length(unique(data_plot_dt$Internal)) %% 25 != 0){
          ncolumns <- ncolumns + 1
        }
        nPerPlot <- floor(length(unique(data_plot_dt$Internal))/ncolumns)
        for(i in 1:ncolumns){
          if(i < ncolumns){
            tmp.areas <- toPlot_order[((i-1)*nPerPlot+1):(i*nPerPlot)]
          }else{
            tmp.areas <- toPlot_order[((i-1)*nPerPlot+1):length(unique(data_plot_dt$Internal))]
          }
          data_plot_dt$frame[data_plot_dt$toPlot %in% tmp.areas] <- i
        }
        nrows <- 1
        if(ncolumns > 5){
          nrows <- 2
        }
        ncolumns <- min(c(ncolumns,5))
        p <- list()
        for(j in 1:max(data_plot_dt$frame)){
          if(outcome=='nmr'){
            ptmp <- ggplot(data_plot_dt[data_plot_dt$Year == 2020 & data_plot_dt$frame==j,], aes(x = NMRperc, y = Area),) +
            xlim(c(0,quantile(data_plot_dt$NMRperc[data_plot_dt$Year == 2020],
                              .99)))
          }
          if(outcome=='u5'){
            ptmp <- ggplot(data_plot_dt[data_plot_dt$Year == 2020&data_plot_dt$frame==j,], aes(x = U5MRperc, y = Area),) +
              xlim(c(0,quantile(data_plot_dt$U5MRperc[data_plot_dt$Year == 2020],
                                .99)))
          }
          ptmp <- ptmp + geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
            scale_fill_gradientn(colours = manual.col,
                                 name = c("NMR","U5MR")[outcome==outcome_vt]) + 
            xlab("") +
            theme(axis.text.y = element_text(size = 6),
                  legend.position = 'none') +
            ylab("")
          
          p[[j]] <- ptmp
        }
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/ridge/",
                   country, "_",
                   admin, "_", strat,"_", outcome, "_2020_ridgeplot.pdf"),
            width = ncolumns*2.5 +1, height = 7)
        
        if(nrows  == 1){
          do.call(grid.arrange, c(p, nrow = nrows))
        }else{
          nplots <- length(p)
          npages <- ceiling(nplots/ncolumns)
          for(j in 1:npages){
            if(j < npages){
              plot.ids <- (1:nplots)[((j-1)*ncolumns+1):(j*ncolumns)]
            }else{
              plot.ids <- (1:nplots)[((j-1)*ncolumns+1):nplots]
            }
            p.plot <- p[plot.ids]
            do.call(grid.arrange,c(p.plot, ncol = ncolumns))
          }
        }
        dev.off()
      }
    }
  }

## TCP plots ####
  # We make true classification probability plot, which colors different admin regions based on the range their NMR/U5MR lies in.

  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/NMR/tcp'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/tcp'))
  }
  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/U5MR/tcp'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/tcp'))
  }

  ### parameter setup 
  K_vt <- c(2, 3, 4)
  admin_level_dt <- data.table(Admin = admin_vt,
                               level = level_vt)
  year <- end.proj.year
  
  ### load function
  get_measure_dt <- function(postsamp_mt, pred_dt, grp_thresh){
    # postsamp_mt <- state_postsamp_mt
    # pred_dt <- pred_dt_state
    # grp_thresh <- c(0, 0.5, 1)
    
    # create group lookup table based on thresholds
    n_grp <- length(grp_thresh)-1
    grp_dt <- data.table(grp = 1:n_grp,
                         lower = grp_thresh[1:(n_grp)],
                         upper = grp_thresh[2:(n_grp+1)])
    
    n_area <- nrow(postsamp_mt)
    n_postsamp <- ncol(postsamp_mt)
    
    
    #### assign group based on model max post prob ####
    
    
    grp_cnt_mt <- matrix(0, nrow = n_area, ncol = n_grp)
    for (i in 1:n_grp){
      grp_cnt_mt[, i] <- apply(postsamp_mt, 1, 
                               function(x){sum(x > grp_dt[i, lower] & x <= grp_dt[i, upper])})
    }
    
    DF <- data.frame(grp_cnt_mt)
    DT <- data.table(value = unlist(DF, use.names=FALSE), 
                     colid = 1:nrow(DF), 
                     rowid = rep(names(DF), each=nrow(DF)))
    setkey(DT, colid, value)
    
    grp_cnt_dt_max <- as.data.table(DF)
    grp_cnt_dt_max[, "grp"] <- DT[J(unique(colid), DT[J(unique(colid)), value, mult="last"]), rowid, mult="first"]
    grp_cnt_dt_max[, "TCP" := 0]
    for (i in 1:n_grp){
      idx_grp <- which(grp_cnt_dt_max[, grp] == paste0("X", i))
      grp_cnt_dt_max[idx_grp, "TCP"] <- grp_cnt_dt_max[idx_grp, paste0("X", i), with = F]/n_postsamp
    }
    
    pred_dt[, "grp"] <- grp_cnt_dt_max[, grp]
    pred_dt[, "TCP"] <- grp_cnt_dt_max[, TCP]
    
    return(pred_dt)
  }
  
  
  ### prepare results and plot
  for(outcome in outcome_vt){
    for(admin in admin_vt){
      admin_name_dt <- as.data.table(get(paste0(c('admin1','admin2')[admin==admin_vt], ".names")))
      map_shp <- c(poly.adm1,poly.adm2)[admin==admin_vt][[1]]
      
        for(strat in strat_vt){
          load( file = paste0(res.dir,"/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],'/', country, "_", admin,
                              "_", strat,"_",outcome, "_postsamp.RData"))
          
          cond <- lapply(postsamp_mt_list, function(x){x$years == year})
          postsamp_mt <- postsamp_mt_list[unlist(cond)][[1]]$postsamp_mt
          
          for(K in K_vt){
            
            intv <- 1/K
            quant_vt <- seq(0, 1, intv)
            quant_val_vt <- (quant_vt[1:K]+quant_vt[2:(K+1)])/2
            
            post_med_vt <- as.numeric(postsamp_mt) # apply(postsamp_mt, 1, median)
            
            L_vt <- quantile(post_med_vt, quant_vt)
            L_vt[1] <- quantile(post_med_vt, 0.01)
            L_vt[K+1] <- quantile(post_med_vt, 0.99)
            L_val_vt <- quantile(post_med_vt, quant_val_vt)
            L_dt <- data.table(grp = paste0("X", 1:K),
                               grp_low = L_vt[1:K],
                               grp_up = L_vt[2:(K+1)],
                               grp_val = L_val_vt)
            
            # create plotting area names (just admin 1 name if admin = 1, or 'admin2,\n admin1' if admin = 2)
            if (admin == "adm1") {
              namesToPlot <- eval(str2lang(poly.label.adm1))
            } else if (admin == "adm2") {
              namesToPlot <- paste0(eval(str2lang(poly.label.adm2)),
                                             #need to change if using alternative GADM files
                                             ",\n", poly.adm2@data$NAME_1)
            }
            
            measure_dt <- get_measure_dt(postsamp_mt = postsamp_mt,
                                         pred_dt = data.table(adm_name = paste0(c('admin1','admin2')[admin==admin_vt], "_", 1:nrow(postsamp_mt)),
                                                              adm_name_toPlot = namesToPlot),
                                         grp_thresh = L_vt)
            measure_dt <- merge(measure_dt, L_dt, by = "grp", all.x = T)
            measure_dt[, "K"] <- K
            
            measure_dt[, "Internal" := adm_name]
            measure_dt <- merge(measure_dt, admin_name_dt, by = "Internal", all.x = T)
            measure_dt[, paste0("NAME_", admin_level_dt[Admin == admin, level]) := GADM]
            
            save(measure_dt, L_dt,
                 file = paste0(res.dir,"/Betabinomial/", c('NMR','U5MR')[outcome==outcome_vt], "/", country, "_",
                               c('admin1','admin2')[admin==admin_vt], "_", 
                               strat, "_" ,outcome,"_Y", year,
                               "_K", K, "_measure.RData"))
            
            # set map names to be unique for merging (add in Internal names)
            map_shp$NAME_INTERNAL <- as.character(admin_name_dt[, Internal])
            
            shp_plot <- merge(map_shp, measure_dt,
                              by.x = "NAME_INTERNAL",
                              by.y = "adm_name")
            
            #manual.col <- rev(colorRampPalette(brewer.pal(8, "RdYlGn"))(length(L_dt$grp_val))) # 1000)
            manual.col <- colorRampPalette(viridis_pal(direction = -1)(10))(10)[c(1,3,6,9)]# 1000)
            manual.col[4] <- "navyblue"
            # color.match <- manual.col[1:length(L_dt$grp_val)] # round(sort(L_dt$grp_val)*1000)]
            if(K == 2){
              color.match <- manual.col[c(1,4)]
            }else if(K == 3){
              color.match <- manual.col[c(1,3,4)]
            }else if(K == 4){
              color.match <- manual.col
            }
            lookup_dt <- data.table(grp_val = sort(L_dt$grp_val),
                                    col = color.match)
            shp_plot <- merge(shp_plot, lookup_dt, by = "grp_val", duplicateGeoms = TRUE)
            shp_plot$grp_val <- as.factor(shp_plot$grp_val)
            col_regions <- as.vector(lookup_dt[grp_val %in% shp_plot$grp_val, col])
            
            labelat <- sort(unique(c(L_dt$grp_low, 
                                     L_dt$grp_up)))
            labeltext <- format(round(labelat, 2), nsmall = 2)
            
            #### measure map ####
            
            pdf(paste0(res.dir,"/Figures/Betabinomial/", c('NMR','U5MR')[outcome==outcome_vt], '/tcp/',
                       country, "_", admin,
                       "_", strat, "_", outcome, "_Y", year,
                       "_K", K, "_measuremap.pdf"),
                width = 8, height = 8)
            sp_plot <- spplot(shp_plot, zcol = "grp_val",
                              main = list(label=paste0("ATCP = ", format(round(mean(measure_dt$TCP), 2), nsmall = 2)),
                                          cex=1.5),
                              xlab = "", ylab = "",
                              # sp.layout = list(scale_bar, text_x, text_y),
                              col.regions = col_regions,
                              colorkey = list(col = color.match,
                                              at = labelat,
                                              labels = list(at = labelat, labels = labeltext,
                                                            cex=1.5)))
            print(sp_plot)
            dev.off()
            
          }  
        }
    }
  }
  
  
  
  
  
  
  