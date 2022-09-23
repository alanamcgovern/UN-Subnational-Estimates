rm(list=ls())

# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Guinea'

## Setup -----------------------------------------------
#### Load libraries and info ----------------------------------------------------------

# Libraries
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
options(gsubfn.engine = "R")
library(rgdal)
library(ggplot2)
library(SUMMER)
library(Rfast)
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

if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/U5MR'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/NMR'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/NMR'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Summary'))){
  dir.create(paste0(res.dir,
                    '/Figures/Summary'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Summary/U5MR'))){
  dir.create(paste0(res.dir,
                    '/Figures/Summary/U5MR'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Summary/NMR'))){
  dir.create(paste0(res.dir,
                    '/Figures/Summary/NMR'))}

#### Load model data ----------------------------------------------------------

load(paste0(data.dir, '/', country, '_cluster_dat.rda'), envir = .GlobalEnv)

mod.dat$years <- as.numeric(as.character(mod.dat$years))
dat.years <- sort(unique(mod.dat$years))
mod.dat$strata.orig <- mod.dat$strata
mod.dat$strata <- mod.dat$urban
mod.dat$country <- as.character(country)

#### Load polygon files  ------------------------------------------------------
setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm0)) # load the national shape file
# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions

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


#### Parameters ####
outcome_vt <- c('nmr','u5')
strat_vt <- c("strat","unstrat") 
admin_vt <- c("adm1","adm2") ## admin level
level_vt <- c(1,2) ## polygon level
admin_level_dt <- as.data.table(cbind(Admin = admin_vt, level = level_vt))
year_vt <- 2000:end.proj.year
n_years <- length(year_vt)

ages <- levels(mod.dat$age)
ages.strata <- c(paste0(ages,':urban'),paste0(ages,':rural'))
age.strata.cols <- rainbow(length(ages.strata))
age.cols[2] <- "orange"

plot.years <- beg.year:end.proj.year
obs.idx <- which(beg.year:end.proj.year %in% mod.dat$years)
pred.idx <- which(!(beg.year:end.proj.year %in% mod.dat$years))

pane.years <- seq(beg.year + 2,  end.proj.year - 2, 5)
est.idx <- which(seq(beg.year,   end.proj.year - 5, 5) < max(as.numeric(as.character(mod.dat$years))))
cols <- rainbow(8+1+1+1)
cols <- cols[c(1,3,7,2,4,11,9,5,6,8,10)]

survey.legends <- unique(mod.dat[,c("survey","survey.type")])
survey.legends <- survey.legends[order(survey.legends$survey),]
survey_names <- paste0(survey.legends$survey.type, ' ', survey.legends$survey)

## National figures ####

### Load results ####
if(doHIVAdj){
  load(paste0(res.dir, '/Direct/U5MR/', country, '_directHIV_natl_yearly_u5.rda'), envir = .GlobalEnv)
}else{
  load(paste0(res.dir, '/Direct/U5MR/', country,  '_direct_natl_yearly_u5.rda'),  envir = .GlobalEnv)
}
load(paste0(res.dir, '/Direct/U5MR/', country,  '_res_natl_yearly_u5_SmoothedDirect.rda'),  envir = .GlobalEnv)
load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_res_natl_strat_u5.rda'), envir = .GlobalEnv)
load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_temporals_natl_strat_u5.rda'), envir = .GlobalEnv)
load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_fixed_natl_strat_u5.rda'), envir = .GlobalEnv)
load(paste0(res.dir, '/Betabinomial/U5MR/', country, '_hyperpar_natl_strat_u5.rda'), envir = .GlobalEnv)

## Convert to U5MR (# of children per 1000)
res.natl.yearly.u5[,c("median", "lower", "upper")] <-
  res.natl.yearly.u5[,c("median", "lower", "upper")]*1000
direct.natl.yearly.u5[,c("mean", "upper", "lower")] <-
  direct.natl.yearly.u5[,c("mean", "upper", "lower")]*1000

### Spaghetti Plots ####
pdf(paste0(res.dir,'/Figures/Summary/U5MR/', country, '_natl_strat_u5_spaghetti.pdf'), height = 8, width = 8)
{
  tmp.area <-bb.res.natl.strat.u5$overall
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  
  tmp.area$median <- tmp.area$median*1000
  tmp.area$upper <- tmp.area$upper*1000
  tmp.area$lower <- tmp.area$lower*1000
  
  par(mfrow = c(2,2), lend=1)
  
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
  lines(plot.years,  res.natl.yearly.u5[res.natl.yearly.u5$years %in% plot.years,]$median,  col = cols[11], lwd = 2)
  
  ## Add Betabinomial
  lines(plot.years, tmp.area$median,  col = 'black', lwd = 2)
  
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
  
  #### Add Betabinomial 
  polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
          y = c(tmp.area$upper, rev(tmp.area$lower)),
          col = alpha('black', 0.25), border = FALSE)
  
  ### Plot 3: Smoothed Uncertainty ####  
  
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
          y = c(tmp.area$upper, rev(tmp.area$lower)),
          col = alpha('black', 0.25), border = FALSE)
  lines(tmp.area$years.num,tmp.area$median, 
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
}
dev.off() 

### Hazards over time ####

temporals <- bb.temporals.natl.strat.u5

pdf(paste0(res.dir, '/Figures/Betabinomial/U5MR/trend/', country, '_natl_strat_u5_temporal.pdf'), 
    height = 5, width = 7.5)
{
  
  for(age.strata in ages.strata){
      age <- ages[sapply(ages,grepl,age.strata)]
      age.idx <- which(age==ages)
      temporal.idx <- grepl(paste0(age.strata), temporals$group) | is.na(temporals$group)
      fixed.idx <- grepl(paste0(age.strata),  row.names(bb.fixed.natl.strat.u5))|grepl(paste0('group',age.idx),  row.names(bb.fixed.natl.strat.u5))
      fixed.eff <- sum(bb.fixed.natl.strat.u5$`0.5quant`[fixed.idx])
      tmp <- data.frame(temporals[temporal.idx,] %>% group_by(years.num) %>% summarise(sum(median)))
      colnames(tmp) <- c('years.num','median')
      
      if(match(age.strata, ages.strata) == 1){
        plot.min <- min(outer(temporals$median, bb.fixed.natl.strat.u5$`0.5quant`,
                              FUN="+")) - 0.25
        plot.max <- max(outer(temporals$median, 
                              bb.fixed.natl.strat.u5$`0.5quant`, 
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


## Admin1 figures ####
### Load results ####
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

### Spaghetti Uncertainty Plots (9 per) ####

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
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx],
            col = cols[length(survey_years)+1],
            lwd = 2)
      lines(tmp.area$years.num[pred.idx], 
            tmp.area$median[pred.idx], 
            col = cols[length(survey_years)+1], 
            lwd = 2, lty = 2)
      
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
### Load results ####
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

### Spaghetti Uncertainty Plots (9 per) ####

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
      lines(tmp.area$years.num[obs.idx], 
            tmp.area$median[obs.idx],
            col = cols[length(survey_years)+1],
            lwd = 2)
      lines(tmp.area$years.num[pred.idx], 
            tmp.area$median[pred.idx], 
            col = cols[length(survey_years)+1], 
            lwd = 2, lty = 2)
      
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

## generate posterior samples ####
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

## national comparison plot (need to check) ####
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/comparison'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/U5MR/comparison'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/comparison'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/NMR/comparison'))}
setwd(res.dir)

##### function to get posterior draws from BB8 #####
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

#### load admin1 and admin2 weights #####
load(paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm1_weights_u5.rda'))
load(paste0(data.dir,'/worldpop/adm2_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm2_weights_u5.rda'))

#### prepare national level models ####
### national yearly direct
load(file = paste0('Direct/NMR/', country, '_direct_natl_yearly_nmr.rda'))  
load(file = paste0('Direct/U5MR/', country, '_direct_natl_yearly_u5.rda')) 

natl.direct.yearly.nmr.draw = expit(Rfast::rmvnorm(10000, mu = direct.natl.yearly.nmr$logit.est,sigma = direct.natl.yearly.nmr$var.est*diag(nrow(direct.natl.yearly.nmr))))
natl.direct.yearly.nmr = t(apply(natl.direct.yearly.nmr.draw, 2, quantile, probs = c(0.025, 0.5, 0.975)))
colnames(natl.direct.yearly.nmr) = c("lower_nmr", "median_nmr", "upper_nmr")

natl.direct.yearly.u5.draw = expit(Rfast::rmvnorm(10000, mu = direct.natl.yearly.u5$logit.est,sigma = direct.natl.yearly.u5$var.est*diag(nrow(direct.natl.yearly.u5))))
natl.direct.yearly.u5 = t(apply(natl.direct.yearly.u5.draw, 2, quantile, probs = c(0.025, 0.5, 0.975)))
colnames(natl.direct.yearly.u5) = c("lower_u5", "median_u5", "upper_u5")

natl.direct.yearly.frame <- as.data.frame(cbind(natl.direct.yearly.nmr,natl.direct.yearly.u5))
natl.direct.yearly.frame$method <- 'natl.direct'
natl.direct.yearly.frame$years <- c(beg.year: max(survey_years))

### national yearly smooth direct
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
                          method='natl.sd', years=natl.sd.year.u5)

### national betabinomial models
load(file = paste0('Betabinomial/NMR/', country, '_res_natl_unstrat_nmr.rda'))  
load(file = paste0('Betabinomial/NMR/', country, '_res_natl_strat_nmr.rda')) 
load(file = paste0('Betabinomial/U5MR/', country, '_res_natl_unstrat_u5.rda'))  
load(file = paste0('Betabinomial/U5MR/', country, '_res_natl_strat_u5.rda')) 

#### prepare admin1 level models ####
### smooth direct admin1 3-year window
load(file = paste0('Direct/NMR/', country, '_res_admin1_nmr_SmoothedDirect.rda'))
admin1.sd.nmr <- res.admin1.nmr$results
admin1.sd.nmr.draws <- res.admin1.nmr$draws
load(file = paste0('Direct/U5MR/', country, '_res_admin1_u5_SmoothedDirect.rda'))  
admin1.sd.u5 <- res.admin1.u5$results
admin1.sd.u5.draws <- res.admin1.u5$draws

beg.period.years <- unique(admin1.sd.nmr[admin1.sd.nmr$years.num<=end.proj.year,]$years.num)
sd.adm1.to.natl.frame = matrix(NA, nrow = length(beg.period.years), ncol =  6)

for (i in 1: length(beg.period.years)){
  year = beg.period.years[i]
  
  adm1.pop.nmr <- weight.adm1.u1[weight.adm1.u1$years==year,]
  tmp.res<-admin1.sd.nmr[order(admin1.sd.nmr$region.gadm),]
  sd.idx<-which(tmp.res$years.num==year) #assuming admin1_idx has the correct order
  t.sd.nmr.draw <- t(admin1.sd.nmr.draws)
  t.sd.nmr.draw<-t.sd.nmr.draw[,sd.idx]
  colnames(t.sd.nmr.draw)<-adm1.pop.nmr$region
  adm1.sd.natl.nmr.draw <- t.sd.nmr.draw %*% adm1.pop.nmr$proportion
  
  adm1.pop.u5 <- weight.adm1.u5[weight.adm1.u5$years==year,]
  tmp.res<-admin1.sd.u5[order(admin1.sd.u5$region.gadm),]
  sd.idx<-which(tmp.res$years.num==year) #assuming admin1_idx has the correct order
  t.sd.u5.draw <- t(admin1.sd.u5.draws)
  t.sd.u5.draw<-t.sd.u5.draw[,sd.idx]
  colnames(t.sd.u5.draw)<-adm1.pop.u5$region
  adm1.sd.natl.u5.draw <- t.sd.u5.draw %*% adm1.pop.u5$proportion
  
  sd.adm1.to.natl.frame[i, ] = c(quantile(adm1.sd.natl.nmr.draw, probs = c(0.025, 0.5, 0.975)), quantile(adm1.sd.natl.u5.draw, probs = c(0.025, 0.5, 0.975)))
}

sd.adm1.to.natl.frame<-as.data.frame(sd.adm1.to.natl.frame)
colnames(sd.adm1.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
sd.adm1.to.natl.frame$method <- "aggre.sd.adm1"
sd.adm1.to.natl.frame$years = beg.period.years

### smooth direct admin1 yearly
load(file = paste0('Direct/NMR/', country, '_res_admin1_nmr_SmoothedDirect_yearly.rda'))
admin1.sd.yearly.nmr <- sd.admin1.yearly.nmr$results
admin1.sd.yearly.nmr.draws <- sd.admin1.yearly.nmr$draws
load(file = paste0('Direct/U5MR/', country, '_res_admin1_u5_SmoothedDirect_yearly.rda'))  
admin1.sd.yearly.u5 <- sd.admin1.yearly.u5$results
admin1.sd.yearly.u5.draws <- sd.admin1.yearly.u5$draws

sd.adm1.yl.to.natl.frame = matrix(NA, nrow = length(beg.year:end.proj.year), ncol =  6)

for (i in 1: length(beg.year:end.proj.year)){
  year = (beg.year:end.proj.year)[i]
  
  adm1.pop.nmr <- weight.adm1.u1[weight.adm1.u1$years==year,]
  tmp.res<-admin1.sd.yearly.nmr[order(admin1.sd.yearly.nmr$region.gadm),]
  sd.idx<-which(tmp.res$years.num==year) #assuming admin1_idx has the correct order
  t.sd.nmr.draw <- t(admin1.sd.yearly.nmr.draws)
  t.sd.nmr.draw<-t.sd.nmr.draw[,sd.idx]
  colnames(t.sd.nmr.draw)<-adm1.pop.nmr$region
  adm1.sd.natl.nmr.draw <- t.sd.nmr.draw %*% adm1.pop.nmr$proportion
  
  adm1.pop.u5 <- weight.adm1.u5[weight.adm1.u5$years==year,]
  tmp.res<-admin1.sd.yearly.u5[order(admin1.sd.yearly.u5$region.gadm),]
  sd.idx<-which(tmp.res$years.num==year) #assuming admin1_idx has the correct order
  t.sd.u5.draw <- t(admin1.sd.yearly.u5.draws)
  t.sd.u5.draw<-t.sd.u5.draw[,sd.idx]
  colnames(t.sd.u5.draw)<-adm1.pop.u5$region
  adm1.sd.natl.u5.draw <- t.sd.u5.draw %*% adm1.pop.u5$proportion
  
  sd.adm1.yl.to.natl.frame[i, ] = c(quantile(adm1.sd.natl.nmr.draw, probs = c(0.025, 0.5, 0.975)), quantile(adm1.sd.natl.u5.draw, probs = c(0.025, 0.5, 0.975)))
}

sd.adm1.yl.to.natl.frame<-as.data.frame(sd.adm1.yl.to.natl.frame)
colnames(sd.adm1.yl.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
sd.adm1.yl.to.natl.frame$method <- "aggre.sd.yearly.adm1"
sd.adm1.yl.to.natl.frame$years = beg.year:end.proj.year

### BB8 admin1 stratified  -- change to benchmarked later
load(file = paste0('Betabinomial/NMR/',country,'_res_adm1_strat_nmr.rda'))
res.strat.admin1.nmr <- bb.res.adm1.strat.nmr
admin1.strat.nmr.BB8<-res.strat.admin1.nmr$overall
load(file = paste0('Betabinomial/U5MR/',country,'_res_adm1_strat_u5.rda'))
res.strat.admin1.u5 <- bb.res.adm1.strat.u5
admin1.strat.u5.BB8<-res.strat.admin1.u5$overall

BB8.adm1.to.natl.frame <- matrix(NA, nrow = length(beg.year:end.proj.year), ncol =  6)
for (i in 1: length(beg.year:end.proj.year)){
  year = (beg.year:end.proj.year)[i]
  
  adm1.pop.nmr <- weight.adm1.u1[weight.adm1.u1$years==year,]
  admin1.strat.nmr.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin1.nmr$draws.est.overall,
                                         year_num=year,
                                         admin_vec=admin1.names$Internal)
  natl.tmp.nmr <- admin1.strat.nmr.BB8.draw %*% adm1.pop.nmr$proportion
  
  adm1.pop.u5 <- weight.adm1.u5[weight.adm1.u5$years==year,]
  admin1.strat.u5.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin1.u5$draws.est.overall,
                                        year_num=year,
                                        admin_vec=admin1.names$Internal)
  natl.tmp.u5 <- admin1.strat.u5.BB8.draw %*% adm1.pop.u5$proportion
  
  BB8.adm1.to.natl.frame[i, ] = c(quantile(natl.tmp.nmr, probs = c(0.025, 0.5, 0.975)),  quantile(natl.tmp.u5, probs = c(0.025, 0.5, 0.975)))
  
}

BB8.adm1.to.natl.frame<-as.data.frame(BB8.adm1.to.natl.frame)
colnames(BB8.adm1.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
BB8.adm1.to.natl.frame$method <- "aggre.adm1.strat.BB8"
BB8.adm1.to.natl.frame$years = beg.year:end.proj.year

#### prepare admin2 level models ####
### BB8 admin2 stratified -- change to benchmarked later
load(file = paste0('Betabinomial/NMR/',country,'_res_adm2_strat_nmr.rda'))
res.strat.admin2.nmr <- bb.res.adm2.strat.nmr
admin2.strat.nmr.BB8<-res.strat.admin2.nmr$overall
load(file = paste0('Betabinomial/U5MR/',country,'_res_adm2_strat_u5.rda'))
res.strat.admin2.u5 <- bb.res.adm2.strat.u5
admin2.strat.u5.BB8<-res.strat.admin2.u5$overall

BB8.adm2.to.natl.frame <- matrix(NA, nrow = length(beg.year:end.proj.year), ncol =  6)

for (i in 1: length(beg.year:end.proj.year)){
  year = (beg.year:end.proj.year)[i]
  
  adm2.pop.nmr <- weight.adm2.u1[weight.adm2.u1$years==year,]
  admin2.strat.nmr.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin2.nmr$draws.est.overall,
                                         year_num=year,
                                         admin_vec=admin2.names$Internal)
  natl.tmp.nmr <- admin2.strat.nmr.BB8.draw %*% adm2.pop.nmr$proportion
  
  adm2.pop.u5 <- weight.adm2.u5[weight.adm2.u5$years==year,]
  admin2.strat.u5.BB8.draw<-draw_1y_adm(admin_draws=res.strat.admin2.u5$draws.est.overall,
                                        year_num=year,
                                        admin_vec=admin2.names$Internal)
  natl.tmp.u5 <- admin2.strat.u5.BB8.draw %*% adm2.pop.u5$proportion
  
  BB8.adm2.to.natl.frame[i, ] = c(quantile(natl.tmp.nmr, probs = c(0.025, 0.5, 0.975)),  quantile(natl.tmp.u5, probs = c(0.025, 0.5, 0.975)))
  
}
BB8.adm2.to.natl.frame<-as.data.frame(BB8.adm2.to.natl.frame)
colnames(BB8.adm2.to.natl.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
BB8.adm2.to.natl.frame$method <- "aggre.adm2.strat.BB8"
BB8.adm2.to.natl.frame$years = beg.year:end.proj.year

#### prepare IGME estimates ####
igme.frame <- as.data.frame(cbind(igme.ests.nmr$LOWER_BOUND,igme.ests.nmr$OBS_VALUE,igme.ests.nmr$UPPER_BOUND,
      igme.ests.u5$LOWER_BOUND,igme.ests.u5$OBS_VALUE,igme.ests.u5$UPPER_BOUND))
colnames(igme.frame) = c("lower_nmr", "median_nmr", "upper_nmr","lower_u5", "median_u5", "upper_u5")
igme.frame$method <- "igme"
igme.frame$years <- beg.year:end.proj.year

### combine all methods

natl.all<-rbind(natl.direct.yearly.frame, sd.adm1.yl.to.natl.frame,
                sd.adm1.to.natl.frame, natl.sd.frame,
                BB8.adm1.to.natl.frame,  BB8.adm2.to.natl.frame,
                igme.frame)
natl.all$years.num <- as.numeric(natl.all$years)
natl.all$is.yearly <- FALSE
class(natl.all)<-class(res.strat.admin1.nmr$overall)

#### final plot ####
natl.to.plot <- natl.all %>% filter(!(method %in% c('natl.direct','aggre.sd.yearly.adm1')))

g1 <- natl.to.plot %>% ggplot(aes(x=years,y=median_nmr,group=method,color=method)) + geom_line()

g1 <- plot(natl.all$median_nmr, plot.CI = TRUE, dodge.width = 0.25, proj_year = end.proj.year + 1, per1000=TRUE) +
  theme(legend.position = 'bottom') + scale_linetype(guide='none')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(beg.year:end.proj.year))+
  ylab('Deaths per 1000 live births')+
  labs(color='Method')+
  scale_colour_discrete(labels = c( "Aggregated BB8 admin-2", "National direct yearly"))

## trend plots ------------------------------------------------------
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



## map plots ####
if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/U5MR/map'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/map'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/map'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/NMR/map'))}
for(admin in admin_vt){
    
    map_shp <- c(poly.adm1,poly.adm2)[admin==admin_vt]
    admin_name_dt <- as.data.table(get(paste0(c('admin1','admin2')[admin==admin_vt], ".names")))
    
    for(outcome in outcome_vt){    
      for(strat in strat_vt){
        
        load(paste0(res.dir, "/Betabinomial/", c('NMR','U5MR')[outcome==outcome_vt], "/",
                    country,'_', admin, "_",
                    strat,'_', outcome, "_postsamp.RData"))
        
        data_plot_dt <- NULL
        
        for(year in year_vt){
          
          postsamp_mt <- postsamp_mt_list[[which(year==year_vt)]]$postsamp_mt
          
          # create plotting area names (just admin 1 name if admin = 1, or 'admin2,\n admin1' if admin = 2)
          if (admin == "adm1") {
            admin_name_dt$nameToPlot <- eval(str2lang(poly.label.adm1))
          } else if (admin == "adm2") {
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
        
        #### plot ####
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                   country, "_", admin, 
                   "_", strat,"_",outcome, "_mean.pdf"),
            width = 10, height = 3.5*rowcount)
        data_plot_dt_df <- as.data.frame(data_plot_dt)
        
        print(SUMMER::mapPlot(data = data_plot_dt_df, 
                              variables="Year", is.long = T,
                              values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_mean"), direction = -1,
                              geo = map_shp[[1]], ncol = 5,
                              by.data = "GADM",
                              legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                              per1000 = TRUE,
                              by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[admin==admin_vt])))
        
        dev.off()
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                   country, "_", admin, 
                   "_", strat,"_",outcome, "_median.pdf"),
            width = 7.5, height = 10)
        
        print(SUMMER::mapPlot(data = data_plot_dt_df, is.long = T,
                              variables = "Year",
                              values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_median"),direction = -1,
                              geo = map_shp[[1]], ncol = 5,
                              legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                              per1000 = TRUE,
                              by.data = "GADM",
                              by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[admin==admin_vt])))
        dev.off()
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                  country, "_", admin, 
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
                              by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[admin==admin_vt])))
        }
        dev.off()
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                   country, "_", admin, 
                   "_", strat,"_",outcome, "_2020.pdf"),
            width = 3.5, height = 3.5)
        {
          print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year == 2020,],
                                is.long = T, 
                                variables = "Year", 
                                values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_median"),direction = -1,
                                geo = map_shp[[1]], ncol = 5,
                                legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                                per1000 = TRUE,
                                by.data = "GADM",
                                by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[admin==admin_vt])))
        }
        dev.off()
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                   country, "_", admin, 
                   "_", strat,"_",outcome, "_someyearsMedian.pdf"),
            width = 7, height = 7)
        {
          print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year %in% 
                                                         c(2005,2010,2015,2020),],
                                is.long = T, 
                                variables = "Year", 
                                values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_median"),direction = -1,
                                geo = map_shp[[1]], ncol = 2,
                                legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                                per1000 = TRUE,
                                by.data = "GADM",
                                by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[admin==admin_vt])))
        }
        dev.off()
        
        
        
        
        
      }
    }
}

## rank plot ####
  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/NMR/rank'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/rank'))
  }
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/rank'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/U5MR/rank'))
}
  year <- 2020
  for(admin in admin_vt){
    
    admin_name_dt <- as.data.table(get(paste0(c('admin1','admin2')[admin==admin_vt], ".names")))
    
    map_shp <- c(poly.adm1,poly.adm2)[admin==admin_vt][[1]]
    
    # create plotting area names (just admin 1 name if admin = 1, or 'admin2,\n admin1' if admin = 2)
    if (admin == "adm1") {
      admin_name_dt$nameToPlot <- eval(str2lang(poly.label.adm1))
    } else if (admin == "adm2") {
      admin_name_dt$nameToPlot <- paste0(eval(str2lang(poly.label.adm2)),
                                         #need to change if using alternative GADM files
                                         ",\n", poly.adm2@data$NAME_1)
    }
    
    for(outcome in outcome_vt){
      for(strat in strat_vt){
        
        load(paste0(res.dir, "/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],'/',
                    country, "_",
                    admin, "_",
                    strat, "_",
                    outcome, "_postsamp.RData"))
        
          cond <- lapply(postsamp_mt_list, function(x){x$years == year})
          postsamp_mt <- postsamp_mt_list[which(year == year_vt)][[1]]$postsamp_mt
          
          #### rank postsamps ####
          
          rank_mt <- apply(postsamp_mt, 2, rank)
          
          pred_dt <- admin_name_dt
          pred_dt[, "ID"] <- 1:nrow(pred_dt)
          pred_dt[, "avg_rank"] <- apply(rank_mt, 1, mean)
          pred_dt[, "low_rank"] <- apply(rank_mt, 1, min)
          pred_dt[, "up_rank"] <- apply(rank_mt, 1, max)
          
          #### all states hist ####
          rowcount <- ceiling(nrow(pred_dt_order)/3)
          pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/rank/",
                     country, "_",
                     admin, "_", strat, "_", outcome,
                     "_Y", year, "_rankall.pdf"),
              width = 15, height = rowcount*2)
          
        {  
          par(mar = c(2.5, 1, 2, 1), mfcol = c(rowcount, 3))
          
          pred_dt_order <- pred_dt[order(avg_rank)]
          
          for (i in 1:nrow(pred_dt_order)){
            # i <- 1
            
            id <- pred_dt_order[i, ID]
            name <- pred_dt_order[i, nameToPlot]
            
            rank_vt <- rank_mt[id, ]
            
            avg_rank <- pred_dt_order[i, avg_rank]
            
            ranktable <- as.data.table(table(rank_vt))
            ranktable <- merge(data.table(rank = as.character(1:nrow(pred_dt_order))), ranktable, 
                               by.x = "rank", by.y = "rank_vt", all.x = T)
            ranktable[, "rank" := as.integer(rank)]
            ranktable <- ranktable[order(rank)]
            ranktable[is.na(N), "N"] <- 0
            
            barplot(ranktable$N, width = 0.825, 
                    xlim = c(nrow(pred_dt_order), 0), xlab = "", ylab = "",
                    main = paste0(name, "\nER = ", format(round(avg_rank, 1), nsmall = 1)),
                    xaxt = "n", yaxt = "n", col = "#08519c", border = F,
                    cex.main = 0.75)
            axis(1, at = nrow(pred_dt_order):1-0.5, labels = as.character(nrow(pred_dt_order):1), tick = F)
          }
        }
          dev.off()
        
      }
    }
  }


## heatmap plots ####
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
                             est = paste0(sprintf("%0.2f", round(admin.est$median * 1000, digits = 2))))  %>% 
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
  
  
## ridge plots ####

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
          data_plot_dt[, U5MRperc := as.numeric(U5MR*1000)]
        }
        if(outcome=='nmr'){
          data_plot_dt_1[, "NMR_med" := median(NMR), by = c("Year", "Internal")]
          data_plot_dt_1_order <- data_plot_dt_1[order(NMR_med, NMR)]
          data_plot_dt[, NMRperc := as.numeric(NMR*1000)]
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
          data_plot_dt[, NMRperc := as.numeric(NMR*1000)]
        }
        
        if(outcome=='u5'){
        data_plot_dt_1[, "U5MR_med" := median(U5MR), by = c("Year", "Internal")]
        data_plot_dt_1_order <- data_plot_dt_1[order(U5MR_med, U5MR)]
        data_plot_dt[, U5MRperc := as.numeric(U5MR*1000)]
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
            labeltext <- format(round(labelat*1000, 2), nsmall = 2)
            
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
  
  
  
  
  
  
  