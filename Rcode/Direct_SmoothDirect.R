rm(list = ls())
# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'

# Setup
# Load libraries and info ----------------------------------------------------------

options(gsubfn.engine = "R")
library(rgdal)
library(SUMMER)
library(dplyr)

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

# Load polygon files  ------------------------------------------------------
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

# load a helper function from the R script getSmoothed.R in the same folder
source(file=paste0(home.dir, '/Rcode/getSmoothed.R'))


# Define periods for 3-year estimates ------------------------------------------------------
#### adjusted slightly when number of years is not divisible by 3

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


# Load data and separate each survey  ------------------------------------------------------
svy.idx <- 0
births.list <- list()
births.list.nmr <- list()

load(paste0(country,'_cluster_dat.rda'),
       envir = .GlobalEnv)
mod.dat$years <- as.numeric(as.character(mod.dat$years)) # convert the years from string into numbers
mod.dat$v005 <- mod.dat$v005/1e6
# create subset of data for nmr estimates
mod.dat.nmr <- mod.dat %>% filter(age == '0')
  for(survey in survey_years){
    svy.idx <- svy.idx + 1
    # data for u5mr
    births.list[[svy.idx]] <- mod.dat[mod.dat$survey == survey,] %>%
      as.data.frame()
    births.list[[svy.idx]]$died <- births.list[[svy.idx]]$Y
    births.list[[svy.idx]]$total <- as.numeric(births.list[[svy.idx]]$total)
    births.list[[svy.idx]]$period <- as.character(cut(births.list[[svy.idx]]$years, breaks = c(beg.period.years, beg.period.years[length(beg.period.years)]+5),
                                       include.lowest = T, right = F, labels = periods)) # generate period label 
    # data for nmr
    births.list.nmr[[svy.idx]] <- mod.dat.nmr[mod.dat.nmr$survey == survey,] %>%
      as.data.frame()
    births.list.nmr[[svy.idx]]$died <- births.list.nmr[[svy.idx]]$Y
    births.list.nmr[[svy.idx]]$total <- as.numeric(births.list.nmr[[svy.idx]]$total)
    births.list.nmr[[svy.idx]]$period <- as.character(cut(births.list.nmr[[svy.idx]]$years, breaks = c(beg.period.years, beg.period.years[length(beg.period.years)]+5),
                                                      include.lowest = T, right = F, labels = periods)) # generate period label 
  }

names(births.list) <- survey_years
names(births.list.nmr) <- survey_years


# Direct Estimates  ------------------------------------------------------

setwd(paste0(res.dir,'/Direct'))

## National ------------------------------------------------------

#if there is more than one survey
if(length(births.list) != 1){
    # 3-year estimates
     direct.natl.u5 <-  SUMMER::getDirectList(births.list, periods,
                                           regionVar = "admin1.char",
                                           timeVar = "period", 
                                           clusterVar =  "~cluster",
                                          ageVar = "age", Ntrials = "total",
                                           weightsVar = "v005",national.only = T)
     direct.natl.nmr <-  SUMMER::getDirectList(births.list.nmr, periods,
                                              regionVar = "admin1.char",
                                              timeVar = "period", 
                                              clusterVar =  "~cluster",
                                              ageVar = "age", Ntrials = "total",
                                              weightsVar = "v005",national.only = T)
     # yearly estimates
    direct.natl.yearly.u5 <- SUMMER::getDirectList(births.list, beg.year:end.year,
                                                regionVar = "admin1.char",
                                                timeVar = "years", 
                                                clusterVar =  "~cluster",
                                                ageVar = "age", Ntrials = "total",
                                                weightsVar = "v005",national.only = T)
    direct.natl.yearly.nmr <- SUMMER::getDirectList(births.list.nmr, beg.year:end.year,
                                                   regionVar = "admin1.char",
                                                   timeVar = "years", 
                                                   clusterVar =  "~cluster",
                                                   ageVar = "age", Ntrials = "total",
                                                   weightsVar = "v005",national.only = T)
    
    direct.natl.u5$region_num <- direct.natl.u5$region
    direct.natl.yearly.u5$region_num <- direct.natl.yearly.u5$region
    direct.natl.nmr$region_num <- direct.natl.nmr$region
    direct.natl.yearly.nmr$region_num <- direct.natl.yearly.nmr$region
#if there is only one survey
}else{
      # 3-year estimates
     direct.natl.u5 <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), periods,
                                       regionVar = "admin1.char",
                                       timeVar = "period", 
                                       clusterVar =  "~cluster",
                                       ageVar = "age", Ntrials = "total",
                                       weightsVar = "v005",national.only = T)
     direct.natl.nmr <-  SUMMER::getDirect(as.data.frame(births.list.nmr[[1]]), periods,
                                         regionVar = "admin1.char",
                                         timeVar = "period", 
                                         clusterVar =  "~cluster",
                                         ageVar = "age", Ntrials = "total",
                                         weightsVar = "v005",national.only = T)
     # yearly estimates
    direct.natl.yearly.u5 <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), beg.year:end.year,
                                             regionVar = "admin1.char",
                                             timeVar = "years", 
                                             clusterVar =  "~cluster",
                                             ageVar = "age", Ntrials = "total",
                                             weightsVar = "v005",national.only = T)
    direct.natl.yearly.nmr <-  SUMMER::getDirect(as.data.frame(births.list.nmr[[1]]), beg.year:end.year,
                                                regionVar = "admin1.char",
                                                timeVar = "years", 
                                                clusterVar =  "~cluster",
                                                ageVar = "age", Ntrials = "total",
                                                weightsVar = "v005",national.only = T)
    
    direct.natl.u5$survey <- direct.natl.nmr$survey <- direct.natl.yearly.u5$survey <- direct.natl.yearly.nmr$survey <- 1
    direct.natl.u5$surveyYears <- direct.natl.nmr$surveyYears <- direct.natl.yearly.u5$surveyYears <- direct.natl.yearly.nmr$surveyYears <- survey_years[1]
    direct.natl.u5$region_num <- direct.natl.u5$region
    direct.natl.nmr$region_num <- direct.natl.nmr$region
    direct.natl.yearly.u5$region_num <- direct.natl.yearly.u5$region
    direct.natl.yearly.nmr$region_num <- direct.natl.yearly.nmr$region
    
  }

  # save national direct estimates
  save(direct.natl.u5, file = paste0('U5MR/',country, '_direct_natl_u5.rda'))
  save(direct.natl.yearly.u5, file = paste0('U5MR/',country, '_direct_natl_yearly_u5.rda'))
  save(direct.natl.nmr, file = paste0('NMR/',country, '_direct_natl_nmr.rda'))
  save(direct.natl.yearly.nmr, file = paste0('NMR/',country, '_direct_natl_yearly_nmr.rda'))
  
## Admin1 ------------------------------------------------------
  
  if(length(births.list) != 1){
     direct.admin1.u5 <-  SUMMER::getDirectList(births.list, periods,
                                           regionVar = "admin1.char",
                                           timeVar = "period", 
                                           clusterVar =  "~cluster",
                                           ageVar = "age", Ntrials = "total",
                                           weightsVar = "v005",national.only = F)
    direct.admin1.yearly.u5 <- SUMMER::getDirectList(births.list, beg.year:end.year,
                                                regionVar = "admin1.char",
                                                timeVar = "years", 
                                                clusterVar =  "~cluster",
                                                ageVar = "age", Ntrials = "total",
                                                weightsVar = "v005",national.only = F)
    direct.admin1.nmr <-  SUMMER::getDirectList(births.list.nmr, periods,
                                               regionVar = "admin1.char",
                                               timeVar = "period", 
                                               clusterVar =  "~cluster",
                                               ageVar = "age", Ntrials = "total",
                                               weightsVar = "v005",national.only = F)
    direct.admin1.yearly.nmr <- SUMMER::getDirectList(births.list.nmr, beg.year:end.year,
                                                     regionVar = "admin1.char",
                                                     timeVar = "years", 
                                                     clusterVar =  "~cluster",
                                                     ageVar = "age", Ntrials = "total",
                                                     weightsVar = "v005",national.only = F)
    
    direct.admin1.u5$region_num <- direct.admin1.u5$region
    direct.admin1.yearly.u5$region_num <- direct.admin1.yearly.u5$region
    direct.admin1.nmr$region_num <- direct.admin1.nmr$region
    direct.admin1.yearly.nmr$region_num <- direct.admin1.yearly.nmr$region
  }else{
     direct.admin1.u5 <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), periods,
                                       regionVar = "admin1.char",
                                       timeVar = "period", 
                                       clusterVar =  "~cluster",
                                       ageVar = "age", Ntrials = "total",
                                       weightsVar = "v005",national.only = F)
    direct.admin1.yearly.u5 <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), beg.year:end.year,
                                             regionVar = "admin1.char",
                                             timeVar = "years", 
                                             clusterVar =  "~cluster",
                                             ageVar = "age", Ntrials = "total",
                                             weightsVar = "v005",national.only = F)
    direct.admin1.nmr <-  SUMMER::getDirect(as.data.frame(births.list.nmr[[1]]), periods,
                                           regionVar = "admin1.char",
                                           timeVar = "period", 
                                           clusterVar =  "~cluster",
                                           ageVar = "age", Ntrials = "total",
                                           weightsVar = "v005",national.only = F)
    direct.admin1.yearly.nmr <-  SUMMER::getDirect(as.data.frame(births.list.nmr[[1]]), beg.year:end.year,
                                                  regionVar = "admin1.char",
                                                  timeVar = "years", 
                                                  clusterVar =  "~cluster",
                                                  ageVar = "age", Ntrials = "total",
                                                  weightsVar = "v005",national.only = F)
    
    direct.admin1.u5$survey <- direct.admin1.yearly.u5$survey <- direct.admin1.nmr$survey <- direct.admin1.yearly.nmr$survey <- 1
    direct.admin1.u5$surveyYears <- direct.admin1.yearly.u5$surveyYears <- direct.admin1.nmr$surveyYears <- direct.admin1.yearly.nmr$surveyYears <- survey_years[1]
    
    direct.admin1.u5$region_num <- direct.admin1.u5$region
    direct.admin1.yearly.u5$region_num <- direct.admin1.yearly.u5$region
    direct.admin1.nmr$region_num <- direct.admin1.nmr$region
    direct.admin1.yearly.nmr$region_num <- direct.admin1.yearly.nmr$region
    
  }
  
  save(direct.admin1.u5, file = paste0('U5MR/',country, '_direct_admin1_u5.rda'))
  save(direct.admin1.yearly.u5, file = paste0('U5MR/',country, '_direct_admin1_yearly_u5.rda'))
  save(direct.admin1.nmr, file = paste0('NMR/',country, '_direct_admin1_nmr.rda'))
  save(direct.admin1.yearly.nmr, file = paste0('NMR/',country, '_direct_admin1_yearly_nmr.rda'))
  
## Admin2  ------------------------------------------------------
  
  # compute 3-year direct estimates for at admin2 level. But this may fail due to the data sparsity at admin2 level.
  if(length(births.list) != 1){
    direct.admin2.u5 <-  SUMMER::getDirectList(births.list, periods,
                                               regionVar = "admin2.char",
                                               timeVar = "period", 
                                               clusterVar =  "~cluster",
                                               ageVar = "age", Ntrials = "total",
                                               weightsVar = "v005",national.only = F)
    direct.admin2.nmr <-  SUMMER::getDirectList(births.list.nmr, periods,
                                                regionVar = "admin2.char",
                                                timeVar = "period", 
                                                clusterVar =  "~cluster",
                                                ageVar = "age", Ntrials = "total",
                                                weightsVar = "v005",national.only = F)
    
    direct.admin2.u5$region_num <- direct.admin2.u5$region
    direct.admin2.nmr$region_num <- direct.admin2.nmr$region
  }else{
    direct.admin2.u5 <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), periods,
                                           regionVar = "admin2.char",
                                           timeVar = "period", 
                                           clusterVar =  "~cluster",
                                           ageVar = "age", Ntrials = "total",
                                           weightsVar = "v005",national.only = F)
    direct.admin2.nmr <-  SUMMER::getDirect(as.data.frame(births.list.nmr[[1]]), periods,
                                            regionVar = "admin2.char",
                                            timeVar = "period", 
                                            clusterVar =  "~cluster",
                                            ageVar = "age", Ntrials = "total",
                                            weightsVar = "v005",national.only = F)
    
    direct.admin2.u5$survey <- direct.admin2.nmr$survey <- 1
    direct.admin2.u5$surveyYears <- direct.admin2.nmr$surveyYears <- survey_years[1]
    
    direct.admin2.u5$region_num <- direct.admin2.u5$region
    direct.admin2.nmr$region_num <- direct.admin2.nmr$region
    
  }
  # compute yearly direct estimates for at admin2 level. But this may fail due to the data sparsity at admin2 level.
  if(length(births.list) != 1){
  direct.admin2.yearly.u5 <- SUMMER::getDirectList(births.list, beg.year:end.year,
                                                   regionVar = "admin2.char",
                                                   timeVar = "years", 
                                                   clusterVar =  "~cluster",
                                                   ageVar = "age", Ntrials = "total",
                                                   weightsVar = "v005",national.only = F)
  direct.admin2.yearly.nmr <- SUMMER::getDirectList(births.list.nmr, beg.year:end.year,
                                                    regionVar = "admin2.char",
                                                    timeVar = "years", 
                                                    clusterVar =  "~cluster",
                                                    ageVar = "age", Ntrials = "total",
                                                    weightsVar = "v005",national.only = F)
  direct.admin2.yearly.u5$region_num <- direct.admin2.yearly.u5$region
  direct.admin2.yearly.nmr$region_num <- direct.admin2.yearly.nmr$region
  }else{
    direct.admin2.yearly.u5 <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), beg.year:end.year,
                                                  regionVar = "admin2.char",
                                                  timeVar = "years", 
                                                  clusterVar =  "~cluster",
                                                  ageVar = "age", Ntrials = "total",
                                                  weightsVar = "v005",national.only = F)
    direct.admin2.yearly.nmr <-  SUMMER::getDirect(as.data.frame(births.list.nmr[[1]]), beg.year:end.year,
                                                   regionVar = "admin2.char",
                                                   timeVar = "years", 
                                                   clusterVar =  "~cluster",
                                                   ageVar = "age", Ntrials = "total",
                                                   weightsVar = "v005",national.only = F)
    
    direct.admin2.yearly.u5$survey <- direct.admin2.yearly.nmr$survey <- 1
    direct.admin2.yearly.u5$surveyYears <- direct.admin2.yearly.nmr$surveyYears <- survey_years[1]
    
    direct.admin2.yearly.u5$region_num <- direct.admin2.yearly.u5$region
    direct.admin2.yearly.nmr$region_num <- direct.admin2.yearly.nmr$region
  }
  
  save(direct.admin2.u5, file = paste0('U5MR/',country, '_direct_admin2_u5.rda'))
  save(direct.admin2.yearly.u5, file = paste0('U5MR/',country, '_direct_admin2_yearly_u5.rda'))
  save(direct.admin2.nmr, file = paste0('NMR/',country, '_direct_admin2_nmr.rda'))
  save(direct.admin2.yearly.nmr, file = paste0('NMR/',country, '_direct_admin2_yearly_nmr.rda'))
  
# Make HIV adjustment ------------------------------------------------------
  
  if(doHIVAdj){
    load(paste0(home.dir,'/Data/HIV/',
                'HIVAdjustments.rda'),
         envir = .GlobalEnv)
    hiv.adj <- hiv.adj[hiv.adj$country == country,]
    if(unique(hiv.adj$area)[1] == country){
      natl.unaids <- T
    }else{
      natl.unaids <- F
    }
    
    ## National Adjustment  ------------------------------------------------------
    for(survey in survey_years){
      if(natl.unaids){
        adj.frame <- hiv.adj[hiv.adj$survey == survey,]
        adj.varnames <- c("country", "years")
      }else{
        adj.frame <- hiv.adj[hiv.adj$survey == survey,]
        adj.frame <- aggregate(ratio ~ country + years,data = adj.frame, FUN = mean)
        adj.varnames <- c("country", "years")
      }
      
      # warning if HIV adjustment has not been calculated for that year
      if(nrow(adj.frame)==0){message(paste0('HIV Adjustment for ',country,' ',survey,' survey has not been calculated. Please calculate the HIV adjustment before proceeding.'))}
      
      # adjustment for yearly u5mr
      tmp.adj <- SUMMER::getAdjusted(direct.natl.yearly.u5[direct.natl.yearly.u5$surveyYears == survey,],
                                     ratio = adj.frame, 
                                     logit.lower = NULL,
                                     logit.upper = NULL,
                                     prob.upper = "upper",
                                     prob.lower = "lower")
      direct.natl.yearly.u5[direct.natl.yearly.u5$surveyYears == survey,] <- 
        tmp.adj[ ,  match(colnames(direct.natl.yearly.u5),
                          colnames(tmp.adj))]
      
      adj.frame.tmp <- adj.frame[adj.frame$years %in%  beg.period.years, ]
      adj.frame.tmp$years <- periods
      
      # adjustment for 3-year period u5mr
      tmp.adj <- SUMMER::getAdjusted(direct.natl.u5[direct.natl.u5$surveyYears == survey,],
                                     ratio = adj.frame.tmp, 
                                     logit.lower = NULL,
                                     logit.upper = NULL,
                                     prob.upper = "upper",
                                     prob.lower = "lower")
      direct.natl.u5[direct.natl.u5$surveyYears == survey,] <- 
        tmp.adj[ ,  match(colnames(direct.natl.u5),
                          colnames(tmp.adj))]
      
      # adjustment for yearly nmr
      tmp.adj <- SUMMER::getAdjusted(direct.natl.yearly.nmr[direct.natl.yearly.nmr$surveyYears == survey,],
                                     ratio = adj.frame, 
                                     logit.lower = NULL,
                                     logit.upper = NULL,
                                     prob.upper = "upper",
                                     prob.lower = "lower")
      direct.natl.yearly.nmr[direct.natl.yearly.nmr$surveyYears == survey,] <- 
        tmp.adj[ ,  match(colnames(direct.natl.yearly.nmr),
                          colnames(tmp.adj))]
      
      adj.frame.tmp <- adj.frame[adj.frame$years %in%  beg.period.years, ]
      adj.frame.tmp$years <- periods
      
      # adjustment for 3-year period nmr
      tmp.adj <- SUMMER::getAdjusted(direct.natl.nmr[direct.natl.nmr$surveyYears == survey,],
                                     ratio = adj.frame.tmp, 
                                     logit.lower = NULL,
                                     logit.upper = NULL,
                                     prob.upper = "upper",
                                     prob.lower = "lower")
      direct.natl.nmr[direct.natl.nmr$surveyYears == survey,] <- 
        tmp.adj[ ,  match(colnames(direct.natl.nmr),
                          colnames(tmp.adj))]
    }
    
    ## Admin 1 and 2 Adjustment ------------------------------------------------------
    for(survey in survey_years){
      if(natl.unaids){
        adj.frame <- hiv.adj[hiv.adj$survey == survey,]
        adj.varnames <- c("country", "years")
      }else{
        adj.frame <- hiv.adj[hiv.adj$survey == survey,]
        adj.varnames <- c("country", "region", "years")
      }
      
      for(area in admin1.names$GADM){
        if(natl.unaids){
          adj.frame.tmp <- adj.frame[adj.frame$years %in% 
                                       (beg.period.years+1), ]
          adj.frame.tmp$years <- periods
          
        }else{
          if(country == "Zambia" & area == "North-Western"){
            adj.frame$area[adj.frame$area == "Northwestern"] <- area
          }
          adj.frame.tmp <- adj.frame[adj.frame$area == area &
                                       adj.frame$years %in%
                                       beg.period.years+1, ]
          adj.frame.tmp$years <- periods
        }
        
        area.int <- admin1.names$Internal[match(area, admin1.names$GADM)]
        
        #adjustment for 3-year period U5MR
         tmp.adj <- SUMMER::getAdjusted(direct.admin1.u5[direct.admin1.u5$region == as.character(area.int) &
                                                        direct.admin1.u5$surveyYears == survey,],
                                        ratio = adj.frame.tmp, 
                                        logit.lower = NULL,
                                        logit.upper = NULL,
                                        prob.upper = "upper",
                                        prob.lower = "lower")
         direct.admin1.u5[direct.admin1.u5$region == as.character(area.int) &
                         direct.admin1.u5$surveyYears == survey,] <- 
           tmp.adj[ , match(colnames(direct.admin1.u5),
                            colnames(tmp.adj))]
        
         #adjustment for yearly U5MR
        tmp.adj <- SUMMER::getAdjusted(direct.admin1.yearly.u5[direct.admin1.yearly.u5$region == as.character(area.int) &
                                                       direct.admin1.yearly.u5$surveyYears == survey,],
                                       ratio = adj.frame, 
                                       logit.lower = NULL,
                                       logit.upper = NULL,
                                       prob.upper = "upper",
                                       prob.lower = "lower")
        direct.admin1.yearly.u5[direct.admin1.yearly.u5$region == as.character(area.int) &
                        direct.admin1.yearly.u5$surveyYears == survey,] <- 
          tmp.adj[ , match(colnames(direct.admin1.yearly.u5),
                           colnames(tmp.adj))]
        
        #adjustment for 3-year period NMR
        tmp.adj <- SUMMER::getAdjusted(direct.admin1.nmr[direct.admin1.nmr$region == as.character(area.int) &
                                                          direct.admin1.nmr$surveyYears == survey,],
                                       ratio = adj.frame.tmp, 
                                       logit.lower = NULL,
                                       logit.upper = NULL,
                                       prob.upper = "upper",
                                       prob.lower = "lower")
        direct.admin1.nmr[direct.admin1.nmr$region == as.character(area.int) &
                           direct.admin1.nmr$surveyYears == survey,] <- 
          tmp.adj[ , match(colnames(direct.admin1.nmr),
                           colnames(tmp.adj))]
        
        #adjustment for yearly NMR
        tmp.adj <- SUMMER::getAdjusted(direct.admin1.yearly.nmr[direct.admin1.yearly.nmr$region == as.character(area.int) &
                                                                 direct.admin1.yearly.nmr$surveyYears == survey,],
                                       ratio = adj.frame, 
                                       logit.lower = NULL,
                                       logit.upper = NULL,
                                       prob.upper = "upper",
                                       prob.lower = "lower")
        direct.admin1.yearly.nmr[direct.admin1.yearly.nmr$region == as.character(area.int) &
                                  direct.admin1.yearly.nmr$surveyYears == survey,] <- 
          tmp.adj[ , match(colnames(direct.admin1.yearly.nmr),
                           colnames(tmp.adj))]
        
        if(exists('direct.admin2.u5')){
          #adjustment for 3-year period U5MR
          admin2.to.admin1 <- births.list[[1]][!duplicated(births.list[[1]]$admin2.name),] %>% 
            dplyr::select(GADM.adm2=admin2.name,Internal.adm2=admin2.char,GADM.adm1 = admin1.name)
          admin2.to.admin1 <- data.frame(admin2.to.admin1,
                                         Internal.adm1 = NA)
          admin2.to.admin1$Internal.adm1 <- admin1.names$Internal[match(admin2.to.admin1$GADM.adm1,
                                                                        admin1.names$GADM)]
          admin2s <- admin2.to.admin1$Internal.adm2[admin2.to.admin1$Internal.adm1 == 
                                                      as.character(area.int)]
          tmp.adj <- SUMMER::getAdjusted(direct.admin2.u5[direct.admin2.u5$region %in% admin2s &
                                                         direct.admin2.u5$surveyYears == survey,],
                                         ratio = adj.frame.tmp, 
                                         logit.lower = NULL,
                                         logit.upper = NULL,
                                         prob.upper = "upper",
                                         prob.lower = "lower")
          direct.admin2.u5[direct.admin2.u5$region %in% admin2s &
                          direct.admin2.u5$surveyYears == survey,] <- 
            tmp.adj[ , match(colnames(direct.admin2.u5), 
                             colnames(tmp.adj))]
          
          #adjustment for yearly U5MR
          tmp.adj <- SUMMER::getAdjusted(direct.admin2.yearly.u5[direct.admin2.yearly.u5$region %in% admin2s &
                                                                   direct.admin2.yearly.u5$surveyYears == survey,],
                                         ratio = adj.frame, 
                                         logit.lower = NULL,
                                         logit.upper = NULL,
                                         prob.upper = "upper",
                                         prob.lower = "lower")
          direct.admin2.yearly.u5[direct.admin2.yearly.u5$region %in% admin2s &
                                    direct.admin2.yearly.u5$surveyYears == survey,] <- 
            tmp.adj[ , match(colnames(direct.admin2.yearly.u5),
                             colnames(tmp.adj))]
          
          #adjustment for 3-year period NMR
          admin2.to.admin1 <- births.list.nmr[[1]][!duplicated(births.list.nmr[[1]]$admin2.name),] %>% 
            dplyr::select(GADM.adm2=admin2.name,Internal.adm2=admin2.char,GADM.adm1 = admin1.name)
          admin2.to.admin1 <- data.frame(admin2.to.admin1,
                                         Internal.adm1 = NA)
          admin2.to.admin1$Internal.adm1 <- admin1.names$Internal[match(admin2.to.admin1$GADM.adm1,
                                                                        admin1.names$GADM)]
          admin2s <- admin2.to.admin1$Internal.adm2[admin2.to.admin1$Internal.adm1 == 
                                                      as.character(area.int)]
          tmp.adj <- SUMMER::getAdjusted(direct.admin2.nmr[direct.admin2.nmr$region %in% admin2s &
                                                            direct.admin2.nmr$surveyYears == survey,],
                                         ratio = adj.frame.tmp, 
                                         logit.lower = NULL,
                                         logit.upper = NULL,
                                         prob.upper = "upper",
                                         prob.lower = "lower")
          direct.admin2.nmr[direct.admin2.nmr$region %in% admin2s &
                             direct.admin2.nmr$surveyYears == survey,] <- 
            tmp.adj[ , match(colnames(direct.admin2.nmr), 
                             colnames(tmp.adj))]
          
          #adjustment for yearly NMR
          tmp.adj <- SUMMER::getAdjusted(direct.admin2.yearly.nmr[direct.admin2.yearly.nmr$region %in% admin2s &
                                                                   direct.admin2.yearly.nmr$surveyYears == survey,],
                                         ratio = adj.frame, 
                                         logit.lower = NULL,
                                         logit.upper = NULL,
                                         prob.upper = "upper",
                                         prob.lower = "lower")
          direct.admin2.yearly.nmr[direct.admin2.yearly.nmr$region %in% admin2s &
                                    direct.admin2.yearly.nmr$surveyYears == survey,] <- 
            tmp.adj[ , match(colnames(direct.admin2.yearly.nmr),
                             colnames(tmp.adj))]
        }
      }
    }
    
    save(direct.natl.u5, file = paste0('U5MR/',country, '_directHIV_natl_u5.rda'))
    save(direct.natl.yearly.u5, file = paste0('U5MR/',country, '_directHIV_natl_yearly_u5.rda'))
    save(direct.admin1.u5, file = paste0('U5MR/',country, '_directHIV_admin1_u5.rda'))
    save(direct.admin1.yearly.u5, file = paste0('U5MR/',country, '_directHIV_admin1_yearly_u5.rda'))
    if(exists('direct.admin2.u5')){
      save(direct.admin2.u5, file = paste0('U5MR/',country, '_directHIV_admin2_u5.rda'))
      save(direct.admin2.yearly.u5, file = paste0('U5MR/',country, '_directHIV_admin2_yearly_u5.rda'))
    }
    save(direct.natl.nmr, file = paste0('NMR/',country, '_directHIV_natl_nmr.rda'))
    save(direct.natl.yearly.nmr, file = paste0('NMR/',country, '_directHIV_natl_yearly_nmr.rda'))
    save(direct.admin1.nmr, file = paste0('NMR/',country, '_directHIV_admin1_nmr.rda'))
    save(direct.admin1.yearly.nmr, file = paste0('NMR/',country, '_directHIV_admin1_yearly_nmr.rda'))
    if(exists('direct.admin2.nmr')){
      save(direct.admin2.nmr, file = paste0('NMR/',country, '_directHIV_admin2_nmr.rda'))
      save(direct.admin2.yearly.nmr, file = paste0('NMR/',country, '_directHIV_admin2_yearly_nmr.rda'))
    }
    
  }
  
  

# Smoothed direct estimates  ------------------------------------------------------

## load in appropriate direct estimates  ------------------------------------------------------
if(doHIVAdj){
  load(paste0('U5MR/',country, '_directHIV_natl_u5.rda'))
  load(paste0('U5MR/',country, '_directHIV_natl_yearly_u5.rda'))
  load(paste0('U5MR/',country, '_directHIV_admin1_u5.rda'))
  load(paste0('U5MR/',country, '_directHIV_admin1_yearly_u5.rda'))
  load(paste0('U5MR/',country, '_directHIV_admin2_u5.rda'))
  
  load(paste0('NMR/',country, '_directHIV_natl_nmr.rda'))
  load(paste0('NMR/',country, '_directHIV_natl_yearly_nmr.rda'))
  load(paste0('NMR/',country, '_directHIV_admin1_nmr.rda'))
  load(paste0('NMR/',country, '_directHIV_admin1_yearly_nmr.rda'))
  load(paste0('NMR/',country, '_directHIV_admin2_nmr.rda'))
}else{
  load(paste0('U5MR/',country, '_direct_natl_u5.rda'))
  load(paste0('U5MR/',country, '_direct_natl_yearly_u5.rda'))
  load(paste0('U5MR/',country, '_direct_admin1_u5.rda'))
  load(paste0('U5MR/',country, '_direct_admin1_yearly_u5.rda'))
  load(paste0('U5MR/',country, '_direct_admin2_u5.rda'))
  
  load(paste0('NMR/',country, '_direct_natl_nmr.rda'))
  load(paste0('NMR/',country, '_direct_natl_yearly_nmr.rda'))
  load(paste0('NMR/',country, '_direct_admin1_nmr.rda'))
  load(paste0('NMR/',country, '_direct_admin1_yearly_nmr.rda'))
  load(paste0('NMR/',country, '_direct_admin2_nmr.rda'))
}
  
## aggregate surveys  ------------------------------------------------------
data.natl.u5 <- SUMMER::aggregateSurvey(direct.natl.u5)
data.natl.yearly.u5 <- SUMMER::aggregateSurvey(direct.natl.yearly.u5)
data.admin1.u5 <- SUMMER::aggregateSurvey(direct.admin1.u5)
data.admin1.yearly.u5 <- SUMMER::aggregateSurvey(direct.admin1.yearly.u5)
data.admin2.u5 <- SUMMER::aggregateSurvey(direct.admin2.u5)
data.admin2.yearly.u5 <- SUMMER::aggregateSurvey(direct.admin2.yearly.u5)

data.natl.nmr <- SUMMER::aggregateSurvey(direct.natl.nmr)
data.natl.yearly.nmr <- SUMMER::aggregateSurvey(direct.natl.yearly.nmr)
data.admin1.nmr <- SUMMER::aggregateSurvey(direct.admin1.nmr)
data.admin1.yearly.nmr <- SUMMER::aggregateSurvey(direct.admin1.yearly.nmr)
data.admin2.nmr <- SUMMER::aggregateSurvey(direct.admin2.nmr)
data.admin2.yearly.nmr <- SUMMER::aggregateSurvey(direct.admin2.yearly.nmr)

## extend periods to include projected years  ------------------------------------------------------
beg.proj.years <- seq(end.year+1,end.proj.year,3)
end.proj.years <- beg.proj.years+2
proj.per <- paste(beg.proj.years, end.proj.years, sep = "-") # add the 3-year period to be projected

## full time period (including projected years)
periods.survey <- periods
periods <- c(periods,proj.per)

## National, 3-year period ------------------------------------------------------

## U5MR
fit.natl.u5 <- smoothDirect(data.natl.u5, Amat = NULL, # national level model doesn't need to specify adjacency matrix since it would just be 1.
                     year_label = c(periods),
                     year_range = c(beg.year, max(end.proj.years)),
                     control.inla = list(strategy = "adaptive", int.strategy = "eb"), is.yearly = F) # fit the smoothed direct model. Changing the year label and year range can change the years the estimators to be computed, 
## even for future years where DHS data is not yet available. But this would lead to less accurate estimates and larger uncertainty level.

 res.natl.u5 <- getSmoothed(fit.natl.u5,year_range = c(beg.year, max(end.proj.years)),
                         year_label = periods) # sample for smoothed direct estimates
 
 res.natl.u5$years.num <- seq(beg.year,max(end.proj.years),3)
 res.natl.u5$region.gadm <- country
 save(res.natl.u5, file = paste0('U5MR/',country, "_res_natl_u5_SmoothedDirect.rda")) # save the national 3-year smoothed direct U5MR
 
 ## NMR
 fit.natl.nmr <- smoothDirect(data.natl.nmr, geo = NULL, Amat = NULL, # national level model doesn't need to specify adjacency matrix since it would just be 1.
                             year_label = c(periods),
                             year_range = c(beg.year, max(end.proj.years)),
                             control.inla = list(strategy = "adaptive", int.strategy = "eb"),
                             is.yearly = F) # fit the smoothed direct model. Changing the year label and year range can change the years the estimators to be computed, 
 ## even for future years where DHS data is not yet available. But this would lead to less accurate estimates and larger uncertainty level.
 
 res.natl.nmr <- getSmoothed(fit.natl.nmr,year_range = c(beg.year, max(end.proj.years)),
                            year_label = periods) # sample for smoothed direct estimates
 
 res.natl.nmr$years.num <- seq(beg.year,max(end.proj.years),3)
 res.natl.nmr$region.gadm <- country
 save(res.natl.nmr, file = paste0('NMR/',country, "_res_natl_nmr_SmoothedDirect.rda"))
 

## National, yearly  ------------------------------------------------------
  # include 3 years after last survey
 #U5MR
fit.natl.yearly.u5 <- smoothDirect(data.natl.yearly.u5, geo = NULL, Amat = NULL,
                           year_label = as.character(beg.year:max(end.proj.years)),
                           year_range = c(beg.year, max(end.proj.years)), 
                           control.inla = list(strategy = "adaptive", int.strategy = "eb"),  is.yearly = F)
res.natl.yearly.u5 <- getSmoothed(fit.natl.yearly.u5, year_range = c(beg.year, max(end.proj.years)),
                               year_label = as.character(beg.year:max(end.proj.years)))
res.natl.yearly.u5$years.num <- beg.year:max(end.proj.years)
res.natl.yearly.u5$region.gadm <- country
save(res.natl.yearly.u5, file = paste0('U5MR/',country, "_res_natl_yearly_u5_SmoothedDirect.rda")) # save the national yearly smoothed direct U5MR

#NMR
fit.natl.yearly.nmr <- smoothDirect(data.natl.yearly.nmr, geo = NULL, Amat = NULL,
                                   year_label = as.character(beg.year:max(end.proj.years)),
                                   year_range = c(beg.year, max(end.proj.years)), 
                                   control.inla = list(strategy = "adaptive", int.strategy = "eb"), is.yearly = F)
res.natl.yearly.nmr <- getSmoothed(fit.natl.yearly.nmr, year_range = c(beg.year, max(end.proj.years)),
                                  year_label = as.character(beg.year:max(end.proj.years)))
res.natl.yearly.nmr$years.num <- beg.year:max(end.proj.years)
res.natl.yearly.nmr$region.gadm <- country
save(res.natl.yearly.nmr, file = paste0('NMR/',country, "_res_natl_yearly_nmr_SmoothedDirect.rda")) # save the national yearly smoothed direct U5MR


## Admin 1, 3-year period  ------------------------------------------------------

## U5MR
data.admin1.u5 <- data.admin1.u5[data.admin1.u5$region!='All',] # direct.admin1 is a matrix containing all national and admin1 level estimates. Only admin1 estimates are interested here.
fit.admin1.u5 <- smoothDirect(data.admin1.u5, Amat = admin1.mat,
                      year_label = periods, type.st = 4,
                      year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
res.admin1.u5 <- getSmoothed(fit.admin1.u5, Amat = admin1.mat,
                             year_label = periods,
                             year_range = c(beg.year, max(end.proj.years)),
                             save.draws = TRUE)

res.admin1.u5$years.num <- seq(beg.year,max(end.proj.years),3)[match(res.admin1.u5$years, periods)]
res.admin1.u5$region.gadm <- admin1.names$GADM[match(res.admin1.u5$region, admin1.names$Internal)]

save(res.admin1.u5, file = paste0('U5MR/',country, "_res_admin1_u5_SmoothedDirect.rda"))# save the admin1 3-year smoothed direct U5MR

## NMR
data.admin1.nmr <- data.admin1.nmr[data.admin1.nmr$region!='All',] # direct.admin1 is a matrix containing all national and admin1 level estimates. Only admin1 estimates are interested here.
fit.admin1.nmr <- smoothDirect(data.admin1.nmr, Amat = admin1.mat,
                              year_label = periods, type.st = 4,
                              year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
res.admin1.nmr <- getSmoothed(fit.admin1.nmr, Amat = admin1.mat,
                                year_label = periods,
                                year_range = c(beg.year, max(end.proj.years)),
                                save.draws = TRUE)

res.admin1.nmr$years.num <- seq(beg.year,max(end.proj.years),3)[match(res.admin1.nmr$years, periods)]
res.admin1.nmr$region.gadm <- admin1.names$GADM[match(res.admin1.nmr$region, admin1.names$Internal)]

save(res.admin1.nmr, file = paste0('NMR/',country, "_res_admin1_nmr_SmoothedDirect.rda"))# save the admin1 3-year smoothed direct U5MR


## Admin 1, yearly  ------------------------------------------------------

##U5MR
data.admin1.yearly.u5 <- data.admin1.yearly.u5[data.admin1.yearly.u5$region!='All',]
fit.admin1.yearly.u5 <- smoothDirect(data.admin1.yearly.u5, Amat = admin1.mat,
                             year_label = as.character(beg.year:max(end.proj.years)),type.st = 4,
                             year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
sd.admin1.yearly.u5 <- getSmoothed(fit.admin1.yearly.u5, Amat = admin1.mat,
                                   year_label = as.character(beg.year:max(end.proj.years)),
                                   year_range = c(beg.year, max(end.proj.years)),
                                   save.draws = TRUE)
sd.admin1.yearly.u5$region.gadm <- admin1.names$GADM[match(sd.admin1.yearly.u5$region, admin1.names$Internal)]

save(sd.admin1.yearly.u5, file = paste0('U5MR/',country, "_res_admin1_u5_SmoothedDirect_yearly.rda")) # save the admin1 yearly smoothed direct U5MR

##NMR
data.admin1.yearly.nmr <- data.admin1.yearly.nmr[data.admin1.yearly.nmr$region!='All',]
fit.admin1.yearly.nmr <- smoothDirect(data.admin1.yearly.nmr, Amat = admin1.mat,
                                     year_label = as.character(beg.year:max(end.proj.years)),type.st = 4,
                                     year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
sd.admin1.yearly.nmr <- getSmoothed(fit.admin1.yearly.nmr, Amat = admin1.mat,
                                      year_label = as.character(beg.year:max(end.proj.years)),
                                      year_range = c(beg.year, max(end.proj.years)),
                                      save.draws = TRUE)
sd.admin1.yearly.nmr$region.gadm <- admin1.names$GADM[match(sd.admin1.yearly.nmr$region, admin1.names$Internal)]

save(sd.admin1.yearly.nmr, file = paste0('NMR/',country, "_res_admin1_nmr_SmoothedDirect_yearly.rda")) # save the admin1 yearly smoothed direct U5MR


## Admin 2, 3-year period ------------------------------------------------------

## U5MR
data.admin2.u5 <- data.admin2.u5[data.admin2.u5$region!='All',] # direct.admin1 is a matrix containing all national and admin1 level estimates. Only admin1 estimates are interested here.
fit.admin2.u5 <- smoothDirect(data.admin2.u5, Amat = admin2.mat,
                      year_label = periods,type.st = 4,
                      year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
res.admin2.u5 <- getSmoothed(fit.admin2.u5, Amat = admin2.mat,
                             year_label = periods,
                             year_range = c(beg.year, max(end.proj.years)),
                             save.draws = TRUE)

res.admin2.u5$years.num <- seq(beg.year,max(end.proj.years),3)[match(res.admin2.u5$years, periods)]
res.admin2.u5$region.gadm <- admin2.names$GADM[match(res.admin2.u5$region, admin2.names$Internal)]

save(res.admin2.u5, file = paste0('U5MR/',country, "_res_admin2_u5_SmoothedDirect.rda"))

## NMR
data.admin2.nmr <- data.admin2.nmr[data.admin2.nmr$region!='All',] # direct.admin1 is a matrix containing all national and admin1 level estimates. Only admin1 estimates are interested here.
fit.admin2.nmr <- smoothDirect(data.admin2.nmr, Amat = admin2.mat,
                              year_label = periods,type.st = 4,
                              year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
res.admin2.nmr <- getSmoothed(fit.admin2.nmr, Amat = admin2.mat,
                                year_label = periods,
                                year_range = c(beg.year, max(end.proj.years)),
                                save.draws = TRUE)

res.admin2.nmr$years.num <- seq(beg.year,max(end.proj.years),3)[match(res.admin2.nmr$years, periods)]
res.admin2.nmr$region.gadm <- admin2.names$GADM[match(res.admin2.nmr$region, admin2.names$Internal)]

save(res.admin2.nmr, file = paste0('NMR/',country, "_res_admin2_nmr_SmoothedDirect.rda"))


## Admin 2, yearly  ------------------------------------------------------

##U5MR
data.admin2.yearly.u5 <- data.admin2.yearly.u5[data.admin2.yearly.u5$region!='All',]
fit.admin2.yearly.u5 <- smoothDirect(data.admin2.yearly.u5, Amat = admin2.mat,
                                     year_label = as.character(beg.year:max(end.proj.years)),type.st = 4,
                                     year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
sd.admin2.yearly.u5 <- getSmoothed(fit.admin2.yearly.u5, Amat = admin2.mat,
                                   year_label = as.character(beg.year:max(end.proj.years)),
                                   year_range = c(beg.year, max(end.proj.years)),
                                   save.draws = TRUE)
sd.admin2.yearly.u5$region.gadm <- admin2.names$GADM[match(sd.admin2.yearly.u5$region, admin2.names$Internal)]

save(sd.admin2.yearly.u5, file = paste0('U5MR/',country, "_res_admin2_u5_SmoothedDirect_yearly.rda")) # save the admin2 yearly smoothed direct U5MR

##NMR
data.admin2.yearly.nmr <- data.admin2.yearly.nmr[data.admin2.yearly.nmr$region!='All',]
fit.admin2.yearly.nmr <- smoothDirect(data.admin2.yearly.nmr, Amat = admin2.mat,
                                      year_label = as.character(beg.year:max(end.proj.years)),type.st = 4,
                                      year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
sd.admin2.yearly.nmr <- getSmoothed(fit.admin2.yearly.nmr, Amat = admin2.mat,
                                    year_label = as.character(beg.year:max(end.proj.years)),
                                    year_range = c(beg.year, max(end.proj.years)),
                                    save.draws = TRUE)
sd.admin2.yearly.nmr$region.gadm <- admin2.names$GADM[match(sd.admin2.yearly.nmr$region, admin2.names$Internal)]

save(sd.admin2.yearly.nmr, file = paste0('NMR/',country, "_res_admin2_nmr_SmoothedDirect_yearly.rda")) # save the admin2 yearly smoothed direct U5MR

# Polygon plots ------------------------------------------------------
setwd(res.dir)
## Admin 1 Direct, by survey  ------------------------------------------------------

for(plotyears in periods.survey){
  
  # U5MR
  tmp <- direct.admin1.u5[direct.admin1.u5$years == paste(plotyears),]
  tmp$regionPlot <- admin1.names$GADM[match(tmp$region, admin1.names$Internal)]
  
  pdf(paste0("Figures/Direct/U5MR/Admin1/",
             country, 
             "_admin1_u5_direct_poly_bySurvey_",
             plotyears, ".pdf"),
      width = 3.5, height = 3.5)
  {
    print(SUMMER::mapPlot(data = tmp,
                          is.long = T, 
                          variables = "surveyYears", 
                          values = "mean",direction = -1,
                          geo = poly.adm1, ncol = 2,
                          legend.label = "U5MR",
                          per1000 = TRUE,
                          by.data = "regionPlot",
                          by.geo = paste0(sub(".*data[$]","",poly.label.adm1))))
  }
  dev.off()
  
  # NMR
  tmp <- direct.admin1.nmr[direct.admin1.nmr$years == paste(plotyears),]
  tmp$regionPlot <- admin1.names$GADM[match(tmp$region, admin1.names$Internal)]
  
  pdf(paste0("Figures/Direct/NMR/Admin1/",
             country, 
             "_admin1_nmr_direct_poly_bySurvey_",
             plotyears, ".pdf"),
      width = 3.5, height = 3.5)
  {
    print(SUMMER::mapPlot(data = tmp,
                          is.long = T, 
                          variables = "surveyYears", 
                          values = "mean",direction = -1,
                          geo = poly.adm1, ncol = 2,
                          legend.label = "NMR",
                          per1000 = TRUE,
                          by.data = "regionPlot",
                          by.geo = paste0(sub(".*data[$]","",poly.label.adm1))))
  }
  dev.off()
}

## Admin 1 Direct, aggregated  ------------------------------------------------------

## U5MR
plotagg.admin1.u5 <- aggregateSurvey(direct.admin1.u5)
plotagg.admin1.u5$regionPlot <- admin1.names$GADM[match(plotagg.admin1.u5$region,
                                                     admin1.names$Internal)]
pdf(paste0("Figures/Direct/U5MR/Admin1/",
           country,
           "_admin1_direct_u5_poly_Meta.pdf"))
{
  print(SUMMER::mapPlot(data = plotagg.admin1.u5,
                        is.long = T, 
                        variables = "years", 
                        values = "mean",
                        direction = -1,
                        geo = poly.adm1,
                        ncol = 3,
                        legend.label = "U5MR",
                        per1000 = TRUE,
                        by.data = "regionPlot",
                        by.geo = sub(".*data[$]","",poly.label.adm1)))
}
dev.off()

## NMR
plotagg.admin1.nmr <- aggregateSurvey(direct.admin1.nmr)
plotagg.admin1.nmr$regionPlot <- admin1.names$GADM[match(plotagg.admin1.nmr$region,
                                                        admin1.names$Internal)]
pdf(paste0("Figures/Direct/NMR/Admin1/",
           country,
           "_admin1_direct_nmr_poly_Meta.pdf"))
{
  print(SUMMER::mapPlot(data = plotagg.admin1.nmr,
                        is.long = T, 
                        variables = "years", 
                        values = "mean",
                        direction = -1,
                        geo = poly.adm1,
                        ncol = 3,
                        legend.label = "NMR",
                        per1000 = TRUE,
                        by.data = "regionPlot",
                        by.geo = sub(".*data[$]","",poly.label.adm1)))
}
dev.off()


## Admin 1 Smoothed Direct  ------------------------------------------------------

## U5MR
pdf(paste0("Figures/SmoothedDirect/U5MR/",
           country,
           '_admin1_u5_SmoothedDirect_poly.pdf'))
{
  print(SUMMER::mapPlot(data = res.admin1.u5$results,
                        is.long = T, 
                        variables = "years", 
                        values = "median",
                        direction = -1,
                        geo = poly.adm1,
                        ncol = 3,
                        legend.label = "U5MR",
                        per1000 = TRUE,
                        by.data = "region.gadm",
                        by.geo = sub(".*data[$]","",poly.label.adm1)))
  
}
dev.off()

## NMR
pdf(paste0("Figures/SmoothedDirect/NMR/",
           country,
           '_admin1_nmr_SmoothedDirect_poly.pdf'))
{
  print(SUMMER::mapPlot(data = res.admin1.nmr$results,
                        is.long = T, 
                        variables = "years", 
                        values = "median",
                        direction = -1,
                        geo = poly.adm1,
                        ncol = 3,
                        legend.label = "NMR",
                        per1000 = TRUE,
                        by.data = "region.gadm",
                        by.geo = sub(".*data[$]","",poly.label.adm1)))
  
}
dev.off()


## Admin 2 Direct, by survey  ------------------------------------------------------
for(plotyears in periods.survey){
  
  ## U5MR
  tmp <- direct.admin2.u5[direct.admin2.u5$years == paste(plotyears),]
  tmp$regionPlot <- admin2.names$GADM[match(tmp$region, admin2.names$Internal)]
  
  pdf(paste0("Figures/Direct/U5MR/Admin2/",
             country, 
             "_admin2_u5_direct_poly_bySurvey_",
             plotyears, ".pdf"),
      width = 3.5, height = 3.5)
  {
    print(SUMMER::mapPlot(data = tmp,
                          is.long = T, 
                          variables = "surveyYears", 
                          values = "mean",direction = -1,
                          geo = poly.adm2, ncol = 2,
                          legend.label = "U5MR",
                          per1000 = TRUE,
                          by.data = "regionPlot",
                          #changed for Malawi (generalise later)
                          by.geo = sub(".*data[$]","",poly.label.adm2)))
  }
  dev.off()
  
  ## NMR
  tmp <- direct.admin2.nmr[direct.admin2.nmr$years == paste(plotyears),]
  tmp$regionPlot <- admin2.names$GADM[match(tmp$region, admin2.names$Internal)]
  
  pdf(paste0("Figures/Direct/NMR/Admin2/",
             country, 
             "_admin2_nmr_direct_poly_bySurvey_",
             plotyears, ".pdf"),
      width = 3.5, height = 3.5)
  {
    print(SUMMER::mapPlot(data = tmp,
                          is.long = T, 
                          variables = "surveyYears", 
                          values = "mean",direction = -1,
                          geo = poly.adm2, ncol = 2,
                          legend.label = "NMR",
                          per1000 = TRUE,
                          by.data = "regionPlot",
                          by.geo = sub(".*data[$]","",poly.label.adm2)))
  }
  dev.off()
}

## Admin 2 Direct, aggregated  ------------------------------------------------------

## U5MR
plotagg.admin2.u5 <- aggregateSurvey(direct.admin2.u5)
plotagg.admin2.u5$regionPlot <- admin2.names$GADM[match(plotagg.admin2.u5$region,
                                                     admin2.names$Internal)]
pdf(paste0("Figures/Direct/U5MR/Admin2/",
           country,
           "_admin2_u5_direct_poly_Meta.pdf"))
{
  print(SUMMER::mapPlot(data = plotagg.admin2.u5,
                        is.long = T, 
                        variables = "years", 
                        values = "mean",
                        direction = -1,
                        geo = poly.adm2,
                        ncol = 3,
                        legend.label = "U5MR",
                        per1000 = TRUE,
                        by.data = "regionPlot",
                        by.geo =sub(".*data[$]","",poly.label.adm2)))
}
dev.off()

## NMR
plotagg.admin2.nmr <- aggregateSurvey(direct.admin2.nmr)
plotagg.admin2.nmr$regionPlot <- admin2.names$GADM[match(plotagg.admin2.nmr$region,
                                                        admin2.names$Internal)]
pdf(paste0("Figures/Direct/NMR/Admin2/",
           country,
           "_admin2_nmr_direct_poly_Meta.pdf"))
{
  print(SUMMER::mapPlot(data = plotagg.admin2.nmr,
                        is.long = T, 
                        variables = "years", 
                        values = "mean",
                        direction = -1,
                        geo = poly.adm2,
                        ncol = 3,
                        legend.label = "NMR",
                        per1000 = TRUE,
                        by.data = "regionPlot",
                        by.geo =sub(".*data[$]","",poly.label.adm2)))
}
dev.off()

## Admin 2 Smoothed Direct  ------------------------------------------------------

## U5MR
pdf(paste0("Figures/SmoothedDirect/U5MR/",
           country,
           '_admin2_u5_', 
           'SmoothedDirect_poly.pdf'))
{
  print(SUMMER::mapPlot(data = res.admin2.u5$results,
                        is.long = T, 
                        variables = "years", 
                        values = "median",
                        direction = -1,
                        geo = poly.adm2,
                        ncol = 3,
                        legend.label = "U5MR",
                        per1000 = TRUE,
                        by.data = "region.gadm",
                        by.geo = sub(".*data[$]","",poly.label.adm2)))
  
}
dev.off()

## NMR
pdf(paste0("Figures/SmoothedDirect/NMR/",
           country,
           '_admin2_nmr_', 
           'SmoothedDirect_poly.pdf'))
{
  print(SUMMER::mapPlot(data = res.admin2.nmr$results,
                        is.long = T, 
                        variables = "years", 
                        values = "median",
                        direction = -1,
                        geo = poly.adm2,
                        ncol = 3,
                        legend.label = "NMR",
                        per1000 = TRUE,
                        by.data = "region.gadm",
                        by.geo = sub(".*data[$]","",poly.label.adm2)))
  
}
dev.off()

# Spaghetti plots ------------------------------------------------------

## Load IGME estimates ------------------------------------------------------
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


## National, 3-year period ------------------------------------------------------
setwd(res.dir)
cols <- rainbow(length(survey_years))
pane.years <- jitter(end.period.years)

## U5MR
direct.natl.u5$width <- direct.natl.u5$upper - direct.natl.u5$lower
direct.natl.u5$cex2 <- median(direct.natl.u5$width, na.rm = T)/direct.natl.u5$width
direct.natl.u5$cex2[direct.natl.u5$cex2 > 6] <- 6

if(dim(direct.natl.u5)[1] != 0 &
   !(sum(is.na(direct.natl.u5$mean)) == nrow(direct.natl.u5))){
  plot.max <- max(res.natl.u5$upper+.025, na.rm = T)
}else{plot.max <- 0.25}

pdf(paste0("Figures/SmoothedDirect/U5MR/",
           country, 
           '_natl_u5_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1))
  if(nrow(direct.natl.u5) > 0 & sum(is.na(direct.natl.u5$mean)) == nrow(direct.natl.u5)){
    plot(NA,
         xlab = "Year",
         ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, max(end.proj.years)),
         type = 'l',
         col = cols[svy.idx],
         lwd = 2,
         main = country)
    
    legend('topright',
           bty = 'n',
           col = c(cols, 'grey37', 'black'),
           lwd = 2, lty = 1,
           legend = c(survey_years,"UN IGME", "Smoothed"))
    
  }else{
    for(survey in survey_years){
      tmp <- direct.natl.u5[direct.natl.u5$surveyYears == survey,]
      svy.idx <- match(survey, survey_years) 
      
      if(svy.idx==1){
        plot(NA, xlab = "Year", ylab = "U5MR",
             ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
             type = 'l', col = cols[svy.idx], lwd = 2, main = country)
        
        lines(pane.years, tmp$mean, cex = tmp$cex2,
              type = 'l',  col = adjustcolor(cols[svy.idx], 0.35), lwd = 2)
        
        points(pane.years, tmp$mean, pch = 19,
               col = adjustcolor(cols[svy.idx], 0.35),
               cex = tmp$cex2)
        
        #add IGME reference lines
        igme.years <- jitter(beg.year:max(igme.ests.u5$year))
        lines(igme.years,
              igme.ests.u5$OBS_VALUE,
              lwd = 2, col  = 'grey37')
        lines(igme.years,
              igme.ests.u5$UPPER_BOUND,
              lty = 2, col  = 'grey37')
        lines(igme.years,
              igme.ests.u5$LOWER_BOUND, 
              lty = 2, col  = 'grey37')
        
      }else{
        lines(pane.years, tmp$mean, cex = tmp$cex2,
              type = 'l',  col = adjustcolor(cols[svy.idx], 0.35), lwd = 2)
        points(pane.years, tmp$mean, pch = 19, 
               col = adjustcolor(cols[svy.idx], 0.35),
               cex = tmp$cex2)
      }
      
    }
  }
  lines(res.natl.u5$years.num, res.natl.u5$median,
        col = 'black', lwd = 2)
  lines(res.natl.u5$years.num, res.natl.u5$upper,
        col = 'black', lty = 2)
  lines(res.natl.u5$years.num,res.natl.u5$lower, 
        col = 'black', lty = 2)
  legend('topright', bty = 'n',
         col = c(cols, 'grey37','black'),
         lwd = 2, legend = c(survey_years,'UN IGME', "Smoothed"))
  
}
dev.off()

## NMR
direct.natl.nmr$width <- direct.natl.nmr$upper - direct.natl.nmr$lower
direct.natl.nmr$cex2 <- median(direct.natl.nmr$width, na.rm = T)/direct.natl.nmr$width
direct.natl.nmr$cex2[direct.natl.nmr$cex2 > 6] <- 6

if(dim(direct.natl.nmr)[1] != 0 &
   !(sum(is.na(direct.natl.nmr$mean)) == nrow(direct.natl.nmr))){
  plot.max <- max(res.natl.nmr$upper+.025, na.rm = T)
}else{plot.max <- 0.25}

pdf(paste0("Figures/SmoothedDirect/NMR/",
           country, 
           '_natl_nmr_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1))
  if(nrow(direct.natl.nmr) > 0 & sum(is.na(direct.natl.nmr$mean)) == nrow(direct.natl.nmr)){
    plot(NA,
         xlab = "Year",
         ylab = "NMR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, max(end.proj.years)),
         type = 'l',
         col = cols[svy.idx],
         lwd = 2,
         main = country)
    
    legend('topright',
           bty = 'n',
           col = c(cols, 'grey37', 'black'),
           lwd = 2, lty = 1,
           legend = c(survey_years,"UN IGME", "Smoothed"))
    
  }else{
    for(survey in survey_years){
      tmp <- direct.natl.nmr[direct.natl.nmr$surveyYears == survey,]
      svy.idx <- match(survey, survey_years) 
      
      if(svy.idx==1){
        plot(NA, xlab = "Year", ylab = "NMR",
             ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
             type = 'l', col = cols[svy.idx], lwd = 2, main = country)
        
        lines(pane.years, tmp$mean, cex = tmp$cex2,
              type = 'l',  col = adjustcolor(cols[svy.idx], 0.35), lwd = 2)
        
        points(pane.years, tmp$mean, pch = 19,
               col = adjustcolor(cols[svy.idx], 0.35),
               cex = tmp$cex2)
        
        #add IGME reference lines
        igme.years <- jitter(beg.year:max(igme.ests.nmr$year))
        lines(igme.years,
              igme.ests.nmr$OBS_VALUE,
              lwd = 2, col  = 'grey37')
        lines(igme.years,
              igme.ests.nmr$UPPER_BOUND,
              lty = 2, col  = 'grey37')
        lines(igme.years,
              igme.ests.nmr$LOWER_BOUND, 
              lty = 2, col  = 'grey37')
        
      }else{
        lines(pane.years, tmp$mean, cex = tmp$cex2,
              type = 'l',  col = adjustcolor(cols[svy.idx], 0.35), lwd = 2)
        points(pane.years, tmp$mean, pch = 19, 
               col = adjustcolor(cols[svy.idx], 0.35),
               cex = tmp$cex2)
      }
      
    }
  }
  lines(res.natl.nmr$years.num, res.natl.nmr$median,
        col = 'black', lwd = 2)
  lines(res.natl.nmr$years.num, res.natl.nmr$upper,
        col = 'black', lty = 2)
  lines(res.natl.nmr$years.num,res.natl.nmr$lower, 
        col = 'black', lty = 2)
  legend('topright', bty = 'n',
         col = c(cols, 'grey37','black'),
         lwd = 2, legend = c(survey_years,'UN IGME', "Smoothed"))
  
}
dev.off()


## National, yearly ------------------------------------------------------

cols <- rainbow(length(survey_years))

## U5MR
direct.natl.yearly.u5$width <- direct.natl.yearly.u5$upper - direct.natl.yearly.u5$lower
direct.natl.yearly.u5$cex2 <- median(direct.natl.yearly.u5$width, na.rm = T)/direct.natl.yearly.u5$width
direct.natl.yearly.u5$cex2[direct.natl.yearly.u5$cex2 > 6] <- 6

if(dim(direct.natl.yearly.u5)[1] != 0 & !(sum(is.na(direct.natl.yearly.u5$mean)) == nrow(direct.natl.yearly.u5))){
  plot.max <- max(res.natl.yearly.u5$upper+.025, na.rm = T)
}else{plot.max <- 0.25}

pdf(paste0("Figures/SmoothedDirect/U5MR/",
           country, 
           '_natl_yearly_u5_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1))
  if (nrow(direct.natl.yearly.u5) > 0 & sum(is.na(direct.natl.yearly.u5$mean)) == nrow(direct.natl.yearly.u5)) {
    plot(NA, xlab = "Year", ylab = "U5MR",
         ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
         type = 'l', lwd = 2,col = cols[svy.idx], main = country)
    
    legend('topright', bty = 'n',
           col = c(cols, 'grey37', 'black'),
           lwd = 2,
           legend = c(survey_years,"UN IGME","Smoothed"))
    
  }else{
    for(survey in survey_years){
      tmp <- direct.natl.yearly.u5[direct.natl.yearly.u5$surveyYears == survey,]
      svy.idx <- match(survey, survey_years) 
      pane.years <- jitter(tmp$years)
      
      if(svy.idx==1){
        plot(NA, xlab = "Year", ylab = "U5MR",
             ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
             type = 'l', col = cols[svy.idx], lwd = 2, main = country)
        
        lines(pane.years, tmp$mean, cex = tmp$cex2,
              type = 'l', col = cols[svy.idx], lwd = 2)
        
        points(pane.years, tmp$mean, pch = 19,
               col = adjustcolor(cols[svy.idx], 0.35),
               cex = tmp$cex2)
        
        #add IGME reference lines
        igme.years <- jitter(beg.year:max(igme.ests.u5$year))
        lines(igme.years,
              igme.ests.u5$OBS_VALUE,
              lwd = 2, col  = 'grey37')
        lines(igme.years,
              igme.ests.u5$UPPER_BOUND,
              lty = 2, col  = 'grey37')
        lines(igme.years,
              igme.ests.u5$LOWER_BOUND, 
              lty = 2, col  = 'grey37')
        
      }else{
        lines(pane.years, tmp$mean, cex = tmp$cex2,
              type = 'l', col = cols[svy.idx], lwd = 2)
        points(pane.years, tmp$mean, pch = 19, 
               col = adjustcolor(cols[svy.idx], 0.35),
               cex = tmp$cex2)
      }
    }
  }
  
  lines(res.natl.yearly.u5$years.num, res.natl.yearly.u5$median,
        col = 'black', lwd = 2)
  lines(res.natl.yearly.u5$years.num, res.natl.yearly.u5$upper,
        col = 'black', lty = 2)
  lines(res.natl.yearly.u5$years.num,res.natl.yearly.u5$lower, 
        col = 'black', lty = 2)
  legend('topright', bty = 'n',
         col = c(cols, 'grey37','black'),
         lwd = 2, legend = c(survey_years,"UN IGME", "Smoothed"))
}
dev.off()

## NMR
direct.natl.yearly.nmr$width <- direct.natl.yearly.nmr$upper - direct.natl.yearly.nmr$lower
direct.natl.yearly.nmr$cex2 <- median(direct.natl.yearly.nmr$width, na.rm = T)/direct.natl.yearly.nmr$width
direct.natl.yearly.nmr$cex2[direct.natl.yearly.nmr$cex2 > 6] <- 6

if(dim(direct.natl.yearly.nmr)[1] != 0 & !(sum(is.na(direct.natl.yearly.nmr$mean)) == nrow(direct.natl.yearly.nmr))){
  plot.max <- max(res.natl.yearly.nmr$upper+.025, na.rm = T)
}else{plot.max <- 0.25}

pdf(paste0("Figures/SmoothedDirect/NMR/",
           country, 
           '_natl_yearly_nmr_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1))
  if (nrow(direct.natl.yearly.nmr) > 0 & sum(is.na(direct.natl.yearly.nmr$mean)) == nrow(direct.natl.yearly.nmr)) {
    plot(NA, xlab = "Year", ylab = "NMR",
         ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
         type = 'l', lwd = 2,col = cols[svy.idx], main = country)
    
    legend('topright', bty = 'n',
           col = c(cols, 'grey37', 'black'),
           lwd = 2,
           legend = c(survey_years,"UN IGME","Smoothed"))
    
  }else{
    for(survey in survey_years){
      tmp <- direct.natl.yearly.nmr[direct.natl.yearly.nmr$surveyYears == survey,]
      svy.idx <- match(survey, survey_years) 
      pane.years <- jitter(tmp$years)
      
      if(svy.idx==1){
        plot(NA, xlab = "Year", ylab = "NMR",
             ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
             type = 'l', col = cols[svy.idx], lwd = 2, main = country)
        
        lines(pane.years, tmp$mean, cex = tmp$cex2,
              type = 'l', col = cols[svy.idx], lwd = 2)
        
        points(pane.years, tmp$mean, pch = 19,
               col = adjustcolor(cols[svy.idx], 0.35),
               cex = tmp$cex2)
        
        #add IGME reference lines
        igme.years <- jitter(beg.year:max(igme.ests.nmr$year))
        lines(igme.years,
              igme.ests.nmr$OBS_VALUE,
              lwd = 2, col  = 'grey37')
        lines(igme.years,
              igme.ests.nmr$UPPER_BOUND,
              lty = 2, col  = 'grey37')
        lines(igme.years,
              igme.ests.nmr$LOWER_BOUND, 
              lty = 2, col  = 'grey37')
        
      }else{
        lines(pane.years, tmp$mean, cex = tmp$cex2,
              type = 'l', col = cols[svy.idx], lwd = 2)
        points(pane.years, tmp$mean, pch = 19, 
               col = adjustcolor(cols[svy.idx], 0.35),
               cex = tmp$cex2)
      }
    }
  }
  
  lines(res.natl.yearly.nmr$years.num, res.natl.yearly.nmr$median,
        col = 'black', lwd = 2)
  lines(res.natl.yearly.nmr$years.num, res.natl.yearly.nmr$upper,
        col = 'black', lty = 2)
  lines(res.natl.yearly.nmr$years.num,res.natl.yearly.nmr$lower, 
        col = 'black', lty = 2)
  legend('topright', bty = 'n',
         col = c(cols, 'grey37','black'),
         lwd = 2, legend = c(survey_years,"UN IGME", "Smoothed"))
}
dev.off()


## Admin 1, 3-year period ------------------------------------------------------

cols <- rainbow(nrow(admin1.names))

## U5MR
pdf(paste0("Figures/SmoothedDirect/U5MR/",country, 
           '_admin1_u5_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1),lend=1)
  plot.max <- max(res.admin1.u5$results$upper+.025, na.rm = T)
  
  plot(NA, xlab = "Year", ylab = "U5MR",
       ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)), main = paste0(country,' - Admin 1 Regions'))
  legend('topright', bty = 'n',
           col = cols, lwd = 2,  legend = admin1.names$GADM)
  
  for(area in 1:dim(poly.adm1)[1]){
    tmp.area <- direct.admin1.u5[direct.admin1.u5$region == 
                                as.character(admin1.names$Internal[area]),]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    
    res.area <- res.admin1.u5$results[res.admin1.u5$results$region == as.character(admin1.names$Internal[area]),]
    
    lines(res.area$years.num,res.area$median,
          col = cols[area], lwd = 2)
    lines(res.area$years.num, res.area$upper,
          col = cols[area], lty = 2)
    lines(res.area$years.num,
          res.area$lower, col = cols[area], lty = 2)
    }  
   
}
dev.off()

## NMR
pdf(paste0("Figures/SmoothedDirect/NMR/",country, 
           '_admin1_nmr_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1),lend=1)
  plot.max <- max(res.admin1.nmr$results$upper+.025, na.rm = T)
  
  plot(NA, xlab = "Year", ylab = "NMR",
       ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)), main = paste0(country,' - Admin 1 Regions'))
  legend('topright', bty = 'n',
         col = cols, lwd = 2,  legend = admin1.names$GADM)
  
  for(area in 1:dim(poly.adm1)[1]){
    tmp.area <- direct.admin1.nmr[direct.admin1.nmr$region == 
                                   as.character(admin1.names$Internal[area]),]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    
    res.area <- res.admin1.nmr$results[res.admin1.nmr$results$region == as.character(admin1.names$Internal[area]),]
    
    lines(res.area$years.num,res.area$median,
          col = cols[area], lwd = 2)
    lines(res.area$years.num, res.area$upper,
          col = cols[area], lty = 2)
    lines(res.area$years.num,
          res.area$lower, col = cols[area], lty = 2)
  }  
  
}
dev.off()

## Admin 2, 3-year period ------------------------------------------------------

cols <- rainbow(nrow(admin2.names))

## U5MR
pdf(paste0("Figures/SmoothedDirect/U5MR/",country, 
           '_admin2_u5_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1),lend=1)
  plot.max <- max(res.admin2.u5$results$median+.025, na.rm = T)
  
  plot(NA, xlab = "Year", ylab = "U5MR",
       ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)), main = paste0(country,' - Admin 2 Regions'))
  
  for(area in 1:dim(poly.adm2)[1]){
    tmp.area <- direct.admin2.u5[direct.admin2.u5$region == 
                                as.character(admin2.names$Internal[area]),]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    
    res.area <- res.admin2.u5$results[res.admin2.u5$results$region == as.character(admin2.names$Internal[area]),]
    
    lines(res.area$years.num,res.area$median,
          col = cols[area], lwd = 1)
    
  }  
  
}
dev.off()

## NMR
pdf(paste0("Figures/SmoothedDirect/NMR/",country, 
           '_admin2_nmr_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1),lend=1)
  plot.max <- max(res.admin2.nmr$results$median+.025, na.rm = T)
  
  plot(NA, xlab = "Year", ylab = "NMR",
       ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)), main = paste0(country,' - Admin 2 Regions'))
  
  for(area in 1:dim(poly.adm2)[1]){
    tmp.area <- direct.admin2.nmr[direct.admin2.nmr$region == 
                                   as.character(admin2.names$Internal[area]),]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    
    res.area <- res.admin2.nmr$results[res.admin2.nmr$results$region == as.character(admin2.names$Internal[area]),]
    
    lines(res.area$years.num,res.area$median,
          col = cols[area], lwd = 1)
    
  }  
  
}
dev.off()

