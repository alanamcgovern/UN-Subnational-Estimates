###############################################################
######## Direct estimates are computed separately for each survey and then aggregated for smoothed estimates
###############################################################

################################################################
#########   Load libraries
################################################################

rm(list = ls())
options(gsubfn.engine = "R")
library(rgdal)
library(SUMMER)
library(dplyr)

#### ----------------------------------------------------------
#### ----------------------------------------------------------
# ENTER COUNTRY BEING ANALYZED
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'
#### ----------------------------------------------------------
#### ----------------------------------------------------------


# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve user-specified directory
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

################################################################
#########   load polygon files
################################################################

setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm0)) # load the national shape file
# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

if(sum(grepl(paste('gadm36', gadm.abbrev,
                   '2', sep = "_"), list.files(poly.path))) != 0){
  poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                       layer = as.character(poly.layer.adm2))} # load the shape file of admin-2 regions

# set coordinate reference system to be equal
if(exists("poly.adm2")){
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0('shapeFiles_gadm/', country, '_Amat.rda'))
load(paste0('shapeFiles_gadm/', country, '_Amat_Names.rda'))

# load a helper function from the R script getSmoothed.R in the same folder
source(file=paste0(home.dir, '/Rcode/getSmoothed.R'))

###############################################################
####### define the periods over which the 3-year estimates will be calculated -- adjusted slightly when number of years is not divisible by 3
###############################################################
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


###############################################################
##### load in and prepare data; separate each survey into its own dataset in births.list
###############################################################
svy.idx <- 0
births.list <- list()
  load(paste0(country,'_cluster_dat.rda'),
       envir = .GlobalEnv)
  mod.dat$years <- as.numeric(as.character(mod.dat$years)) # convert the years from string into numbers
  # After we process the data, we compute for the direct U5MR estimates at national level
  mod.dat$v005 <- mod.dat$v005/1e6
  for(survey in survey_years){
    svy.idx <- svy.idx + 1
    births.list[[svy.idx]] <- mod.dat[mod.dat$survey == survey,] %>%
      as.data.frame()
    births.list[[svy.idx]]$died <- births.list[[svy.idx]]$Y
    births.list[[svy.idx]]$total <- as.numeric(births.list[[svy.idx]]$total)
    births.list[[svy.idx]]$period <- as.character(cut(births.list[[svy.idx]]$years, breaks = c(beg.period.years, beg.period.years[length(beg.period.years)]+5),
                                       include.lowest = T, right = F, labels = periods)) # generate period label 
  }


names(births.list) <- survey_years
setwd(paste0(res.dir,'/Direct'))


#####-----------------------------------------------------------
#####-----------------------------------------------------------
#########   DIRECT ESTIMATES
#####-----------------------------------------------------------
#####-----------------------------------------------------------

######################################################
######## National
######################################################

#if there is more than one survey
if(length(births.list) != 1){
    # 3-year estimates
     direct.natl <-  SUMMER::getDirectList(births.list, periods,
                                           regionVar = "admin1.char",
                                           timeVar = "period", 
                                           clusterVar =  "~cluster",
                                          ageVar = "age", Ntrials = "total",
                                           weightsVar = "v005",national.only = T)
     # yearly estimates
    direct.natl.yearly <- SUMMER::getDirectList(births.list, beg.year:end.year,
                                                regionVar = "admin1.char",
                                                timeVar = "years", 
                                                clusterVar =  "~cluster",
                                                ageVar = "age", Ntrials = "total",
                                                weightsVar = "v005",national.only = T)
    
    direct.natl$region_num <- direct.natl$region
    direct.natl.yearly$region_num <- direct.natl.yearly$region
#if there is only one survey
}else{
      # 3-year estimates
     direct.natl <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), periods,
                                       regionVar = "admin1.char",
                                       timeVar = "period", 
                                       clusterVar =  "~cluster",
                                       ageVar = "age", Ntrials = "total",
                                       weightsVar = "v005",national.only = T)
     # yearly estimates
    direct.natl.yearly <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), beg.year:end.year,
                                             regionVar = "admin1.char",
                                             timeVar = "years", 
                                             clusterVar =  "~cluster",
                                             ageVar = "age", Ntrials = "total",
                                             weightsVar = "v005",national.only = T)
    
    direct.natl$survey <- 1
    direct.natl$surveyYears <- surveys[1]
    direct.natl$region_num <- direct.natl$region
    
    direct.natl.yearly$survey <- 1
    direct.natl.yearly$surveyYears <- surveys[1]
    direct.natl.yearly$region_num <- direct.natl.yearly$region
    
  }

  # save national direct estimates
  save(direct.natl, file = paste0(country, '_direct_natl.rda'))
  save(direct.natl.yearly, file = paste0(country, '_direct_natl_yearly.rda'))
  
  ######################################################
  ######## Admin 1
  ######################################################

  if(length(births.list) != 1){
     direct.admin1 <-  SUMMER::getDirectList(births.list, periods,
                                           regionVar = "admin1.char",
                                           timeVar = "period", 
                                           clusterVar =  "~cluster",
                                           ageVar = "age", Ntrials = "total",
                                           weightsVar = "v005",national.only = F)
    direct.admin1.yearly <- SUMMER::getDirectList(births.list, beg.year:end.year,
                                                regionVar = "admin1.char",
                                                timeVar = "years", 
                                                clusterVar =  "~cluster",
                                                ageVar = "age", Ntrials = "total",
                                                weightsVar = "v005",national.only = F)
    
    direct.admin1$region_num <- direct.admin1$region
    direct.admin1.yearly$region_num <- direct.admin1.yearly$region
  }else{
     direct.admin1 <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), periods,
                                       regionVar = "admin1.char",
                                       timeVar = "period", 
                                       clusterVar =  "~cluster",
                                       ageVar = "age", Ntrials = "total",
                                       weightsVar = "v005",national.only = F)
    direct.admin1.yearly <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), beg.year:end.year,
                                             regionVar = "admin1.char",
                                             timeVar = "years", 
                                             clusterVar =  "~cluster",
                                             ageVar = "age", Ntrials = "total",
                                             weightsVar = "v005",national.only = F)
    
    direct.admin1$survey <- 1
    direct.admin1$surveyYears <- surveys[1]
    direct.admin1$region_num <- direct.admin1$region
    
    direct.admin1.yearly$survey <- 1
    direct.admin1.yearly$surveyYears <- surveys[1]
    direct.admin1.yearly$region_num <- direct.admin1.yearly$region
    
  }
  
  save(direct.admin1, file = paste0(country, '_direct_admin1.rda'))
  save(direct.admin1.yearly, file = paste0(country, '_direct_admin1_yearly.rda'))
  
  ######################################################
  ######## Admin 2
  ######################################################

  # compute 3-year direct U5MR for at admin2 level. But this may fail due to the data sparsity at admin2 level.
    if(length(births.list) != 1){
      direct.admin2 <-  SUMMER::getDirectList(births.list, periods,
                                              regionVar = "admin2.char",
                                              timeVar = "period", 
                                              clusterVar =  "~cluster",
                                              ageVar = "age", Ntrials = "total",
                                              weightsVar = "v005",national.only = F)
      direct.admin2$region_num <- direct.admin2$region
    }else{
      direct.admin2 <-  SUMMER::getDirect(as.data.frame(births.list[[1]]), periods,
                                          regionVar = "admin2.char",
                                          timeVar = "period", 
                                          clusterVar =  "~cluster",
                                          ageVar = "age", Ntrials = "total",
                                          weightsVar = "v005",national.only = F)
      
      direct.admin2$survey <- 1
      direct.admin2$surveyYears <- surveys[1]
      direct.admin2$region_num <- direct.admin2$region
      
    }
    
  save(direct.admin2, file = paste0(country,'_direct_admin2.rda')) # save the admin2 3-year direct U5MR

  ######################################################
  ######## HIV Adjustment
  ######################################################
  
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
    
    ## National Adjustments
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
      
      tmp.adj <- SUMMER::getAdjusted(direct.natl.yearly[direct.natl.yearly$surveyYears == survey,],
                                     ratio = adj.frame, 
                                     logit.lower = NULL,
                                     logit.upper = NULL,
                                     prob.upper = "upper",
                                     prob.lower = "lower")
      direct.natl.yearly[direct.natl.yearly$surveyYears == survey,] <- 
        tmp.adj[ ,  match(colnames(direct.natl.yearly),
                          colnames(tmp.adj))]
      
      adj.frame.tmp <- adj.frame[adj.frame$years %in%  beg.period.years, ]
      adj.frame.tmp$years <- periods
      
      tmp.adj <- SUMMER::getAdjusted(direct.natl[direct.natl$surveyYears == survey,],
                                     ratio = adj.frame.tmp, 
                                     logit.lower = NULL,
                                     logit.upper = NULL,
                                     prob.upper = "upper",
                                     prob.lower = "lower")
      direct.natl[direct.natl$surveyYears == survey,] <- 
        tmp.adj[ ,  match(colnames(direct.natl),
                          colnames(tmp.adj))]
    }
    
    ## Admin 1 and 2 Adjustments ##
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
         tmp.adj <- SUMMER::getAdjusted(direct.admin1[direct.admin1$region == as.character(area.int) &
                                                        direct.admin1$surveyYears == survey,],
                                        ratio = adj.frame.tmp, 
                                        logit.lower = NULL,
                                        logit.upper = NULL,
                                        prob.upper = "upper",
                                        prob.lower = "lower")
         direct.admin1[direct.admin1$region == as.character(area.int) &
                         direct.admin1$surveyYears == survey,] <- 
           tmp.adj[ , match(colnames(direct.admin1),
                            colnames(tmp.adj))]
        
        tmp.adj <- SUMMER::getAdjusted(direct.admin1.yearly[direct.admin1.yearly$region == as.character(area.int) &
                                                       direct.admin1.yearly$surveyYears == survey,],
                                       ratio = adj.frame, 
                                       logit.lower = NULL,
                                       logit.upper = NULL,
                                       prob.upper = "upper",
                                       prob.lower = "lower")
        direct.admin1.yearly[direct.admin1.yearly$region == as.character(area.int) &
                        direct.admin1.yearly$surveyYears == survey,] <- 
          tmp.adj[ , match(colnames(direct.admin1.yearly),
                           colnames(tmp.adj))]
        
        if(exists('direct.admin2')){
          admin2.to.admin1 <- births.list[[1]][!duplicated(births.list[[1]]$admin2.name),] %>% 
            dplyr::select(GADM.adm2=admin2.name,Internal.adm2=admin2.char,GADM.adm1 = admin1.name)
          admin2.to.admin1 <- data.frame(admin2.to.admin1,
                                         Internal.adm1 = NA)
          admin2.to.admin1$Internal.adm1 <- admin1.names$Internal[match(admin2.to.admin1$GADM.adm1,
                                                                        admin1.names$GADM)]
          admin2s <- admin2.to.admin1$Internal.adm2[admin2.to.admin1$Internal.adm1 == 
                                                      as.character(area.int)]
          tmp.adj <- SUMMER::getAdjusted(direct.admin2[direct.admin2$region %in% admin2s &
                                                         direct.admin2$surveyYears == survey,],
                                         ratio = adj.frame.tmp, 
                                         logit.lower = NULL,
                                         logit.upper = NULL,
                                         prob.upper = "upper",
                                         prob.lower = "lower")
          direct.admin2[direct.admin2$region %in% admin2s &
                          direct.admin2$surveyYears == survey,] <- 
            tmp.adj[ , match(colnames(direct.admin2), 
                             colnames(tmp.adj))]
        }
      }
    }
    
    save(direct.natl.yearly, file = paste0(country, '_directHIV_natl_yearly.rda'))
    save(direct.natl, file = paste0(country, '_directHIV_natl.rda'))
    save(direct.admin1, file = paste0(country, '_directHIV_admin1.rda'))
    save(direct.admin1.yearly, file = paste0(country, '_directHIV_admin1_yearly.rda'))
    if(exists('direct.admin2')){
      save(direct.admin2, file = paste0(country, '_directHIV_admin2.rda'))
    }
    
  }
  
  

################################################################
#########   smooth direct estimates
################################################################
# make sure that we are using correct direct estimates (HIV adjusted if they exist)
if(doHIVAdj){
  load( paste0(country, '_directHIV_natl_yearly.rda'))
  load(paste0(country, '_directHIV_natl.rda'))
  load(paste0(country, '_directHIV_admin1.rda'))
  load(paste0(country, '_directHIV_admin1_yearly.rda'))
  #if(exists('direct.admin2')){
    load(paste0(country, '_directHIV_admin2.rda'))
  #}
}else{
  load( paste0(country, '_direct_natl_yearly.rda'))
  load(paste0(country, '_direct_natl.rda'))
  load(paste0(country, '_direct_admin1.rda'))
  load(paste0(country, '_direct_admin1_yearly.rda'))
  #if(exists('direct.admin2')){
    load(paste0(country, '_direct_admin2.rda'))
  #}
}
  
# aggregate surveys
data.natl <- SUMMER::aggregateSurvey(direct.natl)
data.natl.yearly <- SUMMER::aggregateSurvey(direct.natl.yearly)
data.admin1 <- SUMMER::aggregateSurvey(direct.admin1)
data.admin1.yearly <- SUMMER::aggregateSurvey(direct.admin1.yearly)
data.admin2 <- SUMMER::aggregateSurvey(direct.admin2)

#### National Model ####
  
## compute smoothed direct U5MR estimates for 3-year periods at national level
end.year<- max(survey_years)
beg.proj.years <- seq(end.year+1,end.proj.year,3)
end.proj.years <- beg.proj.years+2
proj.per <- paste(beg.proj.years, end.proj.years, sep = "-") # add the 3-year period to be projected

fit.natl <- smoothDirect(data.natl, geo = NULL, Amat = NULL, # national level model doesn't need to specify adjacency matrix since it would just be 1.
                     year_label = c(periods, proj.per),
                     year_range = c(beg.year, max(end.proj.years)), is.yearly = F) # fit the smoothed direct model. Changing the year label and year range can change the years the estimators to be computed, 
## even for future years where DHS data is not yet available. But this would lead to less accurate estimates and larger uncertainty level.
# 
 res.natl <- getSmoothed(fit.natl,year_range = c(beg.year, max(end.proj.years)),
                         year_label = c(periods, proj.per)) # sample for smoothed direct estimates
 
 res.natl$years.num <- seq(beg.year, end.proj.year+3, 3)
 res.natl$region.gadm <- country
 save(res.natl, file = paste0(country, "_res_natl_SmoothedDirect.rda")) # save the national 3-year smoothed direct U5MR

# compute yearly smoothed direct U5MR estimates for at national level -- include 3 years after last survey
fit.natl.yearly <- smoothDirect(data.natl.yearly, geo = NULL, Amat = NULL,
                           year_label = as.character(beg.year:max(end.proj.years)),
                           year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
res.natl.yearly <- getSmoothed(fit.natl.yearly, year_range = c(beg.year, max(end.proj.years)),
                               year_label = as.character(beg.year:max(end.proj.years)))
res.natl.yearly$years.num <- beg.year:max(end.proj.years)
res.natl.yearly$region.gadm <- country
save(res.natl.yearly, file = paste0(country, "_res_natl_yearly_SmoothedDirect.rda")) # save the national yearly smoothed direct U5MR


#### Admin-1 Model ####

# compute smoothed direct U5MR estimates for 3-year periods at admin1 level
data.admin1 <- data.admin1[data.admin1$region!='All',] # direct.admin1 is a matrix containing all national and admin1 level estimates. Only admin1 estimates are interested here.
fit.admin1 <- smoothDirect(data.admin1, Amat = admin1.mat,
                      year_label = c(periods, proj.per),
                      year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
res.admin1 <- getSmoothed_sd(fit.admin1, Amat = admin1.mat,
                             year_label = c(periods, proj.per),
                             year_range = c(beg.year, max(end.proj.years)),
                             save.draws = TRUE)

res.admin1$results$years.num <- seq(beg.year, end.proj.year+3, 3)[match(res.admin1$results$years, c(periods, proj.per))]
res.admin1$results$region.gadm <- admin1.names$GADM[match(res.admin1$results$region, admin1.names$Internal)]

save(res.admin1, file = paste0(country, "_res_admin1_SmoothedDirect.rda"))# save the admin1 3-year smoothed direct U5MR


# compute yearly smoothed direct U5MR estimates at admin1 level
data.admin1.yearly <- data.admin1.yearly[data.admin1.yearly$region!='All',]
fit.admin1.yearly <- smoothDirect(data.admin1.yearly, Amat = admin1.mat,
                             year_label = as.character(beg.year:max(end.proj.years)),
                             year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
sd.admin1.yearly <- getSmoothed_sd(fit.admin1.yearly, Amat = admin1.mat,
                                   year_label = as.character(beg.year:max(end.proj.years)),
                                   year_range = c(beg.year, max(end.proj.years)),
                                   save.draws = TRUE)
sd.admin1.yearly$results$region.gadm <- admin1.names$GADM[match(sd.admin1.yearly$results$region, admin1.names$Internal)]

save(sd.admin1.yearly, file = paste0(country, "_res_admin1_SmoothedDirect_yearly.rda")) # save the admin1 yearly smoothed direct U5MR

#### Admin-2 Model ####

# compute smoothed direct estimates for 3-year periods at admin2 level
data.admin2 <- data.admin2[data.admin2$region!='All',] # direct.admin1 is a matrix containing all national and admin1 level estimates. Only admin1 estimates are interested here.
fit.admin2 <- smoothDirect(data.admin2, Amat = admin2.mat,
                      year_label = c(periods, proj.per),
                      year_range = c(beg.year, max(end.proj.years)), is.yearly = F)
res.admin2 <- getSmoothed_sd(fit.admin2, Amat = admin2.mat,
                             year_label = c(periods, proj.per),
                             year_range = c(beg.year, max(end.proj.years)),
                             save.draws = TRUE)

res.admin2$results$years.num <- seq(beg.year, end.proj.year+3, 3)[match(res.admin2$results$years, c(periods, proj.per))]
res.admin2$results$region.gadm <- admin2.names$GADM[match(res.admin2$results$region, admin2.names$Internal)]

save(res.admin2, file = paste0(country, "_res_admin2_SmoothedDirect.rda"))
