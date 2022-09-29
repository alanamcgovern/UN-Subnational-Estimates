rm(list = ls())
# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Lesotho'

# Load libraries and info ----------------------------------------------------------
options(gsubfn.engine = "R")
library(rgdal)
options(warn=0)
library(spdep)
library(SUMMER)
library(geosphere)
library(stringr)
library(tidyverse)
#devtools::install_github("ropensci/rdhs")
library(rdhs)

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

# set API to get DHS data -- you will need to change this to your information!
set_rdhs_config(email = "amcgov@uw.edu",
                project = "Spatial Modeling for Subnational Administrative Level 2 Small-Area Estimation - Under 5 Mortality Rate")

#update_rdhs_config(email = "amcgov@uw.edu", password = T,
 #               project = "Spatial Modeling for Subnational Administrative Level 2 Small-Area Estimation - Under 5 Mortality Rate")


# Load polygon files ----------------------------------------------------------

setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm0)) # load the national shape file
# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

if(exists('poly.layer.adm2')){
  poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                       layer = as.character(poly.layer.adm2))} # load the shape file of admin-2 regions

# set coordinate reference system to be equal
if(exists("poly.adm2")){
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

# Create Adjacency Matrix ----------------------------------------------------------

# Adjacency matrix is a symmetric matrix with each entry of 1's or 0's indicating if the two administrative regions are adjacent. 
# Each row or column represents an administrative region.
# The codes below generates spatial adjacency matrix based on the spatial polygon file.

if(exists("poly.adm1")){ # create the adjacency matrix for admin1 regions.
  admin1.mat <- poly2nb(SpatialPolygons(poly.adm1@polygons))
  admin1.mat <- nb2mat(admin1.mat, zero.policy = TRUE)
  colnames(admin1.mat) <- rownames(admin1.mat) <- paste0("admin1_", 1:dim(admin1.mat)[1])
  admin1.names <- data.frame(GADM = eval(str2lang(poly.label.adm1)),
                             Internal = rownames(admin1.mat))
}else{
  message("There is no Admin1 polygon file.")
}
if(exists("poly.adm2")){  # create the adjacency matrix for admin2 regions.
  admin2.mat <- poly2nb(SpatialPolygons(poly.adm2@polygons))
  admin2.mat <- nb2mat(admin2.mat, zero.policy = TRUE)
  colnames(admin2.mat) <- rownames(admin2.mat) <- paste0("admin2_", 1:dim(admin2.mat)[1])
  admin2.names <- data.frame(GADM = eval(str2lang(poly.label.adm2)),
                              Internal = rownames(admin2.mat))
}else{
  message("There is no Admin2 polygon file.")
}

if(exists("poly.adm2")){
  save(admin1.mat, admin2.mat, file = paste0(poly.path,'/', country, '_Amat.rda')) # save the admin1 and admin2 adjacency matrix
  save(admin1.names, admin2.names, file = paste0(poly.path, '/', country, '_Amat_Names.rda')) # save the admin1 and admin2 names
}else{
  save(admin1.mat, file = paste0(poly.path,'/', country, '_Amat.rda'))
  save(admin1.names, file = paste0(poly.path, '/', country, '_Amat_Names.rda'))
}

# Polygon Plots ----------------------------------------------------------

if(!dir.exists(paste0(res.dir,'/Figures/ShapeCheck'))){
  dir.create(paste0(res.dir,'/Figures/ShapeCheck'))
}

cent <- getSpPPolygonsLabptSlots(poly.adm1)
cols <- rainbow(min(10,
                    dim(admin1.mat)[1]))

### Admin1 neighbors ####
pdf(paste0(res.dir,
           '/Figures/ShapeCheck/',
           country, '_adm1_neighb.pdf'),
    height = 4, width = 4)
{
  plot(poly.adm1, col = cols, border = F, axes = F,)
  for(i in 1:dim(cent)[1]){
    neighbs <- which(admin1.mat[i,] != 0)
    if(length(neighbs) != 0){
      for(j in 1:length(neighbs)){
        ends <- cent[neighbs,]
        segments(x0 = cent[i, 1],
                 y0 = cent[i, 2],
                 x1 = cent[neighbs[j], 1],
                 y1 = cent[neighbs[j], 2],
                 col = 'black')
      }
    }
  }
}
dev.off()

### Admin2 neighbors ####
if(exists("admin2.mat")){
  
  cent <- getSpPPolygonsLabptSlots(poly.adm2)
  cols <- rainbow(min(10,
                      dim(admin2.mat)[1]))
  
  pdf(paste0(res.dir,
             '/Figures/ShapeCheck/',
             country, '_adm2_neighb.pdf'),
      height = 4, width = 4)
  {
    plot(poly.adm2, col = cols, border = F, axes = F,)
    for(i in 1:dim(cent)[1]){
      neighbs <- which(admin2.mat[i,] != 0)
      if(length(neighbs) != 0){
        for(j in 1:length(neighbs)){
          ends <- cent[neighbs,]
          segments(x0 = cent[i, 1],
                   y0 = cent[i, 2],
                   x1 = cent[neighbs[j], 1],
                   y1 = cent[neighbs[j], 2], 
                   col = 'black')
        }
      }
    }
  }
  dev.off()
}

# Find DHS surveys ----------------------------------------------------------

#get country ID
countryId <- dhs_countries()[dhs_countries()$ISO3_CountryCode==toupper(gadm.abbrev),]

potential_surveys <- dhs_datasets(countryIds = countryId$DHS_CountryCode, surveyYearStart = 2000) %>% dplyr::filter((FileType == 'Births Recode' & FileFormat=='Stata dataset (.dta)') |(FileType == 'Geographic Data' & FileFormat =='Flat ASCII data (.dat)'))

#only keep surveys with both a births recode and a geographic dataset
dhs_survey_ids <- as.numeric(unique(potential_surveys$SurveyNum)[sapply(unique(potential_surveys$SurveyNum),
                                                                           function(num){
                                                                             if(sum(c("Births Recode","Geographic Data") %in% (potential_surveys %>% filter(SurveyNum==num))$FileType) ==2){return(T)
                                                                             }else(return(F))})])

surveys <- potential_surveys %>% filter(SurveyNum %in% dhs_survey_ids) %>% group_by(SurveyYear) %>% arrange(SurveyYear,DatasetType)
dhs_survey_years <- as.numeric(unique(surveys$SurveyYear))

# CHECK THAT SURVEYS FOR CORRECT COUNTRY HAVE BEEN CHOSEN
unique(surveys$CountryName)

# Process data for each DHS survey year ----------------------------------------------------------

# The codes below first loads the raw DHS data, then it assigns the GPS coordinates to each sampling cluster and admin regions where
# the sampling is conducted and assigns the admin regions where the clusters are located.

for(survey_year in dhs_survey_years){
  
    dhs.svy.ind <- which(dhs_survey_years==survey_year)
    message('Processing DHS data for ', country,' ', survey_year,'\n')
    
    data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear==survey_year,]$FileName, clear_cache = T)
    
    raw.dat.tmp <- readRDS(paste0(data.paths.tmp[2]))
  
    # convert some variables to factors
    alive <- attr(raw.dat.tmp$b5, which = "labels")
    names(alive) <- tolower(names(alive))
    raw.dat.tmp$b5 <- ifelse(raw.dat.tmp$b5 == alive["yes"][[1]], "yes", "no")
    raw.dat.tmp$b5 <- factor(raw.dat.tmp$b5, levels = c("yes", "no"))
    
    strat <- attr(raw.dat.tmp$v025,which='labels')
    names(strat) <- tolower(names(strat))
    raw.dat.tmp$v025 <- ifelse(raw.dat.tmp$v025 == strat["urban"][[1]],'urban','rural')
    raw.dat.tmp$v025 <- factor(raw.dat.tmp$v025, levels = c('urban','rural'))
    
    # read DHS data
    dat.tmp <- getBirths(data=raw.dat.tmp,
                     surveyyear = survey_year,
                     year.cut = seq(beg.year, survey_year + 1, 1),compact = T)
    

    # retrieve the some columns of the full data
    dat.tmp <- dat.tmp[ ,c("v001", "v024", "time", "total",
                       "age", "v005", "v025", "strata", "died")]

    # specify the name of DHS GPS file, which contains the GPS coordinates of the sampling cluster where the data is sampled
    points <- readRDS(paste0(data.paths.tmp[1]))
    
    # detect points in the DHS GPS file with mis-specified coordinates and remove them if any
    wrong.points <- which(points@data$LATNUM == 0.0 & points@data$LONGNUM == 0.0)
    if(!is.null(dim(wrong.points))){message("There are wrong GPS points: (Longitude, Latitude) = (0, 0)")}

    # remove wrong points in the data if any
    dat.tmp <- dat.tmp[!(dat.tmp$v001 %in% points@data$DHSCLUST[wrong.points]),]
    points@data$DHSCLUST[wrong.points] %in% unique(dat.tmp$v001)

    # add the column for GPS coordinate in the data
   dat.tmp$LONGNUM <- dat.tmp$LATNUM <- NA
    for(i in 1:dim(points)[1]){
     dat.tmp$LATNUM[dat.tmp$v001 == points@data$DHSCLUST[i]] <- points@data$LATNUM[i] # assign latitude to DHS cluster location
     dat.tmp$LONGNUM[dat.tmp$v001 == points@data$DHSCLUST[i]] <- points@data$LONGNUM[i] # assign longitude to DHS cluster location
   }

   # remove missing points in the data if any
   miss <- which(dat.tmp$LATNUM == 0 & dat.tmp$LONGNUM == 0)
    if(length(miss != 0)){
      dat.tmp <- dat.tmp[-miss,]
    }

    message("\n Assigned LAT & LONG")


    # assign admin regions based on coordinates and polygon files
    adm1.ind <- exists("poly.adm1")
    adm2.ind <- exists("poly.adm2")

    points.frame <- as.data.frame(dat.tmp[,c("LONGNUM", "LATNUM")]) # retrieve GPS coordinates where data is sampled.
    points.frame <- SpatialPoints(points.frame) # convert the GPS coordinates into "sp" object.
    if(adm2.ind){
      poly.over.adm2 <- SpatialPolygons(poly.adm2@polygons)
      proj4string(points.frame) <- proj4string(poly.over.adm2) <- 
      proj4string(poly.adm2)  <- 
      proj4string(poly.adm1)  
      admin2.key <- over(points.frame, poly.over.adm2)
      miss.frame.adm2 <- unique(points.frame@coords[which(is.na(admin2.key)),])
  
      if(dim(miss.frame.adm2)[1] != 0){
        miss.poly.adm2 <- dist2Line( miss.frame.adm2, poly.over.adm2)
    
        for(i in 1:dim(miss.poly.adm2)[1]){
          long.ids <- which(points.frame@coords[,c("LONGNUM")] %in% miss.frame.adm2[i,1])
          lat.ids <- which(points.frame@coords[,c("LATNUM")] %in% miss.frame.adm2[i,2])
          ids <- intersect(long.ids, lat.ids)
          admin2.key[ids] <- rep(miss.poly.adm2[i, 'ID'], length(ids))
        }
      }
  
      dat.tmp$admin2 <- admin2.key
      dat.tmp$admin2.char <- paste0("admin2_", admin2.key)
      dat.tmp$admin2.name <- as.character(eval(str2lang(poly.label.adm2)))[admin2.key]
    }else{
      dat.tmp$admin2 <- dat.tmp$admin2.name <- NA
      message("There is no Admin2 polygon to assign points to.")
    }

    if(adm1.ind){
      poly.over.adm1 <- SpatialPolygons(poly.adm1@polygons)
      proj4string(points.frame) <- proj4string(poly.over.adm1) <- 
      proj4string(poly.adm1) 
      admin1.key <- over(points.frame, poly.over.adm1)
      miss.frame.adm1 <- unique(points.frame@coords[which(is.na(admin1.key)),])
  
      if(dim(miss.frame.adm1)[1] != 0){
        miss.poly.adm1 <- dist2Line( miss.frame.adm1, poly.over.adm1)
    
        for(i in 1:dim(miss.poly.adm1)[1]){
          long.ids <- which(points.frame@coords[,c("LONGNUM")] %in% miss.frame.adm1[i,1])
          lat.ids <- which(points.frame@coords[,c("LATNUM")] %in% miss.frame.adm1[i,2])
          ids <- intersect(long.ids, lat.ids)
          admin1.key[ids] <- rep(miss.poly.adm1[i, 'ID'], length(ids))
        }
      }
  
      dat.tmp$admin1 <- admin1.key
      dat.tmp$admin1.char <- paste0("admin1_", admin1.key)
      dat.tmp$admin1.name <- as.character(eval(str2lang(poly.label.adm1)))[admin1.key]
    }else{
      dat.tmp$admin1 <- dat.tmp$admin1.name <- NA
      message("There is no Admin1 polygon to assign points to.")
    }  

    if(FALSE){
      check <- dat.tmp$strata
      check <- gsub(" - rural", "", check)
      check <- gsub(" - urban", "", check)
      inconsist <- which(check != tolower(dat.tmp$admin1.name))
      table(check[inconsist], dat.tmp$admin1.name[inconsist])
      unique(dat.tmp[which(check == "neno" & dat.tmp$admin1.name == "Balaka"), "v001"])
    }

    # finish preparing data ###
    if(adm2.ind){
      dat.tmp <- dat.tmp[,c("v001", "age", "time", "total", "died", "v005", 
                            "v025", "LONGNUM", "LATNUM","strata",
                            "admin1", "admin2", "admin1.char", "admin2.char", "admin1.name", "admin2.name")]
      colnames(dat.tmp) <- c("cluster", "age", "years", "total",
                             "Y", "v005", "urban", "LONGNUM", "LATNUM","strata",
                             "admin1", "admin2", "admin1.char", "admin2.char", "admin1.name", "admin2.name")
    }else{
      dat.tmp <- dat.tmp[,c("v001", "age", "time", "total", "died", "v005", "v025", "LONGNUM", "LATNUM","strata",
                            "admin1", "admin1.char", "admin1.name")]
      colnames(dat.tmp) <- c("cluster", "age", "years", "total", "Y", "v005", "urban", "LONGNUM", "LATNUM","strata",
                             "admin1", "admin1.char", "admin1.name")
    }
    
    dat.tmp$survey <- raw.dat.tmp$survey_year <-survey_year
    dat.tmp$survey.type <- 'DHS'
  
    if(survey_year==dhs_survey_years[1]){
      mod.dat <- dat.tmp
      raw.dat <- raw.dat.tmp[,c("caseid", "v001", "v022", "b5",'b7','survey_year')]
    }else{mod.dat <- rbind(mod.dat,dat.tmp)
          raw.dat <- rbind(raw.dat,raw.dat.tmp[,c("caseid", "v001", "v022", "b5",'b7','survey_year')])}
  
  }

# Process data for each MICS survey year ----------------------------------------------------------
if(dir.exists(paste0(home.dir,'/Data/MICS/',country))){
  
  mics_files <- list.files(paste0(home.dir,'/Data/MICS/',country))[(grepl('tmp.rda',list.files(paste0(home.dir,'/Data/MICS/',country))))]
  
  #make admin key
  if(adm2.ind){
    admin.key <- mod.dat %>% dplyr::select(admin1,admin2,admin1.char,admin2.char,admin1.name,admin2.name,strata) %>% distinct()
  
  for(k in 1:length(mics_files)){
    
    load(file=paste0(home.dir,'/Data/MICS/',country,'/',mics_files[k]))
    
    #match admin area codes
    dat.tmp$admin1 <- dat.tmp$admin2 <-dat.tmp$strata <- NA
    dat.tmp$admin1.char <- dat.tmp$admin2.char <- admin1.name <- ''
    for(perm in 1:nrow(admin.key)){
      perm.ind <- dat.tmp$admin2.name==admin.key$admin2.name[perm]
      dat.tmp$admin1.name[perm.ind] <- admin.key$admin1.name[perm]
      dat.tmp$admin1[perm.ind] <- admin.key$admin1[perm]
      dat.tmp$admin1.char[perm.ind] <- admin.key$admin1.char[perm]
      dat.tmp$admin2[perm.ind] <- admin.key$admin2[perm]
      dat.tmp$admin2.char[perm.ind] <- admin.key$admin2.char[perm]
      dat.tmp$strata[perm.ind] <- admin.key$strata[perm]
    }
    
    #prepare to merge with DHS data
    dat.tmp$LONGNUM <- dat.tmp$LATNUM <- NA
    dat.tmp$survey.type <- 'MICS'
    dat.tmp <- dat.tmp[,c("cluster",'age','years','total','Y','v005','urban',"LONGNUM","LATNUM",'strata','admin1','admin2',
                          'admin1.char','admin2.char','admin1.name','admin2.name','survey','survey.type')]
    
    #add to prepared data
    mod.dat <- rbind(mod.dat,dat.tmp)
    }
  }else{
    admin.key <- mod.dat %>% dplyr::select(admin1,admin1.char,admin1.name,strata) %>% distinct()
    
    for(k in 1:length(mics_files)){
      
      load(file=paste0(home.dir,'/Data/MICS/',country,'/',mics_files[k]))
      
      #match admin area codes
      dat.tmp$admin1 <- dat.tmp$strata <- NA
      dat.tmp$admin1.char <-  ''
      for(perm in 1:nrow(admin.key)){
        perm.ind <- dat.tmp$admin1.name==admin.key$admin1.name[perm]
        dat.tmp$admin1[perm.ind] <- admin.key$admin1[perm]
        dat.tmp$admin1.char[perm.ind] <- admin.key$admin1.char[perm]
        dat.tmp$strata[perm.ind] <- admin.key$strata[perm]
      }
      
      #prepare to merge with DHS data
      dat.tmp$LONGNUM <- dat.tmp$LATNUM <- NA
      dat.tmp$survey.type <- 'MICS'
      dat.tmp <- dat.tmp[,c("cluster",'age','years','total','Y','v005','urban',"LONGNUM","LATNUM",'strata','admin1',
                            'admin1.char','admin1.name','survey','survey.type')]
      
      #add to prepared data
      mod.dat <- rbind(mod.dat,dat.tmp)
    }
    message('Processing MICS data for ', country,' ', unique(dat.tmp$survey),'\n')
  }
}

# Change cluster numbers to get rid of duplicates ----------------------------------------------------------
clusters <- unique(mod.dat[,c("cluster","survey")])
clusters$cluster.new <- 1:nrow(clusters)
mod.dat <- merge(mod.dat,clusters,by=c('cluster','survey'))
mod.dat$cluster <- mod.dat$cluster.new
mod.dat <- mod.dat[,!(names(mod.dat)=='cluster.new')]

survey_years <- sort(unique(mod.dat$survey))
mod.dat$survey.id<- unlist(sapply(1:nrow(mod.dat),function(x){which(mod.dat$survey[x] ==survey_years)}))

# Use raw data to calculate age band intercept priors for benchmarking ----------------------------------------------------------
raw.u5mr <- nrow(raw.dat[raw.dat$b7<60 & raw.dat$b5==0,])/nrow(raw.dat)
int.priors.bench <- c(nrow(raw.dat[raw.dat$b7==0 & raw.dat$b5==0,])/nrow(raw.dat)*(1/raw.u5mr), #<1 month
                      nrow(raw.dat[(raw.dat$b7 %in% 1:11) & raw.dat$b5==0,])/nrow(raw.dat[raw.dat$b7>=1 | raw.dat$b5==1,])*(1/raw.u5mr), #1-11 months  
                      nrow(raw.dat[(raw.dat$b7 %in% 12:23) & raw.dat$b5==0,])/nrow(raw.dat[raw.dat$b7>=12 | raw.dat$b5==1,])*(1/raw.u5mr), #12-23 months  
                      nrow(raw.dat[(raw.dat$b7 %in% 24:35) & raw.dat$b5==0,])/nrow(raw.dat[raw.dat$b7>=24 | raw.dat$b5==1,])*(1/raw.u5mr), #24-35 months  
                      nrow(raw.dat[(raw.dat$b7 %in% 36:47) & raw.dat$b5==0,])/nrow(raw.dat[raw.dat$b7>=36 | raw.dat$b5==1,])*(1/raw.u5mr), #36-47 months  
                      nrow(raw.dat[(raw.dat$b7 %in% 48:59) & raw.dat$b5==0,])/nrow(raw.dat[raw.dat$b7>=48 | raw.dat$b5==1,])*(1/raw.u5mr)) #48-59 months

# Save processed data  ----------------------------------------------------------

save(mod.dat, file = paste0(country,'_cluster_dat.rda'))
save(int.priors.bench, file = paste0(country,'_age_int_priors_bench.rda'))


