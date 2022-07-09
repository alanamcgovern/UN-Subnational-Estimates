rm(list = ls())
# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'

# Load libraries and info ----------------------------------------------------------
options(gsubfn.engine = "R")
library(rgdal)
library(spdep)
library(SUMMER)
library(geosphere)
library(stringr)

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

# Load polygon files ----------------------------------------------------------

setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm0)) # load the national shape file
# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

if(sum(grepl(poly.layer.adm2, list.files(poly.path))) != 0){
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

save(admin1.mat, admin2.mat, file = paste0(poly.path,'/', country, '_Amat.rda')) # save the admin1 and admin2 adjacency matrix
save(admin1.names, admin2.names, file = paste0(poly.path, '/', country, '_Amat_Names.rda')) # save the admin1 and admin2 names


# Process DHS data for each survey year ----------------------------------------------------------

# The codes below first loads the raw DHS data, then it assigns the GPS coordinates to each sampling cluster and admin regions where
# the sampling is conducted and assigns the admin regions where the clusters are located.

for(survey_year in survey_years){
  
    svy.ind <- which(survey_years==survey_year)
    message('Processing data for ', country,' ', survey_year,'\n')
  
    # read DHS data
    dat.tmp <- getBirths(filepath = paste0(survey_year,'/dhsStata/',dhsStata.files[svy.ind]),
                     surveyyear = survey_year,
                     year.cut = seq(beg.year, survey_year + 1, 1),
                     strata = c("v022"), compact = T)

    # retrieve the some columns of the full data
    dat.tmp <- dat.tmp[ ,c("v001", "v024", "time", "total",
                       "age", "v005", "v025", "strata", "died")]

    # specify the name of DHS GPS file, which contains the GPS coordinates of the sampling cluster where the data is sampled
    points.path <- paste0(survey_year, "/dhsFlat/", dhsFlat.files[svy.ind])
    points <- readOGR(dsn = path.expand(points.path), # read the GPS file
                  layer = as.character(dhsFlat.files[svy.ind]))

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
      dat.tmp$admin2 <- dat.tmp$admin2.name <- NA
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
    dat.tmp <- dat.tmp[,c("v001", "age", "time", "total", "died", "v005", 
                      "strata", "v025", "LONGNUM", "LATNUM",
                      "admin1", "admin2", "admin1.char", "admin2.char", "admin1.name", "admin2.name")]
    colnames(dat.tmp) <- c("cluster", "age", "years", "total",
                       "Y", "v005", "strata", "urban", "LONGNUM", "LATNUM",
                       "admin1", "admin2", "admin1.char", "admin2.char", "admin1.name", "admin2.name")
    dat.tmp$survey<-survey_year
    dat.tmp$survey.id<-svy.ind
  
    if(survey_year==survey_years[1]){
      mod.dat <- dat.tmp
    }else{mod.dat <- rbind(mod.dat,dat.tmp)}
  
  }

# Save processed data  ----------------------------------------------------------

save(mod.dat, file = paste0(country,'_cluster_dat.rda'))


