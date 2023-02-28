rm(list = ls())

# Load libraries and info ----------------------------------------------------------
# options(gsubfn.engine = "R")
library(rgdal)
library(spdep)
library(maptools)
library(tidyverse)

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/shapeFiles_gadm') # set the directory to store the data
setwd(data.dir)

#GADM countries -----------------------
countries.gadm <- c('Angola','Bangladesh','Benin',
                    'Cameroon','Chad','Ethiopia',
                    'Guinea','Haiti','Kenya','Laos','Lesotho','Liberia',
                    'Madagascar','Mali','Mauritania','Mozambique','Myanmar',
                    'Namibia','Nigeria','Rwanda','Senegal',
                    'Togo','Tanzania','Zambia','Zimbabwe')
for(country in countries.gadm){
  info.name <- paste0(country, "_general_info.Rdata")
  load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info
  
  gadm.link <- paste0("gadm41_",gadm.abbrev,'_shp.zip')
  poly.path <- paste0(data.dir,"/gadm41_",gadm.abbrev,'_shp')
  
  if(!file.exists(gsub('.zip','',gadm.link))){
    usethis:::tidy_download(paste0("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/", gadm.link))
    usethis:::tidy_unzip(paste0(gadm.link),cleanup = T) 
  }
  
  # use encoding to read special characters
  poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                       layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions
  
  if(exists('poly.layer.adm2')){
    poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                         layer = as.character(poly.layer.adm2))} # load the shape file of admin-2 regions
  
  # set coordinate reference system to be equal
  if(exists("poly.adm2")){
    proj4string(poly.adm1)  <- proj4string(poly.adm2)
  }
  
  # create the adjacency matrix for admin1 regions.
  admin1.mat <- poly2nb(SpatialPolygons(poly.adm1@polygons))
  admin1.mat <- nb2mat(admin1.mat, zero.policy = TRUE)
  colnames(admin1.mat) <- rownames(admin1.mat) <- paste0("admin1_", 1:dim(admin1.mat)[1])
  admin1.names <- data.frame(admin1.name = eval(str2lang(poly.label.adm1)),
                             admin1.char = rownames(admin1.mat))
  
  if(exists("poly.adm2")){  # create the adjacency matrix for admin2 regions.
    admin2.mat <- poly2nb(SpatialPolygons(poly.adm2@polygons))
    admin2.mat <- nb2mat(admin2.mat, zero.policy = TRUE)
    colnames(admin2.mat) <- rownames(admin2.mat) <- paste0("admin2_", 1:dim(admin2.mat)[1])
    admin2.names <- data.frame(admin2.name = eval(str2lang(poly.label.adm2)),
                               admin2.char = rownames(admin2.mat),
                               admin1.name = poly.adm2@data$NAME_1)
  }
  
  #create link dataframe and record
  if(exists("poly.adm2")){
    adm_link_tmp <- merge(admin1.names,admin2.names,by='admin1.name')
  }else{
    adm_link_tmp <- data.frame(admin1.names,admin2.name=NA,admin2.char=NA)
  }
  if(exists('adm_link')){
    adm_link <- rbind(adm_link,data.frame(country, adm_link_tmp))
  }else{
    adm_link <- data.frame(country, adm_link_tmp)
  }
  
  #clean up
  rm(poly.adm1, admin1.mat,admin1.names,
     country,country.abbrev,gadm.abbrev,gadm.link,
     poly.label.adm1,poly.layer.adm0,poly.layer.adm1)
  if(exists('poly.adm2')){
    rm(poly.adm2,admin2.mat,admin2.names,
       poly.label.adm2,poly.layer.adm2)
  }
}

#Special countries ------------------------
## Burundi ----------------
country <- 'Burundi'
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info
poly.path <- paste0(home.dir,"/Data/shapeFiles_alt/Burundi/shapeFiles")

# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                       layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions

# set coordinate reference system to be equal
proj4string(poly.adm1)  <- proj4string(poly.adm2)

# create the adjacency matrix for admin1 regions.
admin1.mat <- poly2nb(SpatialPolygons(poly.adm1@polygons))
admin1.mat <- nb2mat(admin1.mat, zero.policy = TRUE)
colnames(admin1.mat) <- rownames(admin1.mat) <- paste0("admin1_", 1:dim(admin1.mat)[1])
admin1.names <- data.frame(admin1.name = eval(str2lang(poly.label.adm1)),
                           admin1.char = rownames(admin1.mat))

# create the adjacency matrix for admin2 regions.
admin2.mat <- poly2nb(SpatialPolygons(poly.adm2@polygons))
admin2.mat <- nb2mat(admin2.mat, zero.policy = TRUE)
colnames(admin2.mat) <- rownames(admin2.mat) <- paste0("admin2_", 1:dim(admin2.mat)[1])
admin2.names <- data.frame(admin2.name = eval(str2lang(poly.label.adm2)),
                             admin2.char = rownames(admin2.mat),
                             admin1.name = poly.adm2@data$admin1Name)

adm_link_tmp <- merge(admin1.names,admin2.names,by='admin1.name')
adm_link <- rbind(adm_link,data.frame(country, adm_link_tmp))

#clean up
rm(poly.adm1, admin1.mat,admin1.names,
   country,country.abbrev,gadm.abbrev,
   poly.label.adm1,poly.layer.adm0,poly.layer.adm1,
   poly.adm2,admin2.mat,admin2.names,
   poly.label.adm2,poly.layer.adm2)


## Ghana -----------------------------
country <- 'Ghana'
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

gadm.link <- paste0("gadm40_",gadm.abbrev,'_shp.zip')
poly.path <- paste0(data.dir,"/gadm40_",gadm.abbrev,'_shp')

if(!file.exists(gsub('.zip','',gadm.link))){
  usethis:::tidy_download(paste0("https://geodata.ucdavis.edu/gadm/gadm4.0/shp/", gadm.link))
  usethis:::tidy_unzip(paste0(gadm.link),cleanup = T) 
}

# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

# create the adjacency matrix for admin1 regions.
admin1.mat <- poly2nb(SpatialPolygons(poly.adm1@polygons))
admin1.mat <- nb2mat(admin1.mat, zero.policy = TRUE)
colnames(admin1.mat) <- rownames(admin1.mat) <- paste0("admin1_", 1:dim(admin1.mat)[1])
admin1.names <- data.frame(admin1.name = eval(str2lang(poly.label.adm1)),
                           admin1.char = rownames(admin1.mat))
adm_link_tmp <- data.frame(admin1.names,admin2.name=NA,admin2.char=NA)
adm_link <- rbind(adm_link,data.frame(country, adm_link_tmp))

#clean up
rm(poly.adm1, admin1.mat,admin1.names,
   country,country.abbrev,gadm.abbrev,gadm.link,
   poly.label.adm1,poly.layer.adm0,poly.layer.adm1)
if(exists('poly.adm2')){
  rm(poly.adm2,admin2.mat,admin2.names,
     poly.label.adm2,poly.layer.adm2)
}

## Malawi ---------------------
country <- 'Malawi'
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info
poly.path <- paste0(home.dir,"/Data/shapeFiles_alt/Malawi/shapeFiles_gadm")

# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions

# set coordinate reference system to be equal
proj4string(poly.adm1)  <- proj4string(poly.adm2)

# create the adjacency matrix for admin1 regions.
admin1.mat <- poly2nb(SpatialPolygons(poly.adm1@polygons))
admin1.mat <- nb2mat(admin1.mat, zero.policy = TRUE)
colnames(admin1.mat) <- rownames(admin1.mat) <- paste0("admin1_", 1:dim(admin1.mat)[1])
admin1.names <- data.frame(admin1.name = eval(str2lang(poly.label.adm1)),
                           admin1.char = rownames(admin1.mat))

#read in admin1 to admin2 sheet
adm_name_key <- read.csv(paste0(poly.path,'/MWI_adm1_to_adm2.csv'))
admin2.names <- data.frame(admin2.name = adm_name_key$Admin.2,
                           admin2.char = paste0("admin2_", 1:dim(adm_name_key)[1]),
                           admin1.name = adm_name_key$Admin.1)

adm_link_tmp <- merge(admin1.names,admin2.names,by='admin1.name')
adm_link <- rbind(adm_link,data.frame(country, adm_link_tmp))

#clean up
rm(poly.adm1, admin1.mat,admin1.names,
   country,country.abbrev,gadm.abbrev,
   poly.label.adm1,poly.layer.adm0,poly.layer.adm1,
   poly.adm2,admin2.mat,admin2.names,
   poly.label.adm2,poly.layer.adm2)
## Nepal ----------------
country <- 'Nepal'
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info
poly.path <- paste0(home.dir,"/Data/shapeFiles_alt/Nepal/shapeFiles")

# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions

# set coordinate reference system to be equal
proj4string(poly.adm1)  <- proj4string(poly.adm2)

# create the adjacency matrix for admin1 regions.
admin1.mat <- poly2nb(SpatialPolygons(poly.adm1@polygons))
admin1.mat <- nb2mat(admin1.mat, zero.policy = TRUE)
colnames(admin1.mat) <- rownames(admin1.mat) <- paste0("admin1_", 1:dim(admin1.mat)[1])
admin1.names <- data.frame(admin1.name = paste0("District #", poly.adm1@data$ADM1_EN),
                           admin1.char = rownames(admin1.mat))

# create the adjacency matrix for admin2 regions.
admin2.mat <- poly2nb(SpatialPolygons(poly.adm2@polygons))
admin2.mat <- nb2mat(admin2.mat, zero.policy = TRUE)
colnames(admin2.mat) <- rownames(admin2.mat) <- paste0("admin2_", 1:dim(admin2.mat)[1])
admin2.names <- data.frame(admin2.name = eval(str2lang(poly.label.adm2)),
                           admin2.char = rownames(admin2.mat),
                           admin1.name = paste0("District #",poly.adm2@data$ADM1_EN))

adm_link_tmp <- merge(admin1.names,admin2.names,by='admin1.name')
adm_link <- rbind(adm_link,data.frame(country, adm_link_tmp))

#clean up
rm(poly.adm1, admin1.mat,admin1.names,
   country,country.abbrev,gadm.abbrev,
   poly.label.adm1,poly.layer.adm0,poly.layer.adm1,
   poly.adm2,admin2.mat,admin2.names,
   poly.label.adm2,poly.layer.adm2)


## Pakistan  -------------------
country <- 'Pakistan'
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

gadm.link <- paste0("gadm41_",gadm.abbrev,'_shp.zip')
poly.path <- paste0(data.dir,"/gadm41_",gadm.abbrev,'_shp')

if(!file.exists(gsub('.zip','',gadm.link))){
  usethis:::tidy_download(paste0("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/", gadm.link))
  usethis:::tidy_unzip(paste0(gadm.link),cleanup = T) 
}

# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions
# use encoding to read special characters
poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm2)) # load the shape file of admin-1 regions
# set coordinate reference system to be equal
proj4string(poly.adm1)  <- proj4string(poly.adm2)


# create the adjacency matrix for admin1 regions.
admin1.mat <- poly2nb(SpatialPolygons(poly.adm1@polygons))
admin1.mat <- nb2mat(admin1.mat, zero.policy = TRUE)
colnames(admin1.mat) <- rownames(admin1.mat) <- paste0("admin1_", 1:dim(admin1.mat)[1])
admin1.names <- data.frame(admin1.name = eval(str2lang(poly.label.adm1)),
                           admin1.char = rownames(admin1.mat))
admin1.names <- admin1.names[!(admin1.names$admin1.name %in% c("Azad Kashmir", "Northern Areas","Gilgit-Baltistan")),]

# create the adjacency matrix for admin2 regions.
admin2.mat <- poly2nb(SpatialPolygons(poly.adm2@polygons))
admin2.mat <- nb2mat(admin2.mat, zero.policy = TRUE)
colnames(admin2.mat) <- rownames(admin2.mat) <- paste0("admin2_", 1:dim(admin2.mat)[1])
admin2.names <- data.frame(admin2.name = eval(str2lang(poly.label.adm2)),
                           admin2.char = rownames(admin2.mat),
                           admin1.name = poly.adm2@data$NAME_1)
admin2.names <- admin2.names[!(admin2.names$admin2.name %in% c("Azad Kashmir", "Northern Areas","Gilgit-Baltistan")),]

adm_link_tmp <- merge(admin1.names,admin2.names,by='admin1.name')
adm_link <- rbind(adm_link,data.frame(country, adm_link_tmp))

#clean up
rm(poly.adm1, admin1.mat,admin1.names,
   country,country.abbrev,gadm.abbrev,gadm.link,
   poly.label.adm1,poly.layer.adm0,poly.layer.adm1)
if(exists('poly.adm2')){
  rm(poly.adm2,admin2.mat,admin2.names,
     poly.label.adm2,poly.layer.adm2)
}


## Sierra Leone ----------------
country <- 'Sierra_Leone'
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info
poly.path <- paste0(home.dir,"/Data/shapeFiles_alt/Sierra_Leone/shapeFiles")

# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions

# set coordinate reference system to be equal
proj4string(poly.adm1)  <- proj4string(poly.adm2)

# create the adjacency matrix for admin1 regions.
admin1.mat <- poly2nb(SpatialPolygons(poly.adm1@polygons))
admin1.mat <- nb2mat(admin1.mat, zero.policy = TRUE)
colnames(admin1.mat) <- rownames(admin1.mat) <- paste0("admin1_", 1:dim(admin1.mat)[1])
admin1.names <- data.frame(admin1.name = eval(str2lang(poly.label.adm1)),
                           admin1.char = rownames(admin1.mat))

# create the adjacency matrix for admin2 regions.
admin2.mat <- poly2nb(SpatialPolygons(poly.adm2@polygons))
admin2.mat <- nb2mat(admin2.mat, zero.policy = TRUE)
colnames(admin2.mat) <- rownames(admin2.mat) <- paste0("admin2_", 1:dim(admin2.mat)[1])
admin2.names <- data.frame(admin2.name = eval(str2lang(poly.label.adm2)),
                           admin2.char = rownames(admin2.mat),
                           admin1.name = poly.adm2@data$district_2)
admin2.names[admin2.names$admin1.name!='Western',]$admin1.name <- paste0(admin2.names[admin2.names$admin1.name!='Western',]$admin1.name,' Province')
admin2.names[admin2.names$admin1.name=='Western',]$admin1.name <- paste0(admin2.names[admin2.names$admin1.name=='Western',]$admin1.name,' Area')

adm_link_tmp <- merge(admin1.names,admin2.names,by='admin1.name')
adm_link <- rbind(adm_link,data.frame(country, adm_link_tmp))

#clean up
rm(poly.adm1, admin1.mat,admin1.names,
   country,country.abbrev,gadm.abbrev,
   poly.label.adm1,poly.layer.adm0,poly.layer.adm1,
   poly.adm2,admin2.mat,admin2.names,
   poly.label.adm2,poly.layer.adm2)
## Uganda ---------------------------
country <- 'Uganda'
poly.path <- paste0(home.dir,"/Data/shapeFiles_alt/Uganda/shapeFiles")

poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = "uga_admbnda_adm2_2020") # load the shape file of admin-1 regions
poly.adm1 <- poly.adm2
poly.adm1.poly <- SpatialPolygons(poly.adm1@polygons)
poly.adm1 <- unionSpatialPolygons(poly.adm1.poly,
                                  IDs = match(poly.adm1@data$ADM1_EN,
                                              unique(poly.adm1@data$ADM1_EN)))
proj4string(poly.adm1) <- proj4string(poly.adm2)
merge.dat <- poly.adm2@data %>% group_by(ADM1_EN) %>% summarise(n = n(), 
                                                                ADM1_PCODE = unique(ADM1_PCODE))
poly.adm1 <- SpatialPolygonsDataFrame(poly.adm1, merge.dat)

# create the adjacency matrix for admin1 regions.
admin1.mat <- poly2nb(SpatialPolygons(poly.adm1@polygons))
admin1.mat <- nb2mat(admin1.mat, zero.policy = TRUE)
colnames(admin1.mat) <- rownames(admin1.mat) <- paste0("admin1_", 1:dim(admin1.mat)[1])
admin1.names <- data.frame(admin1.name = poly.adm1@data$ADM1_EN,
                           admin1.char = rownames(admin1.mat))

if(exists("poly.adm2")){  # create the adjacency matrix for admin2 regions.
  admin2.mat <- poly2nb(SpatialPolygons(poly.adm2@polygons))
  admin2.mat <- nb2mat(admin2.mat, zero.policy = TRUE)
  colnames(admin2.mat) <- rownames(admin2.mat) <- paste0("admin2_", 1:dim(admin2.mat)[1])
  admin2.names <- data.frame(admin2.name = poly.adm2@data$ADM2_EN,
                             admin2.char = rownames(admin2.mat),
                             admin1.name = poly.adm2@data$ADM1_EN)
}

adm_link_tmp <- merge(admin1.names,admin2.names,by='admin1.name')
adm_link <- rbind(adm_link,data.frame(country, adm_link_tmp))

rm(poly.adm1,poly.adm2,admin1.names,admin2.names,admin1.mat,admin2.mat)







openxlsx::write.xlsx(adm_link, file='Admin1_Admin2_Key.xlsx')
