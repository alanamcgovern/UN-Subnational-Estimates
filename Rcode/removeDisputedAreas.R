# This script edits shapefiles to remove disputed areas
## will work for any set of GADM shapefiles

rm(list = ls())
# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Pakistan'
disputed_regions <- c("Azad Kashmir", "Northern Areas")

# Load libraries and info ----------------------------------------------------------
library(SUMMER)
#help(package = "SUMMER", help_type = "html")
#utils::browseVignettes(package = "SUMMER")
library(classInt)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(rgdal)
library(scales)
library(INLA)
library(survey)
library(ggplot2)
library(gridExtra)
library(parallel)
library(cartography)
library(rgeos)

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
info.name <- paste0(country, "_general_info.Rdata")
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

# Load polygon files  ------------------------------------------------------

setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm0)) # load the national shape file
# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions
if(exists("poly.layer.adm2")){
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

#### delete disputed regions from admin2 polygon files and neighbor matrices, resave####

# delete regions from polygon files
poly.adm1_excluding_disputed <- poly.adm1[!(poly.adm1$NAME_1 %in% disputed_regions),]
## save new file
writeOGR(poly.adm1_excluding_disputed,
         dsn = poly.path,
         layer = paste0(poly.layer.adm1, "_excluding_disputed"),
         driver = "ESRI Shapefile")
poly.adm2_excluding_disputed <- poly.adm2[!(poly.adm2$NAME_2 %in% disputed_regions),]
## save new file
writeOGR(poly.adm2_excluding_disputed,
         dsn = poly.path,
         layer = paste0(poly.layer.adm2, "_excluding_disputed"),
         driver = "ESRI Shapefile")

# delete regions from neighbor matrices
admin1.names_excluding_disputed <- admin1.names[!(admin1.names$GADM %in% disputed_regions),]
admin1.mat_excluding_disputed <- admin1.mat[which(rownames(admin1.mat) %in% 
                                                    as.character(admin1.names_excluding_disputed$Internal)),
                                            which(rownames(admin1.mat) %in% 
                                                    as.character(admin1.names_excluding_disputed$Internal))]

admin2.names_excluding_disputed <- admin2.names[!(admin2.names$GADM %in% disputed_regions),]
admin2.mat_excluding_disputed <- admin2.mat[which(rownames(admin2.mat) %in% 
                                                    as.character(admin2.names_excluding_disputed$Internal)),
                                            which(rownames(admin2.mat) %in% 
                                                    as.character(admin2.names_excluding_disputed$Internal))]

save(admin1.mat_excluding_disputed,admin2.mat_excluding_disputed,file=paste0(poly.path,'/', country, '_Amat_excluding_disputed.rda'))
save(admin1.names_excluding_disputed,admin2.names_excluding_disputed,file=paste0(poly.path,'/', country, '_Amat_names_excluding_disputed.rda'))

## remake maps ------------------------------------------------------

# resave maps
pdf(paste0(res.dir,'/Figures/ShapeCheck/', country, '_adm1_neighb_excluding_disputed.pdf'))
cent <- getSpPPolygonsLabptSlots(poly.adm1_excluding_disputed)
cols <- rainbow(10)
plot(poly.adm1_excluding_disputed, col = cols, border = F, axes = F,)
for(i in 1:dim(cent)[1]){
  neighbs <- which(admin1.mat_excluding_disputed[i,] != 0)
  if(length(neighbs) != 0){
    for(j in 1:length(neighbs)){
      ends <- cent[neighbs,]
      segments(x0 = cent[i, 1], y0 = cent[i, 2],
               x1 = cent[neighbs[j], 1], y1 = cent[neighbs[j], 2], col = 'black')
    }
  }
}
dev.off()

pdf(paste0(res.dir, '/Figures/ShapeCheck/', country, '_adm2_neighb_excluding_disputed.pdf'))
cent <- getSpPPolygonsLabptSlots(poly.adm2_excluding_disputed)
cols <- rainbow(10)
plot(poly.adm2_excluding_disputed, col = cols, border = F, axes = F,)
for(i in 1:dim(cent)[1]){
  neighbs <- which(admin2.mat_excluding_disputed[i,] != 0)
  if(length(neighbs) != 0){
    for(j in 1:length(neighbs)){
      ends <- cent[neighbs,]
      segments(x0 = cent[i, 1], y0 = cent[i, 2],
               x1 = cent[neighbs[j], 1], y1 = cent[neighbs[j], 2], col = 'black')
    }
  }
}
dev.off()

centroids <- gCentroid(poly.adm1_excluding_disputed, byid = TRUE,
                       id = poly.adm1_excluding_disputed@data$GID_1)
pdf(paste0(res.dir, '/Figures/ShapeCheck/',
           country,'_Admin1Names_excluding_disputed.pdf'))
par(lend=1)
plot(poly.adm1_excluding_disputed,
     xlim = poly.adm1_excluding_disputed@bbox['x',],
     ylim = poly.adm1_excluding_disputed@bbox['y',],
     axes = F)
text(centroids$x, centroids$y,
     labels = poly.adm1_excluding_disputed@data$NAME_1,
     cex = 0.45)
dev.off()

centroids <- gCentroid(poly.adm2_excluding_disputed, byid = TRUE,
                       id = poly.adm2_excluding_disputed@data$GID_2)
pdf(paste0(res.dir, '/Figures/ShapeCheck/',
           country,'_Admin2Names_excluding_disputed.pdf'))
par(lend=1)
plot(poly.adm2_excluding_disputed,
     xlim = poly.adm2_excluding_disputed@bbox['x',],
     ylim = poly.adm2_excluding_disputed@bbox['y',],
     axes = F)
text(centroids$x, centroids$y,
     labels = poly.adm2_excluding_disputed@data$NAME_2,
     cex = 0.45)
dev.off()


