rm(list = ls())
# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.

country <- "Senegal"


# Load libraries and info ----------------------------------------------------------

options(gsubfn.engine = "R")
library(rgdal)
library(raster)
library(rgeos)
library(sqldf)
library(geosphere)
library(Matrix)
library(openxlsx)

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)],
                  collapse = "/")
#data.dir <- paste0(home.dir,'/Data/',country)
data.dir <- "R:/Project/STAB/Senegal"# set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

# Load Polygons ----------------------------------------------------------

setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm0)) # load the national shape file
poly.adm1 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions
if(exists('poly.layer.adm2')){
  poly.adm2 <- readOGR(dsn = poly.path,
                       layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions
  proj4string(poly.adm0) <- proj4string(poly.adm1) <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

if(country=='Uganda'){
  poly.adm1.poly <- SpatialPolygons(poly.adm1@polygons)
  poly.adm1 <- unionSpatialPolygons(poly.adm1.poly,
                                    IDs = match(poly.adm1@data$ADM1_EN,
                                                unique(poly.adm1@data$ADM1_EN)))
  proj4string(poly.adm1) <- proj4string(poly.adm2)
  merge.dat <- poly.adm2@data %>% group_by(ADM1_EN) %>% summarise(n = n(), 
                                                                  ADM1_PCODE = unique(ADM1_PCODE))
  poly.adm1 <- SpatialPolygonsDataFrame(poly.adm1, merge.dat)
  
}

load(paste0(poly.path,'/', country, '_Amat.rda'))
load(paste0(poly.path,'/', country, '_Amat_Names.rda'))

# Load cluster data  ----------------------------------------------------------

setwd(paste0(data.dir))
load(paste0(country,'_cluster_dat.rda'),
     envir = .GlobalEnv)

cluster_list<-mod.dat[!duplicated(mod.dat[c('cluster','survey',
                                            'LONGNUM','LATNUM')]),]

survey_years <- unique(mod.dat$survey)
end.proj.year <- 2021


# function that calculates population in each admin area ----------------------------------------------------------
pop_adm<-function(adm.shp, wp,admin_pop_dat){
  
  # make sure polygons have same crs as population raster
  adm.shp <- spTransform(adm.shp, wp@crs)
  
  # admin level population
  wp.adm.list <- lapply(1:nrow(adm.shp), function(x) {
    list(state_id = x, state_raster = raster::mask(crop(wp,adm.shp[x,]),
                                                   mask = adm.shp[x,]))
  })
  
  # store total population at admin
  pop.adm<-vector()
  for ( j in 1:nrow(adm.shp)){
    pop_j<-wp.adm.list[[j]]
    pop.adm[j]<-sum(values(pop_j$state_raster),na.rm=TRUE)
    
  }
  
  # add admin population 
  admin_pop_dat$admin_pop<-pop.adm
  # not using matching because repeated admin2 names
  # assume order in admin.link is the same as polygon
  
  return (admin_pop_dat)
}

# Download U5 population  ----------------------------------------------------------
#### if not working, try manually download
#### downloading might take a long time, especially for big countries
setwd(paste0(data.dir,'/worldpop'))

pop.year <- beg.year:2020
pop.abbrev <- tolower(gadm.abbrev)

options(timeout = 2000) # adjust this time, should be longer than each download
rigorousFileTest = TRUE # set to TRUE after files have been downloaded to test 
# if files were downloaded correctly, i.e. if they can be loaded into R

for(year in pop.year){
  print(year)
  # includes ages 0-1 years and 1-5 years
  for(age in c(0, 1)){
    for(sex in c("f", "m")){
      file <- paste0(pop.abbrev,'_', sex, '_', age, '_', year,'.tif')
      
      # check if the raster file exists. If rigorousFileTest == TRUE, also check 
      # if the file can be successfully loaded
      goodFile = file.exists(file)
      if(goodFile && rigorousFileTest) {
        goodFile = tryCatch(
          {
            test = raster(file)
            TRUE
          }, 
          error = function(e) {FALSE}
        )
      }
      
      if(!goodFile){
        url <- paste0("https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/", 
                      year, "/", toupper(pop.abbrev), "/", pop.abbrev, "_", 
                      sex, "_", age, "_", year, ".tif")
        download.file(url, file, method = "libcurl",mode="wb")
      }
    }
  }
}

# Prepare U1 and U5 Populations ----------------------------------------------------------


# aggregate the under-five population spatial surface to resolution of 1km*1km
for (year in 2019){
  print(year)
  
  # load worldpop
  setwd(paste0(data.dir,'/Population'))
  file <- paste0(pop.abbrev,'_ppp_',year,'_1km_Aggregated_UNadj.tif')
  if(!file.exists(file)){
    
    url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/", 
                  year, "/", toupper(pop.abbrev),"/",      
                  pop.abbrev,'_ppp_',year,'_1km_Aggregated_UNadj.tif')
    
    download.file(url, file, method = "libcurl",mode="wb")
  }
  
  # UNadjusted population counts
  worldpop <- raster(paste0(country.abbrev,
                            '_ppp_',year,
                            '_1km_Aggregated_UNadj.tif',sep=''))
  
  setwd(paste0(data.dir,'/worldpop'))
  ## first sum up four rasters female 0-1, 1-5, male 0-1, 1-5
  
  f_0_name = paste0(pop.abbrev,'_f_0_',year,'.tif')
  f_1_name = paste0(pop.abbrev,'_f_1_',year,'.tif')
  m_0_name = paste0(pop.abbrev,'_m_0_',year,'.tif')
  m_1_name = paste0(pop.abbrev,'_m_1_',year,'.tif')
  
  
  pop_f_0<-raster(f_0_name)
  pop_f_1<-raster(f_1_name)
  pop_m_0<-raster(m_0_name)
  pop_m_1<-raster(m_1_name)
  
  
  proj4string(pop_f_0) <- proj4string(pop_f_1) <- 
    proj4string(pop_m_0) <- proj4string(pop_m_1) <- 
    proj4string(poly.adm1)
  
  setwd(paste0(data.dir,'/Population'))
  pop_grid<-as.data.frame(coordinates(worldpop))
  colnames(pop_grid)<-c('x','y')
  
  #U1 Population
  pop_u1<- pop_f_0+pop_m_0
  
  writeRaster(pop_u1, overwrite=TRUE,
              paste0(pop.abbrev,'_u1_',year,'_100m','.tif'))
  
  pop_surf<-raster(paste0(country.abbrev,'_u1_',year,'_100m.tif'))
  
  
  pop_u1_aggregate <- aggregate(pop_surf, fact=10,sum)
  
  pop_grid$u1_pop <- raster::extract(pop_u1_aggregate, 
                                     pop_grid[c('x','y')])
  
  u1_pop<-worldpop
  values(u1_pop)<-pop_grid$u1_pop 
  
  writeRaster(u1_pop, overwrite=TRUE,
              paste0(country.abbrev,'_u1_',year,'_1km.tif'))
  
  #U5 Population
  pop_u5<- pop_f_0+pop_f_1+pop_m_0+pop_m_1
  
  writeRaster(pop_u5, overwrite=TRUE,
              paste0(pop.abbrev,'_u5_',year,'_100m','.tif'))
  
  pop_surf<-raster(paste0(country.abbrev,'_u5_',year,'_100m.tif'))
  
  
  pop_u5_aggregate <- aggregate(pop_surf, fact=10,sum)
  
  pop_grid$u5_pop <- raster::extract(pop_u5_aggregate, 
                                     pop_grid[c('x','y')])
  
  u5_pop<-worldpop
  values(u5_pop)<-pop_grid$u5_pop 
  
  writeRaster(u5_pop, overwrite=TRUE,
              paste0(country.abbrev,'_u5_',year,'_1km.tif'))
  
}


# Calculate weights ----------------------------------------------------------

for(year in pop.year){
  
  print(year)
  
  pop_u5<- raster(paste0(data.dir,'/Population/',country.abbrev,'_u5_',year,'_100m','.tif'))
  pop_u1<- raster(paste0(data.dir,'/Population/',country.abbrev,'_u1_',year,'_100m','.tif'))
  
  if(exists('poly.adm2')){
    #Under 5
  # admin 2 population fraction 
  adm2_pop<-pop_adm(adm.shp=poly.adm2,
                     wp=pop_u5,
                     admin_pop_dat=admin2.names)
  
  adm2_pop$proportion <- adm2_pop$admin_pop/sum(adm2_pop$admin_pop)
  adm2_pop <- adm2_pop[,c('Internal','proportion')]
  colnames(adm2_pop) <- c('region','proportion')
  adm2_pop$years <- year
  
  if(year==pop.year[1]){
    weight.adm2.u5 <- adm2_pop
  }else{
    weight.adm2.u5 <- rbind(weight.adm2.u5,adm2_pop)
  }
  
  #Under 1 (best approximation for NMR)
  # admin 2 population fraction 
  adm2_pop<-pop_adm(adm.shp=poly.adm2,
                    wp=pop_u1,
                    admin_pop_dat=admin2.names)
  
  adm2_pop$proportion <- adm2_pop$admin_pop/sum(adm2_pop$admin_pop)
  adm2_pop <- adm2_pop[,c('Internal','proportion')]
  colnames(adm2_pop) <- c('region','proportion')
  adm2_pop$years <- year
  
  if(year==pop.year[1]){
    weight.adm2.u1 <- adm2_pop
  }else{
    weight.adm2.u1 <- rbind(weight.adm2.u1,adm2_pop)
  }
}
  #Under 5
  # admin 1 population fraction 
  adm1_pop<-pop_adm(adm.shp=poly.adm1,
                    wp=pop_u5,
                    admin_pop_dat=admin1.names)
  
  adm1_pop$proportion <- adm1_pop$admin_pop/sum(adm1_pop$admin_pop)
  adm1_pop <- adm1_pop[,c('Internal','proportion')]
  colnames(adm1_pop) <- c('region','proportion')
  adm1_pop$years <- year
  
  if(year==pop.year[1]){
    weight.adm1.u5 <- adm1_pop
  }else{
    weight.adm1.u5 <- rbind(weight.adm1.u5,adm1_pop)
  }
  
  #Under 1 (best approximation for NMR)
  # admin 1 population fraction 
  adm1_pop<-pop_adm(adm.shp=poly.adm1,
                    wp=pop_u1,
                    admin_pop_dat=admin1.names)
  
  adm1_pop$proportion <- adm1_pop$admin_pop/sum(adm1_pop$admin_pop)
  adm1_pop <- adm1_pop[,c('Internal','proportion')]
  colnames(adm1_pop) <- c('region','proportion')
  adm1_pop$years <- year
  
  if(year==pop.year[1]){
    weight.adm1.u1 <- adm1_pop
  }else{
    weight.adm1.u1 <- rbind(weight.adm1.u1,adm1_pop)
  }
  
}

# use 2020 weights for any future years
if(end.proj.year>2020){
  weight.adm1.u1 <- rbind(weight.adm1.u1,data.frame(region=rep(weight.adm1.u1[weight.adm1.u1$years==2020,]$region,(end.proj.year - 2020)),
                                                    proportion=rep(weight.adm1.u1[weight.adm1.u1$years==2020,]$proportion,(end.proj.year - 2020)),
                                                    years=sort(rep(2021:end.proj.year,length(admin1.names$GADM)))))
  weight.adm1.u5 <- rbind(weight.adm1.u5,data.frame(region=rep(weight.adm1.u5[weight.adm1.u5$years==2020,]$region,(end.proj.year - 2020)),
                                                    proportion=rep(weight.adm1.u5[weight.adm1.u5$years==2020,]$proportion,(end.proj.year - 2020)),
                                                    years=sort(rep(2021:end.proj.year,length(admin1.names$GADM)))))
if(exists('poly.layer.adm2')){
  weight.adm2.u1 <- rbind(weight.adm2.u1,data.frame(region=rep(weight.adm2.u1[weight.adm2.u1$years==2020,]$region,(end.proj.year - 2020)),
                                                    proportion=rep(weight.adm2.u1[weight.adm2.u1$years==2020,]$proportion,(end.proj.year - 2020)),
                                                    years=sort(rep(2021:end.proj.year,length(admin2.names$GADM)))))
  weight.adm2.u5 <- rbind(weight.adm2.u5,data.frame(region=rep(weight.adm2.u5[weight.adm2.u5$years==2020,]$region,(end.proj.year - 2020)),
                                                    proportion=rep(weight.adm2.u5[weight.adm2.u5$years==2020,]$proportion,(end.proj.year - 2020)),
                                                    years=sort(rep(2021:end.proj.year,length(admin2.names$GADM)))))
}
}

save(weight.adm1.u1,file=paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
save(weight.adm1.u5,file=paste0(data.dir,'/worldpop/adm1_weights_u5.rda'))
if(exists('poly.layer.adm2')){
save(weight.adm2.u1,file=paste0(data.dir,'/worldpop/adm2_weights_u1.rda'))
save(weight.adm2.u5,file=paste0(data.dir,'/worldpop/adm2_weights_u5.rda'))
}

# Get map plots of population weights ----------------------------------------------------------

load(paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
load(paste0(data.dir,'/worldpop/adm1_weights_u5.rda'))
if(exists('poly.layer.adm2')){
  load(paste0(data.dir,'/worldpop/adm2_weights_u1.rda'))
  load(paste0(data.dir,'/worldpop/adm2_weights_u5.rda'))
}

pdf(paste0(data.dir,'/worldpop/admin1_u1_weights.pdf'))
{
  weight.adm1.u1$regionPlot <- admin1.names$GADM[match(weight.adm1.u1$region,admin1.names$Internal)]
print(SUMMER::mapPlot(data = weight.adm1.u1,
                      is.long = T, 
                      variables = "years", 
                      values = "proportion",
                      direction = -1,
                      geo = poly.adm1,
                      ncol = 5,
                      legend.label = "Population weight",
                      per1000 = FALSE,
                      by.data = "regionPlot",
                      by.geo = sub(".*data[$]","",poly.label.adm1)))
}
dev.off()

pdf(paste0(data.dir,'/worldpop/admin1_u5_weights.pdf'))
{
  weight.adm1.u5$regionPlot <- admin1.names$GADM[match(weight.adm1.u1$region,admin1.names$Internal)]
  print(SUMMER::mapPlot(data = weight.adm1.u5,
                        is.long = T, 
                        variables = "years", 
                        values = "proportion",
                        direction = -1,
                        geo = poly.adm1,
                        ncol = 5,
                        legend.label = "Population weight",
                        per1000 = FALSE,
                        by.data = "regionPlot",
                        by.geo = sub(".*data[$]","",poly.label.adm1)))
}
dev.off()

pdf(paste0(data.dir,'/worldpop/admin2_u1_weights.pdf'))
{
  weight.adm2.u1$regionPlot <- admin2.names$GADM[match(weight.adm2.u1$region,admin2.names$Internal)]
  print(SUMMER::mapPlot(data = weight.adm2.u1,
                        is.long = T, 
                        variables = "years", 
                        values = "proportion",
                        direction = -1,
                        geo = poly.adm2,
                        ncol = 5,
                        legend.label = "Population weight",
                        per1000 = FALSE,
                        by.data = "regionPlot",
                        by.geo = sub(".*data[$]","",poly.label.adm2)))
}
dev.off()

pdf(paste0(data.dir,'/worldpop/admin2_u5_weights.pdf'))
{
  weight.adm2.u5$regionPlot <- admin2.names$GADM[match(weight.adm2.u1$region,admin2.names$Internal)]
  print(SUMMER::mapPlot(data = weight.adm2.u5,
                        is.long = T, 
                        variables = "years", 
                        values = "proportion",
                        direction = -1,
                        geo = poly.adm2,
                        ncol = 5,
                        legend.label = "Population weight",
                        per1000 = FALSE,
                        by.data = "regionPlot",
                        by.geo = sub(".*data[$]","",poly.label.adm2)))
}
dev.off()
