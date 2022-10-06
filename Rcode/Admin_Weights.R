rm(list = ls())
# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Myanmar'

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

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
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

load(paste0(poly.path,'/', country, '_Amat.rda'))
load(paste0(poly.path,'/', country, '_Amat_Names.rda'))

# Load cluster data  ----------------------------------------------------------

setwd(paste0(data.dir))
load(paste0(country,'_cluster_dat.rda'),
     envir = .GlobalEnv)

cluster_list<-mod.dat[!duplicated(mod.dat[c('cluster','survey',
                                            'LONGNUM','LATNUM')]),]

survey_years <- unique(mod.dat$survey)

if(max(survey_years)>2018){
  end.proj.year <- 2022
}else{
  end.proj.year <- 2020
}

# function that calculates population in each admin2 area ----------------------------------------------------------
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

# link across admin1 and admin2 ----------------------------------------------------------
if(exists('poly.layer.adm2')){
  adm_link <- unique(mod.dat[,c('admin1.name','admin2.name','admin1.char','admin2.char')])
  admin2.names$admin2.name <- admin2.names$GADM
  admin2.names$order <- 1:nrow(admin2.names)
  adm_link <- merge(adm_link,admin2.names,by='admin2.name')
  adm_link$admin2_idx<- adm_link$Internal
  adm_link$admin1_idx <- adm_link$admin1.char
  adm_link <- adm_link[order(adm_link$order),]
}else{
  adm_link <- unique(mod.dat[,c('admin1.name','admin1.char')])
  admin1.names$admin1.name <- admin1.names$GADM
  admin1.names$order <- 1:nrow(admin1.names)
  adm_link <- merge(adm_link,admin1.names,by='admin1.name')
  adm_link$admin1_idx <- adm_link$admin1.char
  adm_link <- adm_link[order(adm_link$order),]
}

# Download U5 population  ----------------------------------------------------------
#### if not working, try manually download
#### downloading might take a long time, especially for big countries
setwd(paste0(data.dir,'/worldpop'))

pop.year <- beg.year:2020
pop.abbrev <- tolower(gadm.abbrev)

options(timeout = 1000) # adjust this time, should be longer than each download
for(year in pop.year){
  print(year)
  # includes ages 0-1 years and 1-5 years
  for(age in c(0, 1)){
    for(sex in c("f", "m")){
      file <- paste0(pop.abbrev,'_', sex, '_', age, '_', year,'.tif')
      if(!file.exists(file)){
        url <- paste0("https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/", 
                      year, "/", toupper(pop.abbrev), "/", pop.abbrev, "_", 
                      sex, "_", age, "_", year, ".tif")
        download.file(url, file, method = "libcurl",mode="wb")
      }
    }
  }
}

# Prepare U1 and U5 Populations ----------------------------------------------------------

pop.year <- beg.year:2020
pop.abbrev <- tolower(gadm.abbrev)
# aggregate the under-five population spatial surface to resolution of 1km*1km
for (year in pop.year){
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


# calculate weights ----------------------------------------------------------

for(year in pop.year){
  
  print(year)
  
  pop_u5<- raster(paste0(data.dir,'/Population/',country.abbrev,'_u5_',year,'_100m','.tif'))
  pop_u1<- raster(paste0(data.dir,'/Population/',country.abbrev,'_u1_',year,'_100m','.tif'))
  
  if(exists('poly.adm2')){
    #Under 5
  # admin 2 population fraction 
  adm2_pop<-pop_adm(adm.shp=poly.adm2,
                     wp=pop_u5,
                     admin_pop_dat=adm_link)
  
  adm2_pop$admin2_pop <- adm2_pop$admin_pop 
  adm2_pop<-adm2_pop %>% 
    group_by(admin1_idx) %>% 
    mutate(admin1_pop = sum(admin2_pop))
  
  # fraction of admin2 w.r.t. admin1
  adm2_pop$admin2_frac<-adm2_pop$admin2_pop/
    adm2_pop$admin1_pop
  
  # sanity check, fraction for admin2 in each admin1 sum up to 1
  aggregate(admin2_frac~admin1_idx, data = adm2_pop, FUN = sum)
  
  adm2.pop <- adm2_pop
  adm2.pop$proportion <- adm2.pop$admin2_pop/sum(adm2.pop$admin2_pop)
  adm2.pop <- adm2.pop[,c('admin2.char','proportion')]
  colnames(adm2.pop) <- c('region','proportion')
  adm2.pop$years <- year
  
  if(year==pop.year[1]){
    weight.adm2.u5 <- adm2.pop
  }else{
    weight.adm2.u5 <- rbind(weight.adm2.u5,adm2.pop)
  }
  
  ### admin-1 level population fraction w.r.t. natl population
  adm1.pop<-adm2_pop[!duplicated(adm2_pop[,c('admin1_idx')]),]
  # create an ordered admin1 list
  match.order = match(paste("admin1", 1: nrow(adm1.pop), 
                            sep = "_"), adm1.pop$admin1_idx)
  adm1.pop = adm1.pop[match.order, ]
  adm1.pop$proportion <-adm1.pop$admin1_pop/sum(adm1.pop$admin1_pop)
  adm1.pop<-adm1.pop[,c('admin1.char','proportion')]
  colnames(adm1.pop) <- c('region','proportion')
  adm1.pop$years <- year
  
  if(year==pop.year[1]){
    weight.adm1.u5 <- adm1.pop
  }else{
    weight.adm1.u5 <- rbind(weight.adm1.u5,adm1.pop)
  }
  
  #Under 1 (best approximation for NMR)
  
  # admin 2 population fraction 
  adm2_pop<-pop_adm(adm.shp=poly.adm2,
                     wp=pop_u1,
                     admin_pop_dat=adm_link)
  adm2_pop$admin2_pop <- adm2_pop$admin_pop 
  
  adm2_pop<-adm2_pop %>% 
    group_by(admin1_idx) %>% 
    mutate(admin1_pop = sum(admin2_pop))
  
  # fraction of admin2 w.r.t. admin1
  adm2_pop$admin2_frac<-adm2_pop$admin2_pop/
    adm2_pop$admin1_pop
  
  # sanity check, fraction for admin2 in each admin1 sum up to 1
  aggregate(admin2_frac~admin1_idx, data = adm2_pop, FUN = sum)
  
  adm2.pop <- adm2_pop
  adm2.pop$proportion <- adm2.pop$admin2_pop/sum(adm2.pop$admin2_pop)
  adm2.pop <- adm2.pop[,c('admin2.char','proportion')]
  colnames(adm2.pop) <- c('region','proportion')
  adm2.pop$years <- year
  
  if(year==pop.year[1]){
    weight.adm2.u1 <- adm2.pop
  }else{
    weight.adm2.u1 <- rbind(weight.adm2.u1,adm2.pop)
  }
  
  ### admin-1 level population fraction w.r.t. natl population
  adm1.pop<-adm2_pop[!duplicated(adm2_pop[,c('admin1_idx')]),]
  # create an ordered admin1 list
  match.order = match(paste("admin1", 1: nrow(adm1.pop), 
                            sep = "_"), adm1.pop$admin1_idx)
  adm1.pop = adm1.pop[match.order, ]
  adm1.pop$proportion <-adm1.pop$admin1_pop/sum(adm1.pop$admin1_pop)
  adm1.pop<-adm1.pop[,c('admin1.char','proportion')]
  colnames(adm1.pop) <- c('region','proportion')
  adm1.pop$years <- year
  
  if(year==pop.year[1]){
    weight.adm1.u1 <- adm1.pop
  }else{
    weight.adm1.u1 <- rbind(weight.adm1.u1,adm1.pop)
  }
}else{
  #Under 5
  adm1.pop<-pop_adm(adm.shp=poly.adm1,
                    wp=pop_u5,
                    admin_pop_dat=adm_link)
  
  # create an ordered admin1 list
  match.order = match(paste("admin1", 1: nrow(adm1.pop), 
                            sep = "_"), adm1.pop$admin1_idx)
  adm1.pop = adm1.pop[match.order, ]
  adm1.pop$proportion <-adm1.pop$admin_pop/sum(adm1.pop$admin_pop)
  adm1.pop<-adm1.pop[,c('admin1.char','proportion')]
  colnames(adm1.pop) <- c('region','proportion')
  adm1.pop$years <- year
  
  if(year==pop.year[1]){
    weight.adm1.u5 <- adm1.pop
  }else{
    weight.adm1.u5 <- rbind(weight.adm1.u5,adm1.pop)
  }
  
  #Under 1
  adm1.pop<-pop_adm(adm.shp=poly.adm1,
                    wp=pop_u1,
                    admin_pop_dat=adm_link)
  
  # create an ordered admin1 list
  match.order = match(paste("admin1", 1: nrow(adm1.pop), 
                            sep = "_"), adm1.pop$admin1_idx)
  adm1.pop = adm1.pop[match.order, ]
  adm1.pop$proportion <-adm1.pop$admin_pop/sum(adm1.pop$admin_pop)
  adm1.pop<-adm1.pop[,c('admin1.char','proportion')]
  colnames(adm1.pop) <- c('region','proportion')
  adm1.pop$years <- year
  
  if(year==pop.year[1]){
    weight.adm1.u1 <- adm1.pop
  }else{
    weight.adm1.u1 <- rbind(weight.adm1.u1,adm1.pop)
  }
  
  
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
