# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Zambia'
year_range <- 2009:2013

# Load libraries and info ----------------------------------------------------------

library(terra)
library(raster)

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0("/Users/alanamcgovern/Desktop/Research/UN_Estimates/UN-Subnational-Estimates/Info/",info.name, sep='')) # load the country info

pop.abbrev <- tolower(gadm.abbrev)


# Load Polygons ----------------------------------------------------------

poly.path <- paste0("/Users/alanamcgovern/Desktop/Research/UN_Estimates/UN-Subnational-Estimates/Data/shapeFiles_gadm/gadm41_",gadm.abbrev,"_shp")
# other cases
if(!dir.exists(poly.path)){
  poly.path <- paste0("/Users/alanamcgovern/Desktop/Research/UN_Estimates/UN-Subnational-Estimates/Data/shapeFiles_alt/",country_t)
}

poly.adm0 <- vect(paste0(poly.path,'/',poly.layer.adm0,'.shp')) # load the national shape file
poly.adm1 <- vect(paste0(poly.path,'/',poly.layer.adm1,'.shp')) # load the national shape file
if(exists('poly.layer.adm2')){
  poly.adm2 <- vect(paste0(poly.path,'/',poly.layer.adm2,'.shp')) # load the national shape file
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

# Download U1 population  ----------------------------------------------------------
#### if not working, try manually download
#### downloading might take a long time, especially for big countries
setwd(paste0(data.dir,'/worldpop'))

options(timeout = 2000000) # adjust this time, should be longer than each download
rigorousFileTest = TRUE # set to TRUE after files have been downloaded to test 
# if files were downloaded correctly, i.e. if they can be loaded into R
for(year in year_range){
  print(year)
  for(sex in c("f", "m")){
    file <- paste0(pop.abbrev,'_', sex, '_0_', year,'.tif')
    
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
                    sex, "_0_", year, ".tif")
      download.file(url, file, method = "libcurl",mode="wb")
    }
  }
}

# Prepare U1 Population ----------------------------------------------------------

# aggregate the under-five population spatial surface to resolution of 1km*1km
for (year in year_range){
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
  m_0_name = paste0(pop.abbrev,'_m_0_',year,'.tif')
  
  pop_f_0<-raster(f_0_name)
  pop_m_0<-raster(m_0_name)
  
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
  
}

# Calculate weights  ----------------------------------------------------------

weight.adm1.u1 <- weight.adm2.u1 <- NULL
for (year in year_range){
  print(year)
  pop_u1 <- rast(paste0(data.dir,'/Population/',country.abbrev,'_u1_',year,'_100m','.tif'))
  
  # make sure polygons have same crs as population raster
  adm1.shp <- terra::project(poly.adm1, crs(pop_u1))

# admin level population
  wp.adm1.list <- lapply(1:nrow(adm1.shp), function(x) {
    list(state_id = x, state_raster = terra::mask(crop(pop_u1,adm1.shp[x,]),
                                                mask = adm1.shp[x,]))
  })

  # store total population at admin
  pop.adm1 <- sapply(1:nrow(adm1.shp), function(j){
    sum(values(wp.adm1.list[[j]]$state_raster),na.rm=TRUE)})

  # add admin population 
  weight.adm1.u1.tmp <- data.frame(admin1.char = paste0('admin1_',1:nrow(poly.adm1)),admin1.name = poly.adm1$NAME_1, admin_pop= pop.adm1)
  weight.adm1.u1.tmp$proportion <- weight.adm1.u1.tmp$admin_pop/sum(weight.adm1.u1.tmp$admin_pop)
  weight.adm1.u1.tmp$year <- year
  
  weight.adm1.u1 <- rbind(weight.adm1.u1, weight.adm1.u1.tmp)

  # make sure polygons have same crs as population raster
  adm2.shp <- terra::project(poly.adm2, crs(pop_u1))
  
  # admin level population
  wp.adm2.list <- lapply(1:nrow(adm2.shp), function(x) {
    list(state_id = x, state_raster = terra::mask(crop(pop_u1,adm2.shp[x,]),
                                                   mask = adm2.shp[x,]))
  })
  
  # store total population at admin
  pop.adm2 <- sapply(1:nrow(adm2.shp), function(j){
  sum(values(wp.adm2.list[[j]]$state_raster),na.rm=TRUE)})

  # add admin population 
  weight.adm2.u1.tmp <- data.frame(admin2.char = paste0('admin2_',1:nrow(poly.adm2)),admin2.name = poly.adm2$NAME_2, 
                             admin_pop= pop.adm2)
  weight.adm2.u1.tmp$proportion <- weight.adm2.u1.tmp$admin_pop/sum(weight.adm2.u1.tmp$admin_pop)
  weight.adm2.u1.tmp$year <- year
  
  weight.adm2.u1 <- rbind(weight.adm2.u1, weight.adm2.u1.tmp)

}

save(weight.adm1.u1,file=paste0(data.dir,'/worldpop/adm1_weights_u1.rda'))
save(weight.adm2.u1,file=paste0(data.dir,'/worldpop/adm2_weights_u1.rda'))


