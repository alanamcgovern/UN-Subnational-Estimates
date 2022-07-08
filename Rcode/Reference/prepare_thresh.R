################################################################
#########   load libraries
################################################################
#### Libraries ####

rm(list = ls())

options(gsubfn.engine = "R")
library(rgdal)
library(raster)
library(rgeos)
library(sqldf)
library(geosphere)

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

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info


################################################################
#########   load polygons
################################################################

setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm0)) # load the national shape file
poly.adm1 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions
poly.adm2 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions

proj4string(poly.adm0) <- proj4string(poly.adm1) <- proj4string(poly.adm2)
load(paste0('shapeFiles_gadm/', country, '_Amat.rda'))  # load the adjacency matrix
load(paste0('shapeFiles_gadm/', country, '_Amat_Names.rda'))  # load names of admin1 and admin2 regions


########################################################################
### download U5 population, if not working, try manually download
#! downloading might take a long time, especially for big countries
########################################################################

pop.year <- beg.year:end.proj.year
pop.abbrev <- tolower(gadm.abbrev)

setwd(paste0(data.dir,'/worldpop'))

options(timeout = 1000) # adjust this time, should be longer than each download
for(year in pop.year){
  print(year)
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

################################################################
#########   Correct urban cluster (define function for later use)
################################################################

# The function below corrects urban clusters that are misclassified to be rural due to jittering. Jittered location is not the exact location 
# but a randomly shifted location for the purpose of confidentiality. This may results in some urban clusters to be jittered to some rural areas, 
# which is unexpected to our classification algorithm and therefore correcting the misclassified clusters is of interest. 

constr_prior <- function(obs,jitter_r,prob_r,poly_admin,pop_ras){
  
  # for the cluster, find its coordinates and admin1 area
  admin1_index<-obs$admin2
  sp_xy<-SpatialPoints(as.data.frame(obs)[,c('LONGNUM','LATNUM')],
                       proj4string = CRS(proj4string(poly_admin)))
  #pt<-as.data.frame(obs)[,c('LONGNUM','LATNUM')]
  #colnames(pt)<-c('x','y')
  
  # generate gitter 
  #jitter_r<-2000
  cluster_buffer<-buffer(sp_xy, width=jitter_r)
  
  # extract pixels within the buffer
  temp_pop_cir<-mask(crop(pop_ras,cluster_buffer),
                     cluster_buffer)
  
  # put admin area restriction
  admin1_poly<-poly_admin[admin1_index,]
  temp_pop_admin<-mask(crop(temp_pop_cir,admin1_poly),
                       admin1_poly)
  
  
  # check whether need to adjust for constraint 
  cir_val<-values(temp_pop_cir)
  admin_val<-values(temp_pop_admin)
  
  admin_adj<-length(which(!is.na(cir_val)))!=length(which(!is.na(admin_val)))
  
  if(admin_adj){
    #normc<-admin1_normc(pt,jitter_r,admin1_poly,ntrial=1000)
    normc<-1
  }else{  normc<-1}
  
  
  
  ## prepare sample frame
  
  temp_val<-values(temp_pop_admin)
  pop_index<-which(!is.na(temp_val))
  
  
  temp_frame<-as.data.frame(coordinates(temp_pop_admin))
  pixel_candidate<-temp_frame[pop_index,]
  pixel_candidate$pop_den<-temp_val[pop_index]
  
  pixel_candidate$center_x<-obs$LONGNUM
  pixel_candidate$center_y<-obs$LATNUM
  pixel_candidate$dist<-diag(distm(pixel_candidate[,c('x','y')], 
                                   pixel_candidate[,c('center_x','center_y')]))
  
  pixel_candidate$unn_w<-pixel_candidate$pop_den*
    1/(2*pi * 2 * pixel_candidate$dist)*normc*prob_r
  
  pixel_candidate$normc<-normc
  return(pixel_candidate[,c("x","y",'normc','unn_w')])
  #return(pixel_candidate)
  
}

  ################################################################
  #########   load worldpop
  ################################################################

  ### automated downloading, if not working, try manually download
  setwd(paste0(data.dir,'/Population'))
 
  file <- paste0( pop.abbrev,'_ppp_',frame_year,'_1km_Aggregated_UNadj.tif')
  
  if(!file.exists(file)){
    
    url <- paste0("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/", 
                  frame_year, "/", toupper(pop.abbrev),"/",      
                  pop.abbrev,'_ppp_',frame_year,'_1km_Aggregated_UNadj.tif')
    
    
    download.file(url, file, method = "libcurl",mode="wb")
    }
  
    # UNadjusted population counts
    worldpop <- raster(paste0(country.abbrev,
                            '_ppp_',frame_year,
                            '_1km_Aggregated_UNadj.tif',sep=''))



  ################################################################
  #########  Load cluster data
  ################################################################

  # load DHS cluster information
  setwd(paste0(data.dir))
  load(paste0(country,'_cluster_dat.rda'),
       envir = .GlobalEnv)

  # clusters
  cluster_list<-mod.dat[!duplicated(mod.dat[c('cluster','survey',
                                            'LONGNUM','LATNUM')]),]


  ################################################################
  #########   Correct urban cluster
  ################################################################

  # The codes below fulfills the process defined in the constr_prior function by assigning the possibly misclassified urban clusters to the nearest most densely populated areas.
  # It's generally true that the urban areas tend to have a higher population density so this process can alleviate the side effect
  # of jittering.

  # only correct urban clusters 
  # check if the strata are named 'urban' and 'rural'
  urban_clus<-cluster_list[cluster_list$urban=='urban',]
  rural_clus<-cluster_list[cluster_list$urban=='rural',]

  urban_clus$x_adj<-NA
  urban_clus$y_adj<-NA
  rural_clus$x_adj<-rural_clus$LONGNUM
  rural_clus$y_adj<-rural_clus$LATNUM

  # points have to stay within the same admin2 region 
  #for( i in 1:dim(urban_clus)[1]){
  for( i in 1:dim(urban_clus)[1]){
    print(i)
    temp_frame<-constr_prior(urban_clus[i,],2000,1,poly.adm2,worldpop)
    p_mode = sqldf("SELECT * FROM temp_frame GROUP BY x,y ORDER BY SUM(unn_w) DESC LIMIT 1")
    urban_clus[i,]$x_adj<-p_mode$x
    urban_clus[i,]$y_adj<-p_mode$y
  
  }


  prep_dat<-rbind(urban_clus,rural_clus)
  
  # create directory to store cluster data
  setwd(paste0(data.dir))

  if(!dir.exists(paths = paste0('prepared_dat/'))){
    dir.create(path = paste0('prepared_dat/'))
  }


  save(prep_dat,file='prepared_dat/prep_dat.rda')



  ################################################################
  ######### adding covariates 
  ################################################################

  crc_dat<-prep_dat
  
  # set up corrected xy for clusters
  xy_crc <- as.matrix(crc_dat[c('x_adj','y_adj')])
  crc_dat$x<-crc_dat$x_adj # x_adj and y_adj: corrected coordinates
  crc_dat$y<-crc_dat$y_adj

  # extract covariates
  crc_dat$pop_den<-raster::extract(worldpop,xy_crc)

  # only retain part of the columns to reduce redundancy
  col_select<-c('cluster','urban','admin1','admin2',
              'admin1.name','admin2.name',
              'admin1.char','admin2.char',
              'survey','pop_den','x','y')

  crc_dat_final<-crc_dat[,col_select]
  save(crc_dat_final,file='prepared_dat/crc_dat.rda')




  ################################################################
  ######### prepare data without urban correction
  ################################################################

  uncrc_dat<-prep_dat

  # set up uncorrected xy for clusters
  xy_uncrc <- as.matrix(uncrc_dat[c('LONGNUM','LATNUM')])
  uncrc_dat$x<-uncrc_dat$LONGNUM # x_adj and y_adj: corrected coordinates
  uncrc_dat$y<-uncrc_dat$LATNUM


  # extract covariates
  uncrc_dat$pop_den<-raster::extract(worldpop,xy_uncrc)


  # keep columns
  col_select<-c('cluster','urban','admin1','admin2',
              'admin1.name','admin2.name',
              'admin1.char','admin2.char',
              'survey','pop_den','x','y')

  uncrc_dat_final<-uncrc_dat[,col_select]
  save(uncrc_dat_final,file='prepared_dat/uncrc_dat.rda')



  ################################################################
  ######### prepare U5 population
  ################################################################
  
  # aggregate the under-five population spatial surface to resolution of 1km*1km
  for (year in pop.year){
  
    print(year)
    
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
  
  
    pop_u5<- pop_f_0+pop_f_1+pop_m_0+pop_m_1
  
    setwd(paste0(data.dir,'/Population'))
    writeRaster(pop_u5, overwrite=TRUE,
                paste0(pop.abbrev,'_u5_',year,'_100m','.tif'))
  
    pop_surf<-raster(paste0(country.abbrev,'_u5_',year,'_100m.tif'))
  
  
    pop_u5_aggregate <- aggregate(pop_surf, fact=10,sum)
  
    pop_grid<-as.data.frame(coordinates(worldpop))
    colnames(pop_grid)<-c('x','y')
  
    pop_grid$u5_pop <- raster::extract(pop_u5_aggregate, 
                                     pop_grid[c('x','y')])
  
    u5_pop<-worldpop
    values(u5_pop)<-pop_grid$u5_pop 
  
    writeRaster(u5_pop, overwrite=TRUE,
                paste0(country.abbrev,'_u5_',year,'_1km.tif'))
  
  }



  ################################################################
  #########   prepare national grid
  ################################################################

  ## set up grid
  urb_dat<-as.data.frame(coordinates(worldpop))
  colnames(urb_dat)<-c('x','y')


  ## add population density

  urb_dat$pop_den<-raster::extract(worldpop,urb_dat[c('x','y')])


  ## add admin1 region for pixel
  points.frame <- as.data.frame(urb_dat[,c("x", "y")])
  points.frame <- SpatialPoints(points.frame)
  poly.over.adm1 <- SpatialPolygons(poly.adm1@polygons)
  admin1.key <- over(points.frame, poly.over.adm1)
  urb_dat$admin1<-admin1.key
  urb_dat$admin1.char <- paste0("admin1_", admin1.key)

  setwd(paste0(data.dir))
  save(urb_dat,file='prepared_dat/natl_grid.rda')
  

