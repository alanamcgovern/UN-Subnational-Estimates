rm(list = ls())
# ENTER COUNTRY OF INTEREST  -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'

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
load(paste0(res.dir,'/UR/urb_prop.rda')) # load sampling frame urban proportion at admin1 

# alter info to exclude surveys not in same frame -- to do stratified model all surveys must be from same frame
survey_years <- survey_years[frame_years==max(frame_years)]

# Load Polygons ----------------------------------------------------------

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



# Download U5 population  ----------------------------------------------------------
#### if not working, try manually download
#### downloading might take a long time, especially for big countries

pop.year <- beg.year:end.proj.year
pop.abbrev <- tolower(gadm.abbrev)

setwd(paste0(data.dir,'/worldpop'))

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

# Load worldpop  ----------------------------------------------------------
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


# Load cluster data  ----------------------------------------------------------

  setwd(paste0(data.dir))
  load(paste0(country,'_cluster_dat.rda'),
       envir = .GlobalEnv)
  
  #exclude surveys not in same frame
  mod.dat <- mod.dat[mod.dat$survey %in% survey_years,]
  save(mod.dat, file=paste0(country,'_cluster_dat.rda'),
       envir = .GlobalEnv)

  cluster_list<-mod.dat[!duplicated(mod.dat[c('cluster','survey',
                                            'LONGNUM','LATNUM')]),]

# Define function to correct urban clusters ----------------------------------------------------------
  
  ## The function below corrects urban clusters that are misclassified to be rural due to jittering. Jittered location is not the exact location 
  ## but a randomly shifted location for the purpose of confidentiality. This may results in some urban clusters to be jittered to some rural areas, 
  ## which is unexpected to our classification algorithm and therefore correcting the misclassified clusters is of interest. 
  
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
  
# Define function to compute urban population threshold ----------------------------------------------------------
  
  ## This function computes the urban population threshold for a given admin1 area.
  ## This is done by keep counting the urban locations until the urban population fraction in the reference table is reached.
  thresh_urb<-function(adm_grid,ref_tab){
    
    # sort grid population
    vals <- adm_grid$pop_den
    vals[is.na(vals)] <- 0
    sort.obj <- sort.int(vals, decreasing = TRUE, index.return = TRUE, method = 'shell')
    svals <- sort.obj$x
    svals.int <- sort.obj$ix
    
    # extract cutoff proportion based on admin1
    adm.idx <- adm_grid$admin1.char[1]
    cutoff <- ref_tab[ref_tab$Internal==adm.idx,]$urb_frac
    
    # determine population threshold and urban rural
    csvals <- cumsum(svals)/sum(svals)
    is.urb <- csvals <= cutoff
    org.isurb <- is.urb[invPerm(svals.int)]
    threshold <- min(vals[org.isurb == 1]) #cutoff
    
    # prepare return object (grid with urban/rural)
    adm_grid$threshold <- threshold
    adm_grid$urban <- as.numeric(org.isurb)
    #adm_grid[is.na(adm_grid$pop_den),]$urban<-NA
    
    return(adm_grid)
    
  }
  
# Define function to get urban fraction ----------------------------------------------------------

  get_subnatl_frac<-function(adm.names,adm.idx,wp,poly_file,wp_adm=NULL,
                             urb_vec){
    
    poly_file <- spTransform(poly_file, wp@crs)
    
    
    
    if(is.null(wp_adm))
      wp_adm <- lapply(1:nrow(poly_file), function(x) {
        list(state_id = x, state_raster = mask(crop(wp,poly_file[x,]), poly_file[x,]))
      })
    
    pred_surf <- wp
    values(pred_surf)<-urb_vec
    
    urb_adm <- lapply(1:nrow(poly_file), function(x) {
      list(state_id = x, state_raster = mask(crop(pred_surf,poly_file[x,]), poly_file[x,]))
    })
    
    frac_vec<-vector()
    
    for(j in 1:length(adm.names)){
      urb_j<-urb_adm[[j]]
      wp_j<-wp_adm[[j]]
      
      val_urb_j<-values(urb_j$state_raster)
      val_wp_j<-values(wp_j$state_raster)
      
      frac_vec[j]<-sum(val_urb_j*val_wp_j,na.rm=TRUE)/
        sum(val_wp_j,na.rm=TRUE)
    }
    
    subnatl_frac<-data.frame(adm_name=adm.names,adm_idx=adm.idx,urb_frac=frac_vec)
    return(subnatl_frac)
  }
  
# Correct urban clusters ----------------------------------------------------------
  
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


# Add covariates ----------------------------------------------------------
  
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

# Prepare data w/o urban correction ----------------------------------------------------------
  
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


# Prepare U1 and U5 Populations ----------------------------------------------------------
  
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

# Prepare national grid ----------------------------------------------------------

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
  
# Admin1 threshold ----------------------------------------------------------
  
  # index the grid
  urb_dat$index <- c(1:nrow(urb_dat))
  adm1_dat <- split( urb_dat , f = urb_dat$admin1 )
  
  urb_list<-lapply(adm1_dat, FUN=thresh_urb,ref_tab=ref.tab)
  
  urb_class <- do.call("rbind", urb_list)
  
  
  urb_grid <- urb_dat
  urb_grid$urb_ind <-NA
  urb_grid[urb_class$index,]$urb_ind <- urb_class$urban
  
  
  urb_surf<-worldpop
  values(urb_surf)<-urb_grid$urb_ind
  
  
  ## save reference table along with calculated threshold 
  thresh_ref <- urb_class[!duplicated(urb_class[,c('admin1')]),]
  ref.tab$threshold <- thresh_ref$threshold # check whether the thresholds are sensible (shouldn't be NA or all 0)
  write.xlsx(ref.tab, file='prepared_dat/reference_table.xlsx',
             row.names = FALSE)
 
# Check classification accuracy based on clusters ----------------------------------------------------------

  ### remove rows with missing covariates, could also build model with missing data
  crc_dat<-crc_dat_final[complete.cases(crc_dat_final), ]
  uncrc_dat<-uncrc_dat_final[complete.cases(uncrc_dat_final), ]
  
  
  xy_crc <- as.matrix(crc_dat[c('x','y')])
  xy_uncrc <- as.matrix(uncrc_dat[c('x','y')])
  
  # extract the urban/rural prediction
  crc_dat$urb_pred<-raster::extract(urb_surf,xy_crc)
  uncrc_dat$urb_pred<-raster::extract(urb_surf,xy_uncrc)
  pred_crc <- factor( ifelse(crc_dat$urb_pred ==1 ,"urban","rural" ))
  pred_crc  <- relevel(pred_crc, "urban") # make sure levels are same 
  pred_uncrc <- factor( ifelse(uncrc_dat$urb_pred ==1 ,"urban","rural" ))
  pred_uncrc  <- relevel(pred_uncrc, "urban") # make sure levels are same 
  
  ### set directory for results
  setwd(paste0(res.dir,'/UR/'))
  
  
  # compute the confusion to evaluate the accuracy
  confmatrix_crc<-caret::confusionMatrix(
    data = pred_crc,
    reference = crc_dat$urban
  )
  
  confmatrix_crc
  save(confmatrix_crc,file='Threshold/confmatrix_crc.rda')
  
  confmatrix_uncrc<-caret::confusionMatrix(
    data = pred_uncrc,
    reference = uncrc_dat$urban
  )
  
  confmatrix_uncrc
  save(confmatrix_uncrc,file='Threshold/confmatrix_uncrc.rda')
  
  
# Save national U1 and U5 urban proportions ----------------------------------------------------------  
  setwd(paste0(data.dir,'/Population'))
  years <- c(beg.year:end.proj.year)
  natl.u1.urb <- vector()
  natl.u5.urb <- vector()
  
  for ( t in 1:length(years)){
    
    print(t)
    year <- years[t]
    
    # load U1 population at year t
    u1_pop<-raster(paste0(country.abbrev,'_u1_',year,'_1km.tif'))
    
    # national urban fraction for U1 population at year t
    u1_natl <- sum(urb_grid$urb_ind*values(u1_pop),na.rm=TRUE)/
      sum(values(u1_pop),na.rm=TRUE)
    natl.u1.urb[t] <- u1_natl
    
    # load U5 population at year t
    u5_pop<-raster(paste0(country.abbrev,'_u5_',year,'_1km.tif'))
    
    # national urban fraction for U5 population at year t
    u5_natl <- sum(urb_grid$urb_ind*values(u5_pop),na.rm=TRUE)/
      sum(values(u5_pop),na.rm=TRUE)
    natl.u5.urb[t] <- u5_natl
    
  }
  
  natl.u1.urb.weights <- data.frame(years= years, urban=natl.u1.urb)
  natl.u5.urb.weights <- data.frame(years= years, urban=natl.u5.urb)
  
  
  setwd(paste0(res.dir,'/UR'))
  saveRDS(natl.u1.urb.weights,paste0('U1_fraction/natl_u1_urban_weights.rds'))
  saveRDS(natl.u5.urb.weights,paste0('U5_fraction/natl_u5_urban_weights.rds'))
 
# Save subnational U1 and U5 urban proportions ----------------------------------------------------------  

  adm1.u1.weight.frame <- data.frame()
  adm2.u1.weight.frame <- data.frame()
  adm1.u5.weight.frame <- data.frame()
  adm2.u5.weight.frame <- data.frame()
  
  for ( t in 1:length(years)){
    
    print(t)
    year <- years[t]
    
    # load populations at year t
    setwd(paste0(data.dir,'/','/Population'))
    u1_pop<-raster(paste0(country.abbrev,'_u1_',year,'_1km.tif'))
    u5_pop<-raster(paste0(country.abbrev,'_u5_',year,'_1km.tif'))
    
    # admin1 urban fraction for populations at year t
    u1_urb_admin1<-get_subnatl_frac(adm.names = admin1.names$GADM,
                                    adm.idx = admin1.names$Internal,
                                    wp=u1_pop,
                                    poly_file = poly.adm1,
                                    wp_adm = NULL,
                                    urb_vec = urb_grid$urb_ind)
    
    u5_urb_admin1<-get_subnatl_frac(adm.names = admin1.names$GADM,
                                    adm.idx = admin1.names$Internal,
                                    wp=u5_pop,
                                    poly_file = poly.adm1,
                                    wp_adm = NULL,
                                    urb_vec = urb_grid$urb_ind)
    
    u1_urb_admin1$years <- u5_urb_admin1$years <- year
    adm1.u1.weight.frame <- rbind(adm1.u1.weight.frame,u1_urb_admin1)
    adm1.u5.weight.frame <- rbind(adm1.u5.weight.frame,u5_urb_admin1)
    
    # admin2 urban fraction for U5 population at year t
    u1_urb_admin2<-get_subnatl_frac(adm.names = admin2.names$GADM,
                                    adm.idx = admin2.names$Internal,
                                    wp=u1_pop,
                                    poly_file = poly.adm2,
                                    wp_adm = NULL,
                                    urb_vec = urb_grid$urb_ind)
    
    u5_urb_admin2<-get_subnatl_frac(adm.names = admin2.names$GADM,
                                    adm.idx = admin2.names$Internal,
                                    wp=u5_pop,
                                    poly_file = poly.adm2,
                                    wp_adm = NULL,
                                    urb_vec = urb_grid$urb_ind)
    
    u1_urb_admin2$years <- u5_urb_admin2$years <- year
    adm2.u1.weight.frame <- rbind(adm2.u1.weight.frame,u1_urb_admin2)
    adm2.u5.weight.frame <- rbind(adm2.u5.weight.frame,u5_urb_admin2)
    
    setwd(paste0(res.dir,'/UR'))
    
    # save calculated urban fractions
    saveRDS(u1_urb_admin1,file=paste0('U1_fraction/','admin1_u1_',
                                      year, '_urban_frac.rds'))
    saveRDS(u1_urb_admin2,file=paste0('U1_fraction/','admin2_u1_',
                                      year, '_urban_frac.rds'))
    saveRDS(u5_urb_admin1,file=paste0('U5_fraction/','admin1_u5_',
                                      year, '_urban_frac.rds'))
    saveRDS(u5_urb_admin2,file=paste0('U5_fraction/','admin2_u5_',
                                      year, '_urban_frac.rds'))
    
  }
  
  
  # process admin 1 urban rural weights data frame
  adm1.u1.weight.frame <- adm1.u1.weight.frame[,c('adm_idx','years','urb_frac')]
  colnames(adm1.u1.weight.frame) <- c('region','years','urban')
  adm1.u1.weight.frame$rural <- 1 - adm1.u1.weight.frame$urban
  saveRDS(adm1.u1.weight.frame,paste0('U1_fraction/','admin1_u1_urban_weights.rds'))
  
  adm1.u5.weight.frame <- adm1.u5.weight.frame[,c('adm_idx','years','urb_frac')]
  colnames(adm1.u5.weight.frame) <- c('region','years','urban')
  adm1.u5.weight.frame$rural <- 1 - adm1.u5.weight.frame$urban
  saveRDS(adm1.u5.weight.frame,paste0('U5_fraction/','admin1_u5_urban_weights.rds'))
  
  # process admin 2 urban rural weights data frame
  adm2.u1.weight.frame <- adm2.u1.weight.frame[,c('adm_idx','years','urb_frac')]
  colnames(adm2.u1.weight.frame) <- c('region','years','urban')
  adm2.u1.weight.frame$rural <- 1 - adm2.u1.weight.frame$urban
  saveRDS(adm2.u1.weight.frame,paste0('U1_fraction/','admin2_u1_urban_weights.rds'))
  
  adm2.u5.weight.frame <- adm2.u5.weight.frame[,c('adm_idx','years','urb_frac')]
  colnames(adm2.u5.weight.frame) <- c('region','years','urban')
  adm2.u5.weight.frame$rural <- 1 - adm2.u5.weight.frame$urban
  saveRDS(adm2.u5.weight.frame,paste0('U5_fraction/','admin2_u5_urban_weights.rds'))
  

