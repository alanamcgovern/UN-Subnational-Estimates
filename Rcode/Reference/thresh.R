################################################################
#########   load libraries
################################################################
rm(list = ls())
options(gsubfn.engine = "R")
library(rgdal)
library(raster)
library(Matrix)
library(openxlsx)

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
load(file = paste0(home.dir,'/Info/', info.name, sep='')) # load the country info

################################################################
#########   load polygon files
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

################################################################
#########   load sampling frame urban proportion at admin1 
################################################################

load(paste0(res.dir,'/UR/urb_prop.rda'))

  ################################################################
  #########   load worldpop
  ################################################################

  # Unadjusted population counts
  worldpop <- raster(paste0('Population/',country.abbrev,
                          '_ppp_',frame_year,
                          '_1km_Aggregated_UNadj.tif',sep=''))


  ################################################################
  #########   admin 1 threshold 
  ################################################################

  ## load grid
  load(file='prepared_dat/natl_grid.rda')

  # index the grid
  urb_dat$index <- c(1:nrow(urb_dat))
  adm1_dat <- split( urb_dat , f = urb_dat$admin1 )


  # This function computes the urban population threshold for a given admin1 area.
  # This is done by keep counting the urban locations until the urban population fraction in the reference table is reached.
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

  ################################################################
  #########   check classification accuracy based on clusters
  ################################################################

  load('prepared_dat/crc_dat.rda')
  load('prepared_dat/uncrc_dat.rda')

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


  ################################################################
  #########  load function for urban fraction
  ################################################################

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


  ################################################################
  #########  national U5 urban proportion 
  ################################################################
  setwd(paste0(data.dir,'/Population'))
  years <- c(beg.year:end.proj.year)
  natl.u5.urb <- vector()

  for ( t in 1:length(years)){
  
    print(t)
    year <- years[t]
  
    # load U5 population at year t
    u5_pop<-raster(paste0(country.abbrev,'_u5_',year,'_1km.tif'))
  
    # national urban fraction for U5 population at year t
    u5_natl <- sum(urb_grid$urb_ind*values(u5_pop),na.rm=TRUE)/
      sum(values(u5_pop),na.rm=TRUE)
    natl.u5.urb[t] <- u5_natl
  
  }

  natl.urb.weights <- data.frame(years= years, urban=natl.u5.urb)


  setwd(paste0(res.dir,'/UR'))
  saveRDS(natl.urb.weights,paste0('U5_fraction/natl_urban_weights.rds'))

  ################################################################
  #########  subnatl U5 urban proportion 
  ################################################################
  
  adm1.weight.frame <- data.frame()
  adm2.weight.frame <- data.frame()

  for ( t in 1:length(years)){
  
    print(t)
    year <- years[t]
  
    # load U5 population at year t
    setwd(paste0(data.dir,'/','/Population'))
    u5_pop<-raster(paste0(country.abbrev,'_u5_',year,'_1km.tif'))
  
    # admin1 urban fraction for U5 population at year t
    u5_urb_admin1<-get_subnatl_frac(adm.names = admin1.names$GADM,
                                  adm.idx = admin1.names$Internal,
                                  wp=u5_pop,
                                  poly_file = poly.adm1,
                                  wp_adm = NULL,
                                  urb_vec = urb_grid$urb_ind)
  
    u5_urb_admin1$years <- year
    adm1.weight.frame <- rbind(adm1.weight.frame,u5_urb_admin1)
  
    # admin2 urban fraction for U5 population at year t
    u5_urb_admin2<-get_subnatl_frac(adm.names = admin2.names$GADM,
                                  adm.idx = admin2.names$Internal,
                                  wp=u5_pop,
                                  poly_file = poly.adm2,
                                  wp_adm = NULL,
                                  urb_vec = urb_grid$urb_ind)
  
    u5_urb_admin2$years <- year
    adm2.weight.frame <- rbind(adm2.weight.frame,u5_urb_admin2)
  
    setwd(paste0(res.dir,'/UR'))
  
    # save calculated urban fractions
    saveRDS(u5_urb_admin1,file=paste0('U5_fraction/','admin1_',
                                    year, '_urban_frac.rds'))
    saveRDS(u5_urb_admin2,file=paste0('U5_fraction/','admin2_',
                                    year, '_urban_frac.rds'))
  
  }


  # process admin 1 urban rural weights data frame
  adm1.weight.frame <- adm1.weight.frame[,c('adm_idx','years','urb_frac')]
  colnames(adm1.weight.frame) <- c('region','years','urban')
  adm1.weight.frame$rural <- 1 - adm1.weight.frame$urban

  # process admin 2 urban rural weights data frame
  adm2.weight.frame <- adm2.weight.frame[,c('adm_idx','years','urb_frac')]
  colnames(adm2.weight.frame) <- c('region','years','urban')
  adm2.weight.frame$rural <- 1 - adm2.weight.frame$urban

  # save weights frames
  saveRDS(adm1.weight.frame,paste0('U5_fraction/','admin1_urban_weights.rds'))
  saveRDS(adm2.weight.frame,paste0('U5_fraction/','admin2_urban_weights.rds'))

