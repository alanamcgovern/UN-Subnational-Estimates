rm(list=ls())

# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Guinea'

# Setup
# Load libraries and info ----------------------------------------------------------

# Libraries
library(data.table)
library(survey)
library(sp)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(raster)
library(maptools)
library(latticeExtra)
library(viridis)
library(xtable)
library(Hmisc)
library(INLA)
library(spdep)
library(rasterVis)
library(plotrix)
library(ggridges)
options(gsubfn.engine = "R")
library(rgdal)
library(ggplot2)
library(SUMMER)

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/',info.name, sep='')) # load the country info

if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/U5MR'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/NMR'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/NMR'))}


# Load polygon files  ------------------------------------------------------
setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm0)) # load the national shape file
# use encoding to read special characters
poly.adm1 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm1)) # load the shape file of admin-1 regions

poly.adm2 <- readOGR(dsn = poly.path,encoding = "UTF-8", use_iconv = TRUE,
                     layer = as.character(poly.layer.adm2)) # load the shape file of admin-2 regions

# set coordinate reference system to be equal
if(exists("poly.adm2")){
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0(poly.path,'/', country, '_Amat.rda'))
load(paste0(poly.path,'/', country, '_Amat_Names.rda'))


#### Parameters ####
outcome_vt <- c('nmr','u5')
strat_vt <- c("strat","unstrat") 
admin_vt <- c("adm1","adm2") ## admin level
level_vt <- c(1,2) ## polygon level
admin_level_dt <- data.table(Admin = admin_vt,
                             level = level_vt)
year_vt <- 2000:end.proj.year
n_years <- length(year_vt)

#### generate posterior samples ####
n_samp <- 1000

setwd(paste0(res.dir,'/Betabinomial'))
for(outcome in outcome_vt){
  for(admin in admin_vt){
    for(strat in strat_vt){
        
        load(paste0(c('NMR','U5MR')[outcome==outcome_vt],'/',
                    country, "_res_",
                    admin, "_",strat,'_',outcome, ".rda"))
        
          res_obj <- get(paste0("bb.res.", admin,'.',strat,'.',outcome))
        
        draws_list <- res_obj$draws.est.overall
        n_admin <- length(draws_list)/n_years
        
        postsamp_mt_list <- vector(mode = "list", length = n_years)
        
        for (i in 1:n_years){
          # i <- 1
          postsamp_mt_list[[i]]$years <- years_vt[i]
          postsamp_mt <- matrix(0, nrow = n_admin, ncol = n_samp)
          
          for(j in 1:n_admin){
            # j <- 1
            postsamp_mt[j, ] <- draws_list[[n_years*(j-1)+i]]$draws
          }
          
          postsamp_mt_list[[i]]$postsamp_mt <- postsamp_mt
        }
        
        load(paste0(c('NMR','U5MR')[outcome==outcome_vt],'/',
                    country, "_res_",
                    admin, "_",strat,'_',outcome, ".rda"))
        
        save(postsamp_mt_list, 
             file = paste0(c('NMR','U5MR')[outcome==outcome_vt],'/', country, "_",
                          admin,"_", strat,'_',outcome, "_postsamp.RData"))
        
      }
    }
}  



#### national comparison plot ####
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/comparison'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/U5MR/comparison'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/comparison'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/NMR/comparison'))}
setwd(res.dir)

## load national level models

### national yearly direct
load(file = paste0('Direct/NMR/', country, '_direct_natl_yearly_nmr.rda'))  
load(file = paste0('Direct/U5MR/', country, '_direct_natl_yearly_u5.rda'))  
### national yearly smooth direct
load(file = paste0('Direct/NMR/', country, '_res_natl_yearly_nmr_SmoothedDirect.rda'))  
load(file = paste0('Direct/U5MR/', country, '_res_natl_yearly_u5_SmoothedDirect.rda'))  

### national betabinomial models
load(file = paste0('Betabinomial/NMR/', country, '_res_natl_unstrat_nmr.rda'))  
load(file = paste0('Betabinomial/NMR/', country, '_res_natl_strat_nmr.rda')) 
load(file = paste0('Betabinomial/U5MR/', country, '_res_natl_unstrat_u5.rda'))  
load(file = paste0('Betabinomial/U5MR/', country, '_res_natl_strat_u5.rda')) 

## load admin1 level models

### smooth direct admin1 3-year window
load(file = paste0('Direct/NMR/', country, '_res_admin1_nmr_SmoothedDirect.rda'))
admin1.sd.nmr <- res.admin1.nmr$results
admin1.sd.nmr.draws <- res.admin1.nmr$draws
load(file = paste0('Direct/U5MR/', country, '_res_admin1_u5_SmoothedDirect.rda'))  
admin1.sd.u5 <- res.admin1.u5$results
admin1.sd.u5.draws <- res.admin1.u5$draws

### smooth direct admin1 yearly
load(file = paste0('Direct/NMR/', country, '_res_admin1_nmr_SmoothedDirect_yearly.rda'))
admin1.sd.yearly.nmr <- sd.admin1.yearly.nmr$results
admin1.sd.yearly.nmr.draws <- sd.admin1.yearly.nmr$draws
load(file = paste0('Direct/U5MR/', country, '_res_admin1_u5_SmoothedDirect_yearly.rda'))  
admin1.sd.yearly.u5 <- sd.admin1.yearly.u5$results
admin1.sd.yearly.u5.draws <- sd.admin1.yearly.u5$draws

### BB8 admin1 stratified (and benchmarked)
load(file = paste0('Betabinomial/NMR/',country,'_res_adm1_strat_nmr_bench.rda'))
res.strat.admin1.nmr <- bb.res.adm1.strat.nmr.bench
admin1.strat.nmr.BB8<-res.strat.admin1.nmr$overall
load(file = paste0('Betabinomial/U5MR/',country,'_res_adm1_strat_u5_bench.rda'))
res.strat.admin1.u5 <- bb.res.adm1.strat.u5.bench
admin1.strat.u5.BB8<-res.strat.admin1.u5$overall

## load admin2 level models

### BB8 admin2 stratified (and benchmarked)
load(file = paste0('Betabinomial/NMR/',country,'_res_adm2_strat_nmr_bench.rda'))
res.strat.admin2.nmr <- bb.res.adm2.strat.nmr.bench
admin2.strat.nmr.BB8<-res.strat.admin2.nmr$overall
load(file = paste0('Betabinomial/U5MR/',country,'_res_adm2_strat_u5_bench.rda'))
res.strat.admin2.u5 <- bb.res.adm2.strat.u5.bench
admin2.strat.u5.BB8<-res.strat.admin2.u5$overall


#### trend plots (add more?) ------------------------------------------------------
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/trend'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/U5MR/trend'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/trend'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/NMR/trend'))}

for(outcome in outcome_vt){
  for(strat in strat_vt){
    load(paste0(res.dir,'/Betabinomial/',c('NMR','U5MR')[outcome==outcome_vt],'/',
                country, "_res_adm1_",strat,'_',outcome, ".rda"))
    
    res_obj <- get(paste0("bb.res.adm1.",strat,'.',outcome))
    res_merged <- merge(res_obj$overall, admin1.names, 
                        by.x=c("region"),
                        by.y=c("Internal"))
    res_merged$region <- res_merged$GADM
    res_merged$width <- res_merged$upper - res_merged$lower
    class(res_merged) <- class(res_obj$overall)
    
    cols <- rainbow(nrow(admin1.names))
    
    pdf(paste0(res.dir, "/Figures/Betabinomial/",paste0(c('NMR','U5MR')[outcome==outcome_vt]),"/trend/",
               country, "_adm1_", strat,"_",outcome, ".pdf"),height = 6,width = 6)
    
    par(mfrow=c(1,1),lend=1)
    plot.max <- max(res_merged$upper+.025, na.rm = T)
    
    plot(NA, xlab = "Year", ylab = paste0(c('NMR','U5MR')[outcome==outcome_vt]),
         ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year), main = paste0(country,' - Admin 1 Regions'))
    legend('topright', bty = 'n',
           col = cols, lwd = 2,  legend = admin1.names$GADM)
    
    for(area in 1:dim(poly.adm1)[1]){
      tmp.area <- res_merged[res_merged$region == as.character(admin1.names$GADM[area]),]
      tmp.area$width <- tmp.area$upper - tmp.area$lower
      tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
      tmp.area$cex2[tmp.area$cex2 > 6] <- 6
      
      res.area <- res_merged[res_merged$region == as.character(admin1.names$GADM[area]),]
      
      lines(res.area$years.num,res.area$median,
            col = cols[area], lwd = 2)
    }  
    
    dev.off()
    
    load(paste0(res.dir,'/Betabinomial/',c('NMR','U5MR')[outcome==outcome_vt],'/',
                country, "_res_adm2_",strat,'_',outcome, ".rda"))
    
    res_obj <- get(paste0("bb.res.adm2.",strat,'.',outcome))
    res_merged <- merge(res_obj$overall, admin2.names, 
                        by.x=c("region"),
                        by.y=c("Internal"))
    res_merged$region <- res_merged$GADM
    res_merged$width <- res_merged$upper - res_merged$lower
    class(res_merged) <- class(res_obj$overall)
    
    cols <- rainbow(nrow(admin2.names))
    
    pdf(paste0(res.dir, "/Figures/Betabinomial/",paste0(c('NMR','U5MR')[outcome==outcome_vt]),"/trend/",
               country, "_adm2_", strat,"_",outcome, ".pdf"),height = 6,width = 6)
    
    par(mfrow=c(1,1),lend=1)
    plot.max <- max(res_merged$upper+.025, na.rm = T)
    
    plot(NA, xlab = "Year", ylab = paste0(c('NMR','U5MR')[outcome==outcome_vt]),
         ylim = c(0, plot.max), xlim = c(beg.year, end.proj.year), main = paste0(country,' - Admin 2 Regions')) + theme(legend.position = 'none')
    
    for(area in 1:dim(poly.adm2)[1]){
      tmp.area <- res_merged[res_merged$region == as.character(admin2.names$GADM[area]),]
      tmp.area$width <- tmp.area$upper - tmp.area$lower
      tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
      tmp.area$cex2[tmp.area$cex2 > 6] <- 6
      
      res.area <- res_merged[res_merged$region == as.character(admin2.names$GADM[area]),]
      
      lines(res.area$years.num,res.area$median,
            col = cols[area], lwd = 2)
    }  
    
    dev.off()
    
  }
}



#### map plots ####
if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/U5MR/map'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/map'))}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/map'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/NMR/map'))}
for(admin in admin_vt){
    
    map_shp <- c(poly.adm1,poly.adm2)[admin==admin_vt]
    admin_name_dt <- as.data.table(get(paste0(c('admin1','admin2')[admin==admin_vt], ".names")))
    
    for(outcome in outcome_vt){    
      for(strat in strat_vt){
        
        load(paste0(res.dir, "/Betabinomial/", c('NMR','U5MR')[outcome==outcome_vt], "/",
                    country,'_', admin, "_",
                    strat,'_', outcome, "_postsamp.RData"))
        
        data_plot_dt <- NULL
        
        for(year in year_vt){
          
          postsamp_mt <- postsamp_mt_list[[which(year==year_vt)]]$postsamp_mt
          
          # create plotting area names (just admin 1 name if admin = 1, or 'admin2,\n admin1' if admin = 2)
          if (admin == "adm1") {
            admin_name_dt$nameToPlot <- eval(str2lang(poly.label.adm1))
          } else if (admin == "adm2") {
            admin_name_dt$nameToPlot <- paste0(eval(str2lang(poly.label.adm2)),
                                               #need to change if using alternative GADM files
                                               ",\n", poly.adm2@data$NAME_1)
          }
          
          # create data to plot
          data_plot_dt_year <- data.table(Year = year, 
                                          Internal = admin_name_dt[, Internal],
                                          GADM = admin_name_dt[, GADM],
                                          nameToPlot = admin_name_dt[, nameToPlot],
                                          U5MR_mean = apply(postsamp_mt, 1, mean, na.rm = T),
                                          U5MR_median = apply(postsamp_mt, 1, median, na.rm = T),
                                          U5MR_sd = apply(postsamp_mt, 1, sd, na.rm = T),
                                          U5MR_low95 = apply(postsamp_mt, 1, quantile, 0.025, na.rm = T),
                                          U5MR_up95 = apply(postsamp_mt, 1, quantile, 0.975, na.rm = T),
                                          U5MR_low90 = apply(postsamp_mt, 1, quantile, 0.05, na.rm = T),
                                          U5MR_up90 = apply(postsamp_mt, 1, quantile, 0.95, na.rm = T),
                                          U5MR_low80 = apply(postsamp_mt, 1, quantile, 0.1, na.rm = T),
                                          U5MR_up80 = apply(postsamp_mt, 1, quantile, 0.9, na.rm = T))
          
          data_plot_dt <- rbind(data_plot_dt, data_plot_dt_year)
        }
        
        data_plot_dt[, "U5MR_wid95" := U5MR_up95 - U5MR_low95]
        data_plot_dt[, "U5MR_wid90" := U5MR_up90 - U5MR_low90]
        data_plot_dt[, "U5MR_wid80" := U5MR_up80 - U5MR_low80]
        
        #changes names of columns if plotting NMR
        if(outcome=='nmr'){
          colnames(data_plot_dt) <- str_replace(colnames(data_plot_dt),'U5','N')
        }
        rowcount <- ceiling(length(year_vt)/5)
        
        #### plot ####
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                   country, "_", admin, 
                   "_", strat,"_",outcome, "_mean.pdf"),
            width = 10, height = 3.5*rowcount)
        data_plot_dt_df <- as.data.frame(data_plot_dt)
        
        print(SUMMER::mapPlot(data = data_plot_dt_df, 
                              variables="Year", is.long = T,
                              values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_mean"), direction = -1,
                              geo = map_shp[[1]], ncol = 5,
                              by.data = "GADM",
                              legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                              per1000 = TRUE,
                              by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[admin==admin_vt])))
        
        dev.off()
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                   country, "_", admin, 
                   "_", strat,"_",outcome, "_median.pdf"),
            width = 7.5, height = 10)
        
        print(SUMMER::mapPlot(data = data_plot_dt_df, is.long = T,
                              variables = "Year",
                              values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_median"),direction = -1,
                              geo = map_shp[[1]], ncol = 5,
                              legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                              per1000 = TRUE,
                              by.data = "GADM",
                              by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[admin==admin_vt])))
        dev.off()
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                  country, "_", admin, 
                  "_", strat,"_",outcome, "_wid95.pdf"),
            width = 10, height = 3.5*rowcount)
        {
        print(SUMMER::mapPlot(data = data_plot_dt_df, is.long = T, 
                              variables = "Year", 
                              values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_wid95"),direction = -1,
                              geo = map_shp[[1]], ncol = 5,
                              by.data = "GADM",
                              legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                              per1000 = TRUE,
                              by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[admin==admin_vt])))
        }
        dev.off()
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                   country, "_", admin, 
                   "_", strat,"_",outcome, "_2020.pdf"),
            width = 3.5, height = 3.5)
        {
          print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year == 2020,],
                                is.long = T, 
                                variables = "Year", 
                                values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_median"),direction = -1,
                                geo = map_shp[[1]], ncol = 5,
                                legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                                per1000 = TRUE,
                                by.data = "GADM",
                                by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[admin==admin_vt])))
        }
        dev.off()
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/map/",
                   country, "_", admin, 
                   "_", strat,"_",outcome, "_someyearsMedian.pdf"),
            width = 7, height = 7)
        {
          print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year %in% 
                                                         c(2005,2010,2015,2020),],
                                is.long = T, 
                                variables = "Year", 
                                values = paste0(c('NMR','U5MR')[outcome==outcome_vt], "_median"),direction = -1,
                                geo = map_shp[[1]], ncol = 2,
                                legend.label = c("NMR","U5MR")[outcome==outcome_vt],
                                per1000 = TRUE,
                                by.data = "GADM",
                                by.geo = sub(".*data[$]","",c(poly.label.adm1,poly.label.adm2)[admin==admin_vt])))
        }
        dev.off()
        
        
        
        
        
      }
    }
}

#### rank plot ####
  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/NMR/rank'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/rank'))
  }
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/rank'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/U5MR/rank'))
}
  year <- 2020
  for(admin in admin_vt){
    
    admin_name_dt <- as.data.table(get(paste0(c('admin1','admin2')[admin==admin_vt], ".names")))
    
    map_shp <- c(poly.adm1,poly.adm2)[admin==admin_vt][[1]]
    
    # create plotting area names (just admin 1 name if admin = 1, or 'admin2,\n admin1' if admin = 2)
    if (admin == "adm1") {
      admin_name_dt$nameToPlot <- eval(str2lang(poly.label.adm1))
    } else if (admin == "adm2") {
      admin_name_dt$nameToPlot <- paste0(eval(str2lang(poly.label.adm2)),
                                         #need to change if using alternative GADM files
                                         ",\n", poly.adm2@data$NAME_1)
    }
    
    for(outcome in outcome_vt){
      for(strat in strat_vt){
        
        load(paste0(res.dir, "/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],'/',
                    country, "_",
                    admin, "_",
                    strat, "_",
                    outcome, "_postsamp.RData"))
        
          cond <- lapply(postsamp_mt_list, function(x){x$years == year})
          postsamp_mt <- postsamp_mt_list[which(year == year_vt)][[1]]$postsamp_mt
          
          #### rank postsamps ####
          
          rank_mt <- apply(postsamp_mt, 2, rank)
          
          pred_dt <- admin_name_dt
          pred_dt[, "ID"] <- 1:nrow(pred_dt)
          pred_dt[, "avg_rank"] <- apply(rank_mt, 1, mean)
          pred_dt[, "low_rank"] <- apply(rank_mt, 1, min)
          pred_dt[, "up_rank"] <- apply(rank_mt, 1, max)
          
          #### all states hist ####
          rowcount <- ceiling(nrow(pred_dt_order)/3)
          pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/rank/",
                     country, "_",
                     admin, "_", strat, "_", outcome,
                     "_Y", year, "_rankall.pdf"),
              width = 15, height = rowcount*2)
          
        {  
          par(mar = c(2.5, 1, 2, 1), mfcol = c(rowcount, 3))
          
          pred_dt_order <- pred_dt[order(avg_rank)]
          
          for (i in 1:nrow(pred_dt_order)){
            # i <- 1
            
            id <- pred_dt_order[i, ID]
            name <- pred_dt_order[i, nameToPlot]
            
            rank_vt <- rank_mt[id, ]
            
            avg_rank <- pred_dt_order[i, avg_rank]
            
            ranktable <- as.data.table(table(rank_vt))
            ranktable <- merge(data.table(rank = as.character(1:nrow(pred_dt_order))), ranktable, 
                               by.x = "rank", by.y = "rank_vt", all.x = T)
            ranktable[, "rank" := as.integer(rank)]
            ranktable <- ranktable[order(rank)]
            ranktable[is.na(N), "N"] <- 0
            
            barplot(ranktable$N, width = 0.825, 
                    xlim = c(nrow(pred_dt_order), 0), xlab = "", ylab = "",
                    main = paste0(name, "\nER = ", format(round(avg_rank, 1), nsmall = 1)),
                    xaxt = "n", yaxt = "n", col = "#08519c", border = F,
                    cex.main = 0.75)
            axis(1, at = nrow(pred_dt_order):1-0.5, labels = as.character(nrow(pred_dt_order):1), tick = F)
          }
        }
          dev.off()
        
      }
    }
  }


#### ridge plots ####

year_vt <- c(2005,2010,2015,2020)

  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/NMR/ridge'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/ridge'))
  }
  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/U5MR/ridge'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/ridge'))
  }
  
  manual.col <- colorRampPalette(viridis_pal(direction = -1)(10))(10)[c(1,3,6,9)]# 1000)
  
  for(admin in admin_vt){
    
    admin_name_dt <- as.data.table(get(paste0(c('admin1','admin2')[admin==admin_vt], ".names")))
    
    map_shp <- c(poly.adm1,poly.adm2)[admin==admin_vt][[1]]
    
    # create plotting area names (just admin 1 name if admin = 1, or 'admin2,\n admin1' if admin = 2)
    if (admin == "adm1") {
      admin_name_dt$toPlot <- eval(str2lang(poly.label.adm1))
    } else if (admin == "adm2") {
      admin_name_dt$toPlot <- paste0(eval(str2lang(poly.label.adm2)),
                                         #need to change if using alternative GADM files
                                         ",\n", poly.adm2@data$NAME_1)
    }
    
    for(outcome in outcome_vt){
      for(strat in strat_vt){
        load(paste0(res.dir,'/Betabinomial/',c('NMR','U5MR')[outcome==outcome_vt], "/",
                    country, "_",
                    admin, "_", strat,  "_",
                    outcome, "_postsamp.RData"))
        
        data_plot_dt <- NULL
        
        for(year in year_vt){
          
          postsamp_mt <- postsamp_mt_list[which(year==year_vt)][[1]]$postsamp_mt
          
          data_plot_dt_year <- data.table(Year = year, 
                                          Internal = rep(admin_name_dt[, Internal], 1000),
                                          GADM = rep(admin_name_dt[, GADM], 1000),
                                          toPlot = rep(admin_name_dt[, toPlot], 1000),
                                          U5MR = as.numeric(postsamp_mt))
          
          data_plot_dt <- rbind(data_plot_dt, data_plot_dt_year)
        }
        
        # change column name if NMR
        if(outcome=='nmr'){
          colnames(data_plot_dt) <- str_replace(colnames(data_plot_dt),'U5MR','NMR')
        }
        
        data_plot_dt_1 <- data_plot_dt[Year == year_vt[1], ]
        if(outcome=='u5'){
          data_plot_dt_1[, "U5MR_med" := median(U5MR), by = c("Year", "Internal")]
          data_plot_dt_1_order <- data_plot_dt_1[order(U5MR_med, U5MR)]
          data_plot_dt[, U5MRperc := as.numeric(U5MR*1000)]
        }
        if(outcome=='nmr'){
          data_plot_dt_1[, "NMR_med" := median(NMR), by = c("Year", "Internal")]
          data_plot_dt_1_order <- data_plot_dt_1[order(NMR_med, NMR)]
          data_plot_dt[, NMRperc := as.numeric(NMR*1000)]
        }
        
        toPlot_order <- unique(data_plot_dt_1_order$toPlot)
        data_plot_dt[, Year := as.factor(Year)]
        data_plot_dt[, Area := factor(toPlot, levels = rev(toPlot_order))]
        
        rowcount <- ceiling(length(year_vt)/2)
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/ridge/",
                   country, "_",
                   admin, "_", strat,"_", outcome, "_ridgeplot.pdf"),
            width = 12, height = nrow(admin_name_dt)/15*rowcount*4)
        
        if(outcome=='nmr'){
          p <- ggplot(data_plot_dt, aes(x = NMRperc, y = Area))
        }
        if(outcome=='u5'){
          p <- ggplot(data_plot_dt, aes(x = U5MRperc, y = Area))
        }
        p <- p + geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
          scale_fill_gradientn(colours = manual.col,
                               name = c('NMR','U5MR')[outcome==outcome_vt]) +
          facet_wrap( ~ Year, nrow = rowcount) + 
          theme(axis.text.y = element_text(size = 6)) +
          xlab("")
        print(p)
        
        dev.off()
        
        data_plot_dt_1 <- data_plot_dt[Year == 2020, ]
        if(outcome=='nmr'){
          data_plot_dt_1[, "NMR_med" := median(NMR), by = c("Year", "Internal")]
          data_plot_dt_1_order <- data_plot_dt_1[order(NMR_med, NMR)]
          data_plot_dt[, NMRperc := as.numeric(NMR*1000)]
        }
        
        if(outcome=='u5'){
        data_plot_dt_1[, "U5MR_med" := median(U5MR), by = c("Year", "Internal")]
        data_plot_dt_1_order <- data_plot_dt_1[order(U5MR_med, U5MR)]
        data_plot_dt[, U5MRperc := as.numeric(U5MR*1000)]
        }
        
        toPlot_order <- unique(data_plot_dt_1_order$toPlot)
        data_plot_dt[, Year := as.factor(Year)]
        data_plot_dt[, Area := factor(toPlot, levels = rev(toPlot_order))]
        
        ncolumns <- length(unique(data_plot_dt$Internal)) %/% 25
        if(length(unique(data_plot_dt$Internal)) %% 25 != 0){
          ncolumns <- ncolumns + 1
        }
        nPerPlot <- floor(length(unique(data_plot_dt$Internal))/ncolumns)
        for(i in 1:ncolumns){
          if(i < ncolumns){
            tmp.areas <- toPlot_order[((i-1)*nPerPlot+1):(i*nPerPlot)]
          }else{
            tmp.areas <- toPlot_order[((i-1)*nPerPlot+1):length(unique(data_plot_dt$Internal))]
          }
          data_plot_dt$frame[data_plot_dt$toPlot %in% tmp.areas] <- i
        }
        nrows <- 1
        if(ncolumns > 5){
          nrows <- 2
        }
        ncolumns <- min(c(ncolumns,5))
        p <- list()
        for(j in 1:max(data_plot_dt$frame)){
          if(outcome=='nmr'){
            ptmp <- ggplot(data_plot_dt[data_plot_dt$Year == 2020 & data_plot_dt$frame==j,], aes(x = NMRperc, y = Area),) +
            xlim(c(0,quantile(data_plot_dt$NMRperc[data_plot_dt$Year == 2020],
                              .99)))
          }
          if(outcome=='u5'){
            ptmp <- ggplot(data_plot_dt[data_plot_dt$Year == 2020&data_plot_dt$frame==j,], aes(x = U5MRperc, y = Area),) +
              xlim(c(0,quantile(data_plot_dt$U5MRperc[data_plot_dt$Year == 2020],
                                .99)))
          }
          ptmp <- ptmp + geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
            scale_fill_gradientn(colours = manual.col,
                                 name = c("NMR","U5MR")[outcome==outcome_vt]) + 
            xlab("") +
            theme(axis.text.y = element_text(size = 6),
                  legend.position = 'none') +
            ylab("")
          
          p[[j]] <- ptmp
        }
        
        pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/ridge/",
                   country, "_",
                   admin, "_", strat,"_", outcome, "_2020_ridgeplot.pdf"),
            width = ncolumns*2.5 +1, height = 7)
        
        if(nrows  == 1){
          do.call(grid.arrange, c(p, nrow = nrows))
        }else{
          nplots <- length(p)
          npages <- ceiling(nplots/ncolumns)
          for(j in 1:npages){
            if(j < npages){
              plot.ids <- (1:nplots)[((j-1)*ncolumns+1):(j*ncolumns)]
            }else{
              plot.ids <- (1:nplots)[((j-1)*ncolumns+1):nplots]
            }
            p.plot <- p[plot.ids]
            do.call(grid.arrange,c(p.plot, ncol = ncolumns))
          }
        }
        dev.off()
      }
    }
  }

#### TCP plots ####
  # We make true classification probability plot, which colors different admin regions based on the range their NMR/U5MR lies in.

  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/NMR/tcp'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/tcp'))
  }
  if(!dir.exists(paste0(res.dir,
                        '/Figures/Betabinomial/U5MR/tcp'))){
    dir.create(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/tcp'))
  }

  ### parameter setup 
  K_vt <- c(2, 3, 4)
  admin_level_dt <- data.table(Admin = admin_vt,
                               level = level_vt)
  year <- end.proj.year
  
  ### load function
  get_measure_dt <- function(postsamp_mt, pred_dt, grp_thresh){
    # postsamp_mt <- state_postsamp_mt
    # pred_dt <- pred_dt_state
    # grp_thresh <- c(0, 0.5, 1)
    
    # create group lookup table based on thresholds
    n_grp <- length(grp_thresh)-1
    grp_dt <- data.table(grp = 1:n_grp,
                         lower = grp_thresh[1:(n_grp)],
                         upper = grp_thresh[2:(n_grp+1)])
    
    n_area <- nrow(postsamp_mt)
    n_postsamp <- ncol(postsamp_mt)
    
    
    #### assign group based on model max post prob ####
    
    
    grp_cnt_mt <- matrix(0, nrow = n_area, ncol = n_grp)
    for (i in 1:n_grp){
      grp_cnt_mt[, i] <- apply(postsamp_mt, 1, 
                               function(x){sum(x > grp_dt[i, lower] & x <= grp_dt[i, upper])})
    }
    
    DF <- data.frame(grp_cnt_mt)
    DT <- data.table(value = unlist(DF, use.names=FALSE), 
                     colid = 1:nrow(DF), 
                     rowid = rep(names(DF), each=nrow(DF)))
    setkey(DT, colid, value)
    
    grp_cnt_dt_max <- as.data.table(DF)
    grp_cnt_dt_max[, "grp"] <- DT[J(unique(colid), DT[J(unique(colid)), value, mult="last"]), rowid, mult="first"]
    grp_cnt_dt_max[, "TCP" := 0]
    for (i in 1:n_grp){
      idx_grp <- which(grp_cnt_dt_max[, grp] == paste0("X", i))
      grp_cnt_dt_max[idx_grp, "TCP"] <- grp_cnt_dt_max[idx_grp, paste0("X", i), with = F]/n_postsamp
    }
    
    pred_dt[, "grp"] <- grp_cnt_dt_max[, grp]
    pred_dt[, "TCP"] <- grp_cnt_dt_max[, TCP]
    
    return(pred_dt)
  }
  
  
  ### prepare results and plot
  for(outcome in outcome_vt){
    for(admin in admin_vt){
      admin_name_dt <- as.data.table(get(paste0(c('admin1','admin2')[admin==admin_vt], ".names")))
      map_shp <- c(poly.adm1,poly.adm2)[admin==admin_vt][[1]]
      
        for(strat in strat_vt){
          load( file = paste0(res.dir,"/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],'/', country, "_", admin,
                              "_", strat,"_",outcome, "_postsamp.RData"))
          
          cond <- lapply(postsamp_mt_list, function(x){x$years == year})
          postsamp_mt <- postsamp_mt_list[unlist(cond)][[1]]$postsamp_mt
          
          for(K in K_vt){
            
            intv <- 1/K
            quant_vt <- seq(0, 1, intv)
            quant_val_vt <- (quant_vt[1:K]+quant_vt[2:(K+1)])/2
            
            post_med_vt <- as.numeric(postsamp_mt) # apply(postsamp_mt, 1, median)
            
            L_vt <- quantile(post_med_vt, quant_vt)
            L_vt[1] <- quantile(post_med_vt, 0.01)
            L_vt[K+1] <- quantile(post_med_vt, 0.99)
            L_val_vt <- quantile(post_med_vt, quant_val_vt)
            L_dt <- data.table(grp = paste0("X", 1:K),
                               grp_low = L_vt[1:K],
                               grp_up = L_vt[2:(K+1)],
                               grp_val = L_val_vt)
            
            # create plotting area names (just admin 1 name if admin = 1, or 'admin2,\n admin1' if admin = 2)
            if (admin == "adm1") {
              namesToPlot <- eval(str2lang(poly.label.adm1))
            } else if (admin == "adm2") {
              namesToPlot <- paste0(eval(str2lang(poly.label.adm2)),
                                             #need to change if using alternative GADM files
                                             ",\n", poly.adm2@data$NAME_1)
            }
            
            measure_dt <- get_measure_dt(postsamp_mt = postsamp_mt,
                                         pred_dt = data.table(adm_name = paste0(c('admin1','admin2')[admin==admin_vt], "_", 1:nrow(postsamp_mt)),
                                                              adm_name_toPlot = namesToPlot),
                                         grp_thresh = L_vt)
            measure_dt <- merge(measure_dt, L_dt, by = "grp", all.x = T)
            measure_dt[, "K"] <- K
            
            measure_dt[, "Internal" := adm_name]
            measure_dt <- merge(measure_dt, admin_name_dt, by = "Internal", all.x = T)
            measure_dt[, paste0("NAME_", admin_level_dt[Admin == admin, level]) := GADM]
            
            save(measure_dt, L_dt,
                 file = paste0(res.dir,"/Betabinomial/", c('NMR','U5MR')[outcome==outcome_vt], "/", country, "_",
                               c('admin1','admin2')[admin==admin_vt], "_", 
                               strat, "_" ,outcome,"_Y", year,
                               "_K", K, "_measure.RData"))
            
            # set map names to be unique for merging (add in Internal names)
            map_shp$NAME_INTERNAL <- as.character(admin_name_dt[, Internal])
            
            shp_plot <- merge(map_shp, measure_dt,
                              by.x = "NAME_INTERNAL",
                              by.y = "adm_name")
            
            #manual.col <- rev(colorRampPalette(brewer.pal(8, "RdYlGn"))(length(L_dt$grp_val))) # 1000)
            manual.col <- colorRampPalette(viridis_pal(direction = -1)(10))(10)[c(1,3,6,9)]# 1000)
            manual.col[4] <- "navyblue"
            # color.match <- manual.col[1:length(L_dt$grp_val)] # round(sort(L_dt$grp_val)*1000)]
            if(K == 2){
              color.match <- manual.col[c(1,4)]
            }else if(K == 3){
              color.match <- manual.col[c(1,3,4)]
            }else if(K == 4){
              color.match <- manual.col
            }
            lookup_dt <- data.table(grp_val = sort(L_dt$grp_val),
                                    col = color.match)
            shp_plot <- merge(shp_plot, lookup_dt, by = "grp_val", duplicateGeoms = TRUE)
            shp_plot$grp_val <- as.factor(shp_plot$grp_val)
            col_regions <- as.vector(lookup_dt[grp_val %in% shp_plot$grp_val, col])
            
            labelat <- sort(unique(c(L_dt$grp_low, 
                                     L_dt$grp_up)))
            labeltext <- format(round(labelat*1000, 2), nsmall = 2)
            
            #### measure map ####
            
            pdf(paste0(res.dir,"/Figures/Betabinomial/", c('NMR','U5MR')[outcome==outcome_vt], '/tcp/',
                       country, "_", admin,
                       "_", strat, "_", outcome, "_Y", year,
                       "_K", K, "_measuremap.pdf"),
                width = 8, height = 8)
            sp_plot <- spplot(shp_plot, zcol = "grp_val",
                              main = list(label=paste0("ATCP = ", format(round(mean(measure_dt$TCP), 2), nsmall = 2)),
                                          cex=1.5),
                              xlab = "", ylab = "",
                              # sp.layout = list(scale_bar, text_x, text_y),
                              col.regions = col_regions,
                              colorkey = list(col = color.match,
                                              at = labelat,
                                              labels = list(at = labelat, labels = labeltext,
                                                            cex=1.5)))
            print(sp_plot)
            dev.off()
            
          }  
        }
    }
  }
  
  
  
  
  
  
  