rm(list=ls())

#### Libraries ####
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
library(rgdal)
library(ggplot2)
library(SUMMER)

#### Parameters ####

# ARE WE TESTING/DEBUGGING THE CODE (i.e. debugging/editing within a forloop)
testing <- FALSE
country_vt <- c("Nigeria") 
admin_vt <- c("admin1","admin2")[1] ## admin level
level_vt <- c(1,2)[1] ## polygon level

#### If you are doing 
timemod_vt <- c("rw2main_randomSlopes_ar1xICAR",
                "rw2main_randomSlopes_rw1xICAR")[2] ## if Notstrat
# timemod_vt <- c("frameAgg_rw2main_randomSlopes_ar1xICAR",
#                 "frameAgg_rw2main_randomSlopes_rw1xICAR") ## if Strat

## Either NotStrat, Strat
strat_vt <- c("NotStrat") 



data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
setwd(data.dir)

if(!exists("sheet_key", envir = .GlobalEnv)){
  source(paste0(code.dir.rel,'/LoadCommandCenter.R'))
}
CountryList <- range_read(sheet_key, sheet = "CountryList")
# CountryList <- read.csv("CountryList.csv", header = T)

folder.names <- CountryList$folderName[CountryList$Country %in% country_vt]
code_vt <- CountryList$gadmCode[CountryList$Country %in% country_vt]

country_code_dt <- data.table(Country = country_vt,
                              code = code_vt)

admin_level_dt <- data.table(Admin = admin_vt,
                             level = level_vt)

#### postsamp ####
years_vt <- 1990:2019
n_years <- length(years_vt)
n_samp <- 1000

for (country in country_vt){
  folder.name <- folder.names[match(country,country_vt)]
  for(timemod in timemod_vt){
    for(admin in admin_vt){
      for(strat in strat_vt){
        
        # if (testing) {
        #   country <- country_vt[1]
        #   timemod <- timemod_vt[1]
        #   admin <- admin_vt[1]
        #   strat <- strat_vt[1]
        # }
        
        load(paste0(folder.name, "/",
                    country, "_res_",
                    timemod, "_", admin, "Bench.rda"))
        
        if(strat == "NotStrat"){
          res_obj <- get(paste0("res.", admin))
        }else if(strat == "Strat"){
          res_obj <- get(paste0("res.frame.agg.", admin ))
        }
        
        draws_list <- res_obj$draws.est
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
        
        save(postsamp_mt_list, 
             file = paste0(folder.name,
                           "/", country, "_",
                           timemod, "_", admin,
                           "_", strat, "_postsamp.RData"))
        
      }
    }
  }
}


#### map ####
year_vt <- 1990:2019

for (country in country_vt){
  folder.name <- folder.names[match(country, country_vt)]
  if(!dir.exists(paste0(folder.name,
                        '/Plots/Betabinomial/map'))){
    dir.create(paste0(folder.name,
                      '/Plots/Betabinomial/map'))
  }
  for(admin in admin_vt){
    
    if (testing) {
      country <- country_vt[1]
      admin <- admin_vt[1]
    }
    
    map_shp <- readOGR(dsn = paste0(folder.name, shapes.sub.dir), 
                       layer = paste0("gadm36_", country_code_dt[Country == country, code], "_", 
                                      admin_level_dt[Admin == admin, level]), 
                       stringsAsFactors = F)
    load(paste0(folder.name,
                shapes.sub.dir,
                '/', country,
                "_Amat_Names.rda"))
    
    admin_name_dt <- as.data.table(get(paste0(admin, ".names")))
    
    for(timemod in timemod_vt){
      for(strat in strat_vt){
        
        if (testing) {
          timemod <- timemod_vt[1]
          strat <- strat_vt[1]
        }
        
        load(paste0(folder.name, "/", country, "_",
                    timemod, "_", admin, "_",
                    strat, "_postsamp.RData"))
        
        data_plot_dt <- NULL
        
        for(year in year_vt){
          
          if (testing) {
            year <- year_vt[1]
          }
          
          cond <- lapply(postsamp_mt_list, function(x){x$years == year})
          postsamp_mt <- postsamp_mt_list[unlist(cond)][[1]]$postsamp_mt
          
          # create plotting area names (just admin 1 name if admin = 1, or 'admin2,\n admin1' if admin = 2)
          if (admin == "admin1") {
            admin_name_dt$nameToPlot <- map_shp$NAME_1
          } else if (admin == "admin2") {
            admin_name_dt$nameToPlot <- paste0(map_shp$NAME_2,
                                               ",\n", map_shp$NAME_1)
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
          
          data_plot_dt_year[, paste0("NAME_", admin_level_dt[Admin == admin, level]) := GADM]
          
          data_plot_dt <- rbind(data_plot_dt, data_plot_dt_year)
        }
        
        data_plot_dt[, "U5MR_wid95" := U5MR_up95 - U5MR_low95]
        data_plot_dt[, "U5MR_wid90" := U5MR_up90 - U5MR_low90]
        data_plot_dt[, "U5MR_wid80" := U5MR_up80 - U5MR_low80]
        
        rowcount <- ceiling(length(year_vt)/5)
        
        #### plot ####
        pdf(paste0(folder.name, "/Plots/Betabinomial/map/",
                   country, "_", timemod, "_", admin,
                   "_", strat, "_mean.pdf"),
            width = 10, height = 3.5*rowcount)
        data_plot_dt_df <- as.data.frame(data_plot_dt)
        print(SUMMER::mapPlot(data = data_plot_dt_df, 
                              "Year", is.long = T,
                              values = "U5MR_mean", direction = -1,
                              geo = map_shp, ncol = 5,
                              by.data = "GADM",
                              legend.label = "U5MR",
                              per1000 = TRUE,
                              by.geo = paste0("NAME_",
                                              admin_level_dt[Admin == admin, level])))
        dev.off()
        
        pdf(paste0(folder.name, "/Plots/Betabinomial/map/",
                   country, "_", timemod, "_", admin,
                   "_", strat, "_median.pdf"),
            width = 7.5, height = 10)
        
        data_plot_dt_df <- as.data.frame(data_plot_dt)
        print(SUMMER::mapPlot(data = data_plot_dt_df, is.long = T,
                              variables = "Year",
                              values = "U5MR_median",direction = -1,
                              geo = map_shp, ncol = 5,
                              legend.label = "U5MR",
                              per1000 = TRUE,
                              by.data = "GADM",
                              by.geo = paste0("NAME_",
                                              admin_level_dt[Admin == admin, level])))
        dev.off()
        
        
        pdf(paste0(folder.name, "/Plots/Betabinomial/map/",
                   country, "_", timemod, "_", admin,
                   "_", strat, "_wid90.pdf"),
            width = 10, height = 3.5*rowcount)
        
        data_plot_dt_df <- as.data.frame(data_plot_dt)
        print(SUMMER::mapPlot(data = data_plot_dt_df, is.long = T, 
                              variables = "Year", 
                              values = "U5MR_wid90",direction = -1,
                              geo = map_shp, ncol = 5,
                              by.data = "GADM",
                              legend.label = "U5MR",
                              per1000 = TRUE,
                              by.geo = paste0("NAME_", 
                                              admin_level_dt[Admin == admin, level])))
        dev.off()
        
        pdf(paste0(folder.name, "/Plots/Betabinomial/map/",
                   country, "_", timemod, "_",
                   admin, "_", strat, "_2019_median.pdf"),
            width = 3.5, height = 3.5)
        {
          
          data_plot_dt_df <- as.data.frame(data_plot_dt)
          print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year == 2019,],
                                is.long = T, 
                                variables = "Year", 
                                values = "U5MR_median",direction = -1,
                                geo = map_shp, ncol = 5,
                                legend.label = "U5MR",
                                per1000 = TRUE,
                                by.data = "GADM",
                                by.geo = paste0("NAME_",
                                                admin_level_dt[Admin == admin, level])))
        }
        dev.off()
        pdf(paste0(folder.name, "/Plots/Betabinomial/map/",
                   country, "_", timemod, "_",
                   admin, "_", strat, "_2009_median.pdf"),
            width = 3.5, height = 3.5)
        {
          data_plot_dt_df <- as.data.frame(data_plot_dt)
          print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year == 2009,],
                                is.long = T, 
                                variables = "Year", 
                                values = "U5MR_median",direction = -1,
                                geo = map_shp, ncol = 5,
                                legend.label = "U5MR",
                                per1000 = TRUE,
                                by.data = "GADM",
                                by.geo = paste0("NAME_",
                                                admin_level_dt[Admin == admin, level])))
        }
        dev.off()
        pdf(paste0(folder.name, "/Plots/Betabinomial/map/",
                   country, "_", timemod, "_",
                   admin, "_", strat, "_1999_median.pdf"),
            width = 3.5, height = 3.5)
        {
          
          data_plot_dt_df <- as.data.frame(data_plot_dt)
          print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year == 1999,],
                                is.long = T, 
                                variables = "Year", 
                                values = "U5MR_median",direction = -1,
                                geo = map_shp, ncol = 5,
                                legend.label = "U5MR",
                                per1000 = TRUE,
                                by.data = "GADM",
                                by.geo = paste0("NAME_",
                                                admin_level_dt[Admin == admin, level])))
        }
        dev.off()
        pdf(paste0(folder.name, "/Plots/Betabinomial/map/",
                   country, "_", timemod, "_",
                   admin, "_", strat, "_1990_median.pdf"),
            width = 3.5, height = 3.5)
        {
          
          data_plot_dt_df <- as.data.frame(data_plot_dt)
          print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year == 1990,],
                                is.long = T, 
                                variables = "Year", 
                                values = "U5MR_median",direction = -1,
                                geo = map_shp, ncol = 5,
                                legend.label = "U5MR",
                                per1000 = TRUE,
                                by.data = "GADM",
                                by.geo = paste0("NAME_",
                                                admin_level_dt[Admin == admin, level])))
        }
        dev.off()
        
        pdf(paste0(folder.name, "/Plots/Betabinomial/map/",
                   country, "_", timemod, "_",
                   admin, "_", strat, "_19902019_median.pdf"),
            width = 7, height = 7)
        {
          
          data_plot_dt_df <- as.data.frame(data_plot_dt)
          print(SUMMER::mapPlot(data = data_plot_dt_df[data_plot_dt_df$Year %in% 
                                                         c(1990, 1999, 2009, 2019),],
                                is.long = T, 
                                variables = "Year", 
                                values = "U5MR_median",direction = -1,
                                geo = map_shp, ncol = 2,
                                legend.label = "U5MR",
                                per1000 = TRUE,
                                by.data = "GADM",
                                by.geo = paste0("NAME_",
                                                admin_level_dt[Admin == admin, level])))
        }
        dev.off()
        
        
        
        
        
      }
    }
  }
}

#### measure ####
K_vt <- c(2, 3, 4)
year_vt <- c(2016:2019)


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

for (country in country_vt){
  folder.name <- folder.names[match(country, country_vt)]
  if(!dir.exists(paste0(folder.name,
                        '/Plots/Betabinomial/measure'))){
    dir.create(paste0(folder.name,
                      '/Plots/Betabinomial/measure'))
  }
  for(admin in admin_vt){
    
    if (testing) {
      country <- country_vt[1]
      admin <- admin_vt[1]
    }
    
    map_shp <- readOGR(dsn = paste0(folder.name, shapes.sub.dir), 
                       layer = paste0("gadm36_", country_code_dt[Country == country, code], "_", 
                                      admin_level_dt[Admin == admin, level]), 
                       stringsAsFactors = F)
    load(paste0(folder.name, shapes.sub.dir,
                '/', country, "_Amat_Names.rda"))
    
    admin_name_dt <- as.data.table(get(paste0(admin, ".names")))
    
    for(timemod in timemod_vt){
      for(strat in strat_vt){
        
        if (testing) {
          timemod <- timemod_vt[1]
          strat <- strat_vt[1]
        }
        
        load(paste0(folder.name, "/", country,
                    "_", timemod, "_", admin,
                    "_", strat, "_postsamp.RData"))
        
        for(year in year_vt){
          
          if (testing) {
            year <- year_vt[1]
          }
          
          cond <- lapply(postsamp_mt_list, function(x){x$years == year})
          postsamp_mt <- postsamp_mt_list[unlist(cond)][[1]]$postsamp_mt
          
          for(K in K_vt){
            
            if (testing) {
              K <- K_vt[1]
            }
            
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
            
            # set plotting names (different for admin 1 and 2)
            if (admin == "admin1") {
              namesToPlot <- map_shp$NAME_1
            } else if (admin == "admin2") {
              namesToPlot <- paste0(map_shp$NAME_2,
                                    ",\n", map_shp$NAME_1)
            }
            
            measure_dt <- get_measure_dt(postsamp_mt = postsamp_mt,
                                         pred_dt = data.table(adm_name = paste0(admin, "_", 1:nrow(postsamp_mt)),
                                                              adm_name_toPlot = namesToPlot),
                                         grp_thresh = L_vt)
            measure_dt <- merge(measure_dt, L_dt, by = "grp", all.x = T)
            measure_dt[, "K"] <- K
            
            measure_dt[, "Internal" := adm_name]
            measure_dt <- merge(measure_dt, admin_name_dt, by = "Internal", all.x = T)
            measure_dt[, paste0("NAME_", admin_level_dt[Admin == admin, level]) := GADM]
            
            save(measure_dt, L_dt,
                 file = paste0(folder.name, "/",
                               country, "_",
                               timemod, "_",
                               admin, "_", 
                               strat, "_Y", year,
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
            shp_plot <- merge(shp_plot, lookup_dt, by = "grp_val")
            shp_plot$grp_val <- as.factor(shp_plot$grp_val)
            col_regions <- as.vector(lookup_dt[grp_val %in% shp_plot$grp_val, col])
            
            labelat <- sort(unique(c(L_dt$grp_low, 
                                     L_dt$grp_up)))
            labeltext <- format(round(labelat*1000, 2), nsmall = 2)
            
            #### measure map ####
            
            pdf(paste0(folder.name, "/Plots/Betabinomial/measure/",
                       country, "_", timemod, "_", admin,
                       "_", strat, "_Y", year,
                       "_K", K, "_measuremap.pdf"),
                width = 4, height = 4)
            sp_plot <- spplot(shp_plot, zcol = "grp_val",
                              main = paste0("ATCP = ", format(round(mean(measure_dt$TCP), 2), nsmall = 2)),
                              xlab = "", ylab = "",
                              # sp.layout = list(scale_bar, text_x, text_y),
                              col.regions = col_regions,
                              colorkey = list(col = color.match,
                                              at = labelat,
                                              labels = list(at = labelat, labels = labeltext)))
            print(sp_plot)
            dev.off()
            
            #### stack histogram ####
            L_dt[, "grp_name" := paste0("(", format(round(grp_low*100, 2), nsmall = 2), "%, ", format(round(grp_up*100, 2), nsmall = 2), "%]")]
            measure_dt[, "grp_name" := paste0("(", format(round(grp_low*100, 2), nsmall = 2), "%, ", format(round(grp_up*100, 2), nsmall = 2), "%]")]
            measure_dt$grp_name <- factor(measure_dt$grp_name, rev(L_dt$grp_name))
            
            pdf(paste0(folder.name,
                       "/Plots/Betabinomial/measure/",
                       country, "_", timemod, "_", admin,
                       "_", strat, "_Y", year, "_K",
                       K, "_measurehist.pdf"),
                width = 4, height = 4)
            par(mar = c(4, 4, 2, 2))
            histStack(TCP ~ grp_name, 
                      data = measure_dt,
                      col = rev(color.match), 
                      main = paste0("ATCP = ", format(round(mean(measure_dt$TCP), 2), nsmall = 2)),
                      xlab = "TCP", cex.legend = 0.5,
                      legend.pos = "topleft", 
                      breaks = seq(0,1, 0.05))
            abline(v = mean(measure_dt$TCP), col = "blue", lwd = 2)
            dev.off()
            
          }
        }
      }
    }
  }
}

#### rank ####
year_vt <- c(2016:2019)

for (country in country_vt){
  folder.name <- folder.names[match(country, country_vt)]
  if(!dir.exists(paste0(folder.name,
                        '/Plots/Betabinomial/rank'))){
    dir.create(paste0(folder.name,
                      '/Plots/Betabinomial/rank'))
  }
  for(admin in admin_vt){
    
    load(paste0(folder.name, shapes.sub.dir,
                '/', country, "_Amat_Names.rda"))
    
    admin_name_dt <- as.data.table(get(paste0(admin, ".names")))
    
    map_shp <- readOGR(dsn = paste0(folder.name, shapes.sub.dir), 
                       layer = paste0("gadm36_", country_code_dt[Country == country, code], "_", 
                                      admin_level_dt[Admin == admin, level]), 
                       stringsAsFactors = F)
    
    # set plotting names (different for admin 1 and 2)
    if (admin == "admin1") {
      admin_name_dt$toPlot <- map_shp$NAME_1
    } else if (admin == "admin2") {
      admin_name_dt$toPlot <- paste0(map_shp$NAME_2, ", ", map_shp$NAME_1)
    }
    
    for(timemod in timemod_vt){
      for(strat in strat_vt){
        
        load(paste0(folder.name, "/",
                    country, "_",
                    timemod, "_",
                    admin, "_",
                    strat, "_postsamp.RData"))
        
        for(year in year_vt){
          
          cond <- lapply(postsamp_mt_list, function(x){x$years == year})
          postsamp_mt <- postsamp_mt_list[unlist(cond)][[1]]$postsamp_mt
          
          #### rank postsamps ####
          
          rank_mt <- apply(postsamp_mt, 2, rank)
          
          pred_dt <- admin_name_dt
          pred_dt[, "ID"] <- 1:nrow(pred_dt)
          pred_dt[, "avg_rank"] <- apply(rank_mt, 1, mean)
          pred_dt[, "low_rank"] <- apply(rank_mt, 1, min)
          pred_dt[, "up_rank"] <- apply(rank_mt, 1, max)
          
          # top 5 state
          pdf(paste0(folder.name, "/Plots/Betabinomial/rank/",
                     country, "_", timemod, "_",
                     admin, "_", strat,
                     "_Y", year, "_rankupp5.pdf"),
              width = 2.5, height = 5)
          par(mar = c(2.5, 1, 2, 1), mfrow = c(5, 1))
          
          pred_dt_order <- pred_dt[order(avg_rank)]
          
          for (i in 1:5){
            
            id <- pred_dt_order[i, ID]
            name <- pred_dt_order[i, toPlot]
            
            rank_vt <- rank_mt[id, ]
            rank_vt <- ifelse(rank_vt <= 10, rank_vt, 10)
            
            avg_rank <- pred_dt_order[i, avg_rank]
            
            ranktable <- as.data.table(table(rank_vt))
            ranktable <- merge(data.table(rank = as.character(1:10)), ranktable, 
                               by.x = "rank", by.y = "rank_vt", all.x = T)
            ranktable[, "rank" := as.integer(rank)]
            ranktable <- ranktable[order(rank)]
            ranktable[is.na(N), "N"] <- 0
            
            barplot(ranktable$N, width = 0.825, 
                    xlim = c(10, 0), xlab = "", ylab = "",
                    main = paste0(name, "\nER = ", format(round(avg_rank, 1), nsmall = 1)),
                    xaxt = "n", yaxt = "n", col = "#31a354", border = F,
                    cex.main = 0.75)
            axis(1, at = 10:1-0.5, labels = c("10+", as.character(9:1)), tick = F)
          }
          dev.off()
          
          # bottom 5 state
          pdf(paste0(folder.name, "/Plots/Betabinomial/rank/",
                     country, "_", timemod, "_",
                     admin, "_", strat,
                     "_Y", year, "_ranklow5.pdf"),
              width = 2.5, height = 5)
          par(mar = c(2.5, 1, 2, 1), mfrow = c(5, 1))
          
          pred_dt_order <- pred_dt[order(-avg_rank)]
          
          for (i in 1:5){
            
            id <- pred_dt_order[i, ID]
            name <- pred_dt_order[i, toPlot]
            
            avg_rank <- pred_dt_order[i, avg_rank]
            
            rank_vt <- rank_mt[id, ]
            rank_vt <- ifelse(rank_vt >= (nrow(pred_dt_order)-9), rank_vt, (nrow(pred_dt_order)-9))
            
            ranktable <- as.data.table(table(rank_vt))
            ranktable <- merge(data.table(rank = as.character(nrow(pred_dt_order):(nrow(pred_dt_order)-9))), ranktable, 
                               by.x = "rank", by.y = "rank_vt", all.x = T)
            ranktable[, "rank" := as.integer(rank)]
            ranktable <- ranktable[order(rank)]
            ranktable[is.na(N), "N"] <- 0
            
            barplot(ranktable$N, width = 0.825, 
                    xlim = c(10, 0), xlab = "", ylab = "",
                    main = paste0(name, "\nER = ", format(round(avg_rank, 1), nsmall = 1)),
                    xaxt = "n", yaxt = "n", col = "#fc4e2a", border = F,
                    cex.main = 0.75)
            axis(1, at = 10:1-0.5, labels = c(as.character(nrow(pred_dt_order):(nrow(pred_dt_order)-8)), paste0(nrow(pred_dt_order)-9, "-")), tick = F)
          }
          dev.off()
          
          #### all states hist ####
          rowcount <- ceiling(nrow(pred_dt_order)/3)
          pdf(paste0(folder.name, "/Plots/Betabinomial/rank/",
                     country, "_", timemod, "_",
                     admin, "_", strat,
                     "_Y", year, "_rankall.pdf"),
              width = 15, height = rowcount*2)
          par(mar = c(2.5, 1, 2, 1), mfcol = c(rowcount, 3))
          
          pred_dt_order <- pred_dt[order(avg_rank)]
          
          for (i in 1:nrow(pred_dt_order)){
            # i <- 1
            
            id <- pred_dt_order[i, ID]
            name <- pred_dt_order[i, toPlot]
            
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
          dev.off()
        }
      }
    }
  }
}

#### ridge ####

year_vt <- c(1990, 1999, 2009, 2019)

for (country in country_vt){
  folder.name <- folder.names[match(country, country_vt)]
  if(!dir.exists(paste0(folder.name,
                        '/Plots/Betabinomial/ridge'))){
    dir.create(paste0(folder.name,
                      '/Plots/Betabinomial/ridge'))
  }
  for(admin in admin_vt){
    load(paste0(folder.name, shapes.sub.dir, '/',
                country, "_Amat_Names.rda"))
    
    admin_name_dt <- as.data.table(get(paste0(admin, ".names")))
    
    map_shp <- readOGR(dsn = paste0(folder.name, shapes.sub.dir), 
                       layer = paste0("gadm36_", country_code_dt[Country == country, code], "_", 
                                      admin_level_dt[Admin == admin, level]), 
                       stringsAsFactors = F)
    
    # set plotting names (different for admin 1 and 2)
    if (admin == "admin1") {
      admin_name_dt$toPlot <- map_shp$NAME_1
    } else if (admin == "admin2") {
      admin_name_dt$toPlot <- paste0(map_shp$NAME_2, ",\n", map_shp$NAME_1)
    }
    
    for(timemod in timemod_vt){
      for(strat in strat_vt){
        load(paste0(folder.name, "/",
                    country, "_",
                    timemod, "_",
                    admin, "_",
                    strat, "_postsamp.RData"))
        
        data_plot_dt <- NULL
        
        for(year in year_vt){
          
          cond <- lapply(postsamp_mt_list, function(x){x$years == year})
          postsamp_mt <- postsamp_mt_list[unlist(cond)][[1]]$postsamp_mt
          
          data_plot_dt_year <- data.table(Year = year, 
                                          Internal = rep(admin_name_dt[, Internal], 1000),
                                          GADM = rep(admin_name_dt[, GADM], 1000),
                                          toPlot = rep(admin_name_dt[, toPlot], 1000),
                                          U5MR = as.numeric(postsamp_mt))
          
          data_plot_dt <- rbind(data_plot_dt, data_plot_dt_year)
        }
        
        data_plot_dt_1 <- data_plot_dt[Year == year_vt[1], ]
        data_plot_dt_1[, "U5MR_med" := median(U5MR), by = c("Year", "Internal")]
        data_plot_dt_1_order <- data_plot_dt_1[order(U5MR_med, U5MR)]
        toPlot_order <- unique(data_plot_dt_1_order$toPlot)
        
        data_plot_dt[, Year := as.factor(Year)]
        data_plot_dt[, Area := factor(toPlot, levels = rev(toPlot_order))]
        data_plot_dt[, U5MRperc := as.numeric(U5MR*1000)]
        
        rowcount <- ceiling(length(year_vt)/4)
        
        pdf(paste0(country, "/Plots/Betabinomial/ridge/",
                   country, "_", timemod, "_",
                   admin, "_", strat, "_ridgeplot.pdf"),
            width = 12, height = nrow(admin_name_dt)/15*rowcount*4)
        
        p <- ggplot(data_plot_dt, aes(x = U5MRperc, y = Area)) +
          geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
          scale_fill_gradientn(colours = manual.col,
                               name = "U5MR") +
          facet_wrap( ~ Year, nrow = rowcount) + 
          theme(axis.text.y = element_text(size = 6)) +
          xlab("")
        print(p)
        
        dev.off()
        
        data_plot_dt_1 <- data_plot_dt[Year == 2019, ]
        data_plot_dt_1[, "U5MR_med" := median(U5MR), by = c("Year", "Internal")]
        data_plot_dt_1_order <- data_plot_dt_1[order(U5MR_med, U5MR)]
        toPlot_order <- unique(data_plot_dt_1_order$toPlot)
        
        data_plot_dt[, Year := as.factor(Year)]
        data_plot_dt[, Area := factor(toPlot, levels = rev(toPlot_order))]
        data_plot_dt[, U5MRperc := as.numeric(U5MR*1000)]
        
        
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
          ptmp <- ggplot(data_plot_dt[data_plot_dt$Year == 2019 &
                                        data_plot_dt$frame == j,],
                         aes(x = U5MRperc, y = Area),) +
            geom_density_ridges_gradient(aes(fill = ..x..), scale = 3, size = 0.3) +
            scale_fill_gradientn(colours = manual.col,
                                 name = "U5MR") + 
            xlim(c(0,quantile(data_plot_dt$U5MRperc[data_plot_dt$Year == 2019],
                              .99))) + 
            xlab("") +
            theme(axis.text.y = element_text(size = 6),
                  legend.position = 'none') +
            ylab("")
          
          p[[j]] <- ptmp
        }
        pdf(paste0(country, "/Plots/Betabinomial/ridge/",
                   country, "_",
                   timemod, "_",
                   admin, "_",
                   strat, "_2019_ridgeplot.pdf"),
            height = 9, width = ncolumns*2.5 +1)
        
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
}

