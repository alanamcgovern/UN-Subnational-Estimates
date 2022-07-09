
# ENTER COUNTRY OF INTEREST -----------------------------------------------
## Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'

####
# Load libraries and info -------------------------------------------------
####
rm(list = ls())
library(SUMMER)
library(RColorBrewer)
options(gsubfn.engine = "R")
library(rgdal)
library(stringr)

code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
load(file = paste0(home.dir,'/Info/', country, "_general_info.Rdata", sep=''))

####
# Load polygon files ------------------------------------------------------
####
setwd(data.dir)

poly.adm0 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm0))
poly.adm1 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm1))
poly.adm2 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm2))

proj4string(poly.adm0) <- proj4string(poly.adm1) <- proj4string(poly.adm2)
load(paste0('shapeFiles_gadm/', country, '_Amat.rda'))
load(paste0('shapeFiles_gadm/', country, '_Amat_Names.rda'))

####
# Create directories ------------------------------------------------------
####
if(!dir.exists(paste0(res.dir,'/Figures/Direct'))){
  dir.create(paste0(res.dir,'/Figures/Direct'))}
if(!dir.exists(paste0(res.dir,'/Figures/SmoothedDirect'))){
  dir.create(paste0(res.dir,'/Figures/SmoothedDirect'))}
if(!dir.exists(paste0(res.dir,'/Figures/Direct/National'))){
  dir.create(paste0(res.dir,'/Figures/Direct/National'))}
if(!dir.exists(paste0(res.dir,'/Figures/Direct/Admin1'))){
  dir.create(paste0(res.dir,'/Figures/Direct/Admin1'))}
if(!dir.exists(paste0(res.dir,'/Figures/Direct/Admin2'))){
  dir.create(paste0(res.dir,'/Figures/Direct/Admin2'))}

####
# Load estimates ------------------------------------------------------
####
setwd(res.dir)
if(doHIVAdj){
  load(paste0('Direct/',country, '_directHIV_natl_yearly.rda'))
  load(paste0('Direct/',country, '_directHIV_natl.rda'))
  load(paste0('Direct/',country, '_directHIV_admin1.rda'))
  load(paste0('Direct/',country, '_directHIV_admin1_yearly.rda'))
  #if(exists('direct.admin2')){
    load(paste0('Direct/',country, '_directHIV_admin2.rda'))
  #}
}else{
  load( paste0('Direct/',country, '_direct_natl_yearly.rda'))
  load(paste0('Direct/',country, '_direct_natl.rda'))
  load(paste0('Direct/',country, '_direct_admin1.rda'))
  load(paste0('Direct/',country, '_direct_admin1_yearly.rda'))
  #if(exists('direct.admin2')){
    load(paste0('Direct/',country, '_direct_admin2.rda'))
  #}
}

load(paste0('Direct/',country, "_res_natl_SmoothedDirect.rda"))
load(paste0('Direct/',country, "_res_natl_yearly_SmoothedDirect.rda"))
load(paste0('Direct/',country, "_res_admin1_SmoothedDirect.rda"))
load(paste0('Direct/',country, "_res_admin1_SmoothedDirect_yearly.rda"))
load(paste0('Direct/',country, "_res_admin2_SmoothedDirect.rda"))

####
# Define periods ------------------------------------------------------
####
end.year <- max(survey_years)
if(((end.year-beg.year+1) %% 3)==0){
  beg.period.years <- seq(beg.year,end.year,3) 
  end.period.years <- beg.period.years + 2 
}else if(((end.year-beg.year+1) %% 3)==1){
  beg.period.years <- c(beg.year,beg.year+2,seq(beg.year+4,end.year,3))
  end.period.years <- c(beg.year+1,beg.year+3,seq(beg.year+6,end.year,3))
}else if(((end.year-beg.year+1) %% 3)==2){
  beg.period.years <- c(beg.year,seq(beg.year+2,end.year,3))
  end.period.years <- c(beg.year+1,seq(beg.year+4,end.year,3))
}

## years included in survey
periods.survey <- paste(beg.period.years, end.period.years, sep = "-") # convert the periods into string

beg.proj.years <- seq(end.year+1,end.proj.year,3)
end.proj.years <- beg.proj.years+2
proj.per <- paste(beg.proj.years, end.proj.years, sep = "-") # add the 3-year period to be projected

## full time period (including projected years)
periods <- c(periods.survey,proj.per)


####
# Load IGME estimates ------------------------------------------------------
####
setwd(data.dir)
igme.ests <- read.csv(paste0(country.abbrev,'_igme_est.csv'),
                      header = T)
names(igme.ests) <- c('year','OBS_VALUE','LOWER_BOUND','UPPER_BOUND')
igme.ests$year <- igme.ests$year-0.5
igme.ests <- igme.ests[igme.ests$year %in% beg.year:max(end.proj.years),]
igme.ests <- igme.ests[order(igme.ests$year),]

####
# Generate Polygon Plots  ------------------------------------------------------
####

#### Admin1 Direct, by survey ####
setwd(res.dir)
for(plotyears in periods.survey){
  tmp <- direct.admin1[direct.admin1$years == paste(plotyears),]
  tmp$regionPlot <- admin1.names$GADM[match(tmp$region, admin1.names$Internal)]
  
  pdf(paste0("Figures/Direct/Admin1/",
             country, 
             "_admin1_direct_poly_bySurvey_",
             plotyears, ".pdf"),
      width = 3.5, height = 3.5)
  {
    print(SUMMER::mapPlot(data = tmp,
                          is.long = T, 
                          variables = "surveyYears", 
                          values = "mean",direction = -1,
                          geo = poly.adm1, ncol = 2,
                          legend.label = "U5MR",
                          per1000 = TRUE,
                          by.data = "regionPlot",
                          by.geo = paste0(sub(".*data[$]","",poly.label.adm1))))
  }
  dev.off()
}

#### Admin1 Direct, aggregated ####

plotagg.admin1 <- aggregateSurvey(direct.admin1)
plotagg.admin1$regionPlot <- admin1.names$GADM[match(plotagg.admin1$region,
                                                     admin1.names$Internal)]
pdf(paste0("Figures/Direct/Admin1/",
           country,
           "_admin1_direct_poly_Meta.pdf"))
{
  print(SUMMER::mapPlot(data = plotagg.admin1,
                        is.long = T, 
                        variables = "years", 
                        values = "mean",
                        direction = -1,
                        geo = poly.adm1,
                        ncol = 3,
                        legend.label = "U5MR",
                        per1000 = TRUE,
                        by.data = "regionPlot",
                        by.geo = sub(".*data[$]","",poly.label.adm1)))
}
dev.off()

#### Admin1 Smoothed Direct ####
pdf(paste0("Figures/SmoothedDirect/",
           country,
           '_admin1_', 
           'SmoothedDirect_poly.pdf'))
{
  print(SUMMER::mapPlot(data = res.admin1$results,
                        is.long = T, 
                        variables = "years", 
                        values = "median",
                        direction = -1,
                        geo = poly.adm1,
                        ncol = 3,
                        legend.label = "U5MR",
                        per1000 = TRUE,
                        by.data = "region.gadm",
                        by.geo = sub(".*data[$]","",poly.label.adm1)))
  
}
dev.off()

#### Admin2 Direct, by survey ####

for(plotyears in periods.survey){
  tmp <- direct.admin2[direct.admin2$years == paste(plotyears),]
  tmp$regionPlot <- admin2.names$GADM[match(tmp$region, admin2.names$Internal)]
  
  pdf(paste0("Figures/Direct/Admin2/",
             country, 
             "_admin2_direct_poly_bySurvey_",
             plotyears, ".pdf"),
      width = 3.5, height = 3.5)
  {
    print(SUMMER::mapPlot(data = tmp,
                          is.long = T, 
                          variables = "surveyYears", 
                          values = "mean",direction = -1,
                          geo = poly.adm2, ncol = 2,
                          legend.label = "U5MR",
                          per1000 = TRUE,
                          by.data = "regionPlot",
                          #changed for Malawi (generalise later)
                          by.geo = sub(".*data[$]","",poly.label.adm2)))
  }
  dev.off()
}

#### Admin2 Direct, aggregated ####

plotagg.admin2 <- aggregateSurvey(direct.admin2)
plotagg.admin2$regionPlot <- admin2.names$GADM[match(plotagg.admin2$region,
                                                     admin2.names$Internal)]
pdf(paste0("Figures/Direct/Admin2/",
           country,
           "_admin2_direct_poly_Meta.pdf"))
{
  print(SUMMER::mapPlot(data = plotagg.admin2,
                        is.long = T, 
                        variables = "years", 
                        values = "mean",
                        direction = -1,
                        geo = poly.adm2,
                        ncol = 3,
                        legend.label = "U5MR",
                        per1000 = TRUE,
                        by.data = "regionPlot",
                        by.geo =sub(".*data[$]","",poly.label.adm2)))
}
dev.off()

#### Admin2 Smoothed Direct ####

pdf(paste0("Figures/SmoothedDirect/",
           country,
           '_admin2_', 
           'SmoothedDirect_poly.pdf'))
{
  print(SUMMER::mapPlot(data = res.admin2$results,
                        is.long = T, 
                        variables = "years", 
                        values = "median",
                        direction = -1,
                        geo = poly.adm2,
                        ncol = 3,
                        legend.label = "U5MR",
                        per1000 = TRUE,
                        by.data = "region.gadm",
                        by.geo = sub(".*data[$]","",poly.label.adm2)))
  
}
dev.off()





################################################################
################################################################
## Spaghetti Plots for Smoothed Direct Estimates
################################################################
################################################################

################################################################
## National (estimated by 3-year period)
################################################################
cols <- rainbow(length(survey_years))
pane.years <- jitter(end.period.years)

direct.natl$width <- direct.natl$upper - direct.natl$lower
direct.natl$cex2 <- median(direct.natl$width, na.rm = T)/direct.natl$width
direct.natl$cex2[direct.natl$cex2 > 6] <- 6

if(dim(direct.natl)[1] != 0 &
   !(sum(is.na(direct.natl$mean)) == nrow(direct.natl))){
  plot.max <- max(direct.natl$upper+.025, na.rm = T)
}else{plot.max <- 0.25}

pdf(paste0("Figures/SmoothedDirect/",
           country, 
           '_natl_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1))
  if(nrow(direct.natl) > 0 & sum(is.na(direct.natl$mean)) == nrow(direct.natl)){
    plot(NA,
         xlab = "Year",
         ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, max(end.proj.years)),
         type = 'l',
         col = cols[svy.idx],
         lwd = 2,
         main = country)
    
    legend('topright',
           bty = 'n',
           col = c(cols, 'grey37', 'black'),
           lwd = 2, lty = 1,
           legend = c(survey_years,"UN IGME", "Smoothed"))
    
  }else{
  for(survey in survey_years){
    tmp <- direct.natl[direct.natl$surveyYears == survey,]
    svy.idx <- match(survey, survey_years) 
    
    if(svy.idx==1){
      plot(NA, xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
           type = 'l', col = cols[svy.idx], lwd = 2, main = country)
      
      lines(pane.years, tmp$mean, cex = tmp$cex2,
            type = 'l',  col = adjustcolor(cols[svy.idx], 0.35), lwd = 2)
      
      points(pane.years, tmp$mean, pch = 19,
             col = adjustcolor(cols[svy.idx], 0.35),
             cex = tmp$cex2)
      
      #add IGME reference lines
      igme.years <- jitter(beg.year:max(igme.ests$year))
      lines(igme.years,
            igme.ests$OBS_VALUE/1000,
            lwd = 2, col  = 'grey37')
      lines(igme.years,
            igme.ests$UPPER_BOUND/1000,
            lty = 2, col  = 'grey37')
      lines(igme.years,
            igme.ests$LOWER_BOUND/1000, 
            lty = 2, col  = 'grey37')
      
    }else{
      lines(pane.years, tmp$mean, cex = tmp$cex2,
            type = 'l',  col = adjustcolor(cols[svy.idx], 0.35), lwd = 2)
      points(pane.years, tmp$mean, pch = 19, 
             col = adjustcolor(cols[svy.idx], 0.35),
             cex = tmp$cex2)
    }
    
    }
  }
  lines(res.natl$years.num, res.natl$median,
        col = 'black', lwd = 2)
  lines(res.natl$years.num, res.natl$upper,
        col = 'black', lty = 2)
  lines(res.natl$years.num,res.natl$lower, 
        col = 'black', lty = 2)
  legend('topright', bty = 'n',
         col = c(cols, 'grey37','black'),
         lwd = 2, legend = c(survey_years,'UN IGME', "Smoothed"))
  
}
dev.off()

################################################################
## National (estimated yearly)
################################################################
cols <- rainbow(length(survey_years))

direct.natl.yearly$width <- direct.natl.yearly$upper - direct.natl.yearly$lower
direct.natl.yearly$cex2 <- median(direct.natl.yearly$width, na.rm = T)/direct.natl.yearly$width
direct.natl.yearly$cex2[direct.natl.yearly$cex2 > 6] <- 6

if(dim(direct.natl.yearly)[1] != 0 & !(sum(is.na(direct.natl.yearly$mean)) == nrow(direct.natl.yearly))){
  plot.max <- max(direct.natl.yearly$upper+.025, na.rm = T)
}else{plot.max <- 0.25}

pdf(paste0("Figures/SmoothedDirect/",
           country, 
           '_natl_yearly_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1))
  if (nrow(direct.natl.yearly) > 0 & sum(is.na(direct.natl.yearly$mean)) == nrow(direct.natl.yearly)) {
    plot(NA, xlab = "Year", ylab = "U5MR",
         ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
         type = 'l', lwd = 2,col = cols[svy.idx], main = country)
    
    legend('topright', bty = 'n',
           col = c(cols, 'grey37', 'black'),
           lwd = 2,
           legend = c(survey_years,"UN IGME","Smoothed"))
   
  }else{
  for(survey in survey_years){
    tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
    svy.idx <- match(survey, survey_years) 
    pane.years <- jitter(tmp$years)
    
    if(svy.idx==1){
      plot(NA, xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
           type = 'l', col = cols[svy.idx], lwd = 2, main = country)
      
      lines(pane.years, tmp$mean, cex = tmp$cex2,
            type = 'l', col = cols[svy.idx], lwd = 2)
      
      points(pane.years, tmp$mean, pch = 19,
             col = adjustcolor(cols[svy.idx], 0.35),
             cex = tmp$cex2)
      
      #add IGME reference lines
      igme.years <- jitter(beg.year:max(igme.ests$year))
      lines(igme.years,
            igme.ests$OBS_VALUE/1000,
            lwd = 2, col  = 'grey37')
      lines(igme.years,
            igme.ests$UPPER_BOUND/1000,
            lty = 2, col  = 'grey37')
      lines(igme.years,
            igme.ests$LOWER_BOUND/1000, 
            lty = 2, col  = 'grey37')
      
    }else{
      lines(pane.years, tmp$mean, cex = tmp$cex2,
            type = 'l', col = cols[svy.idx], lwd = 2)
      points(pane.years, tmp$mean, pch = 19, 
             col = adjustcolor(cols[svy.idx], 0.35),
             cex = tmp$cex2)
    }
  }
  }
  
  lines(res.natl.yearly$years.num, res.natl.yearly$median,
        col = 'black', lwd = 2)
  lines(res.natl.yearly$years.num, res.natl.yearly$upper,
        col = 'black', lty = 2)
  lines(res.natl.yearly$years.num,res.natl.yearly$lower, 
        col = 'black', lty = 2)
  legend('topright', bty = 'n',
         col = c(cols, 'grey37','black'),
         lwd = 2, legend = c(survey_years,"UN IGME", "Smoothed"))
}
dev.off()
















################################################################
## Admin1 (estimated by 3-year period)
################################################################
pdf(paste0("Figures/SmoothedDirect/",country, 
           '_admin1_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1),lend=1)
  for(area in 1:dim(poly.adm1)[1]){
    tmp.area <- direct.admin1[direct.admin1$region == 
                                as.character(admin1.names$Internal[area]),]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    
    res.area <- res.admin1$results[res.admin1$results$region == as.character(admin1.names$Internal[area]),]
    
    if(dim(tmp.area)[1] != 0 &!(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(tmp.area$upper+.025, na.rm = T)
    }else{plot.max <- 0.25}
   
   if(nrow(tmp.area) > 0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)){
      plot(NA, xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
           type = 'l', col = cols[svy.idx],
           lwd = 2, main = admin1.names$GADM[area])
      legend('topright', bty = 'n',
             col = c(cols, 'black'),
             lwd = 2, 
             legend = c(survey_years,  "Smoothed"))
    } else { 
    for(survey in survey_years){
        tmp <- tmp.area[tmp.area$surveyYears == survey,]
        svy.idx <- match(survey, survey_years) 
        pane.years <- jitter(end.period.years)
        
        if(svy.idx== 1){
            plot(NA,  xlab = "Year",  ylab = "U5MR",
                 ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
                 type = 'l', col = cols[svy.idx],
                 lwd = 2, main = admin1.names$GADM[area])
            
            lines(pane.years, tmp$mean,
                  cex = tmp$cex2,  type = 'l',
                  col = cols[svy.idx], lwd = 2)
            
            points(pane.years, tmp$mean,
                   pch = 19, col = adjustcolor(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
        }else{
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = survey_years[svy.idx], lwd = 2)
            points(pane.years, tmp$mean,  pch = 19,
                   col = adjustcolor(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
         }
      }
  }    
    lines(res.area$years.num,res.area$median,
            col = 'black', lwd = 2)
    lines(res.area$years.num, res.area$upper,
            col = 'black', lty = 2)
    lines(res.area$years.num,
            res.area$lower, col = 'black', lty = 2)
      
    legend('topright',
             bty = 'n', col = c(cols,  'black'),
             lwd = 2, 
             legend = c(survey_years,"Smoothed"))
  }
}
dev.off()

################################################################
## Admin2 (estimated by 3-year period)
################################################################
pdf(paste0("Figures/SmoothedDirect/",country, 
           '_admin2_SmoothedDirect_spaghetti.pdf'),
    height = 6, width = 6)
{
  par(mfrow=c(1,1),lend=1)
  for(area in 1:dim(poly.adm2)[1]){
    tmp.area <- direct.admin2[direct.admin2$region == 
                                as.character(admin2.names$Internal[area]),]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    
    res.area <- res.admin2$results[res.admin2$results$region == as.character(admin2.names$Internal[area]),]
    
    if(dim(tmp.area)[1] != 0 &!(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(tmp.area$upper+.025, na.rm = T)
    }else{plot.max <- 0.25}
   
    if(nrow(tmp.area) > 0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)){
      plot(NA, xlab = "Year",
           ylab = "U5MR", ylim = c(0, plot.max),
           xlim = c(beg.year, max(end.proj.years)), type = 'l',
           col = cols[svy.idx], lwd = 2,
           main = admin2.names$GADM[area])
      legend('topright', bty = 'n',
             col = c(cols, 'black'),
             lwd = 2, legend = c(surveys,"Smoothed"))
    } else {
    for(survey in survey_years){
      tmp <- tmp.area[tmp.area$surveyYears == survey,]
      svy.idx <- match(survey, survey_years) 
      pane.years <- jitter(end.period.years)
      
      if(svy.idx== 1){
        plot(NA,  xlab = "Year",  ylab = "U5MR",
             ylim = c(0, plot.max), xlim = c(beg.year, max(end.proj.years)),
             type = 'l', col = cols[svy.idx],
             lwd = 2, main = admin2.names$GADM[area])
        
        if(nrow(tmp)!=0){
        lines(pane.years, tmp$mean,
              cex = tmp$cex2,  type = 'l',
              col = cols[svy.idx], lwd = 2)
        
        points(pane.years, tmp$mean,
               pch = 19, col = adjustcolor(cols[svy.idx], 0.35),
               cex = tmp$cex2)
        }
        
      }else{
        if(nrow(tmp)!=0){
        lines(pane.years, tmp$mean, cex = tmp$cex2,
              type = 'l', col = cols[svy.idx],
              main = survey_years[svy.idx], lwd = 2)
        points(pane.years, tmp$mean,  pch = 19,
               col = adjustcolor(cols[svy.idx], 0.35),
               cex = tmp$cex2)
        }
      }
    }
  }
    
    lines(res.area$years.num,res.area$median,
          col = 'black', lwd = 2)
    lines(res.area$years.num, res.area$upper,
          col = 'black', lty = 2)
    lines(res.area$years.num,
          res.area$lower, col = 'black', lty = 2)
    
    legend('topright',
           bty = 'n', col = c(cols, 'black'),
           lwd = 2, 
           legend = c(survey_years,"Smoothed"))
  }
}
dev.off()
