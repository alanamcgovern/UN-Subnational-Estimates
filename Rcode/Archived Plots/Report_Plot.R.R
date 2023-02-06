rm(list = ls())
#### Libraries ####
#devtools::install_github("bryandmartin/SUMMER",
#                        build_vignettes = F, force = T)
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

#### Parameters ####
country <- "Ghana"
cluster <- FALSE
message("If have the same subfolder structure as 
        AfricaAdmin2Estimates/Data/countryDataFolders/. Do nothing!\n
        Otherwise, edit the following paths as needed.\n")

# data.dir <- './toCluster'
# code.dir.rel <- '../../Analysis/R'
# igme.dir.rel <- '..'
# ihme.dir.rel <- '..'
# shapes.sub.dir <- '/shapeFiles_gadm'
# hiv.dir.rel <- '..'

data.dir <- '~/Dropbox/AfricaAdmin2Estimates/Data/countryDataFolders/'
code.dir.rel <- '../../Analysis/R'
igme.dir.rel <- '../../Analysis/R'
ihme.dir.rel <- '../../Analysis/R'
shapes.sub.dir <- '/shapeFiles_gadm'
hiv.dir.rel <- '../HIV/'

setwd(data.dir)

if(!exists("sheet_key", envir = .GlobalEnv)){
  source(paste0(code.dir.rel,'/LoadCommandCenter.R'))
}
CountryList <- sheets_read(sheet_key, sheet = "CountryList")
#CountryList <- read.csv("CountryList.csv", header = T)

folder.name <- CountryList$folderName[CountryList$Country == country]
gadm.abbrev <- CountryList$gadmCode[CountryList$Country == country]

message('Where is IHMEHand_CountryName.rda?\n')
hand.dir.rel <- paste0("../../Analysis/countryAnalysisFolders/",
                       gsub(" ", "", folder.name))
#hand.dir.rel <- paste0(gsub(" ", "", folder.name))


#### More Params ####

beg.year <- 1990
end.year <- 2020
# time.mod <- "rw2"
# time.mod <- "ar1"
# time.mod <- "rw2_rw1"
# time.mod <- "rw2main_randomSlopes_rw1xICAR"
time.mod <- "rw2main_randomSlopes_ar1xICAR"

#### Load polygon data ####
poly.file <- shapes.sub.dir
poly.layer.adm0 <- paste('gadm36', gadm.abbrev,
                         '0', sep = "_")
poly.layer.adm1 <- paste('gadm36', gadm.abbrev,
                         '1', sep = "_")
poly.layer.adm2 <- paste('gadm36', gadm.abbrev,
                         '2', sep = "_")

poly.path <- paste0(folder.name, poly.file)
poly.adm0 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm0))
poly.adm1 <- readOGR(dsn = poly.path,
                     layer = as.character(poly.layer.adm1))
if(sum(grepl(paste('gadm36', gadm.abbrev,
                   '2', sep = "_"), list.files(paste0(folder.name, poly.file)))) != 0){
  poly.adm2 <- readOGR(dsn = poly.path,
                       layer = as.character(poly.layer.adm2))
}

if(exists("poly.adm2")){
  proj4string(poly.adm0) <- proj4string(poly.adm1)  <- proj4string(poly.adm2)
}else{
  proj4string(poly.adm0) <- proj4string(poly.adm1)
}

load(paste0(folder.name, 
            shapes.sub.dir,
            '/', country, '_Amat.rda'))
load(paste0(folder.name, 
            shapes.sub.dir,
            '/', country, '_Amat_Names.rda'))


#### Use HIV Adjusted data? ####
#HIV.sheet <- gs_read(sheet_key, ws = "HIV")
HIV.sheet <- sheets_read(sheet_key, sheet = "HIV")
#HIV.sheet <- read.csv("HIV.csv", header = T)
HIV.country <- as.data.frame(HIV.sheet[HIV.sheet$Country == country,])
useHIVAdj <- (unique(HIV.country$`MM Adj by IGME`) == "Y" &
                unique(HIV.country$`UNAIDS data?`) == "Y")
#useHIVAdj <- (unique(HIV.country$MM.Adj.by.IGME) == "Y" &
#                unique(HIV.country$UNAIDS.data.) == "Y")

#### Get Survey years #### 

#SurveyInfo <- gs_read(sheet_key, ws = "SurveyInfo")
SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
#SurveyInfo <- read.csv("SurveyInfo.csv", header = T)
#surveys <- SurveyInfo$Survey.Year[SurveyInfo$Country == country &
#                                    SurveyInfo$`GPS.` == "Y"]

#SurveyInfo <- sheets_read(sheet_key, sheet = "SurveyInfo")
surveys <- SurveyInfo$`Survey Year`[SurveyInfo$Country == country &
                                      SurveyInfo$`GPS?` == "Y"]



#### Load model data ####
load(paste0(folder.name,'/',country,'_cluster_dat.rda'),
     envir = .GlobalEnv)

mod.dat$years <- as.numeric(as.character(mod.dat$years))
dat.years <- sort(unique(mod.dat$years))
beg.years <- seq(beg.year, end.year,5)
end.years <- beg.years + 4
periods <- paste(beg.years, end.years, sep = "-")
mod.dat$period <- as.character(cut(mod.dat$years, breaks = c(beg.years, beg.years[length(beg.years)]+5),
                                   include.lowest = T, right = F, labels = periods))

mod.dat$strata.orig <- mod.dat$strata
mod.dat$strata <- mod.dat$urban
mod.dat$country <- as.character(country)

#### Load IGME data ####

file.list <- list.files(igme.dir.rel)
igme.file <- file.list[grepl("Results.csv", file.list)]
igme.ests <- read.csv(paste0(igme.dir.rel,'/',igme.file),
                      header = T)
# igme.ests <- igme.ests[igme.ests$INDICATOR == "Under-five mortality rate" &
#                          igme.ests$SEX == "Total" &
#                          igme.ests$SERIES_YEAR == "2019" &
#                          igme.ests$SERIES_NAME == "UN IGME estimate 2019",]
# igme.ests$year <- igme.ests$REF_DATE - 0.5
# if(country == "Cote dIvoire"){
#   igme.ests <- igme.ests[igme.ests$REF_AREA == levels(igme.ests$REF_AREA)[45],]  
# }else{
#   igme.ests <- igme.ests[igme.ests$REF_AREA == country, ]
# }
# igme.ests <- igme.ests[order(igme.ests$year),]
# igme.ests <- igme.ests[igme.ests$year %in% beg.year:end.year,]


igme.ests <- igme.ests[igme.ests$Indicator== "Under-five Mortality Rate" &
                         igme.ests$Subgroup == "Total",]
names(igme.ests) <- gsub("X", "", names(igme.ests))

igme.ests <- igme.ests[igme.ests$Country.Name == country,]
a <- reshape(igme.ests,idvar = c("Country.Name", "Quantile"), 
             varying = list((1:dim(igme.ests)[2])[-c(1:5)]),
             v.names = "OBS_VALUE" ,direction = "long", 
             times = names(igme.ests)[-c(1:5)])
igme.ests <- reshape(a, idvar = c("time"),
                     v.names = "OBS_VALUE", 
                     timevar = "Quantile", direction = "wide")

names(igme.ests)[grepl(".Lower", names(igme.ests))] <- "LOWER_BOUND"
names(igme.ests)[grepl(".Upper", names(igme.ests))] <- "UPPER_BOUND"
names(igme.ests)[grepl(".Median", names(igme.ests))] <- "OBS_VALUE"
names(igme.ests)[grepl("time", names(igme.ests))] <- "REF_DATE"
igme.ests$year <- as.numeric(as.character(igme.ests$REF_DATE)) - 0.5
igme.ests <- igme.ests[order(igme.ests$year),]
igme.ests <- igme.ests[igme.ests$year %in% beg.year:end.year,]

#### Load IHME data ####
file.list <- list.files(ihme.dir.rel)
ihme.files <- file.list[grepl("IHME", file.list)]

ihme.ests <- list()
ihme.ests[['adm0']] <- read.csv( paste0(ihme.dir.rel, '/',
                                        ihme.files[grepl("ADM0_Y2019M10D16.CSV", ihme.files)]),
                                 header = T)
ihme.ests[['adm1']] <- read.csv( paste0(ihme.dir.rel, '/',
                                        ihme.files[grepl("ADM1_Y2019M10D16.CSV", ihme.files)]),
                                 header = T)

ihme.ests[['adm2']] <- read.csv( paste0(ihme.dir.rel,'/',
                                        ihme.files[grepl("ADM2_Y2019M10D16.CSV", ihme.files)]),
                                 header = T)

ihme.ests <- lapply(ihme.ests, function(x){
  if(!(country %in% x$ADM0_NAME)){
    message('\n Country name not found in one of the IHME files.\n')
  }
  if(country != "Cote dIvoire"){
    x[x$ADM0_NAME == country,]
  }else{
    x[x$ADM0_NAME %in% c(levels(ihme.ests[[1]]$ADM0_NAME)[20],
                         levels(ihme.ests[[2]]$ADM0_NAME)[6],
                         levels(ihme.ests[[3]]$ADM0_NAME)[6]),]
  }
})

if(country == "Malawi"){
  ihme.ests$adm1 <- ihme.ests$adm2
  ihme.ests$adm1$ADM1_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
  doAdmin2 <- FALSE
}

source(paste0(hand.dir.rel,
              '/IHMEHand_', country, '.R'))




#### National ####
load(paste0(folder.name,'/', country, '_rw2_natl.rda'))
load(paste0(folder.name,'/', country, '_res_rw2_natl.rda'))

if(useHIVAdj){
  load(paste0(folder.name, '/',
              country, '_directHIV_natl_yearly.rda'), envir = .GlobalEnv)
}else{
  load(paste0(folder.name, '/',
              country, '_direct_natl_yearly.rda'), envir = .GlobalEnv)
}

load(paste0(folder.name, '/',
            country, '_res_natl_yearly_SmoothedDirect.rda'))
res.natl.yearly[,c("median", "lower", "upper")] <-
  res.natl.yearly[,c("median", "lower", "upper")]*1000
direct.natl.yearly[,c("mean", "upper", "lower")] <-
  direct.natl.yearly[,c("mean", "upper", "lower")]*1000

hyperpar.table <- fit.natl$fit$summary.hyperpar
save(hyperpar.table,
     file = paste0(folder.name, '/', 
                   country, '_rw2',
                   '_natl_noStrata_hyperpar.rda'))

fixed.eff <- fit.natl$fit$summary.fixed
save(fixed.eff,
     file = paste0(folder.name, '/', 
                   country, '_rw2',
                   '_natl_noStrata_fixed.rda'))

#### Spaghetti Plot ####
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natl_noStrata_spaghetti.pdf'), 
    height = 9, width = 3)
{
  tmp.area <- res.natl$overall
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  tmp.ihme <- ihme.ests[[1]]
  tmp.ihme[,c("mean", "lower","upper")] <- 
    tmp.ihme[,c("mean", "lower","upper")]*1000
  cols <- rainbow(length(surveys)+1+1+1)
  cols[4] <- cols[3]
  cols[3] <- "orange"
  plot.years <- beg.year:end.year
  tmp.area$median <- tmp.area$median*1000
  tmp.area$upper <- tmp.area$upper*1000
  tmp.area$lower <- tmp.area$lower*1000
  
  
  par(mfrow = c(3,1),lend=1)
  if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
    plot.max <- max(direct.natl.yearly$mean+25, na.rm = T)
  }else{
    plot.max <- 0.25
  }
  
  if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
    plot(NA,
         xlab = "Year", ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, end.year + 1),
         type = 'l', col = cols[1], lwd = 2,
         main = country)
    legend('topright', bty = 'n', col = c(cols, 'black'),
           lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
    
  } else {
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      pane.years <- (tmp$years)
      svy.idx <- match(survey, surveys) 
      
      
      if(svy.idx== 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                main = surveys[svy.idx], lwd = 2)
          
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
          
          ihme.years <- (tmp.ihme$year)
          lines(ihme.years, tmp.ihme$mean, lwd = 2, col  = cols[length(surveys)+1])
          #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
          #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
          
          # for(year.id in 1:length(periods)){
          #   segments(pane.years[year.id], tmp$upper[year.id],
          #            pane.years[year.id], tmp$lower[year.id],
          #            col = cols[svy.idx])
          # }
        }else{
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main =  country)
        }
      }else{
        if(dim(tmp)[1] != 0){
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                lwd = 2)
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
          # for(year.id in 1:length(periods)){
          #   segments(pane.years[year.id], tmp$upper[year.id],
          #            pane.years[year.id], tmp$lower[year.id],
          #            col = cols[svy.idx])
          # }
        } 
      }
      
      
    }
    ihme.years <- (tmp.ihme$year)
    lines(ihme.years, tmp.ihme$mean,
          lty = 1, lwd = 2, col = cols[length(surveys) + 1])
    igme.years <- (igme.ests$year)
    lines(igme.years, igme.ests$OBS_VALUE,
          lty = 1, lwd = 2, col = cols[length(surveys)+2])
    # polygon(x = c(igme.years, rev(igme.years)),
    #         y = c(igme.ests$UPPER_BOUND/1000, rev(igme.ests$LOWER_BOUND/1000)),
    #         col = alpha(cols[length(surveys)+2], 0.15), border = F)
    # # lines(igme.years, igme.ests$UPPER_BOUND/1000,
    #       lty = 3, lwd = 1, col = 'gray35')
    # lines(igme.years, igme.ests$LOWER_BOUND/1000,
    #       lty = 3, lwd = 1, col = 'gray35')
    res.tmp <- tmp.area
    res.tmp$years <- as.numeric(as.character(res.tmp$years))
    
    pane.years <- (beg.year:end.year)
    lines(pane.years[1:length(beg.year:max(mod.dat$years))],
          res.natl.yearly$median[1:length(beg.year:max(mod.dat$years))], 
          col = cols[length(surveys)+3],
          lwd = 2, lty = 1)
    lines(pane.years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.natl.yearly$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = cols[length(surveys)+3], 
          lwd = 2, lty = 2)
    lines(res.tmp$years[1:length(beg.year:max(mod.dat$years))], 
          res.tmp$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
          lwd = 2, lty = 1)
    lines(res.tmp$years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.tmp$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = 'black', 
          lwd = 2, lty = 2)
    
    legend('topright', bty = 'n', col = c(cols, 'black'),
           lwd = 2, lty = c(rep(1, length(cols)+1)),
           legend = c(surveys, 'IHME', 'IGME', 'Smoothed Direct', 'Betabinomial'),
           cex = 0.6)
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      pane.years <- (tmp$years[!is.na(tmp$mean)])
      svy.idx <- match(survey, surveys) 
      
      if(svy.idx == 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          
          polygon(x = c(pane.years,
                        rev(pane.years)),
                  y = c(tmp$upper[!is.na(tmp$upper)],
                        rev(tmp$lower[!is.na(tmp$lower)])),
                  col = alpha(cols[svy.idx], 0.25),
                  border = FALSE)
          
          
        }else{
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main =  paste0(country))
          
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                lwd = 2)
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
        }
      }else{
        pane.years <- (tmp$years[!is.na(tmp$mean)])
        polygon(x = c(pane.years,
                      rev(pane.years)),
                y = c(tmp$upper[!is.na(tmp$upper)],
                      rev(tmp$lower[!is.na(tmp$lower)])),
                col = alpha(cols[svy.idx], 0.25),
                border = FALSE)
      }
      
    }  
    
    legend('topright', bty = 'n', fill = alpha(cols[1:length(surveys)], 0.25),
           border = cols[1:length(surveys)],
           legend = c(surveys),
           cex = 0.6)
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      
      if(svy.idx == 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          
        }
      }  
    }
    ihme.years <- (tmp.ihme$year)
    polygon(x = c(ihme.years, rev(ihme.years)),
            y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
            col = alpha(cols[length(surveys)+1], 0.25),
            border = FALSE)
    
    
    
    igme.years <- (igme.ests$year)
    polygon(x = c(igme.years, rev(igme.years)),
            y = c(igme.ests$UPPER_BOUND, rev(igme.ests$LOWER_BOUND)),
            col = alpha(cols[length(surveys)+2], 0.25),
            border = FALSE)
    res.tmp <- tmp.area
    res.tmp$years <- as.numeric(as.character(res.tmp$years))
    polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
            y = c(res.natl.yearly$upper[1:length(beg.year:end.year)],
                  rev(res.natl.yearly$lower[1:length(beg.year:end.year)])),
            col = alpha(cols[length(surveys)+3], 0.25),
            border = FALSE)
    polygon(x = c(res.tmp$years, rev(res.tmp$years)),
            y = c(res.tmp$upper,
                  rev(res.tmp$lower)),
            col = alpha('black', 0.25),
            border = FALSE)
    lines(res.tmp$years[1:length(beg.year:max(mod.dat$years))], 
          res.tmp$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
          lwd = 2, lty = 1)
    lines(res.tmp$years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.tmp$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = 'black', 
          lwd = 2, lty = 2)
    legend('topright', bty = 'n', fill = alpha(c(cols[-c(1:length(surveys))],
                                                 'black'), .25),
           border = c(cols[-c(1:length(surveys))],
                      'black'), cex = 0.65,
           legend = c('IHME', 'IGME',
                      'Smoothed Direct', 'Betabinomial'))
  }
  
  
}
dev.off() 


temporals <- getDiag(fit.natl, field = "time",
                     year_label = beg.year:end.year)
ages <- c("0", "1-11", "12-23",
          "24-35", "36-47", "48-59")

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natl_temporal.pdf'), height = 5, width = 7.5)
{
  par(mfrow =c(1,1),lend=1)
  age.cols <- rainbow(length(ages))
  age.cols[2] <- "orange"
  for(age in ages){
    age.idx <- grepl(paste0(age),temporals$group)
    fixed.idx <- grepl(paste0(age,"$"), row.names(fit.natl$fit$summary.fixed))
    fixed.eff <- fit.natl$fit$summary.fixed$`0.5quant`[fixed.idx]
    tmp<- temporals[age.idx,]
    
    if(match(age, ages) == 1 ){
      plot.min <- min(outer(temporals$median, fit.natl$fit$summary.fixed$`0.5quant`, FUN="+")) - 0.25
      plot.max <- max(outer(temporals$median, fit.natl$fit$summary.fixed$`0.5quant`, FUN="+")) + 0.25
      #lim <- max(abs(plot.min),abs(plot.max))
      
      plot(NA, xlim = c(beg.year, end.year),
           ylim = c(plot.min, plot.max),
           
           xlab = "Year",
           ylab = "Effect size",
           main = country)
    }
    
    lines(beg.year:end.year, tmp$median + fixed.eff,
          col = age.cols[match(age, ages)], lty = 1)
    
  }
  
  legend('topright', cex = 0.7, lty = 1,
         col = c(age.cols),
         legend = c(ages), bty = 'n')
}
dev.off()

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natl_coefs.pdf'), height = 5, width = 5)
{
  par(mfrow =c(1,1), lend=1)
  age.cols <- rainbow(length(ages))
  age.cols[2] <- "orange"
  for(age in ages){
    age.idx <- grepl(paste0(age),temporals$group)
    fixed.idx <- grepl(paste0(age,"$"), row.names(fit.natl$fit$summary.fixed))
    fixed.eff <- fit.natl$fit$summary.fixed[fixed.idx,]
    tmp<- temporals[age.idx,]
    
    if(match(age, ages) == 1 ){
      plot.min <- min(fit.natl$fit$summary.fixed$`0.025quant`) - 0.025
      plot.max <- max(fit.natl$fit$summary.fixed$`0.975quant`) + 0.025
      #lim <- max(abs(plot.min),abs(plot.max))
      
      plot(NA, xlim = c(0,7), xaxt = 'n',
           ylim = c(plot.min, plot.max),
           xlab = "Year",
           ylab = "Effect size",
           main = country)
    }
    
    points(match(age,ages), fixed.eff$`0.5quant`,
           pch = 19, col = age.cols[match(age, ages)])
    segments(match(age,ages), fixed.eff$`0.025quant`,
             match(age,ages), fixed.eff$`0.975quant`,
             col = age.cols[match(age, ages)], lty = 1)
    
  }
  
  legend('bottomleft', cex = 0.7, lty = 1,
         col = c(age.cols), pch =19,
         legend = c(ages), bty = 'n')
}
dev.off()


#### National Benchmark ####
load(paste0(folder.name, '/',
            country, '_res_natlBench_yearly_SmoothedDirect.rda'))
res.natl.yearly[,c("median", "lower", "upper")] <-
  res.natl.yearly[,c("median", "lower", "upper")]*1000
load(paste0(folder.name,'/', country, '_rw2_natlBench.rda'))
load(paste0(folder.name,'/', country, '_res_rw2_natlBench.rda'))
load(paste0(folder.name, '/', country, '_rw2',
            '_natlBenchmarks.rda'))

hyperpar.table <- fit.natl$fit$summary.hyperpar
save(hyperpar.table,
     file = paste0(folder.name, '/', 
                   country, '_rw2',
                   '_natlBench_noStrata_hyperpar.rda'))

fixed.eff <- fit.natl$fit$summary.fixed
save(fixed.eff,
     file = paste0(folder.name, '/', 
                   country, '_rw2',
                   '_natlBench_noStrata_fixed.rda'))

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natlBenchmarks.pdf'), height = 4, width = 4)
{ par(lend=1)
  plot(NA, xlim = c(beg.year, end.year),
       ylim = c(min(bench.adj$ratio)-0.025,
                max(bench.adj$ratio) + 0.025),
       xlab = "Year", ylab = "Offset",
       main = country)
  abline(h = 1)
  
  
  bench.tmp <- bench.adj$ratio
  bench.tmp[bench.tmp == 1] <- NA
  lines(beg.year:end.year,
        bench.tmp,
        lwd = 2, col = 'red')
  
}
dev.off()

#### Spaghetti Plot ####
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country,
           '_rw2_natlBench_noStrata_spaghetti.pdf'),
    height = 9, width = 3)
{
  tmp.area <- res.natl$overall
  tmp.area$width <- tmp.area$upper - tmp.area$lower
  tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
  tmp.area$cex2[tmp.area$cex2 > 6] <- 6
  tmp.ihme <- ihme.ests[[1]]
  tmp.ihme[,c("mean", "lower","upper")] <- 
    tmp.ihme[,c("mean", "lower","upper")]*1000
  cols <- rainbow(length(surveys)+1+1+1)
  cols[4] <- cols[3]
  cols[3] <- "orange"
  plot.years <- beg.year:end.year
  tmp.area$median <- tmp.area$median*1000
  tmp.area$upper <- tmp.area$upper*1000
  tmp.area$lower <- tmp.area$lower*1000
  
  
  par(mfrow = c(3,1),lend=1)
  if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
    plot.max <- max(direct.natl.yearly$mean+25, na.rm = T)
  }else{
    plot.max <- 0.25
  }
  
  if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
    plot(NA,
         xlab = "Year", ylab = "U5MR",
         ylim = c(0, plot.max),
         xlim = c(beg.year, end.year + 1),
         type = 'l', col = cols[1], lwd = 2,
         main = country)
    legend('topright', bty = 'n', col = c(cols, 'black'),
           lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
    
  } else {
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      pane.years <- (tmp$years)
      svy.idx <- match(survey, surveys) 
      
      
      if(svy.idx== 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                main = surveys[svy.idx], lwd = 2)
          
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
          
          ihme.years <- (tmp.ihme$year)
          lines(ihme.years, tmp.ihme$mean, lwd = 2, col  = cols[length(surveys)+1])
          #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
          #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
          
          # for(year.id in 1:length(periods)){
          #   segments(pane.years[year.id], tmp$upper[year.id],
          #            pane.years[year.id], tmp$lower[year.id],
          #            col = cols[svy.idx])
          # }
        }else{
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main =  country)
        }
      }else{
        if(dim(tmp)[1] != 0){
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                lwd = 2)
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
          # for(year.id in 1:length(periods)){
          #   segments(pane.years[year.id], tmp$upper[year.id],
          #            pane.years[year.id], tmp$lower[year.id],
          #            col = cols[svy.idx])
          # }
        } 
      }
      
      
    }
    ihme.years <- (tmp.ihme$year)
    lines(ihme.years, tmp.ihme$mean,
          lty = 1, lwd = 2, col = cols[length(surveys) + 1])
    igme.years <- (igme.ests$year)
    lines(igme.years, igme.ests$OBS_VALUE,
          lty = 1, lwd = 2, col = cols[length(surveys)+2])
    # polygon(x = c(igme.years, rev(igme.years)),
    #         y = c(igme.ests$UPPER_BOUND/1000, rev(igme.ests$LOWER_BOUND/1000)),
    #         col = alpha(cols[length(surveys)+2], 0.15), border = F)
    # # lines(igme.years, igme.ests$UPPER_BOUND/1000,
    #       lty = 3, lwd = 1, col = 'gray35')
    # lines(igme.years, igme.ests$LOWER_BOUND/1000,
    #       lty = 3, lwd = 1, col = 'gray35')
    res.tmp <- tmp.area
    res.tmp$years <- as.numeric(as.character(res.tmp$years))
    
    pane.years <- (beg.year:end.year)
    lines(pane.years[1:length(beg.year:max(mod.dat$years))],
          res.natl.yearly$median[1:length(beg.year:max(mod.dat$years))], 
          col = cols[length(surveys)+3],
          lwd = 2, lty = 1)
    lines(pane.years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.natl.yearly$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = cols[length(surveys)+3], 
          lwd = 2, lty = 2)
    lines(res.tmp$years[1:length(beg.year:max(mod.dat$years))], 
          res.tmp$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
          lwd = 2, lty = 1)
    lines(res.tmp$years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.tmp$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = 'black', 
          lwd = 2, lty = 2)
    
    legend('topright', bty = 'n', col = c(cols, 'black'),
           lwd = 2, lty = c(rep(1, length(cols)+1)),
           legend = c(surveys, 'IHME', 'IGME', 'Smoothed Direct', 'Betabinomial'),
           cex = 0.6)
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      pane.years <- (tmp$years[!is.na(tmp$mean)])
      svy.idx <- match(survey, surveys) 
      
      if(svy.idx == 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          
          polygon(x = c(pane.years,
                        rev(pane.years)),
                  y = c(tmp$upper[!is.na(tmp$upper)],
                        rev(tmp$lower[!is.na(tmp$lower)])),
                  col = alpha(cols[svy.idx], 0.25),
                  border = FALSE)
          
          
        }else{
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main =  paste0(country))
          
          lines(pane.years, tmp$mean, cex = tmp$cex2,
                type = 'l', col = cols[svy.idx],
                lwd = 2)
          points(pane.years, tmp$mean, pch = 19,
                 col = alpha(cols[svy.idx], 0.35),
                 cex = tmp$cex2)
        }
      }else{
        pane.years <- (tmp$years[!is.na(tmp$mean)])
        polygon(x = c(pane.years,
                      rev(pane.years)),
                y = c(tmp$upper[!is.na(tmp$upper)],
                      rev(tmp$lower[!is.na(tmp$lower)])),
                col = alpha(cols[svy.idx], 0.25),
                border = FALSE)
      }
      
    }  
    
    legend('topright', bty = 'n',
           fill = alpha(cols[1:length(surveys)], 0.25),
           border = cols[1:length(surveys)],
           legend = c(surveys),
           cex = 0.6)
    
    for(survey in surveys){
      tmp <- direct.natl.yearly[direct.natl.yearly$surveyYears == survey,]
      svy.idx <- match(survey, surveys) 
      
      if(svy.idx == 1){
        if(dim(tmp)[1] != 0){
          plot(NA,
               xlab = "Year", ylab = "U5MR",
               ylim = c(0, plot.max),
               xlim = c(beg.year, end.year),
               type = 'l', col = cols[svy.idx], lwd = 2,
               main = country)
          
        }
      }  
    }
    ihme.years <- (tmp.ihme$year)
    polygon(x = c(ihme.years, rev(ihme.years)),
            y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
            col = alpha(cols[length(surveys)+1], 0.25),
            border = FALSE)
    
    
    
    igme.years <- (igme.ests$year)
    polygon(x = c(igme.years, rev(igme.years)),
            y = c(igme.ests$UPPER_BOUND, rev(igme.ests$LOWER_BOUND)),
            col = alpha(cols[length(surveys)+2], 0.25),
            border = FALSE)
    res.tmp <- tmp.area
    res.tmp$years <- as.numeric(as.character(res.tmp$years))
    polygon(x = c(beg.year:end.year, rev(beg.year:end.year)),
            y = c(res.natl.yearly$upper[1:length(beg.year:end.year)],
                  rev(res.natl.yearly$lower[1:length(beg.year:end.year)])),
            col = alpha(cols[length(surveys)+3], 0.25),
            border = FALSE)
    polygon(x = c(res.tmp$years, rev(res.tmp$years)),
            y = c(res.tmp$upper,
                  rev(res.tmp$lower)),
            col = alpha('black', 0.25),
            border = FALSE)
    lines(res.tmp$years[1:length(beg.year:max(mod.dat$years))], 
          res.tmp$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
          lwd = 2, lty = 1)
    lines(res.tmp$years[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          res.tmp$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
          col = 'black', 
          lwd = 2, lty = 2)
    legend('topright', bty = 'n', fill = alpha(c(cols[-c(1:length(surveys))],
                                                 'black'), .25),
           border = c(cols[-c(1:length(surveys))],
                      'black'), cex = 0.65,
           legend = c('IHME', 'IGME',
                      'Smoothed Direct', 'Betabinomial'))
  }
  
  
}
dev.off()



temporals <- getDiag(fit.natl, field = "time",
                     year_label = beg.year:end.year)
ages <- c("0", "1-11", "12-23",
          "24-35", "36-47", "48-59")

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natlBench_temporal.pdf'), height = 5, width = 7.5)
{
  par(mfrow =c(1,1),lend=1)
  age.cols <- rainbow(length(ages))
  age.cols[2] <- "orange"
  for(age in ages){
    age.idx <- grepl(paste0(age),temporals$group)
    fixed.idx <- grepl(paste0(age,"$"), row.names(fit.natl$fit$summary.fixed))
    fixed.eff <- fit.natl$fit$summary.fixed$`0.5quant`[fixed.idx]
    tmp<- temporals[age.idx,]
    
    if(match(age, ages) == 1 ){
      plot.min <- min(outer(temporals$median, fit.natl$fit$summary.fixed$`0.5quant`, FUN="+")) - 0.25
      plot.max <- max(outer(temporals$median, fit.natl$fit$summary.fixed$`0.5quant`, FUN="+")) + 0.25
      #lim <- max(abs(plot.min),abs(plot.max))
      
      plot(NA, xlim = c(beg.year, end.year),
           ylim = c(plot.min, plot.max),
           
           xlab = "Year",
           ylab = "Effect size",
           main = country)
    }
    
    lines(beg.year:end.year, tmp$median + fixed.eff,
          col = age.cols[match(age, ages)], lty = 1)
    
  }
  
  legend('topright', cex = 0.7, lty = 1,
         col = c(age.cols),
         legend = c(ages), bty = 'n')
}
dev.off()


pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_rw2_natlBench_coefs.pdf'), height = 5, width = 5)
{
  par(mfrow =c(1,1),lend=1)
  age.cols <- rainbow(length(ages))
  age.cols[2] <- "orange"
  for(age in ages){
    age.idx <- grepl(paste0(age),temporals$group)
    fixed.idx <- grepl(paste0(age,"$"), row.names(fit.natl$fit$summary.fixed))
    fixed.eff <- fit.natl$fit$summary.fixed[fixed.idx,]
    tmp<- temporals[age.idx,]
    
    if(match(age, ages) == 1 ){
      plot.min <- min(fit.natl$fit$summary.fixed$`0.025quant`) - 0.025
      plot.max <- max(fit.natl$fit$summary.fixed$`0.975quant`) + 0.025
      #lim <- max(abs(plot.min),abs(plot.max))
      
      plot(NA, xlim = c(0,7), xaxt = 'n',
           ylim = c(plot.min, plot.max),
           xlab = "Year",
           ylab = "Effect size",
           main = country)
    }
    
    points(match(age,ages), fixed.eff$`0.5quant`,
           pch = 19, col = age.cols[match(age, ages)])
    segments(match(age,ages), fixed.eff$`0.025quant`,
             match(age,ages), fixed.eff$`0.975quant`,
             col = age.cols[match(age, ages)], lty = 1)
    
  }
  
  legend('bottomleft', cex = 0.7, lty = 1,
         col = c(age.cols), pch =19,
         legend = c(ages), bty = 'n')
}
dev.off()


#### Admin 1 ####
  
  if(useHIVAdj){
    load(paste0(folder.name, '/',
                country, '_directHIV_admin1.rda'), envir = .GlobalEnv)
  }else{
    load(paste0(folder.name, '/',
                country, '_direct_admin1.rda'), envir = .GlobalEnv)
  }
  direct.admin1[,c("mean", "lower", "upper")] <- 
    direct.admin1[,c("mean", "lower", "upper")]*1000
  load(paste0(folder.name, '/',
              country, '_res_admin1_SmoothedDirect.rda'))
  res.smoothdir <- res.admin1
  
  # load(paste0(folder.name,'/', country, '_', time.mod, '_admin1Bench.rda'))
  load(paste0(folder.name,'/', country,
              '_res_', time.mod, '_admin1.rda'))
  load(paste0(folder.name,'/', country, '_', 
              time.mod, '_admin1Benchmarks.rda'))
  load(paste0(folder.name, '/', country, 
              '_', time.mod,'_admin1_noStrata_temporals.rda'))
  load(paste0(folder.name, '/',
              country, '_', time.mod,'_admin1_noStrata_spatials.rda'))
  load(paste0(folder.name, '/',
              country, 
              '_', time.mod,'_admin1_noStrata_fixedeff.rda'))
  load(paste0(folder.name, '/',
              country,'_',
              time.mod,'_admin1_noStrata_PosteriorInteractions.rda'))
  
  if (grepl("randomSlopes",time.mod)) {
    load(paste0(folder.name, '/',
                country,'_',
                time.mod,'_admin1_noStrata_posteriorRandomSlopes.rda'))
  }
  
#### Spaghetti Plot ####
  
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod, '_admin1Benchmarks.pdf'), height = 4, width = 4)
  par(lend=1)
  plot(NA, xlim = c(beg.year,end.year),
       ylim = c(0.75,1.25), xlab= "Year",
       ylab = "Offset", main = country)
  abline(h = 1,col = 'gray45')
  max.year <- max(mod.dat$years)
  lines(beg.year:max.year, 
        bench.adj$ratio[1:length(beg.year:max.year)], col = 'red', lwd = 2)
  dev.off()
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod, '_admin1_noStrata_spaghetti_6per.pdf'),
      height = 9, width = 6)
  {
    par(mfrow = c(3,2),lend=1)
    area.idx <- 0
    for(area in admin1.names$Internal){
      area.idx <- area.idx + 1
      tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
      tmp.area$width <- tmp.area$upper - tmp.area$lower
      tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
      tmp.area$cex2[tmp.area$cex2 > 6] <- 6
      tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == as.character(admin1.names$GADM[area.idx]),]
      tmp.ihme[ ,c("mean", "lower", "upper")] <- 
        tmp.ihme[ ,c("mean", "lower", "upper")]*1000
      # print(tmp.ihme)
      cols <- rainbow(length(surveys)+1+1)
      plot.years <- beg.year:end.year
      tmp.area[,c("median", "lower","upper")] <-
        tmp.area[,c("median", "lower","upper")]*1000
      res.tmp <- res.smoothdir[res.smoothdir$region == 
                                 as.character(admin1.names$Internal[area.idx]),]
      res.tmp[,c("median", "lower", "upper")] <-
        res.tmp[,c("median", "lower", "upper")]*1000
      
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      
      
      
      if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
        plot.max <- max(c(tmp.area$upper,
                          direct.admin1$upper[direct.admin1$region == as.character(area)]),
                        na.rm = T) + 25
      }
      
      if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
        plot(NA,
             xlab = "Year", ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year + 1),
             type = 'l', col = cols[1], lwd = 2,
             main = admin1.names$GADM[area.idx])
        legend('topright', bty = 'n', col = c(cols, 'black'),
               lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
        
      } else {
        
        for(survey in surveys){
          tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                                 direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
          svy.idx <- match(survey, surveys) 
          
          
          if(svy.idx== 1){
            if(dim(tmp)[1] != 0){
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main = admin1.names$GADM[area.idx])
              pane.years <- (seq(beg.year+2, end.year-2,5))
              lines(pane.years, tmp$mean, cex = tmp$cex2,
                    type = 'l', col = cols[svy.idx],
                    main = surveys[svy.idx], lwd = 2)
              
              points(pane.years, tmp$mean, pch = 19,
                     col = alpha(cols[svy.idx], 0.35),
                     cex = tmp$cex2)
              
              ihme.years <- (tmp.ihme$year)
              lines(ihme.years, tmp.ihme$mean, lwd = 2, col  = cols[length(surveys)+1])
              #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
              #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
              
              # for(year.id in 1:length(periods)){
              #   segments(pane.years[year.id], tmp$upper[year.id],
              #            pane.years[year.id], tmp$lower[year.id],
              #            col = cols[svy.idx])
              # }
            }else{
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main =  paste0(admin1.names$GADM[area.idx]))
            }
          }else{
            if(dim(tmp)[1] != 0){
              lines(pane.years, tmp$mean, cex = tmp$cex2,
                    type = 'l', col = cols[svy.idx],
                    lwd = 2)
              points(pane.years, tmp$mean, pch = 19,
                     col = alpha(cols[svy.idx], 0.35),
                     cex = tmp$cex2)
              # for(year.id in 1:length(periods)){
              #   segments(pane.years[year.id], tmp$upper[year.id],
              #            pane.years[year.id], tmp$lower[year.id],
              #            col = cols[svy.idx])
              # }
            } 
          }
          
          
        }
        
        pane.years <- (seq(beg.year+2, end.year-2, 5))
        est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
        lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
              tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
              lwd = 2, lty = 1)
        lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              col = 'black', 
              lwd = 2, lty = 2)
        
        lines(pane.years[est.ids],
              res.tmp$median[est.ids], 
              col = cols[length(surveys)+2],
              lwd = 2, lty = 1)
        lines(pane.years[max(est.ids):length(pane.years)], 
              res.tmp$median[max(est.ids):length(pane.years)], 
              col = cols[length(surveys)+2], 
              lwd = 2, lty = 2)
        legend('topright', bty = 'n', col = c(cols, 'black'),
               lwd = 2, lty = c(rep(1, length(cols)+1)),
               legend = c(surveys, 'IHME', 'Smoothed Direct', 'Betabinomial'),
               cex = 0.6)
        
        
        for(survey in surveys){
          tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                                 direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
          svy.idx <- match(survey, surveys) 
          
          if(svy.idx == 1){
            if(dim(tmp)[1] != 0){
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main = admin1.names$GADM[area.idx])
              pane.years <- seq(beg.year+2, end.year-3, 5)[which(!is.na(tmp$mean))]
              polygon(x = c(pane.years,
                            rev(pane.years)),
                      y = c(tmp$upper[!is.na(tmp$upper)],
                            rev(tmp$lower[!is.na(tmp$lower)])),
                      col = alpha(cols[svy.idx], 0.25),
                      border = FALSE)
              
              
            }else{
              
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main =  paste0(admin1.names$GADM[area.idx]))
            }
            
          }else{
            
            pane.years <- seq(beg.year+2, end.year-3, 5)[which(!is.na(tmp$mean))]
            polygon(x = c(pane.years,
                          rev(pane.years)),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha(cols[svy.idx], 0.25),
                    border = FALSE)
          }
          
        }  
        
        legend('topright', bty = 'n',
               fill = alpha(cols[1:length(surveys)], 0.25),
               border = cols[1:length(surveys)],
               legend = c(surveys),
               cex = 0.6)
        
        
        for(survey in surveys){
          # debugging
          # survey <- surveys[1]
          # survey <- surveys[2]
          tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                                 direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
          svy.idx <- match(survey, surveys) 
          
          if(svy.idx== 1){
            if(dim(tmp)[1] != 0){
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main = admin1.names$GADM[area.idx])
            }else{
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main =  paste0(admin1.names$GADM[area.idx]))
            }
          }
          
          
        }
        
        ihme.years <- tmp.ihme$year
        polygon(x = c(ihme.years, rev(ihme.years)),
                y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
                col = alpha(cols[length(surveys)+1], 0.25),
                border = FALSE)
        pane.years <- seq(beg.year+2.5,end.year-2.5,5)
        polygon(x = c(pane.years, rev(pane.years)),
                y = c(res.tmp$upper[1:length(pane.years)],
                      rev(res.tmp$lower[1:length(pane.years)])),
                col = alpha(cols[length(surveys)+2], 0.25),
                border = FALSE)
        
        polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
                y = c(tmp.area$upper,
                      rev(tmp.area$lower)),
                col = alpha('black', 0.25), 
                border = FALSE)
        
        est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
        lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
              tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
              lwd = 2, lty = 1)
        lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              col = 'black', 
              lwd = 2, lty = 2)
        
        #      }
        legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                     'black'), .25),
               border = c(cols[(length(surveys)+1):length(cols)],
                          'black'),
               legend = c('IHME', 'Smoothed Direct', 'Betabinomial'), cex = 0.75)
      }
      
      
    }
  }
  dev.off()
  
  ages <- c("0", "1-11", "12-23",
            "24-35", "36-47", "48-59")
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod, '_admin1_temporal.pdf'), height = 5, width = 7.5)
  {
    par(mfrow =c(1,1),lend=1)
    age.cols <- rainbow(length(ages))
    age.cols[2] <- "orange"
    for(age in ages){
      age.idx <- grepl(paste0(age),temporals$group)
      fixed.idx <- grepl(paste0(age,"$"), row.names(fixed.eff.table))
      fixed.eff <- fixed.eff.table$`0.5quant`[fixed.idx]
      tmp<- temporals[age.idx,]
      
      if(match(age, ages) == 1 ){
        plot.min <- min(temporals$median +
                          min(fixed.eff.table$`0.5quant`)) -0.25
        plot.max <- max(temporals$median +
                          max(fixed.eff.table$`0.5quant`)) +0.25
        #lim <- max(abs(plot.min),abs(plot.max))
        
        plot(NA, xlim = c(beg.year, end.year),
             ylim = c(plot.min, plot.max),
             
             xlab = "Year",
             ylab = "Monthly hazard",
             main = country)
        #  axis(2, at = seq(plot.min, plot.max, 1),
        #       labels = round(expit(seq(plot.min, plot.max, 1))*1000, digits = 1))
      }
      
      lines(beg.year:end.year, tmp$median + fixed.eff,
            col = age.cols[match(age, ages)], lty = 2)
      
    }
    
    legend('bottomleft',
           cex = 0.7, lty = 2,
           col = c(age.cols),
           legend = c(ages), bty = 'n')
  }
  dev.off()
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod, '_admin1_coefs.pdf'), height = 5, width = 5)
  {
    par(mfrow =c(1,1),lend=1)
    age.cols <- rainbow(length(ages))
    age.cols[2] <- "orange"
    for(age in ages){
      age.idx <- grepl(paste0(age),temporals$group)
      fixed.idx <- grepl(paste0(age,"$"), row.names(fixed.eff.table))
      fixed.eff <- fixed.eff.table[fixed.idx,]
      tmp<- temporals[age.idx,]
      
      if(match(age, ages) == 1 ){
        plot.min <- min(fixed.eff.table$`0.025quant`) - 0.025
        plot.max <- max(fixed.eff.table$`0.975quant`) + 0.025
        #lim <- max(abs(plot.min),abs(plot.max))
        
        plot(NA, xlim = c(0,7), xaxt = 'n',
             ylim = c(plot.min, plot.max),
             xlab = "Year",
             ylab = "Effect size",
             main = country)
      }
      
      points(match(age,ages), fixed.eff$`0.5quant`,
             pch = 19, col = age.cols[match(age, ages)])
      segments(match(age,ages), fixed.eff$`0.025quant`,
               match(age,ages), fixed.eff$`0.975quant`,
               col = age.cols[match(age, ages)], lty = 1)
      
    }
    
    legend('bottomleft', cex = 0.7, lty = 1,
           col = c(age.cols), pch =19,
           legend = c(ages), bty = 'n')
  }
  dev.off()
  
  if (grepl("randomSlopes",time.mod)) {
    pdf(paste0(folder.name, '/Plots/Betabinomial/',
               country, '_', time.mod, '_admin1_randomSlopes.pdf'), height = 5, width = 5)
    years.std <- ((beg.year:end.year)-mean(beg.year:end.year))/sd(beg.year:end.year)
    # plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.025quant","0.975quant")],
    #                       years.std[length(years.std)]*posteriorRandomSlopes[,c("0.025quant","0.975quant")]))
    plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.5quant")],
                          years.std[length(years.std)]*posteriorRandomSlopes[,c("0.5quant")]))
    par(lend=1)
    plot(NA,xlim=range(years.std),ylim = plot.range,
         ylab = "effect",xlab="year (standardized)")
    
    region.cols <- viridis_pal(option = "A")(nrow(posteriorRandomSlopes))
    for (i in 1:nrow(posteriorRandomSlopes)) {
      abline(0,posteriorRandomSlopes$`0.5quant`[i],col=region.cols[i])
      # polygon(x=c(years.std,rev(years.std)),
      #         y=c(years.std*posteriorRandomSlopes$`0.975quant`[i],rev(years.std*posteriorRandomSlopes$`0.025quant`[i])),
      #         col=alpha(region.cols[i],0.25),border = NA)
    }
    legend("topleft",legend=admin1.names[,1],col=region.cols,lty=1,ncol=2,cex=0.5)
    dev.off()
  }
  
#### Polygon Plots  ####
  centroids <- gCentroid(poly.adm1, byid = TRUE,
                         id = poly.adm1@data$NAME_1)
  pdf(paste0(folder.name, '/Plots/ShapeCheck/',
             country,'_Admin1Names.pdf'))
  par(lend=1)
  plot(poly.adm1,
       xlim = poly.adm1@bbox['x',],
       ylim = poly.adm1@bbox['y',],
       axes = F)
  text(centroids$x, centroids$y,
       labels = row.names(centroids),cex = 0.45)
  dev.off()
  
  
# Admin 1 benchmark ####
  
  load(paste0(folder.name, '/',
              country, '_res_admin1Bench_SmoothedDirect.rda'))
  res.smoothdir <- res.admin1
  
  # reloading these in case you're just running the benchmark section of the code
  if(useHIVAdj){
    load(paste0(folder.name, '/',
                country, '_directHIV_admin1.rda'), envir = .GlobalEnv)
  }else{
    load(paste0(folder.name, '/',
                country, '_direct_admin1.rda'), envir = .GlobalEnv)
  }
  
  direct.admin1[,c("mean", "lower", "upper")] <- 
    direct.admin1[,c("mean", "lower", "upper")]*1000
  # load(paste0(folder.name,'/', country, '_', time.mod, '_admin1Bench.rda'))
  load(paste0(folder.name,'/', country,
              '_res_', time.mod, '_admin1Bench.rda'))
  load(paste0(folder.name,'/', country, '_', 
              time.mod, '_admin1Benchmarks.rda'))
  load(paste0(folder.name, '/', country, 
              '_', time.mod,'_admin1Bench_noStrata_temporals.rda'))
  load(paste0(folder.name, '/',
              country, '_', time.mod,'_admin1Bench_noStrata_spatials.rda'))
  load(paste0(folder.name, '/',
              country, 
              '_', time.mod,'_admin1Bench_noStrata_fixedeff.rda'))
  
  if (grepl("randomSlopes",time.mod)) {
    load(paste0(folder.name, '/',
                country,'_',
                time.mod,'_admin1Bench_noStrata_posteriorRandomSlopes.rda'))
  }
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_',
             time.mod, '_admin1Benchmarks.pdf'),
      height = 4, width = 4)
  {
    par(lend=1)
    plot(NA, xlim = c(beg.year, end.year),
         ylim = c(min(bench.adj$ratio)-0.025,
                  max(bench.adj$ratio) + 0.025),
         xlab = "Year", ylab = "Offset",
         main = country)
    abline(h = 1)
    
    
    bench.tmp <- bench.adj$ratio
    bench.tmp[bench.tmp == 1] <- NA
    lines(beg.year:end.year,
          bench.tmp,
          lwd = 2, col = 'red')
    
  }
  dev.off()
  
## Spaghetti Plot ####
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', 
             time.mod,
             '_admin1Bench_noStrata_spaghetti.pdf'),
      height = 9, width = 6)
  {
    par(mfrow = c(3,2),lend=1)
    area.idx <- 0
    for(area in admin1.names$Internal){
      area.idx <- area.idx + 1
      tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
      tmp.area$width <- tmp.area$upper - tmp.area$lower
      tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
      tmp.area$cex2[tmp.area$cex2 > 6] <- 6
      tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == as.character(admin1.names$GADM[area.idx]),]
      tmp.ihme[ ,c("mean", "lower", "upper")] <- 
        tmp.ihme[ ,c("mean", "lower", "upper")]*1000
      # print(tmp.ihme)
      cols <- rainbow(length(surveys)+1+1)
      plot.years <- beg.year:end.year
      tmp.area[,c("median", "lower","upper")] <-
        tmp.area[,c("median", "lower","upper")]*1000
      res.tmp <- res.smoothdir[res.smoothdir$region == 
                                 as.character(admin1.names$Internal[area.idx]),]
      res.tmp[,c("median", "lower", "upper")] <-
        res.tmp[,c("median", "lower", "upper")]*1000
      
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      
      
      
      if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
        plot.max <- max(c(tmp.area$upper,
                          direct.admin1$upper[direct.admin1$region == as.character(area)]),
                        na.rm = T) + 25
      }
      
      if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
        plot(NA,
             xlab = "Year", ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year + 1),
             type = 'l', col = cols[1], lwd = 2,
             main = admin1.names$GADM[area.idx])
        legend('topright', bty = 'n', col = c(cols, 'black'),
               lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
        
      } else {
        
        for(survey in surveys){
          tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                                 direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
          svy.idx <- match(survey, surveys) 
          
          
          if(svy.idx== 1){
            if(dim(tmp)[1] != 0){
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main = admin1.names$GADM[area.idx])
              pane.years <- (seq(beg.year+2, end.year-2,5))
              lines(pane.years, tmp$mean, cex = tmp$cex2,
                    type = 'l', col = cols[svy.idx],
                    main = surveys[svy.idx], lwd = 1)
              
              points(pane.years, tmp$mean, pch = 19,
                     col = alpha(cols[svy.idx], 0.35),
                     cex = tmp$cex2)
              
              ihme.years <- (tmp.ihme$year)
              lines(ihme.years, tmp.ihme$mean, 
                    lwd = 2, col  = cols[length(surveys)+1])
              #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
              #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
              
              # for(year.id in 1:length(periods)){
              #   segments(pane.years[year.id], tmp$upper[year.id],
              #            pane.years[year.id], tmp$lower[year.id],
              #            col = cols[svy.idx])
              # }
            }else{
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main =  paste0(admin1.names$GADM[area.idx]))
            }
          }else{
            if(dim(tmp)[1] != 0){
              lines(pane.years, tmp$mean, cex = tmp$cex2,
                    type = 'l', col = cols[svy.idx],
                    lwd = 1)
              points(pane.years, tmp$mean, pch = 19,
                     col = alpha(cols[svy.idx], 0.35),
                     cex = tmp$cex2)
              # for(year.id in 1:length(periods)){
              #   segments(pane.years[year.id], tmp$upper[year.id],
              #            pane.years[year.id], tmp$lower[year.id],
              #            col = cols[svy.idx])
              # }
            } 
          }
          
          
        }
        
        pane.years <- (seq(beg.year+2, end.year-2, 5))
        est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
        lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
              tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
              lwd = 2, lty = 1)
        lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              col = 'black', 
              lwd = 2, lty = 2)
        
        lines(pane.years[est.ids],
              res.tmp$median[est.ids], 
              col = cols[length(surveys)+2],
              lwd = 2, lty = 1)
        lines(pane.years[max(est.ids):length(pane.years)], 
              res.tmp$median[max(est.ids):length(pane.years)], 
              col = cols[length(surveys)+2], 
              lwd = 2, lty = 2)
        legend('topright', bty = 'n', col = c(cols, 'black'),
               lwd = c(rep(1, length(surveys)),
                       2, 2, 2),
               lty = c(rep(1, length(cols)+1)),
               legend = c(surveys, 'IHME',
                          'Smoothed Direct', 'Betabinomial'),
               cex = 0.6)
        
        
        for(survey in surveys){
          tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                                 direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
          svy.idx <- match(survey, surveys) 
          
          if(svy.idx == 1){
            if(dim(tmp)[1] != 0){
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main = admin1.names$GADM[area.idx])
              pane.years <- seq(beg.year+2, end.year-3, 5)[which(!is.na(tmp$mean))]
              polygon(x = c(pane.years,
                            rev(pane.years)),
                      y = c(tmp$upper[!is.na(tmp$upper)],
                            rev(tmp$lower[!is.na(tmp$lower)])),
                      col = alpha(cols[svy.idx], 0.25),
                      border = FALSE)
              
              
            }else{
              
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main =  paste0(admin1.names$GADM[area.idx]))
            }
            
          }else{
            
            pane.years <- seq(beg.year+2, end.year-3, 5)[which(!is.na(tmp$mean))]
            polygon(x = c(pane.years,
                          rev(pane.years)),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha(cols[svy.idx], 0.25),
                    border = FALSE)
          }
          
        }  
        
        legend('topright', bty = 'n',
               fill = alpha(cols[1:length(surveys)], 0.25),
               border = cols[1:length(surveys)],
               legend = c(surveys),
               cex = 0.6)
        
        
        for(survey in surveys){
          # debugging
          # survey <- surveys[1]
          # survey <- surveys[2]
          tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                                 direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
          svy.idx <- match(survey, surveys) 
          
          if(svy.idx== 1){
            if(dim(tmp)[1] != 0){
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main = admin1.names$GADM[area.idx])
            }else{
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main =  paste0(admin1.names$GADM[area.idx]))
            }
          }
          
          
        }
        
        ihme.years <- tmp.ihme$year
        polygon(x = c(ihme.years, rev(ihme.years)),
                y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
                col = alpha(cols[length(surveys)+1], 0.25),
                border = FALSE)
        pane.years <- seq(beg.year+2.5,end.year-2.5,5)
        polygon(x = c(pane.years, rev(pane.years)),
                y = c(res.tmp$upper[1:length(pane.years)],
                      rev(res.tmp$lower[1:length(pane.years)])),
                col = alpha(cols[length(surveys)+2], 0.25),
                border = FALSE)
        
        polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
                y = c(tmp.area$upper,
                      rev(tmp.area$lower)),
                col = alpha('black', 0.25), border = FALSE)
        #      }
        est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
        lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
              tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
              lwd = 2, lty = 1)
        lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              col = 'black', 
              lwd = 2, lty = 2)
        legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                     'black'), .25),
               border = c(cols[(length(surveys)+1):length(cols)],
                          'black'),
               legend = c('IHME', 'Smoothed Direct', 'Betabinomial'), cex = 0.75)
      }
      
      
    }
  }
  dev.off()
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod,
             '_admin1Bench_noStrata_spaghettiSingle.pdf'),
      height = 5, width = 5)
  {
    area.idx <- 0
    for(area in admin1.names$Internal){
      area.idx <- area.idx + 1
      tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
      tmp.area$width <- tmp.area$upper - tmp.area$lower
      tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
      tmp.area$cex2[tmp.area$cex2 > 6] <- 6
      tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == as.character(admin1.names$GADM[area.idx]),]
      tmp.ihme[ ,c("mean", "lower", "upper")] <- 
        tmp.ihme[ ,c("mean", "lower", "upper")]*1000
      # print(tmp.ihme)
      cols <- rainbow(length(surveys)+1+1)
      plot.years <- beg.year:end.year
      tmp.area[,c("median", "lower","upper")] <-
        tmp.area[,c("median", "lower","upper")]*1000
      res.tmp <- res.smoothdir[res.smoothdir$region == 
                                 as.character(admin1.names$Internal[area.idx]),]
      res.tmp[,c("median", "lower", "upper")] <-
        res.tmp[,c("median", "lower", "upper")]*1000
      
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      
      
      par(mfrow = c(1,1),lend=1)
      if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
        plot.max <- max(c(tmp.area$upper,
                          direct.admin1$upper[direct.admin1$region == as.character(area)]),
                        na.rm = T) + 25
      }
      
      if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
        plot(NA,
             xlab = "Year", ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year + 1),
             type = 'l', col = cols[1], lwd = 2,
             main = admin1.names$GADM[area.idx])
        legend('topright', bty = 'n', col = c(cols, 'black'),
               lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
        
      } else {
        
        for(survey in surveys){
          tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                                 direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
          svy.idx <- match(survey, surveys) 
          
          
          if(svy.idx== 1){
            if(dim(tmp)[1] != 0){
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main = admin1.names$GADM[area.idx])
              pane.years <- (seq(beg.year+2, end.year-2,5))
              lines(pane.years, tmp$mean, cex = tmp$cex2,
                    type = 'l', col = cols[svy.idx],
                    main = surveys[svy.idx], lwd = 2)
              
              polygon(x = c(pane.years[!is.na(tmp$upper)],
                            rev(pane.years[!is.na(tmp$upper)])),
                      y = c(tmp$upper[!is.na(tmp$upper)],
                            rev(tmp$lower[!is.na(tmp$lower)])),
                      col = alpha('grey85', 0.25), 
                      border = FALSE)
              
              points(pane.years, tmp$mean, pch = 19,
                     col = alpha(cols[svy.idx], 0.35),
                     cex = tmp$cex2)
              
              #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
              #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
              
              # for(year.id in 1:length(periods)){
              #   segments(pane.years[year.id], tmp$upper[year.id],
              #            pane.years[year.id], tmp$lower[year.id],
              #            col = cols[svy.idx])
              # }
            }else{
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main =  paste0(admin1.names$GADM[area.idx]))
            }
          }else{
            if(dim(tmp)[1] != 0){
              lines(pane.years, tmp$mean, cex = tmp$cex2,
                    type = 'l', col = cols[svy.idx],
                    lwd = 2)
              polygon(x = c(pane.years[!is.na(tmp$upper)],
                            rev(pane.years[!is.na(tmp$upper)])),
                      y = c(tmp$upper[!is.na(tmp$upper)],
                            rev(tmp$lower[!is.na(tmp$lower)])),
                      col = alpha('grey85', 0.25), 
                      border = FALSE)
              
              points(pane.years, tmp$mean, pch = 19,
                     col = alpha(cols[svy.idx], 0.35),
                     cex = tmp$cex2)
            } 
          }
          
          
        }
        
        
        lines(tmp.ihme$year, 
              tmp.ihme$mean, lwd = 2,
              lty = 1, 
              col  = cols[length(surveys)+1])
        polygon(c(tmp.ihme$year,
                  rev(tmp.ihme$year)),
                c(tmp.ihme$upper,
                  rev(tmp.ihme$lower)),
                col = alpha(cols[length(surveys)+1], 0.25),
                border = FALSE)
        pane.years <- (seq(beg.year+2, end.year-2, 5))
        est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
        
        
        lines(pane.years[est.ids],
              res.tmp$median[est.ids], 
              col = cols[length(surveys)+2],
              lwd = 2, lty = 1)
        lines(pane.years[max(est.ids):length(pane.years)], 
              res.tmp$median[max(est.ids):length(pane.years)], 
              col = cols[length(surveys)+2], 
              lwd = 2, lty = 2)
        polygon(x = c(pane.years,
                      rev(pane.years)),
                y = c(res.tmp$upper[1:6],
                      rev(res.tmp$lower[1:6])),
                col = alpha(cols[length(surveys)+2], 0.25),
                border = FALSE)
        lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
              tmp.area$median[1:length(beg.year:max(mod.dat$years))],
              col = 'black',
              lwd = 2, lty = 1)
        lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              col = 'black', 
              lwd = 2, lty = 2)
        polygon(x = c(beg.year:end.year,
                      rev(beg.year:end.year)),
                y = c(tmp.area$upper,
                      rev(tmp.area$lower)),
                col = alpha('black', 0.25),
                border = FALSE)
        legend('topright', bty = 'n', col = c(cols, 'black'),
               lwd = 2, lty = c(rep(1, length(surveys)+2)),
               legend = c(surveys, 'IHME',
                          'Smoothed Direct', 'Betabinomial'),
               cex = 0.6)
        
        
      }
      
      
    }
  }
  dev.off()
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod,
             '_admin1Bench_noStrata_spaghettiSingle_noIHME.pdf'),
      height = 5, width = 5)
  {
    area.idx <- 0
    for(area in admin1.names$Internal){
      area.idx <- area.idx + 1
      tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
      tmp.area$width <- tmp.area$upper - tmp.area$lower
      tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
      tmp.area$cex2[tmp.area$cex2 > 6] <- 6
      tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == as.character(admin1.names$GADM[area.idx]),]
      tmp.ihme[ ,c("mean", "lower", "upper")] <- 
        tmp.ihme[ ,c("mean", "lower", "upper")]*1000
      # print(tmp.ihme)
      cols <- rainbow(length(surveys)+1+1)
      plot.years <- beg.year:end.year
      tmp.area[,c("median", "lower","upper")] <-
        tmp.area[,c("median", "lower","upper")]*1000
      res.tmp <- res.smoothdir[res.smoothdir$region == 
                                 as.character(admin1.names$Internal[area.idx]),]
      res.tmp[,c("median", "lower", "upper")] <-
        res.tmp[,c("median", "lower", "upper")]*1000
      
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      
      
      par(mfrow = c(1,1),lend=1)
      if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
        plot.max <- max(c(tmp.area$upper,
                          direct.admin1$upper[direct.admin1$region == as.character(area)]),
                        na.rm = T) + 25
      }
      
      if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
        plot(NA,
             xlab = "Year", ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year + 1),
             type = 'l', col = cols[1], lwd = 2,
             main = admin1.names$GADM[area.idx])
        legend('topright', bty = 'n', col = c(cols, 'black'),
               lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
        
      } else {
        
        for(survey in surveys){
          tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                                 direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
          svy.idx <- match(survey, surveys) 
          
          
          if(svy.idx== 1){
            if(dim(tmp)[1] != 0){
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main = admin1.names$GADM[area.idx])
              pane.years <- (seq(beg.year+2, end.year-2,5))
              lines(pane.years, tmp$mean, cex = tmp$cex2,
                    type = 'l', col = cols[svy.idx],
                    main = surveys[svy.idx], lwd = 2)
              
              polygon(x = c(pane.years[!is.na(tmp$upper)],
                            rev(pane.years[!is.na(tmp$upper)])),
                      y = c(tmp$upper[!is.na(tmp$upper)],
                            rev(tmp$lower[!is.na(tmp$lower)])),
                      col = alpha('grey85', 0.25), 
                      border = FALSE)
              
              points(pane.years, tmp$mean, pch = 19,
                     col = alpha(cols[svy.idx], 0.35),
                     cex = tmp$cex2)
              
              
              #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
              #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
              
              # for(year.id in 1:length(periods)){
              #   segments(pane.years[year.id], tmp$upper[year.id],
              #            pane.years[year.id], tmp$lower[year.id],
              #            col = cols[svy.idx])
              # }
            }else{
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main =  paste0(admin1.names$GADM[area.idx]))
            }
          }else{
            if(dim(tmp)[1] != 0){
              lines(pane.years, tmp$mean, cex = tmp$cex2,
                    type = 'l', col = cols[svy.idx],
                    lwd = 2)
              polygon(x = c(pane.years[!is.na(tmp$upper)],
                            rev(pane.years[!is.na(tmp$upper)])),
                      y = c(tmp$upper[!is.na(tmp$upper)],
                            rev(tmp$lower[!is.na(tmp$lower)])),
                      col = alpha('grey85', 0.25), 
                      border = FALSE)
              
              points(pane.years, tmp$mean, pch = 19,
                     col = alpha(cols[svy.idx], 0.35),
                     cex = tmp$cex2)
            } 
          }
          
          
        }
        
        pane.years <- (seq(beg.year+2, end.year-2, 5))
        est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
        
        
        lines(pane.years[est.ids],
              res.tmp$median[est.ids], 
              col = cols[length(surveys)+2],
              lwd = 2, lty = 1)
        lines(pane.years[max(est.ids):length(pane.years)], 
              res.tmp$median[max(est.ids):length(pane.years)], 
              col = cols[length(surveys)+2], 
              lwd = 2, lty = 2)
        polygon(x = c(pane.years,
                      rev(pane.years)),
                y = c(res.tmp$upper[1:6],
                      rev(res.tmp$lower[1:6])),
                col = alpha(cols[length(surveys)+2], 0.25),
                border = FALSE)
        lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
              tmp.area$median[1:length(beg.year:max(mod.dat$years))],
              col = 'black',
              lwd = 2, lty = 1)
        lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              col = 'black', 
              lwd = 2, lty = 2)
        polygon(x = c(beg.year:end.year,
                      rev(beg.year:end.year)),
                y = c(tmp.area$upper,
                      rev(tmp.area$lower)),
                col = alpha('black', 0.25),
                border = FALSE)
        legend('topright', bty = 'n', 
               col = c(cols[1:length(surveys)],
                       cols[length(surveys) + 2],
                       'black'),
               lwd = 2, lty = c(rep(1, length(surveys)+1)),
               legend = c(surveys,
                          'Smoothed Direct',
                          'Betabinomial'),
               cex = 0.6)
        
        
      }
      
      
    }
  }
  dev.off()
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod,
             '_admin1Bench_noStrata_spaghetti_6per.pdf'),
      height = 9, width = 6)
  {
    par(mfrow = c(3,2),lend=1)
    area.idx <- 0
    for(area in admin1.names$Internal){
      area.idx <- area.idx + 1
      tmp.area <- res.admin1$overall[res.admin1$overall$region == area,]
      tmp.area$width <- tmp.area$upper - tmp.area$lower
      tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
      tmp.area$cex2[tmp.area$cex2 > 6] <- 6
      tmp.ihme <- ihme.ests[[2]][ihme.ests[[2]]$ADM1_NAME == as.character(admin1.names$GADM[area.idx]),]
      tmp.ihme[ ,c("mean", "lower", "upper")] <- 
        tmp.ihme[ ,c("mean", "lower", "upper")]*1000
      # print(tmp.ihme)
      cols <- rainbow(length(surveys)+1+1)
      plot.years <- beg.year:end.year
      tmp.area[,c("median", "lower","upper")] <-
        tmp.area[,c("median", "lower","upper")]*1000
      res.tmp <- res.smoothdir[res.smoothdir$region == 
                                 as.character(admin1.names$Internal[area.idx]),]
      res.tmp[,c("median", "lower", "upper")] <-
        res.tmp[,c("median", "lower", "upper")]*1000
      
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      
      
      if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
        plot.max <- max(c(tmp.area$upper,
                          direct.admin1$upper[direct.admin1$region == as.character(area)]),
                        na.rm = T) + 25
      }
      
      if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
        plot(NA,
             xlab = "Year", ylab = "U5MR",
             ylim = c(0, plot.max),
             xlim = c(beg.year, end.year + 1),
             type = 'l', col = cols[1], lwd = 2,
             main = admin1.names$GADM[area.idx])
        legend('topright', bty = 'n', col = c(cols, 'black'),
               lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
        
      } else {
        
        for(survey in surveys){
          tmp <- direct.admin1[direct.admin1$surveyYears == survey &
                                 direct.admin1$region == as.character(admin1.names$Internal[area.idx]),]
          svy.idx <- match(survey, surveys) 
          
          
          if(svy.idx== 1){
            if(dim(tmp)[1] != 0){
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main = admin1.names$GADM[area.idx])
              pane.years <- (seq(beg.year+2, end.year-2,5))
              lines(pane.years, tmp$mean, cex = tmp$cex2,
                    type = 'l', col = cols[svy.idx],
                    main = surveys[svy.idx], lwd = 2)
              
              polygon(x = c(pane.years[!is.na(tmp$upper)],
                            rev(pane.years[!is.na(tmp$upper)])),
                      y = c(tmp$upper[!is.na(tmp$upper)],
                            rev(tmp$lower[!is.na(tmp$lower)])),
                      col = alpha('grey85', 0.25), 
                      border = FALSE)
              
              points(pane.years, tmp$mean, pch = 19,
                     col = alpha(cols[svy.idx], 0.35),
                     cex = tmp$cex2)
              
              
              #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
              #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
              
              # for(year.id in 1:length(periods)){
              #   segments(pane.years[year.id], tmp$upper[year.id],
              #            pane.years[year.id], tmp$lower[year.id],
              #            col = cols[svy.idx])
              # }
            }else{
              plot(NA,
                   xlab = "Year", ylab = "U5MR",
                   ylim = c(0, plot.max),
                   xlim = c(beg.year, end.year),
                   type = 'l', col = cols[svy.idx], lwd = 2,
                   main =  paste0(admin1.names$GADM[area.idx]))
            }
          }else{
            if(dim(tmp)[1] != 0){
              lines(pane.years, tmp$mean, cex = tmp$cex2,
                    type = 'l', col = cols[svy.idx],
                    lwd = 2)
              polygon(x = c(pane.years[!is.na(tmp$upper)],
                            rev(pane.years[!is.na(tmp$upper)])),
                      y = c(tmp$upper[!is.na(tmp$upper)],
                            rev(tmp$lower[!is.na(tmp$lower)])),
                      col = alpha('grey85', 0.25), 
                      border = FALSE)
              
              points(pane.years, tmp$mean, pch = 19,
                     col = alpha(cols[svy.idx], 0.35),
                     cex = tmp$cex2)
            } 
          }
          
          
        }
        
        lines(tmp.ihme$year, 
              tmp.ihme$mean, lwd = 2,
              lty = 1, 
              col  = cols[length(surveys)+1])
        polygon(c(tmp.ihme$year,
                  rev(tmp.ihme$year)),
                c(tmp.ihme$upper,
                  rev(tmp.ihme$lower)),
                col = alpha(cols[length(surveys)+1], 0.25),
                border = FALSE)
        
        pane.years <- (seq(beg.year+2, end.year-2, 5))
        est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
        
        
        lines(pane.years[est.ids],
              res.tmp$median[est.ids], 
              col = cols[length(surveys)+2],
              lwd = 2, lty = 1)
        lines(pane.years[max(est.ids):length(pane.years)], 
              res.tmp$median[max(est.ids):length(pane.years)], 
              col = cols[length(surveys)+2], 
              lwd = 2, lty = 2)
        polygon(x = c(pane.years,
                      rev(pane.years)),
                y = c(res.tmp$upper[1:6],
                      rev(res.tmp$lower[1:6])),
                col = alpha(cols[length(surveys)+2], 0.25),
                border = FALSE)
        lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
              tmp.area$median[1:length(beg.year:max(mod.dat$years))],
              col = 'black',
              lwd = 2, lty = 1)
        lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
              col = 'black', 
              lwd = 2, lty = 2)
        polygon(x = c(beg.year:end.year,
                      rev(beg.year:end.year)),
                y = c(tmp.area$upper,
                      rev(tmp.area$lower)),
                col = alpha('black', 0.25),
                border = FALSE)
        legend('topright', bty = 'n', col = c(cols, 'black'),
               lwd = 2, lty = c(rep(1, length(surveys)+3)),
               legend = c(surveys,'IHME', 'Smoothed Direct', 'Betabinomial'),
               cex = 0.6)
        
        
      }
      
      
    }
  }
  dev.off()
  
  ages <- c("0", "1-11", "12-23",
            "24-35", "36-47", "48-59")
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_',
             time.mod,
             '_admin1Bench_temporal.pdf'),
      height = 3, width = 4)
  {
    par(mfrow =c(1,1),lend=1)
    age.cols <- rainbow(length(ages))
    age.cols[2] <- "orange"
    for(age in ages){
      age.idx <- grepl(paste0(age),temporals$group)
      fixed.idx <- grepl(paste0(age,"$"), row.names(fixed.eff.table))
      fixed.eff <- fixed.eff.table$`0.5quant`[fixed.idx]
      tmp<- temporals[age.idx,]
      
      if(match(age, ages) == 1 ){
        plot.min <- min(outer(temporals$median, fixed.eff.table$`0.5quant`, FUN="+")) - 0.25
        plot.max <- max(outer(temporals$median, fixed.eff.table$`0.5quant`, FUN="+")) + 0.25
        #lim <- max(abs(plot.min),abs(plot.max))
        
        plot(NA, xlim = c(beg.year, end.year),
             ylim = c(plot.min, plot.max),
             
             xlab = "Year",
             ylab = "Effect size",
             main = country)
      }
      
      lines(beg.year:end.year, tmp$median + fixed.eff,
            col = age.cols[match(age, ages)], lty = 1)
      
    }
    
    legend('topright', cex = 0.7, lty = 1,
           col = c(age.cols),
           legend = c(ages), bty = 'n')
  }
  dev.off()
  
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_',
             time.mod, '_admin1Bench_coefs.pdf'),
      height = 5, width = 5)
  {
    par(mfrow =c(1,1),lend=1)
    age.cols <- rainbow(length(ages))
    age.cols[2] <- "orange"
    for(age in ages){
      age.idx <- grepl(paste0(age),temporals$group)
      fixed.idx <- grepl(paste0(age,"$"), row.names(fixed.eff.table))
      fixed.eff <- fixed.eff.table[fixed.idx,]
      tmp<- temporals[age.idx,]
      
      if(match(age, ages) == 1 ){
        plot.min <- min(fixed.eff.table$`0.025quant`) - 0.025
        plot.max <- max(fixed.eff.table$`0.975quant`) + 0.025
        #lim <- max(abs(plot.min),abs(plot.max))
        
        plot(NA, xlim = c(0,7), xaxt = 'n',
             ylim = c(plot.min, plot.max),
             xlab = "Year",
             ylab = "Effect size",
             main = country)
      }
      
      points(match(age,ages), fixed.eff$`0.5quant`,
             pch = 19, col = age.cols[match(age, ages)])
      segments(match(age,ages), fixed.eff$`0.025quant`,
               match(age,ages), fixed.eff$`0.975quant`,
               col = age.cols[match(age, ages)], lty = 1)
      
    }
    
    legend('bottomleft', cex = 0.7, lty = 1,
           col = c(age.cols), pch =19,
           legend = c(ages), bty = 'n')
  }
  dev.off()
  
  if (grepl("randomSlopes",time.mod)) {
    pdf(paste0(folder.name, '/Plots/Betabinomial/',
               country, '_', time.mod, '_admin1Bench_randomSlopes.pdf'), height = 5, width = 5)
    years.std <- ((beg.year:end.year)-mean(beg.year:end.year))/sd(beg.year:end.year)
    # plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.025quant","0.975quant")],
    #                       years.std[length(years.std)]*posteriorRandomSlopes[,c("0.025quant","0.975quant")]))
    plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.5quant")],
                          years.std[length(years.std)]*posteriorRandomSlopes[,c("0.5quant")]))
    par(lend=1)
    plot(NA,xlim=range(years.std),ylim = plot.range,
         ylab = "effect",xlab="year (standardized)")
    
    region.cols <- rainbow(nrow(posteriorRandomSlopes))
    for (i in 1:nrow(posteriorRandomSlopes)) {
      abline(0,posteriorRandomSlopes$`0.5quant`[i],col=region.cols[i])
      # polygon(x=c(years.std,rev(years.std)),
      #         y=c(years.std*posteriorRandomSlopes$`0.975quant`[i],rev(years.std*posteriorRandomSlopes$`0.025quant`[i])),
      #         col=alpha(region.cols[i],0.25),border = NA)
    }
    legend("top",legend=admin1.names[,1],col=region.cols,lty=1,ncol=ceiling(nrow(posteriorRandomSlopes)/2),cex=0.5)
    dev.off()
  }
  
  
  med.palette <- rev(brewer.pal(n = 7, name = "RdBu"))
  med.int <- classIntervals(round(spaces$median[spaces$label == "Total"], 3),
                            n = 7, style = 'jenks')
  med.col <- findColours(med.int, med.palette)
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod, '_admin1Bench_spatial.pdf'))
  par(mar = c(0.25,0.25,0.25,0.25),lend=1)
  plot(poly.adm1, col = med.col,
       xlim = poly.adm1@bbox['x',],
       ylim = poly.adm1@bbox['y',],
       axes = F)
  legend('bottomleft', fill = med.palette,
         legend = names(attr(med.col, 'table')),
         bty = 'n', cex = 0.75)
  dev.off()
  
## Spaghetti plots for all regions over time ####
  # change the region names from Internal to GADM for legend labels
  res.admin1$overall$region.orig <- res.admin1$overall$region
  for (i in 1:nrow(admin1.names)) {
    res.admin1$overall$region[as.character(res.admin1$overall$region) == 
                                as.character(admin1.names$Internal[i])] <- paste(as.character(admin1.names$GADM[i]))
  }
  
  # make the plot
  
  numberAreasPerPage <- 30
  numberAreasTotal <- nrow(admin1.names)
  numberPages <- ceiling(numberAreasTotal/numberAreasPerPage)
  
  # order data by median magnitude in 2020
  res.admin1.order <- res.admin1$overall %>% 
    filter(years.num == 2019) %>%
    arrange(median)
  areaOrder <- res.admin1.order$region.orig
  # loop and make plots
  for (i in 1:numberPages) {
    if (i != numberPages) {
      areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
    } else {
      areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
    }
    
    tmp <- res.admin1$overall[res.admin1$overall$region.orig %in% areas,]
    pdf(paste0(folder.name, '/Plots/Betabinomial/',
               country, '_',
               time.mod,
               '_admin1Bench_spaghetti_all_',
               i,'.pdf'))
    par(lend=1)
    
    g <- ggplot(tmp, aes(x = years.num, 
                         y = median*1000,
                         col = region)) +
      geom_line() +
      geom_point() +
      theme_light() +
      xlab("Year") +
      ylab("U5MR: deaths per 1000 live births") +
      ggtitle(country) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 8)) +
      guides(col = guide_legend(ncol=2)) +
      ylim(c(0, max(res.admin1$overall$median)*1000))
    print(g)
    dev.off()
  }
  
  
  
# Admin 2 #### 
if(useHIVAdj){
  load(paste0(folder.name, '/',
              country, '_directHIV_admin2.rda'), envir = .GlobalEnv)
}else{
  load(paste0(folder.name, '/',
              country, '_direct_admin2.rda'), envir = .GlobalEnv)
}

direct.admin2[,c("mean", "lower", "upper")] <- 
  direct.admin2[,c("mean", "lower", "upper")]*1000

load(paste0(folder.name,'/', country,
            '_res_', time.mod, '_admin2.rda'))
load(paste0(folder.name,'/', country, '_', 
            time.mod, '_admin2Benchmarks.rda'))
load(paste0(folder.name, '/', country, 
            '_', time.mod,'_admin2_noStrata_temporals.rda'))
load(paste0(folder.name, '/',
            country, '_', time.mod,'_admin2_noStrata_spatials.rda'))
load(paste0(folder.name, '/',
            country, 
            '_', time.mod,'_admin2_noStrata_fixedeff.rda'))

if (grepl("randomSlopes",time.mod)) {
  load(paste0(folder.name, '/',
              country,'_',
              time.mod,'_admin2_noStrata_posteriorRandomSlopes.rda'))
}
## Spaghetti Plot ####

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2_noStrata_spaghetti.pdf'),
    height = 9, width = 6)
{
  par(mfrow = c(3,2),lend=1)
  area.idx <- 0
  for(area in admin2.names$Internal){
    
    # testing
    # area <- "admin2_31"
    # area.idx <- 30
    
    area.idx <- area.idx + 1
    tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == as.character(admin2.names$GADM[area.idx]) & 
                                 ihme.ests[[3]]$ADM1_NAME == as.character(poly.adm2$NAME_1[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <-
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    tmp.area[,c("median", "lower", "upper")] <- 
      tmp.area[,c("median", "lower", "upper")]*1000
    # print(tmp.ihme)
    cols <- rainbow(length(surveys)+1)
    plot.years <- beg.year:end.year
    pane.years <- (seq(beg.year+2, end.year-2, 5))
    est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
    
    
    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper, 
                        direct.admin2$mean[direct.admin2$region == area]),
                      na.rm = T) + 25
    }else{
      plot.max <- 0.25
    }
    
    if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           type = 'l', col = cols[1], lwd = 2,
           main = paste(admin2.names$GADM[area.idx], poly.adm2$NAME_1[area.idx], sep = ", "),
           cex.main = 0.8)
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
      
    } else {
      
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = paste(admin2.names$GADM[area.idx], poly.adm2$NAME_1[area.idx], sep = ", "),
                 cex.main = 0.8)
            pane.years <- (seq(beg.year+2, end.year-2,5))
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            ihme.years <- (tmp.ihme$year)
            lines(ihme.years, tmp.ihme$mean, lwd = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            
            # for(year.id in 1:length(periods)){
            #   segments(pane.years[year.id], tmp$upper[year.id],
            #            pane.years[year.id], tmp$lower[year.id],
            #            col = cols[svy.idx])
            # }
          }else{
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = paste(admin2.names$GADM[area.idx], poly.adm2$NAME_1[area.idx], sep = ", "),
                 cex.main = 0.8)
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  lwd = 2)
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            # for(year.id in 1:length(periods)){
            #   segments(pane.years[year.id], tmp$upper[year.id],
            #            pane.years[year.id], tmp$lower[year.id],
            #            col = cols[svy.idx])
            # }
          } 
        }
        
        
      }
      
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
            tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
            lwd = 2, lty = 1)
      lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = c(rep(1, length(cols)+1)),
             legend = c(surveys, 'IHME', 'Betabinomial'),
             cex = 0.6)
      
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx == 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = "")
            pane.years <- seq(beg.year+2, end.year-3, 5)[which(!is.na(tmp$mean))]
            polygon(x = c(pane.years,
                          rev(pane.years)),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha(cols[svy.idx], 0.25),
                    border = FALSE)
            
            
          }else{
            
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = "")
          }
          
        }else{
          
          pane.years <- seq(beg.year+2, end.year-3, 5)[which(!is.na(tmp$mean))]
          polygon(x = c(pane.years,
                        rev(pane.years)),
                  y = c(tmp$upper[!is.na(tmp$upper)],
                        rev(tmp$lower[!is.na(tmp$lower)])),
                  col = alpha(cols[svy.idx], 0.25),
                  border = FALSE)
        }
        
      }  
      
      legend('topright', bty = 'n',
             fill = alpha(cols[1:length(surveys)], 0.25),
             border = cols[1:length(surveys)],
             legend = c(surveys),
             cex = 0.6)
      
      for(survey in surveys){
        # debugging
        # survey <- surveys[1]
        # survey <- surveys[2]
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = "")
          }else{
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = "")
          }
        }
        
        
      }
      
      ihme.years <- tmp.ihme$year
      polygon(x = c(ihme.years, rev(ihme.years)),
              y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
              col = alpha(cols[length(surveys)+1], 0.25),
              border = FALSE)
      pane.years <- seq(beg.year+2.5,end.year-2.5,5)
      polygon(x = c(pane.years, rev(pane.years)),
              y = c(tmp.area$upper[1:length(pane.years)],
                    rev(tmp.area$lower[1:length(pane.years)])),
              col = alpha(cols[length(surveys)+2], 0.25),
              border = FALSE)
      
      polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
            tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
            lwd = 2, lty = 1)
      lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      #      }
      legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                   'black'), .25),
             border = c(cols[(length(surveys)+1):length(cols)],
                        'black'),
             legend = c('IHME', 'Betabinomial'), cex = 0.75)
    }
    
    
  }
}
dev.off()

ages <- c("0", "1-11", "12-23",
          "24-35", "36-47", "48-59")

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2_temporal.pdf'), height = 5, width = 7.5)
{
  par(mfrow =c(1,1),lend=1)
  age.cols <- rainbow(length(ages))
  age.cols[2] <- "orange"
  for(age in ages){
    age.idx <- grepl(paste0(age),temporals$group)
    fixed.idx <- grepl(paste0(age,"$"), row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table$`0.5quant`[fixed.idx]
    tmp<- temporals[age.idx,]
    
    if(match(age, ages) == 1 ){
      plot.min <- min(temporals$median +
                        min(fixed.eff.table$`0.5quant`)) -0.25
      plot.max <- max(temporals$median +
                        max(fixed.eff.table$`0.5quant`)) +0.25
      #lim <- max(abs(plot.min),abs(plot.max))
      
      plot(NA, xlim = c(beg.year, end.year),
           ylim = c(plot.min, plot.max),
           
           xlab = "Year",
           ylab = "Monthly hazard",
           main = country)
      #axis(2, at = seq(plot.min, plot.max, 1),
      #     labels = round(expit(seq(plot.min, plot.max, 1))*1000, digits = 1))
    }
    
    lines(beg.year:end.year, tmp$median + fixed.eff,
          col = age.cols[match(age, ages)], lty = 1,
          lwd = 2)
    
  }
  
  legend('bottomleft', cex = 0.7, lwd = 2,
         col = c(age.cols),
         legend = c(ages), bty = 'n')
}
dev.off()

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2_coefs.pdf'), height = 5, width = 5)
{
  par(mfrow =c(1,1),lend=1)
  age.cols <- rainbow(length(ages))
  age.cols[2] <- "orange"
  for(age in ages){
    age.idx <- grepl(paste0(age),temporals$group)
    fixed.idx <- grepl(paste0(age,"$"), row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table[fixed.idx,]
    tmp<- temporals[age.idx,]
    
    if(match(age, ages) == 1 ){
      plot.min <- min(fixed.eff.table$`0.025quant`) - 0.025
      plot.max <- max(fixed.eff.table$`0.975quant`) + 0.025
      #lim <- max(abs(plot.min),abs(plot.max))
      
      plot(NA, xlim = c(0,7), xaxt = 'n',
           ylim = c(plot.min, plot.max),
           xlab = "Year",
           ylab = "Effect size",
           main = country)
    }
    
    points(match(age,ages), fixed.eff$`0.5quant`,
           pch = 19, col = age.cols[match(age, ages)])
    segments(match(age,ages), fixed.eff$`0.025quant`,
             match(age,ages), fixed.eff$`0.975quant`,
             col = age.cols[match(age, ages)], lty = 1)
    
  }
  
  legend('bottomleft', cex = 0.7, lty = 1,
         col = c(age.cols), pch =19,
         legend = c(ages), bty = 'n')
}
dev.off()

if (grepl("randomSlopes",time.mod)) {
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod, '_admin2_randomSlopes.pdf'), height = 5, width = 5)
  years.std <- ((beg.year:end.year)-mean(beg.year:end.year))/sd(beg.year:end.year)
  # plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.025quant","0.975quant")],
  #                       years.std[length(years.std)]*posteriorRandomSlopes[,c("0.025quant","0.975quant")]))
  plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.5quant")],
                        years.std[length(years.std)]*posteriorRandomSlopes[,c("0.5quant")]))
  par(lend=1)
  plot(NA,xlim=range(years.std),ylim = plot.range,
       ylab = "effect",xlab="year (standardized)")
  
  region.cols <- rainbow(nrow(posteriorRandomSlopes))
  for (i in 1:nrow(posteriorRandomSlopes)) {
    abline(0,posteriorRandomSlopes$`0.5quant`[i],col=region.cols[i])
    # polygon(x=c(years.std,rev(years.std)),
    #         y=c(years.std*posteriorRandomSlopes$`0.975quant`[i],rev(years.std*posteriorRandomSlopes$`0.025quant`[i])),
    #         col=alpha(region.cols[i],0.25),border = NA)
  }
  legend("top",legend=admin1.names[,1],col=region.cols,lty=1,ncol=ceiling(nrow(posteriorRandomSlopes)/2),cex=0.5)
  dev.off()
}


## Polygon Plots ####

centroids <- gCentroid(poly.adm2, byid = TRUE,
                       id = poly.adm2@data$GID_2)
pdf(paste0(folder.name, '/Plots/ShapeCheck/',
           country,'_Admin2Names.pdf'))
par(lend=1)
plot(poly.adm2,
     xlim = poly.adm2@bbox['x',],
     ylim = poly.adm2@bbox['y',],
     axes = F)
text(centroids$x, centroids$y,
     labels = poly.adm2@data$NAME_2,
     cex = 0.45)
dev.off()


med.palette <- rev(brewer.pal(n = 7, name = "RdBu"))
med.int <- classIntervals(round(spaces$median[spaces$label == "Total"], 3),
                          n = 7, style = 'jenks')
med.col <- findColours(med.int, med.palette)

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2_spatial.pdf'))
par(mar = c(0.25,0.25,0.25,0.25),lend=1)
plot(poly.adm2, col = med.col,
     xlim = poly.adm2@bbox['x',],
     ylim = poly.adm2@bbox['y',],
     axes = F)
legend('bottomleft', fill = med.palette,
       legend = names(attr(med.col, 'table')),
       bty = 'n', cex = 0.75)
dev.off()


## Spaghetti plots of all regions over time ####

# change the region names from Internal to GADM for legend labels
res.admin2$overall$region1.gadm <- 
  res.admin2$overall$region.gadm <- 
  res.admin2$overall$region.orig <- 
  res.admin2$overall$region
for (i in 1:nrow(admin2.names)) {
  res.admin2$overall$region[as.character(res.admin2$overall$region) == 
                              as.character(admin2.names$Internal[i])] <- paste(as.character(admin2.names$GADM[i]),
                                                                               as.character(poly.adm2$NAME_1[i]), sep =", ")
  res.admin2$overall$region.gadm[as.character(res.admin2$overall$region.orig) == 
                              as.character(admin2.names$Internal[i])] <- paste(as.character(admin2.names$GADM[i]))
  res.admin2$overall$region1.gadm[as.character(res.admin2$overall$region.orig) == 
                                   as.character(admin2.names$Internal[i])] <- paste(as.character(poly.adm2$NAME_1[i]))
                                                                               
}

# make the plot ordered by magnitude of u5mr in 2020

# numberAreasPerPage <- 15
numberAreasTotal <- nrow(admin2.names)
# numberPages <- ceiling(numberAreasTotal/numberAreasPerPage)
numberPages <- nrow(admin1.names)
# order data by median magnitude in 2020
res.admin2.order <- res.admin2$overall %>% 
  filter(years.num == 2020) %>%
  arrange(region.gadm)
areaOrder <- res.admin2.order$region.orig

# loop and make plots
for (i in 1:numberPages) {

  if (i != numberPages) {
    #areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
    areas <- res.admin2.order$region.orig[res.admin2.order$region1.gadm ==
                                                as.character(admin1.names$GADM[i])]
  } else {
    #areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
    areas <- res.admin2.order$region.orig[res.admin2.order$region1.gadm ==
                                            as.character(admin1.names$GADM[i])]
  }
  tmp <- res.admin2$overall[res.admin2$overall$region.orig %in% areas,]
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_',
             time.mod, 
             '_admin2_spaghetti_allByAdmin1_',
             i,'.pdf'),
      height = 6, width = 6)
  {
  par(lend=1)
  
  g <- ggplot(tmp, aes(x = years.num,
                        y = median*1000,
                        col = region.gadm)) +
          geom_line() +
          geom_point() +
          theme_light() +
          xlab("Year") +
          ylab("U5MR: deaths per 1000 live births") +
          ggtitle(paste0(unique(tmp$region1.gadm))) +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 8)) +
          ylim(c(0, max(res.admin2$overall$median)*1000))
  g <- g + 
    guides(col = guide_legend(ncol=3,
                              title = "Admin 2")) 
  print(g)
  dev.off()
  }
}

numberAreasPerPage <- 30
numberAreasTotal <- nrow(admin2.names)
numberPages <- ceiling(numberAreasTotal/numberAreasPerPage)
# order data by median magnitude in 2020
res.admin2.order <- res.admin2$overall %>% 
  filter(years.num == 2019) %>%
  arrange(desc(median))
areaOrder <- res.admin2.order$region.orig

# loop and make plots
for (i in 1:numberPages) {
  
  if (i != numberPages) {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
    # areas <- res.admin2.order$region.orig[res.admin2.order$region1.gadm ==
    #                                         as.character(admin1.names$GADM[i])]
  } else {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
    # areas <- res.admin2.order$region.orig[res.admin2.order$region1.gadm ==
    #                                         as.character(admin1.names$GADM[i])]
  }
  tmp <- res.admin2$overall[res.admin2$overall$region.orig %in% areas,]
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_',
             time.mod, 
             '_admin2_spaghetti_allByMedian_',
             i,'.pdf'),
      height = 6, width = 6)
  {
  par(lend=1)
  
  print(ggplot(tmp, aes(x = years.num,
                        y = median*1000,
                        col = region.gadm)) +
          geom_line() +
          geom_point() +
          theme_light() +
          xlab("Year") +
          ylab("U5MR: deaths per 1000 live births") +
          ggtitle(country) +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 8)) +
          guides(col = guide_legend(ncol=3,
                                    title = "Admin 2")) +
          ylim(c(0, max(res.admin2$overall$median)*1000)))
  dev.off()
  }
}



# make the plot ordered by admin1/admin2
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2_spaghetti_all_GADMorder.pdf'))
par(lend=1)

# order data by GADM
areaOrder <- admin2.names$Internal

# change the order 
# loop and make plots
for (i in 1:numberPages) {
  if (i != numberPages) {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
  } else {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
  }
  tmp <- res.admin2$overall[res.admin2$overall$region.orig %in% areas,]
  
  print(ggplot(tmp, aes(x = years.num, y = median*1000, col = region)) +
          geom_line() +
          geom_point() +
          theme_light() +
          xlab("Year") +
          ylab("U5MR: deaths per 1000 live births") +
          ggtitle(country) +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 8)) +
          guides(col = guide_legend(ncol=3,
                                    title = "Admin 2")) +
          ylim(c(0, max(res.admin2$overall$median)*1000)))
}

dev.off()

# Admin 2 benchmark ####

# reloading these in case you're just running the benchmark section of the code
if(useHIVAdj){
  load(paste0(folder.name, '/',
              country, '_directHIV_admin2.rda'), envir = .GlobalEnv)
}else{
  load(paste0(folder.name, '/',
              country, '_direct_admin2.rda'), envir = .GlobalEnv)
}

direct.admin2[,c("mean", "lower", "upper")] <- 
  direct.admin2[,c("mean", "lower", "upper")]*1000


load(paste0(folder.name,'/', country,
            '_res_', time.mod, '_admin2Bench.rda'))
load(paste0(folder.name,'/', country, '_', 
            time.mod, '_admin2Benchmarks.rda'))
load(paste0(folder.name, '/', country, 
            '_', time.mod,'_admin2Bench_noStrata_temporals.rda'))
load(paste0(folder.name, '/',
            country, '_', time.mod,'_admin2Bench_noStrata_spatials.rda'))
load(paste0(folder.name, '/',
            country, 
            '_', time.mod, '_admin2Bench_noStrata_fixedeff.rda'))

if (grepl("randomSlopes",time.mod)) {
  load(paste0(folder.name, '/',
              country,'_',
              time.mod,'_admin2Bench_noStrata_posteriorRandomSlopes.rda'))
}
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2Benchmarks.pdf'), height = 4, width = 4)
{par(lend=1)
  plot(NA, xlim = c(beg.year, end.year),
       ylim = c(min(bench.adj$ratio)-0.025,
                max(bench.adj$ratio) + 0.025),
       xlab = "Year", ylab = "Offset",
       main = country)
  abline(h = 1)
  
  
  bench.tmp <- bench.adj$ratio
  bench.tmp[bench.tmp == 1] <- NA
  lines(beg.year:end.year,
        bench.tmp,
        lwd = 2, col = 'red')
  
}
dev.off()

#### Spaghetti Plot  ####

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_',
           time.mod,
           '_admin2Bench_noStrata_spaghetti.pdf'),
    height = 9, width = 6)
{
  par(mfrow = c(3,2),lend=1)
  area.idx <- 0
  for(area in admin2.names$Internal){
    
    # testing
    # area <- "admin2_31"
    # area.idx <- 30
    
    area.idx <- area.idx + 1
    tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == as.character(admin2.names$GADM[area.idx]) & 
                                 ihme.ests[[3]]$ADM1_NAME == as.character(poly.adm2$NAME_1[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <-
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    tmp.area[,c("median", "lower", "upper")] <- 
      tmp.area[,c("median", "lower", "upper")]*1000
    # print(tmp.ihme)
    cols <- rainbow(length(surveys)+1)
    plot.years <- beg.year:end.year
    pane.years <- (seq(beg.year+2, end.year-2, 5))
    est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
    
    
    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper, 
                        direct.admin2$mean[direct.admin2$region == area]),
                      na.rm = T) + 25
    }else{
      plot.max <- 0.25
    }
    
    if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           type = 'l', col = cols[1], lwd = 2,
           main = paste(admin2.names$GADM[area.idx], poly.adm2$NAME_1[area.idx], sep = ", "),
           cex.main = 0.8)
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
      
    } else {
      
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = paste(admin2.names$GADM[area.idx], poly.adm2$NAME_1[area.idx], sep = ", "),
                 cex.main = 0.8)
            pane.years <- (seq(beg.year+2, end.year-2,5))
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            ihme.years <- (tmp.ihme$year)
            lines(ihme.years, tmp.ihme$mean, lwd = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            
            # for(year.id in 1:length(periods)){
            #   segments(pane.years[year.id], tmp$upper[year.id],
            #            pane.years[year.id], tmp$lower[year.id],
            #            col = cols[svy.idx])
            # }
          }else{
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = paste(admin2.names$GADM[area.idx], poly.adm2$NAME_1[area.idx], sep = ", "),
                 cex.main = 0.8)
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  lwd = 2)
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            # for(year.id in 1:length(periods)){
            #   segments(pane.years[year.id], tmp$upper[year.id],
            #            pane.years[year.id], tmp$lower[year.id],
            #            col = cols[svy.idx])
            # }
          } 
        }
        
        
      }
      
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
            tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
            lwd = 2, lty = 1)
      lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = c(rep(1, length(cols)+1)),
             legend = c(surveys, 'IHME', 'Betabinomial'),
             cex = 0.6)
      
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx == 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = "")
            pane.years <- seq(beg.year+2, end.year-3, 5)[which(!is.na(tmp$mean))]
            polygon(x = c(pane.years,
                          rev(pane.years)),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha(cols[svy.idx], 0.25),
                    border = FALSE)
            
            
          }else{
            
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = "")
          }
          
        }else{
          
          pane.years <- seq(beg.year+2, end.year-3, 5)[which(!is.na(tmp$mean))]
          polygon(x = c(pane.years,
                        rev(pane.years)),
                  y = c(tmp$upper[!is.na(tmp$upper)],
                        rev(tmp$lower[!is.na(tmp$lower)])),
                  col = alpha(cols[svy.idx], 0.25),
                  border = FALSE)
        }
        
      }  
      
      legend('topright', bty = 'n',
             fill = alpha(cols[1:length(surveys)], 0.25),
             border = cols[1:length(surveys)],
             legend = c(surveys),
             cex = 0.6)
      
      for(survey in surveys){
        # debugging
        # survey <- surveys[1]
        # survey <- surveys[2]
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = "")
          }else{
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = "")
          }
        }
        
        
      }
      
      ihme.years <- tmp.ihme$year
      polygon(x = c(ihme.years, rev(ihme.years)),
              y = c(tmp.ihme$upper, rev(tmp.ihme$lower)),
              col = alpha(cols[length(surveys)+1], 0.25),
              border = FALSE)
      pane.years <- seq(beg.year+2.5,end.year-2.5,5)
      polygon(x = c(pane.years, rev(pane.years)),
              y = c(tmp.area$upper[1:length(pane.years)],
                    rev(tmp.area$lower[1:length(pane.years)])),
              col = alpha(cols[length(surveys)+2], 0.25),
              border = FALSE)
      
      polygon(x = c(tmp.area$years.num, rev(tmp.area$years.num)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
            tmp.area$median[1:length(beg.year:max(mod.dat$years))], col = 'black',
            lwd = 2, lty = 1)
      lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            col = 'black', 
            lwd = 2, lty = 2)
      
      #      }
      legend('topright', bty = 'n', fill = alpha(c(cols[(length(surveys)+1):length(cols)],
                                                   'black'), .25),
             border = c(cols[(length(surveys)+1):length(cols)],
                        'black'),
             legend = c('IHME', 'Betabinomial'), cex = 0.75)
    }
    
    
  }
}
dev.off()

ages <- c("0", "1-11", "12-23",
          "24-35", "36-47", "48-59")

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2Bench_temporal.pdf'), height = 5, width = 7.5)
{
  par(mfrow =c(1,1),lend=1)
  age.cols <- rainbow(length(ages))
  age.cols[2] <- "orange"
  for(age in ages){
    age.idx <- grepl(paste0(age),temporals$group)
    fixed.idx <- grepl(paste0(age,"$"), row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table$`0.5quant`[fixed.idx]
    tmp<- temporals[age.idx,]
    
    if(match(age, ages) == 1 ){
      plot.min <- min(temporals$median +
                        min(fixed.eff.table$`0.5quant`)) -0.25
      plot.max <- max(temporals$median +
                        max(fixed.eff.table$`0.5quant`)) +0.25
      #lim <- max(abs(plot.min),abs(plot.max))
      
      plot(NA, xlim = c(beg.year, end.year),
           ylim = c(plot.min, plot.max),
           
           xlab = "Year",
           ylab = "Monthly hazard",
           main = country)
      #axis(2, at = seq(plot.min, plot.max, 1),
      #     labels = round(expit(seq(plot.min, plot.max, 1))*1000, digits = 1))
    }
    
    lines(beg.year:end.year, tmp$median + fixed.eff,
          col = age.cols[match(age, ages)], lty = 1,
          lwd = 2)
    
  }
  
  legend('bottomleft', cex = 0.7, lwd = 2,
         col = c(age.cols),
         legend = c(ages), bty = 'n')
}
dev.off()

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod,
           '_admin2Bench_noStrata_spaghettiSingle.pdf'),
    height = 5, width = 5)
{
  area.idx <- 0
  for(area in admin2.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == as.character(admin2.names$GADM[area.idx]) & 
                               ihme.ests[[3]]$ADM1_NAME == as.character(poly.adm2@data$NAME_1[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <- 
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    # print(tmp.ihme)
    cols <- rainbow(length(surveys)+1+1)
    plot.years <- beg.year:end.year
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
    
    pane.years <- (seq(beg.year+2, end.year-2, 5))
    est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
    
    
    par(mfrow = c(1,1),lend=1)
    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin2$upper[direct.admin2$region == as.character(area)]),
                      na.rm = T) + 25
    }
    
    if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           type = 'l', col = cols[1], lwd = 2,
           main = paste(admin2.names$GADM[area.idx],
                        poly.adm2$NAME_1[area.idx], 
                        sep = ", "))
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
      
    } else {
      
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = paste(admin2.names$GADM[area.idx],
                              poly.adm2$NAME_1[area.idx], 
                              sep = ", "))
            pane.years <- (seq(beg.year+2, end.year-2,5))
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            
            polygon(x = c(pane.years[!is.na(tmp$upper)],
                          rev(pane.years[!is.na(tmp$upper)])),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            
            # for(year.id in 1:length(periods)){
            #   segments(pane.years[year.id], tmp$upper[year.id],
            #            pane.years[year.id], tmp$lower[year.id],
            #            col = cols[svy.idx])
            # }
          }else{
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main =  paste(admin2.names$GADM[area.idx],
                               poly.adm2$NAME_1[area.idx], 
                               sep = ", "))
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  lwd = 2)
            polygon(x = c(pane.years[!is.na(tmp$upper)],
                          rev(pane.years[!is.na(tmp$upper)])),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      
      lines(tmp.ihme$year, 
            tmp.ihme$mean, lwd = 2,
            lty = 1, 
            col  = cols[length(surveys)+1])
      polygon(c(tmp.ihme$year,
                rev(tmp.ihme$year)),
              c(tmp.ihme$upper,
                rev(tmp.ihme$lower)),
              col = alpha(cols[length(surveys)+1], 0.25),
              border = FALSE)
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      
      
      lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
            tmp.area$median[1:length(beg.year:max(mod.dat$years))],
            col = 'black',
            lwd = 2, lty = 1)
      lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            col = 'black', 
            lwd = 2, lty = 2)
      polygon(x = c(beg.year:end.year,
                    rev(beg.year:end.year)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      legend('topright', bty = 'n',
             col = c(cols[1:(length(surveys)+1)],
                                            'black'),
             lwd = 2, lty = c(rep(1, length(surveys)+1)),
             legend = c(surveys, 'IHME',
                        'Betabinomial'),
             cex = 0.6)
      
      
    }
    
    
  }
}
dev.off()

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod,
           '_admin2Bench_noStrata_spaghettiSingle_noIHME.pdf'),
    height = 5, width = 5)
{
  area.idx <- 0
  for(area in admin2.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME == 
                                 as.character(admin2.names$GADM[area.idx]) &
                                 ihme.ests[[3]]$ADM1_NAME == as.character(poly.adm2@data$NAME_1[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <- 
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    # print(tmp.ihme)
    cols <- rainbow(length(surveys)+1+1)
    plot.years <- beg.year:end.year
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
      pane.years <- (seq(beg.year+2, end.year-2, 5))
    est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
    
    
    par(mfrow = c(1,1),lend=1)
    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin2$upper[direct.admin2$region ==
                                              as.character(area)]),
                      na.rm = T) + 25
    }
    
    if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           type = 'l', col = cols[1], lwd = 2,
           main = paste(admin2.names$GADM[area.idx],
                        poly.adm2$NAME_1[area.idx], 
                        sep = ", "))
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
      
    } else {
      
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region == as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = admin2.names$GADM[area.idx])
            pane.years <- (seq(beg.year+2, end.year-2,5))
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  main = surveys[svy.idx], lwd = 2)
            
            polygon(x = c(pane.years[!is.na(tmp$upper)],
                          rev(pane.years[!is.na(tmp$upper)])),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            
            #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            
            # for(year.id in 1:length(periods)){
            #   segments(pane.years[year.id], tmp$upper[year.id],
            #            pane.years[year.id], tmp$lower[year.id],
            #            col = cols[svy.idx])
            # }
          }else{
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = paste(admin2.names$GADM[area.idx],
                              poly.adm2$NAME_1[area.idx], 
                              sep = ", "))
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  lwd = 2)
            polygon(x = c(pane.years[!is.na(tmp$upper)],
                          rev(pane.years[!is.na(tmp$upper)])),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      
      
   
      lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
            tmp.area$median[1:length(beg.year:max(mod.dat$years))],
            col = 'black',
            lwd = 2, lty = 1)
      lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            col = 'black', 
            lwd = 2, lty = 2)
      polygon(x = c(beg.year:end.year,
                    rev(beg.year:end.year)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      legend('topright', bty = 'n', 
             col = c(cols[1:length(surveys)],
                     'black'),
             lwd = 2, lty = c(rep(1, length(surveys))),
             legend = c(surveys,
                        "Betabinomial"),
             cex = 0.6)
      
      
    }
    
    
  }
}
dev.off()

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod,
           '_admin2Bench_noStrata_spaghetti_6per.pdf'),
    height = 9, width = 6)
{
  par(mfrow = c(3,2),lend=1)
  area.idx <- 0
  for(area in admin2.names$Internal){
    area.idx <- area.idx + 1
    tmp.area <- res.admin2$overall[res.admin2$overall$region == area,]
    tmp.area$width <- tmp.area$upper - tmp.area$lower
    tmp.area$cex2 <- median(tmp.area$width, na.rm = T)/tmp.area$width
    tmp.area$cex2[tmp.area$cex2 > 6] <- 6
    tmp.ihme <- ihme.ests[[3]][ihme.ests[[3]]$ADM2_NAME ==
                                 as.character(admin2.names$GADM[area.idx]) &
                                 ihme.ests[[3]]$ADM1_NAME == as.character(poly.adm2@data$NAME_1[area.idx]),]
    tmp.ihme[ ,c("mean", "lower", "upper")] <- 
      tmp.ihme[ ,c("mean", "lower", "upper")]*1000
    # print(tmp.ihme)
    cols <- rainbow(length(surveys)+1+1)
    plot.years <- beg.year:end.year
    tmp.area[,c("median", "lower","upper")] <-
      tmp.area[,c("median", "lower","upper")]*1000
  
    pane.years <- (seq(beg.year+2, end.year-2, 5))
    est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
    
    
    if(dim(tmp.area)[1] != 0 & !(sum(is.na(tmp.area$mean)) == nrow(tmp.area))){
      plot.max <- max(c(tmp.area$upper,
                        direct.admin2$upper[direct.admin2$region == as.character(area)]),
                      na.rm = T) + 25
    }
    
    if (nrow(tmp.area) >0 & sum(is.na(tmp.area$mean)) == nrow(tmp.area)) {
      plot(NA,
           xlab = "Year", ylab = "U5MR",
           ylim = c(0, plot.max),
           xlim = c(beg.year, end.year + 1),
           type = 'l', col = cols[1], lwd = 2,
           main = paste(admin2.names$GADM[area.idx],
                        poly.adm2$NAME_1[area.idx], 
                        sep = ", "))
      legend('topright', bty = 'n', col = c(cols, 'black'),
             lwd = 2, lty = 1, legend = c(surveys, "Betabinomial"))
      
    } else {
      
      for(survey in surveys){
        tmp <- direct.admin2[direct.admin2$surveyYears == survey &
                               direct.admin2$region ==
                               as.character(admin2.names$Internal[area.idx]),]
        svy.idx <- match(survey, surveys) 
        
        
        if(svy.idx== 1){
          if(dim(tmp)[1] != 0){
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main = paste(admin2.names$GADM[area.idx],
                              poly.adm2$NAME_1[area.idx], 
                              sep = ", "))
            pane.years <- (seq(beg.year+2, end.year-2,5))
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  lwd = 2)
            
            polygon(x = c(pane.years[!is.na(tmp$upper)],
                          rev(pane.years[!is.na(tmp$upper)])),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
            
            
            #lines(ihme.years, tmp.ihme$lower, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            #lines(ihme.years, tmp.ihme$upper, lwd = 1, lty = 2, col  = cols[length(surveys)+1])
            
            # for(year.id in 1:length(periods)){
            #   segments(pane.years[year.id], tmp$upper[year.id],
            #            pane.years[year.id], tmp$lower[year.id],
            #            col = cols[svy.idx])
            # }
          }else{
            plot(NA,
                 xlab = "Year", ylab = "U5MR",
                 ylim = c(0, plot.max),
                 xlim = c(beg.year, end.year),
                 type = 'l', col = cols[svy.idx], lwd = 2,
                 main =  paste(admin2.names$GADM[area.idx],
                               poly.adm2$NAME_1[area.idx], 
                               sep = ", "))
          }
        }else{
          if(dim(tmp)[1] != 0){
            lines(pane.years, tmp$mean, cex = tmp$cex2,
                  type = 'l', col = cols[svy.idx],
                  lwd = 2)
            polygon(x = c(pane.years[!is.na(tmp$upper)],
                          rev(pane.years[!is.na(tmp$upper)])),
                    y = c(tmp$upper[!is.na(tmp$upper)],
                          rev(tmp$lower[!is.na(tmp$lower)])),
                    col = alpha('grey85', 0.25), 
                    border = FALSE)
            
            points(pane.years, tmp$mean, pch = 19,
                   col = alpha(cols[svy.idx], 0.35),
                   cex = tmp$cex2)
          } 
        }
        
        
      }
      
      lines(tmp.ihme$year, 
            tmp.ihme$mean, lwd = 2,
            lty = 1, 
            col  = cols[length(surveys)+1])
      polygon(c(tmp.ihme$year,
                rev(tmp.ihme$year)),
              c(tmp.ihme$upper,
                rev(tmp.ihme$lower)),
              col = alpha(cols[length(surveys)+1], 0.25),
              border = FALSE)
      
      pane.years <- (seq(beg.year+2, end.year-2, 5))
      est.ids <- which(seq(beg.year,end.year-5,5) < max(mod.dat$years))
      
      
      lines(tmp.area$years.num[1:length(beg.year:max(mod.dat$years))], 
            tmp.area$median[1:length(beg.year:max(mod.dat$years))],
            col = 'black',
            lwd = 2, lty = 1)
      lines(tmp.area$years.num[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            tmp.area$median[(length(beg.year:max(mod.dat$years))+1):length(beg.year:end.year)], 
            col = 'black', 
            lwd = 2, lty = 2)
      polygon(x = c(beg.year:end.year,
                    rev(beg.year:end.year)),
              y = c(tmp.area$upper,
                    rev(tmp.area$lower)),
              col = alpha('black', 0.25),
              border = FALSE)
      legend('topright', bty = 'n', col = c(cols[1:(length(surveys) +1)], 'black'),
             lwd = 2, lty = c(rep(1, length(surveys)+2)),
             legend = c(surveys,'IHME', "Betabinomial"),
             cex = 0.6)
      
      
    }
    
    
  }
}
dev.off()

pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', time.mod, '_admin2Bench_coefs.pdf'), height = 5, width = 5)
{
  par(mfrow =c(1,1),lend=1)
  age.cols <- rainbow(length(ages))
  age.cols[2] <- "orange"
  for(age in ages){
    age.idx <- grepl(paste0(age),temporals$group)
    fixed.idx <- grepl(paste0(age,"$"), row.names(fixed.eff.table))
    fixed.eff <- fixed.eff.table[fixed.idx,]
    tmp<- temporals[age.idx,]
    
    if(match(age, ages) == 1 ){
      plot.min <- min(fixed.eff.table$`0.025quant`) - 0.025
      plot.max <- max(fixed.eff.table$`0.975quant`) + 0.025
      #lim <- max(abs(plot.min),abs(plot.max))
      
      plot(NA, xlim = c(0,7), xaxt = 'n',
           ylim = c(plot.min, plot.max),
           xlab = "Year",
           ylab = "Effect size",
           main = country)
    }
    
    points(match(age,ages), fixed.eff$`0.5quant`,
           pch = 19, col = age.cols[match(age, ages)])
    segments(match(age,ages), fixed.eff$`0.025quant`,
             match(age,ages), fixed.eff$`0.975quant`,
             col = age.cols[match(age, ages)], lty = 1)
    
  }
  
  legend('bottomleft', cex = 0.7, lty = 1,
         col = c(age.cols), pch =19,
         legend = c(ages), bty = 'n')
}
dev.off()

if (grepl("randomSlopes",time.mod)) {
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_', time.mod, '_admin2Bench_randomSlopes.pdf'), height = 5, width = 5)
  years.std <- ((beg.year:end.year)-mean(beg.year:end.year))/sd(beg.year:end.year)
  # plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.025quant","0.975quant")],
  #                       years.std[length(years.std)]*posteriorRandomSlopes[,c("0.025quant","0.975quant")]))
  plot.range <- range(c(years.std[1]*posteriorRandomSlopes[,c("0.5quant")],
                        years.std[length(years.std)]*posteriorRandomSlopes[,c("0.5quant")]))
  par(lend=1)
  plot(NA,xlim=range(years.std),ylim = plot.range,
       ylab = "effect",xlab="year (standardized)")
  
  region.cols <- rainbow(nrow(posteriorRandomSlopes))
  for (i in 1:nrow(posteriorRandomSlopes)) {
    abline(0,posteriorRandomSlopes$`0.5quant`[i],col=region.cols[i])
    # polygon(x=c(years.std,rev(years.std)),
    #         y=c(years.std*posteriorRandomSlopes$`0.975quant`[i],rev(years.std*posteriorRandomSlopes$`0.025quant`[i])),
    #         col=alpha(region.cols[i],0.25),border = NA)
  }
  legend("top",legend=admin1.names[,1],col=region.cols,lty=1,ncol=ceiling(nrow(posteriorRandomSlopes)/2),cex=0.5)
  dev.off()
}


#### Spaghetti plots for all regions over time ####

# change the region names from Internal to GADM for legend labels
res.admin2$overall$region1.gadm <- 
  res.admin2$overall$region.gadm <- 
  res.admin2$overall$region.orig <- 
  res.admin2$overall$region
for (i in 1:nrow(admin2.names)) {
  res.admin2$overall$region[as.character(res.admin2$overall$region) == 
                              as.character(admin2.names$Internal[i])] <- paste(as.character(admin2.names$GADM[i]),
                                                                               as.character(poly.adm2$NAME_1[i]), sep =", ")
  res.admin2$overall$region.gadm[as.character(res.admin2$overall$region.orig) == 
                                   as.character(admin2.names$Internal[i])] <- paste(as.character(admin2.names$GADM[i]))
  res.admin2$overall$region1.gadm[as.character(res.admin2$overall$region.orig) == 
                                    as.character(admin2.names$Internal[i])] <- paste(as.character(poly.adm2$NAME_1[i]))
  
}

# make the plot ordered by magnitude of u5mr in 2020

# numberAreasPerPage <- 15
numberAreasTotal <- nrow(admin2.names)
# numberPages <- ceiling(numberAreasTotal/numberAreasPerPage)
numberPages <- nrow(admin1.names)
# order data by median magnitude in 2020
res.admin2.order <- res.admin2$overall %>% 
  filter(years.num == 2020) %>%
  arrange(region.gadm)
areaOrder <- res.admin2.order$region.orig

# loop and make plots
for (i in 1:numberPages) {
  
  if (i != numberPages) {
    #areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
    areas <- res.admin2.order$region.orig[res.admin2.order$region1.gadm ==
                                            as.character(admin1.names$GADM[i])]
  } else {
    #areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
    areas <- res.admin2.order$region.orig[res.admin2.order$region1.gadm ==
                                            as.character(admin1.names$GADM[i])]
  }
  tmp <- res.admin2$overall[res.admin2$overall$region.orig %in% areas,]
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_',
             time.mod, 
             '_admin2Bench_spaghetti_allByAdmin1_',
             i,'.pdf'),
      height = 6, width = 6)
  {
    par(lend=1)
    
    g <- ggplot(tmp, aes(x = years.num,
                         y = median*1000,
                         col = region.gadm)) +
      geom_line() +
      geom_point() +
      theme_light() +
      xlab("Year") +
      ylab("U5MR: deaths per 1000 live births") +
      ggtitle(paste0(unique(tmp$region1.gadm))) +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 8)) +
      ylim(c(0, max(res.admin2$overall$median)*1000))
    g <- g + 
      guides(col = guide_legend(ncol=3,
                                title = "Admin 2")) 
    print(g)
    dev.off()
  }
}

numberAreasPerPage <- 30
numberAreasTotal <- nrow(admin2.names)
numberPages <- ceiling(numberAreasTotal/numberAreasPerPage)
# order data by median magnitude in 2020
res.admin2.order <- res.admin2$overall %>% 
  filter(years.num == 2019) %>%
  arrange(desc(median))
areaOrder <- res.admin2.order$region.orig

# loop and make plots
for (i in 1:numberPages) {
  
  if (i != numberPages) {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
    # areas <- res.admin2.order$region.orig[res.admin2.order$region1.gadm ==
    #                                         as.character(admin1.names$GADM[i])]
  } else {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
    # areas <- res.admin2.order$region.orig[res.admin2.order$region1.gadm ==
    #                                         as.character(admin1.names$GADM[i])]
  }
  tmp <- res.admin2$overall[res.admin2$overall$region.orig %in% areas,]
  
  pdf(paste0(folder.name, '/Plots/Betabinomial/',
             country, '_',
             time.mod, 
             '_admin2Bench_spaghetti_allByMedian_',
             i,'.pdf'),
      height = 6, width = 6)
  {
    par(lend=1)
    
    print(ggplot(tmp, aes(x = years.num,
                          y = median*1000,
                          col = region.gadm)) +
            geom_line() +
            geom_point() +
            theme_light() +
            xlab("Year") +
            ylab("U5MR: deaths per 1000 live births") +
            ggtitle(country) +
            theme(legend.position = "bottom",
                  legend.text = element_text(size = 8)) +
            guides(col = guide_legend(ncol=3,
                                      title = "Admin 2")) +
            ylim(c(0, max(res.admin2$overall$median)*1000)))
    dev.off()
  }
}



# make the plot ordered by admin1/admin2
pdf(paste0(folder.name, '/Plots/Betabinomial/',
           country, '_', 
           time.mod,
           '_admin2Bench_spaghetti_all_GADMorder.pdf'))
par(lend=1)

# order data by GADM
areaOrder <- admin2.names$Internal

# change the order 
# loop and make plots
for (i in 1:numberPages) {
  if (i != numberPages) {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):(i*numberAreasPerPage)]
  } else {
    areas <- areaOrder[(((i-1)*numberAreasPerPage)+1):numberAreasTotal]
  }
  tmp <- res.admin2$overall[res.admin2$overall$region.orig %in% areas,]
  
  print(ggplot(tmp, aes(x = years.num, y = median*1000, col = region)) +
          geom_line() +
          geom_point() +
          theme_light() +
          xlab("Year") +
          ylab("U5MR: deaths per 1000 live births") +
          ggtitle(country) +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 8)) +
          guides(col = guide_legend(ncol=3,
                                    title = "Admin 2")) +
          ylim(c(0, max(res.admin2$overall$median)*1000)))
}

dev.off()
