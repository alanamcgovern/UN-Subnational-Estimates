rm(list = ls())
## ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- 'Malawi'

## Libraries -----------------------------------------------

options(gsubfn.engine = "R")
library(rgdal)
library(SUMMER)
library(ggplot2)

library(classInt)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(scales)
library(INLA)
library(survey)
library(raster)
library(maptools)
library(gridExtra)
library(mgcv)
library(caret)
library(geosphere)
library(rgeos)
library(haven)
library(labelled)
library(data.table)
library(sqldf)
library(sp)
library(gstat)
library(ggridges)

## Retrieve directories and country info -----------------------------------------------
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/")
data.dir <- paste0(home.dir,'/Data/',country) # set the directory to store the data
res.dir <- paste0(home.dir,'/Results/',country) # set the directory to store the results (e.g. fitted R objects, figures, tables in .csv etc.)
load(file = paste0(home.dir,'/Info/', country, "_general_info.Rdata", sep=''))

## Load polygon files -----------------------------------------------
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


## Load BB8 results -----------------------------------------------
setwd(res.dir)

# include benchmarked estimates later
load( paste0('Betabinomial/U5MR/', country,'_res_natl_strat_u5.rda'))
load( paste0('Betabinomial/U5MR/', country,'_res_natl_unstrat_u5.rda'))
load( paste0('Betabinomial/U5MR/', country,'_res_adm1_strat_u5.rda'))
load( paste0('Betabinomial/U5MR/', country,'_res_adm1_unstrat_u5.rda'))
load( paste0('Betabinomial/U5MR/', country,'_res_adm2_strat_u5.rda'))
load( paste0('Betabinomial/U5MR/', country,'_res_adm2_unstrat_u5.rda'))

load( paste0('Betabinomial/NMR/', country,'_res_natl_strat_nmr.rda'))
load( paste0('Betabinomial/NMR/', country,'_res_natl_unstrat_nmr.rda'))
load( paste0('Betabinomial/NMR/', country,'_res_adm1_strat_nmr.rda'))
load( paste0('Betabinomial/NMR/', country,'_res_adm1_unstrat_nmr.rda'))
load( paste0('Betabinomial/NMR/', country,'_res_adm2_strat_nmr.rda'))
load( paste0('Betabinomial/NMR/', country,'_res_adm2_unstrat_nmr.rda'))


if(!dir.exists(paths = paste0('Betabinomial/Postsamp'))){
  dir.create(path = paste0('Betabinomial/Postsamp'))}
  
if(!dir.exists(paths = paste0('Figures/Betabinomial/'))){
  dir.create(path = paste0('Figures/Betabinomial/'))}

## Figure 1a : admin-2 U5MR map for latest year -----------------------------------------------
# We plot the U5MR estimates of the latest year fitted by beta-binomial model at admin2 level on the map of the given country.

# prepare results
res.admin2_overall<-res.admin2.strat$overall
admin2_res_merged<-merge(res.admin2_overall, admin2.names, 
                         by.x=c("region"),
                         by.y=c("Internal"))
admin2_res_merged$region<-admin2_res_merged$GADM
admin2_res_merged$width <- admin2_res_merged$upper- admin2_res_merged$lower
class(admin2_res_merged) <- class(res.admin2.strat$overall)


# admin2 level maps, last year
last_year <- subset(admin2_res_merged, years == max(end.years))
g1a <- mapPlot(last_year, geo = poly.adm2, by.data = "region", by.geo = sub(".*data[$]","",poly.label.adm2), variable = "median", 
               is.long=FALSE, per1000=TRUE, removetab=TRUE, legend.label = "U5MR", direction = -1, 
               size= 0.1)

g1a <- g1a+
  theme(legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),legend.text=element_text(size=16),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=18))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                title='U5MR (deaths per 1000 live births)',
                                label.position = "bottom"))

#ggsave(g1a, device='tiff',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "_", max(end.years), "_admin2_map.tiff"))
ggsave(g1a, device='pdf',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "_", max(end.years), "_admin2_map.pdf"))

## Figure 1b : admin-2 U5MR CI width map for latest year -----------------------------------------------

# We plot the width of credible of our U5MR estimates of the latest year fitted by beta-binomial model at admin2 level on the map of the given country.

g1b <- mapPlot(last_year, geo = poly.adm2, by.data = "region", by.geo = sub(".*data[$]","",poly.label.adm2), variable = "width",
               is.long=FALSE, per1000=TRUE,  removetab=TRUE,legend.label = "Width of 95% CI", direction = -1,
               size= 0.1)

g1b <- g1b + scale_fill_viridis_c("Width of 95% CI",option="B",direction=-1)+
  theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),legend.text=element_text(size=16),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=18))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                label.position = "bottom"))


#ggsave(g1b, device='tiff',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "_", max(end.years), "-wid-95CI-admin2-map.tiff"))
ggsave(g1b, device='pdf',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "_", max(end.years), "-wid-95CI-admin2-map.pdf"))

## Figure 1c : admin-2 U5MR map for each year -----------------------------------------------

# We plot the U5MR estimates of the each year fitted by beta-binomial model at admin2 level on the map of the given country.

g1c <- mapPlot(admin2_res_merged, geo = poly.adm2, by.data = "region", by.geo = sub(".*data[$]","",poly.label.adm2), variable = "years",
               values = "median", is.long=TRUE, per1000=TRUE, legend.label = "U5MR", direction = -1, ncol = 3, 
               size= 0.1) +
  theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),legend.text=element_text(size=15),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=15),
         strip.text = element_text(size = 14))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                title='U5MR (deaths per 1000 live births)',
                                label.position = "bottom"))

#ggsave(g1c, device='tiff',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "-all-years-admin2-map.tiff"))
ggsave(g1c, device='pdf',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "-all-years-admin2-map.pdf"))


## Figure 1d : admin-2 U5MR CI width map for each year -----------------------------------------------

# We plot the width of credible of our U5MR estimates for all years fitted by beta-binomial model at admin2 level on the map of the given country.

g1d <- mapPlot(admin2_res_merged, geo = poly.adm2, by.data = "region", by.geo = sub(".*data[$]","",poly.label.adm2), variable = "years",
               values = "width", is.long=TRUE, per1000=TRUE, legend.label = "Width of 95% CI", 
               direction = -1, ncol = 3)+
  theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),legend.text=element_text(size=15),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=15),
         strip.text = element_text(size = 14))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                label.position = "bottom"))



#ggsave(g1d, device='tiff',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "-all-years-wid-95CI-admin2-map.tiff"))
ggsave(g1d, device='pdf',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "-all-years-wid-95CI-admin2-map.pdf"))

## Figure 1e : admin-1 U5MR map for each year -----------------------------------------------

# We plot our U5MR estimates for all years fitted by beta-binomial model at admin1 level on the map of the given country.

# prepare results
res.admin1_overall<-res.admin1.strat$overall
admin1_res_merged<-merge(res.admin1_overall, admin1.names, 
                         by.x=c("region"),
                         by.y=c("Internal"))
admin1_res_merged$region<-admin1_res_merged$GADM
admin1_res_merged$width <- admin1_res_merged$upper- admin1_res_merged$lower
class(admin1_res_merged) <- class(res.admin1.strat$overall)


g1e <- mapPlot(admin1_res_merged, geo = poly.adm1, by.data = "region", by.geo = sub(".*data[$]","",poly.label.adm1), variable = "years",
               values = "median", is.long=TRUE, per1000=TRUE, legend.label = "U5MR", direction = -1, ncol = 3, 
               size= 0.1) +
  theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),legend.text=element_text(size=15),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=15),
         strip.text = element_text(size = 14))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                title='U5MR (deaths per 1000 live births)',
                                label.position = "bottom"))

#ggsave(g1e, device='tiff',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "-all-years-admin1-map.tiff"))
ggsave(g1e, device='pdf',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "-all-years-admin1-map.pdf"))

## Figure 1f : admin-1 U5MR CI width map for each year -----------------------------------------------

# We plot the width of credible of our U5MR estimates for all years fitted by beta-binomial model at admin1 level on the map of the given country.

g1f <- mapPlot(admin1_res_merged, geo = poly.adm1, by.data = "region", by.geo = sub(".*data[$]","",poly.label.adm1), variable = "years",
               values = "width", is.long=TRUE, per1000=TRUE, legend.label = "Width of 95% CI", 
               direction = -1, ncol = 3)+
  theme (legend.position = 'bottom',legend.key.height=unit(0.5,'cm'),legend.text=element_text(size=15),
         legend.key.width = unit(2,'cm'),legend.title = element_text(size=15),
         strip.text = element_text(size = 14))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = .5,
                                label.position = "bottom"))

#ggsave(g1f, device='tiff',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "_", max(end.years), "-all-years-wid-95CI-admin1-map.tiff"))
ggsave(g1f, device='pdf',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "-all-years-wid-95CI-admin1-map.pdf"))


## Figure 2 : admin-1 U5MR trend plot -----------------------------------------------

# We plot our U5MR estimates versus years fitted by beta-binomial model at admin1 level.

# prepare results
res.admin1_overall<-res.admin1.strat$overall
admin1_res_merged<-merge(res.admin1_overall, admin1.names, 
                         by.x=c("region"),
                         by.y=c("Internal"))
admin1_res_merged$region<-admin1_res_merged$GADM
admin1_res_merged$width <- admin1_res_merged$upper- admin1_res_merged$lower
class(admin1_res_merged) <- class(res.admin1.strat$overall)

### Admin 1 level trend plot
g2 <- plot(admin1_res_merged, plot.CI = TRUE, dodge.width = 0.5, proj_year = max(end.years) + 1, per1000=TRUE) +
  theme(legend.position = 'bottom') + scale_linetype(guide='none')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(beg.year:max(end.years)))+
  ylab('Deaths per 1000 live births')+
  theme(text = element_text(size=16))

#ggsave(g2, device='tiff',width=8, height = 10, file = paste0("Figures/Betabinomial/", country,"-trends-admin1.tiff"))
ggsave(g2, device='pdf',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "-trends-admin1.pdf"))

## Figure 3 : admin-2 U5MR trend plot, no legends -----------------------------------------------

# We plot our U5MR estimates versus years fitted by beta-binomial model at admin2 level.

range_adm2 <- range(c(admin2_res_merged$median, admin2_res_merged$median)) * 1000
admin2_trend_plot <- admin2_res_merged
admin2_trend_plot$region <- res.admin2.strat$overall$region
g3 <- plot(admin2_trend_plot, plot.CI = FALSE, dodge.width = 0.5, proj_year = max(end.years) + 1, per1000=TRUE) +
  theme(legend.position = 'none') + scale_linetype(guide='none')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(beg.year:max(end.years)))+
  ylim(range_adm2)+
  ylab('Deaths per 1000 live births')+
  theme(text = element_text(size=16))

#ggsave(g3, device='tiff',width=8, height = 10, file = paste0("Figures/Betabinomial/", country,"-trends-admin2.tiff"))
ggsave(g3, device='pdf',width=8, height = 10, file = paste0("Figures/Betabinomial/", country, "-trends-admin2.pdf"))




