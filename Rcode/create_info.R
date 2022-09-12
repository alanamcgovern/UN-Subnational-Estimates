##################################################################
##################################################################
# This script is used to generate the info file for a given country
##################################################################
##################################################################
rm(list = ls())

################################################################
#########   set parameters
################################################################

# Files info (For those lines with ### xxx ### above, please fill in as commented)
country <- 'Malawi'

### please fill in the country abbreviation in all upper case of gadm files ### (e.g. fill in SEN for gadm36_SEN_3.shp)
gadm.abbrev <- "MWI"
doHIVAdj <- T

### please fill in the name of the folder containing the DHS data and the name of the DHS data file inside, separated by "/" ###
dhsStata.files<-c("MWBR61DT/MWBR61FL.dta",'MWBR7ADT/MWBR7AFL.dta')

### please fill in the file name containing the DHS GPS data ###
dhsFlat.files<-c('MWGE62FL','MWGE7AFL')

### please fill in the following information ####
dhs_survey_years<-c(2010,2015) # years of the DHS surveys
survey_years <- c(2010,2014,2015,2020)
frame_years <- c(2008, 2008, 2008, 2008)

### please fill in the path to country shape files (give more explanation on this later) ####
poly.path <- paste0("shapeFiles_gadm")

## save url for frame year census
census.urls <- c('http://www.nsomalawi.mw/index.php?option=com_content&view=article&id=107%3A2008-population-and-housing-census-results&catid=8%3Areports&Itemid=1')

##### explain how these may need to be changed
poly.layer.adm0 <- paste('gadm36', gadm.abbrev,
                         '0', sep = "_") # specify the name of the national shape file
poly.layer.adm1 <- 'sdr_subnational_boundaries' # specify the name of the admin1 shape file
poly.layer.adm2 <- paste('gadm36', gadm.abbrev,
                         '1', sep = "_") # specify the name of the admin2 shape file


poly.label.adm1 <- "poly.adm1@data$DHSREGEN"
poly.label.adm2 <- "poly.adm2@data$NAME_1"

##################################################################
##################################################################
##################################################################

## setting rest of parameters using info from above
country.abbrev <- tolower(gadm.abbrev)           # lower the country gadm abbreviation 
beg.year <- 2000 # the first year of the interest
end.proj.year <- 2020 # last year we would like to project to 

info.name <- paste0(country, "_general_info.Rdata")

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

save.image(file = paste0(paste(code.path.splitted[1: (length(code.path.splitted)-2)], collapse = "/"),'/Info/', info.name, sep=''))

