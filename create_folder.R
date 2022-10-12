rm(list = ls()) # clear the R environment and prepare for the pipeline

# ENTER COUNTRY OF INTEREST -----------------------------------------------
# Please capitalize the first letter of the country name and replace " " in the country name to "_" if there is.
country <- "Mauritania"

# set directory -----------------------------------------------
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve user-specified directory
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-1)], collapse = "/")
info.name <- paste0(country, "_general_info.Rdata")
load(file = paste0(home.dir,'/Info/', info.name, sep='')) # load the country info

# set the directory, which is the user-specified folder
setwd(home.dir)

# create folders for data -----------------------------------------------

# create a folder called Data to store all required data for each country including DHS survey data, country shape files etc. No actions if the folder is already there.
if(!dir.exists(paths = paste0('Data'))){ 
  dir.create(path = paste0('Data'))
}

# create a folder for the user-specified country inside the Data folder
if(!dir.exists(paths=paste0('Data/',country))){
  dir.create(path = paste0('Data/',country))}

## create sub-folders within the country data folder for:
# worldpop: This folder contains all the under-five population fraction of each admin2 region for each year as R objects.
if(!dir.exists(paths = paste0('Data/',country, '/worldpop'))){
  dir.create(path = paste0('Data/',country, '/worldpop'))}
# Population: This folder contains the under-five population raster objects (i.e. population surface) of each year at the resolution of 100m and 1km.
if(!dir.exists(paths = paste0('Data/',country, '/Population'))){
  dir.create(path = paste0('Data/',country, '/Population'))}
# create a folder for shapefiles
if(!dir.exists(paths = paste0('Data/',country, '/shapeFiles'))){
  dir.create(path = paste0('Data/',country, '/shapeFiles'))}
  

# create a folder for Results to store all the results for each country -----------------------------------------------
# including the fitted R models, figures and tables in .csv etc. No actions if the folder is already there.
if(!dir.exists(paths = paste0('Results'))){ 
  dir.create(path = paste0('Results'))
}

# create a sub-folders for the country in the folder of Results to store the results.
if(!dir.exists(paths = paste0('Results/',country))){
  dir.create(path = paste0('Results/',country))}
res.dir <- paste0(home.dir,'/Results/',country) # set as results directory

# subfolders for NMR and U5MR BB estimates
if(!dir.exists(paths = paste0(res.dir,'/Betabinomial/'))){
  dir.create(path = paste0(res.dir,'/Betabinomial/'))}
if(!dir.exists(paths = paste0(res.dir,'/Betabinomial/U5MR'))){
  dir.create(path = paste0(res.dir,'/Betabinomial/U5MR'))}
if(!dir.exists(paths = paste0(res.dir,'/Betabinomial/NMR'))){
  dir.create(path = paste0(res.dir,'/Betabinomial/NMR'))}

# subfolders for NMR and U5MR direct estimates
if(!dir.exists(paths = paste0(res.dir,'/Direct/'))){
  dir.create(path = paste0(res.dir,'/Direct/'))}
if(!dir.exists(paths = paste0(res.dir,'/Direct/U5MR'))){
  dir.create(path = paste0(res.dir,'/Direct/U5MR'))}
if(!dir.exists(paths = paste0(res.dir,'/Direct/NMR'))){
  dir.create(path = paste0(res.dir,'/Direct/NMR'))}

# subfolders for urban-rural fractions
if(!dir.exists(paths = paste0(res.dir,'/UR/'))){
  dir.create(path = paste0(res.dir,'/UR/'))}
if(!dir.exists(paths = paste0(res.dir,'/UR/Threshold/'))){
  dir.create(path = paste0(res.dir,'/UR/Threshold/'))}
if(!dir.exists(paths = paste0(res.dir,'/UR/U1_fraction/'))){
  dir.create(path = paste0(res.dir,'/UR/U1_fraction/'))}
if(!dir.exists(paths = paste0(res.dir,'/UR/U5_fraction/'))){
  dir.create(path = paste0(res.dir,'/UR/U5_fraction/'))}

# subfolders for figures
if(!dir.exists(paths = paste0(res.dir,'/Figures/'))){
  dir.create(path = paste0(res.dir,'/Figures/'))}

# for direct NMR and U5MR estimate figures
if(!dir.exists(paste0(res.dir,'/Figures/Direct'))){
  dir.create(paste0(res.dir,'/Figures/Direct'))}
if(!dir.exists(paste0(res.dir,'/Figures/Direct/U5MR'))){
  dir.create(paste0(res.dir,'/Figures/Direct/U5MR'))}
if(!dir.exists(paste0(res.dir,'/Figures/Direct/NMR'))){
  dir.create(paste0(res.dir,'/Figures/Direct/NMR'))}
if(!dir.exists(paste0(res.dir,'/Figures/Direct/U5MR/National'))){
  dir.create(paste0(res.dir,'/Figures/Direct/U5MR/National'))}
if(!dir.exists(paste0(res.dir,'/Figures/Direct/U5MR/Admin1'))){
  dir.create(paste0(res.dir,'/Figures/Direct/U5MR/Admin1'))}
if(!dir.exists(paste0(res.dir,'/Figures/Direct/U5MR/Admin2'))){
  dir.create(paste0(res.dir,'/Figures/Direct/U5MR/Admin2'))}
if(!dir.exists(paste0(res.dir,'/Figures/Direct/NMR/National'))){
  dir.create(paste0(res.dir,'/Figures/Direct/NMR/National'))}
if(!dir.exists(paste0(res.dir,'/Figures/Direct/NMR/Admin1'))){
  dir.create(paste0(res.dir,'/Figures/Direct/NMR/Admin1'))}
if(!dir.exists(paste0(res.dir,'/Figures/Direct/NMR/Admin2'))){
  dir.create(paste0(res.dir,'/Figures/Direct/NMR/Admin2'))}

# for smoothed direct NMR and U5MR estimate figures
if(!dir.exists(paste0(res.dir,'/Figures/SmoothedDirect'))){
  dir.create(paste0(res.dir,'/Figures/SmoothedDirect'))}
if(!dir.exists(paste0(res.dir,'/Figures/SmoothedDirect/U5MR'))){
  dir.create(paste0(res.dir,'/Figures/SmoothedDirect/U5MR'))}
if(!dir.exists(paste0(res.dir,'/Figures/SmoothedDirect/NMR'))){
  dir.create(paste0(res.dir,'/Figures/SmoothedDirect/NMR'))}





