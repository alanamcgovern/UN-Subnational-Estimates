rm(list = ls())
library(stringdist)

#### ----------------------------------------------------------
#### ----------------------------------------------------------
# enter country being analyzed
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

setwd(data.dir)

load(paste0('shapeFiles_gadm/', country, '_Amat.rda'))  # load the adjacency matrix
load(paste0('shapeFiles_gadm/', country, '_Amat_Names.rda'))  # load names of admin1 and admin2 regions

###################################################################
###################################################################
#### format urban proportion table for sample frame
###################################################################
###################################################################

# read the txt file containing urban population fraction at admin1 level.
frame <- read.table(paste0(country.abbrev,'_frame_urb_prop.txt'))
# IF using number of household from DHS, multiply each column by average number of household in that area type (found in DHS)
######### Ex. from Burundi 2008 frame
######### frame$V2 <- frame$V2*5.2
######### frame$V3 <- frame$V3*4.7
######### frame$V4 <- frame$V4*4.8

# IF using info from a census or other DHS survey, check that that the admin1 names in your table and admin1.names (from the DHS data) 
# are the same (differences in spacing of names is fine)
sort(frame$V1)==sort(admin1.names$GADM)
sort(frame$V1)[sort(frame$V1)!=sort(admin1.names$GADM)]
sort(admin1.names$GADM)[sort(frame$V1)!=sort(admin1.names$GADM)]

# identify column for fraction -- index urban and total columns, in that order (need additional processing in general)
frame[,c(3,2)] <- lapply(frame[,c(3,2)],   ## function to remove comma in numbers
                         function(x){as.numeric(gsub(",", "", x))})
frame$frac <- frame[, 3]/frame[, 2]

# greedy algorithm to match admin names 
adm1.ref <- expand.grid(tolower(frame[, 1]),
                        tolower(admin1.names$GADM)) # Distance matrix in long form
names(adm1.ref) <- c("frame_name","gadm_name")
### string distance,  jw=jaro winkler distance, try 'dl' if not working
adm1.ref$dist <- stringdist(adm1.ref$frame_name,
                            adm1.ref$gadm_name, method="jw") 

greedyAssign <- function(a,b,d){
  x <- numeric(length(a)) # assgn variable: 0 for unassigned but assignable, 
  # 1 for already assigned, -1 for unassigned and unassignable
  while(any(x==0)){
    min_d <- min(d[x==0]) # identify closest pair, arbitrarily selecting 1st if multiple pairs
    a_sel <- a[d==min_d & x==0][1] 
    b_sel <- b[d==min_d & a == a_sel & x==0][1] 
    x[a==a_sel & b == b_sel] <- 1
    x[x==0 & (a==a_sel|b==b_sel)] <- -1
  }
  cbind(a=a[x==1],b=b[x==1],d=d[x==1])
}

match_order<-data.frame(greedyAssign(adm1.ref$frame_name,
                                     adm1.ref$gadm_name,
                                     adm1.ref$dist))

# create reference table 
ref.tab <- admin1.names
ref.tab$matched_name <- frame$V1[match_order$a] ### check!!!
ref.tab$urb_frac <- frame$frac[match_order$a] 

#fix estimates which are greater then 1 (only necessary when using household measures)
ref.tab$urb_frac[ref.tab$urb_frac>1] <- 1

###########################################################################
#### save reference table
###########################################################################
setwd(res.dir)
save(ref.tab,file='UR/urb_prop.rda')

