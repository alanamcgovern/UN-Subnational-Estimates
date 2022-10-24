
# format U5MR HIV adjustment factors from UN

# Setup -------------------------------------------------------------------

library(tidyverse)
library(readxlsb)

# Note: set working directory or use R Project to set working directory
# as desired before proceeding to run this script

# In which folder are workbooks saved, within your working directory?
# (obtained from Dave Sharrow at UN 9/14/22)
folder <- "MM adjustment template 2022"

# Subnational filename formats
# When splitting filename by underscores, which section is the area name?
subnat_formats <- list(
  "Zimbabwe" = 4,
  "Mozambique" = 4,
  "Kenya" = 3
)

code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]
setwd(paste(code.path.splitted[1: (length(code.path.splitted)-1)], collapse = "/"))


# Formatting --------------------------------------------------------------

files <- list.files(folder, pattern = "Q5_", full.names = T, recursive = T)

# loop over files and format
factors <- list()
for (ff in files) {
  print(ff)
  
  # read in file
  country <- readxlsb::read_xlsb(ff, range = "U5MRAdjFac!A1:A2")[1,1]
  ff_short <- gsub(paste0(folder, "/"), "", ff)
  df <- readxlsb::read_xlsb(ff, sheet = "U5MRAdjFac", skip = 4)
  
  # format factors long, take reciprocal, convert years to integer form
  names(df)[1] <- "survey"
  df <- df %>%
    filter(!is.na(survey)) %>%
    mutate(survey = gsub("\\.5", "", as.character(survey))) %>%
    pivot_longer(!survey) %>%
    filter(!is.na(value)) %>%
    mutate(year = gsub("\\.5", "", gsub("X", "", name)),
           country = country,
           filename = ff_short,
           ratio = 1/value) %>%
    dplyr::select(filename, country, survey, year, ratio)
  
  # if subnational estimate, label area
  if (country %in% names(subnat_formats) &
      stringr::str_detect(ff_short, paste0(country, "/"), )) {
    area <- strsplit(ff_short, "_")[[1]][subnat_formats[[country]]]
    df$area <- area
    df$area <- str_replace(df$area,country,'')
  } else {
    df$area <- country
  }
  
  factors[[ff]] <- df
}
factors <- do.call(rbind, factors)

factors <- factors %>%
  mutate(years = year) %>%
  dplyr::select(country, area, survey, years, ratio, year)

factors <- data.frame(factors)
factors$survey <- as.double(factors$survey)
factors$years <- as.numeric(factors$years)
factors$year <- as.numeric(factors$year)

load('HIVAdjustments_Old.rda')

hiv.adj.new <- left_join(factors,hiv.adj,by = c('country','area','survey','years','year'))
colnames(hiv.adj.new) <- c('country','area','survey','years','ratio','year','old_ratio')
hiv.adj <- hiv.adj.new

sapply(unique(hiv.adj$country),function(x){unique(hiv.adj[hiv.adj$country==x,]$area)})

hiv.adj <- hiv.adj %>% filter(!(country=='Kenya' & area!='Kenya'),!(country=='Mozambique' & area=='Mozambique'),!(country=='Zimbabwe' & area=='Zimbabwe'),!(country=='Zambia' & area=='Zambia'),country!='Cote dIvoire') %>%
        dplyr::mutate(area=if_else(country=='Mozambique' & area=='Niassa','Nassa',
                                   if_else(country=='Mozambique' & area=='CaboDelgado',"Cabo Delgado",
                                    if_else(country=='Mozambique' & area=="Maputo Cidade","Maputo City",
                                    if_else(country=='Mozambique' & area=="Maputo Provincia",'Maputo',
                                    if_else(country=='Zimbabwe' & area=="MashonalandCentral", "Mashonaland Central",
                                    if_else(country=='Zimbabwe' & area=="MashonalandEast", "Mashonaland East",
                                    if_else(country=='Zimbabwe' & area=="MashonalandWest", "Mashonaland West",
                                    if_else(country=='Zimbabwe' & area=="MatabelelandNorth","Matabeleland North",
                                    if_else(country=='Zimbabwe' & area=="MatabelelandSouth", "Matabeleland South",
                                    if_else(country=='Zimbabwe' & area=="Harare Chitungwiza", "Harare",
                                    if_else(country=='Zambia' & area=="Northwestern", "North-Western",area)))))))))))) %>%
      dplyr::mutate(country=if_else(country=="United Republic of Tanzania",'Tanzania',country)) %>%
      dplyr::mutate(area=if_else(area=="United Republic of Tanzania",'Tanzania',area))
  
# create plots to compare old and new adjustments
pdf(file='HIV Adjustment Comparisons.pdf')
hiv.adj.compare <- hiv.adj %>% filter(!is.na(old_ratio))
for(country_t in unique(hiv.adj.compare$country)){
  print(hiv.adj.compare %>% filter(country==country_t & years>1999) %>% ggplot(aes(x=old_ratio,y=ratio)) + geom_point() + geom_abline(slope = 1,intercept = 0) + facet_wrap(vars(survey)) + ggtitle(paste0(country_t)))
}
dev.off()
# save
save(hiv.adj, file = "HIVAdjustments.rda")






