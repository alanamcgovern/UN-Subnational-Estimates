
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

hiv.adj.new <- full_join(factors,hiv.adj,by = c('country','area','survey','years','year'))
hiv.adj.new[is.na(hiv.adj.new$ratio.x),]$ratio.x <- hiv.adj.new[is.na(hiv.adj.new$ratio.x),]$ratio.y
hiv.adj.new <- hiv.adj.new[,c('country','area','survey','years','ratio.x','year')]
colnames(hiv.adj.new) <- c('country','area','survey','years','ratio','year')
hiv.adj <- hiv.adj.new

# save
save(hiv.adj, file = "HIVAdjustments.rda")

