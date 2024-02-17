library(rJava)
library(tabulizer)
library(tidyverse)

pdf.file <- "/Users/alanamcgovern/Desktop/Kenya 2022 DHS Appendix.pdf"

## load in total HH and population
pdf.tables <- extract_tables(pdf.file, output='data.frame')
frame.info <- pdf.tables[[1]][-c(1:5,53:54),1:2]

#separate first variable
frame.info$Table.A.1..Population.and.household.distribution <- str_replace_all(frame.info$Table.A.1..Population.and.household.distribution,' na',' \na')
messy_var <- str_split(frame.info$Table.A.1..Population.and.household.distribution,' ')
admin.name.ids <- lapply(messy_var, function(x){str_detect(x,'[A-Z]') & !str_detect(x,'\na')})
frame.info$admin1.name <- sapply(1:length(messy_var), function(k){paste(messy_var[[k]][admin.name.ids[[k]]],collapse = ' ')})
value.ids <- lapply(messy_var, function(x){str_detect(x,'[0-9]') | str_detect(x,'\na')})
frame.info$values <- sapply(1:length(messy_var), function(k){paste(messy_var[[k]][value.ids[[k]]],collapse = ' ')})

#separate second variable
frame.info$X <- str_replace(frame.info$X,'   ',' ')
frame.info$X <- str_replace(frame.info$X,'  ',' ')
frame.info <- frame.info %>% separate(X,c('Rural_HH','Urban_HH'),sep=' ') %>% dplyr::select(-Table.A.1..Population.and.household.distribution) %>%
  separate(values,c('Total_pop','Rural_pop','Urban_pop','Total_HH'),sep=' ')

#clean up
frame.info <- frame.info[,c('admin1.name','Urban_pop','Rural_pop','Total_pop','Urban_HH','Rural_HH','Total_HH')]
for(j in 2:ncol(frame.info)){
  frame.info[frame.info[,j]%in% c('\na','na'),j] <- 0
  frame.info[,j] <- str_remove_all(frame.info[,j],',')
}
  
