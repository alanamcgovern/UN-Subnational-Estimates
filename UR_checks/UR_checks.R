## this script evaluates over/undersampling of urban households

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

setwd(paste(code.path.splitted[1: (length(code.path.splitted)-1)], collapse = "/"))
library(openxlsx)
library(tidyverse)

## specify filepath
filenames <- c('Ethiopia/eth_urbhh_2011.xlsx','Ethiopia/eth_urbhh_2016.xlsx','Ethiopia/eth_urbhh_2019.xlsx',
               'Angola/ang_urbhh_2015-16.xlsx')

get_URdiff <- function(file){
  tab <- read.xlsx(paste0(file))
  tab <- tab %>% mutate(perc_diff = (sample_prop-frame_prop)/frame_prop,
                        chisq = ifelse(frame_prop>0 & frame_prop<1, (s_urban-(s_total*f_urban/f_total))^2/(s_total*frame_prop*(1-frame_prop)),0))
  
  res <- c(nrow(tab),sum(tab$chisq),1-pchisq(sum(tab$chisq),nrow(tab)-1),median(tab$perc_diff),tab$perc_diff[which.max(abs(tab$perc_diff))])
  return(round(res,3))
}

comparisons <- get_URdiff(filenames[1])
for(i in 2:length(filenames)){
  comparisons <- rbind(comparisons,get_URdiff(filenames[i]))
}

row.names(comparisons) <- filenames
colnames(comparisons) <- c('NumberofAreas','Chi-Square','p','MedianPercDiff','MaxPercDiff')

comparisons
