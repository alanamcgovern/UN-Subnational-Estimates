## this script evaluates over/undersampling of urban households

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

setwd(paste(code.path.splitted[1: (length(code.path.splitted)-1)], collapse = "/"))
library(openxlsx)
library(tidyverse)

## specify filepath
filenames <- c('Angola/ang_urbhh_2015-16.xlsx', # add 2011 MIS
               'Bangladesh/bgd_urbhh_2011.xlsx','Bangladesh/bgd_urbhh_2014.xlsx','Bangladesh/bgd_urbhh_2017-18.xlsx', #add 2019 MICS
               'Benin/ben_urbhh_2011-12.xlsx', 'Benin/ben_urbhh_2017-18.xlsx', 
               'Burkina_Faso/bfa_urbhh_2010.xlsx',
               'Ethiopia/eth_urbhh_2011.xlsx','Ethiopia/eth_urbhh_2016.xlsx','Ethiopia/eth_urbhh_2019.xlsx')

get_URdiff <- function(file){
  tab <- read.xlsx(paste0(file))
  tab <- tab %>% mutate(diff = sample_prop-frame_prop,perc_diff = (sample_prop-frame_prop)/frame_prop)
  if(sum(is.na(tab$f_urban) + is.na(tab$f_total) + is.na(tab$s_urban) + is.na(tab$s_total))==0){
    tab <- tab %>% mutate(chisq = ifelse(frame_prop>0 & frame_prop<1, (s_urban-(s_total*f_urban/f_total))^2/(s_total*frame_prop*(1-frame_prop)),0))
  }else{tab <- tab %>% mutate(chisq = NA)}
                        
  res <- c(nrow(tab),sum(tab$chisq),1-pchisq(sum(tab$chisq),nrow(tab)-1),median(tab$diff),tab$diff[which.max(abs(tab$diff))],median(tab$perc_diff),tab$perc_diff[which.max(abs(tab$perc_diff))])
  plot <- tab %>% ggplot() + geom_histogram(aes(x=diff)) + ggtitle(paste0(file))
  return(list(round(res,3),plot))
}

plot_list <- list()
comparisons <- get_URdiff(filenames[1])[[1]]
plot_list[[1]] <- get_URdiff(filenames[1])[[2]]
for(i in 2:length(filenames)){
  comparisons <- rbind(comparisons,get_URdiff(filenames[i])[[1]])
  plot_list[[i]] <- get_URdiff(filenames[i])[[2]]
}

row.names(comparisons) <- filenames
colnames(comparisons) <- c('NumberofAreas','Chi-Square','p','MedianDiff','MaxDiff','MedianPercDiff','MaxPercDiff')

comparisons

par(mfrow=c(3,3))
plot_list
