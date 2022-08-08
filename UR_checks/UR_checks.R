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
               'Burundi/bdi_urbhh_2010.xlsx', 'Burundi/bdi_urbhh_2016-17.xlsx',
               'Cameroon/cmr_urbhh_2011.xlsx',  'Cameroon/cmr_urbhh_2018.xlsx',
               'Chad/tcd_urbhh_2014-15.xlsx',
               'Cote_dIvoire/civ_urbhh_2011-12.xlsx',
               'DR_Congo/cod_urbhh_2013-14.xlsx',
               'Ethiopia/eth_urbhh_2011.xlsx','Ethiopia/eth_urbhh_2016.xlsx','Ethiopia/eth_urbhh_2019.xlsx',
               'Ghana/gha_urbhh_2014.xlsx',
               'Guinea/gin_urbhh_2012.xlsx','Guinea/gin_urbhh_2018.xlsx',
               'Haiti/hti_urbhh_2012.xlsx','Haiti/hti_urbhh_2016-17.xlsx',
               'India/ind_urbhh_2015-16.xlsx', # can't find frame/sample info for 2019-2021 survey
               'Kenya/ken_urbhh_2014.xlsx',
               'Lesotho/lso_urbhh_2014.xlsx', #add 2018 MICS  
               'Liberia/lbr_urbhh_2013.xlsx', 'Liberia/lbr_urbhh_2019-20.xlsx',
               # add MICS Madagascar
               'Malawi/mwi_urbhh_2015-16.xlsx', # can't find frame households for 2010, add 2013-14 and 2019-20 MICS
               'Mali/mli_urbhh_2012-13.xlsx','Mali/mli_urbhh_2018.xlsx',
               'Mauritania/mrt_urbhh_2019-21.xlsx',
               'Mozambique/moz_urbhh_2015.xlsx', # no adequate sample info for 2011
               'Myanmar/mmr_urbhh_2015-16.xlsx',
               'Nepal/npl_urbhh_2011.xlsx','Nepal/npl_urbhh_2016.xlsx',
               'Niger/ner_urbhh_2012.xlsx')
               # couldn't find sample frame for Nigeria (2010, 2013, 2018)

get_URdiff <- function(file){
  tab <- read.xlsx(paste0(file))
  tab <- tab %>% mutate(diff = sample_prop-frame_prop,perc_diff = (sample_prop-frame_prop)/frame_prop)
  if(sum(is.na(tab$f_urban) + is.na(tab$f_total) + is.na(tab$s_urban) + is.na(tab$s_total))==0){
    tab <- tab %>% mutate(chisq = ifelse(frame_prop>0 & frame_prop<1, (s_urban-(s_total*f_urban/f_total))^2/(s_total*frame_prop*(1-frame_prop)),0))
  }else{tab <- tab %>% mutate(chisq = NA)}
                        
  res <- c(nrow(tab),sum(tab$chisq),1-pchisq(sum(tab$chisq),nrow(tab)-1),median(tab$diff),tab$diff[which.max(abs(tab$diff))],median(tab$perc_diff),tab$perc_diff[which.max(abs(tab$perc_diff))])
  if(-min(tab$diff)>0.29){
    plot.min <- min(tab$diff) - 0.01
  }else{plot.min <- -0.3}
  if(max(tab$diff)>0.29){
    plot.max <- max(tab$diff) + 0.01
  }else{plot.max <- 0.3
  }
  plot <- tab %>% ggplot() + geom_point(aes(x=as.numeric(row.names(tab)),y=diff)) + ylab('Sample - Frame') + xlab('') + geom_hline(yintercept = 0) + ggtitle(paste0(file)) + ylim(c(plot.min,plot.max))
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

pdf('UR Comparison Plots.pdf')
plot_list
dev.off()

