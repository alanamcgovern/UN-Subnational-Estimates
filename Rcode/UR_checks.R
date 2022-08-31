## this script evaluates over/undersampling of urban households

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

setwd(paste(code.path.splitted[1: (length(code.path.splitted)-1)], collapse = "/"))
library(openxlsx)
library(tidyverse)
options(digits=5)

## specify filepath
filenames <- c('ang_urbhh.xlsx',
               'bgd_urbhh.xlsx', #missing 2019 MICS -- oversampled anyway
               'ben_urbhh.xlsx',
               'bfa_urbhh.xlsx',
               'bdi_urbhh.xlsx',
               'cmr_urbhh.xlsx', 
               'tcd_urbhh.xlsx',
               'civ_urbhh.xlsx',
               'cod_urbhh.xlsx',
               'eth_urbhh.xlsx',
               'gha_urbhh.xlsx',
               'gin_urbhh.xlsx',
               'hti_urbhh.xlsx',
               'ind_urbhh.xlsx', # can't find frame/sample info for 2019-2021 survey*
               'ken_urbhh.xlsx',
               'lso_urbhh.xlsx', #missing 2018 MICS -- oversampled anyway
               'lbr_urbhh.xlsx', 
               'mdg_urbhh.xlsx',
               'mwi_urbhh.xlsx', # can't find frame households for 2010, add 2013-14 and 2019-20 MICS -- oversampled anyway
               'mli_urbhh.xlsx',
               'mrt_urbhh.xlsx',
               'moz_urbhh.xlsx', # no adequate sample info for 2011 -- oversampled anyway
               'mmr_urbhh.xlsx',
               'npl_urbhh.xlsx',
               'ner_urbhh.xlsx',
               'nga_urbhh.xlsx', # couldn't find sample frame for 2010
               'pak_urbhh.xlsx',
               'rwa_urbhh.xlsx', # couldn't find sample household for 2010 -- oversampled anyway
               'sen_urbhh.xlsx',
               'sle_urbhh.xlsx', # add 2017 MICS -- oversampled anyway
               'tgo_urbhh.xlsx',
               'uga_urbhh.xlsx',
               'tza_urbhh.xlsx', # no adequate infor for 2010 survey*
               'zmb_urbhh.xlsx',
               'zwe_urbhh.xlsx') 

get_URdiff <- function(file){
  tab <- read.xlsx(paste0(file))
  tab <- tab %>% filter(is.na(frame_prop) + is.na(sample_prop)==0) %>% mutate(diff = sample_prop-frame_prop,perc_diff = (sample_prop-frame_prop)/frame_prop,
                                                                              weighted_absdiff = f_total/sum(f_total)*abs(diff))
  if(sum(is.na(tab$f_urban) + is.na(tab$f_total) + is.na(tab$s_urban) + is.na(tab$s_total))==0){
    tab <- tab %>% mutate(chisq = ifelse(frame_prop>0 & frame_prop<1, (s_urban-(s_total*f_urban/f_total))^2/(s_total*frame_prop*(1-frame_prop)),0),
                          chisq2 = ifelse(frame_prop>0 & frame_prop<1 & sample_prop>frame_prop, (s_urban-(s_total*f_urban/f_total))^2/(s_total*frame_prop*(1-frame_prop)),0))
  }else{tab <- tab %>% mutate(chisq = NA,chisq2 = NA)}
                        
  res <- c(nrow(tab),sum(tab$chisq),1-pchisq(sum(tab$chisq),nrow(tab)-1),sum(tab$chisq2),1-pchisq(sum(tab$chisq2),nrow(tab)-1),median(tab$diff),tab$diff[which.max(abs(tab$diff))],median(tab$perc_diff),tab$perc_diff[which.max(abs(tab$perc_diff))])
  if(-min(tab$diff)>0.29){
    plot.min <- min(tab$diff) - 0.01
  }else{plot.min <- -0.3}
  if(max(tab$diff)>0.29){
    plot.max <- max(tab$diff) + 0.01
  }else{plot.max <- 0.3
  }
  if(sum(is.na(tab$f_urban) + is.na(tab$f_total) + is.na(tab$s_urban) + is.na(tab$s_total))==0){
  plot <- tab %>% ggplot() + geom_point(aes(x=as.numeric(row.names(tab)),y=diff,size=sqrt(f_total))) + 
    geom_point(x=nrow(tab)+1, y=sum(tab$weighted_absdiff), col='red',pch=3,size = 3) +
    ylab('Sample - Frame') + xlab('') + geom_hline(yintercept = 0) + ggtitle(paste0(file), paste0('Weighted Sum of Absolute Differences=',sum(tab$weighted_absdiff))) + 
    ylim(c(plot.min,plot.max)) + xlim(c(0,nrow(tab)+2)) + theme(legend.position = 'none')
  }else{plot <- tab %>% ggplot() + geom_point(aes(x=as.numeric(row.names(tab)),y=diff)) + 
    ylab('Sample - Frame') + xlab('') + geom_hline(yintercept = 0) + ggtitle(paste0(file),paste0('Household/population count not available')) + ylim(c(plot.min,plot.max))}
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
colnames(comparisons) <- c('NumberofAreas','Chi-Square','p','Chi-Square2','p2','MedianDiff','MaxDiff','MedianPercDiff','MaxPercDiff')

comparisons

pdf('UR Comparison Plots.pdf')
plot_list
dev.off()

