
#load libraries
library(haven)
library(tidyverse)
library(SUMMER)

# Malawi 2013-2014 ------------------------------------------------------------------------
#read data and get info
bh <- read_sav('2014/mwi_2014_bh.sav')
sapply(bh,attr,'label')

bh <- bh %>% select(HH1,BH5,BH9C,BH4C,HH6,HH7,WDOI,wmweight,region) %>%
#put some variables in proper format for getBirths
      mutate(urban=ifelse(HH6==1,'urban',
                           ifelse(HH6==2,'rural',NA)),
             alive=ifelse(BH5==1,'yes',
                          ifelse(BH5==2,'no',NA)))

bh$admin2.name <- ''
for(i in 1:length(unique(bh$HH7))){
  bh$admin2.name[bh$HH7==attr(bh$HH7,'labels')[i]] <- names(attr(bh$HH7,'labels'))[i]
}
bh$admin2.name <- str_remove(bh$admin2.name,' city')
bh <- bh %>% mutate(admin2.name=ifelse(admin2.name %in% c('Mzuzu','Nkhatabay'),'Nkhata Bay',admin2.name))
            
dat.tmp <- getBirths(data=bh, surveyyear = 2014, variables = c('HH1','BH4C','BH9C','admin2.name','WDOI','alive','urban','wmweight'),
                     strata = c('admin2.name','urban'),dob = 'BH4C',alive = 'alive',age = 'BH9C',date.interview = 'WDOI',
                     age.truncate = 36,year.cut = seq(2000, 2014 + 1, 1),compact.by = c("HH1", 'wmweight', 'admin2.name','urban'),compact=T)

#put in correct order
mwi.2014.tmp <- dat.tmp[,c('HH1','age','time','total','died','wmweight','urban','admin2.name')]

colnames(mwi.2014.tmp) <- c("cluster", "age", "years", "total",
                       "Y", "v005", 'urban', "admin2.name")

dat.tmp <- mwi.2014.tmp %>% select(c(cluster,age,years,total,Y,v005,urban,admin2.name))
dat.tmp$survey <- 2014

save(dat.tmp,file='2014/mwi.2014.tmp.rda')

# Malawi 2019-2020 ------------------------------------------------------------------------
#read data and get info
bh <- read_sav('2020/mwi_2020_bh.sav')
sapply(bh,attr,'label')

bh <- bh %>% select(HH1,BH5,BH4C,BH9C,HH6,WDOI,wmweight,stratum)%>%
  #put some variables in proper format for getBirths
  mutate(urban=ifelse(HH6==1,'urban',
                      ifelse(HH6==2,'rural',NA)),
         alive=ifelse(BH5==1,'yes',
                      ifelse(BH5==2,'no',NA)))
bh$strata <- ''
for(i in 1:length(names(attr(bh$stratum,'labels')))){
  bh$strata[bh$stratum==attr(bh$stratum,'labels')[i]] <- names(attr(bh$stratum,'labels'))[i]
}
bh$strata <- str_remove(bh$strata,' City')
bh$admin2.name <- ''
bh$admin2.name[bh$HH6==1] <- str_remove(bh$strata[bh$HH6==1],' Urban')
bh$admin2.name[bh$HH6==2] <- str_remove(bh$strata[bh$HH6==2],' Rural')
bh$admin2.name <- str_trim(bh$admin2.name)

bh <- bh %>% mutate(admin2.name=ifelse(admin2.name=='Mzuzu','Nkhata Bay',admin2.name))

dat.tmp <- getBirths(data=bh, surveyyear = 2020, variables = c('HH1','BH4C','BH9C','WDOI','alive','strata','wmweight','urban','admin2.name'),
                     strata = 'strata',dob = 'BH4C',alive = 'alive',age = 'BH9C',date.interview = 'WDOI',
                     age.truncate = 36,year.cut = seq(2000, 2020 + 1, 1),compact.by = c("HH1", 'wmweight','urban','admin2.name'),compact=T)

mwi.2020.tmp <- dat.tmp[,c('HH1','age','time','total','died','wmweight','urban','admin2.name')]
colnames(mwi.2020.tmp) <-  c("cluster", "age", "years", "total",
                             "Y", "v005",'urban', "admin2.name")

dat.tmp <- mwi.2020.tmp %>% select(c(cluster,age,years,total,Y,v005,urban,admin2.name))
dat.tmp$survey <- 2020

save(dat.tmp,file='2020/mwi.2020.tmp.rda')

# Bangladesh 2019 ---------------------------------------
#read data and get info
bh <- read_sav('Data/Bangladesh/2019/bgd_2019_bh.sav')
sapply(bh,attr,'label')

bh <- bh %>% select(c(HH1,BH4C,BH5,BH9C,HH6,HH7A,WDOI,wmweight)) %>%
  #put some variables in proper format for getBirths
  mutate(urban=ifelse(HH6==1,'urban',
                      ifelse(HH6==2,'rural',NA)),
         alive=ifelse(BH5==1,'yes',
                      ifelse(BH5==2,'no',NA)))
bh$admin2.name <- ''
for(i in 1:length(names(attr(bh$HH7A,'labels')))){
  bh$admin2.name[bh$HH7A==attr(bh$HH7A,'labels')[i]] <- names(attr(bh$HH7A,'labels'))[i]
}

dat.tmp <- getBirths(data=bh, surveyyear = 2019, variables = c('HH1','BH4C','BH9C','WDOI','alive','wmweight','urban','admin2.name'),
                     strata = c('urban','admin2.name'),dob = 'BH4C',alive = 'alive',age = 'BH9C',date.interview = 'WDOI',
                     age.truncate = 24,year.cut = seq(2000, 2020 + 1, 1),compact.by = c("HH1", 'wmweight','urban','admin2.name'),compact=T)

bgd.2019.tmp <- dat.tmp[,c('HH1','age','time','total','died','wmweight','urban','admin2.name')]
colnames(bgd.2019.tmp)<-  c("cluster", "age", "years", "total",
                            "Y", "v005",'urban', "admin2.name")

dat.tmp <- bgd.2019.tmp %>% select(c(cluster,age,years,total,Y,v005,urban,admin2.name))
dat.tmp$survey <- 2019

save(dat.tmp,file='Data/Bangladesh/2019/bgd.2019.tmp.rda')

# Madagascar 2018 ---------------------------------------
#read data and get info
bh <- read_sav('Data/Madagascar/2018/mdg_2018_bh.sav')
sapply(bh,attr,'label')

bh <- bh %>% select(c(HH1,BH4C,BH5,BH9C,HH6,HH7,WDOI,wmweight)) %>%
  #put some variables in proper format for getBirths
  mutate(urban=ifelse(HH6==1,'urban',
                      ifelse(HH6==2,'rural',NA)),
         alive=ifelse(BH5==1,'yes',
                      ifelse(BH5==2,'no',NA)))

bh$admin2.name <- ''
for(i in 1:length(names(attr(bh$HH7,'labels')))){
  bh$admin2.name[bh$HH7==attr(bh$HH7,'labels')[i]] <- names(attr(bh$HH7,'labels'))[i]
}
bh$admin2.name <- tolower(bh$admin2.name)

dat.tmp <- getBirths(data=bh, surveyyear = 2018, variables = c('HH1','BH4C','BH9C','WDOI','alive','wmweight','urban','admin2.name'),
                     strata = c('urban','admin2.name'),dob = 'BH4C',alive = 'alive',age = 'BH9C',date.interview = 'WDOI',
                     age.truncate = 24,year.cut = seq(2000, 2018 + 1, 1),compact.by = c("HH1", 'wmweight','urban','admin2.name'),compact=T)

mdg.2018.tmp <- dat.tmp[,c('HH1','age','time','total','died','wmweight','urban','admin2.name')]
colnames(mdg.2018.tmp)<-  c("cluster", "age", "years", "total",
                            "Y", "v005",'urban', "admin2.name")

## add admin1 areas
admin1.key <-data.frame(c('antananarivo','antsiranana','fianarantsoa','mahajanga','toamasina','toliara'))
colnames(admin1.key) <- 'admin1.name'
admin1.key$admin1 <- 1:nrow(admin1.key)
admin1.key$admin1.char <- paste0('admin1_',admin1.key$admin1)

admin.key <- data.frame(unique(dat.tmp$admin2.name))
colnames(admin.key) <- 'admin2.name'
admin.key$admin2 <- 1:nrow(admin.key)
admin.key$admin2.char <- paste0('admin2_',admin.key$admin2)
admin.key$admin1.name <- c('toamasina','fianarantsoa','antananarivo','toamasina','toliara','toliara','toliara','fianarantsoa','toamasina',
                           'mahajanga','mahajanga','antananarivo','antsiranana','fianarantsoa','fianarantsoa','antananarivo','mahajanga',
                           'toliara','antsiranana','mahajanga','antananarivo','fianarantsoa')
admin.key <- merge(admin.key,admin1.key,by='admin1.name')
dat.tmp <- merge(mdg.2018.tmp,admin.key,by='admin2.name')

dat.tmp <- dat.tmp %>% select(c(cluster,age,years,total,Y,v005,urban,admin1,admin1.name,admin1.char,admin2,admin2.name,admin2.char))
dat.tmp$survey <- 2018

save(dat.tmp,file='Data/Madagascar/2018/mdg.2018.tmp.rda')
