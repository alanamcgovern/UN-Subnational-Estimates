
## Admin 1 map plot, direct, by survey  ------------------------------------------------------

for(plotyears in periods.survey){
  
  # U5MR
  tmp <- direct.admin1.u5[direct.admin1.u5$years == paste(plotyears),]
  tmp$regionPlot <- admin1.names$GADM[match(tmp$region, admin1.names$Internal)]
  
  pdf(paste0("Figures/Direct/U5MR/Admin1/",
             country, 
             "_admin1_u5_direct_poly_bySurvey_",
             plotyears, ".pdf"),
      width = 3.5, height = 3.5)
  {
    print(SUMMER::mapPlot(data = tmp,
                          is.long = T, 
                          variables = "surveyYears", 
                          values = "mean",direction = -1,
                          geo = poly.adm1, ncol = 2,
                          legend.label = "U5MR",
                          per1000 = TRUE,
                          by.data = "regionPlot",
                          by.geo = paste0(sub(".*data[$]","",poly.label.adm1))))
  }
  dev.off()
  
  # NMR
  tmp <- direct.admin1.nmr[direct.admin1.nmr$years == paste(plotyears),]
  tmp$regionPlot <- admin1.names$GADM[match(tmp$region, admin1.names$Internal)]
  
  pdf(paste0("Figures/Direct/NMR/Admin1/",
             country, 
             "_admin1_nmr_direct_poly_bySurvey_",
             plotyears, ".pdf"),
      width = 3.5, height = 3.5)
  {
    print(SUMMER::mapPlot(data = tmp,
                          is.long = T, 
                          variables = "surveyYears", 
                          values = "mean",direction = -1,
                          geo = poly.adm1, ncol = 2,
                          legend.label = "NMR",
                          per1000 = TRUE,
                          by.data = "regionPlot",
                          by.geo = paste0(sub(".*data[$]","",poly.label.adm1))))
  }
  dev.off()
}

## Admin 2 map plot, direct, by survey  ------------------------------------------------------

for(plotyears in periods.survey){
  
  ## U5MR
  tmp <- direct.admin2.u5[direct.admin2.u5$years == paste(plotyears),]
  tmp$regionPlot <- admin2.names$GADM[match(tmp$region, admin2.names$Internal)]
  
  pdf(paste0("Figures/Direct/U5MR/Admin2/",
             country, 
             "_admin2_u5_direct_poly_bySurvey_",
             plotyears, ".pdf"),
      width = 3.5, height = 3.5)
  {
    print(SUMMER::mapPlot(data = tmp,
                          is.long = T, 
                          variables = "surveyYears", 
                          values = "mean",direction = -1,
                          geo = poly.adm2, ncol = 2,
                          legend.label = "U5MR",
                          per1000 = TRUE,
                          by.data = "regionPlot",
                          #changed for Malawi (generalise later)
                          by.geo = sub(".*data[$]","",poly.label.adm2)))
  }
  dev.off()
  
  ## NMR
  tmp <- direct.admin2.nmr[direct.admin2.nmr$years == paste(plotyears),]
  tmp$regionPlot <- admin2.names$GADM[match(tmp$region, admin2.names$Internal)]
  
  pdf(paste0("Figures/Direct/NMR/Admin2/",
             country, 
             "_admin2_nmr_direct_poly_bySurvey_",
             plotyears, ".pdf"),
      width = 3.5, height = 3.5)
  {
    print(SUMMER::mapPlot(data = tmp,
                          is.long = T, 
                          variables = "surveyYears", 
                          values = "mean",direction = -1,
                          geo = poly.adm2, ncol = 2,
                          legend.label = "NMR",
                          per1000 = TRUE,
                          by.data = "regionPlot",
                          by.geo = sub(".*data[$]","",poly.label.adm2)))
  }
  dev.off()
}


## Rank plots ####
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/NMR/rank'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/NMR/rank'))
}
if(!dir.exists(paste0(res.dir,
                      '/Figures/Betabinomial/U5MR/rank'))){
  dir.create(paste0(res.dir,
                    '/Figures/Betabinomial/U5MR/rank'))
}
year <- 2020
for(admin in admin_vt){
  
  admin_name_dt <- as.data.table(get(paste0(c('admin1','admin2')[admin==admin_vt], ".names")))
  
  map_shp <- c(poly.adm1,poly.adm2)[admin==admin_vt][[1]]
  
  # create plotting area names (just admin 1 name if admin = 1, or 'admin2,\n admin1' if admin = 2)
  if (admin == "adm1") {
    admin_name_dt$nameToPlot <- eval(str2lang(poly.label.adm1))
  } else if (admin == "adm2") {
    admin_name_dt$nameToPlot <- paste0(eval(str2lang(poly.label.adm2)),
                                       #need to change if using alternative GADM files
                                       ",\n", poly.adm2@data$NAME_1)
  }
  
  for(outcome in outcome_vt){
    for(strat in strat_vt){
      
      load(paste0(res.dir, "/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],'/',
                  country, "_",
                  admin, "_",
                  strat, "_",
                  outcome, "_postsamp.RData"))
      
      cond <- lapply(postsamp_mt_list, function(x){x$years == year})
      postsamp_mt <- postsamp_mt_list[which(year == year_vt)][[1]]$postsamp_mt
      
      #### rank postsamps ####
      
      rank_mt <- apply(postsamp_mt, 2, rank)
      
      pred_dt <- admin_name_dt
      pred_dt[, "ID"] <- 1:nrow(pred_dt)
      pred_dt[, "avg_rank"] <- apply(rank_mt, 1, mean)
      pred_dt[, "low_rank"] <- apply(rank_mt, 1, min)
      pred_dt[, "up_rank"] <- apply(rank_mt, 1, max)
      
      #### all states hist ####
      rowcount <- ceiling(nrow(pred_dt_order)/3)
      pdf(paste0(res.dir, "/Figures/Betabinomial/",c('NMR','U5MR')[outcome==outcome_vt],"/rank/",
                 country, "_",
                 admin, "_", strat, "_", outcome,
                 "_Y", year, "_rankall.pdf"),
          width = 15, height = rowcount*2)
      
      {  
        par(mar = c(2.5, 1, 2, 1), mfcol = c(rowcount, 3))
        
        pred_dt_order <- pred_dt[order(avg_rank)]
        
        for (i in 1:nrow(pred_dt_order)){
          # i <- 1
          
          id <- pred_dt_order[i, ID]
          name <- pred_dt_order[i, nameToPlot]
          
          rank_vt <- rank_mt[id, ]
          
          avg_rank <- pred_dt_order[i, avg_rank]
          
          ranktable <- as.data.table(table(rank_vt))
          ranktable <- merge(data.table(rank = as.character(1:nrow(pred_dt_order))), ranktable, 
                             by.x = "rank", by.y = "rank_vt", all.x = T)
          ranktable[, "rank" := as.integer(rank)]
          ranktable <- ranktable[order(rank)]
          ranktable[is.na(N), "N"] <- 0
          
          barplot(ranktable$N, width = 0.825, 
                  xlim = c(nrow(pred_dt_order), 0), xlab = "", ylab = "",
                  main = paste0(name, "\nER = ", format(round(avg_rank, 1), nsmall = 1)),
                  xaxt = "n", yaxt = "n", col = "#08519c", border = F,
                  cex.main = 0.75)
          axis(1, at = nrow(pred_dt_order):1-0.5, labels = as.character(nrow(pred_dt_order):1), tick = F)
        }
      }
      dev.off()
      
    }
  }
}

