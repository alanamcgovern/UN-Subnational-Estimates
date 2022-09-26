
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

