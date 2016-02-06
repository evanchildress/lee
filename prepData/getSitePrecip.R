precip<-readRDS("~/lee/figures/allPrecip.rds")
load("~/lee/dataStore/cleanData/niles.rdata")
rm(fish,nonTrout,skippedPasses)

getPrecip<-function(lat,long){
#   latDiff<-precip$lat-lat
#   longDiff<-precip$long-long
#   which.min()
  precip$precip[which.min(abs(precip$lat-lat)+abs(precip$long-long))]
}

sitePrecip<-siteData %>% 
  .[,.(lat=mean(siteLatitude),
       long=mean(siteLongitude)),
       by=site] %>%
   .[,.(precip=getPrecip(lat,long)),by=site]
saveRDS(sitePrecip,"~/lee/dataStore/cleanData/sitePrecip.rds")  
              