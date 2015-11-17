# rm(list=ls())
library(R2jags)
library(lme4)
library(MCMCpack) # rwish function
library(nlme)
library(plyr)
library(lubridate)
library(lattice)
library(RgoogleMaps)
library(PBSmapping)
library(maptools)
library(rgdal)
library(maps)
library(GISTools)
library(mapplots)
library(plotrix)
library(seqinr)
library(rgdal)


## Read in data
dat <- read.csv('qrystbtpopest.csv')
head(dat)
dim(dat) # [1] 129238     22

dat$date<-as.Date(dat$SurveyDate,format="%m/%d/%Y") 
head(dat)

dat$year <- year(dat$date)
dat$month <- month(dat$date)
head(dat)

# Rename some column headings
dat <- rename(dat, c('WaterSiteSurvey_ID'='surveyid','SurveySiteLatDD'='lat', 'SurveySiteLonDD'='long','SiteLength_m'='length',
              'SiteWidth_m'='width', 'Comname'='species','GroupSize'='sizebin', 'EffortCatch'='catch',
              'SiteGearDescription'='gear','SurveyPurposeDescription'='surveypurpose','WaterSectionID'='waterid') )
head(dat)
summary(dat)

# Number of unique surveys
length(unique(dat$surveyid)) # 5,127
# Number of unique stream sections
length(unique(dat$waterid)) # 1,775
# Number of unique stream sections - brook
length(unique(dat$waterid[dat$species=='Brook Trout'])) # 1,541
# Number of unique stream sections - brook
length(unique(dat$waterid[dat$species=='Brown Trout'])) # 1,267
# Number of Zippin 3-pass surveys
length(unique(dat$surveyid[dat$EstimateType=='Zippen 3 Pass Removel'])) # 531
# Number of Zippin 4-pass surveys
length(unique(dat$surveyid[dat$EstimateType=='Zippen 4 Pass Removel'])) # 16
# Number of Peterson M & R
length(unique(dat$surveyid[dat$EstimateType=='Petersen M & R'])) # 4,331
# Number of Jolly 2 Pass Removel
length(unique(dat$surveyid[dat$EstimateType=='Jolly 2 Pass Removel'])) # 249


# Range of years
range(dat$year) # 1975- 2015

###--------  Plot unique sites ---------- ###############
# Create id and year variable for below
dat$id.year <- as.factor(paste(dat$waterid, dat$year, sep='') )
summary(dat)

# Select out most recent date for each site id
# sort by comid and yearsamp (the "-" before yearsamp makes the most recent year first)
# This is necessary for the loop below.
dat2 <- dat[order(dat$waterid, -dat$year) , ]
head(dat2,50)

# Check sorting
# dat2[dat2$surveyid==10, ] 

# Get unique ids (used in loop below)
ids <- unique(dat2$waterid)
length(ids)

# Create a container to hold the most recent data for each id in the loop below
# Create a new data frame called dat3 from dat2 (we use dat2 so dat3 has the same column names, etc), 
# but only want to make dat3 with as many rows as there are unique comids.
dat3 <- dat2[1:length(ids),]

# This loop will go through and grab the first row for each id in dat2 and its most recent year,
# because this was sorted by comid and year, it will be grabbing the most recent year.
# We are simply overwriting the data contained in dat3 (our container) created above with
# the new data we actually want.
for(i in 1:length(ids) ){   
  dat3[i,] <- dat2[dat2$waterid == ids[i], ][1,]
  
}
head(dat3)
dim(dat3)


bb <- qbbox(lat = dat3[,"lat"], lon = dat3[,"long"])
zoom <- min(MaxZoom(range(dat3$lat), range(dat3$lon)))


MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "sites.png", maptype="terrain",zoom=zoom) # terrain, hybrid

png("All_sites.png", 900, 900, res=300)
PlotOnStaticMap(MyMap, dat3$lat, dat3$long, col="red",pch='*', add = F, cex=0.5)
dev.off() 


########################### 
