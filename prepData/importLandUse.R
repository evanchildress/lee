library(readxl)
dir<-"~/lee/dataStore/nilesData"
lu<-read_excel(file.path(dir,"Loyalsock_Long_Term_Sites_Drainage_LULC.xlsx"))

nameMap<-list("Stream Name"="site",
              "Site_Latit"="lat",
              "Site_Longi"="long",
              "Total Area of Drainage Area (Sq Meters)"="drainageArea",
              "Forest Area (Sq. Meters)"="forest",
              "% Forest"="percentForest",
              "Open Water"="openWater",
              "Developed, Open Space"="developedOpen",
              "Developed, Low Intensity"="developedLow",
              "Developed, Medium Intensity"="developedMed",
              "Barren Land (Rock/Sand/Clay)"="barren",
              "Deciduous Forest"="forestDeciduous",
              "Evergreen Forest"="forestEvergreen",
              "Mixed Forest"="forestMixed",
              "Shrub/Scrub"="shrub",
              "Grassland/Herbaceous"="grassland",
              "Pasture/Hay"="pasture",
              "Cultivated Crops"="crops",
              "Woody Wetlands"="woodyWetlands",
              "Emergent Herbaceous Wetlands"="herbaceousWetlands")
names(lu)<-nameMap[match(names(lu),names(nameMap))]

capAndCollapse <- function(x) {
  s <- strsplit(x,":")[[1]]
  s <- strsplit(s, " ")[[1]]
  s<-paste0(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
            collapse="")
  paste0(tolower(substring(s,1,1)),substring(s,2))
}

lu<-data.table(lu)
lu[site=="Dry Run (Loyalsock)",site:="Dry Run Loyalsock"]
lu[site=="Dry Run (Hoagland)",site:="Dry Run Hoagland"]
lu[site=="Ellis Creek",site:="Ellis Run"]
lu<-lu[,site:=capAndCollapse(site),by=site][site!="nANA"]

coverVariables<-c('forest','openWater','developedOpen',
'developedLow','developedMed','barren','forestDeciduous',
'forestEvergreen','forestMixed','shrub','grassland','pasture',
'crops','woodyWetlands','herbaceousWetlands')

for(c in coverVariables){
  lu[[c]]<-lu[[c]]/lu[['drainageArea']]
}
lu[,percentForest:=NULL]
saveRDS(lu,"~/lee/dataStore/cleanData/lu.rds")
