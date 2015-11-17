library(readxl)
dir<-"~/lee/dataStore/nilesData"
files<-list.files(dir)

capAndCollapse <- function(x) {
  s <- strsplit(x,":")[[1]]
  s <- strsplit(s, " ")[[1]]
  s<-paste0(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        collapse="")
  paste0(tolower(substring(s,1,1)),substring(s,2))
  }

parseData<-function(textData){
  
  nas<-c("N/A","n/a")
  for(n in nas){
    textData[textData == n]<-NA
  }
  
  #make sure the year was entered correctly
  date<-as.Date(as.numeric(textData[grep("Date",textData[,1]),2]),
                origin="1899/12/30")
  date<-as.POSIXlt(date)
  year<-as.numeric(substring(f,nchar(f)-7,nchar(f)-4))
  yearDiff<-year-year(date)
  date$year<-date$year+yearDiff
  date<-as.Date(date)
  
  #identify which rows are breaks between data sections
  naRows<-which(is.na(textData[,1]))
  passRows<-grep("ass",textData[,1])
  breakRows<-c(naRows,passRows)
  
  #get site level data
  firstSiteData<-1
  lastSiteData<-min(breakRows[which(breakRows>firstSiteData)])-1
  
  #parse site data names
  siteDataNames<-as.list(textData[firstSiteData:lastSiteData,1])
  siteDataNames<-sapply(siteDataNames,capAndCollapse)
  siteDataNames[which(siteDataNames=="totAlk")]<-"totAlkalinity"
  
  siteData<-textData[firstSiteData:lastSiteData,2]
  siteData<-data.frame(t(siteData),stringsAsFactors=F)
  names(siteData)<-siteDataNames
  siteData$date<-date
  
  #function to get data from a specific pass (and assign effort to siteData)
  getPassData<-function(passNum){
    firstRow<-grep(paste0("ass ",passNum),textData[,1])+2
    if(length(firstRow)==0){
      siteData[[paste0('pass',passNum,'Effort')]]<<-NA
      skippedPasses<<-rbind(skippedPasses,
                            data.table(latitude=siteData$siteLatitude,
                                       date=siteData$date,
                                       pass=passNum))
      return(NULL)
    }
    if(length(firstRow>1)){firstRow<-firstRow[1]}
    siteData[[paste0('pass',passNum,'Effort')]]<<-textData[firstRow-2,2]
    if(firstRow>nrow(textData)){
      skippedPasses<<-rbind(skippedPasses,
                            data.table(latitude=siteData$siteLatitude,
                                       date=siteData$date,
                                       pass=passNum))
      return(NULL)}
    lastRow<-min(breakRows[which(breakRows>=firstRow)])-1
    
    
    if(grepl("not done",tolower(textData[firstRow,1]))|
       lastRow<firstRow){
      skippedPasses<<-rbind(skippedPasses,
                           data.table(latitude=siteData$siteLatitude,
                                      date=siteData$date,
                                      pass=passNum))
      return(NULL)}
    if(grepl("no",tolower(textData[firstRow,1]))|
       is.na(textData[firstRow,1])){return(NULL)}
    
    thisPass<-data.frame(textData[firstRow:lastRow,1:4],stringsAsFactors = F)
    names(thisPass)<-c("species","length","weight","dietNum")
    thisPass$pass<-passNum
    thisPass$latitude<-siteData$siteLatitude
    thisPass$date<-siteData$date

    return(thisPass)
  }
  #collate data from all the passes
  for(p in 1:3){
    if(p==1){passData<-getPassData(p)
    } else {passData<-rbind(passData,getPassData(p))}
  }

  #get a list of the other species observed during the survey
  otherSpeciesFirstCol<-grep("Other Species",textData)
  otherSpeciesFirstRow<-grep("Other Species",textData[,otherSpeciesFirstCol])+1
  
  if(is.na(textData[otherSpeciesFirstRow,otherSpeciesFirstCol])){
    otherSpecies<-NULL
  } else {
      otherSpeciesLastRow<-max(which(!is.na(textData[otherSpeciesFirstRow:nrow(textData),
                                            otherSpeciesFirstCol])))+otherSpeciesFirstRow-1
      species=textData[otherSpeciesFirstRow:otherSpeciesLastRow,
                                                    otherSpeciesFirstCol]
          approxAbundance=textData[otherSpeciesFirstRow:otherSpeciesLastRow,
                                                            otherSpeciesFirstCol+1]
          if(is.null(approxAbundance)){approxAbundance<-rep('present',length(species))}
          otherSpecies<-data.frame(species=species,approxAbundance=approxAbundance,
                                   latitude=siteData$siteLatitude,date=siteData$date,
                                   stringsAsFactors=F)
          if(all(grepl("[(]",otherSpecies$species))){
            split<-strsplit(otherSpecies$species,"[(]")[[1]]
            species<-split[1]
            abund<-substring(split[2],1,nchar(split[2])-1)
            otherSpecies$species<-species
            otherSpecies$approxAbundance<-abund
          }
  }
  
  #collate this sites data with all others previously looped
  if(!is.null(passData)){
   allTroutData<<-rbindlist(list(allTroutData,data.table(passData)),fill=T)
  }
  
   allSiteData<<-rbindlist(list(allSiteData,data.table(siteData)),fill=T)
   
  if(!is.null(otherSpecies)){
   allOtherSpecies<<-rbindlist(list(allOtherSpecies,otherSpecies),fill=T)
  }

}

allTroutData<-NULL
allSiteData<-NULL
allOtherSpecies<-NULL
skippedPasses<-NULL

for(f in files){
  sheets<-excel_sheets(file.path(dir,f))
  for(s in sheets){
    if(s=="Summary Data"){next}
    parseData(data.frame(read_excel(file.path(dir,f),s,col_names=F),
              stringsAsFactors=F))
    }
}

#function to convert to numeric with a 
#specified warning if the contents are not numeric
as.numeric2<-Vectorize(function(x){
  if(is.na(x)){return(as.numeric(x))}
  a<-suppressWarnings(as.numeric(x))
  if(is.na(a)){
    for(i in nchar(x):1){
      a<-suppressWarnings(as.numeric(substring(x,1,i)))
      if(!is.na(a)){
        warning(paste0("'",x,"'"," was converted to ",a," in as.numeric2 call"))
        break
      }
    }
    if(is.na(a)){warning(paste0("'",x,"'"," was converted to NA in as.numeric2 call"))}
  }
  return(a)
})

#Correct formats and naming in allSiteData
numericVariables<-c("siteLatitude",
                    "siteLongitude",
                    "waterTemp",
                    "ph",
                    "specCond",
                    "totAlkalinity",
                    "shockVoltage",
                    "siteLength",
                    "siteAvgWidth",
                    "pass1Effort",
                    "pass2Effort",
                    "pass3Effort",
                    "do",
                    "hardness")
for(v in numericVariables){
  allSiteData[[v]]<-as.numeric2(allSiteData[[v]])
  print(v)
}

allSiteData[streamName=="Dry Run" & tributaryTo=="Hoagland Branch",
            streamName:= "Dry Run Hoagland"]

allSiteData[streamName=="Dry Run" & tributaryTo=="Lower Loyalsock Creek",
            streamName:= "Dry Run Loyalsock"]

allSiteData[streamName %in% c("Ellis Run","Ellis Creek"),
            streamName:="Ellis Run"]

setnames(allSiteData,"streamName","site")
allSiteData[,site:=sapply(site,capAndCollapse)]

#correct number and date formatsfor allTroutData
nameKey<-unique(allSiteData[,list(siteLatitude,site)])
setkey(nameKey,siteLatitude)

allTroutData[,latitude:=as.numeric2(latitude)]
setkey(allTroutData,latitude)
allTroutData<-nameKey[allTroutData]
allTroutData[,siteLatitude:=NULL]

allTroutData[,length:=as.numeric2(length)]
allTroutData[,weight:=as.numeric2(weight)]

makeNotes<-Vectorize(function(x){
  a<-suppressWarnings(as.numeric(x))
  if(!is.na(a)){return(paste0("diet",a))}
  else {return(x)}
})

allTroutData[,notes:=makeNotes(dietNum)]
allTroutData[,dietNum:=NULL]
allTroutData[,species:=sapply(species,capAndCollapse)]

#allOtherSpecies corrections


equivalencies<-list('Blacknose Dace'= c("Black Nose Dace",
                                     "Blacknose Dace",
                                     "Black Nosed Dace",
                                     "Black Nosed Dace ",
                                     "Blacknose Dace ",
                                     "Black Nose Dace ",
                                     "Blacknose dace",
                                     "BND"),
                    'Longnose Dace'=  c("Long Nose Dace",
                                     "LND",
                                     "Long Nosed Dace",
                                     "Longnose Dace ",
                                     "Longnose Dace",
                                     "Longnosed Dace"),
                    'bluegill'=      c("Bluegill",
                                     "Bluegill "),
                    'Mottled Sculpin'=c("Mottled Sculpin",
                                     "Mottled Sculpin ",
                                     "Sculpin"),
                    'Margined Madtom'=c("Madtom",
                                     "Mad Tom",
                                     "Margined Madtom"),
                    'Creek Chub'=     c("Creek Chub",
                                     "Creek Chub"),
                    'Pumpkinseed'=   c("Pumpkinseed",
                                     "Pumpkin Seed"),
                    'Chain Pickerel'= c("chainPickeral",
                                     "Pickerel")
                                     )
rename<-function(x){
  otherNames<-equivalencies[[x]]
  rows<-which(allOtherSpecies$species %in% otherNames)
  allOtherSpecies[rows,species:=x]
}

for(n in names(equivalencies)){rename(n)}
allOtherSpecies[,species:=sapply(species,capAndCollapse)]
allOtherSpecies[,latitude:=as.numeric2(latitude)]
allOtherSpecies[,approxAbundance:=tolower(approxAbundance)]
setkey(allOtherSpecies,latitude)
allOtherSpecies<-nameKey[allOtherSpecies]
allOtherSpecies[,siteLatitude:=NULL]


#skipped passes

skippedPasses[,latitude:=as.numeric2(latitude)]
setkey(skippedPasses,latitude)
skippedPasses<-nameKey[skippedPasses]
skippedPasses[,siteLatitude:=NULL]


#test to see if everything is in order
# source("qualityControlFunctions.r")
# checkLengthWeight()
# checkPass()

#change names and write data
siteData<-allSiteData
fish<-allTroutData
nonTrout<-allOtherSpecies

setkey(siteData,site,date)
setkey(fish,site,date)
setkey(nonTrout,site,date)
setkey(skippedPasses,site,date)

save(siteData,fish,nonTrout,skippedPasses,
     file="~/lee/dataStore/cleanData/niles.rDATA")




