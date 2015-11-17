#The data set excludes many of the zero count sample/size bin/species/pass combos
#This script fills the complete combo set for every siteSurveyId

species<-c('bkt','bnt') #adding species here will create sample/bins/passes for it

dataBySiteSurvey<-data[,list(siteId,siteSurveyId,lat,long,estimateType,
                             siteLength,siteWidth,date,month,year)]
dataBySiteSurvey<-dataBySiteSurvey[!duplicated(dataBySiteSurvey)]
setkey(dataBySiteSurvey,siteSurveyId)

fillZeros<-function(type,species){
  fillEnv<-environment()
  
  #define number of passes based on sample type (estimateType)
  sampleTypes<-unique(data$estimateType)
  nPass<-c(2,3,4,2)[which(type==sampleTypes)]
  
  #Get the unique size bins and how many there are
  bins<-data[species %in% get('species',env=fillEnv),unique(sizeBin)]
  bins<-bins[bins!=1111]
  bins<-bins[order(bins)]
  nBins<-length(bins)
  
  nSpecies<-length(species)
  
  #get siteSurveyIds for all samples of the correct type
  allSamples<-data[estimateType==type,unique(siteSurveyId)]
  nSamples<-length(allSamples)
  
  #create data.table with all siteSurveyId,sizeBin,species,pass combos
  allRows<-data.table(
    siteSurveyId=rep(allSamples,each=nBins*nSpecies*nPass),
    sizeBin=rep(bins,nSamples*nSpecies*nPass),
    species=rep(rep(species,each=nBins*nPass),nSamples),
    pass=rep(rep(1:nPass,each=nBins),nSamples*nSpecies)
    )

  allRows<-allRows[species!='bkt'|sizeBin<=525] #there aren't any bkt that big
  setkey(allRows,species,siteSurveyId,sizeBin,pass)
  
  #merge with actual data
  trimmedData<-data[estimateType==type & species %in% get('species',env=fillEnv),
                    list(siteSurveyId,species,sizeBin,count,recapCount,pass)]
  setkey(trimmedData,species,siteSurveyId,sizeBin,pass)
  filledData<-trimmedData[allRows]
  filledData[is.na(count),count:=0]
  if(type=="Petersen M & R"){
    filledData[is.na(recapCount)&pass==2,recapCount:=0]
  }
  
  setkey(filledData,siteSurveyId)
  filledData<-dataBySiteSurvey[filledData]
  
return(filledData)
}

sampleTypes<-unique(data$estimateType)
filledData<-NULL
for(s in sampleTypes){
  filledData<-rbind(filledData,fillZeros(s,species))
}

assign('data',filledData,env=shared_data)



