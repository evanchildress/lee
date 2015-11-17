library(RPostgreSQL)
library(integrator)
library(readxl)
options(stringsAsFactors=F)

sharedData<-local(expr={
  link<-db_connector("~/lee/leeCredentials.rds")
  dataStore<-"~/lee/dataStore"
  figureDir<-"~/lee/figures"
  
  return(environment(NULL))
})