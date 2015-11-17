library(RPostgreSQL)
library(data.table)
library(reshape2)

#library(cruftery)

shared_data <<- local(expr={
  #could make this a local database if the data prep is computationally intensive
	#link <- db_connector("~/wb_credentials.rds")
	
	## This is where the global env gets a 'shared_data' as constructed above.
	return(environment(NULL))   
})