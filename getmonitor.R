getmonitor <- function(id, directory,summarize=FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
	if(id<10) id <- paste("00",as.character(id),sep="")
       else if (id<100) id <- paste("0",as.character(id),sep="")
       else id <- as.character(id)
	
	data<-read.csv(paste(getwd(),"/",directory,"/",id,".csv",sep=""))
	
 	
	
	if(summarize=="TRUE"){
		
	print(summary(data))
	}

return(data)
}
