corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
	data<-complete(directory)
	clean<-na.omit(data[data$nobs>threshold,])
	cleanids<-clean[,1]
	out <- vector(mode="numeric", length=0)
	
	for(i in cleanids){
		csvfiles <- sprintf("/Users/puneeth_nn/Documents/%s/%03d.csv", directory, as.numeric(i))
		
		frame<-read.csv(csvfiles)
		frame<-na.omit(frame)

		corr<-cor(as.numeric(frame$sulfate),as.numeric(frame$nitrate))
		out <- append(out, corr)
	
	}
	return(out)
}

complete <- function(directory,id = 1:332) {

	iid<-id
	csvfiles <- sprintf("/Users/puneeth_nn/Documents/%s/%03d.csv", directory, as.numeric(iid))
	nrows <- sapply( csvfiles, function(f) nrow(read.csv(f)))
	rowlabels <- nrow(nrows)
	data.frame(id=sprintf('%3d', iid), 
	nobs=sapply(csvfiles,function(f) nrow(na.omit(read.csv(f))))
	row.names=rowlabels
    
}