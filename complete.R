complete <- function(directory,id = 1:332) {

   iid<-id

   csvfiles <- sprintf("/Users/puneeth_nn/Documents/%s/%03d.csv", directory, as.numeric(iid))
	print(csvfiles)
   ##nrows <- sapply( csvfiles, function(f) nrow(read.csv(f)))
   ##final<-read.csv(is.character(nrows))
   ##omitted<-final[complete.cases(final),]
	##rownew<-nrow(omitted)
	##print(rownew)
   nrows <- sapply( csvfiles, function(f) nrow(read.csv(f)))
   ##sum(nrows)
	##print(nrows)
        rowlabels <- nrow(nrows)

        data.frame(id=sprintf('%3d', iid), 
            nobs=sapply(csvfiles,function(f) nrow(na.omit(read.csv(f))))
		,row.names=rowlabels
           )
       }