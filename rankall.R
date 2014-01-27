rankall <- function(outcome, num = "best") {

if((outcome %in% c("heart attack", "heart failure", "pneumonia")) == FALSE)
    stop("invalid outcome")

data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
##print("Yo!")
tbl<-table(data$State)
statesnames<-names(tbl)

df <- data.frame(hospital= character(100), state= character(100))
df = data.frame(matrix(vector(), 0, 2, dimnames=list(c(),c("hospital", "state"))), stringsAsFactors=F)
	for(i in 1:length(statesnames)){
 	part<-data[data$State==statesnames[i],]
	if((statesnames[i] %in% names(table(data$State))) == FALSE)
    stop("invalid state")
    hospital<-getHospital(outcome,part,num)
	##print(hospital)
	##print(statesnames[i])
	df[i,]<-c(hospital,statesnames[i])
	}
return(df)
}


getHospital<-function(outcome,part,num){

if (outcome == "heart attack")
  {
  Rate <- as.numeric(part[part[,11] != "Not Available", 11])
  ##Hospital.30.Day.Readmission.Rates.from.Heart.Attack
  ##return(part[with(part, order(part[,11],Hospital.Name)), ][1,]$Hospital.Name)
	if(num == "best"){return(part[which.min(part[,11]),]$Hospital.Name) } 
	else if(num == "worst"){return(part[which.max(part[,11]),]$Hospital.Name) }
 	else if(is.numeric(num) >= length(Rate)){return ("NA")}
else{
	return(part[(part[,11]==(sort(part[,11], FALSE)[num])),]$Hospital.Name)

}
}
  
 else if(outcome == "heart failure")
  {
Rate <- as.numeric(part[part[,17] != "Not Available", 17])
 ##Hospital.30.Day.Readmission.Rates.from.Heart.Failure
 ##return(part[with(part, order(part[,17],Hospital.Name)), ][1,]$Hospital.Name)
if(num=="best"){return(part[which.min(part[,17]),]$Hospital.Name)  }
else if(num=="worst"){return(part[which.max(part[,17]),]$Hospital.Name)  }
##print(length(Rate))
if(num >= length(Rate)){return ("NA")}

else{
	##print(num)
	q<-part[part[,17]==sort(Rate)[10],]$Hospital.Name
	p<-sort(q)
	return(p[1])
}
}
  
  else if(outcome == "pneumonia")
  {
Rate <- as.numeric(part[part[,23] != "Not Available", 23])
 ## Hospital.30.Day.Readmission.Rates.from.Pneumonia
 ##return(part[with(part, order(part[,23],Hospital.Name)), ][1,]$Hospital.Name)
## which.min(part[,23])  
if(num=="best"){return(part[which.min(part[,23]),]$Hospital.Name)}
else if(num=="worst"){return(part[which.max(part[,23]),]$Hospital.Name)}
else if(num >= length(Rate)){return ("NA")}
else{
		return(part[(part[,11]==(sort(part[,11], FALSE)[num])),]$Hospital.Name)

}
}
}