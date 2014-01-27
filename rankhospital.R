rankhospital <- function(state, outcome, num) {
## Read outcome data
if((state %in% names(table(data$State))) == FALSE)
    stop("invalid state")
if((outcome %in% c("heart attack", "heart failure", "pneumonia")) == FALSE)
    stop("invalid outcome")
## Read outcome data
data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate

 part<-data[data$State==state,]

if (outcome == "heart attack")
  {
  Rate <- as.numeric(part[part[,11] != "Not Available", 11])
  ##Hospital.30.Day.Readmission.Rates.from.Heart.Attack
  ##return(part[with(part, order(part[,11],Hospital.Name)), ][1,]$Hospital.Name)
	if(num == "best"){return(part[which.min(part[,11]),]$Hospital.Name) } 
	else if(num == "worst"){return(part[which.max(part[,11]),]$Hospital.Name) }
 	else if(num >= length(Rate)){return ("NA")}
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
else if(num >= length(Rate)){return ("NA")}
else{
		return(part[(part[,17]==(sort(part[,17], FALSE)[num])),]$Hospital.Name)


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