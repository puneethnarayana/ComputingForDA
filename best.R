best <- function(state, outcome) {

##Check the Validity of State
if((state %in% names(table(data$State))) == FALSE)
    stop("invalid state")
## Read outcome data
data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")

part<-data[data$State==state,]

## Return hospital name in that state with lowest 30-day death
## rate

	if (outcome == "heart attack")
	{
		##Rate <- as.numeric(part[part[,11] != "Not Available", 11])
		##Hospital.30.Day.Readmission.Rates.from.Heart.Attack
		##return(part[with(part, order(part[,11],Hospital.Name)), ][1,]$Hospital.Name)
		return(part[which.min(part[,11]),]$Hospital.Name)  
	}
  
	else if(outcome == "heart failure")
	{
		##Hospital.30.Day.Readmission.Rates.from.Heart.Failure
		##return(part[with(part, order(part[,17],Hospital.Name)), ][1,]$Hospital.Name)
		return(part[which.min(part[,17]),]$Hospital.Name)  
	}
  
	else if(outcome == "pneumonia")
	{
		## Hospital.30.Day.Readmission.Rates.from.Pneumonia
		##return(part[with(part, order(part[,23],Hospital.Name)), ][1,]$Hospital.Name)
		## which.min(part[,23])  
		return(part[which.min(part[,23]),]$Hospital.Name)
	}
	else
		##Checks if the outcome value given is valid
		{if((outcome %in% c("heart attack", "heart failure", "pneumonia")) == FALSE)
		stop("invalid outcome")}

return(null)

}