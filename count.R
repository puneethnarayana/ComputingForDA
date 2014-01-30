count <- function(cause = NULL) {
## Check that "cause" is non-NULL; else throw error
if(cause == "NULL")
    stop("null cause")
## Check that specific "cause" is allowed; else throw error
if((cause %in% c("asphyxiation", "blunt force", "other","shooting","stabbing","unknown")) == FALSE)
    stop("invalid cause")
## Read "homicides.txt" data file
homicide<-readLines("homicides.txt")
## Extract causes of death
## Return integer containing count of homicides for that cause
if(cause=="shooting" | cause=="Shooting"){
return(length(grep("Cause: [Ss]hooting",homicide)))}
if(cause=="asphyxiation" | cause=="Asphyxiation"){
return(length(grep("Cause: [Aa]sphyxiation",homicide)))}
if(cause=="Blunt force" | cause=="blunt force"){
return(length(grep("Cause: [Bb]lunt force",homicide)))}
if(cause=="Blunt force" | cause=="blunt force"){
return(length(grep("Cause: [Bb]lunt force",homicide)))}
if(cause=="stabbing" | cause=="Stabbing"){
return(length(grep("Cause: [Ss]tabbing",homicide)))}
##return(length(grep("[Ss]hooting",homicide)))
}