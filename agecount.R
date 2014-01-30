agecount <- function(age = NULL) {
## Check that "age" is non-NULL; else throw error
if(age == "NULL")
    stop("null cause")
## Read "homicides.txt" data file
homicide<-readLines("homicides.txt")
## Extract ages of victims; ignore records where no age is
## given
## Return integer containing count of homicides for that age
ages<-paste(age,"years",sep=" ")
##print(ages)
return(length(grep(ages,homicide)))
}
