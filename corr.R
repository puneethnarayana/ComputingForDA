corr <- function(directory, threshold = 0) {
  page <- 1
  data <- list()
  output <- list()

  while (page <= 20){
    while (nchar(as.character(page)) < 3){
      page <- paste("0",as.character(page), sep="")
    }
    
    idString <- paste(directory,"\\",page,".csv",sep="")
    if(nrow(na.omit(read.csv(idString))) >= threshold) {
      data <- na.omit(read.csv(idString))
      output <- append(output, cor(data$sulfate,data$nitrate))
    }
    page <- as.numeric(page) + 1
  }
  return(as.numeric(output))
}