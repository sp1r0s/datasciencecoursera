corr <- function(directory, threshold = 0) {
  allFiles <- list.files(directory)
  corrVector <- vector()
  completeTable <- complete(directory, 1:332)
  nobs <- completeTable$nobs
  ids <- completeTable$id[nobs > threshold]
  counter <- 1

  for (i in ids) {
    currentFileLocation <- paste(directory, "/", allFiles[i], sep="")
    currentFile <- read.csv(currentFileLocation, header=T, sep=",")
    corrVector[counter] <- round(cor(currentFile$sulfate, currentFile$nitrate, use="complete.obs"), 5)
    counter <- counter + 1
  }
  return(corrVector)  
}