complete <- function(directory, id = 1:332) {
  completeVector <- vector()
  allFiles <- list.files(directory)
  counter <- 1
  for (i in id) {
    currentFileLocation <- paste(directory, "/", allFiles[i], sep="")
    currentFile <- read.csv(currentFileLocation, header=T, sep=",")
    completeVector[counter] <- sum(complete.cases(currentFile))
    counter <- counter + 1
  }
  data.frame(id = id, nobs = completeVector)
}