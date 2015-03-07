pollutantmean <- function(directory, pollutant, id = 1:332) {
  pollutantVector <- vector()
  allFiles <- list.files(directory)
  for (i in id) {
    currentFileLocation <- paste(directory, "/", allFiles[i], sep="")
    currentFile <- read.csv(currentFileLocation, header=T, sep=",")
    pollutantVector <- c(pollutantVector, currentFile[, pollutant])
  }
    round(mean(pollutantVector, na.rm=TRUE), 3)
}