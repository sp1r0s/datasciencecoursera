best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!is.character(state) || length(state) > 2) {
    stop("invalid state")
  }
  
  if (!is.character(outcome)) {
      stop("invalid outcome");
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  
}