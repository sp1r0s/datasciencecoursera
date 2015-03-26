best <- function(state, outcome) {

  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check if outcome is valid
  if (!outcome %in% outcomes) {
    stop("invalid outcome")
  }
  
  ## Find the right column
  outcomeColumn <- NULL
  if (outcome == "heart attack") {
    outcomeColumn <- 11  
  } else if (outcome == "heart failure") {
    outcomeColumn <- 17
  } else if (outcome == "pneumonia") {
    outcomeColumn <- 23
  }
  
  ## Read data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,outcomeColumn)]
  
  ## Check if state is valid
  if (!state %in% outcomeData$State) {
    stop("invalid state")
  }
  
  ## Get a subset for the state we want
  outcomeData = subset(outcomeData, State == state & outcomeData[3] != "Not Available")
  
  ## Change the outcome type to numeric for better results
  outcomeData[, 3] = as.numeric(outcomeData[, 3])
  
  ## Return hospital name in that state with lowest 30-sday death rate
  outcomeData = outcomeData[order(outcomeData[, 3], outcomeData$Hospital.Name), ]
  return(outcomeData$Hospital.Name[1])
  
}