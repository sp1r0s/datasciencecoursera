rankall <- function(outcome, num = "best") {
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
  
  ## Change the outcome type to numeric for better results
  outcomeData[, 3] = as.numeric(outcomeData[, 3])
  
  states <- outcomeData$State
  states <- sort(unique(states))
  
  hospitalsPerState <- NULL
  
  for (i in 1:length(states)) {
    currentStateData <- outcomeData[outcomeData$State == states[i],]
    a <- rank(currentStateData[, 3], na.last=NA)
    
    if (num=="best") {
      r <- 1
    } else if (num =="worst") {
      r <- length(a)
    } else if (num <= length(a) ) {
      r <- num
    } else {
      r <- NA
    }
    if (is.na(r)) {
      hospitalsPerState[i] <- NA
    } else {
      hospitalsPerState[i] <- currentStateData$Hospital.Name[order(currentStateData[, 3], currentStateData$Hospital.Name)[r]]
    }
  }
  return(data.frame(hospital=hospitalsPerState, state=states)) 
}