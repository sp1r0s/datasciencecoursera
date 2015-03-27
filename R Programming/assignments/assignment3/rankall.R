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
  
  if(is.numeric(num) && num > nrow(outcomeData)) {
      return(NA)
  } else if(num == "best") {
      return(best(state, outcome))
      apply(outcomeData, 1, best, state = outcomeData$State, outcome = outcome)
  } else if(num == "worst") {
      ## Change the outcome type to numeric for better results
      outcomeData[, 3] = as.numeric(outcomeData[, 3])
    
      ## Return hospital name in that state with highest 30-sday death rate
      outcomeData = outcomeData[order(outcomeData[, 3], outcomeData$Hospital.Name), ]
      return(outcomeData$Hospital.Name[nrow(outcomeData)])
  } else {
      ## Change the outcome type to numeric for better results
      outcomeData[, 3] = as.numeric(outcomeData[, 3])
    
      ## Return hospital name in that state with highest 30-sday death rate
      outcomeData = outcomeData[order(outcomeData[, 3], outcomeData$Hospital.Name), ]
      return(outcomeData$Hospital.Name[num])
  
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    return(apply(outcomeData, 1, rankhospital(), state = outcomeData$State, outcome = outcome, num = num))
  }
}