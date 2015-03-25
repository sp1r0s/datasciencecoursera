best <- function(state, outcome) {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state is valid
    if (!is.character(state) || nchar(state) > 2) {
        stop("invalid state")
    }
    
    # Check that outcome is valid
    if (!is.character(outcome)
        && (outcome != "heart attack" || outcome != "heart failure" || outcome != "pneumonia")) {
        stop("invalid outcome");
    }
    
    
    
    ## Return hospital name in that state with lowest 30-day death rate
    stateData <- subset(outcomeData, State == state)
    lowestRate <- NULL
    
    if (outcome == "heart attack") {
        lowestRate <- min(stateData[, 11])  
    } else if (outcome == "heart failure") {
        lowestRate <- min(stateData[, 17])  
    } else if (outcome == "pneumonia") {
        lowestRate <- min(stateData[, 23])  
    }
    
    return(subset(stateData, 'Hospital 30-Day Death (Mortality) Rates from Heart Attack' == lowestRate))
}