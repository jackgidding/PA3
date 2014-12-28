
## best.R
## Author: Jack Gidding
## 

## Fn name: best
## Args:    state: 2-character abbreviated name of a state 
##          outcome: outcome name, which is one of "heart attack", 
##                    "heart failure", or "pneumonia".
## Purpose: best reads the outcome-of-care-measures.csv file and 
##          returns a character vector with the name of the hospital 
##          that has the best (i.e. lowest) 30-day mortality for the 
##          specied outcome in that state.  
## Notes:   

best <- function(state, outcome) {
  conditions <- c("heart attack", "heart failure", "pneumonia")
  
  ## Read outcome data
  ocData <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  allStates <- unique(ocData[,7])
  
  ## Check that state and outcome are valid
  if (!(state %in% allStates)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% conditions)) {
    stop("invalid outcome")
  }
  
  ## force the column to be numeric
  if (outcome == conditions[1]) ocData[,11] <- suppressWarnings(as.numeric(ocData[,11]))
  if (outcome == conditions[2]) ocData[,17] <- suppressWarnings(as.numeric(ocData[,17]))
  if (outcome == conditions[3]) ocData[,23] <- suppressWarnings(as.numeric(ocData[,23]))
  
  ## Split the overall data by state and get just the state we're interested in
  ocDataByState <- split(ocData,ocData[,7])
  stateData <- na.omit(ocDataByState[[state]])
  
  ## Sort the results alphabetically by Hospital Name
  stateData <- stateData[order(stateData$Hospital.Name),]
  
  ## Find the lowest result based on condition
  if (outcome == conditions[1]) results <- stateData[which.min(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]$Hospital.Name
  if (outcome == conditions[2]) results <- stateData[which.min(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]$Hospital.Name
  if (outcome == conditions[3]) results <- stateData[which.min(stateData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]$Hospital.Name
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  results
  
}
