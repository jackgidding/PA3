
## rankhospital.R
## Author: Jack Gidding
## 

## Fn name: rankhospital
## Args:    state: 2-character abbreviated name of a state 
##          outcome: outcome name, which is one of "heart attack", 
##                    "heart failure", or "pneumonia".
##          num: ranking of a hospital in that state for that outcome.
##                The num argument can take values "best", "worst", 
##                or an integer indicating the ranking (smaller 
##                numbers are better)
## Purpose: rankhospital reads the outcome-of-care-measures.csv file 
##          and returns a character vector with the name of the 
##          hospital that has the ranking specified by the num argument  
## Notes:   

rankhospital <- function(state, outcome, num="best") {
  
  conditions <- c("heart attack", "heart failure", "pneumonia")
  hName <- "Hospital.Name"
  cNames <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
              "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
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
  
  ## force the column to be numeric. Save the column name of interest
  if (outcome == conditions[1]) {
    ocData[,11] <- suppressWarnings(as.numeric(ocData[,11]))
    useCol <- cNames[1]
  }
  if (outcome == conditions[2]) {
    ocData[,17] <- suppressWarnings(as.numeric(ocData[,17]))
    useCol <- cNames[2]
  }
  if (outcome == conditions[3]) {
    ocData[,23] <- suppressWarnings(as.numeric(ocData[,23]))
    useCol <- cNames[3]
  }
  
  ## Split the overall data by state and get just the state we're interested in
  ocDataByState <- split(ocData,ocData[,7])
  myState <- ocDataByState[[state]]
  
  ## Drop all the columns except the ones were interested in
  myState <- myState[c(hName,useCol)]
  
  ## Get rid of the bad data
  stateData <- na.omit(myState)
  
  ## How many rows of hospital data do we have?
  numHospitals <- nrow(stateData)
  
  ## Sort the results by Value then Hospital Name
  stateData <- stateData[order(stateData[useCol],stateData[hName]),]
  
  if (num=="best") {
    result <- stateData[1,1]
  } else if (num == "worst") {
    result <- stateData[numHospitals,1]
  } else {
    ##If the number given by num is larger than the number of hospitals
    ##in that state, then the function should return NA.
    if (num > numHospitals) {
      return(NA)
    }
    result <- stateData[num,1]
  }
  
  result  
}
