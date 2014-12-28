
## rankall.R
## Author: Jack Gidding
## 

## Fn name: rankall
## Args:    
##          outcome: outcome name, which is one of "heart attack", 
##                    "heart failure", or "pneumonia".
##          num: ranking of a hospital in that state for that outcome.
##                The num argument can take values "best", "worst", 
##                or an integer indicating the ranking (smaller 
##                numbers are better)
## Purpose: rankall reads the outcome-of-care-measures.csv file and 
##          returns a 2-column data frame containing the hospital in
##          each state that has the ranking specified in num.  
## Notes:   

rankall <- function(outcome, num = "best") {
  
  conditions <- c("heart attack", "heart failure", "pneumonia")
  hName <- "Hospital.Name"
  sName <- "State"
  cNames <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
              "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
              "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  ## Read outcome data
  ocData <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  allStates <- unique(ocData[,7])

  ## Check that the outcome is valid
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

  ## Drop all the columns except the ones were interested in  
  ocData <- ocData[c(sName, hName, useCol)] 
  
  ## Get rid of the bad data
  ocData <- na.omit(ocData)
  
  ## Sort the results by Value then Hospital Name
  sortedData <- ocData[order(ocData[useCol],ocData[hName]),]

  ## Split the overall data by state
  ocDataByState <- split(sortedData,sortedData[,1])
  
  # Create an empty dataframe
  df <- data.frame(hospital=character(), state=character())
  
  ## Build the resulting data frame
  for (i in allStates) {
    
    ## Get the data for each state
    myState <- ocDataByState[[i]]
    
    ## Build the next data frame row for each state
    if (num=="best") {
      df <- rbind(df,data.frame(hospital=myState[1,2],state=i))
    }
    else if (num =="worst") {
      df <- rbind(df,data.frame(hospital=myState[nrow(myState),2],state=i))
    }
    else {
      if (num>nrow(myState)) {
        df <- rbind(df, data.frame(hospital=NA,state=i))
      } 
      else {
        df <- rbind(df,data.frame(hospital=myState[num,2],state=i))
      }
    }       
  }
  
  # Sort the resulting data frame for good measure
  df <- df[order(df["state"]),]
  
  ## return 
  df
}
