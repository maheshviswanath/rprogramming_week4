##collect the facts and measures from source data frame to getData
##check parameters
##set measure based on input outcome
##collect subset of data into temporary data frame
##convert dataframe from factor to character, numeric
##order the dataframe
##hospital name

rankhospital <- function(stateIn, outcomeIn, numIn = "best") {
  ## Read outcome data
  getData <- outcome[c(2,7,11,17,23)]
  names(getData)[1] <- "hospital"
  names(getData)[2] <- "state"
  names(getData)[3] <- "heart attack"
  names(getData)[4] <- "heart failure"
  names(getData)[5] <- "pneumonia"
  
  ## Check that state and outcome are valid
  if(!(stateIn %in% unique(getData[,2]))){
    stop("invalid state")
  }
  if(!(outcomeIn %in% names(getData))){
    stop("invalid outcome")
  }
  
  if(outcomeIn == "heart attack") measureId <- 1
  else if(outcomeIn == "heart failure") measureId <- 2
  else if(outcomeIn == "pneumonia") measureId <- 3
  
  ## Return hospital name in that state with lowest 30-day death ## rate
  calcData <- cbind(getData$hospital[getData$state==stateIn],getData[getData$state==stateIn,measureId+2])
  calcData <- as.data.frame(calcData)
  names(calcData)[1] <- "hospital"
  names(calcData)[2] <- "measure"
  
  suppressWarnings(calcData[,1] <- as.character(calcData[,1]))
  suppressWarnings(calcData[,2] <- as.numeric(as.character(calcData[,2])))
  
  if(numIn == "best"){
    calcData <- calcData[
    with(calcData, order(measure, hospital)),
    ]
  } else if(numIn == "worst"){
    calcData <- calcData[
      with(calcData, order(-measure, hospital)),
      ]
  } else {
    calcData <- calcData[
      with(calcData, order(measure, hospital)),
      ]
  }
  
  ##calcData
  if(numIn > nrow(calcData)){
    NA
  }
  else{
    tail(head(calcData,as.numeric(numIn)),1)[1,1]
  }
}

##outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
##source("rankhospital.R")
