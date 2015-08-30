

#setwd("/Users/Croninio/GitHub/prog3")

#outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character",na.strings = 'Not Available')

#outcome[,11] <- as.numeric(outcome[,11])
#hist(outcome[,])

state <- 'CA'
outcome <- 'heart attack'

best <- function(state,outcome){
    
    #Read outcome data
    hospData <- read.csv("outcome-of-care-measures.csv",colClasses = "character",na.strings = 'Not Available')
    hospData[,11] <- as.numeric(hospData[,11])
    hospData[,17] <- as.numeric(hospData[,17])
    hospData[,23] <- as.numeric(hospData[,23])
    hospData <- hospData[,c(2,7,11,17,23)]
    colnames(hospData) <- c('name','state','heart attack','heart failure','pneumonia')

    ##Check that state and outcome are valid
    states <- unique(hospData[["state"]])
    outcomes <- c('heart attack','heart failure','pneumonia')
    if (!(state %in% states)) stop("invalid state")
    if (!(outcome %in% outcomes)) stop("invalid outcome")
    
    ##Return hospital name in that state wiht the lowest 30-day death rate
    stateVector <- hospData[,c('state')] == state
    hospData.state <- na.omit(hospData[stateVector,c('name',outcome)])
    minOutcome <- min(hospData.state[,c(outcome)])
    minVector <- hospData.state[,c(outcome)] == minOutcome 
    winners <- sort(hospData.state[minVector,c('name')])
    winners
}