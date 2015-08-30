

#setwd("/Users/Croninio/GitHub/prog3")

#outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character",na.strings = 'Not Available')

#outcome[,11] <- as.numeric(outcome[,11])
#hist(outcome[,])

#outcome <- 'heart attack'
#num <- 20

rankall <- function(outcome,num="best"){
    
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
    if (!(outcome %in% outcomes)) stop("invalid outcome")

    #Create dataframe for final results
    stateRank <- data.frame(Hospital= character(),
                            State = character()
    )
    
    ##Return the hospital name in that state with a given rank
    for (state in sort(states)){

        stateVector <- hospData[,c('state')] == state
        hospData.state <- na.omit(hospData[stateVector,c('name',outcome)])
        colnames(hospData.state) <- c('name','rate')
        
        index <- with(hospData.state,order(rate,name))
        
        hospData.ordered <- hospData.state[index,]
        n <- nrow(hospData.ordered)
        
        hospData.rank <- cbind(hospData.ordered,1:n) 
        colnames(hospData.rank) <- c('Hospital.Name','Rate','Rank')
        
        if(num == "best"){
            hospital <- hospData.rank[hospData.rank$Rank == 1,c('Hospital.Name')]
        }else if(num == "worst"){
            hospital <- hospData.rank[hospData.rank$Rank == n,c('Hospital.Name')]
        }else if(as.numeric(num) > n){
            hospital <- NA
        }else{
            hospital <- hospData.rank[hospData.rank$Rank == as.numeric(num),c('Hospital.Name')]
        }
        
        stateRankRow <- data.frame(hospital,state)
        
        stateRank <- rbind(stateRank,stateRankRow)
    }
    
    stateRank
}