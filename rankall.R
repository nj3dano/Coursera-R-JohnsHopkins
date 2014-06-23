#################################################
# D. Kessler R Coursera June 2014
# Programming assignment 3
# The function reads the outcome-of-care-measures.csv
# file and returns a 2-column data frame
# containing the hospital in each state that has
# the ranking specified in num
#################################################
# usage:
# head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"), 3)
# tail(rankall("heart failure"), 10)
#################################################

# mydebug levels, 0 none, 1 some, 2 verbose
myDebug <- 0

rankall <- function(outcome, num = "best") {

    setwd("C:/Users/dak/My Documents/R/week4_hospital")
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
    # validate outcome to search on
    # outcome must be heart attack, heart failure or pneumonia
    # columns 11, 17 and 23 in the data
    validOutcome <- matrix( c("heart attack", "11",
                              "heart failure", "17",
                              "pneumonia", "23"),
                               nrow=3, ncol=2, byrow = TRUE )
    if( ! any( validOutcome %in% outcome )) { stop("invalid outcome") }

    ## DATA IS VALID, neededColumn for outcome is 11, 17 or 23
    neededColumn = as.numeric(validOutcome[which(validOutcome %in% outcome), 2])
       
    ## get the data you want and exclude hospitals with no data
    # column 2 in origial data is hospital name
    # neededColumn next, in original data  is outcome, either column 11, 17 or 23
    # column 7 is state and that will be our third pull out of the data
    hospitalData <- subset(outcomeData,select = c(2,neededColumn, 7) )
 
    hospitalData[, 2] <- suppressWarnings( sapply( hospitalData[, 2], as.numeric))
    
    # all the Not availables translated into NA, so lets get rid of them
    hospitalData <- na.omit(hospitalData)
    
    ##########################################
    # hospitalData column 1 is hospital name
    # hospitalData column 2 is outcome
    # hospitalData column 3 is state
    ##########################################
    
    # split the data by state
    DataByState <- split( hospitalData, hospitalData$State )
    
    # get a list of the states to traverse
    myStates <- sort(unique(hospitalData[,3]))
      
    # create a data frame for result, include every state. may be NA
    # This creates a frame, puts NA in it, sets the names of the
    # columns, then removes the NA rows, leaving a blank frame
    # http://www.yoursearchbuddy.com/create-empty-data-frame-splus
    HospitalRank <- data.frame(t(rep(NA, 2)))
    names(HospitalRank) <- c("hospital", "state")
    HospitalRank <- HospitalRank[-1,]
    
    # DataByState columns are the same as hospitalData
    # order by outcome, then hospital within each state
    # reference a data frame column with  double square bracket "[[]]"
    
    for( i in names(DataByState) ){
        ThisStateData <- DataByState[[i]]
        
        # sort by outcome (col 2), then hospital (col 1)
        orderedData <- ThisStateData[order(ThisStateData[,2], ThisStateData[,1], na.last=TRUE),]
        orderedData <- na.omit(orderedData)
         
        # now that you have the subset of data that you want
        # you can validate against rank
        myMin <- 1L
        myMax <- nrow(orderedData)
   
        if(num == "best") {
            rank <- myMin
        }
        else if(num == "worst") {
            rank <- myMax
        }
        else {
            rank <- num
        }
               
        # default for row, 2 columns, hospital and state
        myRow <- c(NA, i)
 
        if( is.numeric(rank) ) {       
            rank <- as.integer(rank)
            if( rank >= myMin  & rank <= myMax ) {
                # column 1 in orderedData is hospital
                # take hospital for the row you want known by rank
                # i is the state
               myRow <- c(orderedData[rank,1], i)
            }
        }
        HospitalRank[i,] <- myRow
    }
    
    # now order the final result
    HospitalRank <- (HospitalRank[order(HospitalRank[, 2]),])
    return(HospitalRank)
}


