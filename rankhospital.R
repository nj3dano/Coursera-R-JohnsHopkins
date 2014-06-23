#################################################
# D. Kessler R Coursera June 2014
# Programming assignment 3
# The function reads the outcome-of-care-measures.csv 
# file and returns a character vector with the name
# of the hospital that has the ranking specified by
# the num argument.
#################################################
# usage:
# rankhospital("TX", "heart failure", 4)
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MN", "heart attack", 5000)
#################################################

# mydebug levels, 0 none, 1 some, 2 verbose
myDebug <- 0

rankhospital <- function(state, outcome, num = "best") {

    setwd("C:/Users/dak/My Documents/R/week4_hospital")
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
    # validate state
    validState <- as.character(unique(outcomeData$State)) 
    if( ! any(validState %in% state)) { stop("invalid state") }
    
    # validate outcome to search on
    # outcome must be heart attack, heart failure or pneumonia
    # columns 11, 17 and 23 in the data
    validOutcome <- matrix( c("heart attack", "11",
                              "heart failure", "17",
                              "pneumonia", "23"),
                               nrow=3, ncol=2, byrow = TRUE )
    if( ! any( validOutcome %in% outcome )) { stop("invalid outcome") }

    ## DATA IS VALID, neededColumn is 11, 17 or 23
    neededColumn = as.numeric(validOutcome[which(validOutcome %in% outcome), 2])
       
    ## get the data you want and exclude hospitals with no data
    # column 2 in origial data is hospital name
    # neededColumn in original data  is outcome, either column 11, 17 or 23
    hospitalData <- na.omit( subset(outcomeData,State == state, select = c(2,neededColumn) ) )
 
    hospitalData[, 2] <- suppressWarnings( sapply( hospitalData[, 2], as.numeric))
    
    # all the Not availables translated into NA, so lets get rid of them
    hospitalData <- na.omit(hospitalData)
    
    # now that you have the subset of data that you want
    # you can validate against rank
    myMin <- 1L
    myMax <- nrow(hospitalData)
    # check that num is valid, can be "best", "worst", or an integer
    if(num == "best") {
        rank <- myMin
    }
    else if(num == "worst") {
        rank <- myMax
    }
    else {  
        if( ! is.numeric(num) ) { stop( "invalid num rank" ) }
        rank <- as.integer(num)
        if( rank < myMin ) { stop( "invalid num rank" ) }
        if( rank > myMax ) { return (NA) }
    }
    
  
    # order all the rows by outcome, which is column 2 in subsetted data
    # order is on row, take all the columns
    hospitalData <- hospitalData[order(hospitalData[,2], hospitalData[,1], na.last=TRUE),]
   
    # Column 1 is the hospital name, take row for rank you want
    print(hospitalData[rank, 1])
}


