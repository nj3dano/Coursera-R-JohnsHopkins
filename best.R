#################################################
# D. Kessler R Coursera June 2014
# Programming assignment 3
# Returns a character vector with the name of the
# hospital that has the best (i.e. lowest) 30-day
# mortality for the specified outcome
# in that state
#################################################
# usage:
# best("TX", "heart attack")
# best("MD", "heart attack")
# best("MD", "pneumonia")
#################################################

# mydebug levels, 0 none, 1 some, 2 verbose
myDebug <- 0

best <-function(state, outcome){
    ## read outcome data
    setwd("C:/Users/dak/My Documents/R/week4_hospital")
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    if (myDebug == 1){
        head(outcomeData)
        print( paste("outcome has rows:", nrow(outcomeData)) ) 
        print( paste("outcome has cols:", ncol(outcomeData)) )
        print( paste("column names are:", names(outcomeData)) )
    }
    
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
       
    # order all the rows by outcome, which is column 2 in subsetted data
    # order is on row, take all the columns
    hospitalData <- hospitalData[order(hospitalData[,2], na.last=TRUE),]
   
    return( hospitalData[1,1] )  

}


