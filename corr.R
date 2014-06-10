#################################################
# D. Kessler R Coursera June 2014
# correlation between sulfates and nitrates
# where number of completed cases on all variables
# is greater than the threshold
#################################################
# usage:
# cr <- corr("specdata", 150)
#################################################

# mydebug levels, 0 none, 1 some, 2 verbose
myDebug <- 0

corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    setwd(paste("C:/Users/dak/My Documents/R/week2_AirPollution/",directory, sep= ""))
        
    allFiles <- list.files(pattern="csv")
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    # get completed first, then a subset greater than threshold
    myCompleted <- complete(directory, 1:332)
    myCompleted.sub <- subset(myCompleted, nobs > threshold )
    
    if( myDebug == 1 ){
        print(getwd())
        print(allFiles)
        print(myCompleted.sub)
        print(class(myCompleted.sub))
        print(nrow(myCompleted.sub))
    }
    
    # do we have any data to process
    myResult <- numeric(0)
     
    # find correlation between the columns
    # myCompleted column 1 is file name, column 2 is sum of completed cases
    # so filenames are column 1 for all the rows
    for(i in seq_len(nrow(myCompleted.sub)) ){
        myData <- na.omit(read.csv(paste(sprintf("%03d", myCompleted.sub[i,1]), ".csv", sep="")))
        thisFileResult <- round( (cor(myData$sulfate, myData$nitrate)), digits=4 )
        myResult <- c(myResult, thisFileResult)
    }
       
    ## Return a numeric vector of correlations
    return( myResult )
}


