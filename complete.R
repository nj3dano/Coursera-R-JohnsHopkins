#################################################
# D. Kessler R Coursera June 2014
# reports the number of completely observed cases in each data file 
# across a specified list of monitors
#################################################
# Usage: for example
# source("complete.R")
# complete("specdata", 1)
#   id nobs
# 1  1  117
# complete("specdata", c(2, 4, 8, 10, 12))
#   id nobs
# 1  2 1041
# 2  4  474
# 3  8  192
# 4 10  148
# 5 12   96
#################################################

# mydebug levels, 0 none, 1 some, 2 verbose
myDebug <- 0

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    setwd(paste("C:/Users/dak/My Documents/R/week2_AirPollution/",directory, sep= ""))
    if (myDebug == 1 ) print(getwd())
    
    allFiles <- list.files(pattern="csv")
    if (myDebug == 1 ) {print(allFiles)}
    
    CompletedPerFile <- rep(0, length(id))

    ## 'id' is an integer vector indicating the monitor ID numbers to be user
    i <- 1
    for ( monitorFile in id ){
        if (myDebug == 1 ) {print(paste("looking at file", allFiles[monitorFile]))}
        
        monitorFileData <- read.csv( allFiles[monitorFile], header = TRUE, sep = ",")
        if (myDebug == 1 ) {print( tail(monitorFileData) )}
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases (i.e no missing data)
        CompletedPerFile[i] <- sum(complete.cases(monitorFileData))
        if (myDebug == 1 ) {print( CompletedPerFile )}
        
        i <- i + 1    
    }
    
    ## create the data frame, using the vector of ids
    ## and the vector of the sums
    
    return ( (data.frame(id = id, nobs = CompletedPerFile)) )
    
  
}