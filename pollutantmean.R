#################################################
# D. Kessler R Coursera June 2014
# Calculate the mean of a pollutant (sulfate or nitrate) 
# across a specified list of monitors
#################################################
# Usage: for example
# source("pollutantmean.R")
# pollutantmean("specdata", "sulfate", 1:10)
# [1] 4.064
# pollutantmean("specdata", "nitrate", 70:72)
# [1] 1.706
# pollutantmean("specdata", "nitrate", 23)
# [1] 1.281
#################################################

# mydebug levels, 0 none, 1 some, 2 verbose
myDebug <- 0

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files, vector is length one, not
    ## the size of the charaters making up the vector, ie a string
    
    setwd(paste("C:/Users/dak/My Documents/R/week2_AirPollution/",directory, sep= ""))
    if (myDebug == 1 ) print(getwd())
        
    allFiles <- list.files(pattern="csv")
    if (myDebug == 1 ) {print(allFiles)}
        
    myTotal <- data.frame()
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    ## EXTRACT THE RELEVANT COLUMN
    for ( monitorFile in id ){
        if (myDebug == 1 ) {print(paste("looking at file", allFiles[monitorFile]))}
      
        monitorFileData <- read.csv( allFiles[monitorFile], header = TRUE, sep = ",")
        if (myDebug == 1 ) {print( tail(monitorFileData) )}
        
        # use the pollutant type to extract the the relevant column
        # this results in a data.frame, with n rows, and 1 column
        relevantColumn <- monitorFileData[pollutant]
                                        
        # to join dat sets vertically, use rbind, you end up with objects
        # data frame and not a vector with elements
        myTotal <- rbind(myTotal, relevantColumn)
    }
       
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values), so the one column of all rows
    round( mean(myTotal[,1], na.rm = TRUE), 3)
}