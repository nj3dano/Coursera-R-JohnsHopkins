#################################################
# D. Kessler R Coursera June 2014
# two functions used to cache inverse of a matrix
#################################################

# mydebug levels, 0 none, 1 some, 2 verbose
myDebug <- 0

#################################################
# makeCacheMatrix: parent function that encloses
# 4 closure child functions that: set the value
# of the matrix via set(), get the value of the
# matrix via get(), set the value of inverse via
# setInverse(),and get the value of the inverse
# via getInverse()
# RETURNS: a list containing the 4 functions
#################################################

makeCacheMatrix <- function(x = matrix()) {
        
    myInverse <- NULL
        
    # method 1, set the matrix raw values and null the inverse values
    set <- function(y) {
        x <<- y
        myInverse <<- NULL
    }
    
    # method 2, get matrix values
    get <- function() x
    
    # method 3, set the inverse, need << operator
    setInverse <- function(myNewValue) myInverse <<- myNewValue
    
    # method 4, get the inverse
    getInverse <- function() myInverse
    
    # return the list of enclosed functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}    


#################################################
# cacheSolve: Returns a matrix that is the inverse
# of 'x'.  First check if a value is stored for
# the matrix, if so return it; else compute it
#################################################

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    cachedInverse <- x$getInverse()
    if( !is.null(cachedInverse) ){
        message("getting cached data")
        return( cachedInverse )
    }
    
    # set the inverse, using the raw matrix data
    # message("computation is necessary, no cached data")
    myData <- x$get()
    newInverse <- solve(myData)
    x$setInverse(newInverse)
    return( newInverse )
}

#################################################
# TEST
#################################################
if (myDebug == 1) {
    
    # Call makeCacheMatrix() function and assign it's
    #  return to a variable, v is now a list of four functions
    v <- makeCacheMatrix()
    
    ###########
    # TEST 1
    ###########
    message("+++++++++++ TEST 1 ++++++++++++")

    # use v's set function to create a matrix, myData
    ##       [,1]  [,2]
    ## [1,]  1.00 -0.25
    ## [2,] -0.25  1.00
    myData <- rbind(c(1, -1/4), c(-1/4, 1))
    v$set(myData)
    print(v$get())
    
    # pass the function list v to cacheSolve()
    # The inverse matrix should be returned
    ##           [,1]      [,2]
    ## [1,] 1.0666667 0.2666667
    ## [2,] 0.2666667 1.0666667
    print(cacheSolve(v))

    # pass the function list v to cacheSolve() again
    # This time, no computation is done
    # and you see a message "getting cached data" 
    print(cacheSolve(v))
    
    #print( (v$get() %*% cacheSolve(v) ) )
    #print(all.equal( diag(2), v$get() %*% cacheSolve(v) ))
    
    ###########
    # TEST 2
    ###########
    
    message("+++++++++++ TEST 2 ++++++++++++")
    
    # use v's set function to create a matrix, myData
    #       [,1] [,2]
    # [1,]    4    7
    # [2,]    2    6
    myData <- matrix(c(4,2,7,6), nrow=2)
    v$set(myData)
    print(v$get())
    
    # pass the function list v to cacheSolve()
    # The inverse matrix should be returned
    #        [,1] [,2]
    ## [1,]  0.6 -0.7
    ## [2,] -0.2  0.4
    
    #message("Expect data to be computed, nothing yet in cache")
    print(cacheSolve(v))
 
    #message("Expect output getting cached data")
    print(cacheSolve(v))
}
