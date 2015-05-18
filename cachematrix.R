## MakeCacheMatrx 
## This function initializes the matrix and returns a list of functions to set/get the matrix and its inverse values 

makeCacheMatrix <- function(x = matrix()) {
    ## cached variable
    invOfX <- NULL
    
    ## set function to set initialize the variables
    set <- function(y) {
        x <<- y
        invOfX <<- NULL
    }
    
    ## getter function to get the value of matrix
    get <- function() x
    
    ## set function to set the inverse of the matrix
    setinverse <- 
        function(inverseOfx) {
            invOfX <<- inverseOfx
        }
    
    ## getter function to get the value of matrix inverse
    getinverse <- 
        function(){
            invOfX
        }
    
    ## returns a list of functions 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve 
## This function calculates the inverse of the matrix provided and caches the calculated inverse. 
## The cached value of the inverse matrix is returned if this function is invoked multiple times

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invOfX <- x$getinverse()
    ## check if inverse is already been calculated and return the cached value
    if(!is.null(invOfX)) {
        message("getting cached data")
        ## return the cached inverse
        return(invOfX)
    }
    ## calculate the inverse of the matrix and cache it
    data <- x$get()
    invOfX <- solve(data, ...)
    message("Setting cached data")
    x$setinverse(invOfX)
    ## return the inverse
    invOfX
}
