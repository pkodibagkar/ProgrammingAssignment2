## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invOfX <- NULL
    set <- function(y) {
        x <<- y
        invOfX <<- NULL
    }
    get <- function() x
    setinverse <- 
        function(inverseOfx) {
            invOfX <<- inverseOfx
        }
    getinverse <- 
        function(){
            invOfX
        }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invOfX <- x$getinverse()
    if(!is.null(invOfX)) {
        message("getting cached data")
        return(invOfX)
    }
    data <- x$get()
    invOfX <- solve(data, ...)
    message("Setting cached data")
    x$setinverse(invOfX)
    invOfX
}
