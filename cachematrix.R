## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverseM <- NULL
    set <- function(y){
        x <<- y
        inverseM <<- NULL
    }
    get <- function() x
    setInverseM <- function(im) inverseM <<- im
    getInverseM <- function() inverseM
    list(set = set, get = get,
         setInverseM = setInverseM,
         getInverseM = getInverseM
         )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverseM <- x$getInverseM()
    if(!is.null(inverseM)){
        message("getting cached data")
        return(inverseM)
    }
    data <- x$get()
    inverseM <- solve(data) # inverse the matrix
    x$setInverseM(inverseM)
    return(inverseM)
}
