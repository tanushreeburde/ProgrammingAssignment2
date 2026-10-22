## Functions that create a special matrix object capable of caching 
## its inverse, avoiding costly recomputations.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## Method to set the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Method to get the matrix
    get <- function() {
        x
    }
    
    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    ## Method to get the inverse of the matrix
    getInverse <- function() {
        i
    }
    
    ## Return a list of the methods
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    
    ## If the inverse is already cached, return it
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Otherwise, get the matrix, calculate the inverse, and cache it
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}