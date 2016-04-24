## ---------------------------------------------------------
## This script will cache the inverse of a matrix.
##
## The following script contains two functions:
##      1. makeCacheMatrix()
##      2. cacheSolve()
## ---------------------------------------------------------
## Function makeCacheMatrix() creates a special matrix which
## really creates a list containing a function to:
##  1. set the matrix
##  2. get the matrix
##  3. set the inversed matrix
##  4. get the inversed matrix
## ---------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInversed <- function(solve) inv <<- solve
    getInversed <- function() inv
    list(set = set, get = get,
        setInversed = setInversed,
        getInversed = getInversed)
}
## ---------------------------------------------------------    
## cacheSolve() function will return the inversed matrix  
## stored in cache in case of previous calls, or will solve 
## and return the inversed matrix using 'solve' function.
##
## ---------------------------------------------------------
cacheSolve <- function(x, ...) {
    inv <- x$getInversed()

    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setInversed(inv)
    inv
}
