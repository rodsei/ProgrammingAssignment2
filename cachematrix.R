## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions to control the cached values within the cacheSolve function.


## Write a short comment describing this function
## This function gets the matrix passed and store into the cache for future use, returning a list of functions to handle it.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
## This function verifies if the cache is set and uses it's value. If not, recalculate the matrix solve.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
