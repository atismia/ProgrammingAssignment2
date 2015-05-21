## makeCacheMatrix
## This function creates an interactive object which stores both
## a matrix and its inverse
## The object is in the form of a list of functions which can be 
## used to get and set the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    mSolve <- NULL
    set <- function(y) {
        x <<- y
        mSolve <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) mSolve <<- solve
    getSolve <- function() mSolve
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function can be used to obtain the inverse of a 
## makeCacheMatrix object, via the "solve" function
## If it has already been computed, it will return
## the cached inverse. Otherwise it will compute and
## store it.
cacheSolve <- function(x, ...) {
    mSolve <- x$getSolve()
    if(!is.null(mSolve)) {
        message("getting cached data")
        return(mSolve)
    }
    data <- x$get()
    mSolve <- solve(data, ...)
    x$setSolve(mSolve)
    mSolve
}
