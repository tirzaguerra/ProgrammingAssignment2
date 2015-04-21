## R Programming - Assignment 2
## Code written by: Tirza Guerra - 21/April/2015
## 
## The functions below implement caching of the Inverse of a Matrix. 
## Matrix inversion is usually a costly computation and caching the inverse of 
## a matrix rather than compute it repeatedly can save time, specially for
## large matrices.

## Function: makeCacheMatrix
## Purpose : Creates a list containing 4 functions: 
##              - set: set the value of the matrix
##              - get: get the value of the matrix
##              - setinverse: set the value of the inverse of the matrix
##              - getinverse: get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## Function: cacheSolve
## Purpose : Returns the inverse of the matrix associated with the list created
##           in the function above. First, it checks if the inverse has already
##           been calculated. If so, it gets the inverse from the cache.
##           Otherwise, it calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if (!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
