## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        m <<- inverse
    }
    
    getinverse <- function() {
        m
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    
    if (!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    
    data <- x$get()

    ## if X is a square invertible matrix, then solve(X) returns its inverse
    m <- solve(data, ...)

    x$setinverse(m)
    m
}

############ Test Run ############
x <- matrix(c(1:4, 1, 6:9), 3, 3)
##     [,1] [,2] [,3]
##[1,]    1    4    7
##[2,]    2    1    8
##[3,]    3    6    9

y <- makeCacheMatrix(x)
cacheSolve(y)
##        [,1]   [,2]       [,3]
##[1,] -0.8125  0.125  0.5208333
##[2,]  0.1250 -0.250  0.1250000
##[3,]  0.1875  0.125 -0.1458333

cacheSolve(y)
##Getting cached data
##        [,1]   [,2]       [,3]
##[1,] -0.8125  0.125  0.5208333
##[2,]  0.1250 -0.250  0.1250000
##[3,]  0.1875  0.125 -0.1458333
############ Test Run ############
