## This file consists of two functions:  makeCacheMatrix() and cacheSolve()
##
## These two functions work together to efficiently deliver the inverse of a matrix
## by caching the inverse of a matrix after it is computed once.  On repeated requests
## for the inverse of the matrix, the cached value is returned rather than recomputing the inverse.
## After creating the cache matrix using makeCacheMatrix(), cacheSolve() is called to get the inverse.
##
## Example of how to use these functions together to get the inverse of |2 1|:
##                                                                      |1 2|
## a <- matrix(c(2,1,1,2), 2, 2)
## cachea <- makeCacheMatrix(a)
## ainv <- cacheSolve(cachea)


## The function makeCacheMatrix() creates a cache version of the matrix.
## While the function can be used itself, it is primarily used in conjuction
## with cacheSolve().
##
## args:  x=matrix() -- The matrix to be cached.  By default, x is set to the
##        1x1 empty matrix.
##
## returns:  list of 4 functions supported by the cache matrix (get, set,
##           getinverse, setinverse)
##
## getinverse will return NULL until the inverse is set with the setinverse
## function

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invx <<- inverse
    getinverse <- function() invx
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve() returns the inverse of a cacheMatrix object
## created with makeCacheMatrix().  The first call will compute and store
## and return the inverse.  All subsequent calls will return the cached
## inverse.
##
## args:  x    -- the cacheMatrix to return the inverse of
##        ...  -- additional parameters that are passed to solve() when
##                computing the inverse of the matrix.
##
## returns:  inverse of x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invx <- x$getinverse()
    if(!is.null(invx)) {
        message("getting cached matrix inverse")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$setinverse(invx)
    invx
}
