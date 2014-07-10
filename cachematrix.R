## Coursera - R Programming Course - Programming Assignment 2
## Prepared by d.heatherly@verizon.net
## July 2014
##
## This script includes two functions that can be used to cache a matrix
## and the inverse solution of that matrix in an environment for reuse
## without having to recompute.  This is helpful if you need to reuse the
## inverse in a loop or program and it takes a long time to compute.
 
## The makeCacheMatrix function checks to see if the matrix data is 
## already cached and if not caches the matrix for reuse. You should
## run this and save the output into a variable for input into the
## cacheSolve function. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## The cacheSolve function checks to see if the inverse has already been 
## cached and if not computes the solution and caches for reuse.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
