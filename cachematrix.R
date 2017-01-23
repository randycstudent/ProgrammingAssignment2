## Below are two functions that are used to create a special object
## that stores a numeric matrix and cache's its inverse.

## The first function, makeCacheMatrix, creates a list containing
## a matrix, potentially a cache for the inverse, and
## functions to do the following
##
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

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


## The following function calculates the inverse of the matrix
## associated with the object created with the above function.
## However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse in the cache via the
## setinverse function.

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
