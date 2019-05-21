## These functions are used in combination to cache the inverse of a matrix.
## If the inverse has already been calculated, it retrieves it from memory;
## if it hasn't already been calculated, it does so and then stores it for
## retrieval.

## makeCacheMatrix takes a matrix and returns a list that contains functions for
## storing and retrieving both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL 
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve checks for an existing inverse and returns it if it exists,
## otherwise it retrieves the matrix, computes its inverse, adds that to the
## cache, and then returns it

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}


