## A set of functions that creates a matrix, and calculates it's inverse and 
## stores it in a "cache" for fast retrieval.


## This matrix function returns a list consisting of

## a function to set the value of the matrix
## a function to retrieve the value of the matrix
## a function to store the value of the inverse
## a function to retrive the value of the inverse

## Together this list acts as if it were a matrix witrh the added advantage of
## having the inverse stored when it is calculated and being available for fast
## retrival

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL  #sets the inv to NULL on setting a new value
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return the inverse of a makeCacheMatrix "matrix".
## It tests if a value of the inverse is already stored in the "cache"
## If so it returns the value stored in the "cache"
## If not, it calculates the inverse directly as well as stores the value.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinverse()
    if(!is.null(inv)) {
        # Returns the inverse from "cache" directly.
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) # Calculates the inverse.
    x$setinverse(inv) # Stores the inverse.
    inv # Returns the cache.
}