## Caching the Inverse of a Matrix
## A pair of functions "makeCacheMatrix and cacheSolve" that cache the inverse
## of a matrix

## This function creates a Matrix object that cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function uses the makCahcheMatrix above to calculate the inverse
## however it first checks to see if the inverse has already been calculated, if it has,
## it takes the inverse and skips the calculation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- inverse(data, ...)
        x$setInverse(inv)
        inv
}
