## cachematrix.R contains a set of functions for calculating inverse matrixes.
## Subsequent calculations are quickened by caching the first calculation result.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" created by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Obtaining matrix inverse from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    ## Cache inverse
    x$setInverse(inv)
    inv
}
