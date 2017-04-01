## This is the code for the programming assignment of week 3.

## Holds variables in memory for:
## - The input matrix
## - It's inverse (when required)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i

    list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## Calculates the inverse of the matrix in `x` if there's no cached result
## already. Otherwise, return the cached version.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (is.null(inverse)) {
            inverse <- solve(x$get(), ...)
            x$setInverse(inverse)
        }

        inverse
}
