## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        f <- function(y) {
            x <<- y
            inv <<- NULL
        }
        x <- function() x
        fInverse <- function(inverse) inv <<- inverse
        xInverse <- function() inv
        list(f = f, x = x,
             fInverse = fInverse,
             xInverse = xInverse)
    }
cacheSolve <- function(x, ...) {
        inv <- x$xInverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$x()
        inv <- solve(data, ...)
        x$fInverse(inv)
        inv
    }


