## Demonstration of cached matrix reverse computation

## This is "cached matrix" that can store a computed result

makeCacheMatrix <- function(m = matrix()) {
        mi <- NULL
        set <- function(y) {
                if (m != y) {
                  m <<- y
                  mi <- NULL
                }
        }
        get <- function() m
        setInverse <- function(arg_mi) mi <<- arg_mi
        getInverse <- function() mi
        list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
}


## This function checks if the matrix is the same and, if so, gets cached inverse, otherwise calculates new 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getInverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        message("computing new matrix")
        m  <- x$get()
        mi <- solve(m, ...)
        x$setInverse(mi)
        mi
}
