## This set of functions is to substitute the solve()
## function (the inverse of the matrix) with its
## cachable version.

## Coding standard is as recommended in the course.
## Excessive comments are for training purposes only.

## This function creates a special version of matrix
## object that can keep cached version of its inverse
makeCacheMatrix <- function(x = matrix()) {
        # we're just creating the matrix, so the cached
        # version doesn't exist yet
        cachedSolve <- NULL

        # getter for x
        get <- function() x

        # setter for x
        set <- function(newX = NULL) {
                x <<- newX           # setting the variable
                cachedSolve <<- NULL # and clearing the cache
        }

        # getter for cached inverse
        getSolve <- function() cachedSolve

        # setter for cached inverse
        setSolve <- function(newSolve = NULL) cachedSolve <<- newSolve

        # cacheMatrix is really a 4-tuple, containing
        # the getters for x, and cached version of the
        # inverse matrix
        list(get=get, set=set, getSolve=getSolve, setSolve=setSolve)
}


## This function calculates the inverse of a special
## version of matrix object and save it in its cache.
## If the inverse has been already calculated, it returns
## the cached version.
cacheSolve <- function(x, ...) {
        # attempt to get the cached version
        cachedSolve <- x$getSolve()

        # attempt successful: return the cached version
        if (!is.null(cachedSolve)) {
                message("getting cached data")
                return(cachedSolve)
        }

        # attempt failed: recalculate, cache and return
        message("recalculating")
        cachedSolve <- solve(x$get(), ...)
        x$setSolve(cachedSolve)
        cachedSolve
}
