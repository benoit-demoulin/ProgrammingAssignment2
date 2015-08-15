## The makeCacheMatrix function creates a 'CacheMatrix' structure that can embed a matrix and its inverse.
## The cacheSolve function relies on the CacheMatrix structure created by the makeCacheMatrix function to speed up accesses to the inverse of a matrix by avoiding recomputing the inverse if it has already been computed (and stored in the supplied CacheMatrix structure).

## Function that creates a CacheMatrix structure
makeCacheMatrix <- function(x = matrix()) {
        inversedMatrix <- NULL
        ## Function that resets the inversed matrix in the structure
        setMatrix <- function(y) {
                x <<- y
                inversedMatrix <<- NULL
        }
        ## Function that returns the matrix located in the calling environment
        getMatrix  <- function()        { x }
        ## Function that sets the inversed matrix into the structure
        setInverse <- function(inverse) { inversedMatrix <<- inverse }
        ## Function that returns the inversed matrix from the structure
        getInverse <- function()        { inversedMatrix }
        ## Return the structure
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}

## Function that computes the inverse of a matrix via a CacheMatrix structure
cacheSolve <- function(cachedMatrix, ...) { ## Return a matrix that is the inverse of x, via a 'cachedMatrix' structure
        ## Try to get the cached inversed matrix from CacheMatrix structure
        m <- cachedMatrix$getInverse()
        ## Return the cached inversed matrix if it exists
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Otherwise, get the original matrix
        data <- cachedMatrix$getMatrix()
        ## Compute the inversed matrix
        m <- solve(data, ...)
        ## Store the inversed matrix in the CacheMatrix structure
        cachedMatrix$setInverse(m)
        ## Return the inversed matrix
        m
}
