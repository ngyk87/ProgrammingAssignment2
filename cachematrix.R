## These functions are written in partial fulfilment of Coursera Data Science Specialization:
## R Programming Week 3 Peer-review Assignment. 
##Github user:nyk87
##Matrix inversion is usually a costly computation and there may be some benefits to caching the inverse of a matrix rather 
## Below are a pair of functions that can be used to create a special object that stores a matrix and cache its inverse

## The following function below, makeCacheMatrix, creates a special "Matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {   
        inv <- NULL
        ## The following function below set the value of the matrix, and clear existing
        ## inverse matrix from the cache
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## The following function below get the value of the matrix
        get <- function() x
        ## The following function below set the inverse matrix. 
        setInverse <- function(inverse) inv <<- inverse
        ## The following function get the inverse matrix.
        getInverse <- function() inv
        ## Return a list with the 4 functions above.
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The following function below, returns the inverse of special "matrix" above created by makeCacheMatrix function
## above.In the event that the inverse has already been calculated, while the matrix has not been changed,
## cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## The following fetches the cached value for the inverse matrix       
        inv <- x$getInverse()
        ## If the cache was not empty, return the inverse matrix
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If the cache is empty, calculation of the inverse, cache and return.
        ## Get the value of the matrix
        mat <- x$get()
        ## Calculate the inverse matrix
        inv <- solve(mat, ...)
        ## Cache the result
        x$setInverse(inv)
        ## Return the inverse matrix
        inv
}
