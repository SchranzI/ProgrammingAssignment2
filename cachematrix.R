## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.

## Below there are two functions that cache the inverse of a matrix.

## Creates matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }

        get <- function() x

        setInverse <-function(inverse) iv <<- inverse
        getInverse <- function() iv

        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## computes the matrix returned by makeCacheMatrix
## If inverse has already been calculated then this function retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv <- x$getInverse()

        ## inverse alredy calculated
        if(!is.null(iv)){
                return(iv)
        } 
        
        matrix <- x$get()
        iv <- solve(matrix, ...)
        x$setInverse(iv)
        
        ## output inverse matrix
        iv
}
