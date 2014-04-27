## This program takes a matrix, computes its inverse, and caches the result.

## The following function makeCacheMatrix creates a "special matrix" object 
## that can cache its inverse. The function sets the value of the matrix, 
## retrieves the value of the matrix, sets the value of the solved inverse, 
## and then retrieves that value. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## The following function cacheSolve calculates the inverse of the matrix 
## created with the above function. If a calculation is already stored in 
## cache, it skips the computation.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m ## Return a matrix that is the inverse of 'x'
}