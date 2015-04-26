## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 

## This test assignment is to write a pair of functions that cache 
## the inverse of a matrix.

## For this assignment, I assumed that the matrix supplied is always invertible.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is a matrix we want to invert
    inv <- NULL
    
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## 'x' is a list containing a function created in makeCachMatrix function
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    message("setting cached data")
    data <- x$get()
    inv <-solve(data)
    x$setinverse(inv)
    
    inv
}
