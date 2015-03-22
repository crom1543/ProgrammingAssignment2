## This code is the Programming Assignment 2 for R Programming course in Coursera.org

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly.
# This assignment is to write a pair of functions that cache the inverse of a matrix.

# I assume that the matrix supplied is always invertible in accordance with the assignment instruction.

## makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = makeCacheMatrixtrix()) {
    cachedInv <- NULL                       # Initialize cache variable
    set <- function(y) {                    # Set the value of the matrix
        x <<- y                             
        cachedInv <<- NULL                  
    }
    get <- function() x                     # Get the value of the matrix
    setInverse <- function(inv) cachedInv <<- inv # Set the inverse of the matrix
    getInverse <- function() cachedInv      # Get the inverse of the Matrix
    list(set = set, get = get,              # Lists out the values of the functions
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve() function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    localInv <- x$getInverse()              # Put cachedInv of 'x' object to local variable 
    if(!is.null(localInv)) {                # Check that the inverse of the matrix is already cached
        message("getting cached data")
        return(localInv)                    # if the inverse is cached, return it.
    }
    data <- x$get()                         # Put the original matrix of 'x' object to local variable
    localInv <- solve(data, ...)            # Calculate the inverse of the martix
    x$setInverse(localInv)                  # Save the inverse to cache
    localInv                                # Return the inverse 
}