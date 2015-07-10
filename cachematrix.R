## Matrix inversion can be a costly computation. The functions defined
## in this file use caching to avoid repeated computations of the
## inverse for the same matrix.   


## makeCacheMatrix creates a special data structure in which a matrix
## and its inverse are "stored".

makeCacheMatrix <- function(m = matrix()) {

     # Variables in this scope are "shared" by the functions defined
     # below and can therefore act as a common store. Variables 'm' (the 
     # formal parameter) and 'inverse' used to store the matrix and
     # its inverse respectively. NULL used to indicate that a valid
     # value hasn't been stored/cached yet. 
     inverse <- NULL

     # Define functions to set (store) and get (retrieve) the matrix
     # and its inverse  
     setMatrix <- function(newMatrix) {

          # Update m of the outer scope
          m <<- newMatrix

          # If new matrix being stored then previously cached inverse
          # no longer valid
          inverse <<- NULL
     }
     getMatrix <- function() m
     setInverse <- function(newInverse) inverse <<- newInverse
     getInverse <- function() inverse

     # Return the list of functions defined above
     list(setMatrix = setMatrix, getMatrix = getMatrix, 
          setInverse = setInverse, getInverse = getInverse) 
}

## cacheSolve is a more efficient implementation of inverse which
## caches its computation so that whenever the matrix doesn't change  
## it retrieves the cached value rather than recomputing the inverse.  

cacheSolve <- function(x, ...) {
     ## Input 'x' expected to be a "special" matrix of the form created
     ## by makeCacheMatrix
     ## Return the inverse of the matrix stored in 'x'

     # Retrieve cached value of inverse
     inverse <- x$getInverse()

     # If no cached value then compute inverse and store it  
     if (is.null(inverse)) {
          m <- x$getMatrix()
          inverse <- solve(m, ...)
          x$setInverse(inverse)
     }

     # Return the inverse 
     inverse
}
