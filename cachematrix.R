## This file implements a user defined (custom) object which can store a matrix and its inverse.
## Using the custom matrix object created by makeCacheMatrix and using cacheSolve to compute inverse
## of matrix can greatly improve performance of an application which needs to repeatedly compute inverse of matrices.

## makeCacheMatrix constructs a user defined object which wraps a matrix and its inverse.
## It provides getter/setter method for easy access to stored matrix and its inverse.

makeCacheMatrix <- function(mat = matrix()) {
  ## matinverse stores inverse of the input matrix. Intialize it with NULL.
  matinverse <- NULL
  
  ## A helper method to set matrix. When matrix is set, 
  ## previously cached matinverse is set to NULL.
  set <- function(newmat) {
    mat <<- newmat
    matinverse <<- NULL
  }
  
  ## Helper method to return cached matrix
  get <- function() mat
  
  ## Helper method to set inverse of the matrix.
  setinverse <- function(inverse) matinverse <<- inverse
  
  ## Helper method to get inverse of matrix.
  getinverse <- function() matinverse
  
  ## Return a list containing getter and setter functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## if the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get stored inverse of the matrix.
  inv <- x$getinverse()
  
  ## if stored inverse is null, then compute the matrix inverse and store it.
  if (is.null(inv)) {
    inv <- solve(x$get())
    x$setinverse(inv)
  }
  ## Return the matrix inverse
  inv
}
