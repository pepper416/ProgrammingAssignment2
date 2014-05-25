# R programing
# Assignment 2

# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly (there are also alternatives to matrix inversion that we will
# not discuss here). Your assignment is to write a pair of functions that
# cache the inverse of a matrix.


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  ### set matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ### get matrix
  get <- function() x
  
  ### set inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  ### get inverse of the matrix
  getinverse <- function() i
  
  ### list the four subfunctions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ### get the inverse of matrix 
  i <- x$getinverse()
  
  ### check to see if the returned cache has anything in it 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ### use get() subfunction to get matrix, and place it in a local variable "data"
  data <- x$get()
  
  ### calculate the inverse of local variable "data"
  i <- solve(data, ...)
  
  ### use setinverse() subfunction to set the inverse value calculated 
  x$setinverse(i)
  
  ### return the inverse matrix
  i
}
