## Put comments here that give an overall description of what your
## functions do

# CacheMatrix.R
#
# This file contains two functions used to cache the inverse of a square matrix,
# rather than computing it repeatedly:
#
# makeCacheMatrix - keeps a copy of the matrix and its inverse matrix, once it
#                   has been calculated for the first time
# cacheSolve      - calculates the inverse of a matrix, but recalling its value
#                   from a cache if it has already been calculated

## Write a short comment describing this function

# makeCacheMatrix - keeps a copy of the matrix and its inverse matrix.
#                   Takes as parameter a square matrix
#                   Returns a list of 4 functions, which can be called from then on:
#                   - set: set the value of a new square matrix
#                   - get: get the value of the square matrix
#                   - setinverse: set the value of its inverse matrix
#                   - getinverse: get the value of its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invx <<- inverse
  getinverse <- function() invx
  list (set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}

## Write a short comment describing this function

# cacheSolve - Calculates the inverse matrix of a matrix created with the 
#             makeCacheMatrix function. Checks to see if the inverse matrix
#             has already been calculated. If so, gets the inverse matrix from
#             the cache and skips the computation. Otherwise, it calculates the
#             inverse matrix of the data and sets the value of the inverse matrix
#             in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinverse()
  if (!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinverse(invx)
  invx
}
