# This file contains two functions to fullfill Assignment 2 from the R-programming Coursera course
## The first function, 'makeCacheMatrix' creates a class-type object to store a matrix and its inverse (if called)
## The function is provided with set and get procedures as a class-type object
## The function also implements the inverse set and get procedures to store the matrix inverse value and save computation time
## The second function, 'cacheSolve' uses the previous function to obtain the inverse of a matrix object
## If the inverse matrix has been computed, it return the stored value, othewise it computes the inverse, store it and return the value

## Creates a class-type object to store a matrix and its inverse (if called) with set and get procedures for both matrices

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of a matrix using the class-type object created with 'makeCacheMatrix' and its set and get procedures
## If the inverse has been previously calculated it returns its cached value saving computer time.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
