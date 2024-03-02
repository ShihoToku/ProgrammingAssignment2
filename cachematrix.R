## Put comments here that give an overall description of what your
## functions do

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
  
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

# Function to compute the inverse of the special "matrix" object created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  # If the inverse is already calculated, return it
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  # If the inverse is not calculated, compute it
  mat <- x$get()
  inverse <- solve(mat, ...)
  
  # Cache the inverse
  x$setInverse(inverse)
  inverse
}
