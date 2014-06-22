## These two functions are meant to calculate and cache the inverse of
## a matrix.


## This first function creates a list of methods to set up and manage
## the matrix and to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Fallback value for the variable that caches the inverse
  i <- NULL
  # Method that resets the matrix and clears the cache
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Method that returns the matrix
  get <- function() x
  # Method that caches the inverse
  setinverse <- function(inverse) i <<- inverse
  # Method that returns the cached inverse value
  getinverse <- function() i
  # The following returns the actual list.
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This second function uses the methods of the first function to retrieve
## the cached values. If no value is cached, it inverts the matrix and it
## caches the results.

cacheSolve <- function(x, ...) {
  # Extract the cached value or its falback
  i <- x$getinverse()
  # If there's a cached valye, return it with an appropriate message
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # If there was no cached inverse, retrieve the original matrix
  data <- x$get()
  # Invert the function
  i <- solve(data, ...)
  # Cache the inverse
  x$setinverse(i)
  # Return the inverse
  i
}
