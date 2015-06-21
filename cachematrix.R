# Matrix inversion is considered a costly computation
# This caching of the inverse of a matrix could speed up processing. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# a. set the value of the matrix
# b. get the value of the matrix
# c. set the value of inverse of the matrix
# d. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Define function to set the value of the matrix. It also clears the old
  # inverse from the cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Define function to get the value of the matrix
  get <- function() {
    x
  }
  # Define function to set the inverse. Used by getinverse() when
  # there is no cached inverse
  setinverse <- function(inverse) {
    m <<- inverse
  }
  # Define function to get the inverse
  getinverse <- function() {
    inv
  }
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is invertible.
cacheSolve <- function(x, ...) {
  # get the cached value
  m <- x$getinverse()
  # if a cached value exists return it
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  # otherwise get the matrix, caclulate the inverse and store it in cache
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
