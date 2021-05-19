## makeCacheMatrix and cacheSolve are functions that cache the results of 
## matrix inverting

## makeCacheMatrix creates object that contains functions to set and get matrix
## and set and get inverted matrix
## it is possible to create cacheMatrix only with quadratic matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    if (nrow(y) != ncol(y)) {
      stop("Matrix should be quadratic")
    }
    x <<- y
    x_inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  set_inv <- function(solve) {
    x_inv <<- solve
  }
  
  get_inv <- function() {
    x_inv
  }
  
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## If the inverse matrix is set returns the inverted matrix from cache
## otherwise calculates inverted matrix, stores it in cache and returns result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$get_inv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$set_inv(x_inv)
  x_inv
}
