### Create 2 functions to cache the inverse of a matrix. ###
### Assume that the matrix provided is always invertible ###

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to:
# 1.set the matrix you want to invert
# 2.get the matrix you want to invert
# 3.set the inverse matrix
# 4.get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) m <<- Solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function but it first checks if the inverse matrix 
## has already been calculated in order to skip the calculation

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}