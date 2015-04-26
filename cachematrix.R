## A pair of functions that cache the inverse of a matrix
## Coursera R-Programming - Programming Assignment 2

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setmat <- function(solve) mat <<- solve
  getmat <- function() mat
  list(set = set, get = get, setmat = setmat, getmat = getmat)
}


## This function computes the inverse of the special "matrix" returned by
cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getmat()
  if(!is.null(mat)){
    return(mat)
  }
  matrix <- x$get()
  mat <- solve(matrix, ...)
  x$setmat(mat)
  mat
}
